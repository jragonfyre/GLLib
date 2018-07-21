{-# LANGUAGE PackageImports, RecordWildCards #-}

module Tuttest where

-- modified
-- http://funloop.org/post/2014-03-15-opengl-from-haskell.html 
-- http://funloop.org/post/2014-03-19-opengl-haskell-3-matrices.html

import Lib
import LinAlg
import Geometry
import Matrices hiding (lookAt)
import Matrices as Mat

import Control.Monad
import Data.Maybe
import Data.ObjectName
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import System.Exit
import Graphics.Rendering.OpenGL 

import Foreign.Marshal.Array (withArrayLen)
import Foreign.Storable (sizeOf)
import GHC.Ptr (nullPtr)

data GLObjs = GLObjs
  { program :: Program
  , vertArrObj :: VertexArrayObject
  , vertAttrib :: AttribLocation
  , vertBuffObj :: BufferObject
  , colorAttrib :: AttribLocation
  , colorBuffObj :: BufferObject
  , mvpMatrix :: UniformLocation
  , mvpMat :: GLmatrix GLfloat
  }

freeResources :: GLObjs -> IO ()
freeResources GLObjs{..} = do
  deleteObjectName vertBuffObj 
  deleteObjectName vertArrObj

initialize :: IO GLFW.Window
initialize = do
  putStrLn "initializing"
  ok <- GLFW.init
  putStrLn "error check"
  when (not ok) $ do
    _ <- fail "Failed to initialize GLFW"
    exitFailure
  putStrLn "setup"
  mapM_ GLFW.windowHint
    [ GLFW.WindowHint'Samples 4
    , GLFW.WindowHint'DepthBits 16
    , GLFW.WindowHint'ContextVersionMajor 3
    , GLFW.WindowHint'ContextVersionMinor 3
    , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    ]
  win <- GLFW.createWindow 1280 720 "Title of Window!" Nothing Nothing
  case win of 
    Nothing ->
      do
        _ <- fail "Failed to create OpenGL window"
        GLFW.terminate
        exitFailure
    Just win' ->
      do
        GLFW.makeContextCurrent win
        GLFW.setStickyKeysInputMode win' GLFW.StickyKeysInputMode'Enabled
        return win'

buildShader :: ShaderType -> String -> IO Shader
buildShader sType src = do
  shad <- createShader sType
  (shaderSourceBS shad) $= (packUtf8 src)
  compileShader shad
  cs <- get $ compileStatus shad
  when (not $ cs) $ do
    il <- get $ shaderInfoLog shad
    _ <- fail $ "Shader failed to compile: " ++ il
    exitFailure
  return shad
  

makeProgram :: IO Program
makeProgram = do
  prog <- createProgram
  vs <- buildShader VertexShader vertexShader2
  fs <- buildShader FragmentShader fragmentShader2
  attachShader prog vs
  attachShader prog fs
  linkProgram prog
  deleteObjectName vs
  deleteObjectName fs
  ls <- get $ linkStatus prog
  when (not $ ls) $ do
    il <- get $ programInfoLog prog
    _ <- fail ("Program failed to link: " ++ il)
    exitFailure
  return prog

fillNewBuffer :: [GLfloat] -> IO BufferObject
fillNewBuffer xs = do
  buf <- genObjectName :: IO BufferObject
  (bindBuffer ArrayBuffer) $= (Just buf)
  withArrayLen xs f
  return buf
  where
    f len ptr =
      (bufferData ArrayBuffer) $= (fromIntegral (len * sizeOf (undefined :: GLfloat)), ptr, StaticDraw)

  

initializeGL :: IO GLObjs
initializeGL = do
  clearColor $= Color4 0 0 0 0
  prog <- makeProgram
  vatt <- get $ attribLocation prog "vertexPos_ms"
  catt <- get $ attribLocation prog "vertexColor"
  mvp <- get $ uniformLocation prog "MVP"
  vao <- genObjectName :: IO VertexArrayObject
  bindVertexArrayObject $= (Just vao)
  vbuf <- fillNewBuffer vertexBufferData
  cbuf <- fillNewBuffer colorBufferData
  let v = Mat.lookAt (Point3 4 3 3) (Point3 0 0 0) (Vec3 0 1 0) :: Mat4 GLfloat
  let p = fovProjMat 45 (9/16) (ClipP (0.1) (10)) :: Mat4 GLfloat
  let pv = p *#* v
  -- model matrix is identity here
  mvpmat <- newMatrix ColumnMajor $ matrixToCMList pv
  return $ GLObjs
    { program = prog
    , vertArrObj = vao
    , vertAttrib = vatt
    , vertBuffObj = vbuf
    , colorAttrib = catt
    , colorBuffObj = cbuf
    , mvpMatrix = mvp
    , mvpMat = mvpmat
    }
  
bindBufferToAttrib :: BufferObject -> AttribLocation -> IO ()
bindBufferToAttrib buf loc = do
  (vertexAttribArray loc) $= Enabled
  (bindBuffer ArrayBuffer) $= (Just buf)
  (vertexAttribPointer loc) $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
  

main :: IO ()
main = do
  putStrLn "Hello World!"
  win <- initialize
  globs <- initializeGL
  inputLoop win globs
  freeResources globs
  GLFW.terminate
  return ()

inputLoop :: GLFW.Window -> GLObjs -> IO ()
inputLoop win globs = do
  drawStuff globs
  GLFW.swapBuffers win
  GLFW.pollEvents
  keyState <- GLFW.getKey win GLFW.Key'Escape
  closeWindow <- GLFW.windowShouldClose win
  when (keyState /= GLFW.KeyState'Pressed && closeWindow == False) $
    inputLoop win globs

drawStuff :: GLObjs -> IO ()
drawStuff GLObjs{..} = do
  clear [ColorBuffer, DepthBuffer]
  currentProgram $= (Just program)
  bindBufferToAttrib vertBuffObj vertAttrib
  bindBufferToAttrib colorBuffObj colorAttrib
  (uniform mvpMatrix) $= mvpMat
  drawArrays Triangles 0 3
  (vertexAttribArray vertAttrib) $= Disabled
  (vertexAttribArray colorAttrib) $= Disabled


-- Constants
vertexShader1 :: String
vertexShader1 = unlines
  [ "#version 330 core"
  , "layout(location = 0) in vec3 vPosition_modelspace;"
  , "void main()"
  , "{"
  ,   "gl_Position.xyz = vPosition_modelspace;"
  ,   "gl_Position.w = 1.0;"
  , "}"
  ] 
fragmentShader1 :: String
fragmentShader1 = unlines
  [ "#version 330 core"
  , "out vec3 color;"
  , "void main()"
  , "{"
  ,   "color =  vec3(1,0,0);" -- paint it red!
  , "}"
  ]

vertexShader2 :: String
vertexShader2 = unlines
  [ "#version 330 core"
  , "in vec3 vertexPos_ms;"
  , "in vec3 vertexColor;"
  , "uniform mat4 MVP;"
  , "out vec3 fragmentColor;"
  , "void main()"
  , "{"
  ,   "fragmentColor = vertexColor;"
  ,   "vec4 v = vec4(vertexPos_ms,1);"
  ,   "gl_Position = MVP * v;"
  , "}"
  ]
fragmentShader2 :: String
fragmentShader2 = unlines
  [ "#version 330 core"
  , "in vec3 fragmentColor;"
  , "out vec3 finalColor;"
  , "void main()"
  , "{"
  ,   "finalColor = pow(fragmentColor, vec3(1.0/2.2));" -- gama correct
  , "}"
  ] 


vertexBufferData :: [GLfloat]
vertexBufferData =
  -- x,y,z
  [ -1, -1, 0
  ,  1, -1, 0
  ,  0,  1, 0
  ]

colorBufferData :: [GLfloat]
colorBufferData =
  -- x,y,z
  [ 1, 0, 0
  , 0, 1, 0
  , 0, 0, 1
  ]














