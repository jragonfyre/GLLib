{-# LANGUAGE PackageImports, RecordWildCards #-}

module Tutorial3 where

-- modified
-- http://funloop.org/post/2014-03-15-opengl-from-haskell.html 
-- http://funloop.org/post/2014-03-19-opengl-haskell-3-matrices.html

import Lib
import LinAlg
import Geometry
import Matrices hiding (lookAt)
import Matrices as Mat
import Model

import Control.Monad
import Data.IORef
import Data.Maybe
import Data.ObjectName
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import System.Exit
import Graphics.Rendering.OpenGL 

import Foreign.Marshal.Array (withArrayLen)
import Foreign.Storable (sizeOf)
import GHC.Ptr (nullPtr)
import GHC.Float (double2Float)

data GLObjs = GLObjs
  { program :: Program
  , vertArrObj :: VertexArrayObject
  , vertAttrib :: AttribLocation
  , vertBuffObj :: BufferObject
  , colorAttrib :: AttribLocation
  , colorBuffObj :: BufferObject
  , normalAttrib :: AttribLocation
  , normalBuffObj :: BufferObject
  , mMatrix :: UniformLocation
  , vpMatrix :: UniformLocation
  , numLights :: UniformLocation
  , light0type :: UniformLocation
  , light0color :: UniformLocation
  , light1type :: UniformLocation
  , light1pos :: UniformLocation
  , light1color :: UniformLocation
  , mMat :: GLmatrix GLfloat
  , vpMat :: GLmatrix GLfloat
  , vpMatRaw :: Mat4 GLfloat
  }

freeResources :: GLObjs -> IO ()
freeResources GLObjs{..} = do
  deleteObjectName vertBuffObj 
  deleteObjectName vertArrObj

initialize :: IO GLFW.Window
initialize = do
  ok <- GLFW.init
  when (not ok) $ do
    _ <- fail "Failed to initialize GLFW"
    exitFailure
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
  depthFunc $= Just Less
  prog <- makeProgram
  vatt <- get $ attribLocation prog "vertexPos_ms"
  catt <- get $ attribLocation prog "vertexColor"
  natt <- get $ attribLocation prog "vertexNormal_ms"
  mloc <- get $ uniformLocation prog "M"
  vploc <- get $ uniformLocation prog "VP"
  nl <- get $ uniformLocation prog "numLights"
  l0t <- get $ uniformLocation prog "lights[0].type"
  l0c <- get $ uniformLocation prog "lights[0].color"
  l1t <- get $ uniformLocation prog "lights[1].type"
  l1p <- get $ uniformLocation prog "lights[1].lightPos"
  l1c <- get $ uniformLocation prog "lights[1].color"
  vao <- genObjectName :: IO VertexArrayObject
  bindVertexArrayObject $= (Just vao)
  let (cubeP, cubeC, cubeN) = vertexLists cube
  vbuf <- fillNewBuffer cubeP
  cbuf <- fillNewBuffer cubeC
  nbuf <- fillNewBuffer cubeN
  let v = Mat.lookAt (Point3 0 0 4) (Point3 0 0 0) (Vec3 0 1 0) :: Mat4 GLfloat
  let p = fovProjMat 45 (9/16) (ClipP (0.1) (20)) :: Mat4 GLfloat
  let pv = p *#* v
  -- model matrix is identity here
  vpmat <- newMatrix ColumnMajor $ matrixToCMList pv
  id <- newMatrix ColumnMajor $ matrixToCMList identity
  return $ GLObjs
    { program = prog
    , vertArrObj = vao
    , vertAttrib = vatt
    , vertBuffObj = vbuf
    , colorAttrib = catt
    , colorBuffObj = cbuf
    , normalAttrib = natt
    , normalBuffObj = nbuf
    , mMatrix = mloc
    , vpMatrix = vploc
    , numLights = nl
    , light0type = l0t
    , light0color = l0c
    , light1type = l1t
    , light1pos = l1p
    , light1color = l1c
    , mMat = id
    , vpMat = vpmat
    , vpMatRaw = pv
    }
  
buildMatrix :: (Mat4 GLfloat) -> IO (GLmatrix GLfloat)
buildMatrix = newMatrix ColumnMajor . matrixToCMList

bindBufferToAttrib :: BufferObject -> AttribLocation -> IO ()
bindBufferToAttrib buf loc = do
  (vertexAttribArray loc) $= Enabled
  (bindBuffer ArrayBuffer) $= (Just buf)
  (vertexAttribPointer loc) $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
  

main :: IO ()
main = do
  win <- initialize
  globs <- initializeGL
  time <- GLFW.getTime
  let t = double2Float $ fromMaybe 0 time
  lt <- newIORef t
  anivar1 <- newIORef 0.0
  rate <- newIORef 1.0
  inputLoop lt anivar1 rate win globs
  freeResources globs
  GLFW.terminate
  return ()

inputLoop :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> GLFW.Window -> GLObjs -> IO ()
inputLoop lt anivar1 rate win globs = do
  time <- GLFW.getTime 
  lastt <- readIORef lt
  r <- readIORef rate
  let t = double2Float $ fromMaybe 0 time
  let delta = t-lastt
  writeIORef lt t
  modifyIORef anivar1 (+(delta*r))
  anit1 <- readIORef anivar1
  drawStuff 20 t anit1 globs
  GLFW.swapBuffers win
  GLFW.pollEvents
  plus <- GLFW.getKey win GLFW.Key'Equal
  minus <- GLFW.getKey win GLFW.Key'Minus
  when (plus == GLFW.KeyState'Pressed) $
    modifyIORef rate (*1.1)
  when (minus == GLFW.KeyState'Pressed) $
    modifyIORef rate (/1.1)
  keyState <- GLFW.getKey win GLFW.Key'Escape
  closeWindow <- GLFW.windowShouldClose win
  when (keyState /= GLFW.KeyState'Pressed && closeWindow == False) $
    inputLoop lt anivar1 rate win globs

makePoints :: Int -> Int -> Int -> Int -> [Point3 GLfloat]
makePoints n r a b =
  let
    af = fromIntegral a
    rf = fromIntegral r
    bf = fromIntegral b
  in
    [ Point3 (sin (2*pi*(fromIntegral k)/af/rf)) (cos (2*pi*(fromIntegral k)/bf/rf)) 0 | k <- [1..(n*r*a*b)] ]


drawStuff :: Int -> GLfloat -> GLfloat -> GLObjs -> IO ()
drawStuff n t a GLObjs{..} = do
  clear [ColorBuffer, DepthBuffer]
  currentProgram $= (Just program)
  bindBufferToAttrib vertBuffObj vertAttrib
  bindBufferToAttrib colorBuffObj colorAttrib
  bindBufferToAttrib normalBuffObj normalAttrib
  (uniform numLights) $= (2 :: GLint)
  (uniform light0type) $= (1 :: GLint)
  (uniform light0color) $= (Color3 0.00 0.00 0.00 :: Color3 GLfloat)
  (uniform light1type) $= (0 :: GLint)
  (uniform light1pos) $= (Vertex3 (1.3*(sin (13*t/15))) (1.3*(cos (17*t/15))) 2 :: Vertex3 GLfloat)
  (uniform light1color) $= (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
  let rspeed = 0.2
  let rotator = (rotateX (3*a*rspeed)) *#* (rotateY (5*a*rspeed))
  vpmatrix <- buildMatrix $ vpMatRaw 
  (uniform vpMatrix) $= vpmatrix
  let myPoints = makePoints 1 n 3 5
  forM_ myPoints $ \p -> do
    modelMat <- buildMatrix $ rotator *#* (p3 Geometry.translate p) *#* (scaleUniform3 (pi/6/(fromIntegral n)))
    (uniform mMatrix) $= modelMat
    drawArrays Triangles 0 (3*6*2)
  --let rspeed = 0.2
  --modelMat <- buildMatrix $ (rotateX (3*t*rspeed)) *#* (rotateY (5*t*rspeed))
  --(uniform mMatrix) $= modelMat
  --drawArrays Triangles 0 (3*6*2)
  (vertexAttribArray vertAttrib) $= Disabled
  (vertexAttribArray colorAttrib) $= Disabled


-- Constants
vertexShader2 :: String
vertexShader2 = unlines
  [ "#version 330 core"
  , "in vec3 vertexPos_ms;"
  , "in vec3 vertexColor;"
  , "in vec3 vertexNormal_ms;"
  , "uniform mat4 M;"
  , "uniform mat4 VP;"
  , "out vec3 fragmentColor;"
  , "out vec4 fragmentNormal;"
  , "out vec4 fragPos_ws;"
  , "void main()"
  , "{"
  ,   "fragmentColor = vertexColor;"
  ,   "vec4 v = vec4(vertexPos_ms,1);"
  ,   "fragPos_ws = M * v;"
  ,   "gl_Position = VP * fragPos_ws;"
  ,   "vec4 n = vec4(vertexNormal_ms,0);"
  ,   "fragmentNormal = M * n;"
  , "}"
  ]

-- Type 0
-- Point source
-- Type 1
-- Uniform source
fragmentShader2 :: String
fragmentShader2 = unlines
  [ "#version 330 core"
  , "in vec3 fragmentColor;"
  , "in vec4 fragmentNormal;"
  , "in vec4 fragPos_ws;"
  , "struct Light {"
  ,   "int type;"
  ,   "vec3 lightPos;"
  ,   "vec3 color;"
  , "};"
  , "uniform Light lights[5];"
  , "uniform int numLights;"
  , "out vec3 finalColor;"
  , "void main()"
  , "{"
  ,   "vec3 normal = normalize(fragmentNormal.xyz);"
  ,   "vec3 lightIntensities=vec3(0);"
  ,   "vec3 pos=fragPos_ws.xyz;"
  ,   "vec3 toLight;"
  ,   "for(int j = 0; j < numLights; j++) {"
  ,     "if(lights[j].type == 0) {"
  ,       "toLight=lights[j].lightPos-pos;"
  ,       "float l = length(toLight);"
  ,       "lightIntensities = lightIntensities + max(dot(toLight,normal)/(l*l*l),0)*lights[j].color;"
  ,     "} else if(lights[j].type == 1) {"
  ,       "lightIntensities = lightIntensities + lights[j].color;"
  ,     "}"
  ,   "}"
  ,   "finalColor = pow(lightIntensities * fragmentColor, vec3(1.0/2.2));" -- gamma correct
  , "}"
  ] 















