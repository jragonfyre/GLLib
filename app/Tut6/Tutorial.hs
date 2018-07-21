{-# LANGUAGE PackageImports, RecordWildCards #-}

module Tut6.Tutorial where

-- modified
-- http://funloop.org/post/2014-03-15-opengl-from-haskell.html 
-- http://funloop.org/post/2014-03-19-opengl-haskell-3-matrices.html
--

import Tut6.Shaders
import Tut6.Types
import Tut6.Init

import Lib
import LinAlg
import Geometry
import Matrices -- hiding (lookAt)
import Matrices as Mat
import Model

import Control.Monad
import Data.IORef
import Data.Maybe
import Data.ObjectName
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import System.Exit
import Graphics.GL --(glFramebufferTexture)
import Graphics.Rendering.OpenGL hiding (lookAt)
--import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferTarget
import Unsafe.Coerce
--import Graphics.Rendering.OpenGL.GLU.ErrorsInternal (recordInvalidValue)

import Foreign.Marshal.Array (withArrayLen)
import Foreign.Storable (sizeOf)
import GHC.Ptr (nullPtr)
import GHC.Float (double2Float)



data GLModelData = GLModData
  { vertArrObj :: VertexArrayObject
  , vertBuffObj :: BufferObject
  , colorBuffObj :: BufferObject
  , normalBuffObj :: BufferObject
  }


data LightData = LData
  { ltype :: UniformLocation
  , lpos :: UniformLocation
  , lcolor :: UniformLocation
  }

data LightInfo
  = PointSrc (Model.Color GLfloat) (Model.Position GLfloat)
  | AmbientSrc (Model.Color GLfloat)

lightType :: LightInfo -> GLint
lightType (PointSrc _ _) = 0
lightType (AmbientSrc _) = 1

lightColor :: LightInfo -> Vector3 GLfloat
lightColor (PointSrc (Vec3 x y z) _) = Vector3 x y z
lightColor (AmbientSrc (Vec3 x y z)) = Vector3 x y z

hasPos :: LightInfo -> Bool
hasPos (PointSrc _ _) = True
hasPos (AmbientSrc _) = False

lightPosition :: LightInfo -> Vertex3 GLfloat
lightPosition (PointSrc _ (Point3 x y z)) = Vertex3 x y z
lightPosition (AmbientSrc _) = error "light has no position"

assignLight :: LightData -> LightInfo -> IO ()
assignLight LData{..} li = do
  (uniform ltype) $= (lightType li)
  (uniform lcolor) $= (lightColor li)
  when (hasPos li) $
    (uniform lpos) $= (lightPosition li)
    
screenWidth = 1280
screenHeight = 720

data GLObjs = GLObjs
  { program :: Program
  , mMatrix :: UniformLocation
  , vpMatrix :: UniformLocation
  , numLights :: UniformLocation
  , specPow :: UniformLocation
  , cameraPos :: UniformLocation
  , shadowMap :: UniformLocation
  , farPlane :: UniformLocation
  , vertAttrib :: AttribLocation
  , colorAttrib :: AttribLocation
  , normalAttrib :: AttribLocation
  , shadowProg :: Program
  , shadowBuff :: FramebufferObject
  , cubeTexture :: TextureObject
  --, mat :: UniformLocation
  , mat0 :: UniformLocation
  , mat1 :: UniformLocation
  , mat2 :: UniformLocation
  , mat3 :: UniformLocation
  , mat4 :: UniformLocation
  , mat5 :: UniformLocation
  , shadModel :: UniformLocation
  , shadLL :: UniformLocation
  , shadFarPlane :: UniformLocation
  , shadVertAttrib :: AttribLocation
  , shadowTestProg :: Program
  , mat :: UniformLocation
  , shadTestModel :: UniformLocation
  , shadTestLL :: UniformLocation
  , shadTestFarPlane :: UniformLocation
  , shadTestVertAttrib :: AttribLocation
  , cubeModel :: GLModelData
  , squareModel :: GLModelData
  , light0 :: LightData
  , light1 :: LightData
  , mMat :: GLmatrix GLfloat
  , vpMat :: GLmatrix GLfloat
  , vpMatRaw :: Mat4 GLfloat
  }

freeModel :: GLModelData -> IO ()
freeModel GLModData{..} = do
  deleteObjectName vertBuffObj 
  deleteObjectName colorBuffObj 
  deleteObjectName normalBuffObj
  deleteObjectName vertArrObj

freeResources :: GLObjs -> IO ()
freeResources GLObjs{..} = do
  freeModel cubeModel
  freeModel squareModel

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
  win <- GLFW.createWindow screenWidth screenHeight "Title of Window!" Nothing Nothing
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
  

makeProgram :: ProgramInfo -> IO Program
makeProgram ProgInfo{..} = do
  prog <- createProgram
  vs <- buildShader VertexShader vertexShader
  fs <- buildShader FragmentShader fragmentShader
  attachShader prog vs
  gs <- handleGeometryShader prog geometryShader
  attachShader prog fs
  linkProgram prog
  deleteObjectName vs
  deleteGeometryShader gs
  deleteObjectName fs
  ls <- get $ linkStatus prog
  when (not $ ls) $ do
    il <- get $ programInfoLog prog
    _ <- fail ("Program failed to link: " ++ il)
    exitFailure
  return prog
  where
    handleGeometryShader prog Nothing = return Nothing
    handleGeometryShader prog (Just geom) = do
      gs <- buildShader GeometryShader geom
      attachShader prog gs
      return (Just gs)
    deleteGeometryShader Nothing = return ()
    deleteGeometryShader (Just gs) = deleteObjectName gs

fillNewBuffer :: [GLfloat] -> IO BufferObject
fillNewBuffer xs = do
  buf <- genObjectName :: IO BufferObject
  (bindBuffer ArrayBuffer) $= (Just buf)
  withArrayLen xs f
  return buf
  where
    f len ptr =
      (bufferData ArrayBuffer) $= (fromIntegral (len * sizeOf (undefined :: GLfloat)), ptr, StaticDraw)

cameraPosition :: Point3 GLfloat
cameraPosition = Point3 0 0 4

camPos :: Vector3 GLfloat
camPos = case cameraPosition of 
  (Point3 x y z) ->
    Vector3 x y z
  

initializeGL :: IO GLObjs
initializeGL = do
  clearColor $= Color4 0 0 0 0
  depthFunc $= Just Less
  prog <- makeProgram progInfo2
  vatt <- get $ attribLocation prog "vertexPos_ms"
  catt <- get $ attribLocation prog "vertexColor"
  natt <- get $ attribLocation prog "vertexNormal_ms"
  mloc <- get $ uniformLocation prog "M"
  vploc <- get $ uniformLocation prog "VP"
  nl <- get $ uniformLocation prog "numLights"
  sp <- get $ uniformLocation prog "specPow"
  cp <- get $ uniformLocation prog "cameraPos"
  farp <- get $ uniformLocation prog "far_plane"
  l0t <- get $ uniformLocation prog "lights[0].type"
  l0p <- get $ uniformLocation prog "lights[0].lightPos"
  l0c <- get $ uniformLocation prog "lights[0].color"
  l1t <- get $ uniformLocation prog "lights[1].type"
  l1p <- get $ uniformLocation prog "lights[1].lightPos"
  l1c <- get $ uniformLocation prog "lights[1].color"
  shadmap <- get $ uniformLocation prog "shadowMap"
  shadProg <- makeProgram shadowProgInfo
  --shadvp <- get $ uniformLocation shadProg "projection"
  m0 <- get $ uniformLocation shadProg "cubeMapProjections[0]"
  m1 <- get $ uniformLocation shadProg "cubeMapProjections[1]"
  m2 <- get $ uniformLocation shadProg "cubeMapProjections[2]"
  m3 <- get $ uniformLocation shadProg "cubeMapProjections[3]"
  m4 <- get $ uniformLocation shadProg "cubeMapProjections[4]"
  m5 <- get $ uniformLocation shadProg "cubeMapProjections[5]"
  sM <- get $ uniformLocation shadProg "M"
  sLL <- get $ uniformLocation shadProg "lightPos"
  sFP <- get $ uniformLocation shadProg "far_plane"
  svatt <- get $ attribLocation shadProg "vertexPos_ms"
  shadTestProg <- makeProgram shadTestShader
  shadTestvp <- get $ uniformLocation shadTestProg "projection"
  sTestM <- get $ uniformLocation shadTestProg "M"
  sTestLL <- get $ uniformLocation shadTestProg "lightPos"
  sTestFP <- get $ uniformLocation shadTestProg "far_plane"
  stvatt <- get $ attribLocation shadTestProg "vertexPos_ms"
  spactive <- get $ activeUniforms shadProg
  putStrLn $ show spactive
  putStrLn $ show m0
  putStrLn $ show m1
  putStrLn $ show m2
  putStrLn $ show m3
  putStrLn $ show m4
  putStrLn $ show m5
  (shadBuff,cubeText) <- setupShadowMap
  vao <- genObjectName :: IO VertexArrayObject
  bindVertexArrayObject $= (Just vao)
  let (cubeP, cubeC, cubeN) = vertexLists cube
  vbuf <- fillNewBuffer cubeP
  cbuf <- fillNewBuffer cubeC
  nbuf <- fillNewBuffer cubeN
  vao2 <- genObjectName :: IO VertexArrayObject
  bindVertexArrayObject $= (Just vao2)
  let 
    zdist = -3
    (squareP, squareC, squareN) = vertexLists
      $ parallelogram (Vec3 1.0 0.0 0.0)
      $ Model.Triangle
          (Point3 (-15) (-15) zdist)
          (Point3 15 (-15) zdist)
          (Point3 (-15) (15) zdist)
  vbuf2 <- fillNewBuffer squareP
  cbuf2 <- fillNewBuffer squareC
  nbuf2 <- fillNewBuffer squareN
  let v = Mat.lookAt cameraPosition (Point3 0 0 0) (Vec3 0 1 0) :: Mat4 GLfloat
  let p = fovProjMat 45 (9/16) (ClipP (0.1) (20)) :: Mat4 GLfloat
  let pv = p *#* v
  -- model matrix is identity here
  vpmat <- newMatrix ColumnMajor $ matrixToCMList pv
  id <- newMatrix ColumnMajor $ matrixToCMList identity
  return $ GLObjs
    { program = prog
    , mMatrix = mloc
    , vpMatrix = vploc
    , numLights = nl
    , specPow = sp
    , cameraPos = cp
    , shadowMap = shadmap
    , farPlane = farp
    , vertAttrib = vatt
    , colorAttrib = catt
    , normalAttrib = natt
    , shadowProg = shadProg
    , shadowBuff = shadBuff
    , cubeTexture = cubeText
    --, mat = shadvp
    , mat0 = m0
    , mat1 = m1
    , mat2 = m2
    , mat3 = m3
    , mat4 = m4
    , mat5 = m5
    , shadModel = sM
    , shadLL = sLL
    , shadFarPlane = sFP
    , shadVertAttrib = svatt
    , shadowTestProg = shadTestProg
    , mat = shadTestvp
    , shadTestModel = sTestM
    , shadTestLL = sTestLL
    , shadTestFarPlane = sTestFP
    , shadTestVertAttrib = stvatt
    , cubeModel = GLModData vao vbuf cbuf nbuf
    , squareModel = GLModData vao2 vbuf2 cbuf2 nbuf2
    , light0 = LData l0t l0p l0c
    , light1 = LData l1t l1p l1c
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
  thetavar <- newIORef 0.0
  phivar <- newIORef (pi/2)
  inputLoop lt anivar1 rate thetavar phivar win globs
  freeResources globs
  GLFW.terminate
  return ()

inputLoop :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat 
  -> GLFW.Window -> GLObjs -> IO ()
inputLoop lt anivar1 rate thetavar phivar win globs = do
  time <- GLFW.getTime 
  lastt <- readIORef lt
  r <- readIORef rate
  let t = double2Float $ fromMaybe 0 time
  let delta = t-lastt
  writeIORef lt t
  modifyIORef anivar1 (+(delta*r))
  anit1 <- readIORef anivar1
  theta <- readIORef thetavar
  phi <- readIORef phivar
  drawStuff 20 t anit1 theta phi globs
  GLFW.swapBuffers win
  GLFW.pollEvents
  plus <- GLFW.getKey win GLFW.Key'Equal
  minus <- GLFW.getKey win GLFW.Key'Minus
  when (plus == GLFW.KeyState'Pressed) $
    modifyIORef rate (*1.1)
  when (minus == GLFW.KeyState'Pressed) $
    modifyIORef rate (/1.1)
  left <- GLFW.getKey win GLFW.Key'Left
  right <- GLFW.getKey win GLFW.Key'Right
  up <- GLFW.getKey win GLFW.Key'Up
  down <- GLFW.getKey win GLFW.Key'Down
  when (left == GLFW.KeyState'Pressed) $
    modifyIORef thetavar (\x -> x-0.01)
  when (right == GLFW.KeyState'Pressed) $
    modifyIORef thetavar (\x -> x+0.01)
  when (up == GLFW.KeyState'Pressed) $
    modifyIORef phivar (\x-> x-0.01)
  when (down == GLFW.KeyState'Pressed) $
    modifyIORef phivar (\x-> x+0.01)
  keyState <- GLFW.getKey win GLFW.Key'Escape
  closeWindow <- GLFW.windowShouldClose win
  when (keyState /= GLFW.KeyState'Pressed && closeWindow == False) $
    inputLoop lt anivar1 rate thetavar phivar win globs

makePoints :: Int -> Int -> Int -> Int -> [Point3 GLfloat]
makePoints n r a b =
  let
    af = fromIntegral a
    rf = fromIntegral r
    bf = fromIntegral b
  in
    [ Point3 (sin (2*pi*(fromIntegral k)/af/rf)) (cos (2*pi*(fromIntegral k)/bf/rf)) 0 | k <- [1..(n*r*a*b)] ]

bindModel :: GLModelData -> AttribLocation -> AttribLocation -> AttribLocation -> IO ()
bindModel GLModData{..} vertAttrib colorAttrib normalAttrib = do
  bindVertexArrayObject $= (Just vertArrObj)
  bindBufferToAttrib vertBuffObj vertAttrib
  bindBufferToAttrib colorBuffObj colorAttrib
  bindBufferToAttrib normalBuffObj normalAttrib

bindVertsModel :: GLModelData -> AttribLocation -> IO ()
bindVertsModel GLModData{..} vertAttrib = do
  bindVertexArrayObject $= (Just vertArrObj)
  bindBufferToAttrib vertBuffObj vertAttrib
  

shadowWidth :: GLsizei
shadowWidth = 1024
shadowHeight :: GLsizei
shadowHeight = shadowWidth -- make it square

cubeMapTargets :: [TextureTargetCubeMapFace]
cubeMapTargets = 
  [ TextureCubeMapPositiveX
  , TextureCubeMapNegativeX
  , TextureCubeMapPositiveY
  , TextureCubeMapNegativeY
  , TextureCubeMapPositiveZ
  , TextureCubeMapNegativeZ
  ]

maxColorAttachments = 16

marshalFramebufferTarget :: FramebufferTarget -> GLenum
marshalFramebufferTarget xs = case xs of
   DrawFramebuffer -> GL_DRAW_FRAMEBUFFER
   ReadFramebuffer -> GL_READ_FRAMEBUFFER
   Framebuffer -> GL_FRAMEBUFFER

marshalFramebufferObjectAttachment :: FramebufferObjectAttachment -> Maybe GLenum
marshalFramebufferObjectAttachment x = case x of
   ColorAttachment c -> let ec = fromIntegral c in if ec >= maxColorAttachments
      then Nothing
      else Just $ GL_COLOR_ATTACHMENT0 + ec
   DepthAttachment -> Just GL_DEPTH_ATTACHMENT
   StencilAttachment -> Just GL_STENCIL_ATTACHMENT
   DepthStencilAttachment -> Just GL_DEPTH_STENCIL_ATTACHMENT


framebufferTexture :: FramebufferTarget -> FramebufferObjectAttachment
   -> TextureObject -> Level -> IO ()
framebufferTexture fbt fba (TextureObject t) l = maybe (return ())--recordInvalidValue
  (\mfba -> glFramebufferTexture (marshalFramebufferTarget fbt) mfba t l)
      $ marshalFramebufferObjectAttachment fba


setupShadowMap :: IO (FramebufferObject, TextureObject)
setupShadowMap = do
  framebuff <- genObjectName 
  depthCubeMap <- genObjectName
  (textureBinding TextureCubeMap) $= (Just depthCubeMap)
  forM_ cubeMapTargets $ \t ->
    texImage2D 
      t  -- target
      NoProxy -- Idk what proxy is
      0  -- Level 0 detail
      DepthComponent' -- the pixel format, this is a shadow map, so we just render depth
      (TextureSize2D shadowWidth shadowHeight) -- texture size obvi
      0  -- border size
      (PixelData DepthComponent Float nullPtr) -- pixel data
  (textureFilter TextureCubeMap) $= ((Nearest, Nothing),Nearest)
  (textureWrapMode TextureCubeMap S) $= (Repeated, ClampToEdge)
  (textureWrapMode TextureCubeMap R) $= (Repeated, ClampToEdge)
  (textureWrapMode TextureCubeMap T) $= (Repeated, ClampToEdge)
  (bindFramebuffer Framebuffer) $= framebuff
  framebufferTexture Framebuffer DepthAttachment depthCubeMap 0 -- This doesn't exist yet :/
  drawBuffer $= NoBuffers
  readBuffer $= NoBuffers
  fbStatus <- get $ framebufferStatus Framebuffer
  putStrLn $ show fbStatus
  (bindFramebuffer Framebuffer) $= defaultFramebufferObject
  return (framebuff, depthCubeMap)


drawStuff :: Int -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLObjs -> IO ()
drawStuff n t a theta phi GLObjs{..} = do
  let
    rspeed = 0.2
    rotator = (rotateX (3*a*rspeed)) *#* (rotateY (5*a*rspeed))
    myPoints = makePoints 1 n 3 5
    lpos = Point3 (1.3*(sin (13*t/15))) (1.3*(cos (17*t/15))) 3 :: Point3 GLfloat
    lpvector3 = case lpos of 
      (Point3 x y z) ->
        (Vector3 x y z) :: Vector3 GLfloat
    lookDirection = Vec3 ((sin phi) * (sin theta)) (cos phi) (-(sin phi) * (cos theta))
    upDirection = Vec3 ((cos phi) * (sin theta)) (sin phi) (-(cos phi) * (cos theta))
    screenAspect = (fromIntegral screenHeight)/(fromIntegral screenWidth)
    camProj = (fovProjMat 45.0 screenAspect $ ClipP 0.1 100) 
    camView = lookAt cameraPosition (translatePoint cameraPosition lookDirection) upDirection
    camVP = camProj *#* camView
    aspect = (fromIntegral shadowHeight)/(fromIntegral shadowWidth)
    sFP = 25.0
    proj = projectionMatrix $ fovCamera 90.0 aspect $ ClipP 0.1 sFP
    px = Vec3 1 0 0 :: Vec3 GLfloat
    mx = Vec3 (-1) 0 0 :: Vec3 GLfloat
    py = Vec3 0 1 0 :: Vec3 GLfloat
    my = Vec3 0 (-1) 0 :: Vec3 GLfloat
    pz = Vec3 0 0 1 :: Vec3 GLfloat
    mz = Vec3 0 0 (-1) :: Vec3 GLfloat
    mpx = proj *#* (lookAt lpos (translatePoint lpos px) my) :: Mat4 GLfloat
    mmx = proj *#* (lookAt lpos (translatePoint lpos mx) my) :: Mat4 GLfloat
    mpy = proj *#* (lookAt lpos (translatePoint lpos py) pz) :: Mat4 GLfloat
    mmy = proj *#* (lookAt lpos (translatePoint lpos my) mz) :: Mat4 GLfloat
    mpz = proj *#* (lookAt lpos (translatePoint lpos pz) my) :: Mat4 GLfloat
    mmz = proj *#* (lookAt lpos (translatePoint lpos mz) my) :: Mat4 GLfloat
  matpx <- buildMatrix mpx
  matmx <- buildMatrix mmx
  matpy <- buildMatrix mpy
  matmy <- buildMatrix mmy
  matpz <- buildMatrix mpz
  matmz <- buildMatrix mmz
  --putStrLn $ show mpx
  let 
    matList = [matpx,matmx,matpy,matmy,matpz,matmz]
  (bindFramebuffer Framebuffer) $= shadowBuff
  viewport $= (Position 0 0, Size shadowWidth shadowHeight)
  --forM_ [0..5] $ \i -> do
  --  framebufferTexture2D Framebuffer DepthAttachment (cubeMapTargets !! i) cubeTexture 0 --(fromIntegral i)
  clear [DepthBuffer]
  currentProgram $= (Just shadowProg)
  (uniform shadLL) $= lpvector3
  (uniform shadFarPlane) $= sFP
  --(uniform mat) $= (matList !! i)
  (uniform mat0) $= matpx
  (uniform mat1) $= matmx
  (uniform mat2) $= matpy
  (uniform mat3) $= matmy
  (uniform mat4) $= matpz
  (uniform mat5) $= matmz
  bindVertsModel cubeModel shadVertAttrib
  forM_ myPoints $ \p -> do
    modelMat <- buildMatrix
      $ rotator *#* (p3 Geometry.translate p) *#* (scaleUniform3 (pi/6/(fromIntegral n)))
    (uniform shadModel) $= modelMat
    drawArrays Triangles 0 (3*6*2)
  bindVertsModel squareModel shadVertAttrib
  id <- buildMatrix identity
  (uniform shadModel) $= id
  drawArrays Triangles 0 (3*2)
  (vertexAttribArray shadVertAttrib) $= Disabled
  (bindFramebuffer Framebuffer) $= defaultFramebufferObject
  viewport $= (Position 0 0, Size (fromIntegral screenWidth) (fromIntegral screenHeight))
  clear [ColorBuffer, DepthBuffer]
  {-
  currentProgram $= (Just shadowTestProg)
  (uniform shadTestLL) $= camPos
  (uniform shadTestFarPlane) $= (20.0 :: GLfloat)
  vpmatrix <- buildMatrix $ vpMatRaw 
  (uniform mat) $= vpmatrix
  bindVertsModel cubeModel shadTestVertAttrib
  forM_ myPoints $ \p -> do
    modelMat <- buildMatrix
      $ rotator *#* (p3 Geometry.translate p) *#* (scaleUniform3 (pi/6/(fromIntegral n)))
    (uniform shadTestModel) $= modelMat
    drawArrays Triangles 0 (3*6*2)
  bindVertsModel squareModel shadTestVertAttrib
  id <- buildMatrix identity
  (uniform shadTestModel) $= id
  drawArrays Triangles 0 (3*2)
  (vertexAttribArray shadTestVertAttrib) $= Disabled
  -}
  currentProgram $= (Just program)
  bindModel cubeModel vertAttrib colorAttrib normalAttrib
  (uniform specPow) $= (30.0 :: GLfloat)
  (uniform numLights) $= (1 :: GLint)
  (uniform cameraPos) $= camPos
  (uniform farPlane) $= sFP
  activeTexture $= (TextureUnit 0)
  (textureBinding TextureCubeMap) $= (Just cubeTexture)
  (uniform shadowMap) $= (0 :: GLint)
  assignLight light0
    $ PointSrc
        (Vec3 3.0 3.0 3.0 :: Vec3 GLfloat)
        lpos
  vpmatrix <- buildMatrix $ camVP --vpMatRaw 
  (uniform vpMatrix) $= vpmatrix
  forM_ myPoints $ \p -> do
    modelMat <- buildMatrix $ rotator *#* (p3 Geometry.translate p) *#* (scaleUniform3 (pi/6/(fromIntegral n)))
    (uniform mMatrix) $= modelMat
    drawArrays Triangles 0 (3*6*2)
  --let rspeed = 0.2
  --modelMat <- buildMatrix $ (rotateX (3*t*rspeed)) *#* (rotateY (5*t*rspeed))
  --(uniform mMatrix) $= modelMat
  --drawArrays Triangles 0 (3*6*2)
  bindModel squareModel vertAttrib colorAttrib normalAttrib
  id <- buildMatrix identity
  (uniform mMatrix) $= id
  drawArrays Triangles 0 (3*2)
  (vertexAttribArray vertAttrib) $= Disabled
  (vertexAttribArray colorAttrib) $= Disabled
  (vertexAttribArray normalAttrib) $= Disabled





-- Constants

