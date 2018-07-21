--
-- Matrices.hs
-- Copyright (C) 2016 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--
--

{-# LANGUAGE RecordWildCards #-}

module Matrices
  ( Rectangle (..)
  , SymRectangle (..)
  , ClippingPlanes (..)
  , CameraVolume (..)
  , projectionMatrix
  , fovCamera
  , fovProjMat 
  , buildView
  , lookAt
  , fpsCamera
  , vectorToList
  , matrixToCMList
  ) where

import Geometry
import LinAlg

data Rectangle a = Rect
  { left :: a
  , right :: a
  , top :: a
  , bottom :: a
  } deriving (Show, Eq, Read)

data SymRectangle a = SymRect
  { lr :: a -- distance from 0 to go to left or right
  , tb :: a -- distance from 0 to go to top or bottom
  } deriving (Show, Eq, Read)

data ClippingPlanes a = ClipP
  { near :: a
  , far :: a
  } deriving (Show, Eq, Read)

-- frustum with pyramid's tip at origin.
data CameraVolume a
  = Frustum (Rectangle a) (ClippingPlanes a)
  | SymFrustum (SymRectangle a) (ClippingPlanes a) 
  | Prism (Rectangle a) (ClippingPlanes a)
  | SymPrism (SymRectangle a) (ClippingPlanes a)
  deriving (Show, Eq, Read)

projectionMatrix :: Fractional a => CameraVolume a -> Mat4 a
projectionMatrix (Frustum Rect{..} ClipP{..})
  = let
      width = right-left
      height = top-bottom
      depth = far - near
      rpl = right + left
      tpb = top + bottom
      fpn = far + near
      fn = far * near
    in
      Mat4
        (Vec4 (2*near/width) 0 0 0)
        (Vec4 0 (2*near/height) 0 0)
        (Vec4 (rpl/width) (tpb/height) (-fpn/depth) (-1))
        (Vec4 0 0 ((-2)*fn/depth) 0)
projectionMatrix (SymFrustum SymRect{..} ClipP{..})
  = let
      depth = far - near
      fpn = far + near
      fn = far * near
    in
      Mat4
        (Vec4 (near/lr) 0 0 0)
        (Vec4 0 (near/tb) 0 0)
        (Vec4 0 0 (-fpn/depth) (-1))
        (Vec4 0 0 ((-2)*fn/depth) 0)
projectionMatrix (Prism Rect{..} ClipP{..})
  = let
      width = right-left
      height = top-bottom
      depth = far - near
      rpl = right + left
      tpb = top + bottom
      fpn = far + near
    in
      Mat4
        (Vec4 (2/width) 0 0 0)
        (Vec4 0 (2/height) 0 0)
        (Vec4 0 0 (-2/depth) 0)
        (Vec4 (-rpl/width) (-tpb/height) (-fpn/depth) 1)
projectionMatrix (SymPrism SymRect{..} ClipP{..})
  = let
      depth = far - near
      fpn = far + near
    in
      Mat4
        (Vec4 (1/lr) 0 0 0)
        (Vec4 0 (1/tb) 0 0)
        (Vec4 0 0 (-2/depth) 0)
        (Vec4 0 0 (-fpn/depth) 1)

-- fov is in degrees
-- aspect is height/width 
fovCamera :: (Floating a) => a -> a -> ClippingPlanes a -> CameraVolume a
fovCamera fov aspect cp@(ClipP{..}) = 
  let
    rat = tan (fov*pi/360)
    lr = near * rat
    tb = aspect * lr
  in
    SymFrustum SymRect{..} cp

fovProjMat :: (Floating a) => a -> a -> ClippingPlanes a -> Mat4 a
fovProjMat fov aspect = projectionMatrix . fovCamera fov aspect


-- given orthogonal xaxis, yaxis, zaxis, vectors (!) and camera position point(!) build view matrix.
buildView :: (Num a) => Vec4 a -> Vec4 a -> Vec4 a -> Vec4 a -> Mat4 a
buildView xaxis yaxis zaxis ev =
  let
    ntx = (-(dot xaxis ev))
    nty = (-(dot yaxis ev))
    ntz = (-(dot zaxis ev))
    xax' = 
      let
        (Vec4 x y z _) = xaxis
      in
        (Vec4 x y z ntx)
    yax' = 
      let
        (Vec4 x y z _) = yaxis
      in
        (Vec4 x y z nty)
    zax' = 
      let
        (Vec4 x y z _) = zaxis
      in
        (Vec4 x y z ntz)
  in
    transpose $ Mat4 xax' yax' zax' e4

lookAt :: (Floating a) => Point3 a -> Point3 a -> Vec3 a -> Mat4 a
lookAt eye@(Point3 ex ey ez) target up = -- eye and target should be points, up should be a vector
  let
    ev = homogenize eye
    tv = homogenize target
    uv = homogenize up
    zaxis@(Vec4 x y z _) = normalize $ ev -# tv
    xaxis = normalize $ (v3 crossProduct up) *| zaxis
    yaxis = (crossProduct x y z) *| xaxis
  in
    buildView xaxis yaxis zaxis ev

-- view matrix for a camera at the point given with yaw and then pitch
fpsCamera :: (Floating a) => Point3 a -> a -> a -> Mat4 a
fpsCamera eye yaw pitch = 
  let
    ev = homogenize eye
    cp = cos pitch
    sp = sin pitch
    cy = cos yaw
    sy = sin yaw
    xaxis = Vec4 cy 0 (-sy) 0
    yaxis = Vec4 (sy * sp) cp (cy * sp) 0
    zaxis = Vec4 (sy * cp) (-sp) (cp * cy) 0
  in
    buildView xaxis yaxis zaxis ev

vectorToList :: Vec4 a -> [a]
vectorToList (Vec4 x y z w) = [x,y,z,w]

matrixToCMList :: (Num a) => Mat4 a -> [a]
matrixToCMList (Mat4 s t u v) = concat $ map vectorToList [s,t,u,v]
matrixToCMList (Diag4 s t u v) = [s,0,0,0,0,t,0,0,0,0,u,0,0,0,0,v]


{- unoptimized
lookAt :: (Floating a) => Point3 a -> Point 3 a -> Vec3 a -> Mat4 a
lookAt eye@(Point3 ex ey ez) target up = -- eye and target should be points, up should be a vector
  let
    ev = homogenize eye
    tv = homogenize target
    uv = homogenize up
    zaxis@(Vec4 x y z) = normalize $ ev - tv
    xaxis = normalize $ (v3 crossProduct up) *| zaxis
    yaxis = (crossProduct x y z) *| xaxis
    orient = transpose $ Mat4 xaxis yaxis zaxis e4
    trans = translate (-ex) (-ey) (-ez)
  in
    orient *#* trans
-}



