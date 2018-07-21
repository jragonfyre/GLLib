--
-- Geometry.hs
-- Copyright (C) 2016 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--
--
{-# LANGUAGE FunctionalDependencies #-}

module Geometry
  ( Vec3 (..)
  , Point3 (..)
  , Homogenizable (..)
  , e1
  , e2
  , e3
  , e4
  , t3
  , v3
  , p3
  , vector3
  , point3
  , identity
  , translate
  , scale3
  , scaleUniform
  , scaleUniform3
  , rotateX
  , rotateY
  , rotateZ
  , cross
  , crossProduct
  , triangleNormal
  , tensor4
  , rotate3
  ) where

import LinAlg


e1 :: (Num a) => Vec4 a
e1 = Vec4 1 0 0 0

e2 :: (Num a) => Vec4 a
e2 = Vec4 0 1 0 0

e3 :: (Num a) => Vec4 a
e3 = Vec4 0 0 1 0

e4 :: (Num a) => Vec4 a
e4 = Vec4 0 0 0 1

t3 :: (a -> b -> c -> d) -> (a,b,c) -> d
t3 f (x,y,z) = f x y z

v3 :: (a -> a -> a -> d) -> Vec3 a -> d
v3 f (Vec3 x y z) = f x y z

p3 :: (a -> a -> a -> d) -> Point3 a -> d
p3 f (Point3 x y z) = f x y z

-- same as homogenize . Vec3
vector3 :: (Num a) => a -> a -> a -> Vec4 a
vector3 x y z = Vec4 x y z 0

-- same as homogenize . Point3
point3 :: (Num a) => a -> a -> a -> Vec4 a
point3 x y z = Vec4 x y z 1

identity :: (Num a) => Mat4 a
identity = diagMat4 1 1 1 1

translate :: (Num a) => a -> a -> a -> Mat4 a
translate x y z = basicMat4 e1 e2 e3 (point3 x y z)

scale3 :: (Num a) => a -> a -> a -> Mat4 a
scale3 x y z = diagMat4 x y z 1

scaleUniform :: (Num a) => a -> Mat4 a
scaleUniform x = diagMat4 x x x x

scaleUniform3 :: (Num a) => a -> Mat4  a
scaleUniform3 x = diagMat4 x x x 1

rotateX :: (Floating a) => a -> Mat4 a
rotateX theta = Mat4 e1 (Vec4 0 (cos theta) (sin theta) 0) (Vec4 0 (-(sin theta)) (cos theta) 0) e4

rotateY :: (Floating a) => a -> Mat4 a
rotateY theta = Mat4 (Vec4 (cos theta) 0 (-(sin theta)) 0) e2 (Vec4 (sin theta) 0 (cos theta) 0) e4

rotateZ :: (Floating a) => a -> Mat4 a
rotateZ theta = Mat4 (Vec4 (cos theta) (sin theta) 0 0) (Vec4 (-(sin theta)) (cos theta) 0 0) e3 e4

cross :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
cross (Vec3 u1 u2 u3) (Vec3 v1 v2 v3) = Vec3 (u2*v3-u3*v2) (u3*v1-u1*v3) (u1*v2-u2*v1)

-- points in ccw order looking at the front of the triangle
triangleNormal :: (Eq a, Floating a) => Point3 a -> Point3 a -> Point3 a -> Maybe (Vec3 a)
triangleNormal p1 p2 p3 =
  let
    v1 = difference p2 p1
    v2 = difference p3 p1
    prenormal = cross v1 v2
    l = sqLen prenormal
  in
    if l == 0
    then
      Nothing
    else
      Just (prenormal /# (sqrt l))

-- produces vectors
crossProduct :: (Num a) => a -> a -> a -> Mat4 a
crossProduct x y z = Mat4 (Vec4 0 z (-y) 0) (Vec4 (-z) 0 x 0) (Vec4 y (-x) 0 0) zero'

-- colVec * rowVec^T 
tensor4 :: (Num a) => Vec4 a -> Vec4 a -> Mat4 a
tensor4 rowVec@(Vec4 x y z w) colVec = Mat4 (x *# colVec) (y *# colVec) (z *# colVec) (w *# colVec)

-- not the tensor product, but it is what I need for computing rotations.
tensor3 :: (Num a) => Vec3 a -> Vec3 a -> Mat4 a
tensor3 rowVec colVec = 
  let
    rv = homogenize rowVec
    cv = homogenize colVec
    (Mat4 xv yv zv _) = tensor4 rv cv
  in
    Mat4 xv yv zv e4


rotate3 :: (Floating a) => a -> a -> a -> a -> Mat4 a
rotate3 x y z theta =
  let
    vec = vector3 x y z
    (Vec4 x' y' z' _) = normalize vec
    tup = (x', y', z')
    v3 = t3 Vec3 tup
    tens = tensor3 v3 v3
    cp = t3 crossProduct tup
    cosTheta = cos theta
    sinTheta = sin theta
    omcosTheta = 1-cosTheta
  in
    lcombG [(cosTheta, identity), (sinTheta, cp), (omcosTheta, tens)]
