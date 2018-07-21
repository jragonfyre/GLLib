--
-- LinAlg.hs
-- Copyright (C) 2016 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

{-# LANGUAGE FunctionalDependencies #-}

module LinAlg
  ( Vector (..)
  , IPVector (..)
  , CVector (..)
  , Point (..)
  , Matrix (..)
  , Vec4 (..)
  , Mat4 (..)
  , basicMat4
  , diagMat4
  , Vec3 (..)
  , Point3 (..)
  , Homogenizable (..)
  ) where

import Utils (foldl')


class Vector v where
  zero' :: (Num a) => v a
  dimension :: (Num a) => v a -> a
  (+#) :: (Num a) => v a -> v a -> v a
  infixl 6 +#
  u +# v = lcomb 1 u 1 v
  (-#) :: (Num a) => v a -> v a -> v a
  infixl 6 -#
  u -# v = lcomb 1 u (-1) v
  (*#) :: (Num a) => a -> v a -> v a
  infixl 7 *#
  a *# v = lcomb a v 0 zero'
  (/#) :: (Fractional a) => v a -> a -> v a
  infixl 7 /#
  v /# a = (1/a) *# v
  negate' :: (Num a) => v a -> v a
  negate' v = (-1) *# v
  lcomb :: (Num a) => a -> v a -> a -> v a -> v a
  lcomb a u b v = a *# u +# b *# v
  lcombG :: (Num a) => [(a, v a)] -> v a
  lcombG = foldl' (\res (a,v) -> res +# a *# v) zero'

class (Vector v) => IPVector v where
  dot :: (Num a) => v a -> v a -> a
  (.#) :: (Num a) => v a -> v a -> a
  infixl 7 .#
  (.#) = dot
  sqLen :: (Num a) => v a -> a
  sqLen v = dot v v
  len :: (Floating a) => v a -> a
  len = sqrt . sqLen
  normalize :: (Floating a) => v a -> v a
  normalize v = v /# (len v)
  distance :: (Floating a) => v a -> v a -> a
  distance u v = len (u -# v)
  cos2Angle :: (Fractional a) => v a -> v a -> a
  cos2Angle u v = (dot u v)^2/((sqLen u)*(sqLen v))
  cosAngle :: (Floating a) => v a -> v a -> a
  cosAngle u v = (dot u v)/(sqrt $ (sqLen u) * (sqLen v))
  angle :: (Floating a) => v a -> v a -> a
  angle u v = acos $ cosAngle u v


class (IPVector v) => CVector v where
  basisVector :: (Num a) => Integer -> Maybe (v a)
  -- give an orthonormal basis, with respect to the dot product
  coord :: Integer -> v a -> Maybe a
  mapCoord :: Integer -> (a -> a) -> v a -> v a

class (Vector v) => Point p v | p -> v where
  difference :: (Num a) => p a -> p a -> v a
  translatePoint :: (Num a) => p a -> v a -> p a
  origin :: (Num a) => p a
  toVector :: (Num a) => p a -> v a
  toVector p = difference p origin
  fromVector :: (Num a) => v a -> p a
  fromVector v = translatePoint origin v

class (Vector m, Vector v) => Matrix m v | m -> v where
  transpose :: m a -> m a
  (*|) :: (Num a) => m a -> v a -> v a
  infixr 7 *|
  mat *| vec = vec |* (transpose mat)
  (|*) :: (Num a) => v a -> m a -> v a
  infixl 7 |*
  vec |* mat = (transpose mat) *| vec
  (*#*) :: (Num a) => m a -> m a -> m a
  

data Vec4 a
  = Vec4 a a a a
  deriving (Show, Eq, Read)

data Mat4 a
  = Mat4 (Vec4 a) (Vec4 a) (Vec4 a) (Vec4 a)
  | Diag4 a a a a
  deriving (Show, Eq, Read)

basicMat4 :: Vec4 a -> Vec4 a -> Vec4 a -> Vec4 a -> Mat4 a
basicMat4 = Mat4

diagMat4 :: a -> a -> a -> a -> Mat4 a
diagMat4 = Diag4

instance Vector Vec4 where
  zero' = Vec4 0 0 0 0
  dimension _ = 4
  (Vec4 a b c d) +# (Vec4 e f g h) = Vec4 (a+e) (b+f) (c+g) (d+h)
  (Vec4 a b c d) -# (Vec4 e f g h) = Vec4 (a-e) (b-f) (c-g) (d-h)
  a *# (Vec4 e f g h) = Vec4 (a*e) (a*f) (a*g) (a*h)

instance IPVector Vec4 where
  dot (Vec4 a b c d) (Vec4 e f g h) = a*e + b*f + c*g + d*h

instance CVector Vec4 where
  basisVector 1 = Just (Vec4 1 0 0 0)
  basisVector 2 = Just (Vec4 0 1 0 0)
  basisVector 3 = Just (Vec4 0 0 1 0)
  basisVector 4 = Just (Vec4 0 0 0 1)
  basisVector _ = Nothing
  coord 1 (Vec4 x _ _ _) = Just x
  coord 2 (Vec4 _ x _ _) = Just x
  coord 3 (Vec4 _ _ x _) = Just x
  coord 4 (Vec4 _ _ _ x) = Just x
  coord _ _ = Nothing
  mapCoord 1 f (Vec4 x b c d) = (Vec4 (f x) b c d)
  mapCoord 2 f (Vec4 a x c d) = (Vec4 a (f x) c d)
  mapCoord 3 f (Vec4 a b x d) = (Vec4 a b (f x) d)
  mapCoord 4 f (Vec4 a b c x) = (Vec4 a b c (f x))
  mapCoord _ _ v = v

diagToMat :: (Num a) => a -> a -> a -> a -> (Vec4 a, Vec4 a, Vec4 a, Vec4 a)
diagToMat sx sy sz sw = (Vec4 sx 0 0 0, Vec4 0 sy 0 0, Vec4 0 0 sz 0, Vec4 0 0 0 sw)

instance Vector Mat4 where
  zero' = Diag4 0 0 0 0
  dimension _ = 16
  (Mat4 a b c d) +# (Mat4 e f g h) = Mat4 (a+#e) (b+#f) (c+#g) (d+#h)
  (Mat4 (Vec4 a11 a21 a31 a41) (Vec4 a12 a22 a32 a42) (Vec4 a13 a23 a33 a43) (Vec4 a14 a24 a34 a44))
    +# (Diag4 x y z w) = 
      (Mat4
        (Vec4 (a11+x) a21 a31 a41)
        (Vec4 a12 (a22+y) a32 a42)
        (Vec4 a13 a23 (a33+z) a43)
        (Vec4 a14 a24 a34 (a44+w)))
  (Diag4 x y z w) 
    +# (Mat4 (Vec4 a11 a21 a31 a41) (Vec4 a12 a22 a32 a42) (Vec4 a13 a23 a33 a43) (Vec4 a14 a24 a34 a44)) = 
      (Mat4
        (Vec4 (x+a11) a21 a31 a41)
        (Vec4 a12 (y+a22) a32 a42)
        (Vec4 a13 a23 (z+a33) a43)
        (Vec4 a14 a24 a34 (w+a44)))
  (Diag4 a b c d) +# (Diag4 e f g h) = Diag4 (a+e) (b+f) (c+g) (d+h)
  (Mat4 a b c d) -# (Mat4 e f g h) = Mat4 (a-#e) (b-#f) (c-#g) (d-#h)
  (Mat4 (Vec4 a11 a21 a31 a41) (Vec4 a12 a22 a32 a42) (Vec4 a13 a23 a33 a43) (Vec4 a14 a24 a34 a44))
    -# (Diag4 x y z w) = 
      (Mat4
        (Vec4 (a11-x) a21 a31 a41)
        (Vec4 a12 (a22-y) a32 a42)
        (Vec4 a13 a23 (a33-z) a43)
        (Vec4 a14 a24 a34 (a44-w)))
  (Diag4 x y z w) 
    -# (Mat4 (Vec4 a11 a21 a31 a41) (Vec4 a12 a22 a32 a42) (Vec4 a13 a23 a33 a43) (Vec4 a14 a24 a34 a44)) = 
      (Mat4
        (Vec4 (x-a11) a21 a31 a41)
        (Vec4 a12 (y-a22) a32 a42)
        (Vec4 a13 a23 (z-a33) a43)
        (Vec4 a14 a24 a34 (w-a44)))
  (Diag4 a b c d) -# (Diag4 e f g h) = Diag4 (a-e) (b-f) (c-g) (d-h)
  a *# (Mat4 e f g h) = Mat4 (a*#e) (a*#f) (a*#g) (a*#h)
  a *# (Diag4 x y z w) = Diag4 (a*x) (a*y) (a*z) (a*w)
  
instance Matrix Mat4 Vec4 where
  transpose
    (Mat4
      (Vec4 a11 a12 a13 a14)
      (Vec4 a21 a22 a23 a24)
      (Vec4 a31 a32 a33 a34)
      (Vec4 a41 a42 a43 a44)) =
      (Mat4
        (Vec4 a11 a21 a31 a41)
        (Vec4 a12 a22 a32 a42)
        (Vec4 a13 a23 a33 a43)
        (Vec4 a14 a24 a34 a44))
  transpose m = m
  (Mat4 v1 v2 v3 v4) *| (Vec4 k1 k2 k3 k4) = (lcomb k1 v1 k2 v2) +# (lcomb k3 v3 k4 v4)
  (Diag4 x y z w) *| (Vec4 p q r s) = Vec4 (x*p) (y*q) (z*r) (w*s)
  v |* (Mat4 v1 v2 v3 v4) = Vec4 (v .# v1) (v .# v2) (v .# v3) (v .# v4)
  (Vec4 x y z w) |* (Diag4 p q r s) = Vec4 (x*p) (y*q) (z*r) (w*s)
  (Diag4 x y z w) *#* (Diag4 p q r s) = Diag4 (x*p) (y*q) (z*r) (w*s)
  m *#* (Mat4 v1 v2 v3 v4) = Mat4 (m *| v1) (m *| v2) (m *| v3) (m *| v4)
  (Mat4 x y z w) *#* (Diag4 p q r s) = (Mat4 (p *# x) (q *# y) (r *# z) (s *# w))



data Vec3 a
  = Vec3 a a a
  deriving (Show, Eq, Read)

instance Vector Vec3 where
  zero' = Vec3 0 0 0
  dimension _ = 3
  (Vec3 a b c) +# (Vec3 e f g) = Vec3 (a+e) (b+f) (c+g)
  (Vec3 a b c) -# (Vec3 e f g) = Vec3 (a-e) (b-f) (c-g)
  a *# (Vec3 e f g) = Vec3 (a*e) (a*f) (a*g)

instance IPVector Vec3 where
  dot (Vec3 a b c) (Vec3 e f g) = a*e + b*f + c*g

instance CVector Vec3 where
  basisVector 1 = Just (Vec3 1 0 0)
  basisVector 2 = Just (Vec3 0 1 0)
  basisVector 3 = Just (Vec3 0 0 1)
  basisVector _ = Nothing
  coord 1 (Vec3 x _ _) = Just x
  coord 2 (Vec3 _ x _) = Just x
  coord 3 (Vec3 _ _ x) = Just x
  coord _ _ = Nothing
  mapCoord 1 f (Vec3 x b c) = (Vec3 (f x) b c)
  mapCoord 2 f (Vec3 a x c) = (Vec3 a (f x) c)
  mapCoord 3 f (Vec3 a b x) = (Vec3 a b (f x))
  mapCoord _ _ v = v

data Point3 a 
  = Point3 a a a
  deriving (Show, Eq, Read)

instance Point Point3 Vec3 where
  difference (Point3 x y z) (Point3 x' y' z') = Vec3 (x-x') (y-y') (z-z')
  translatePoint (Point3 x y z) (Vec3 vx vy vz) = Point3 (x+vx) (y+vy) (z+vz)
  origin = Point3 0 0 0
  toVector (Point3 x y z) = Vec3 x y z
  fromVector (Vec3 x y z) = Point3 x y z

class (Vector v) => Homogenizable t v | t -> v where
  homogenize :: (Num a) => t a -> v a
  dehomogenize :: (Eq a, Fractional a) => v a -> Maybe (t a)

instance Homogenizable Vec3 Vec4 where
  homogenize (Vec3 x y z) = Vec4 x y z 0
  dehomogenize (Vec4 x y z 0) = Just $ Vec3 x y z -- dangerous
  dehomogenize _ = Nothing

instance Homogenizable Point3 Vec4 where
  homogenize (Point3 x y z) = Vec4 x y z 1
  dehomogenize (Vec4 _ _ _ 0) = Nothing -- dangerous
  dehomogenize (Vec4 x y z w) = Just $ Point3 (x/w) (y/w) (z/w)



