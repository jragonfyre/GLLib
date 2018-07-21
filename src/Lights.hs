--
-- Lights.hs
-- Copyright (C) 2016 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

{-# LANGUAGE RecordWildCards #-}

module Lights
  ( Color
  , LightSource (..)
  , PointLLocation (..)
  , PlaneLLocation (..)
  , AngularDistribution (..)
  , LightType (..)
  , intensityAt
  ) where

import LinAlg
--import Color
--import Geometry

type Color a = Vec3 a


data LightSource a = LightSrc
  { color :: Color a -- The intensities of the various kinds of light
  , lightType :: LightType a -- governs the shape of the light and whatnot.
  }

-- PointLightLocation
-- 
data PointLLocation a = PointLLoc
  { location :: Point3 a -- location of the point
  , pointFalloff :: Maybe (a,a) -- radius and falloff of the lighting (constant at 1 below radius,
  -- falloff begins at radius), Nothing gives no falloff
  -- (not remotely physically accurate) essentially Just 0.0 0.0,
  -- but possibly more efficient. The maybe might be removed in the future.
  -- inverse square at radius r is (Just (r,-2.0))
  -- Since there shouldn't be any geometry less than the radius, this can maybe be changed to be more
  -- efficient. (particularly in the renderer)
  }

-- PlaneLightLocation
data PlaneLLocation a = PlaneLLoc
  { normal :: Vec3 a -- assumed to be a unit vector
  , distance :: Maybe a -- is it at infinity or a finite location?
  , planeFalloff :: Maybe a -- This should usually be 0. There is no falloff on an infinite plane of light
  }

data AngularDistribution a = AngularDist
  { centralDirection :: Vec3 a -- should be normalized
  , innerAngle :: a -- light is constant at maxintensity inside this angle
  , outerAngle :: a -- light is constant at 0 outside this angle
  , angularFalloff :: a -- interpolation between 0 and 1 depends on this exponent
  }


data LightType a
  = AmbientSrc
  | PointSrc (PointLLocation a)
  | DirectionalSrc (PlaneLLocation a)
  | ConeSrc (PointLLocation a) (AngularDistribution a)

--distanceToLight (Floating a) => LightSource a -> Point3 a -> 
  
-- you probably won't need this, but useful reference
intensityAt :: (Ord a, Floating a) => LightSource a -> Point3 a -> Color a
intensityAt LightSrc{..} pt = 
  let
    multiplier = case lightType of
      AmbientSrc ->
        1
      PointSrc PointLLoc{..} ->
        case pointFalloff of
          Nothing ->
            1
          Just (r,p) ->
            let
              objToL = difference pt location
              sLen = sqLen objToL -- len^2 (more efficient)
              r2 = r^2
              pow = -p/2 -- divide the exponent by 2 so we have a division and a
              -- power rather than a sqrt and power
            in
              if sLen < r2
              then
                1
              else
                (sLen/r2) ** pow
      DirectionalSrc PlaneLLoc{..} ->
        case distance of
          Nothing -> -- plane at infinity, falloff makes no sense
            1
          Just d ->
            let
              r = ((toVector pt) `dot` normal) - d -- here we assume normal is normalized
            in
              if r < 0
              then
                0
              else
                case planeFalloff of
                  Nothing ->
                    1
                  Just p ->
                    r ** (-p)
      ConeSrc PointLLoc{..} AngularDist{..} ->
        let
          lightToObj = difference location pt
          lobj = normalize lightToObj
          cosAng = lobj `dot` centralDirection
          cosOuter = cos $ outerAngle/2
        in
          if cosAng < cosOuter
          then
            0
          else
            let
              ptMult = case pointFalloff of
                Nothing ->
                  1
                Just (r,p) ->
                  let
                    sLen = sqLen lightToObj
                    r2 = r^2
                    pow = -p/2
                  in
                    if sLen < r2
                    then
                      1
                    else
                      (sLen/r2) ** pow
              cosInner = cos $ innerAngle/2
            in
              if cosAng > cosInner
              then
                ptMult
              else
                ptMult * (((cosAng - cosInner)/(cosOuter-cosInner)) ** angularFalloff)
  in
    multiplier *# color
