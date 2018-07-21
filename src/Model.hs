--
-- Model.hs
-- Copyright (C) 2016 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

{-# LANGUAGE RecordWildCards #-}

module Model
  ( Color
  , Position
  , Normal
  , Face (..)
  , Vertex (..)
  , VertexNormal (..)
  , Model (..)
  , BasicModel
  , Triangle (..)
  , cube
  , parallelogram
  , fromTriangles
  , vertexLists
  ) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import LinAlg
import Geometry

--import Graphics.Rendering.OpenGL 

type Position a = Point3 a
type Color a = Vec3 a
type Normal a = Vec3 a

pos :: (Floating a) => a -> a -> a -> Position a
pos = Point3

data Vertex r b = Vertex
  { position :: Position r
  , vertexData :: b
  }
  deriving (Eq, Show, Read)

data VertexNormal a
  = VertexNormal (Normal a)
  | FaceNormal 
  deriving (Eq, Show, Read)



-- counterclockwise order gives default normal?
-- a is vertex index type
data Face a f = Face 
  { vertex1 :: a -- vertex index
  , vertex2 :: a
  , vertex3 :: a
  , faceProperties :: f -- overall properties of the face?
  }
  deriving (Eq, Show, Read)

-- Collection of vertices labeled by whatnot
-- collection of triangles and possibly edges
-- a = vertex index type (e.g. Int)
-- r = float type
-- b = vertex model data (e.g. normals and whatnot)
-- c = vertex face data (e.g. color or whatever, possibly normals as well who knows)
-- f = overall face data (possibly a face normal, overall face color, etc.)
data Model a r b f = Model
  { vertices :: Map.Map a (Vertex r b)
  , faces :: [Face a f]
  }
  deriving (Eq, Show, Read)

type BasicModel a = Model Int a (Color a,VertexNormal a) ()

cube :: Floating a => BasicModel a
cube = Model
  { vertices = Map.fromList $ zip [0..7]
      [ Vertex 
          { position = pos (-1) (-1) (-1)
          , vertexData = (Vec3 0 0 0,FaceNormal)
          }
      , Vertex 
          { position = pos (1) (-1) (-1)
          , vertexData = (Vec3 1 0 0,FaceNormal)
          }
      , Vertex 
          { position = pos (-1) (1) (-1)
          , vertexData = (Vec3 0 1 0,FaceNormal)
          }
      , Vertex 
          { position = pos (1) (1) (-1)
          , vertexData = (Vec3 1 1 0,FaceNormal)
          }
      , Vertex 
          { position = pos (-1) (-1) (1)
          , vertexData = (Vec3 0 0 1,FaceNormal)
          }
      , Vertex 
          { position = pos (1) (-1) (1)
          , vertexData = (Vec3 1 0 1,FaceNormal)
          }
      , Vertex 
          { position = pos (-1) (1) (1)
          , vertexData = (Vec3 0 1 1,FaceNormal)
          }
      , Vertex 
          { position = pos (1) (1) (1)
          , vertexData = (Vec3 1 1 1,FaceNormal)
          }
      ]
  , faces =
      [ Face 0 2 3 ()
      , Face 3 1 0 ()
      , Face 0 4 6 ()
      , Face 6 2 0 ()
      , Face 0 1 5 ()
      , Face 5 4 0 ()
      , Face 7 6 4 ()
      , Face 4 5 7 ()
      , Face 7 5 1 ()
      , Face 1 3 7 ()
      , Face 7 3 2 ()
      , Face 2 6 7 ()
      ]
  }

withFaceNormal :: Normal a -> VertexNormal a -> Normal a
withFaceNormal _ (VertexNormal n) = n
withFaceNormal f _ = f

faceToVertices :: (Eq a, Floating a) => Map.Map Int (Vertex a (Color a, VertexNormal a)) -> Face Int () 
  -> Maybe [(Position a, Color a, Normal a)]
faceToVertices map Face{..} = do
  v1 <- Map.lookup vertex1 map
  v2 <- Map.lookup vertex2 map
  v3 <- Map.lookup vertex3 map
  let v1Pos = position v1
  let v2Pos = position v2
  let v3Pos = position v3
  faceNormal <- triangleNormal v1Pos v2Pos v3Pos
  let (v1Col, v1Norm) = vertexData v1
  let (v2Col, v2Norm) = vertexData v2
  let (v3Col, v3Norm) = vertexData v3
  let f = withFaceNormal faceNormal
  return
    [ (v1Pos, v1Col, f v1Norm)
    , (v2Pos, v2Col, f v2Norm)
    , (v3Pos, v3Col, f v3Norm)
    ]

data Triangle a = Triangle (Position a) (Position a) (Position a)

fromTriangles :: Color a -> [Triangle a] -> BasicModel a
fromTriangles = fromTrianglesHelper 0

fromTrianglesHelper :: Int -> Color a -> [Triangle a] -> BasicModel a
fromTrianglesHelper _ _ [] = Model { vertices = Map.empty, faces = [] }
fromTrianglesHelper n q ((Triangle p1 p2 p3):xs) = 
  let
    mkVert p = Vertex p (q,FaceNormal)
    rest = fromTrianglesHelper (n+3) q xs
    newV = Map.union (vertices rest) 
      . Map.fromList 
      . zip [n..(n+2)]
      $ map mkVert [p1, p2, p3]
    newF = (Face n (n+1) (n+2) ()):(faces rest)
  in
    Model 
    { vertices = newV
    , faces = newF
    }

-- extend a triangle to a parallelogram by reflecting p1 across p2-p3
parallelogram :: (Num a) => Color a -> Triangle a -> BasicModel a
parallelogram q t1@(Triangle p1 p2 p3) =
  let
    p4 = translatePoint p3 $ difference p2 p1
    t2 = Triangle p2 p4 p3
  in
    fromTriangles q [t1,t2]


-- because I don't want to set things up properly yet
-- list of vertices (pos,color,normal)
modelToLists :: (Eq a, Floating a) => BasicModel a -> [(Position a, Color a, Normal a)]
modelToLists Model{..} = do
  face <- faces
  fromMaybe [] $ faceToVertices vertices face 
  -- Don't need to display empty faces anyway, so just return empty list for those faces

-- Not well written, but this is temporary anyway so what the hell
modelLists :: [(Position a, Color a, Normal a)] -> ([a], [a], [a])
modelLists [] = ([], [], [])
modelLists ((Point3 p1 p2 p3,Vec3 c1 c2 c3,Vec3 n1 n2 n3):xs) =
  let
    (ps, cs, ns) = modelLists xs
  in 
    (p1:p2:p3:ps, c1:c2:c3:cs, n1:n2:n3:ns)

vertexLists :: (Eq a, Floating a) => BasicModel a -> ([a], [a], [a])
vertexLists = modelLists . modelToLists

--getVertexBuffer :: Model a b f -> 

