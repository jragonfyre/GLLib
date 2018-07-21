--
-- Types.hs
-- Copyright (C) 2016 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Tut6.Types
  ( ProgramInfo (..)
  ) where


data ProgramInfo = ProgInfo
  { vertexShader :: String
  , geometryShader :: Maybe String
  , fragmentShader :: String
  }
