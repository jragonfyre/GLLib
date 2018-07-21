--
-- Program.hs
-- Copyright (C) 2016 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Tut6.Program
  (
  ) where

class UniformVar a where
  uniformVarName :: a -> String

