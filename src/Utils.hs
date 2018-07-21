--
-- Utils.hs
-- Copyright (C) 2016 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Utils
  ( foldl'
  ) where

foldl' :: (res -> a -> res) -> res -> [a] -> res
foldl' f res [] = res
foldl' f res (x : xs) = let res' = res `f` x
                        in seq res' $ foldl' f res' xs

