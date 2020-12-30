module Dice where

import Data.Bifunctor
import Data.Ratio
import Roll
import Roll.Internal

type Dice = Rolls Int

d :: Int -> Int -> Dice
d 1 f = Rolls $!map (\x -> Roll x ((%) 1 (fromIntegral f))) [1 .. f]
d n f
  | even n = simplify $! mixFmap (+) ((n `div` 2) `d` f) ((n `div` 2) `d` f)
  | odd n = simplify $! mixFmap (+) ((n -1) `d` f) (1 `d` f)

p :: Dice -> Int -> Dice
p d n = simplify $ fmap (\x -> Prelude.max (n + x) 0) d

-- Same as above, just permits negative values
pNeg :: Dice -> Int -> Dice
pNeg d n = simplify $ fmap (+ n) d

_combs :: [Dice] -> Rolls [Int]
_combs []       = Rolls [Roll [] (1 % 1)]
_combs (x : xs) = mixFmap (:) x $_combs xs
