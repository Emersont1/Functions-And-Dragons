module Roll (Rolls (Rolls), uniqueValues, probability, maxValue, minValue, simplify, mix, mixFmap, fmapSimplify) where

import Roll.Internal
import Data.List

newtype Rolls a = Rolls [Roll a]

instance Functor Rolls where
  fmap f (Rolls xs) = Rolls $ map (\(Roll x y) -> Roll (f x) y) xs

instance Show a => Show (Rolls a) where
    show (Rolls xs) = "(" ++ intercalate ", " (map show  xs)++ ")"

uniqueValues :: (Eq a) => Rolls a -> [a]
uniqueValues (Rolls rs) = nub $ map _val rs

probability :: (Eq a) => Rolls a -> a -> Rational
probability (Rolls rs) v = sum $ map _prob $ filter (\x -> v == _val x) rs

maxValue :: (Ord a) => Rolls a -> a
maxValue (Rolls rs) = maximum $ map _val rs

minValue :: (Ord a) => Rolls a -> a
minValue (Rolls rs) = minimum $ map _val rs

simplify :: (Eq a) => Rolls a -> Rolls a
simplify rs =
  Rolls $
    map
      ( \r ->
          Roll r $ probability rs r
      )
      $ uniqueValues rs

mix :: Rolls a -> Rolls b -> Rolls (a, b)
mix (Rolls xs) (Rolls ys) = Rolls [Roll (xv, yv) $xp * yp | (Roll xv xp) <- xs, (Roll yv yp) <- ys]

mixFmap :: (a -> b -> c) -> Rolls a -> Rolls b -> Rolls c
mixFmap f xs ys = fmap (uncurry f) (mix xs ys)

fmapSimplify :: Eq b => (a -> b) -> Rolls a -> Rolls b
fmapSimplify f xs = simplify $ fmap f xs

