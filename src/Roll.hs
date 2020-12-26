module Roll (Rolls (RollCollection), uniqueValues, probability, maxValue, minValue, simplify, mix, mixFmap, fmapSimplify) where

import RollInternal
import Data.List

newtype Rolls a = RollCollection [Roll a]

instance Functor Rolls where
  fmap f (RollCollection xs) = RollCollection $ map (\(Value x y) -> Value (f x) y) xs

instance Show a => Show (Rolls a) where
    show (RollCollection xs) = "(" ++ intercalate ", " (map show  xs)++ ")"

uniqueValues :: (Eq a) => Rolls a -> [a]
uniqueValues (RollCollection rs) = nub $ map _val rs

probability :: (Eq a) => Rolls a -> a -> Rational
probability (RollCollection rs) v = sum $ map _prob $ filter (\x -> v == _val x) rs

maxValue :: (Ord a) => Rolls a -> a
maxValue (RollCollection rs) = maximum $ map _val rs

minValue :: (Ord a) => Rolls a -> a
minValue (RollCollection rs) = minimum $ map _val rs

simplify :: (Eq a) => Rolls a -> Rolls a
simplify rs =
  RollCollection $
    map
      ( \r ->
          Value r $ probability rs r
      )
      $ uniqueValues rs

mix :: Rolls a -> Rolls b -> Rolls (a, b)
mix (RollCollection xs) (RollCollection ys) = RollCollection [Value (xv, yv) $xp * yp | (Value xv xp) <- xs, (Value yv yp) <- ys]

mixFmap :: (a -> b -> c) -> Rolls a -> Rolls b -> Rolls c
mixFmap f xs ys = fmap (uncurry f) (mix xs ys)

fmapSimplify :: Eq b => (a -> b) -> Rolls a -> Rolls b
fmapSimplify f xs = simplify $ fmap f xs

