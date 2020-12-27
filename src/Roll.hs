module Roll (Rolls (Rolls), uniqueValues, probability, maxValue, minValue, simplify, mix, mixFmap, fmapSimplify, flatten, normalise) where

import Data.List
import Roll.Internal

-- It requires Eq for equality when attacks list damage
newtype Rolls a = Rolls [Roll a] deriving (Eq)

instance Functor Rolls where
  fmap f (Rolls xs) = Rolls $ map (\(Roll x y) -> Roll (f x) y) xs

instance Show a => Show (Rolls a) where
  show (Rolls xs) = "(" ++ intercalate ", " (map show xs) ++ ")"

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
mix (Rolls xs) (Rolls ys) =
  Rolls
    [ Roll (xv, yv) $xp * yp
      | (Roll xv xp) <- xs,
        (Roll yv yp) <- ys
    ]

mix3 :: Rolls a -> Rolls b -> Rolls c -> Rolls (a, b, c)
mix3 (Rolls xs) (Rolls ys) (Rolls zs) =
  Rolls
    [ Roll (xv, yv, zv) $xp * yp * zp
      | (Roll xv xp) <- xs,
        (Roll yv yp) <- ys,
        (Roll zv zp) <- zs
    ]

mixFmap :: (a -> b -> c) -> Rolls a -> Rolls b -> Rolls c
mixFmap f xs ys = fmap (uncurry f) (mix xs ys)

fmapSimplify :: Eq b => (a -> b) -> Rolls a -> Rolls b
fmapSimplify f xs = simplify $ fmap f xs

-- last 2 functions designed for infinite loop avoidance

flatten :: Rolls (Rolls a) -> Rolls a
flatten (Rolls x) = Rolls $ concatMap (\(Roll (Rolls vs) p1) -> map (\(Roll u p2) -> Roll u $p1 * p2) vs) x

normalise :: Rolls a -> Rolls a
normalise (Rolls xs) = Rolls $ map (\(Roll v p) -> Roll v (p / pSum)) xs
  where
    pSum = sum $map (\(Roll v p) -> p) xs