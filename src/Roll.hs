module Roll where

import Data.List

data Roll a = Value a Rational

_prob :: Roll a -> Rational
_prob (Value _ x) = x

_val :: Roll a -> a
_val (Value x _) = x

newtype Rolls a = RollCollection [Roll a]

instance Functor Rolls  where
    fmap f (RollCollection xs) = RollCollection $ map (\(Value x y) -> Value (f x) y) xs

uniqueValues :: (Eq a) => Rolls a -> [a]
uniqueValues (RollCollection rs) = nub $ map _val rs

probability ::(Eq a) => Rolls a -> a -> Rational 
probability (RollCollection rs) v = sum $ map _prob $ filter (\x -> v == _val x) rs

simplify :: (Eq a) => Rolls a -> Rolls a
simplify rs = RollCollection $ map (\r ->
     Value r $ probability  rs r)
     $ uniqueValues rs

mix :: (a-> b-> c) -> Rolls a -> Rolls b -> Rolls c
mix f (RollCollection xs) (RollCollection ys) = RollCollection [Value (f xv yv) $xp*yp | (Value xv xp)<- xs, (Value yv yp)<-ys]