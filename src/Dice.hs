module Dice where
import Roll
import RollInternal
import Data.Ratio
import Data.Bifunctor


type Dice = Rolls Integer

d :: Integer -> Integer -> Dice
d 1 f = RollCollection $map (\x-> Value x ((%) 1 f)) [1..f] 
d n f 
    | even n = simplify $ mixFmap (+) ((n `div` 2)`d` f) ((n `div` 2)`d` f)
    | odd n  = simplify $ mixFmap (+) ((n-1)`d` f) (1 `d` f)

p :: Dice -> Integer -> Dice
p d n = simplify $ fmap (\x-> Prelude.max (n + x) 0) d

-- Same as above, just permits negative values
pNeg :: Dice -> Integer -> Dice
pNeg d n = simplify $ fmap (+n) d

empty :: a -> [a]
empty x = tail [x]


_combs :: [Dice] -> Rolls [Integer]
_combs [] = RollCollection [Value (empty 1) ((%)1 1)]
_combs (x:xs) = mixFmap (:) x $_combs xs 
