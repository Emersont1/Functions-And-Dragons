module Dice where
import Roll
import Data.Ratio
import Data.Bifunctor
-- data Roll a = Value a Rational
type Dice = Rolls Integer


d :: Integer -> Integer -> Dice
d 1 f = RollCollection $map (\x-> Value x ((%) 1 f)) [1..f] 
d n f 
    | even n = simplify $ mix (+) ((n `div` 2)`d` f) ((n `div` 2)`d` f)
    | odd n  = simplify $ mix (+) ((n-1)`d` f) (1 `d` f)

p :: Dice -> Integer -> Dice
p d n = simplify $ fmap (\x-> max (n + x) 0) d

-- Same as above, just permits negative values
pNeg :: Dice -> Integer -> Dice
pNeg d n = simplify $ fmap (+n) d

_combs :: [Dice] -> [([Integer], [Rational])]
_combs ((RollCollection x):xs) = concatMap (\(Value u v) -> map ( bimap (u:) (v:) )(_combs xs)) x

