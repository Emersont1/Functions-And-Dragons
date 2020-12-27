module Roll.Internal where

data Roll a = Roll a Rational deriving(Eq)

instance Show a => Show (Roll a) where
    show (Roll v p) = "("++ show v ++", "++ show p++")"


_prob :: Roll a -> Rational
_prob (Roll _ x) = x

_val :: Roll a -> a
_val (Roll x _) = x
