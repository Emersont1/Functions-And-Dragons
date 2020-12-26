module RollInternal where

data Roll a = Value a Rational

instance Show a => Show (Roll a) where
    show (Value v p) = "("++ show v ++", "++ show p++")"


_prob :: Roll a -> Rational
_prob (Value _ x) = x

_val :: Roll a -> a
_val (Value x _) = x
