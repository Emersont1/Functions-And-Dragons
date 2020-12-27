module PathfinderFirst.Internal.Defences where

data Defence = Fortitude | Reflex | Will deriving (Show, Eq)

data Defences = Defences
  { ac :: Integer,
    fortitude :: Integer,
    reflex :: Integer,
    will :: Integer
  }
  deriving (Show, Eq)