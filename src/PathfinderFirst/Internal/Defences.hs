module PathfinderFirst.Internal.Defences where

data Defence = Fortitude | Reflex | Will deriving (Show, Eq)

data Defences = Defences
  { ac :: Int,
    fortitude :: Int,
    reflex :: Int,
    will :: Int
  }
  deriving (Show, Eq)
