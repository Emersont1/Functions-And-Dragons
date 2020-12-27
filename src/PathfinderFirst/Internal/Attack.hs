module PathfinderFirst.Internal.Attack where
import Dice

data Attack = Attack
  { attackName :: String,
    modifier :: Integer,
    damage :: Dice,
    -- Roll value (without mods) that if higher its a crit.
    -- Example: 19-20x2 would be crit = 19, critMultiplier = 2
    crit :: Integer,
    critMultiplier :: Integer
  }
  deriving (Show, Eq)