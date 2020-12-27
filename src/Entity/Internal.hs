module Entity.Internal where

import Dice

data Attack = Attack
  { attackName :: String,
    modifier :: Integer,
    damage :: Dice,
    -- Roll value (without mods) that if higher its a crit
    crit :: Integer
  }
  deriving (Show, Eq)

data Ent = Entity
  { entityName :: String,
    hp :: Integer,
    maxHP :: Integer,
    ac :: Integer,
    initiative :: Integer,
    attacks :: [Attack]
  }
  deriving (Show, Eq)

data Entity = Dead | Conscious Ent | Unconscious Ent deriving (Show)

extendedRest :: Entity -> Entity
extendedRest Dead = Dead
extendedRest (Conscious e) = Conscious $e {hp = maxHP e}
extendedRest (Unconscious e) = Conscious $e {hp = maxHP e}