module PathfinderFirst.Internal.Entity where

import PathfinderFirst.Internal.Attack
import PathfinderFirst.Internal.Defences


data Ent = Entity
  { entityName :: String,
    hp :: Integer,
    maxHP :: Integer,
    defences :: Defences,
    initiative :: Integer,
    attacks :: [Attack]
  }
  deriving (Show, Eq)

data Entity = Dead | Conscious Ent | Unconscious Ent deriving (Show)

extendedRest :: Entity -> Entity
extendedRest Dead = Dead
extendedRest (Conscious e) = Conscious $e {hp = maxHP e}
extendedRest (Unconscious e) = Conscious $e {hp = maxHP e}