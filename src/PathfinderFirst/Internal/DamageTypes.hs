module PathfinderFirst.Internal.DamageTypes where

import Dice
import PathfinderFirst.Internal.Defences

data DamageType
  = Slashing
  | Bludgeoning
  | Piercing
  | Fire
  | Cold
  | Electricity
  | Thunder
  | Poison
  | Necrotic
  | Radiant
  | Psychic
  | Force
  | Acid
  deriving (Show, Eq)

data Damage
  = OneOff
      { damageType :: DamageType,
        amount :: Dice
      }
  | Ongoing
      { damageType :: DamageType,
        amount :: Dice,
        save :: Integer,
        skill :: Defence
      }
  deriving (Show, Eq)