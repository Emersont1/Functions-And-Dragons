module PathfinderFirst.Internal.Damage where

import Dice
import PathfinderFirst.Internal.Defences

data DamageType
  = None
  | Slashing
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
  | ElementalTraits -- Not sure what this is
  deriving (Show, Eq)

data DiceValue = DiceValue Int Int Int deriving(Eq, Show)
diceValue :: DiceValue -> Dice
diceValue (DiceValue n x v) = n`d`x`p`v

data Damage
  = OneOff
      { damageType :: DamageType,
        amount :: DiceValue
      }
--  | OngoingDamage
--      { damageType :: DamageType,
--        amount :: Dice,
--        save :: Integer,
--        skill :: Defence
--      }
  deriving (Show, Eq)

data DamageTrait
  = Immune DamageType
  | Resistance DamageType Integer
  | Weakness DamageType
  deriving(Show, Eq)
