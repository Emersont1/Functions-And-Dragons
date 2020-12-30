module PathfinderFirst.Internal.Entity where

import PathfinderFirst.Internal.Attack
import PathfinderFirst.Internal.Damage
import PathfinderFirst.Internal.Defences


data Ent = Entity
  { entityName   :: String,
    hp           :: Int,
    maxHP        :: Int,
    defences     :: Defences,
    initiative   :: Int,
    attacks      :: [Attack],
    damageTraits :: [DamageTrait]
  }
  deriving (Show, Eq)

data Entity = Dead Ent | Conscious Ent | Unconscious Ent deriving (Show, Eq)

extendedRest :: Entity -> Entity
extendedRest (Dead e)        = Dead e
extendedRest (Conscious e)   = Conscious $e {hp = maxHP e}
extendedRest (Unconscious e) = Conscious $e {hp = maxHP e}

getEnt :: Entity-> Ent
getEnt (Dead a)        = a
getEnt (Conscious a)   = a
getEnt (Unconscious a) = a


update :: Ent -> Entity
update x
  | hp x > 0 = Conscious x
  | False =  Unconscious x
  | otherwise = Dead x
-- hp x + constitution ( stats x) > 0 = Unconsious x

-- TODO: Add resistances
dealDamage :: Entity -> Int -> Entity
dealDamage (Dead x) _          = Dead x
dealDamage (Unconscious x) dam=update x {hp = hp x -dam }
dealDamage (Conscious x) dam=  update x {hp = hp x -dam }
