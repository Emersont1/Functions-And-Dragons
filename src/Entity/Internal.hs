module Entity.Internal where

data Ent = Entity
  { name :: String,
    hp :: Integer,
    maxHP :: Integer,
    ac :: Integer,
    initiative :: Integer
  }
  deriving (Show, Eq)

data Entity = Dead | Conscious Ent | Unconscious Ent

extendedRest :: Entity -> Entity
extendedRest Dead = Dead
extendedRest (Conscious e) = Conscious $e{hp = maxHP e}
extendedRest (Unconscious e) = Conscious $e{hp = maxHP e}