module Entity.Wolf (wolf) where

import Dice
import Entity.Internal

wolfInternal :: Integer -> Entity
wolfInternal e =
  Conscious $
    Entity
      { entityName = "Wolf",
        hp = e,
        maxHP = e,
        ac = 14,
        initiative = 2,
        attacks = [Attack {attackName = "Bite", modifier = 2, damage = 1 `d` 6 `p` 1, crit = 20}]
      }

wolf = fmap wolfInternal $ 2 `d` 8 `p` 4