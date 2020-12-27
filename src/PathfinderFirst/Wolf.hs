module PathfinderFirst.Wolf (wolf) where

import Dice
import PathfinderFirst.Internal

wolfInternal :: Integer -> Entity
wolfInternal e =
  Conscious $
    Entity
      { entityName = "Wolf",
        hp = e,
        maxHP = e,
        defences =
          Defences
            { ac = 14,
              fortitude = 5,
              reflex = 5,
              will = 1
            },
        initiative = 2,
        attacks = [Attack {attackName = "Bite", modifier = 2, damage = 1 `d` 6 `p` 1, crit = 20, critMultiplier = 2}]
      }

wolf = fmap wolfInternal $ 2 `d` 8 `p` 4