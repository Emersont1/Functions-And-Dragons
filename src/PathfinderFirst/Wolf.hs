module PathfinderFirst.Wolf (wolf, averageWolf) where

import Dice
import PathfinderFirst.Internal
import Roll

wolfInternal :: Int -> Entity
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
        attacks =
          [ Attack
              { attackName = "Bite",
                modifier = 2,
                damage = [OneOff None $ DiceValue 1 6 1 ],
                crit = 20,
                critMultiplier = 2
              }
          ],
        damageTraits = []
      }

wolf = fmap wolfInternal $ 2 `d` 8 `p` 4

averageWolf = constant $ wolfInternal 13
