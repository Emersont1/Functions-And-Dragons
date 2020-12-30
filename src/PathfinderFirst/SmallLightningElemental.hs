module PathfinderFirst.SmallLightningElemental (smallLightningElemental, averageSmallLightningElemental) where

import Dice
import PathfinderFirst.Internal
import Roll

smallLightningElementalInternal :: Int -> Entity
smallLightningElementalInternal e =
  Conscious $
    Entity
      { entityName = "Small Lightning Elemental",
        hp = e,
        maxHP = e,
        defences =
          Defences
            { ac = 14,
              fortitude = 3,
              reflex = 5,
              will = 10
            },
        initiative = 6,
        attacks =
          [ Attack
              { attackName = "Slam",
                modifier = 5,
                damage = [OneOff None $ DiceValue 1 4 0, OneOff Electricity $ DiceValue 1 3 0],
                crit = 20,
                critMultiplier = 2
              }
          ],
        damageTraits = [Immune Electricity, Immune ElementalTraits]
      }

smallLightningElemental = fmap smallLightningElementalInternal $ 2 `d` 10

averageSmallLightningElemental = constant $ smallLightningElementalInternal 11
