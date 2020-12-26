module Entity.Wolf(wolf) where
import Entity.Internal
import Dice

wolfInternal :: Integer->Entity
wolfInternal e =  Conscious $ Entity { name = "Wolf",
hp = e,
maxHP = e,
ac = 16,
initiative =2
}
