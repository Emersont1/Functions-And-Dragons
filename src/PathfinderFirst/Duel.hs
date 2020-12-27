module PathfinderFirst.Duel where

import Dice
import PathfinderFirst.Internal
import Roll
import Roll.Internal

data NextMove = First | Second deriving (Eq, Show)

invert First = Second
invert Second = Second

data DuelState = OngoingDuel Entity Entity NextMove | FWon | FLost | BothDead deriving (Eq, Show)

inititiveOrderInternal :: Integer -> Integer -> Integer -> Integer -> [NextMove]
inititiveOrderInternal xinit yinit x y
  | x + xinit > y + yinit = [First]
  | x + xinit < y + yinit = [Second]
  | xinit > yinit = [First]
  | xinit < yinit = [Second]
  | otherwise = [] -- Probably should change this, but I don't want an infinite loop

initiativeOrder :: Entity -> Entity -> Rolls NextMove
initiativeOrder a b =
  normalise $
    simplify $
      Rolls $concat $
        map
          ( \(Roll v p) ->
              map (`Roll` p) (uncurry (inititiveOrderInternal (initiative $getEnt a) (initiative $getEnt b)) v)
          )
          $ (\(Rolls x) -> x) $ mix (1 `d` 20) (1 `d` 20)

duelInternal :: Entity -> Entity -> Rolls DuelState
duelInternal a b = fmap (OngoingDuel a b) (initiativeOrder a b)

duel :: Rolls Entity -> Rolls Entity -> Rolls DuelState
duel a b = flatten $ fmap (uncurry duelInternal) $mix a b

flipDuel FWon = FLost
flipDuel FLost = FWon
flipDuel BothDead = BothDead
flipDuel (OngoingDuel x y z) = OngoingDuel y x $invert z

doDamage :: Integer -> Entity -> attack -> Entity
doDamage roll x att = doDamage' (tail $damage att) x

doDamage' :: [Damage] -> Entity -> Entity
doDamage' [] x = x
doDamage' dam:ds x = dealDamage x 

damageValue :: Attack -> Entity -> Dice -> Rolls Entity
damageValue attack x dmg =
  fmap
    ( \(roll, dam) ->
        if (roll + modifier attack) < (ac (defences x))
          then doDamage roll x attack
          else x
    )
    (mix (1 `d` 20) dmg)

stepDuel :: DuelState -> Rolls DuelState
stepDuel (OngoingDuel x y s)
  | s == First = fmap \(dam -> OngoingDuel x y  Second )  (damageValue (modifier _attack) (ac(defences y)) ( damage _attack))
      where _attack = head $attacks x
  | s == Second = fmap flipDuel $stepDuel $ OngoingDuel y x First
stepDuel x = Rolls [Roll x ((%) 1 1)]
