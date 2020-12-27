module PathfinderFirst.Duel where

import Data.Ratio
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

doDamage :: Integer -> Entity -> Attack -> Rolls Entity
doDamage roll x att =
  fmap
    ( \atk ->
        doDamage' (tail atk) y
    )
    (_combs $map amount $ damage att)
  where
    y = x

doDamage' :: [Integer] -> Entity -> Entity
doDamage' [] x = x
doDamage' (dam : ds) x = doDamage' ds $dealDamage x dam

damageValue :: Attack -> Entity -> Rolls Entity
damageValue attack x =
  simplify $ flatten $ fmap
    ( \roll ->
        if (roll + modifier attack) < ac (defences $getEnt x)
          then doDamage roll x attack
          else Rolls [Roll x ((%) 1 1)]
    )
    (1 `d` 20)

stepDuel :: DuelState -> Rolls DuelState
stepDuel (OngoingDuel x y s)
  | s == First = fmap (\ydam -> OngoingDuel x ydam Second) (damageValue _attack y)
  | s == Second = fmap flipDuel $stepDuel $ OngoingDuel y x First
  where
    _attack = head $attacks $getEnt x
stepDuel x = Rolls [Roll x ((%) 1 1)]
