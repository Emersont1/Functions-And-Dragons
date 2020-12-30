module PathfinderFirst.Duel where

import Data.List
import Data.Ratio
import Debug.Trace
import Dice
import PathfinderFirst.Internal
import Roll
import Roll.Internal


data NextMove = First | Second deriving (Eq, Show)

invert First  = Second
invert Second = Second

data DuelState = OngoingDuel Entity Entity NextMove | FWon | FLost | BothDead | Unfinished deriving (Eq, Show)

inititiveOrderInternal :: Int -> Int -> Int -> Int -> [NextMove]
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
duel a b = flatten $! fmap (uncurry duelInternal) $mix a b

flipDuel :: DuelState -> DuelState
flipDuel FWon                = FLost
flipDuel FLost               = FWon
flipDuel BothDead            = BothDead
flipDuel (OngoingDuel x y z) = OngoingDuel y x $invert z

attack' :: [Damage] -> Entity -> Rolls Entity
attack' [] e = constant e
attack' (d : ds) e = fmap (uncurry dealDamage) (mix (attack' ds e) (diceValue $ amount d))

attackDice :: Attack -> Entity -> Rolls AttackResult
attackDice at e =
  fmapSimplify
    ( \r ->
        if r > crit at
          then Crit
          else if r + modifier at > ac (defences (getEnt e))
            then Hit
            else Miss
    )
    (d 1 20)

attackRoll :: Attack -> Entity -> AttackResult -> Rolls Entity
attackRoll at e Crit =  attack' (head (damage at) : damage at) e
attackRoll at e Hit  =  attack' (head (damage at) : damage at) e
attackRoll _ e _     =  constant e

damageValue :: Attack -> Entity -> Rolls Entity
damageValue at e = flatten $ fmap (attackRoll at e) (attackDice at e)

postAttack :: DuelState -> DuelState
postAttack (OngoingDuel (Conscious x) (Dead y) s) = FWon
postAttack (OngoingDuel (Dead x) (Conscious y) s) = FLost
postAttack (OngoingDuel (Dead x) (Dead y) s)      = FLost
postAttack x                                      = x

stepDuel :: Rolls DuelState -> Rolls DuelState
stepDuel ds = flatten $! fmap stepDuel' ds

stepDuel' :: DuelState -> Rolls DuelState
stepDuel' (OngoingDuel x y m) = normalise $ Rolls [Roll v p | (Roll v p) <- r2, v /= ds]
  where
    ds = OngoingDuel x y m
    r1 = simplify $! stepDuel'' ds
    (Rolls r2) = flatten $! fmap stepDuel'' r1
stepDuel' x = constant x

stepDuel'' :: DuelState -> Rolls DuelState
stepDuel'' (OngoingDuel x y s)
  | s == First =  fmap (\ydam -> postAttack $ OngoingDuel x ydam Second) (damageValue xAttack y)
  | s == Second = fmap (\xdam -> postAttack $ OngoingDuel xdam y First) (damageValue yAttack x)
  where
    yAttack = head $attacks $getEnt y
    xAttack = head $attacks $getEnt x
stepDuel'' x = constant x

lastRound :: DuelState -> DuelState
lastRound OngoingDuel {} = Unfinished
lastRound x              = x

findWinner :: Integer -> Rolls DuelState -> Rolls DuelState
findWinner n duel = fmapSimplify lastRound (findWinner' n duel)

syncopatedStep :: DuelState -> Rolls DuelState
syncopatedStep (OngoingDuel x y First) = stepDuel' (OngoingDuel x y First)
syncopatedStep x                       = constant x

findWinner' :: Integer -> Rolls DuelState -> Rolls DuelState
findWinner' x duel = findWinner'' x $! flatten $! fmap syncopatedStep duel

findWinner'' :: Integer -> Rolls DuelState -> Rolls DuelState
findWinner'' 0 duel = duel
findWinner'' x duel = findWinner'' (x -1) (stepDuel duel)
