module PathfinderFirst.Internal.Size where

data Shape = Tall | Long

data Size
  = Fine
  | Diminuitive
  | Tiny
  | Small
  | Medium
  | Large Shape
  | Huge Shape
  | Gargantuan Shape
  | Colossal Shape

sizeModifier :: Size -> Integer
sizeModifier Fine = 8
sizeModifier Diminuitive = 4
sizeModifier Tiny = 2
sizeModifier Small = 1
sizeModifier Medium = 0
sizeModifier (Large _) = -1
sizeModifier (Huge _) = -2
sizeModifier (Gargantuan _) = -4
sizeModifier (Colossal _) = -8

specialSizeModifier :: Size -> Integer
specialSizeModifier x = negate $ sizeModifier x

flySizeModifier :: Size -> Integer
flySizeModifier Fine = 8
flySizeModifier Diminuitive = 6
flySizeModifier Tiny = 4
flySizeModifier Small = 2
flySizeModifier Medium = 0
flySizeModifier (Large _) = -2
flySizeModifier (Huge _) = -4
flySizeModifier (Gargantuan _) = -6
flySizeModifier (Colossal _) = -8

stealthSizeModifier :: Size -> Integer
stealthSizeModifier Fine = 16
stealthSizeModifier Diminuitive = 12
stealthSizeModifier Tiny = 8
stealthSizeModifier Small = 4
stealthSizeModifier Medium = 0
stealthSizeModifier (Large _) = -4
stealthSizeModifier (Huge _) = -8
stealthSizeModifier (Gargantuan _) = -12
stealthSizeModifier (Colossal _) = -16
