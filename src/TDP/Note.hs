module TDP.Note where

import           Data.Hashable (Hashable, hash, hashWithSalt)
import           Data.Ratio

data Note = Note { noteRatio :: Ratio Integer
                 , noteCents :: Double }
    deriving (Eq)

instance Hashable Note where
  hashWithSalt salt (Note ratio cents) = (salt * (hash ratio + hash cents)) `mod` salt

instance Show Note where
  show (Note ratio cents) = concat $ ["Note <"
                                     , show $ numerator ratio
                                     , "/"
                                     , show $ denominator ratio
                                     , " : "
                                     , show cents
                                     , ">"
                                     ]

mkNote :: Integer -> Integer -> Double -> Note
mkNote = (Note .) . (%)

c :: Note
c = mkNote 1 1 0.0

c_s :: Note
c_s = mkNote 27 25 133.24

d_f :: Note
d_f = c_s

d :: Note
d = mkNote 25 21 301.85

e :: Note
e = mkNote 9 7 435.08

f :: Note
f = mkNote 7 5 582.51

f_s :: Note
f_s = mkNote 75 49 736.95

g_f :: Note
g_f = f_s

g :: Note
g = mkNote 5 3 884.36

h :: Note
h = mkNote 9 5 1017.60

h_s :: Note
h_s = mkNote 49 25 1165.02

j_f :: Note
j_f = h_s

j :: Note
j = mkNote 15 7 1319.44

a :: Note
a = mkNote 7 3 1466.87

a_s :: Note
a_s = mkNote 63 25 1600.11

b_f :: Note
b_f = a_s

b :: Note
b = mkNote 25 9 1768.72

shiftTritaveUp :: Note -> Note
shiftTritaveUp (Note ratio cents) =
  mkNote (numerator ratio * 3) (denominator ratio) (cents + 1901.96)

shiftTritaveDown :: Note -> Note
shiftTritaveDown (Note ratio cents) =
  mkNote (numerator ratio) (denominator ratio * 3) (cents - 1901.96)

unison :: Note
unison = c

greatLimma :: Note
greatLimma = c_s

quasiTemperedMajorThird :: Note
quasiTemperedMajorThird = d

septimalMajorThird :: Note
septimalMajorThird = e

lesserSeptimalTritone :: Note
lesserSeptimalTritone = f

fifth :: Note
fifth = f_s

justMajorSixth :: Note
justMajorSixth = g

greaterJustMinorSeventh :: Note
greaterJustMinorSeventh = h

eighth :: Note
eighth = h_s

septimalMinorNinth :: Note
septimalMinorNinth = j

septimalMinimalTenth :: Note
septimalMinimalTenth = a

quasiTemperedMajorTenth :: Note
quasiTemperedMajorTenth = a_s

classicAugmentedEleventh :: Note
classicAugmentedEleventh = b

justTwelfth :: Note
justTwelfth = shiftTritaveUp c
