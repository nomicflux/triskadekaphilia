module TDP.Note where

import Prelude
import Data.Tuple (Tuple(..))
import Math (pow)

data Tuning = JustIntonation | EqualTemperment

data Note = Note { ratio :: Tuple Number Number
                 , cents :: Number
                 }

derive instance eqNote :: Eq Note

instance showNote :: Show Note where
  show (Note note) = let Tuple num denom = note.ratio in
    "Note " <> (show num) <> "/" <> (show denom) <> " : " <> (show note.cents)

runNote :: Tuning -> Note -> Number -> Number
runNote JustIntonation = runNoteRatio
runNote EqualTemperment = runNoteCents

runNoteRatio :: Note -> Number -> Number
runNoteRatio (Note note) base = let Tuple num denom = note.ratio in
  base * num / denom

runNoteCents :: Note -> Number -> Number
runNoteCents (Note note) base = let cents = note.cents in
  base * pow 2.0 (cents / 1200.0)

mkNote :: Number -> Number -> Number -> Note
mkNote num denom cents = Note { ratio: Tuple num denom
                              , cents: cents
                              }

c :: Note
c = mkNote 1.0 1.0 0.0

d :: Note
d = mkNote 25.0 21.0 301.85

e :: Note
e = mkNote 9.0 7.0 435.08

f :: Note
f = mkNote 7.0 5.0 582.51

g :: Note
g = mkNote 5.0 3.0 884.36

h :: Note
h = mkNote 9.0 5.0 1017.60

j :: Note
j = mkNote 15.0 7.0 1319.44

a :: Note
a = mkNote 7.0 3.0 1466.87

b :: Note
b = mkNote 25.0 9.0 1768.72

shiftTritaveUp :: Note -> Note
shiftTritaveUp (Note note) =
  let Tuple num denom = note.ratio in
  mkNote (num * 3.0) denom (note.cents + 1901.96)

shiftTritaveDown :: Note -> Note
shiftTritaveDown (Note note) =
  let Tuple num denom = note.ratio in
  mkNote num (denom * 3.0) (note.cents - 1901.96)
