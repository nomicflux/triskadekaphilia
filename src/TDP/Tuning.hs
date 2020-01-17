module TDP.Tuning where

import           Data.Ratio (denominator, numerator)

import           TDP.Note

data Tuning = JustIntonation | EqualTemperament

runNoteRatio :: Double -> Note -> Double
runNoteRatio base (Note ratio _) =
  base * (fromInteger $ numerator ratio) / (fromInteger $ denominator ratio)

runNoteCents :: Double -> Note -> Double
runNoteCents base (Note _ cents) =
  base * (2.0 ** (cents / 1200.0))

runNote :: Tuning -> Double -> Note -> Double
runNote JustIntonation   = runNoteRatio
runNote EqualTemperament = runNoteCents
