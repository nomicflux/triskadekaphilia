module TDP.Synth.Play where

import           System.Exit            (ExitCode)

import qualified Synthesizer.Plain.Play as Play

import           TDP.Note
import           TDP.Tuning

getToneParam :: Double -> Double -> Double
getToneParam rate tone = tone * 2.0 * pi / rate

playTones :: Double -> [Double] -> IO ExitCode
playTones sec tones = Play.monoToInt16 rate freqs
  where
    rate = 44100::Double
    k tone = getToneParam rate tone
    end = 1000.0 * sec * 2.0 * pi
    mapper tone = map sin [0::Double,(k tone)..end]
    (sine1 : sines) = mapper <$> tones
    freqs = foldl (zipWith (+)) sine1 sines

playNotes :: [Note] -> IO ExitCode
playNotes notes = playTones 1.0 $ runNote JustIntonation 440.0 <$> notes
