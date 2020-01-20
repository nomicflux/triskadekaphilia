module TDP.Synth.Play where

import           System.Exit            (ExitCode)

import qualified Synthesizer.Plain.Play as Play

import           Pipes                  (Consumer, await, for, lift, yield,
                                         (>->))
import qualified Pipes                  as P
import qualified Pipes.Prelude          as P

import           TDP.Note
import           TDP.Tuning

getToneParam :: Double -> Double -> Double
getToneParam rate tone = tone * 2.0 * pi / rate

playTones :: Double -> Consumer [Double] IO ()
playTones sec = await >>= \tones -> lift $ playTones' sec tones

playTones' :: Double -> [Double] -> IO ()
playTones' sec tones = play
  where
    rate = 44100::Double
    k tone = getToneParam rate tone
    end = 1000.0 * sec * 2.0 * pi
    mapper tone = map sin [0::Double,(k tone)..end]
    play = case mapper <$> tones of
      (sine1 : sines) -> (Play.monoToInt16 rate $ foldl (zipWith (+)) sine1 sines) >> pure ()
      _ -> pure ()

playNotes :: Int -> Consumer [Note] IO ()
playNotes rate = P.map (runNote JustIntonation 440.0 <$>) >-> playTones (1.0 / fromIntegral rate)
