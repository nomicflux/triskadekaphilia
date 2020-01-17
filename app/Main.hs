module Main where

import           Control.Monad   (forever, mapM_)
import           System.IO       (BufferMode (..), hSetBuffering, hSetEcho,
                                  stdin)

import           TDP.Synth.Input
import           TDP.Synth.Play
import           TDP.Tuning

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  forever $ getNotes >>= playNotes
