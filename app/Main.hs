module Main where

import           Control.Concurrent.Async (async, wait)
import           Control.Monad            (forever, mapM_)
import           Pipes                    (Consumer, Pipe, Producer, for, (>->))
import qualified Pipes                    as P
import           Pipes.Concurrent         (fromInput, spawn, toOutput,
                                           unbounded)
import           Pipes.Core               (runEffect)
import qualified Pipes.Prelude            as P
import           System.IO                (BufferMode (..), hSetBuffering,
                                           hSetEcho, stdin)

import           TDP.Note
import           TDP.Synth.Input
import           TDP.Synth.Play
import           TDP.Tuning

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  (outbound, inbound) <- spawn unbounded
  forever $ do notes <- async $ runEffect $ getNotes >-> toOutput outbound
               played <- async $ runEffect $ fromInput inbound >-> playNotes
               wait notes
