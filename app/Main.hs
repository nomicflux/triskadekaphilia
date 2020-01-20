module Main where

import           Control.Concurrent.Async             (async, wait)
import           Control.Monad                        (forever, mapM_)
import           System.IO                            (BufferMode (..),
                                                       hSetBuffering, hSetEcho,
                                                       stdin)

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game     (playIO)
import           Graphics.Gloss.Interface.IO.Interact (Event (..), Key (..),
                                                       KeyState (..))

import           Pipes                                (Consumer, Pipe, Producer,
                                                       for, lift, yield, (>->))
import qualified Pipes                                as P
import           Pipes.Concurrent                     (fromInput, spawn,
                                                       toOutput, unbounded)
import           Pipes.Core                           (runEffect)
import qualified Pipes.Prelude                        as P

import           TDP.Note
import           TDP.Scale
import           TDP.Synth.Input
import           TDP.Synth.InputState
import           TDP.Synth.Keymapping
import           TDP.Synth.Play
import           TDP.Tuning

windowDisplay :: Display
windowDisplay = InWindow "TriskaDekaPhilia" (800, 800) (0, 0)

animation :: Float -> Picture
animation t = Circle (2 * t)

frameRate :: Int
frameRate = 20

drawingFunc :: NoteQueue -> IO Picture
drawingFunc _ = pure Blank

basicKeymapping :: Keymapping
basicKeymapping = digitMapping lambda

inputHandler :: Event -> NoteQueue -> IO NoteQueue
inputHandler (EventKey (Char c) Down _ _) nq   = pure $ addToQueue (applyKeymapping basicKeymapping [c]) nq
inputHandler (EventKey (Char c) Up _ _) nq = pure $ removeFromQueue (applyKeymapping basicKeymapping [c]) nq
inputHandler _ nq                            = pure $ nq

updateFunc :: Float -> NoteQueue -> IO NoteQueue
updateFunc _ nq = (runEffect $ notePipe >-> playNotes frameRate) >> pure nq
  where notePipe :: Producer [Note] IO ()
        notePipe = lift (pure (getNotesFromQueue nq)) >>= yield

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  (notesOutbound, noiseInbound) <- spawn unbounded
  playIO windowDisplay white frameRate emptyNoteQueue drawingFunc inputHandler updateFunc
