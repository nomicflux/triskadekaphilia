module TDP.Synth.EventHandler where

import           Control.Concurrent.STM.TVar          (TVar)

import           Graphics.Gloss.Interface.IO.Interact (Event (..), Key (..),
                                                       KeyState (..))

import           TDP.Note
import           TDP.Synth.Input
import           TDP.Synth.InputState
import           TDP.Synth.Keymapping

handleEvent :: Keymapping -> TVar NoteQueue -> Event -> IO ()
handleEvent km nq (EventKey (Char c) Down _ _) = addNotes nq (applyKeymapping km [c])
handleEvent km nq (EventKey (Char c) Up _ _)   = removeNotes nq (applyKeymapping km [c])
handleEvent _ _ _                             = pure ()
