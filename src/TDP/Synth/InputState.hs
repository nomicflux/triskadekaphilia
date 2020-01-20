module TDP.Synth.InputState where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO,
                                              readTVarIO)

import           Data.HashSet                (HashSet, delete, difference,
                                              empty, fromList, toList, union)

import           TDP.Note

newtype NoteQueue = NoteQueue { _notes :: (HashSet Note) }

emptyNoteQueue :: NoteQueue
emptyNoteQueue = NoteQueue empty

getNoteQueue :: IO (TVar NoteQueue)
getNoteQueue = newTVarIO emptyNoteQueue

addNotes :: TVar NoteQueue -> [Note] -> IO ()
addNotes tvar = atomically . modifyTVar' tvar . addToQueue

removeNotes :: TVar NoteQueue -> [Note] -> IO ()
removeNotes tvar = atomically . modifyTVar' tvar . removeFromQueue

readNotes :: TVar NoteQueue -> IO [Note]
readNotes = ((toList . _notes) <$>) . readTVarIO

addToQueue :: [Note] -> NoteQueue -> NoteQueue
addToQueue notes (NoteQueue q) = NoteQueue $ union (fromList notes) q

removeFromQueue :: [Note] -> NoteQueue -> NoteQueue
removeFromQueue notes (NoteQueue q) = NoteQueue $ difference q (fromList notes)

getNotesFromQueue :: NoteQueue -> [Note]
getNotesFromQueue = toList . _notes
