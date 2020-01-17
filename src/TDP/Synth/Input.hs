module TDP.Synth.Input where

import           System.IO            (hReady, stdin)

import           Data.Maybe           (mapMaybe)

import           TDP.Note
import           TDP.Scale
import           TDP.Synth.Keymapping

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = getChar >>=
          (\c -> hReady stdin >>= \m ->
            (if m then getKey' else return) (c:chars))

getNotes :: IO [Note]
getNotes = (applyKeymapping $ digitMapping lambda) <$> getKey
