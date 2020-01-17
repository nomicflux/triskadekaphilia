module TDP.Synth.Keymapping where

import           Data.Maybe (mapMaybe)

import           TDP.Note
import           TDP.Scale

newtype Keymapping = Keymapping ([Char] -> [Note])

mkKeymapping :: [Char] -> Scale -> Keymapping
mkKeymapping chars (Scale notes) = Keymapping $ mapMaybe find
  where zipped = zip chars notes
        find c = lookup c zipped

digitMapping :: Scale -> Keymapping
digitMapping = mkKeymapping ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']

applyKeymapping :: Keymapping -> [Char] -> [Note]
applyKeymapping (Keymapping f) = f
