module TDP.Synth.Input where

import           Pipes                (Producer, for, lift, yield, (>->))
import qualified Pipes                as P
import qualified Pipes.Prelude        as P

import           System.IO            (hReady, stdin)

import           Data.Maybe           (mapMaybe)

import           TDP.Note
import           TDP.Scale
import           TDP.Synth.Keymapping

getKeys :: Producer [Char] IO ()
getKeys = lift (reverse <$> getKey' "") >>= yield
  where
    getKey' :: [Char] -> IO [Char]
    getKey' chars = getChar >>=
      (\c -> hReady stdin >>= \m ->
        (if m then getKey' else return) (c:chars))

getNotes :: Producer [Note] IO ()
getNotes = getKeys >-> P.map (applyKeymapping (digitMapping lambda))
