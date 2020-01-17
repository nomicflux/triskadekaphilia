module TDP.Scale where

import           TDP.Note

newtype Scale = Scale [Note]

lambda :: Scale
lambda = Scale [c, d, e, f, g, h, j, a, b, shiftTritaveUp c]

chromatic :: Scale
chromatic = Scale [c, c_s, d, e, f, f_s, g, h, h_s, j, a, a_s, b, shiftTritaveUp c]
