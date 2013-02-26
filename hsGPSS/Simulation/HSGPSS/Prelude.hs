module Simulation.HSGPSS.Prelude (module Prelude,
                                  fromInteger
                                 ) where


import Prelude hiding (fromInteger)

fromInteger :: Integer -> Double
fromInteger = realToFrac

