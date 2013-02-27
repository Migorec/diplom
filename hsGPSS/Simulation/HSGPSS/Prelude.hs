module Simulation.HSGPSS.Prelude (module Prelude,
                                  fromInteger,
                                  fromRational
                                 ) where


import Prelude hiding (fromInteger, fromRational)
import qualified Prelude as P

fromInteger :: Integer -> Double
fromInteger = realToFrac

fromRational :: Rational -> Double
fromRational = P.fromRational

