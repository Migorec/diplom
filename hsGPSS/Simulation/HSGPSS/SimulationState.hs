module Simulation.HSGPSS.SimulationState where

import Data.Map
import GHC.Exts (sortWith)
import Data.Array
import Control.Monad.State
import Simulation.HSGPSS.Chains
import Simulation.HSGPSS.Blocks
import Simulation.HSGPSS.Queue
import Simulation.HSGPSS.Facility
import Simulation.HSGPSS.Storage

data SBlock = SBlock { block :: Block,
                       ix :: Int
                     } deriving (Eq, Show)
                     

data SimulationState = SimulationState { fec :: FEC,
                                         cec :: CEC,
                                         blocks :: Array Int SBlock,
                                         facilities :: Map String SFacility,
                                         queues :: Map String SQueue,
                                         storages :: Map String SStorage,
                                         toTerminate :: Int,
                                         currentTime :: Double
                                       } deriving (Eq, Show)
                                       
ssInit :: BlockStateMonad -> Int -> SimulationState
ssInit bsm tt = let (_, BlockState bs c ss) = runState bsm initState 
                in SimulationState [] 
                                   [] 
                                   (listArray (0, c-1) $ 
                                    Prelude.map (\(b,i) -> SBlock b i) $ 
                                    zip (reverse bs) [0 .. c]
                                   )
                                   empty 
                                   empty 
                                   (fromAscList $ 
                                    sortWith fst $ 
                                    fmap (\(Storage name cap) -> (name, stInit cap)) ss
                                   ) 
                                   tt 0
