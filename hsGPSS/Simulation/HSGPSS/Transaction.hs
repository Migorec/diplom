module Simulation.HSGPSS.Transaction where

import Data.IntMap

data TransactionState = Active | Suspended | Passive | Terminated deriving (Eq, Show)

data Transaction = Transaction { currentBlock :: Int,
                                 nextBlock :: Int,
                                 priority :: Int,
                                 state :: TransactionState,
                                 params :: IntMap Double
                               } deriving (Eq, Show)
