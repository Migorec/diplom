{-# LANGUAGE FlexibleInstances #-} 

-- |
module Simulation.HSGPSS.Blocks where

import Text.Show.Functions
import Control.Monad.State
import Data.Array

data BlockState = BlockState {blocks :: [Block],
                              count :: Int
                             } deriving (Eq, Show)
                             
initState = BlockState [] 0

addBlock :: Block -> BlockStateMonad
addBlock newBlock = 
    do BlockState b c <- get
       put $ BlockState (newBlock:b) (c+1)
       return c

type Blocks = Array Int Block

type BlockStateMonad = State BlockState Int

instance Eq (Double->Double) where
    (==) _ _ = True

-- ^ Tип данных для блоков
data Block = GenerateRangeGeneral { mean :: Double,
                                    halfRange :: Double,
                                    delay :: Double,
                                    limit :: Int,
                                    priority :: Int
                                  }
            |GenerateRangeNoLimit { mean :: Double,
                                    halgeRange :: Double,
                                    delay :: Double,
                                    priority :: Int
                                  } 
            |GenerateFuncGeneral  { mean :: Double,
                                    func :: Double -> Double,
                                    delay :: Double,
                                    limit :: Int,
                                    priority :: Int
                                  }
            |GenerateFuncNoLimit  { mean :: Double,
                                    func :: Double -> Double,
                                    delay :: Double,
                                    priority :: Int
                                  } deriving (Eq,Show)

