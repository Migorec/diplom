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
data Block = AdvanceRange         { mean :: Double,
                                    halfRange :: Double
                                  }
            |AdvanceFunc          { mean :: Double,
                                    func :: Double -> Double
                                  }
            |GenerateRangeGeneral { mean :: Double,
                                    halfRange :: Double,
                                    delay :: Double,
                                    limit :: Int,
                                    priority :: Int
                                  }
            |GenerateRangeNoLimit { mean :: Double,
                                    halfRange :: Double,
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
                                  }
            |Queue                { qName :: String,
                                    inc :: Int
                                  }
            |Depart               { qName :: String,
                                    dec :: Int
                                  }
            |Terminate            { countDec :: Int
                                  }
            |TransferUnconditional{newPlace :: Int
                                  }
            |TransferFractional1  {probability :: Double,
                                   newPlace :: Int
                                  }
            |TransferFractional2  {probability :: Double,
                                   firstPlace :: Int,
                                   secondlace :: Int
                                  }
            |TransferBoth         {firstPlace :: Int,
                                   secondPlace :: Int
                                  }
            |TransferAll          {firstPlace :: Int,
                                   lastPlace ::Int,
                                   step :: Int
                                  } 
            |TransferPick         {firstPlace ::Int,
                                   lastPlace :: Int
                                  } 
            |TransferParameter    {placemaker :: String,
                                   increment :: Int
                                  }
            |TransferSubroutine   {newPlace :: Int,
                                   placemaker :: String
                                  } deriving (Eq,Show)

