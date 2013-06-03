{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}

module Simple where

import Simulation.HSGPSS
import Simulation.HSGPSS.Prelude
import Simulation.HSGPSS.Random.Functions
import Control.Monad
import Data.Maybe


simpleUtil = do sr <- simulate simpleModel (round 500)
                return $ fUtilisation sr "f1"


test = do l <- replicateM (round 100) simpleUtil 
          return $ (sum $ fmap (fromJust) l)/100 

simpleModel =  
    do generate (2000, xpdis)
       queue "q1"
       seize "f1"
       depart "q1"
       advance (500, xpdis)
       release "f1"
       terminate 1
