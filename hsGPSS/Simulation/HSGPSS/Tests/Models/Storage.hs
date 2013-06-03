{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}

module Simple where

import Simulation.HSGPSS
import Simulation.HSGPSS.Prelude
import Simulation.HSGPSS.Random.Functions

storageUtil = do sr <- simulate storageModel (round 100)
                 return $ sUtilisation sr "stor"


--test = do l <- replicateM (round 100) simpleUtil 
--          return $ (sum $ fmap (fromJust) l)/100 

storageModel =  
    do storage ("stor", 4)
       generate (20, xpdis)
       queue "q1"
       enter ("stor",2)
       depart "q1"
       advance (20, xpdis)
       leave ("stor", 2)
       terminate 1
