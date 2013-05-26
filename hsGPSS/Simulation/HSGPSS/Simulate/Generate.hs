module Simulation.HSGPSS.Simulate.Generate where

import Simulation.HSGPSS.Blocks as B hiding (blocks)
import Simulation.HSGPSS.SimulationState
import Simulation.HSGPSS.Chains
import Simulation.HSGPSS.Transaction
import Simulation.HSGPSS.MyArray
import Data.IntMap
import Simulation.HSGPSS.Random

generateNoLimit :: SimulationState -> Int -> Double -> Int -> Double -> SimulationState
generateNoLimit ss ix d p t = 
    let time = if currentTime ss == 0 then t + d else t
        transact = Transaction ix (ix+1) p Passive empty ""
    in ss{fec = addFE (fec ss) (time + currentTime ss, transact)}

generateGeneral :: SimulationState -> Int -> Block -> Double -> SimulationState
generateGeneral ss ix b t = 
    if limit b > 0
    then let time = if currentTime ss == 0 then t + delay b else t
             transact = Transaction ix (ix+1) (B.priority b) Passive empty ""
         in ss{fec = addFE (fec ss) (time + currentTime ss,transact), blocks = changeElem (blocks ss) ix (SBlock b ix)}
    else ss

generate :: SimulationState -> SBlock -> IO SimulationState
generate ss (SBlock (GenerateRangeNoLimit m h d p) ix) = 
    do rTime <- randomMHTime m h
       return $ generateNoLimit ss ix d p rTime
       
generate ss (SBlock b@(GenerateRangeGeneral m h d l p) ix) = 
    do rTime <- randomMHTime m h
       return $ generateGeneral ss ix b rTime
    
generate ss (SBlock (GenerateFuncNoLimit m f d p) ix) = 
    do rTime <- randomMFTime m f
       return $ generateNoLimit ss ix d p rTime
       
generate ss (SBlock b@(GenerateFuncGeneral m f d l p) ix) = 
    do rTime <- randomMFTime m f
       return $ generateGeneral ss ix b rTime
       
generate ss _ = return ss
