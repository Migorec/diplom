{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}

module Models.Imitational where

import Simulation.HSGPSS
import Simulation.HSGPSS.Prelude
import Simulation.HSGPSS.Random.Functions
import Control.Applicative 
import Control.Monad
import Models.Param


performance :: MParam -> IO Double
performance (MParam m n k l lambda mu nu alpha beta gamma delta) = 
    do sr <- simulate (complexModel m n k l lambda mu nu alpha beta gamma delta) (round 1)
       let Just count = fcCount sr "counter"
           time = simulationTime sr
       return $ (fromIntegral (count - k)) / time

--breakReairProc :: Int -> Double -> Double -> BlockStateMonad
breakReairProc i alpha beta l' = 
    do generate (0,0,0,1)
       l <- advance (1/alpha, xpdis)
       preempt ("proc" ++ show i, (), l', 1, RE)
       enter "repairers"
       advance (1/beta,xpdis)
       return' ("proc" ++ show i)
       leave "repairers"
       transfer ((),l)

--breakReairChan :: Int -> Double -> Double -> BlockStateMonad
breakReairChan i gamma delta l'= 
    do generate (0,0,0,1)
       l <- advance (1/gamma, xpdis)
       preempt ("chan" ++ show i, (), l', 1, RE)
       enter "repairers"
       advance (1/delta,xpdis)
       return' ("chan" ++ show i)
       leave "repairers"
       transfer ((),l)

--proc :: Int -> Double -> Int -> BlockStateMonad
proc i mu l =
    do seize ("proc" ++ show i)
       advance (1/mu, xpdis)
       release ("proc" ++ show i)
       transfer ((),l)

procFinish i l =
    do seize ("proc" ++ show i)
       advance (Pr 1)
       release ("proc" ++ show i)
       transfer ((),l)

--chan :: Int -> Double -> Int -> BlockStateMonad
chan i nu l =
    do seize ("chan" ++ show i)
       advance (1/nu, xpdis)
       release ("chan" ++ show i)
       transfer ((),l)

chanFinish i l =
    do seize ("chan" ++ show i)
       advance (Pr 1)
       release ("chan" ++ show i)
       transfer ((),l)

complexModel :: Int -> Int -> Int -> Int ->Double -> Double -> Double -> Double -> Double -> Double -> Double -> BlockStateMonad
complexModel m' n' k l lambda mu nu alpha beta gamma delta = 
    do let m = fromIntegral m'
           n = fromIntegral n'
    
       storage ("repairers", l)
       generate (0,0,0,1)
       advance 100
       terminate 1
       
       
       generate (0,0,0,k)
       userPhase <- fromIntegral <$> advance (1/lambda, xpdis)
       seize "counter"
       release "counter"
       procStart <- transfer (All,userPhase +5, userPhase + 5 + (m-1)*4,4)
       chanStart <- transfer (All,userPhase +5+4*m, userPhase + 5 + m*4 + (n-1)*4,4)
       mapM_ (\i -> proc i mu chanStart) [1..m]
       ls <- mapM (\i -> chan i nu userPhase) [1..n]
       
       let l = fromIntegral $ last ls
       procFinishL <- transfer (All,l + 3, l + 3 + (m-1)*4,4)
       chanFinishL <- transfer (All,l + 3 + 4*m, l + 3 + 4*m + (n-1)*4,4)
       mapM_ (\i -> procFinish i chanStart) [1..m]
       mapM_ (\i -> chanFinish i userPhase) [1..n]
       
       when (alpha > 0) $ mapM_ (\i -> breakReairProc i alpha beta procFinishL) [1..m]
       when (gamma > 0) $ mapM_ (\i -> breakReairChan i gamma delta chanFinishL) [1..n]
       
       return (round 0)
