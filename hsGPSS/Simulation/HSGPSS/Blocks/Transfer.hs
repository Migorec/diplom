{-# LANGUAGE FlexibleInstances #-}

module Simulation.HSGPSS.Blocks.Transfer where

import Simulation.HSGPSS.Blocks

data TransferMode = Both | All | Pick | P | Sbr 

class TransferClass a where
    transfer :: a -> BlockStateMonad
    
instance TransferClass ((),Int) where
    transfer ((),np) = addBlock $ TransferUnconditional np

instance TransferClass ((),Double) where
    transfer ((),np) = addBlock $ TransferUnconditional (round np)
    
instance TransferClass (Double,Double,Double) where
    transfer (p,sp,fp) = addBlock $ TransferFractional2 p (round fp) (round sp)
    
instance TransferClass (Double,Int,Double) where
    transfer (p,sp,fp) = addBlock $ TransferFractional2 p (round fp) sp
    
instance TransferClass (Double,Double,Int) where
    transfer (p,sp,fp) = addBlock $ TransferFractional2 p fp (round sp)
    
instance TransferClass (Double,Int,Int) where
    transfer (p,sp,fp) = addBlock $ TransferFractional2 p fp sp
    
instance TransferClass (Double,(),Double) where
    transfer (p,(),np) = addBlock $ TransferFractional1 p (round np)
    
instance TransferClass (Double,(),Int) where
    transfer (p,(),np) = addBlock $ TransferFractional1 p np
    
instance TransferClass (TransferMode,Double, Double) where
    transfer (Both,fp,sp) = addBlock $ TransferBoth (round fp) (round sp)
    transfer (All,fp,sp) = addBlock $ TransferAll (round fp) (round sp) 1
    transfer (Pick,fp,sp) = addBlock $ TransferPick (round fp) (round sp)
    
instance TransferClass (TransferMode,Double, Int) where
    transfer (Both,fp,sp) = addBlock $ TransferBoth (round fp) sp
    transfer (All,fp,sp) = addBlock $ TransferAll (round fp) sp 1
    transfer (Pick,fp,sp) = addBlock $ TransferPick (round fp) sp
    
instance TransferClass (TransferMode,Int, Double) where
    transfer (Both,fp,sp) = addBlock $ TransferBoth fp (round sp)
    transfer (All,fp,sp) = addBlock $ TransferAll fp (round sp) 1
    transfer (Pick,fp,sp) = addBlock $ TransferPick fp (round sp)
    
instance TransferClass (TransferMode,Int, Int) where
    transfer (Both,fp,sp) = addBlock $ TransferBoth fp sp
    transfer (All,fp,sp) = addBlock $ TransferAll fp sp 1
    transfer (Pick,fp,sp) = addBlock $ TransferPick fp sp
    
instance TransferClass (TransferMode,Double,Double,Double) where
    transfer (All,fp,sp,inc) = addBlock $ TransferAll (round fp) (round sp) (round inc)
    
instance TransferClass (TransferMode,Int,Double,Double) where
    transfer (All,fp,sp,inc) = addBlock $ TransferAll fp (round sp) (round inc)
    
instance TransferClass (TransferMode,Int,Int,Double) where
    transfer (All,fp,sp,inc) = addBlock $ TransferAll fp sp (round inc)
    
instance TransferClass (TransferMode,Int,Double,Int) where
    transfer (All,fp,sp,inc) = addBlock $ TransferAll fp (round sp) inc
    
instance TransferClass (TransferMode,Double,Int,Double) where
    transfer (All,fp,sp,inc) = addBlock $ TransferAll (round fp) sp (round inc)
    
instance TransferClass (TransferMode,Double,Int,Int) where
    transfer (All,fp,sp,inc) = addBlock $ TransferAll (round fp) sp inc

instance TransferClass (TransferMode,Double,Double,Int) where
    transfer (All,fp,sp,inc) = addBlock $ TransferAll (round fp) (round sp) inc
    
instance TransferClass (TransferMode,Int,Int,Int) where
    transfer (All,fp,sp,inc) = addBlock $ TransferAll fp sp inc
    
instance TransferClass (TransferMode,String,Double) where
    transfer (P,p,i) = addBlock $ TransferParameter p (round i)
    
instance TransferClass (TransferMode,String,Int) where
    transfer (P,p,i) = addBlock $ TransferParameter p i

instance TransferClass (TransferMode, Double, String) where
    transfer (Sbr,np,p) = addBlock $ TransferSubroutine (round np) p
    
instance TransferClass (TransferMode, Int, String) where
    transfer (Sbr,np,p) = addBlock $ TransferSubroutine np p
