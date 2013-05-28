{-# LANGUAGE FlexibleInstances #-}

module Simulation.HSGPSS.Blocks.Storage where

import Simulation.HSGPSS.Blocks
import Control.Monad.State

class StorageClass a where
    storage :: a -> State BlockState ()
    
instance StorageClass (String, Int) where
    storage (name, capacity) = addStorage $ Storage name capacity
    
instance StorageClass (String, Double) where
    storage (name, capacity) = addStorage $ Storage name (round capacity)
    
    
class EnterLeaveClass a where
    enter :: a -> BlockStateMonad
    leave :: a -> BlockStateMonad
    
instance EnterLeaveClass String where
    enter name = addBlock $ Enter name 1
    leave name = addBlock $ Leave name 1
    
instance EnterLeaveClass (String, Int) where
    enter (name, dec) = addBlock $ Enter name dec
    leave (name, inc) = addBlock $ Leave name inc
    
instance EnterLeaveClass (String, Double) where
    enter (name, dec) = addBlock $ Enter name (round dec)
    leave (name, inc) = addBlock $ Leave name (round inc)
