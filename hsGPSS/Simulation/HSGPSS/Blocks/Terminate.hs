{-# LANGUAGE FlexibleInstances #-}

module Simulation.HSGPSS.Blocks.Terminate where

import Simulation.HSGPSS.Blocks

class TerminateClass a where
    terminate :: a -> BlockStateMonad
    
instance TerminateClass Double where
    terminate c = addBlock $ Terminate (round c)
    
instance TerminateClass Int where
    terminate c = addBlock $ Terminate c
    
