{-# LANGUAGE FlexibleInstances #-}

module Simulation.HSGPSS.Blocks.Generate where

import Simulation.HSGPSS.Blocks

class GenerateClass a where
    generate :: Double -> a

instance GenerateClass BlockStateMonad where
    generate m = addBlock $ GenerateRangeNoLimit m 0 0 (round 0)

instance GenerateClass (Double -> BlockStateMonad) where
    generate m = \r -> addBlock $ GenerateRangeNoLimit m r 0 (round 0)
        
instance GenerateClass (Double -> Double -> BlockStateMonad) where
    generate m = \r d -> addBlock $ GenerateRangeNoLimit m r d (round 0)

instance GenerateClass (Double -> Double -> () -> Double -> BlockStateMonad) where
    generate m = \r d () p -> addBlock $ GenerateRangeNoLimit m r d (round p)

instance GenerateClass (Double -> Double -> Double -> BlockStateMonad) where
    generate m = \r d l -> addBlock $ GenerateRangeGeneral m r d (round l) (round 0)

instance GenerateClass (Double -> Double -> Double -> Double -> BlockStateMonad) where
    generate m = \r d l p-> addBlock $ GenerateRangeGeneral m r d (round l) (round p)


instance GenerateClass ((Double -> Double) -> BlockStateMonad) where
    generate m = \f -> addBlock $ GenerateFuncNoLimit m f 0 (round 0)
        
instance GenerateClass ((Double -> Double) -> Double -> BlockStateMonad) where
    generate m = \f d -> addBlock $ GenerateFuncNoLimit m f d (round 0)

instance GenerateClass ((Double -> Double) -> Double -> () -> Double -> BlockStateMonad) where
    generate m = \f d () p -> addBlock $ GenerateFuncNoLimit m f d (round p)

instance GenerateClass ((Double -> Double) -> Double -> Double -> BlockStateMonad) where
    generate m = \f d l -> addBlock $ GenerateFuncGeneral m f d (round l) (round 0)

instance GenerateClass ((Double -> Double) -> Double -> Double -> Double -> BlockStateMonad) where
    generate m = \f d l p-> addBlock $ GenerateFuncGeneral m f d (round l) (round p)
    
