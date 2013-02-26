{-# LANGUAGE FlexibleInstances #-}

module Simulation.HSGPSS.Blocks.Generate where

import Simulation.HSGPSS.Blocks

class GenerateClass a where
    generate :: a -> BlockStateMonad

instance GenerateClass Double where
    generate m = addBlock $ GenerateRangeNoLimit m 0 0 0

instance GenerateClass (Double, Double) where
    generate (m, r) = addBlock $ GenerateRangeNoLimit m r 0 0
        
instance GenerateClass (Double, Double, Double) where
    generate (m, r, d) = addBlock $ GenerateRangeNoLimit m r d 0

instance GenerateClass (Double, Double, Double, (), Double) where
    generate (m, r, d, (), p) = addBlock $ GenerateRangeNoLimit m r d (round p)

instance GenerateClass (Double, Double, Double, (), Int) where
    generate (m, r, d, (), p) = addBlock $ GenerateRangeNoLimit m r d p

instance GenerateClass (Double, Double, Double, Double) where
    generate (m, r, d, l) = addBlock $ GenerateRangeGeneral m r d (round l) 0

instance GenerateClass (Double, Double, Double, Int) where
    generate (m, r, d, l) = addBlock $ GenerateRangeGeneral m r d l 0

instance GenerateClass (Double, Double, Double, Double, Double) where
    generate (m, r, d, l, p) = addBlock $ GenerateRangeGeneral m r d (round l) (round p)
    
instance GenerateClass (Double, Double, Double, Int, Double) where
    generate (m, r, d, l, p) = addBlock $ GenerateRangeGeneral m r d l (round p)
    
instance GenerateClass (Double, Double, Double, Double, Int) where
    generate (m, r, d, l, p) = addBlock $ GenerateRangeGeneral m r d (round l) p
    
instance GenerateClass (Double, Double, Double, Int, Int) where
    generate (m, r, d, l, p) = addBlock $ GenerateRangeGeneral m r d l p


instance GenerateClass (Double, (Double -> Double)) where
    generate (m, f) = addBlock $ GenerateFuncNoLimit m f 0 0
        
instance GenerateClass (Double, (Double -> Double), Double) where
    generate (m, f, d) = addBlock $ GenerateFuncNoLimit m f d 0

instance GenerateClass (Double, (Double -> Double), Double, (), Double) where
    generate (m, f, d, (), p) = addBlock $ GenerateFuncNoLimit m f d (round p)

instance GenerateClass (Double, (Double -> Double), Double, (), Int) where
    generate (m, f, d, (), p) = addBlock $ GenerateFuncNoLimit m f d p

instance GenerateClass (Double, (Double -> Double), Double, Double) where
    generate (m, f, d, l) = addBlock $ GenerateFuncGeneral m f d (round l) 0

instance GenerateClass (Double, (Double -> Double), Double, Int) where
    generate (m, f, d, l) = addBlock $ GenerateFuncGeneral m f d l 0

instance GenerateClass (Double, (Double -> Double), Double, Double, Double) where
    generate (m, f, d, l, p) = addBlock $ GenerateFuncGeneral m f d (round l) (round p)
    
instance GenerateClass (Double, (Double -> Double), Double, Int, Double) where
    generate (m, f, d, l, p) = addBlock $ GenerateFuncGeneral m f d l (round p)
    
instance GenerateClass (Double, (Double -> Double), Double, Double, Int) where
    generate (m, f, d, l, p) = addBlock $ GenerateFuncGeneral m f d (round l) p
    
instance GenerateClass (Double, (Double -> Double), Double, Int, Int) where
    generate (m, f, d, l, p) = addBlock $ GenerateFuncGeneral m f d l p
