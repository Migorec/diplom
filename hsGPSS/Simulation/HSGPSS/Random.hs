module Simulation.HSGPSS.Random where

import System.Random
import Control.Applicative

randomMHTime :: Double -> Double -> IO Double
randomMHTime m h = randomRIO (m-h, m+h)

randomMFTime :: Double -> (Double -> Double) -> IO Double
randomMFTime m f = randomRIO (0,1) >>= \t -> return (m * f t)

randomDist :: Int -> Int -> IO Int
randomDist d1 d2 = randomRIO (d1, d2)
