module Main where

import Simulation.HSGPSS.Tests.Blocks.Generate
import Test.HUnit

main = runTestTT generateTests
