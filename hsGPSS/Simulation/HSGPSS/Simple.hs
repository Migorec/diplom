{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}

module Simple where

import Simulation.HSGPSS
import Simulation.HSGPSS.Prelude
import Simulation.HSGPSS.Random.Functions

model =  do generate (20, xpdis)
            queue "q1"
            seize "f1"
            depart "q1"
            advance (15, xpdis)
            release "f1"
            terminate 1
