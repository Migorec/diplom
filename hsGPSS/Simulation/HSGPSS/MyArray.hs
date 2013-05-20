module Simulation.HSGPSS.MyArray where


import Data.Array

changeElem :: Array Int e -> Int -> e -> Array Int e
changeElem arr i e = listArray (l,r) (before ++ [e] ++ drop 1 after) 
    where es = elems arr
          (l,r) = bounds arr
          (before, after)= splitAt (i-l) es
