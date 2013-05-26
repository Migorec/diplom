module Simulation.HSGPSS.Chains where

import Simulation.HSGPSS.Transaction
type FECElem = (Double, Transaction)

type FEC = [FECElem]

addFE :: FEC -> FECElem -> FEC
addFE fec (time, transact) = let (before, after) = span (\x -> fst x <= time) fec
                             in before ++ ((time,transact):after) 
                             
                             

type PriorityChain = [Transaction]

type CEC = PriorityChain
type DC = PriorityChain

addPC :: PriorityChain -> Transaction -> PriorityChain
addPC pc transact = before ++ (transact:after)
    where p = priority transact
          (before, after) = span (\x -> priority x >= p) pc
          
type IC = [(Double, Transaction)]

addIC :: IC -> (Double, Transaction) -> IC
addIC ic (t, transact) = before ++ ((t,transact):after)
    where p = priority transact
          (before, after) = span (\(_,x) -> priority x > p) ic
