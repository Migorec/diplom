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


type IR = (Maybe Double, Transaction)

findFInt :: FEC -> String -> (Maybe (Double, Transaction), FEC)
findFInt [] _ = (Nothing, [])
findFInt ((t,transact):fs) f| ownership transact == f = (Just (t,transact),fs)
                            | otherwise = let (r, fs') = findFInt fs f
                                          in (r,(t,transact):fs')

findCInt :: CEC -> String -> (Maybe Transaction,CEC)
findCInt [] _ = (Nothing,[])
findCInt (t:ts) f | ownership t == f = (Just t, ts)
                  | otherwise = let (r, ts') = findCInt ts f
                                in (r,t:ts')

findInt :: FEC -> CEC -> String -> ((Maybe Double, Transaction),FEC,CEC)
findInt fec cec f = 
    case findFInt fec f of
     (Just (t,transact), fec') -> ((Just t, transact), fec', cec)
     _ -> case findCInt cec f of
           (Just transact, cec') -> ((Nothing, transact), fec, cec')
           _ -> error "facility busy, but who did it?"
          
type IC = [(Maybe Double, Transaction)]


addIC :: IC -> (Maybe Double, Transaction) -> IC
addIC ic (t, transact) = before ++ ((t,transact):after)
    where p = priority transact
          (before, after) = span (\(_,x) -> priority x > p) ic
