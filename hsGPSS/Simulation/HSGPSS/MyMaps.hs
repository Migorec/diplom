{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}


module Simulation.HSGPSS.MyMaps where

import Simulation.HSGPSS.Queue
import Simulation.HSGPSS.Facility
import qualified Data.IntMap as IM
import qualified Data.Map as M

class DefaultUpdateClass a b k | a -> k, a -> b where
    defaultUpdate :: (b -> b) -> k -> a -> a
    
    
instance DefaultUpdateClass (M.Map String SFacility) SFacility String where
    defaultUpdate f key m = case val of
                                Nothing -> M.insert key (f initFacility) m
                                Just v  -> M.adjust f key m
        where val = M.lookup key m
        
        

instance DefaultUpdateClass (M.Map String SQueue) SQueue String where
    defaultUpdate f key m = case val of
                                Nothing -> M.insert key (f initQueue) m
                                Just v  -> M.adjust f key m
        where val = M.lookup key m

