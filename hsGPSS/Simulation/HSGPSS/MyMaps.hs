{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}


module Simulation.HSGPSS.MyMaps where

import Simulation.HSGPSS.Queue
import Simulation.HSGPSS.Facility
import Simulation.HSGPSS.Storage
import qualified Data.IntMap as IM
import qualified Data.Map as M

class DefaultUpdateClass a b k | a -> k, a -> b where
    defaultUpdate :: (b -> b) -> k -> a -> a
    
    
instance DefaultUpdateClass (M.Map String SFacility) SFacility String where
    defaultUpdate f key m = case val of
                                Nothing -> M.insert key (f initFacility) m
                                Just v  -> M.adjust f key m
        where val = M.lookup key m
        
instance DefaultUpdateClass (M.Map String SStorage) SStorage String where
    defaultUpdate f key m = case val of
                                Nothing -> error "no default for Storage. Int shoulb be defined explicitly"
                                Just v  -> M.adjust f key m
        where val = M.lookup key m

instance DefaultUpdateClass (M.Map String SQueue) SQueue String where
    defaultUpdate f key m = case val of
                                Nothing -> M.insert key (f initQueue) m
                                Just v  -> M.adjust f key m
        where val = M.lookup key m
        
instance DefaultUpdateClass (IM.IntMap Double) Double Int where
    defaultUpdate f key m = case val of
                                Nothing -> IM.insert key (f 0) m
                                Just v  -> IM.adjust f key m
        where val = IM.lookup  key m

