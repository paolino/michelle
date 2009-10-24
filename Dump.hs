-- | computing serializable values of the all machinary
module Dump (newDump) where

import Control.Monad (forM, forM_)
import Data.Maybe (isNothing)
import Control.Arrow (first)

import Control.Concurrent.STM (STM, TVar, atomically, readTVar, writeTVar)

import Data.Tree (Tree, rootLabel, subForest, flatten)
import Data.Tree.Zipper (tree)

import Lib (contents, pruneM)
import Common (SMs (..), SMrt (..), Store, Node (..), Dump, Coo)

-- import Debug.Trace
-- | compute a Dump value for persistence from the store. The context of the root node is dropped. This is a very starving action, it accesses all TVars in the Store. 
dump :: Store -> STM Dump
dump t = do
	let ys = flatten . tree $ t
	fmap (first tail . unzip) . forM ys $ \(Node (SMrt ts _) ctx _ evs) -> do
		s <- readTVar ts
		ejs <- contents evs
		return (ctx,(SMs s,ejs))

-- | prune off those branches where all nodes have state Nothing
pruning :: Store -> STM (Maybe Store)
pruning  = pruneM $ \(Node (SMrt x _) _ _ _) -> readTVar x >>= return . isNothing 

-- | after pruning the coordinates are to be recomputed as some branches may be missing
recoo :: Coo -> Tree Node -> STM ()
recoo coo t = do
	let (Node _ _ tc _) = rootLabel t
	writeTVar tc coo
	forM_ (zip [0..] (subForest t)) $ \(j,t) -> recoo (coo ++ [j]) t

-- | atomically access the Store to produce a serializable value of it all. BUG in case of industrial server this action is going to starve. SOLUTION add a semaphore on events processor threads
newDump :: Store -> STM (Store,Dump)
newDump t = do
	t <- pruning t
	case t of
		Nothing -> error "no more service, pruned it all"
		Just t -> do 	recoo [] . tree $ t 
				d <- dump t
				return (t,d)


