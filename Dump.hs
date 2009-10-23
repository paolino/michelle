
module Dump where

import Lib
import CoreZ
import Control.Monad
import Control.Applicative
import Control.Arrow
import Data.Typeable
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Maybe
import Data.Maybe
import Data.List
import Debug.Trace
import Data.Tree.Zipper as Z 
import Data.Tree
import Lib (mapAccumM, dup'TChan, contents, pruneM)


dump :: Store -> STM Dump
dump t = do
	let ys = flatten . tree $ t
	fmap unzip . forM ys $ \(Load (SMrt ts _) ctx _ evs) -> do
		s <- readTVar ts
		ejs <- contents evs
		return (ctx,(SMs s,ejs))

pruning :: Store -> STM (Maybe Store)
pruning  = pruneM $ \(Load (SMrt x _) _ _ _) -> readTVar x >>= return . isNothing 

recoo :: Coo -> Tree Load -> STM ()
recoo coo t = do
	let (Load _ _ tc _) = rootLabel t
	writeTVar tc coo
	forM_ (zip [0..] (subForest t)) $ \(j,t) -> recoo (coo ++ [j]) t

type Interval = Int
dumpIO :: TVar Store -> (Dump -> IO ()) -> Interval -> IO ()
dumpIO ts w ti = forever $ do
	threadDelay $ ti * 1000
	d <- atomically $ do
		t <- readTVar ts >>= pruning 
		case t of
			Nothing -> error "no more service"
			Just t -> do 	recoo [] . tree $ t  
					writeTVar ts t
					dump t
	w d


