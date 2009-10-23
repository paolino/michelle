module Restore where

import Lib
import Common
-- import Control.Monad
import Control.Applicative
import Control.Arrow
import Data.Typeable
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Maybe
import Data.Either
import Data.Maybe
import Data.List
import Debug.Trace
import Data.Tree.Zipper
import Data.Tree hiding (Node)
import Lib (mapAccumM, dup'TChan, contents)

-- | setting  state on a store node. The node is already there so its type is already fixed
poke :: SMs -> Store -> STM ()
poke (SMs s) t = do 
	SMrt st  _ <- return . tstate . getLabel $ t 
	case cast s of 
		Nothing -> error "deserialization error, trying to push back a state"
		Just s -> writeTVar st s 


-- | what is really needed for module recreation at each parent met, poking a state, going down last child and computing its position 
pokedowncoo :: (Store,Coo) -> SMs -> STM (Store,Coo)
pokedowncoo (t,c) x  = do
	poke x t
	let n = length . subForest $ tree t
	flip (,) (c ++ [n]) <$> down (n - 1) t

-- | module recreation. This involve executing a Creation structure inside a Store 
recreate :: Store -> Ctx -> STM Store
recreate t c@(e,sms,j) = do
	(t',coo) <- foldM pokedowncoo (root t,[]) $ init sms
	poke (last sms) t' 
	(children,_) <- fire (tstate . getLabel $ t') (Left e) -- process the creation event
	case listToMaybe . drop j $ children of
		Nothing -> error "resuming error , index out of range in selecting borned twin"
		Just (SMr s r) -> do
			tc <- newTVar $ coo ++ [j]
			st <- newTVar s
			nt <- newTChan
			let 	l = Node (SMrt st r) c tc nt
			return $ insertDownLast (return l) t'

put :: Node -> Restoring -> STM Node
put l@(Node (SMrt st r) ctx tcoo evs)  (SMs s,  ejs)  =
	case cast s of 
		Nothing -> error "deserialization error, trying to push back a state"
		Just s -> do
			writeTVar st s
			mapM_ (writeTChan evs) ejs
			return l

restore :: Dump -> Store -> STM Store
restore (cs,rs) t = do
	t' <- foldM recreate (root t) cs
	fromTree <$> mapAccumM put rs (tree t')

launchAll :: Borning -> Store -> STM ()
launchAll bs = mapM_ (writeTChan bs) . flatten . tree

restoreIO :: Borning -> TVar Store -> IO Dump -> IO ()
restoreIO bs ts r = do
	d <- r
	atomically $ do 
		readTVar ts >>= restore d >>= writeTVar ts
		readTVar ts >>= launchAll bs 
