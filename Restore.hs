-- | building a machinery from a serialized value of it. 
module Restore where

import Control.Monad (foldM)
import Control.Applicative ((<$>))
import Data.Typeable (cast)
import Data.Maybe (listToMaybe, fromJust)

import Control.Concurrent.STM 
	(STM, TVar, TChan, atomically, writeTVar, writeTChan, unGetTChan, readTVar, newTChan, newTVar)

import Data.Tree (subForest, flatten)
import Data.Tree.Zipper (tree, root, getLabel, insertDownLast, fromTree)

import Lib (mapAccumM, dup'TChan)
import Common (Module (r0) , SMs (..), Store, SMrt (..), SMr (..) , Node (..) , Events, Restoring, Dump , Borning, tstate, Coo, Ctx, down, fire)

-- import Debug.Trace


-- | setting  state on a store node. The node is already there so its type is already fixed
poke :: SMs -> Store -> STM ()
poke (SMs s) t = do 
	SMrt st  _ <- return . tstate . getLabel $ t 
	case cast s of 
		Nothing -> error "poke: deserialization error, trying to push back a state"
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
		Nothing -> error "recreate: resuming error , index out of range in selecting borned twin"
		Just (SMr s r) -> do
			tc <- newTVar $ coo ++ [j]
			st <- newTVar s
			nt <- dup'TChan (let Node _ _ _ ejs = getLabel t' in ejs)
			let 	l = Node (SMrt st r) c tc nt
			return $ insertDownLast (return l) t'
-- | set state and 
put :: Node -> Restoring -> STM Node
put l@(Node (SMrt st r) ctx tcoo evs)  (SMs s,  ejs)  = do
	case cast s of 
		Nothing -> error "put: deserialization error, trying to push back a state"
		Just s -> do
			writeTVar st s
			mapM_ (unGetTChan evs) ejs
			return l

restore :: Dump -> Store -> STM Store
restore (cs,rs) t = do
	t' <- foldM recreate (root t) cs
	fromTree <$> mapAccumM put rs (tree $ root t')

launchAll :: Borning -> Store -> STM ()
launchAll bs = mapM_ (writeTChan bs) . flatten . tree

-- | this is the booting action for the machinery. Critical choices are here. the contexts list cs in Dump doesn't contain the creation context of the root node, while the value restoring list does. From the head of the latter we extract the type of the root node state 
newStore :: Borning -> Events -> Dump -> STM Store
newStore bs evs d@(cs,(SMs s,_):ss) = let us = undefined `asTypeOf` s in do 
		st <- newTVar us 
		tc <- newTVar []
		evs' <- dup'TChan evs
		let t0 = fromTree (return (Node (SMrt st $ r0 us) (error "accessing creation context of root") tc evs'))
		t <- restore d t0
		launchAll bs t
		return t
