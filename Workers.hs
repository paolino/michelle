

module Workers (workersThread) where

import Control.Monad (forever, foldM, when)
import Control.Concurrent.STM (STM,readTVar, writeTChan, writeTVar, readTChan,atomically, TVar, newTVar)
import Control.Concurrent (forkIO, myThreadId, killThread)
import Data.Tree (subForest)
import Data.Tree.Zipper (parent, insertDownLast, root, tree, fromTree, getLabel)
import Lib (dup'TChan)
import Data.Maybe (isNothing, fromJust)
import Control.Applicative ((<$>))
import Common (Borning, Coo, Events, Store, E, Twin, SMs (..) , Node (..), SMr (..) , SMrt (..), fire, down) 
-- import Debug.Trace

-- | a thread listening to the channel of borning modules . Every time a new module is scheduled to run a new thread is spawned. The thread is closed around its Node . Its job is waiting events on the machine personal channel, process them and modify the Store when the processing produces new machines. There are two reasons to kill a thread, on event request, this is a logical decision, and on external decision, when the store is Nothing.  
workersThread 	:: TVar (Maybe Store) 	-- ^ mutable cell for the store, Nothing signal the death of service
		-> Borning  		-- ^ the channel for thread births
		-> IO ()		-- ^ this is IO, as we manipulate threads
workersThread tt bs = forever $ do 
	Node q@(SMrt ts _)  _ tc evs <- atomically $ readTChan bs
	forkIO . forever  $ do 
		(test, reason) <- atomically $ do
				s <- readTVar ts
				t <- readTVar tt
				case t of
					Nothing -> do 
						return (True, Just $ "store disappeared, thread killed for state " ++ show s)
					Just t -> do
						ne <- readTChan evs
						(ms,ejs) <- fire q ne
						case ne of 
							Left e -> do  
								t' <- register bs e t (SMs s) ms
								writeTVar tt (Just t')
							_ -> return ()
						mapM_ (writeTChan evs) ejs
						flip (,) Nothing <$> isNothing <$> readTVar ts 
		when test $ do
			case reason of 
				Just r -> print r
				Nothing -> return ()
			myThreadId >>= killThread
-- | get the state of the passed location
peek :: Store -> STM (SMs)
peek t = do
	let Node ts _ _ _ = getLabel $ t
	SMrt st  _ <- return ts
	SMs `fmap` readTVar st 

-- | get the state of the passed location ,add it to an accumulator and go down the asked path
peekdown :: ([SMs],Store) -> Twin -> STM ([SMs],Store)
peekdown (ss,t) j = do
	s <- peek t
	t' <- down j t
	return (ss ++ [s],t')

-- | add a new set of created machine in the store under the same supermachine, which generated them. Machines are scheduled to start.
register 	:: Borning 	-- ^ schedule starting channel
		-> E 		-- ^ culprit event
		-> Store 	-- ^ the machine tree
		-> SMs 		-- ^ birthing machine state before event
		-> [SMr] 	-- ^ the environment and initial state of the new machines
		-> STM Store	-- ^ the modified machine tree
register bs e t sm smrs = do
	let Node _ _ tc evs = getLabel t -- read the supermachines data
	c <- readTVar tc -- read the actual coordinates 
	(ss,_) <- foldM peekdown ([],root t) c -- collect the states from root to her, assertion t == t 
	let k t (i,SMr s r) = do -- insert a machine at location t , the machine has an index i to be identified among twins
		tc <- newTVar (c ++ [length . subForest . tree $ t]) -- the coordinate cell of the new machine
		st <- newTVar s -- the state cell of the new machine
		evs' <- dup'TChan evs -- the event channel 
		let l = Node (SMrt st r) (e,ss ++ [sm],i) tc evs' 
		writeTChan bs l -- schedule the new machine
		return . fromJust . parent . insertDownLast (return l) $ t -- add it to the generator node and return the modified tree 
			-- still located at the generator machine	
	foldM k t $ zip [0..] smrs -- update the tree inserting each new machine 



