

module Workers (workersThread) where

import Control.Monad (forever, foldM)
import Control.Concurrent.STM (STM,readTVar, writeTChan, writeTVar, readTChan,atomically, TVar, newTVar)
import Control.Concurrent (forkIO)
import Data.Tree (subForest)
import Data.Tree.Zipper (insertDownLast, tree, fromTree, getLabel)
import Lib (dup'TChan)
import Common (Borning, Coo, Events, Store, E, Twin, SMs (..) , Node (..), SMr (..) , SMrt (..), fire, down) 
-- import Debug.Trace

-- | a thread listening to the channel of borning modules . Every time a new module is scheduled to run a new thread is spawned. The thread is closed around its Node . Its job is waiting events on the machine personal channel, process them and modify the Store when the processing produces new machines.
workersThread :: TVar Store -> Borning  -> IO ()
workersThread tt bs = forever $ do 
	Node q _ tc evs <- atomically $ readTChan bs
	forkIO . forever . atomically $ do
		ne <- readTChan evs
		(ms,ejs) <- fire q ne
		case ne of 
			Left e -> do  
				t <- readTVar tt 
				t' <- register bs e t ms
				writeTVar tt t'
			_ -> return ()
		mapM_ (writeTChan evs) ejs
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
		-> [SMr] 	-- ^ the environment and initial state of the new machines
		-> STM Store	-- ^ the modified machine tree
register bs e t smrs = do
	let Node _ _ tc evs = getLabel t -- read the supermachines data
	c <- readTVar tc		-- read the actual coordinates 
	(ss,t) <- foldM peekdown ([],t) c -- collect the states from root to her, assertion t == t 
	let k t (i,SMr r s) = do -- insert a machine at location t , the machine has an index i to be identified among twins
		tc <- newTVar (c ++ [length . subForest . tree $ t]) -- the coordinate cell of the new machine
		st <- newTVar s -- the state cell of the new machine
		evs' <- dup'TChan evs -- the event channel 
		let l = Node (SMrt st r) (e,ss,i) tc evs' 
		writeTChan bs l -- schedule the new machine
		return $ fromJust . parent . insertDownLast (return l) t -- add it to the generator node and return the modified tree 
			-- still located at the generator machine
	foldM k t $ zip [0..] ms -- update the tree inserting each new machine 



