{-# LANGUAGE ExistentialQuantification ,NoMonomorphismRestriction,  MultiParamTypeClasses, FunctionalDependencies, ViewPatterns#-}



module CoreZ -- (J (..) , rj , E (..), le , SMr (..)  , Module (..), actors, Handles (..))
	where
import Control.Monad
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
import Data.Tree
import Lib (mapAccumM, dup'TChan, contents)


-- | Events which count. the Read and Show constraints could be replaced with some more clever serialization class. These events are persistent , until processed

data E = forall e . (Show e, Read e, Typeable e) => E e

-- | shortcut for Left . E
le :: (Show a, Read a, Typeable a) => a -> Either E b
le = Left . E

-- | Query events, these cannot be serialized and will be discarded on serializations, typically queries. Don't use them to modify states ! These events are not persistent until processed.

data J = forall j . Typeable j => J j

-- | shortcut for Right . J
rj :: Typeable a1 => a1 -> Either a J
rj = Right . J

-- | A channel for both types of events
type Events = TChan (Either E J)

-- | Existential state box. This and the other boxes are used to store and move around modules data
data SMs = forall r s e j. (Show e, Read e, Typeable j, Typeable e, Typeable s, Module r s e j, Read s, Show s) 
	=> SMs (Maybe s)
-- | Existential state and environment box
data SMr = forall r s e j. (Show e, Read e, Typeable j, Typeable e, Typeable s, Module r s e j, Read s, Show s) 
	=> SMr (Maybe s) r
-- | Existential mutable state box
data SMt = forall r s e j. (Show e, Read e, Typeable j, Typeable e, Typeable s, Module r s e j, Read s, Show s) 
	=> SMt (TVar (Maybe s))
-- | Existential mutable state , environment box
data SMrt = forall r s e j. (Show e, Read e, Typeable j, Typeable e, Typeable s, Module r s e j, Read s, Show s) 
	=> SMrt (TVar (Maybe s)) r 

--------------------------------------------------------------------------
----------------- Modules interface to the library------------------------
--------------------------------------------------------------------------


-- | interface for a module to partecipate , make or query function is computed on every event received which is the right type (:: e) or (:: j) for the module. 'r' parameter is the read only environment for the module. A read only parameter is necessary because interactions with supermodules are generally not serializable , usually STM computations. To deserialize a module the 'r' parameter is recreated firing the event responsable for the module creation in a rebuilt context. If 'r' was included in the  mutable state we couldn't  rebuild it without firing all the events caught by the module before serialization.

class Module r s e j | s -> r, s r -> e, s r -> j where
	make 	:: r 			-- ^ the read-only environment for the module
		-> TVar (Maybe s) 	-- ^ the mutable state for the module, Nothing is for a unresponsive module
		-> e 			-- ^ the event to process
		-> STM ([SMr],[Either E J])	-- ^ a new set of submodules and new events to broadcast
	query 	:: r 			-- ^ the read-only environment for the module
		-> Maybe s 		-- ^ the state in readonly fashon
		-> j 			-- ^ the event to process
		-> STM [Either E J]	-- ^ new events to broadcast
	-- zeroenv :: r			-- ^ necessary only for the root node

-- | simple wrapper around make/query calls. Disambiguate the type of event to process and choose the right call
fire	:: SMrt			-- ^ environment and state for the module to accept the event 
	-> Either E J		-- ^ the event to process
	-> STM ([SMr],[Either E J])

fire  (SMrt ts r) (Left (E e)) =  case cast e of
	Nothing -> return ([],[])
	Just e -> make r ts e  
fire (SMrt ts r) (Right (J j)) = case cast j of
	Nothing -> return ([],[])
	Just j -> do 
		ms <- readTVar ts
		(,) [] `fmap` query r ms j

-- | identify a module among the others, when they are resulting from a Module make action
type Twin = Int
-- | The context for a node holds what is necessary in a sanely rebuilt tree for reborning  a module, aka adding a node to a tree. This data is only there for deserialization.'E' is the event happened, which should be serializable. [SMs] are the states of the parents collected during 'r' creation. see insert. Twin identify the module among its brothers, on creation time.
type Ctx = (E,[SMs],Twin)

-- | Index of a node. A list of indices to reach the nodes in the graph. Each representing which branch to choose starting from top node. 
type Coo = [Int] 

-- | Mutable location for a Coo. Mutability is necessary as the tree can be pruned. This operation changes a well defined part of the tree coordinates
type TCoo = TVar Coo

-- | The memory for the modules. We store a mutable cell for the node state, the creation context and its mutable coordinates . A Nothing in the state signal a stopped module. Although a module can be stopped its presence in the tree can still be necessary if there are submodules still alive for their serialization. A pruning for full stopped branches is good before every serialization snapshot. 
data Load = Load 
	SMrt  	-- ^ the cell state of the referenced module
	Ctx  	-- ^ its creation context
	TCoo 	-- ^ its coordinates cell, useful for communication between module thread and tree
	Events 	-- ^ its duplicate of the events channel
	 
tstate (Load x _ _ _) = x
-- | Restoring operations involve some complex movement on the memory module tree. We use a zipper structure to relief some programming weight
type Store = TreeLoc Load

-- | module istantiation values. This can happen after the tree has been rebuilt processing all Creations
type Restoring = (SMs,[Either E J])

-- | channel for modules waiting to start
type Borning = TChan Load

-- | setting  state on a store node. The node is already there so its type is already fixed
poke :: SMs -> Store -> STM ()
poke (SMs s) t = do 
	SMrt st  _ <- return . tstate . getLabel $ t 
	case cast s of 
		Nothing -> error "deserialization error, trying to push back a state"
		Just s -> writeTVar st s 

-- | going down a level in the store
down :: Monad m => Twin -> Store -> m Store
down j t = case getChild j t of 
		Nothing -> error "resuming error, index out of range in selecting path" 
		Just t' -> return t'


{-
-- | branch forward substitution (TODO : intercept index out of range)
substM :: Monad m => [a] -> Int -> (a -> m a) -> m [a]
substM xs i f = let y:ys = drop i xs in do -- BUG : Irrefutable pattern failed to avoi here
			y' <- f y
			return $ take i xs ++ [y'] ++ ys 
-- xs !!! n = listToMaybe . drop n $ xs



-- | The tree of modules is accessed concurrently by actors for register and unregister actions
type NodeT = TVar Node

-- | Modules register subnodes with this. They pass their mutable position to identify theirself on the tree, the initial state for the submodule along with its only-read data in SMr, and the event responsible for the registering aka the event the are processing (possible SECURITY bug, we should know which event they are processing). To insert a node we travel down the tree to the point of attachment described in c :: Coo , while collecting actual states. This also means we have a fairly high degree of starving as we need silence along the path to the attachment point. The actual states collected, with the passed event e :: E forms the context for the new node. 

register 	:: Node 	-- ^ the tree which the module is the top
		-> Borning
		-> Coo		-- ^ coordinates of the mother module cell
		-> Either E J		-- ^ the guilty event for this insertion                                    	
		-> SMr		-- ^ environment and starting data for the new submodule 
		-> STM Node	-- ^ 		

register t control is (Left e) (SMr s r) = do
	ts <- newTVar s -- the SMt for the new node
	maiale <- newTVar undefined	-- can't we use runContT ? 
	let 	f n@(Node x@(SMt tv) ctx tc ch rs) [] bs = do 
			v <- readTVar tv
			is' <- newTVar $ is ++ [length rs]
			ch' <- dup'TChan ch
			writeTVar maiale (is',ch')
			return . Node x ctx tc ch $ rs ++ [Node (SMt ts)  (e,bs ++ [SMs v]) is' ch' []]
		f (Node _ _ _ _ []) _  _ = error "path too long"
		f (Node x@(SMt tv) ctx tc ch rs) (i:is) bs = do
			v <- readTVar tv
			rs' <- substM rs i $ \t -> f t is (bs ++ [SMs v])
			return . Node x ctx tc ch $ rs'
	t' <- f t is []
	(is',ch') <- readTVar maiale  
	writeTChan control (SMrt ts r,is',ch')	
	return t'


-- | flattening the tree
flatten :: Node -> STM [Serial]
flatten (Node (SMt x) ctx tc ch rs) = do
	ejs <- contents ch
	c <- readTVar tc
	s <- readTVar x 
	(Serial (SMs s) c ctx ejs :) `fmap` concat `fmap` mapM flatten rs 

-- | rebuild a tree
rebuild :: Events -> Borning -> [Serial] -> STM Node
rebuild = undefined

-- | what we need to control the system
data Handles = Handles {
	input :: Either E J -> IO (),	-- ^ accept an event
	output :: IO (Either E J) ,	-- ^ wait for an event
	dump :: IO [(Ctx,[E])], 	-- ^ TODO 
	load :: [(Ctx,[E])] -> IO ()	-- ^ TODO
	}


-- | the main IO function which fires and kill the actors
actors 	:: SMr 		-- ^ environment and state for the root module
	-> IO Handles   -- ^ communication channels with the modules
actors (SMr s r) = do
	events <- atomically $ newTChan 
	control <- atomically $ newTChan 
	tree <- atomically $ do 
		events' <- dup'TChan events
		ts <- newTVar s
		tc <- newTVar []
		writeTChan control (SMrt ts r, tc, events') 
		newTVar $ Node (SMt ts) (E (),[]) tc events' [] 
	forkIO . forever $ do
		(smrts @(SMrt ts r) , tc, ch )  <- atomically $ readTChan control
		forkIO . forever $ do
			kth <- atomically $ do
					ej <- readTChan ch 
					(smrs,ejs) <- fire smrts ej
					mapM_ (writeTChan events) ejs
					is <- readTVar tc
					t <- readTVar tree
					t' <- foldM (\t -> register t control is ej) t smrs
					writeTVar tree t'
					maybe True (const False) `fmap` readTVar ts 
			when kth $ myThreadId >>= killThread 
			return ()
		
	return $ Handles 
		(atomically . writeTChan events)
		(atomically $ readTChan events)
		undefined
		undefined

-}
