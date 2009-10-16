{-# LANGUAGE ExistentialQuantification ,NoMonomorphismRestriction,  MultiParamTypeClasses, FunctionalDependencies, ViewPatterns#-}



module Core (J (..) , rj , E (..), le , SMr (..)  , Module (..), actors)
	where
import Control.Monad
import Data.Typeable
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Maybe
import Data.Either
import Debug.Trace


-- | Events received. the Read and Show constraints (BUG) must be replaced with some more clever serialization class. Also consider to separate unserializable events (HINT) 
data E = forall e . (Show e, Read e, Typeable e) => E e

-- | Query events, these cannot be serialized and will be discarded on serializations, typically queries. Don't use them to modify states !
data J = forall j . Typeable j => J j

-- | Existential state box. This and the other boxes are used to store and move around modules data
data SMs = forall r s e j. (Show e, Read e, Typeable j, Typeable e, Typeable s, Module r s e j, Read s, Show s) 
	=> SMs (Maybe s)
-- | Existential state and environment box
data SMr = forall r s e j. (Show e, Read e, Typeable j, Typeable e, Typeable s, Module r s e j, Read s, Show s) 
	=> SMr (Maybe s) r

-- | Existential mutable state box
data SMt = forall r s e j. (Show e, Read e, Typeable j, Typeable e, Typeable s, Module r s e j, Read s, Show s) 
	=> SMt (TVar (Maybe s))

-- | Existential mutable state and environment box
data SMrt = forall r s e j. (Show e, Read e, Typeable j, Typeable e, Typeable s, Module r s e j, Read s, Show s) 
	=> SMrt (TVar (Maybe s)) r

--------------------------------------------------------------------------
----------------- Modules interface to the library------------------------
--------------------------------------------------------------------------


-- | interface for a module to partecipate , step function is computed on every event received which is the right type :: e for the module. 'r' parameter is the read only environment for the module. A read only parameter is necessary because interactions with supermodules are generally not serializable , usually STM computations. To deserialize a module the 'r' parameter is recreated firing the event responsable for the module creation in a rebuilt context. If 'r' was included in the  mutable state we couldn't  rebuild it without firing all the events caught by the module before serialization.

class Module r s e j | s -> r, s r -> e, s r -> j where
	make :: r -> TVar (Maybe s) -> e -> STM ([SMr],[Either E J])
	query :: r -> Maybe s -> j -> STM [Either E J]

-- | The context for a node holds what is necessary in a sanely rebuilt tree for reborning  a module, aka adding a node to a tree. This data is only there for deserialization.

type Ctx = (E,[SMs])


-- | Unforgeable index of a node. A list of indices to reach the nodes in the graph. Each representing which branch to choose starting from top node. Coo (..) should not be exported to avoid modules forging their positions.
data Coo = Coo [Int] 

-- | Mutable location for a Coo
type TCoo = TVar Coo

-- | The store for the active modules. This is a NTree where in each node we store a mutable cell for the node state, the context for serialization and the active submodules. A Nothing in the state signal a stopped module. Although a module can be stopped its presence in the tree can still be necessary if there are submodules still alive for their serialization. A pruning for full stopped branches is good before every serialization snapshot. 
data Node = Node {v :: SMt , ctx :: Ctx , coo :: TCoo, ns :: [Node]} 

 
-- | Node insertion. To insert a node we travel down the tree to the point of attachment described in c :: Coo , while collecting actual states. This also means we have a fairly high degree of starving as we need silence along the path to the attachment point. The actual states collected, with the passed event e :: E forms the context for the new node. 
insert :: Node -> Coo -> SMt -> E -> STM Node
insert t (Coo is) ts e = do
	let 	f n@(Node x@(SMt tv) ctx tc rs) [] bs = do 
			v <- readTVar tv
			is' <- newTVar . Coo $ is ++ [length rs]
			return . Node x ctx tc $ rs ++ [Node ts (e,bs ++ [SMs v]) is' []]
		f (Node _ _ _  []) _  _ = error "path too long"
		f (Node x@(SMt tv) ctx tc rs) (i:is) bs = do
			v <- readTVar tv
			rs' <- substM rs i $ \t -> f t is (bs ++ [SMs v])
			return . Node x ctx tc $ rs'
	t <- f t is []
	return t

-- | branch forward substitution (TODO : intercept index out of range)
substM :: Monad m => [a] -> Int -> (a -> m a) -> m [a]
substM xs i f = let y:ys = drop i xs in do -- BUG : Irrefutable pattern failed
			y' <- f y
			return $ take i xs ++ [y'] ++ ys 
-- xs !!! n = listToMaybe . drop n $ xs

-- | A channel to store new borning or dieing modules . In addition to the state is the read only part which will remain closed in the spawned thread, as it's not stored in the modules tree
type ControlI = TChan (SMrt,TCoo)

-- | The tree of modules is accessed concurrently by actors for register and unregister actions
type NodeT = TVar Node

-- | Modules register subnodes with this. They pass their mutable position to identify theirself on the tree, the initial state for the submodule along with its only-read data in SMr, and the event responsible for the registering aka the event the are processing (possible SECURITY bug, we should know which event they are processing)
register :: NodeT -> ControlI -> TCoo -> E -> SMr -> STM ()
register t ch c e (SMr s r) = do
	tx <- readTVar t
	cx <- readTVar c
	ts <- newTVar s -- the SMt for the new node	
	ty <- insert tx cx (SMt ts) e
	writeTVar t ty
	writeTChan ch (SMrt ts r,c) 
-- | shortcut for Left . E
le :: (Show a, Read a, Typeable a) => a -> Either E b
le = Left . E

-- | shortcut for Right . J
rj :: (Typeable a1) => a1 -> Either a J
rj = Right . J


-- | the main IO function which fires and kill the actors
actors :: SMr -> IO (Either E J -> IO (), IO E,IO J)
actors sr@(SMr s r) = do
	mevents <- atomically $ newTChan 
	qevents <- atomically $ newTChan 
	control <- atomically $ newTChan 
	tree <- atomically $ do
		ts <- newTVar s
		tc <- newTVar . Coo $ []
		writeTChan control (SMrt ts r, tc)
		newTVar $ Node (SMt ts) (E (),[]) tc []

	forkIO . forever $ do

		(SMrt ts r, tc)  <- atomically $ readTChan control

		mcopy <- atomically $ dup'TChan mevents
		qcopy <- atomically $ dup'TChan qevents
		let send ejs = do
			mapM_ (writeTChan mcopy) $ lefts ejs
			mapM_ (writeTChan qcopy) $ rights ejs

		qth <- forkIO . forever . atomically $ do 
			J j <- readTChan qcopy
			case cast j of
				Nothing -> return ()
				Just j -> do
					ms <- readTVar ts 
					query r ms j >>= send
		forkIO . forever $ do
			kth <- atomically $ do
				E e <- readTChan mcopy
				case cast e of
					Nothing -> return False
					Just e -> do
						(smrs, ejs) <- make r ts e  
						send ejs
						mapM_ (register tree control tc (E e)) smrs
						maybe True (const False) `fmap` readTVar ts 
			when kth $ myThreadId >>= killThread >> killThread qth
			return ()
	let 	input (Left x) = atomically $ writeTChan mevents x
		input (Right x) = atomically $ writeTChan qevents x
	return (input, atomically $ readTChan mevents, atomically $ readTChan qevents)

-- | un duplicatore di canale che copia anche gli eventi presenti
dup'TChan t = do	
	c <- dupTChan t
	let flush = do 
		z <- isEmptyTChan t
		case z of 
			False -> do 	x <- readTChan t 
					fmap (x:) flush
			True -> return []
	rs <- fmap reverse $ flush
	mapM_ (unGetTChan c) rs
	mapM_ (unGetTChan t) rs
	return c

