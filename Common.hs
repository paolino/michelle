{-# LANGUAGE Rank2Types, ExistentialQuantification ,NoMonomorphismRestriction,  MultiParamTypeClasses, FunctionalDependencies, ViewPatterns#-}



module Common -- (J (..) , rj , E (..), le , SMr (..)  , Module (..), actors, Handles (..))
	where

import Data.Typeable (cast, Typeable)
import Control.Concurrent.STM (TVar, TChan, readTVar, STM)
import Data.Tree.Zipper (TreeLoc, getChild)


-- | Events which count. the Read and Show constraints could be replaced with some more clever serialization class. These events are persistent , until processed

data E = forall e . (Show e, Read e, Typeable e) => E  {unE :: e}
instance Show E where
	show (E e) = show e

-- | shortcut for Left . E
-- le :: (forall e s r j . (Show e, Read e, Typeable e, Module r s e j) => e) -> Either E b
-- le = Left . E

-- | Query events, these cannot be serialized and will be discarded on serializations, typically queries. Don't use them to modify states ! These events are not persistent until processed.

data J = forall j . Typeable j => J j

-- | shortcut for Right . J
-- rj :: Typeable a1 => a1 -> Either a J
-- rj  = Right . J

-- | A channel for both types of events
type Events = TChan (Either E J)

-- | Existential state box. This and the other boxes are used to store and move around modules data
data SMs = forall r s e j. (Show e, Read e, Typeable j, Typeable e, Typeable s, Module r s e j, Read s, Show s) 
	=> SMs (Maybe s)
instance Show SMs where
	show (SMs s) = show s
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
	s2e 	:: Maybe s -> e 
	s2e 	= undefined

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
data Node = Node 
	SMrt  	-- ^ the cell state of the referenced module
	Ctx  	-- ^ its creation context
	TCoo 	-- ^ its coordinates cell, useful for communication between module thread and tree
	Events 	-- ^ its duplicate of the events channel
	 
tstate (Node x _ _ _) = x
-- | Restoring operations involve some complex movement on the memory module tree. We use a zipper structure to relief some programming weight
type Store = TreeLoc Node

-- | module istantiation values. This can happen after the tree has been rebuilt processing all Creations
type Restoring = (SMs,[Either E J])

-- | channel for modules waiting to start
type Borning = TChan Node


-- | going down a level in the store
down :: Monad m => Twin -> Store -> m Store
down j t = case getChild j t of 
		Nothing -> error "resuming error, index out of range in selecting path" 
		Just t' -> return t'

-- | the persistence type, from this tyoe we are able to rebuild the machine tree
type Dump = ([Ctx],[Restoring])



