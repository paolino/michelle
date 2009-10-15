{-# LANGUAGE ExistentialQuantification , MultiParamTypeClasses, FunctionalDependencies, ViewPatterns#-}



module Machine2 
	where
import Control.Monad
import Data.Typeable
import Control.Concurrent.STM
import Control.Monad.Maybe


data E = forall e . Typeable e => E e

data SMs = forall r s e . (Typeable e, Typeable s, SMClass r s e, Read s, Show s) 
	=> SMs s
data SMr = forall r s e . (Typeable e, Typeable s, SMClass r s e, Read s, Show s) 
	=> SMr s r
data SMt = forall r s e . (Typeable e, Typeable s, SMClass r s e, Read s, Show s) 
	=> SMt (TVar s)
data SMrt = forall r s e . (Typeable e, Typeable s, SMClass r s e, Read s, Show s) 
	=> SMt (TVar s) r

data Response = Response
	Bool -- se è il caso di morire )??
	[SMr] -- eventuale figli
	[E] -- eventi di risposta

class SMClass r s e | s -> r, s -> e
	where
	step :: (r, TVar s) -> e -> STM Response  

-- | The context for a node holds what is necessary in a sanely rebuilt tree for reborning  a module, aka adding a node to a tree. This data is only there for deserialization.
type Ctx = (E,[SMs])

-- | The store for the active modules. This is a NTree where in each node we store a mutable cell for the node state, the context for serialization and the active submodules. The ThreadId is necessary to gc the interceptor after an unregister
data Node = Node {v :: SMt , ctx :: Ctx , thread :: Maybe ThreadId , coo :: TCoo, ns :: [Node]} 

-- | Unforgeable index of a node. A list of indices to reach the nodes in the graph. Each representing which branch to choose starting from top node. Coo (..) should not be exported to avoid modules forging their positions.
data Coo = Coo [Int]  

-- | Node insertion. To insert a node we travel down the tree to the point of attachment described in c :: Coo , while collecting actual states. This also means we have a fairly high degree of starving as we need silence along the path to the attachment point. The actual states collected, with the passed event e :: E forms the context for the new node. 
insert :: Node -> Coo -> SMs -> E -> STM Node
insert t (Coo c) (SMs s) e = do
	smts <- SMt `fmap` newTVar s -- the SMt for the new node	
	let 	f (Node x@(SMt tv) ctx th tc rs) [] bs = do 
			v <- readTVar tv
			return . Node x ctx th tc $ rs ++ [Node smts (e,bs ++ [SMs v]) []]
		f (Node _ _ []) _  _ = error "path too long"
		f (Node x@(SMt tv) ctx th rs tc) (i:is) bs = do
			v <- readTVar tv
			rs' <- substM rs i $ \t -> f t is (bs ++ [SMs v])
			return . Node x ctx th tc $ rs'
	f t is []

-- | branch forward substitution (TODO : intercept index out of range)
substM :: Monad m => [a] -> Int -> (a -> m a) -> m [a]
substM xs i f = let y:ys = drop i xs in do -- BUG : Irrefutable pattern failed
			y' <- f y
			return $ take i xs ++ [y'] ++ ys 
-- xs !!! n = listToMaybe . drop n $ xs

-- | A channel to store new borning or dieing modules . In addition to the state is the read only part which will remain closed in the spawned thread, as it's not stored in the modules tree
type ControlI = TChan (Either ThreadId SMrt)

-- | Mutable location for a Coo
type TCoo = TVar Coo

-- | Modules register subnodes with this. They pass their mutable position to identify theirself on the tree, the initial state for the submodule along with its only-read data in SMr, and the event responsible for the registering aka the event the are processing (possible SECURITY bug, we should know which event they are processing)
register :: TCoo -> SMr -> E -> ReaderT Borning STM ()
register = undefined

-- | Modules can unregister themselves from the tree. 
unregister :: TCoo -> ReaderT 
-- register :: SMt -> Node -> Path -> Node

{-
f :: SMc  -> Node -> (Maybe SMc,[Node])
f (SMc s) (Foglia (SMc s'))  = (fmap SMc $ cast s' `asTypeOf` Just s,[])
f cs@(SMc s) (Nodo (SMc s') rs) = maybe (msum . map (f cs) $ rs) (Just . SMc)  
		(cast s' `asTypeOf` Just s)

-- path :: [SMc] -> Node -> Maybe SMc
-- path [] _ -
-}
