module Lib where
import Control.Concurrent.STM
import Data.Traversable as T
import Control.Monad.State
import Data.Tree.Zipper
import Data.Tree
import Debug.Trace
import Data.Maybe
import Control.Applicative



mapAccumM :: (Traversable t, Monad m) => (a -> b -> m c) -> [b] -> t a -> m (t c)
mapAccumM f xs = flip evalStateT xs . T.mapM k  where
	k y  = do
		x:xs <- get
		put xs
		lift $ f y x 
 
-- | un duplicatore di canale che copia anche gli eventi presenti
dup'TChan t = do	
	c <- dupTChan t
	rs <- contents t
	mapM_ (unGetTChan c) rs
	return c

contents t = do 
	let flush = do 
		z <- isEmptyTChan t
		case z of 
			False -> do 	x <- readTChan t 
					fmap (x:) flush
			True -> return []
	rs <- fmap reverse $ flush
	mapM_ (unGetTChan t) rs
	return rs

onAugmented :: (a -> b) -> (b -> a) -> (TreeLoc b -> Maybe (TreeLoc b)) -> TreeLoc a -> Maybe (TreeLoc a) 
onAugmented fab fba f t = fmap (fromTree . fmap fba . tree) . f . fromTree . fmap fab . tree $ t

onAugmentedM :: (Monad m, Functor m) => (a -> b) -> (b -> a) -> (TreeLoc b -> m (Maybe (TreeLoc b))) -> TreeLoc a -> m (Maybe (TreeLoc a)) 
onAugmentedM fab fba f t = fmap (fmap (fromTree . fmap fba . tree)) .  f . fromTree . fmap fab . tree $ t

process :: (a -> Bool) -> TreeLoc (Bool,a) -> Maybe (TreeLoc (Bool,a))
process f t = 	let 	(x,y) = getLabel t 
			run = (right t `mplus` parent t >>= process f) `mplus` Just t 
		in 	if isLeaf t then 
				if f y then deleteRight t >>= process f
				else run
			else 	if not x then (firstChild . setLabel (True,y) $ t) >>= process f
				else run

processM :: Monad m => (a -> m Bool) -> TreeLoc (Bool,a) -> m (Maybe (TreeLoc (Bool,a)))
processM f t = do
	let 	(x,y) = getLabel t 
		run = case right t `mplus` parent t of 
			Nothing -> return $ Just t
			Just t' -> processM f t' 
	if isLeaf t then do
			g <- f y
			if g then case deleteRight t of
				Nothing -> return Nothing
				Just t' -> processM f t'
				else run
		else 	if not x then case firstChild . setLabel (True,y) $ t of
				Nothing -> return Nothing
				Just t' -> processM f t' 
				else run

deleteRight :: TreeLoc a -> Maybe (TreeLoc a)
deleteRight loc = case rights loc of
	[] -> modifyTree (\t -> t { subForest = lefts loc }) `fmap` parent loc 
	t : ts -> Just loc { tree = t, rights = ts }

prune :: (a -> Bool) -> TreeLoc a -> Maybe (TreeLoc a) 
prune f = onAugmented ((,) False) snd (process f)

pruneM :: (Monad m, Functor m) => (a -> m Bool) -> TreeLoc a -> m (Maybe (TreeLoc a)) 
pruneM f = onAugmentedM ((,) False) snd (processM f)

-----------------------------------------------------------------
reada x q = flip asTypeOf q <$> fst <$> (listToMaybe $ reads x)
readKs :: String -> (String -> k -> Maybe k) -> [k] -> Maybe k
readKs x f = msum . map (f x)

