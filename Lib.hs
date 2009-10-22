module Lib where
import Control.Concurrent.STM
import Data.Traversable as T
import Control.Monad.State
import Data.Tree.Zipper
import Data.Tree



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

process :: Show a => (a -> Bool) -> TreeLoc (Bool,a) -> Maybe (TreeLoc (Bool,a))
process f t = let 	(x,y) = getLabel t 
			k = 	if isLeaf t then 
					if f y then deleteRight t 
					else  right t `mplus` parent t
				else 	if not x then firstChild . setLabel (True,y) $ t 
					else  right t `mplus` parent t
			in (k >>= process f) `mplus` Just t


deleteRight :: Show a => TreeLoc a -> Maybe (TreeLoc a)
deleteRight loc = case rights loc of
	[] -> modifyTree (\t -> t { subForest = lefts loc }) `fmap` parent loc 
	t : ts -> Just loc { tree = t, rights = ts }

prune :: Show a => (a -> Bool) -> TreeLoc a -> Maybe (TreeLoc a) 
prune f = onAugmented ((,) False) snd (process f)

test n m = let 
	t = fromTree $ unfoldTree (\x -> if x < n then (x + 1, [x * 2, x * 3]) else (x + 1,[])) 1 
	Just t' = prune (> m) t
	in do
		putStr . drawTree . fmap show . tree $ t
		putStr . drawTree . fmap show . tree $ t'
