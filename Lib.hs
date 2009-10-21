module Lib where
import Control.Concurrent.STM
import Data.Traversable as T
import Control.Monad.State


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

