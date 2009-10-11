{-# LANGUAGE DeriveDataTypeable #-}
module Launch 
 where

import Machine
import Control.Concurrent.STM (STM, atomically, readTVar, readTChan, writeTChan, 
	writeTVar , newTVar, newTChan, dupTChan, isEmptyTChan,unGetTChan, TVar, TChan)
import Control.Monad
import Control.Concurrent (forkIO, killThread, ThreadId, myThreadId)
import Data.Maybe (isJust, fromJust, isNothing)
import Control.Exception (handle, throwTo, Exception)
import Data.List (delete)
import Data.Typeable (Typeable,cast)
import Debug.Trace

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

-- una mangiaeventi ha un riferimento a un valore condiviso e una copia della coda di eventi

esecuzione 	:: TChan E -> [SM] 			-- ^ la macchina da provare 
		-> IO ()	
esecuzione es' xs = forM_ xs $ \(SM x') -> do
		es <- atomically $ dup'TChan es'
		x <- atomically $ newTVar x'
		forkIO . forever $ do
			(c,bs) <- atomically $ do 
				E y <- readTChan es -- aspettiamo un evento	
				case cast y of 	
					Nothing -> return $ (False,[]) -- l'evento non era associato allo stato
					Just e -> do
						(ops,(bs,ps)) <- coupleEither `fmap` step x e
						forM_ ps $ writeTChan es -- aggiorniamo la coda eventi
						return $ (ops,bs)
			esecuzione es bs -- lancio delle macchine nuove
			when c $ myThreadId >>= killThread -- morte del thread
		return ()

boot	:: [SM] -- gli stati iniziali
	-> IO (TChan E) -- canale eventi 
boot xs  = do
	c <- atomically $ newTChan -- canale eventi
	esecuzione c xs-- lancio macchine
	return c 

