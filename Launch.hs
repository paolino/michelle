{-# LANGUAGE DeriveDataTypeable #-}
module Launch (lancia) where

import Machine
import Control.Concurrent.STM (atomically, readTVar, readTChan, writeTChan, 
	writeTVar , newTVar, newTChan, dupTChan, isEmptyTChan,unGetTChan, TVar, TChan)
import Control.Monad
import Control.Concurrent (forkIO, killThread, ThreadId, myThreadId)
import Data.Maybe (isJust, fromJust)
import Control.Exception (handle, throwTo, Exception)
import Data.List (delete)
import Data.Typeable (Typeable)

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

data Serialize = Serialize  deriving (Show,Typeable)

instance Exception Serialize 

modifyTVar x f = readTVar x >>= writeTVar x . f

nuova :: (SM -> IO ()) -> TVar [ThreadId] -> TChan P -> TChan (P,P) -> SM -> IO ThreadId
nuova se ts c d x = forkIO $ do
	t <- atomically $ dup'TChan c -- copiamo il canale di ingresso
	k <- myThreadId -- ci serve per morire
	atomically $ modifyTVar ts (k:)
	let op x = handle (\Serialize -> se x >> op x) $ do 
		(cond,nms) <- atomically $ do
			p <- readTChan t --aspettiamo un nuovo evento
			let 	result = esecuzione x p
			case result of 
				Nothing -> return (Just x,[]) -- il messaggio non era per noi
				Just (mx, ms, ps, q) -> do
					mapM_ (writeTChan t) ps -- nuovi messaggi in broadcast
					when (isJust q) $ writeTChan d (p,fromJust q) -- il risultato della macchina
					return (mx,ms)
		mapM_ (nuova se ts c d) nms -- nuove macchine in azione
		case cond of
			Nothing -> atomically (modifyTVar ts (delete k)) >> killThread k -- tempo di morire
			Just x -> op x
	op x 
		
lancia :: [SM] -> (SM -> IO ()) -> IO (P -> IO (),IO (P,P),IO ())
lancia xs se = do
	c <- atomically $ newTChan
	d <- atomically $ newTChan
	ts <- atomically $ newTVar []
	mapM_ (nuova se ts c d) xs
	return (atomically . writeTChan c, atomically $ readTChan d, atomically (readTVar ts) >>= mapM_ (flip throwTo Serialize))
