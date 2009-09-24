
module Concurrent2 where

import Machine
import Control.Concurrent.STM
import Control.Monad
import Control.Concurrent
import Data.Maybe
import Debug.Trace
import GHC.Conc


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


nuova :: TChan P -> TChan (P,P) -> SM -> IO ThreadId
nuova c d x = forkIO $ do
	t <- atomically $ dup'TChan c -- copiamo il canale di ingresso
	k <- myThreadId -- ci serve per morire
	let op x = do 
		(cond,nms) <- atomically $ do
			p <- readTChan t --aspettiamo un nuovo evento
			let 	result = esecuzione x $ trace (show p) p
			case result of 
				Nothing -> return (Just x,[]) -- il messaggio non era per noi
				Just (mx, ms, ps, q) -> do
					mapM_ (writeTChan t) ps -- nuovi messaggi in broadcast
					when (isJust q) $ writeTChan d (p,fromJust q) -- il risultato della macchina
					return (mx,ms)
		mapM_ (nuova c d) nms -- nuove macchine in azione
		case cond of
			Nothing -> killThread k -- tempo di morire
			Just x -> op x
	op x
		
lancia :: [SM] -> IO (P -> IO (),IO (P,P))
lancia xs = do
	c <- atomically $ newTChan
	d <- atomically $ newTChan
	mapM_ (nuova c d) xs
	return (atomically . writeTChan c, atomically $ readTChan d)
