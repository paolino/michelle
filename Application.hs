{-# LANGUAGE ScopedTypeVariables #-}
module Application
	where

import Common
import Lib
import Data.Either
import Data.Typeable
import Data.Maybe
import Control.Monad
import Control.Applicative


data ProxySMs s = ProxySMs
data ProxyE e = ProxyE 

toProxyE :: e -> Proxy
toProxyE e = 
toDump :: [SMs] -> [E] -> String -> Dump
toDump sms es x = case reads x of
	[] -> error ":structure messed up"
	[((cs,rs),_)] -> let
		cs' = [(readAnE e, map readAnSMs sms, i) | (e,sms,i) <- cs]
		rs' = [(readAnSMs s, map (Left . readAnE) es) | (s,es) <- rs]
		in (cs',rs')
	where 	readAnE e = case readKs e readE es of 
			Nothing -> error $ "no event type for " ++ show e
			Just e -> e
		readAnSMs sm = case readKs sm readSMs sms of
			Nothing -> error $ "no state type for " ++ show sm
			Just sm -> sm
		readSMs x (SMs q) = SMs <$> reada x q
		readE x (E q) = E <$> reada x q

toString :: Dump -> String 
toString (cs,rs) = show (cs',rs') where
	cs' = do 	(e,sms,i) <- cs
			return (unE e, map unSMs sms, i) 
	rs' = do 	(s,ejs) <- rs
			return (unSMs s,map unE . lefts $ ejs)
	unSMs (SMs s) = show s
	unE (E e) = show e


	

-- | what we need to control the system
data Handles = Handles {
	input :: Either E J -> IO (),	-- ^ accept an event
	output :: IO (Either E J) ,	-- ^ wait for an event
	dump :: FilePath -> IO (), 	-- ^ TODO 
	boot :: FilePath -> IO ()	-- ^ TODO
	}


-- | the main IO function which fires and kill the actors
program :: [SM] -> [E] -> IO Handles   -- ^ communication channels with the modules
program (sm:sms) es  = do
	events <- atomically $ newTChan  -- events common channel (dump channel)
	control <- atomically $ newTChan  -- borning machines channel
	tree <- newTVar $ fromTree (return undefined)
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
