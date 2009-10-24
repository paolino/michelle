{-# LANGUAGE ScopedTypeVariables #-}
module Application (program, Handles (..))
	where

import Common
import Lib
import Data.Either
import Data.Typeable
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Workers
import Dump
import Restore

data ProxySMs s = ProxySMs
data ProxyE e = ProxyE 

toDump :: [SMs] -> [E] -> String -> Dump
toDump sms es x = case reads x of
	[] -> error "structure messed up"
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
data Handles = Handles 
	(Either E J -> IO ())	-- ^ accept an event
	(IO (Either E J)) 	-- ^ wait for an event
	(FilePath -> IO ())	-- ^ dump the michelle to a file
	(FilePath -> IO ())	-- ^ boot michelle from file
	(IO ())			-- ^ turn off michelle
	


-- | the main IO function which fires and kill the actors
program 	:: [SMs] -- ^ state types
		-> [E]  -- ^ event types
		-> IO Handles   -- ^ communication channels with the modules
program sms es  = do
	events <- atomically $ newTChan  -- events common channel (dump channel)
	control <- atomically $ newTChan  -- borning machines channel
	tree <- atomically $ newTVar Nothing
	forkIO $ workersThread tree control
	let 	boot file = do
			d <- toDump sms es <$> readFile file  
			atomically $ newStore control events d >>= writeTVar tree . Just
		dump file = do
			t <- atomically $ readTVar tree
			case t of
				Nothing -> print "could not dump a service not started"
				Just t -> do 	d <- atomically $ do
							(t,d) <- newDump t 
							writeTVar tree (Just t)
							return d
						writeFile file (toString d)
	return $ Handles 
		(atomically . writeTChan events)
		(atomically $ readTChan events)
		dump
		boot
		(atomically $ writeTVar tree Nothing)

