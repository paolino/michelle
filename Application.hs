{-# LANGUAGE ScopedTypeVariables #-}
module Application
	where

import Common
import Data.Either
import Data.Typeable
import Data.Maybe
{-
toDump :: [SMs] -> String -> Dump
toDump sms x = case reads x of
	[] -> error "parsing error 1"
	[((cs,rs),_)] -> let
		cs = do		(e,sms,i) <- cs
-}

reada x = fst `fmap` (listToMaybe $ reads x)

readSMs :: SMs -> String -> Maybe SMs
readSMs (SMs q) x = fmap (SMs . asTypeOf q) $ reada x

readE :: SMs -> String -> Maybe E
readE (SMs q) x = fmap (E . asTypeOf (s2e q)) $ reada x

toString :: Dump -> String 
toString (cs,rs) = show (cs',rs') where
	cs' = do 	(e,sms,i) <- cs
			return (unE e, map unSMs sms, i) 
	rs' = do 	(s,ejs) <- rs
			return (unSMs s,map unE . lefts $ ejs)
	unSMs (SMs s) = show s
	unE (E e) = show e
{-		
type Restoring = (SMs,[Either E J])
type Ctx = (E,[SMs],Twin)

-- | what we need to control the system
data Handles = Handles {
	input :: Either E J -> IO (),	-- ^ accept an event
	output :: IO (Either E J) ,	-- ^ wait for an event
	dump :: FilePath -> IO (), 	-- ^ TODO 
	load :: FilePath -> IO ()	-- ^ TODO
	}


-- | the main IO function which fires and kill the actors
actors 	:: SMr 		-- ^ environment and state for the root module
	-> IO Handles   -- ^ communication channels with the modules
actors (SMr s r) = do
	events <- atomically $ newTChan 
	control <- atomically $ newTChan 
	tree <- atomically $ do 
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
