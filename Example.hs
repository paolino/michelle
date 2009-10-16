
{-# LANGUAGE ScopedTypeVariables, ViewPatterns , FunctionalDependencies,DeriveDataTypeable, MultiParamTypeClasses, NoMonomorphismRestriction #-}
import Control.Monad.State
import Data.Char
import Data.Typeable
import Data.List
import Core 
import System.IO
import Data.Either
import Launch
import Control.Concurrent
import Control.Concurrent.STM
import Debug.Trace

data GEvent = Apri String | Chiudi String deriving (Read,Show,Typeable)
data GState = GState Int [(String,Int)] deriving (Show,Read,Typeable)	


instance Module () GState GEvent () where
	make _ g (Apri s) =  do
		Just (GState k xs) <- readTVar g
		case lookup s xs of 
			Nothing -> do
				writeTVar g . Just $ GState (k + 1) ((s,k):xs)
				let test y = 
					maybe False (\ (GState _ (map snd -> xs)) -> y `elem` xs) `fmap` readTVar g
				return $ ([SMr (Just $ HState k 0) (HEnv test)],[])
			_ -> continue

	make _ g (Chiudi s) = do  
		Just (GState k xs) <- readTVar g
		case lookup s xs of 
			Nothing -> continue 
			Just k' -> writeTVar g (Just $ GState k (delete (s,k') xs)) >> continue
	query = undefined

data HState = HState Int Int deriving (Read,Show,Typeable)
data HEnv = HEnv (Int -> STM Bool) 
data HMake = Aggiorna Int Int  deriving (Read,Show,Typeable)
data HQuery = Conto Int (Int -> J) deriving Typeable

instance Module HEnv HState HMake HQuery where
	make (HEnv t) h (Aggiorna x u) = do 
		Just (HState k y) <- readTVar h
		case x /= k of
			True -> continue
			False -> do 
				r <- t x
				writeTVar h $ case r of
					True -> Just $ HState k (y + u)
					False -> Nothing 
				continue
	query _ (Just (HState k y)) (Conto x f) = do
		case x /= k of
			True -> return []
			False -> return [Right $ f y]		

data Rs = RConto Int deriving (Show,Typeable)

waitRs :: IO J -> IO ()
waitRs c = do
	J l <- c 
        case cast l `asTypeOf` Just (undefined :: Rs) of
                Nothing -> waitRs c 
                Just y -> print y


continue = return ([],[])
evs =	[	Left . E $ Apri "ciao",
		Left . E $ Aggiorna 0 4,
		Left . E $ Aggiorna 0 3,
		Right . J $ Conto 0 (J . RConto), 
		Left . E $ Chiudi "ciao", 
		Left . E $ Aggiorna 0 (-1)
	]
main = do
	hSetBuffering stdout LineBuffering
	(input,_,oqr) <- actors $ SMr (Just $ GState 0 []) ()
	forkIO . forM_ evs $ \x -> do
		-- threadDelay 50000 
		input x 

	waitRs oqr
