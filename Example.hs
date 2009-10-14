{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, MultiParamTypeClasses, NoMonomorphismRestriction #-}
import Control.Monad.State
import Data.Char
import Data.Typeable
import Data.List
import Machine 
import System.IO
import Launch
import Control.Concurrent
import Control.Concurrent.STM
import Debug.Trace

data GQuery = Apri String | Chiudi String deriving (Typeable)
data GState = GState Int [(String,Int)] 	

continue = return $ Response True [] []

instance SMClass GState GQuery where
	step g (Apri s) =  do
		GState k xs <- readTVar g
		case lookup s xs of 
			Nothing -> do
				writeTVar g $ GState (k + 1) ((s,k):xs)
				let test y = do
					GState _ xs <- readTVar g
					return $ y `elem` map snd xs
				return $ Response True [SM $ HState test k 0] []
			_ -> continue

	step g (Chiudi s) = do  
		GState k xs <- readTVar g
		case lookup s xs of 
			Nothing -> continue 
			Just k' ->  writeTVar g (GState k (delete (s,k') xs)) >> continue

data HState = HState (Int -> STM Bool) Int Int
data HQuery = Aggiorna Int (Int  -> Int) | Conto Int (Int -> E) deriving (Typeable)


instance SMClass HState HQuery where
	step h (Aggiorna x u) = do 
		HState t k y <- readTVar h
		case x /= k of
			True -> continue
			False -> do 
				r <- t x
				case r of
					True -> do 
						writeTVar h $ HState t k (u y)
						continue
					False -> return $ Response False [] []
	step h (Conto x f) = do
		HState t k y <- readTVar h
		case x /= k of
			True -> continue
			False -> do 
				return $ Response True [] [f y]		

data Rs = RConto Int deriving (Show,Typeable)

waitRs :: TChan E -> IO ()
waitRs c = do
        E l <- atomically $ readTChan c
        case cast l `asTypeOf` Just (undefined :: Rs) of
                Nothing -> waitRs c 
                Just y -> print y


main = do
	hSetBuffering stdout LineBuffering
	s <- boot [SM $ GState 0 []]
	forkIO $ waitRs s
	mapM_ (\x -> threadDelay 50000 >> atomically (writeTChan s x))
		[	E $ Apri "ciao",
			E $ Aggiorna 0 (+4),
			E $ Aggiorna 0 (+3),
			E $ Conto 0 (E . RConto), 
			E $ Chiudi "ciao"
		]
