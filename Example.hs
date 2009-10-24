
{-# LANGUAGE ScopedTypeVariables, ViewPatterns , FunctionalDependencies,DeriveDataTypeable, MultiParamTypeClasses, NoMonomorphismRestriction #-}
import Control.Monad.State
import Data.Char
import Data.Typeable
import Data.List
import System.IO
import Data.Either
import Control.Concurrent
import Control.Concurrent.STM
import Debug.Trace

import Common
import Application

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
	r0 _ = ()

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
	r0 = undefined
data Rs = RConto Int deriving (Show,Typeable)

waitRs :: IO (Either E J) -> IO ()
waitRs c = do
	q <- c 
	case q of
		Left _ ->  waitRs c
		Right (J l) -> case cast l `asTypeOf` Just (undefined :: Rs) of
			Nothing -> waitRs c 
			Just y -> print y


continue = return ([],[])

h =   program 	[SMs (undefined :: Maybe GState), SMs (undefined :: Maybe HState)]
		[E (undefined :: GEvent), E (undefined :: HMake)]

le = Left . E
rj = Right . J

evs =	[	le $ Apri "ciao",
		le $ Aggiorna 0 4,
		le $ Aggiorna 0 3,
		rj $ Conto 0 (J . RConto), 
		le $ Chiudi "ciao", 
		le $ Aggiorna 0 (-1)
	]

