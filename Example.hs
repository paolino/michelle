{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses #-}
import Control.Monad.State
import Data.Char
import Data.Typeable
import Data.List
import Machine 
import System.IO
import Launch
import Control.Concurrent
import Control.Concurrent.STM

data GQuery = Apri String | Chiudi String | Aperta Int | Chiusa Int deriving (Show,Read,Typeable)
data GState = GState Int [(String,Int)] 	

aperti :: GState -> Maybe (STM [String])
aperti (SM x = cast x >>= f r where
	f x = map fst `fmap` readTVar x 

instance SMClass GState GQuery where
	step g@(GState k xs) (Apri s) = case find ((==s) . fst) xs of 
		Nothing -> Effect (Just $ GState (k + 1) ((s,k):xs)) [SM $ HState (k 0] [E $ Aperta k]
		_ -> Effect (return $ Just g) [] [] 
	step g@(GState k xs)  (Chiudi s) = case find ((==s) . fst) xs of 
		Nothing -> Effect (return $ Just g) [] [] 
		Just (_,k') ->  Effect (return . Just $ GState k (delete (s,k') xs)) [] [E $ Fine k'] 

data HState = HState (Int -> STM Bool) Int Int
data HQuery = Aggiorna Int | Fine Int | Conto Int deriving (Show,Read,Typeable)

instance SMClass HState HQuery where
	step h@(HState t k y) (Aggiorna x) = case x /= k of
		True -> Effect (Just h) [] [] Nothing
		False -> Effect (Just $ HState k (y + 1)) [] [P . QAperti  $ P . CiSono . elem k . map snd] (Just $ Conto (y+1))
	step h@(HState k y) (Fine x) = case x /= k of
		True -> Effect (Just h) [] [] Nothing
		False -> Effect Nothing [] [] (Just $ Morto k)


provaeventi = [P $ Apri "ciao",P $ Aggiorna 0,P $ Aggiorna 0,P $ Chiudi "ciao"] 
provastati = [SM $ GState 0 []]

main = do
	hSetBuffering stdout LineBuffering
	(s,l,q) <- lancia provastati print
	threadDelay 500000 >> q
	mapM_ s [P $ Apri "ciao",P $ Aggiorna 0,P $ Aggiorna 0]
	threadDelay 500000 >> q
	s . P $ Chiudi "ciao"
	threadDelay 500000 >> q
	replicateM 4 $ l >>= print
