{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses #-}
import Control.Monad.State
import Data.Char
import Data.Dynamic
import Data.List
import Machine 
import System.IO
import Launch
import Control.Concurrent

data GQuery = QAperti ([(String,Int)] -> P)| Apri String | Chiudi String deriving (Typeable)
data GResult =  Aperta Int | Chiusa Int deriving (Show,Read,Typeable)
data GState = GState Int [(String,Int)] deriving (Show,Read)	


instance SMClass GState GQuery GResult  where
	step g@(GState k xs) (Apri s) = case find ((==s) . fst) xs of 
		Nothing -> Effect (Just $ GState (k + 1) ((s,k):xs)) [SM $ HState k 0] [] (Just $ Aperta k)
		_ -> Effect (Just g) [] [] Nothing 
	step g@(GState k xs)  (Chiudi s) = case find ((==s) . fst) xs of 
		Nothing -> Effect (Just g) [] [] Nothing
		Just (_,k') ->  Effect (Just $ GState k (delete (s,k') xs)) [] [P $ Fine k'] (Just $ Chiusa k')
	step g@(GState _ xs) (QAperti f) = Effect (Just g) [] [f xs]  Nothing

data HState = HState Int Int deriving (Show,Read)
data HQuery = Aggiorna Int | Fine Int | CiSono Bool deriving (Show,Read,Typeable)
data HResult = Conto Int | Morto Int | Yep deriving (Show,Read,Typeable)

instance SMClass HState HQuery HResult where
	step h@(HState k y) (CiSono t) = case t of 
		True -> Effect (Just h) [] [] (Just Yep)
		False -> Effect Nothing [] [] Nothing
	step h@(HState k y) (Aggiorna x) = case x /= k of
		True -> Effect (Just h) [] [] Nothing
		False -> Effect (Just $ HState k (y +1)) [] [P . QAperti  $ P . CiSono . elem k . map snd] (Just $ Conto (y+1))
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
