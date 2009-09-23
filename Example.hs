{-# LANGUAGE ExistentialQuantification, FunctionalDependencies , TypeSynonymInstances, ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable #-}
import Control.Monad.State
import Data.Char
import Data.Dynamic
import Data.List

 
data Effect s q  = Effect {
	stato :: Maybe s,
	macchine :: [B],
	input :: [C],
	output :: [q]
	}

class (Read s, Show s) => SMClass s p q | s -> p , s -> q where
	step :: s -> p -> Effect s q

data B = forall s p q . (Read q, Show q, Typeable q, 
	Read p , Show p, Typeable p, SMClass s p q) => B s

data C = forall p. (Read p, Show p, Typeable p) => C p
instance Show C where
	show (C x) = show x


data GQuery = Apri String | Chiudi String deriving (Show,Read,Typeable)
data GResult = Aperta Int | Chiusa Int deriving (Show,Read,Typeable)
data GState = GState Int [(String,Int)] deriving (Show,Read)	

instance SMClass GState GQuery GResult  where
	step g@(GState k xs) (Apri s) = case find ((==s) . fst) xs of 
		Nothing -> Effect (Just $ GState (k + 1) ((s,k):xs)) [B $ HState k 0] [] [Aperta k]
		_ -> Effect (Just g) [] [] [] 
	step g@(GState k xs)  (Chiudi s) = case find ((==s) . fst) xs of 
		Nothing -> Effect (Just g) [] [] []
		Just (_,k') ->  Effect (Just $ GState k (delete (s,k') xs)) [] [C $ Fine k'] [Chiusa k']

data HState = HState Int Int deriving (Show,Read)
data HQuery = Aggiorna Int | Fine Int deriving (Show,Read,Typeable)
data HResult = Conto Int | Morto Int deriving (Show,Read,Typeable)

instance SMClass HState HQuery HResult where
	step h@(HState k y) (Aggiorna x) = case x /= k of
		True -> Effect (Just h) [] [] []
		False -> Effect (Just $ HState k (y +1)) [] [] [Conto (y+1)]
	step h@(HState k y) (Fine x) = case x /= k of
		True -> Effect (Just h) [] [] []
		False -> Effect Nothing [] [] [Morto k]


--- sequential runner ---------


gira :: B -> C -> Maybe (Maybe B, [B],[C],(C,[C]))
gira (B s) (C p) = cast p >>= \p -> 
	let 	Effect s' bs ps qs = step s p in Just
		(maybe Nothing (Just . B) s',bs,ps,(C p, map C qs))
giras :: [B] -> C -> ([B],[C],[(C,[C])])
giras bs c = let 
	w b = maybe (Just b,[],[],(c,[])) id (gira b c)
	f (b,nbs,nps,nq) (bs,ps,qs) = (bs ++ nbs ++ maybe [] return b, ps ++ nps, qs ++ case nq of (_,[]) -> []; otherwise -> [nq])
	in foldr f ([],[],[])  . map w $ bs

tutto :: [C] -> [B] -> [(C,[C])]
tutto [] _ = []
tutto (x:xs) bs = let (bs',ps,qs) = giras bs x in qs ++ tutto (xs ++ ps) bs'
