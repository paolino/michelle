import Core
import Data.List
import Data.Maybe
import Control.Concurrent.STM
import Control.Monad
data Command 
	= Apri String
	| Chiudi String
	| Aggiorna Int
	| Fine Int deriving (Show,Read)

data Answers 
	= Conto Int
	| Aperta Int
	| Chiusa Int
	| Morto Int deriving (Show)

noeff = Effect Keep [] [] []

data GState = GState Int [(String,Int)] deriving (Show,Read)	

g :: GState -> SM Command Answers
g (GState k xs) = SM z where
	z (Apri s) = case find ((==s) . fst) xs of 
		Nothing -> Effect (Recond $ g (GState (k + 1) ((s,k):xs))) [h $ HState k 0] [] [Aperta k]
		_ -> noeff
	z (Chiudi s) = case find ((==s) . fst) xs of 
		Nothing -> noeff
		Just (_,k') ->  Effect (Recond $ g (GState k (delete (s,k') xs)))  [] [Fine k'] [Chiusa k']
	z _ = noeff

data HState = HState Int Int deriving (Show,Read)

h :: HState -> SM Command Answers
h (HState k y) = SM z where 
	z (Aggiorna x) = if x /= k then noeff else Effect (Recond $ h (HState k (y +1))) [] [] [Conto (y+1)]
	z (Fine x) = if x /= k then noeff else Effect Destroy [] [] [Morto k]
	z _ = noeff

s = simple [g $ GState 0 []] :: IO (Command -> IO (), IO (Command,[Answers]))

