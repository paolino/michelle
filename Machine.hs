{-# LANGUAGE ExistentialQuantification, FunctionalDependencies, MultiParamTypeClasses  #-}

module Machine where

import Data.Typeable (Typeable, cast)

-- | Cosa produce una macchina quando processa un evento da lei accettato per contratto
data Effect s e  = Effect {
	stato :: Maybe s, 	-- ^ un nuovo stato se continua ad esistere o Nothing se muore
	macchine :: [SM], 	-- ^ un insieme di nuove macchine
	interni :: [I],		-- ^ un insieme di eventi interni
	esterni :: [e]		-- ^ un insieme di eventi esterni
	}

-- | la classe che determina dal tipo dello stato la funzione della macchina e i tipi di ingresso evento e uscita evento
class (Read s, Show s) => SMClass s i e  | s -> i , s -> e where
	step :: s -> Either i e -> Effect s e

-- | la scatola per le macchine
data SM = forall s i e . (Read e, Show e, Typeable e, Typeable i, SMClass s i e) => SM s

instance Show SM where
	show (SM x) = show x

-- | la scatola per gli eventi esterni
data E = forall e. (Read e, Show e, Typeable e) => E e

-- | la scatola per gli eventi interni
data I = forall i . Typeable i => I i

instance Show E where
	show (E x) = show x

-- | l'interfaccia di tentativo di inserimento di un evento in una macchina, se l'evento non e' compatibile ritorna Nothing
-- altrimenti ritorna l'Effect con i valori di uscita opportunamente inscatolati
esecuzione 	:: SM 				-- ^ la macchina da provare 
		-> Either I E			-- ^ l'evento da inserire
		-> Maybe (Maybe SM, [SM],[I],[E])	-- ^ l'effetto dell'inserimento
esecuzione (SM s) (Left (I i)) = cast i >>= Just . r where
	r i =  let 	Effect s' bs ps q = step s (Left i) in 
		(s' >>= Just . SM , bs, ps, map E q)
esecuzione (SM s) (Right (E e)) = cast e >>= Just . r where
	r e =  let 	Effect s' bs ps q = step s (Right e) in 
		(s' >>= Just . SM , bs, ps, map E q)


