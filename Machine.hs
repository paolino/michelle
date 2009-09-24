{-# LANGUAGE ExistentialQuantification, FunctionalDependencies, MultiParamTypeClasses  #-}

module Machine where

import Data.Typeable (Typeable, cast)

-- | Cosa produce una macchina quando processa un evento da lei accettato per contratto
data Effect s q  = Effect {
	stato :: Maybe s, 	-- ^ un nuovo stato se continua ad esistere o Nothing se muore
	macchine :: [SM], 	-- ^ un insieme di nuove macchine
	input :: [P],		-- ^ un insieme di eventi
	output :: Maybe q		-- ^ un valore di output
	}

-- | la classe che determina dal tipo dello stato la funzione della macchina e i tipi di ingresso evento e uscita evento
class (Read s, Show s) => SMClass s p q | s -> p , s -> q where
	step :: s -> p -> Effect s q

-- | la scatola per le macchine
data SM = forall s p q . (Read q, Show q, Typeable q, 
	Read p , Show p, Typeable p, SMClass s p q) => SM s

instance Show SM where
	show (SM x) = show x
-- | la scatola per gli eventi
data P = forall p. (Read p, Show p, Typeable p) => P p

instance Show P where
	show (P x) = show x

-- | l'interfaccia di tentativo di inserimento di un evento in una macchina, se l'evento non e' compatibile ritorna Nothing
-- altrimenti ritorna l'Effect con i valori di uscita opportunamente inscatolati
esecuzione 	:: SM 				-- ^ la macchina da provare 
		-> P 				-- ^ l'evento da inserire
		-> Maybe (Maybe SM, [SM],[P],Maybe P)	-- ^ l'effetto dell'inserimento
esecuzione (SM s) (P p) = cast p >>= Just . r where
	r p =  let 	Effect s' bs ps q = step s p in 
		(s' >>= Just . SM , bs, ps, q >>= Just . P)


