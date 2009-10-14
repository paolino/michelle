{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, FlexibleContexts, Rank2Types, FunctionalDependencies, MultiParamTypeClasses  #-}

module Machine (Response (..), SMClass (..) , SM (..), E (..)) where

import Data.Typeable (Typeable, cast)
import Control.Concurrent.STM

-- | Cosa produce una macchina quando processa un evento da lei accettato per contratto
data Response = Response 
	Bool -- ^ vita o morte per la macchina
	[SM] -- ^ nuovi stati
	[E]  -- ^ nuovi eventi

-- | la classe che determina dal tipo dello stato la funzione della macchina e i tipi di ingresso evento, l'effetto viene calcolato  nella monade di transazione (STM) per permettere la composizione con alte macchine
class Typeable e => SMClass s e  | s -> e where
	step :: TVar s -> e -> STM Response

-- | la scatola per le macchine
data SMT = forall s e . SMClass s e => SMT (TVar s)
data SM = forall s e . SMClass s e => SM s

-- | la scatola per gli eventi esterni
data E = forall e . Typeable e => E e




