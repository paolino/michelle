{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, FlexibleContexts, Rank2Types, FunctionalDependencies, MultiParamTypeClasses  #-}

module Machine where

import Data.Typeable (Typeable, cast)
import Control.Concurrent.STM

-- | Cosa produce una macchina quando processa un evento da lei accettato per contratto
type Effect = ([SM],[E])
type Response = Either Effect Effect

coupleEither = either ((,) False) ((,) True) 
-- | la classe che determina dal tipo dello stato la funzione della macchina e i tipi di ingresso evento, l'effetto viene calcolato  nella monade di transazione (STM) per permettere la composizione con alte macchine
class SMClass s e  | s -> e where
	step :: TVar s -> e -> STM Response

-- | la scatola per le macchine
data SMT = forall s e . (Read e, Show e, Typeable e, SMClass s e) => SMT (TVar s)
data SM = forall s e . (Read e, Show e, Typeable e, SMClass s e) => SM s

-- | la scatola per gli eventi esterni
data E = forall e . (Read e, Show e, Typeable e) => E e

instance Show E where
	show (E x) = show x



