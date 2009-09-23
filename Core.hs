
-- | Un servizio concepito dalla parte della modularita', del protocollo , della responsabilita'

module Core where


import Control.Concurrent.STM
import Control.Monad
import Control.Concurrent

-- | parte di un effetto che riguarda la macchina stessa
data ME p q
	= Keep 		-- ^ la macchina non e' cambiata
	| Destroy 	-- ^ la macchina non esiste più
	| Recond (SM p q) -- ^ la macchina si e' rigenerata

-- | Un effetto di una state machine
data Effect p q = Effect 
	(ME p q) -- effetto sulla macchina
	[SM p q]  -- ^ nuove macchine
	[p] -- ^ nuovi messaggi
	[q] -- ^ risposte
	

-- | una state machine e' una funzione da un messaggio a un effetto	
data SM p q = SM (p -> Effect p q)

-- | come ci ricordiamo una macchina
type TSM p q = TVar (Maybe (SM p q))

-- | la piscina delle macchine
type TSMPool p q =  TVar [TSM p q]

-- | eliminazione delle macchine inesistenti dalla piscina
cleanPool :: PlayGround p l  -> STM ()
cleanPool (PlayGround x _ _ _ _) = do
	ms <- readTVar x
	ms' <- flip filterM ms $ \x -> readTVar x >>= return . maybe False (const $ True)
	writeTVar x ms'

-- | introduzione di nuove macchine nella piscina, se non sono state create nuove macchine si ferma
feedPool ::PlayGround p q  -> STM ()
feedPool pl@(PlayGround y _ _ x _) = do
	r <- readTChan x  >>= newTVar . Just
	cleanPool pl
	readTVar y >>= writeTVar y . (r:) 

-- | Il thread che mantiene la piscina di macchine aggiornata e pulita
poolControl :: PlayGround p l -> IO ThreadId
poolControl = forkIO . forever . atomically . feedPool 
	
-- | passa un messaggio ad una macchina
stepT :: PlayGround p q -> p -> TSM p q -> STM ()
stepT (PlayGround _ z zq x _) p y = do
	mf <- readTVar y
	case mf of 
		Nothing -> return ()
		Just (SM f) -> do 
			let Effect me nms msgs qs = f p -- esecuzione step
			case me of -- analisi effetto su se stessa
				Recond sm -> writeTVar y (Just sm) 
				Destroy -> writeTVar y Nothing
				_ -> return ()
			mapM_  (writeTChan x) nms
			mapM_  (writeTChan z) msgs
			when (not $ null qs) $ writeTChan zq (p,qs)	

-- | passa un messaggio a tutte le macchine, ognuna nel suo thread separato, raccoglie le risposte nella coda fornita
step :: PlayGround p q -> p -> IO [ThreadId]
step pl@(PlayGround z _ _ _ _)  p = do
	ms <- atomically $ readTVar z
	forM ms $  forkIO . atomically . stepT pl p 

-- | thread che consuma i messaggi
consumer :: PlayGround p q -> IO ThreadId
consumer pl@(PlayGround _ y _ _ t) = forkIO . forever $ atomically (readTChan y) >>= step pl >>= atomically . mapM (writeTChan t) 
	 

data PlayGround p q = PlayGround 
	(TSMPool p q)
	(TChan p)
	(TChan (p,[q]))
	(TChan (SM p q))
	(TChan (ThreadId))

simple :: [SM p q] -> IO (p -> IO (), IO (p,[q]))
simple gs = do
	pool  <- mapM (newTVarIO . Just) gs >>= newTVarIO 
	ps <- newTChanIO 
	nms <- newTChanIO
	qs <- newTChanIO
	ths <- newTChanIO
	let pl = PlayGround pool ps qs nms ths  
	forkIO . forever . atomically $ readTChan ths 
	poolControl pl
	consumer pl
	return (atomically . writeTChan ps,atomically (readTChan qs))



