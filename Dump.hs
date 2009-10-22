
module Dump where

import Lib
import CoreZ
import Control.Monad
import Control.Applicative
import Control.Arrow
import Data.Typeable
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Maybe
import Data.Maybe
import Data.List
import Debug.Trace
import Data.Tree.Zipper as Z 
import Data.Tree
import Lib (mapAccumM, dup'TChan, contents, prune)

dump :: Store -> STM ([Ctx],[Restoring])
dump t = do
	let ys = flatten . tree $ pruneM (\(Load (SMrt ts _) _ _ _) -> readTVar ts >>= return . isNothing) t
	fmap unzip . forM ys $ \(Load (SMrt ts _) ctx tc evs) -> do
		s <- readTVar ts
		c <- readTVar tc
		ejs <- contents evs
		return (ctx,(SMs s,ejs))


