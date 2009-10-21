
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
import Data.Either
import Data.Maybe
import Data.List
import Debug.Trace
import Data.Tree.Zipper
import Data.Tree
import Lib (mapAccumM, dup'TChan, contents)

dump :: Store -> STM ([Ctx],[Restoring])
dump t = do
	let ys = flatten . tree $ t
	fmap unzip . forM ys $ \(Load (SMrt ts _) ctx tc evs) -> do
		s <- readTVar ts
		c <- readTVar tc
		ejs <- contents evs
		return (ctx,(SMs s,ejs))

prune :: Store -> STM Store 
prune t = undefined
	
