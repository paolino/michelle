
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
import Lib (mapAccumM, dup'TChan, contents)

dump :: Store -> STM ([Ctx],[Restoring])
dump t = do
	let ys = flatten . tree $ t
	fmap unzip . forM ys $ \(Load (SMrt ts _) ctx tc evs) -> do
		s <- readTVar ts
		c <- readTVar tc
		ejs <- contents evs
		return (ctx,(SMs s,ejs))


onAugmented :: (a -> b) -> (b -> a) -> (TreeLoc b -> Maybe (TreeLoc b)) -> TreeLoc a -> Maybe (TreeLoc a) 
onAugmented fab fba f t = fmap (fromTree . fmap fba . tree) . f . fromTree . fmap fab . tree $ t

process :: Show a => (a -> Bool) -> TreeLoc (Bool,a) -> Maybe (TreeLoc (Bool,a))
process f t = let 	(x,y) = getLabel t 
			k = 	if isLeaf t then 
					if f y then deleteRight t 
					else  Z.right t `mplus` parent t
				else 	if not x then firstChild . setLabel (True,y) $ t 
					else  Z.right t `mplus` parent t
			in (k >>= process f) `mplus` Just t


deleteRight :: Show a => TreeLoc a -> Maybe (TreeLoc a)
deleteRight loc = case rights loc of
	[] -> modifyTree (\t -> t { subForest = lefts loc }) `fmap` parent loc 
	t : ts -> Just loc { tree = t, rights = ts }

prune :: Show a => (a -> Bool) -> TreeLoc a -> Maybe (TreeLoc a) 
prune f = onAugmented ((,) False) snd (process f)

test n m = let 
	t = fromTree $ unfoldTree (\x -> if x < n then (x + 1, [x * 2, x * 3]) else (x + 1,[])) 1 
	Just t' = prune (> m) t
	in do
		putStr . drawTree . fmap show . tree $ t
		putStr . drawTree . fmap show . tree $ t' 
