
{-# LANGUAGE ExistentialQuantification , MultiParamTypeClasses, FunctionalDependencies #-}




import Data.Maybe

class C a b | a -> b where
	a2b :: a -> b
	a2b = undefined

data A = forall a b . (Read a, Read b, C a b) => A a
data B = forall a b . (Read b, C a b) => B b

maybeRead x = fst `fmap` (listToMaybe $ reads x)

readA :: A -> String -> Maybe A
readA (A x) y = fmap (A . asTypeOf x) $ maybeRead y

readB :: A -> String -> Maybe B
readB (A x) y = fmap (B . asTypeOf (a2b x)) $ maybeRead y
