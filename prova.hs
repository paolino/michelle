import Debug.Trace

data A a = N a [A a] deriving Show

subst xs i f = let y:ys = drop i xs in take i xs ++ f y : ys


insert :: Show a => A a -> [Int] -> ([a] -> a) -> A a
insert t is g = let 
	f (N x rs) [] bs = N x $ rs ++ [N (g $ bs ++ [x]) []]
	f (N _ []) _  _ = error "path too long"
	f (N x rs) (i:is) bs = N x (subst rs i $ \t -> f t is (bs ++ [x]))
	in f t is []

z = N 1 [N 2 [N 3 []],N 4 [N  5 []]]

test = insert z [0,0] sum

insert ::   -> [Int] -> ([a] -> a) -> A a

