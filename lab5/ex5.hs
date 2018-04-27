{-# LANGUAGE DeriveFunctor #-}
newtype Box a = MkBox a deriving (Show, Functor)

--instance Functor Box where
  -- fmap f (MkBox x) = MkBox (f x)

data MyList a = EmptyList 
                | Cons a (MyList a) deriving (Show, Functor)

--instance Functor MyList where
  --fmap _ EmptyList = EmptyList
  --fmap f (Cons x mxs) = Cons (f x)(fmap f mxs)

data BinTree a = EmptyBT
                 | NodeBT a (BinTree a)(BinTree a)
                 deriving Show

instance Functor BinTree where
  fmap _ EmptyBT = EmptyBT
  fmap f (NodeBT x lt rt) = NodeBT (f x)(fmap f lt)(fmap f rt)

newtype Pair b a = Pair {getPair :: (a,b) } 

instance Functor (Pair c) where
  fmap f (Pair (x,y)) = Pair ( f x, y)

data Tree2 a = EmptyT2 | Leaf a | Node (Tree2 a) a (Tree2 a) deriving Show

instance Functor Tree2 where
  fmap _ EmptyT2 = EmptyT2
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node lt x rt) = Node (fmap f lt)(f x)(fmap f rt)

data GTree a = GLeaf a | GNode [GTree a] deriving Show 

instance Functor GTree where
  fmap f (GLeaf a) = GLeaf (f a)


