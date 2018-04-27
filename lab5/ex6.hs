newtype Box a = MkBox a deriving Show

instance Applicative Box where
  pure = MkBox
  (MkBox f) <*> w = fmap f w

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)

newtype MyTriple a = MyTriple (a,a,a) deriving Show

instance Functor MyTriple where
  fmap f (MyTriple (x1,x2,x3)) = MyTriple (f x1, f x2, f x3)
 
--instance Applicative MyTriple where
  -- pure a = MyTriple (a,a,a) 
  -- (MyTriple (a1, a2, a3)) <*> w = MyTriple (a1 <*> w)(a2 <*> w)(a3 <*> w)

data Tree2 a = EmptyT2 | Leaf a | Node (Tree2 a)(Tree2 a) deriving Show 

instance Functor Tree2 where
  fmap _ EmptyT2 = EmptyT2
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node lt rt) = Node (fmap f lt)(fmap f rt)

instance Applicative Tree2 where
  pure = Leaf
  EmptyT2 <*> _ = EmptyT2
  (Leaf f)     <*> w = fmap f w  
  tf           <*> Leaf x = fmap ($ x) tf
  Node lt rt <*> w = Node (lt <*> w)(rt <*> w)

