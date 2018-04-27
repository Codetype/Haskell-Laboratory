class Mappable t where 
   fMap :: (a->b) -> t a -> t b

data Vec2D a = Vec2D {m::a,n::a} deriving Show
data Vec3D a = Vec3D {x::a,y::a, z::a} deriving Show

instance Mappable Vec3D where 
   fMap f (Vec3D x y z) = Vec3D (f x)(f y)(f z) 

newtype Pair a = Pair (a,a) deriving Show

instance Mappable Pair where
  fMap f (Pair (x,y)) = Pair (f x, f y) 

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a)(BinTree a)
                 deriving Show

instance Mappable BinTree where
  fMap f EmptyBT = EmptyBT
  fMap f (NodeBT v l r) = NodeBT (f v)(fMap f l)(fMap f r)

instance Mappable Maybe where
  fMap f Nothing = Nothing
  fMap f (Just a) = Just (f a)

instance Mappable (Either a) where
  fMap f (Right r) = Right (f r)
  fMap f (Left l) = Left l

instance Mappable ((->) a) where
  fMap f g = f . g

class VectorLike t where
  (|==|) :: Eq a => t a -> t a -> Bool
  (|+|),(|-|) :: Num a => t a -> t a -> t a
  (|*|) :: Num a => t a -> t a -> a
  (||?), (|-?) :: (Num a, Eq a) => t a -> t a -> Bool
  vectLength :: Floating a => t a -> a
  unitVectOf :: Floating a => t a -> t a

instance VectorLike Vec2D where 
  (|==|) (Vec2D a1 b1)(Vec2D a2 b2) = (a1 == a2) && (b1 == b2)
  (|+|) (Vec2D a1 b1)(Vec2D a2 b2) = (Vec2D (a1+a2) (b1+b2))
  (|-|) (Vec2D a1 b1)(Vec2D a2 b2) = (Vec2D (a1-a2) (b1-b2))
  (|*|) (Vec2D a1 b1)(Vec2D a2 b2) = (a1*a2+b1*b2)
  (||?) (Vec2D a1 b1)(Vec2D a2 b2) = (a1*b2) == (b1*a2)
  (|-?) (Vec2D a1 b1)(Vec2D a2 b2) = (a1*a2)+(b1*b2) == 0
  vectLength (Vec2D a b) = sqrt(a^2+b^2) 
  unitVectOf (Vec2D a b) = (Vec2D 1 1)

instance VectorLike Vec3D where 
  (|==|) (Vec3D a1 b1 c1)(Vec3D a2 b2 c2) = (a1 == a2) && (b1 == b2) && (c1 == c2)
  (|+|) (Vec3D a1 b1 c1)(Vec3D a2 b2 c2) = (Vec3D (a1+a2) (b1+b2) (c1+c2))
  (|-|) (Vec3D a1 b1 c1)(Vec3D a2 b2 c2) = (Vec3D (a1-a2) (b1-b2) (c1-c2))
  (|*|) (Vec3D a1 b1 c1)(Vec3D a2 b2 c2) = (a1*a2+b1*b2+c1*c2)
  (||?) (Vec3D a1 b1 c1)(Vec3D a2 b2 c2) = (a1*b2) == (b1*a2) && (b1*c2) == (c1*b2)
  (|-?) (Vec3D a1 b1 c1)(Vec3D a2 b2 c2) = (a1*a2)+(b1*b2)+(c1*c2) == 0
  vectLength (Vec3D a b c) = sqrt(a^2+b^2+c^2) 
  unitVectOf (Vec3D a b c) = (Vec3D 1 1 1)
