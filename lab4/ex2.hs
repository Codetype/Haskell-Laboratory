data CartInt2DVec = MkCartInt2DVec Int Int

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data CartInt2DVec' a = MkCartInt2DVec' a a

xCoord' :: CartInt2DVec' a -> a
xCoord' (MkCartInt2DVec' x _) = x

yCoord' :: CartInt2DVec' a -> a
yCoord' (MkCartInt2DVec' _ y) = y

data CartInt2DVec'' a = MkCartInt2DVec'' {x::a, y::a} deriving Show

data PolarFloat2DVec a = MkPolarFloat2DVec a a

xPolCoord :: PolarFloat2DVec a -> a
xPolCoord (MkPolarFloat2DVec r _) = r
    
yPolCoord :: PolarFloat2DVec a -> a
yPolCoord (MkPolarFloat2DVec _ phi) = phi

polarToCartesian :: Floating a => PolarFloat2DVec a -> CartInt2DVec' a
polarToCartesian (MkPolarFloat2DVec r phi) = (MkCartInt2DVec' (r*cos phi) (r*sin phi))

data PolarFloat2DVec' a = MkPolarFloat2DVec' {r::a, phi::a} deriving Show

polarToCartesian'' :: Floating a => PolarFloat2DVec' a -> CartInt2DVec'' a
polarToCartesian'' (MkPolarFloat2DVec' {r = r1, phi = phi1}) = (MkCartInt2DVec'' {x = (r1*cos phi1), y = (r1*sin phi1)})

--yCoord'' :: CartInt2DVec'' a -> a
--yCoord'' (MkCartInt2DVec'' {x = _, y = yVal}) = yVal

data List a = EmptyL | Cons a (List a) deriving Show

head' EmptyL      = error "head' : the empty list has no head!"
head' (Cons x xs) = x

data ThreeColors = Blue | White | Redd
type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Redd = "Irene Jacob"

--3D Vectors
data Cart3DVec a = MkCart3DVec a a a deriving Show

xCoord3D :: Cart3DVec a -> a
xCoord3D (MkCart3DVec x _ _) = x

yCoord3D :: Cart3DVec a -> a
yCoord3D (MkCart3DVec _ y _) = y

zCoord3D :: Cart3DVec a -> a
zCoord3D (MkCart3DVec _ _ z) = z

data Cart3DVec' = Cart3DVec'
                  { xCoord3D' :: Int
                  , yCoord3D' :: Int 
                  , zCoord3D' :: Int
                  } deriving Show



data Shape = Circle Float | Rectangle Float Float
area :: Shape -> Float
area (Circle x) = pi*x^2
area (Rectangle a b) = a*b

data Tree a = EmptyT | 
              Node a (Tree a)(Tree a)
              deriving Show

rootValue :: Tree a -> a
rootValue EmptyT      = error "rootValue Tree is empty!"
rootValue (Node a lt rt) = a

data TrafficLights = Green | Yellow | Red
 
actionFor :: TrafficLights -> String
actionFor Green = "Go!"
actionFor Yellow = "Beware/Prepare"
actionFor Red = "Stop"
