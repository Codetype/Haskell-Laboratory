data BinIntTree = EmptyIntBT | 
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree(IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT | 
                  NodeBT a (BinTree a) (BinTree a) deriving Show

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

--Bin tree functions
depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = 1 + (depthOfBT lt) + (depthOfBT rt) 

--preorder
flattenBT :: BinTree a -> [a]
flattenBT EmptyBT = []
flattenBT (NodeBT n lt rt) = [n] ++ (flattenBT lt)  ++ (flattenBT rt)

--inorder
flattenBT' :: BinTree a -> [a]
flattenBT' EmptyBT = []
flattenBT' (NodeBT n lt rt) = (flattenBT' lt) ++ [n] ++ (flattenBT' rt)

--postorder
flattenBT'' :: BinTree a -> [a]
flattenBT'' EmptyBT = []
flattenBT'' (NodeBT n lt rt) = (flattenBT'' lt) ++ (flattenBT'' rt) ++ [n]

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = NodeBT (f n) (mapBT f lt) (mapBT f rt)

insert :: Ord a => a -> BinTree a -> BinTree a
insert x EmptyBT = (NodeBT x EmptyBT EmptyBT)
insert x (NodeBT n lt rt) 
       | x == n = NodeBT n lt rt
       | x < n = NodeBT n (insert x lt) rt
       | otherwise = NodeBT n lt (insert x rt)

list2BST :: Ord a => [a] -> BinTree a
list2BST [] = EmptyBT
list2BST (x:xs) = accTree (NodeBT x EmptyBT EmptyBT) xs
         where accTree tree [] = tree
               accTree tree (x:xs) = accTree (insert x tree) xs

occurs :: Eq a => a -> BinTree a -> Int
occurs x EmptyBT = 0
occurs x (NodeBT n lt rt)
       | x == n = 1 + (occurs x lt) + (occurs x rt)
       | otherwise = (occurs x lt) + (occurs x rt)

elemOf :: Eq a => a -> BinTree a -> Bool
elemOf x EmptyBT = False
elemOf x (NodeBT n lt rt)
       | x == n = True
       | otherwise = (elemOf x lt) || (elemOf x rt)

reflect :: BinTree a -> BinTree a
reflect EmptyBT = EmptyBT
reflect (NodeBT n lt rt) = NodeBT n (reflect rt) (reflect lt)

minElemOf :: Ord a => BinTree a -> a
minElemOf EmptyBT = error "Empty tree - lack of minimal element!"
minElemOf tree = case tree of
          NodeBT val EmptyBT _ -> val
          NodeBT _ lt _ -> minElemOf(lt) 

maxElemOf :: Ord a => BinTree a -> a
maxElemOf EmptyBT = error "Empty tree - lack of maximal element!"
maxElemOf tree = case tree of
          NodeBT val EmptyBT _ -> val
          NodeBT _ _ rt -> maxElemOf(rt) 

foldBinTree :: (a->b->b->b) -> b -> BinTree a -> b
foldBinTree _ s EmptyBT = s
foldBinTree f s (NodeBT n lt rt) = f n (foldBinTree f s lt) (foldBinTree f s rt)

data GTree a = GNode (GTree a) (GTree a) | Leaf a deriving Show

sumGTree :: Num a => GTree a -> a
sumGTree (Leaf a) = a
sumGTree (GNode lt rt) = sumGTree lt + sumGTree rt 

elemOfGTree :: Eq a => a -> GTree a -> Bool
elemOfGTree x (Leaf a) = a == x 
elemOfGTree x (GNode lt rt) = (elemOfGTree x lt) || (elemOfGTree x rt)

depthOfGTree :: GTree a -> Int
depthOfGTree (Leaf a) = 0
depthOfGTree (GNode lt rt) = 1 + depthOfGTree lt + depthOfGTree rt
 
mapGTree :: (a->b) -> GTree a -> GTree b
mapGTree f (Leaf a) = Leaf (f a)
mapGTree f (GNode lt rt) = GNode (mapGTree f lt) (mapGTree f rt)

flattenGTree :: GTree a -> [a]
flattenGTree (Leaf a) = [a]
flattenGTree (GNode lt rt) = (flattenGTree lt) ++ (flattenGTree rt)

countGTreeLeaves :: GTree a -> Int
countGTreeLeaves (Leaf a) = 1
countGTreeLeaves (GNode lt rt) = (countGTreeLeaves lt) + (countGTreeLeaves rt)

{-
data Expr a = Lit a | 
              Add (Expr a)(Expr a) |
              Dif (Expr a)(Expr a) |
              Mul (Expr a)(Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Dif e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Dif e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
show' (Mul e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"

data Expr' a = Lit' a |
               Expr' a :+: Expr' a |
               Expr' a :-: Expr' a |
               Expr' a :*: Expr' a

eval'' :: Num a => Expr' a -> a
eval'' (Lit' n) = n
eval'' (e1 :+: e2) = eval'' e1 + eval'' e2
eval'' (e1 :-: e2) = eval'' e1 - eval'' e2
eval'' (e1 :*: e2) = eval'' e1 * eval'' e2

show'' :: Show a => Expr' a -> String
show'' (Lit' n) = show n
show'' (e1 :+: e2) =  "(" ++ show'' e1 ++ "+" ++ show'' e2 ++ ")"
show'' (e1 :-: e2) =  "(" ++ show'' e1 ++ "-" ++ show'' e2 ++ ")"
show'' (e1 :*: e2) =  "(" ++ show'' e1 ++ "*" ++ show'' e2 ++ ")" -}

data Expr a = 
              Lit a |
              Op Ops (Expr a)(Expr a) |
              If (BExpr a)(Expr a)(Expr a)


data Ops = Add | Sub | Mul

data BExpr a = BoolLit Bool |
             And (BExpr a)(BExpr a) |
             Or (BExpr a)(BExpr a) |
             Not (BExpr a) |
             Equal (Expr a)(Expr a) |
             Greater (Expr a)(Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Op Add e1 e2) = eval e1 + eval e2
eval (Op Sub e1 e2) = eval e1 - eval e2
eval (Op Mul e1 e2) = eval e1 * eval e2

bEval :: BExpr a -> Bool
bEval (BoolLit b) = b 
bEval (And b1 b2) = bEval b1 && bEval b2
bEval (Or b1 b2) = bEval b1 || bEval b2
bEval (Not b) = not (bEval b) 

