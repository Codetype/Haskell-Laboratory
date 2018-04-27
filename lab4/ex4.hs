newtype MyInt = MkMyInt Int

instance Eq MyInt where
  (==) (MkMyInt i1)(MkMyInt i2) = i1 == i2 

instance Ord MyInt where
  (<=) (MkMyInt i1)(MkMyInt i2) = i1 <= i2

instance Num MyInt where
  (+) (MkMyInt i1)(MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1)(MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1)(MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i) = MkMyInt (negate i)
  abs (MkMyInt i) = MkMyInt (abs i)
  signum (MkMyInt i) = MkMyInt (signum i)
  fromInteger int = MkMyInt (fromIntegral int)

instance Show MyInt where
  show (MkMyInt i) = "MkMyInt " ++ show i

data BinTree a = MkBinTree a |
                 EmptyBT | 
                 NodeBT a (BinTree a) (BinTree a)

instance Eq a => Eq (BinTree a) where
  (==) (MkBinTree bt1)(MkBinTree bt2) = bt1 == bt2 

data MyType = C1 Int | C2 Double Bool

instance Eq MyType where
  (==) (C1 arg1)(C1 arg2) = arg1 == arg2 
  (==) (C2 d1 b1)(C2 d2 b2) = d1 == d2 && b1 == b2

data MyNewType = D1 (Bool, Int) | D2 Int | D3 Double

instance Eq MyNewType where
 (==) (D1 (nb1, ni1))(D1 (nb2, ni2)) = nb1 == nb2 && ni1 == ni2
 (==) (D2 i1)(D2 i2) = i1 == i2
 (==) (D3 d1)(D3 d2) = d1 == d2

data Fraction a = MkFraction {num::a, denom::a}

getNum (MkFraction n _) = n
getDenom (MkFraction _ dn) = dn 

instance Show (Fraction a) where
  show (MkFraction {num=n,denom=dn}) = "MkFraction: "  

newtype MyDouble = MkMyDouble Double
instance Eq MyDouble where
  (==) (MkMyDouble d1)(MkMyDouble d2) = d1 == d2 

instance Ord MyDouble where
  (<=) (MkMyDouble d1)(MkMyDouble d2) = d1 <= d2

instance Num MyDouble where
  (+) (MkMyDouble d1)(MkMyDouble d2) = MkMyDouble (d1 + d2)
  (-) (MkMyDouble d1)(MkMyDouble d2) = MkMyDouble (d1 - d2)
  (*) (MkMyDouble d1)(MkMyDouble d2) = MkMyDouble (d1 * d2)
  negate (MkMyDouble d) = MkMyDouble (negate d)
  abs (MkMyDouble d) = MkMyDouble (abs d)
  signum (MkMyDouble d) = MkMyDouble (signum d)
  fromInteger int = MkMyDouble (fromIntegral int)

instance Show MyDouble where
  show (MkMyDouble d) = "MkMyDouble " ++ show d


