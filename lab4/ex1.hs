polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r,phi) = (r*cos phi, r*sin phi)

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r*cos phi, r*sin phi)

newtype CartesianCoord'' a = CartesianCoord'' (a,a) deriving (Show)
newtype PolarCoord'' a = PolarCoord'' (a,a) deriving (Show)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (PolarCoord'' (r,phi)) = (CartesianCoord''(r*cos phi, r*sin phi))

--spherical
sphericalToCartesian :: Floating a => (a,a,a) -> (a,a,a)
sphericalToCartesian (r, theta, phi) = (r * cos theta * cos phi, r * cos theta * sin phi, r * sin theta)

type CartesianCoord3 a = (a,a,a)
type SphericalCoord3 a = (a,a,a)

sphericalToCartesian' :: Floating a => SphericalCoord3 a -> CartesianCoord3 a
sphericalToCartesian' (r, theta, phi) = (r * cos theta * cos phi, r * cos theta * sin phi, r * sin theta)

newtype CartesianCoord4 a = CartesianCoord4 (a,a,a) deriving (Show)
newtype SphericalCoord4 a = SphericalCoord4 (a,a,a) deriving (Show)

sphericalToCartesian'' :: Floating a => SphericalCoord4 a -> CartesianCoord4 a
sphericalToCartesian'' (SphericalCoord4 (r,theta,phi)) = 
   (CartesianCoord4 (r * cos theta * cos phi, r * cos theta * sin phi, r * sin theta))

--cylindryczne
cylindricalToCartesian :: Floating a => (a,a,a) -> (a,a,a)
cylindricalToCartesian (r, phi, z) = (r * cos phi, r * sin phi, z)

type CylindricalCoord a = (a,a,a)

cylindricalToCartesian' :: Floating a => CylindricalCoord a -> CartesianCoord3 a
cylindricalToCartesian' (r, phi, z) = (r * cos phi, r * sin phi, z)

newtype CylindricalCoord' a = CylindricalCoord' (a,a,a) deriving (Show)

cylindricalToCartesian'' :: Floating a => CylindricalCoord' a -> CartesianCoord4 a
cylindricalToCartesian'' (CylindricalCoord' (r, phi, z)) = (CartesianCoord4  (r * cos phi, r * sin phi, z))

personInfoToString :: (String,String,String) -> String
personInfoToString (nm,snm,addr) = 
  "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String

personInfoToString' :: PersonInfoToStringType'
personInfoToString' (nm,snm,addr) = 
  "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

newtype Name'' = Name'' String deriving Show
newtype Surname'' = Surname'' String deriving Show
newtype Address'' = Address'' String deriving Show
newtype PersonInfo'' a = PersonInfo'' (Name'', Surname'', Address'' )

personInfoToString'' :: PersonInfo'' a -> String
personInfoToString'' (PersonInfo'' (nm,snm,addr)) = 
  "name: " ++ show (nm) ++ ", surname: " ++ show (snm) ++ ", addresss: " ++ show (addr) 
