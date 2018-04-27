roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = 
    let d = sqrt (b*b - 4*a*c)
        e = 2*a
    in  ( (-b-d)/e, (-b+d)/e )
    
unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (x, y) = 
    let vLen = sqrt(x^2 + y^2)
    in (x/vLen, y/vLen)
    
unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (x, y, z) = 
    let vLen = sqrt(x^2 + y^2 + z^2)
    in (x/vLen, y/vLen, z/vLen)

heron :: (Double, Double, Double) -> Double
heron (x,y,z) = 
    let p = (x+y+z)/2
    in sqrt(p*(p-x)*(p-y)*(p-z))
