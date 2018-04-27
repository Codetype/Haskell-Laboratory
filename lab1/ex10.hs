roots :: (Double, Double, Double) -> (Double, Double)
roots (a,b,c) = ((-b-d)/e, (-b+d)/e )
  where {d = sqrt (b*b - 4*a*c); e = 2*a }

roots2 :: (Double, Double, Double) -> (Double, Double)
roots2 (a,b,c) = 
  let {d = sqrt (b*b - 4*a*c); e = 2*a}
  in ((-b-d)/e, (-b+d)/e )

--tu trzeba było zrobić klamrowy where
{-
let in też 
ale kto by 
się tym przejmował
xD
-}
