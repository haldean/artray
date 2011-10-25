import Data.Vect.Double
import TrayRacer.Primitives

quadSolve :: Double -> Double -> Double -> (Double, Double)
quadSolve a b c =
    ((-b + disc) / (2 * a), (-b - disc) / (2 * a))
      where disc = sqrt (b * b - 4 * a * c)

intersectAlongRay :: Ray -> Primitive -> (Double, Double)
intersectAlongRay (Ray dir pos) (Sphere center radius _) =
  quadSolve 
    (normsqr dir) 
    (2 * (dotprod pos dir - dotprod center dir))
    (normsqr pos + normsqr center - 2 * dotprod pos center - (radius * radius))

firstIntersection :: Ray -> Primitive -> Maybe (Double, Vec3, Primitive)
firstIntersection (Ray dir pos) prim =
  let (s1, s2) = intersectAlongRay (Ray dir pos) prim 
  in if not (isNaN s1) && (s1 < s2) 
    then Just (s1, (pos &+ scalarMul s1 dir), prim)
    else if not (isNaN s2) then Just (s2, (pos &+ scalarMul s2 dir), prim)
    else Nothing

pointToRay :: Viewer -> Point2D -> Ray
pointToRay (Viewer location u v f) (RelPoint2D hu hv) =
  Ray (p &- location) location
  where p = location &+ f &+ scalarMul hu u &+ scalarMul hv v

reflectAbout :: Vec3 -> Vec3 -> Vec3
reflectAbout vec norm = vec &- scalarMul (2 * dotprod norm vec) norm

normal :: Primitive -> Vec3 -> Vec3
normal (Sphere center _ _) vec = normalize (vec &- center)
