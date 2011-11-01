module ArtRay.Geometry where

import Data.Vect.Double
import ArtRay.Primitives

quadSolve :: Double -> Double -> Double -> (Double, Double)
quadSolve a b c =
    ((-b + disc) / (2 * a), (-b - disc) / (2 * a))
      where disc = sqrt (b * b - 4 * a * c)

firstIntersection :: Ray -> Primitive -> Maybe (Double, Vec3, Primitive)
firstIntersection (Ray dir pos) prim@(Sphere center radius _) =
  let (s1, s2) = quadSolve 
                  (normsqr dir) 
                  (2 * (dotprod pos dir - dotprod center dir))
                  (normsqr pos + normsqr center - 
                   2 * dotprod pos center - (radius * radius))
  in if not (isNaN s1) && (s1 < s2) && (s1 > 0)
    then Just (s1, pos &+ scalarMul s1 dir, prim)
    else if not (isNaN s2) && (s2 > 0) then Just (s2, pos &+ scalarMul s2 dir, prim)
    else Nothing

firstIntersection (Ray dir pos) p@(Plane n x _) =
  let s = (x `dotprod` n - pos `dotprod` n) / (dir `dotprod` n)
  in if s > 0 then Just (s, pos &+ scalarMul s dir, p) else Nothing

pointToRay :: Viewer -> Point2D -> Ray
pointToRay (Viewer location u v f) (RelPoint2D hu hv) =
  Ray (normalize (p &- location)) location
  where p = location &+ f &+ scalarMul hu u &+ scalarMul hv v

reflectAbout :: Vec3 -> Vec3 -> Vec3
reflectAbout vec norm = vec &- scalarMul (2 * dotprod norm vec) norm

refractVector :: Double -> Vec3 -> Vec3 -> Vec3
refractVector refind norm incident =
  refract refind (mkNormal norm) (neg incident)

normal :: Primitive -> Vec3 -> Vec3
normal (Sphere center _ _) vec = normalize (vec &- center)
normal (Plane n _ _) vec = normalize n
