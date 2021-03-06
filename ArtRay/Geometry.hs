module ArtRay.Geometry where

import Data.Vect.Double
import ArtRay.Primitives

-- | Solve a quadratic. Returns NaN for non-real roots, and two equal values for
-- double roots.
quadSolve :: Double -> Double -> Double -> (Double, Double)
quadSolve a b c =
    ((-b + disc) / (2 * a), (-b - disc) / (2 * a))
      where disc = sqrt (b * b - 4 * a * c)

-- | Find the first intersection between a primitive and a ray. Returns Nothing
-- if the ray never intersects the primitive.
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

-- | Find the ray for a point on the image plane
pointToRay' :: Viewer -> Point2D -> Ray
pointToRay' (Viewer location u v f) (RelPoint2D hu hv) =
  Ray (normalize (p &- location)) location
  where p = location &+ f &+ scalarMul hu u &+ scalarMul hv v

-- | Finds the point on a ray whose projection on a vector has a specific length.
farpoint :: Ray -> Vec3 -> Double -> Vec3
farpoint (Ray dir pos) onto dist = pos &+ scalarMul s dir
  where f_hat = normalize onto
        s = (dist - dotprod pos f_hat) / dotprod dir f_hat

-- | Deform a ray by (i, j) in the image plane, maintaining the focal point of
-- the ray.
deformRay :: Scene -> Ray -> (Double, Double) -> Ray
deformRay scene ray@(Ray dir pos) (i, j) =
  let farpt  = farpoint ray (f $ viewer scene) (dofdepth scene)
      newpos = pos &+ (scalarMul i (u $ viewer scene))
                &+ (scalarMul j (v $ viewer scene))
      newdir = normalize (farpt &- newpos)
  in Ray newdir newpos
  
-- | Find a family of rays that converge at the focal distance for a point in
-- the image plane.
pointToRay :: Scene -> Viewer -> Point2D -> [Ray]
pointToRay scene viewer point =
  if not $ dofenabled scene then [pointToRay' viewer point]
  else let ray = pointToRay' viewer point
  in map (deformRay scene ray) [(i,j) | i <- range, j <- range] 
      where ap = 0.5 * (dofaperture scene)
            samplesize = ap / fromIntegral (dofsamples scene)
            range = [-ap, (-ap) + samplesize .. ap]

-- | Reflect the first vector about the second
reflectAbout :: Vec3 -> Vec3 -> Vec3
reflectAbout vec norm = vec &- scalarMul (2 * dotprod norm vec) norm

-- | Refract the second vector around the first with the provided refraction
-- index.
refractVector :: Double -> Vec3 -> Vec3 -> Vec3
refractVector refind norm incident =
  refract refind (mkNormal norm) (neg incident)

-- | Find the normal vector at a point on a primitive. Note that this function
-- does not require that the point be actually incident to the primitive.
normal :: Primitive -> Vec3 -> Vec3
normal (Sphere center _ _) vec = normalize (vec &- center)
normal (Plane n _ _) vec = normalize n
