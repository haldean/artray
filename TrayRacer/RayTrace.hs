module TrayRacer.RayTrace where

import Data.Maybe
import Data.List
import Data.Vect.Double
import Graphics.GD
import TrayRacer.Geometry
import TrayRacer.Primitives

pixelColor :: Size -> Scene -> Viewer -> Point2D -> Color
pixelColor size (Scene geom) viewer (Point2D ix iy) =
  pixelColor size (Scene geom) viewer (toRelPoint size (Point2D ix iy))

pixelColor size scene viewer (RelPoint2D hu hv) =
  colorAtRay scene ray where ray = pointToRay viewer (RelPoint2D hu hv)

colorFor :: Primitive -- | The shape to determine the color for
            -> Vec3   -- | The incident vector of the ray
            -> Vec3   -- | The location of intersection
            -> Color  -- | The color at that point
colorFor shape direction location = materialColor $ material shape

colorAtRay :: Scene -> Ray -> Color
colorAtRay scene ray =
  colorFor shape (direction ray) ip
  where (ip, shape) = geomAtRay scene ray

sortTuples :: (Double, Vec3, Primitive) -> (Double, Vec3, Primitive) -> Ordering
sortTuples (s1, _, _) (s2, _, _) =
  if s1 < s2 then LT
  else if s1 == s2 then EQ
  else GT

geomAtRay :: Scene -> Ray -> (Vec3, Primitive)
geomAtRay scene ray = (point, shape)
  where (_, point, shape) = minimumBy sortTuples $ intersectWithScene scene ray

intersectWithScene :: Scene -> Ray -> [(Double, Vec3, Primitive)]
intersectWithScene (Scene geom) ray = 
  catMaybes $ map (firstIntersection ray) geom

rayTraceImage :: Scene -> Viewer -> (Size -> Point -> Color)
rayTraceImage scene viewer =
  \size (x, y) -> pixelColor size scene viewer (Point2D x y)
  
test =
  let x = (Scene [(Sphere (Vec3 8 2 2) 3 (ColorMaterial 0 255 0)),
                  (Sphere (Vec3 4 0 0) 1 (ColorMaterial 255 0 0))])
  in colorAtRay x (Ray (Vec3 1 0 0) (Vec3 0 0 0))
