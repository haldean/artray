module TrayRacer.RayTrace where

import Data.Maybe
import Data.List
import Data.Vect.Double
import Debug.Trace
import Graphics.GD
import TrayRacer.Geometry
import TrayRacer.Primitives

pixelColor :: Size -> Scene -> Viewer -> Point2D -> Color
pixelColor size scene viewer (Point2D ix iy) =
  pixelColor size scene viewer (toRelPoint size (Point2D ix iy))
pixelColor size scene viewer (RelPoint2D hu hv) =
  colorFrom $ 
    colorAtRay scene ray 0 where ray = pointToRay viewer (RelPoint2D hu hv)

weightColors :: Double -> Material -> Material -> Material
weightColors w (ColorMaterial r1 g1 b1) (ColorMaterial r2 g2 b2) =
  (ColorMaterial (mul w r1 r2) (mul w g1 g2) (mul w b1 b2))
  where mul = \w a b -> round ((w * fromIntegral a) + ((1 - w) * fromIntegral b))

colorFor :: Scene
            -> Primitive      -- | The shape to determine the color for
            -> Vec3           -- | The incident vector of the ray
            -> Vec3           -- | The location of intersection
            -> Int            -- | The stack depth
            -> Material       -- | The color at that point
colorFor scene shape direction location depth = 
  let mat = material shape
  in case mat of
    ColorMaterial _ _ _ -> mat
    ReflectiveMaterial baseColor reflectivity -> 
      weightColors reflectivity reflectColor baseColor
      where ray = Ray (direction `reflectAbout` (normal shape location)) location
            reflectColor = colorAtRay scene ray (depth + 1)

colorAtRay :: Scene -> Ray -> Int -> Material
colorAtRay scene ray depth =
  let geom = geomAtRay scene ray
  in 
    if isNothing geom 
      then background scene 
      else (colorFor 
          scene (snd $ fromJust geom) (direction ray) (fst $ fromJust geom)
          depth)

sortTuples :: (Double, Vec3, Primitive) -> (Double, Vec3, Primitive) -> Ordering
sortTuples (s1, _, _) (s2, _, _) =
  if s1 < s2 then LT
  else if s1 == s2 then EQ
  else GT

geomAtRay :: Scene -> Ray -> Maybe (Vec3, Primitive)
geomAtRay scene ray =
  let intersections = intersectWithScene scene ray
  in  if null intersections then Nothing else
    let (_, point, shape) = minimumBy sortTuples intersections
    in Just (point, shape)

intersectWithScene :: Scene -> Ray -> [(Double, Vec3, Primitive)]
intersectWithScene scene ray = 
  catMaybes $ map (firstIntersection ray) (geom scene)

rayTraceImage :: Scene -> Viewer -> (Size -> Point -> Color)
rayTraceImage scene viewer =
  \size (x, y) -> pixelColor size scene viewer (Point2D x y)
