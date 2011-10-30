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

colorFrom :: ColorTriple -> Color
colorFrom (r, g, b) = 
  rgb (round (r * 255)) (round (g * 255)) (round (b * 255))

combine :: ColorTriple -> ColorTriple -> ColorTriple 
combine (r1, g1, b1) (r2, g2, b2) = (r1 * r2, g1 * g2, b1 * b2)

scale :: ColorTriple -> Double -> ColorTriple
scale (r, g, b) s = (s * r, s * g, s * b)

weightedCombine :: Double -> ColorTriple -> ColorTriple -> ColorTriple
weightedCombine w (r1, g1, b1) (r2, g2, b2) =
  ((mul w r1 r2), (mul w g1 g2), (mul w b1 b2))
  where mul = \w a b -> w * a + (1 - w) * b

normalizeColor (r, g, b) = (n r, n g, n b) 
  where n = \x -> max 0 (min 1 x)

sumLight :: [ColorTriple] -> ColorTriple
sumLight colors = 
  normalizeColor $
    foldl1 (\(r1, g1, b1) (r2, g2, b2) -> (r1 + r2, g1 + g2, b1 + b2)) colors

colorFor :: Scene
            -> Primitive      -- | The shape to determine the color for
            -> Vec3           -- | The incident vector of the ray
            -> Vec3           -- | The location of intersection
            -> Int            -- | The stack depth
            -> ColorTriple    -- | The color at that point
colorFor scene shape direction location depth = 
  let mat = material shape
  in case mat of
    ColorMaterial _ -> basecolor mat

    ReflectiveMaterial baseColor reflectivity -> 
      weightedCombine reflectivity reflectColor baseColor
      where ray = Ray (direction `reflectAbout` (normal shape location)) location
            reflectColor = colorAtRay scene ray (depth + 1)
 
    PhongMaterial spec diff amb exp ->
      sumLight ((combine amb (global_ambient scene)) : 
            (phongLight scene mat ray norm `map` lights scene))
      where ray  = Ray direction location
            norm = normal shape location

tracei x = trace (show x) x

occluded :: Scene -> Vec3 -> Vec3 -> Bool
occluded scene x1 x2 =
  not . null . filter (\(s, loc, p) -> 0 < s && s < 1) $
              intersectWithScene scene (Ray (x2 &- x1) x1)

phongLight :: Scene -> Material -> Ray -> Vec3 -> Light -> ColorTriple
phongLight scene mat ray surfacenorm light =
  if occluded scene (position ray) (loclight light) then (0, 0, 0)
  else let kd           = diffuse mat `combine` difflight light
           ks           = specular mat `combine` speclight light
           alpha        = phongexp mat
           incident     = normalize $ direction ray
           lightdir     = normalize $ loclight light &- position ray
           lightreflect = lightdir `reflectAbout` surfacenorm
           sd           = max 0 (dotprod lightdir surfacenorm)
           ss           = (max 0 (dotprod lightreflect incident)) ^ alpha
       in sumLight [kd `scale` sd, ks `scale` ss]

colorAtRay :: Scene -> Ray -> Int -> ColorTriple
colorAtRay scene ray depth =
  let geom = geomAtRay scene ray
  in 
    if isNothing geom 
      then background scene 
      else (colorFor 
          scene (snd $ fromJust geom) (direction ray) (fst $ fromJust geom) depth)

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
