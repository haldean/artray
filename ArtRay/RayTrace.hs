module ArtRay.RayTrace where

import Data.Maybe
import Data.List
import Data.Vect.Double
import Debug.Trace
import Graphics.GD
import ArtRay.Geometry
import ArtRay.Primitives

pixelColor :: Size -> Scene -> Viewer -> Point2D -> Color
pixelColor size scene viewer (Point2D ix iy) =
  pixelColor size scene viewer (toRelPoint size (Point2D ix iy))
pixelColor size scene viewer (RelPoint2D hu hv) =
  colorFrom $ 
    colorAtRay scene ray 0 where ray = pointToRay viewer (RelPoint2D hu hv)

colorFor :: Scene
            -> Primitive      -- | The shape to determine the color for
            -> Material
            -> Vec3           -- | The incident vector of the ray
            -> Vec3           -- | The location of intersection
            -> Int            -- | The stack depth
            -> ColorTriple    -- | The color at that point
colorFor scene shape mat direction location depth = 
  case mat of
    NullMaterial -> (0, 0, 0)

    ColorMaterial _ -> basecolor mat

    ReflectiveMaterial basemat reflectivity -> 
      weightedCombine reflectivity reflectColor baseColor
      where ray = Ray (direction `reflectAbout` normal shape location) location
            reflectColor = colorAtRay' scene ray [shape] (depth + 1)
            baseColor = colorFor scene shape basemat direction location depth 
 
    PhongMaterial spec diff amb exp ->
      sumLight (combine amb (global_ambient scene)) : 
            (phongLight scene shape mat ray norm `map` lights scene)
      where ray  = Ray direction location
            norm = normal shape location

    TransparentMaterial base cmodel refindex ->
      com throughcolor basecolor
      where ray  = Ray (refractVector refindex (normal shape location) direction) location
            throughcolor = colorAtRay' scene ray [shape] (depth + 1)
            basecolor = colorFor scene shape base direction location depth
            com = case cmodel of
                    WeightSum opacity -> weightedCombine opacity
                    FlatSum -> sumColor
                    Multiply -> combine

tracei x = trace (show x) x

occluded :: Scene -> Primitive -> Vec3 -> Vec3 -> Double
occluded scene shape x1 x2 =
  product
    . map (\(_, _, p) -> transmittance $ material p)
    . filter (\(s, loc, p) -> 0 < s && s < 1) $
      intersectWithScene scene (Ray (x2 &- x1) x1) [shape]

phongLight :: Scene -> Primitive -> Material -> Ray -> Vec3 -> Light -> ColorTriple
phongLight scene shape mat ray surfacenorm light =
  let kd           = diffuse mat `combine` difflight light
      ks           = specular mat `combine` speclight light
      alpha        = phongexp mat
      incident     = normalize $ direction ray
      lightdir     = normalize $ loclight light &- position ray
      lightreflect = lightdir `reflectAbout` surfacenorm
      sd           = max 0 (dotprod lightdir surfacenorm)
      ss           = max 0 (dotprod lightreflect incident) ^ alpha
      occlusion    = occluded scene shape (position ray) (loclight light)
      in weightedCombine occlusion (sumLight [kd `scale` sd, ks `scale` ss]) (0, 0, 0)

colorAtRay :: Scene -> Ray -> Int -> ColorTriple
colorAtRay scene ray = colorAtRay' scene ray []

colorAtRay' :: Scene -> Ray -> [Primitive] -> Int -> ColorTriple
colorAtRay' scene ray exclude depth =
  let geom = geomAtRay scene ray exclude
  in 
    if isNothing geom 
      then background scene 
      else let (loc, shape) = fromJust geom
           in colorFor scene shape (material shape) (direction ray) loc depth

sortTuples :: (Double, Vec3, Primitive) -> (Double, Vec3, Primitive) -> Ordering
sortTuples (s1, _, _) (s2, _, _)
  | s1 < s2 = LT
  | s1 == s2 = EQ
  | otherwise = GT

geomAtRay :: Scene -> Ray -> [Primitive] -> Maybe (Vec3, Primitive)
geomAtRay scene ray exclude =
  let intersections = intersectWithScene scene ray exclude
  in  if null intersections then Nothing else
    let (_, point, shape) = minimumBy sortTuples intersections
    in Just (point, shape)

intersectWithScene :: Scene -> Ray -> [Primitive] -> [(Double, Vec3, Primitive)]
intersectWithScene scene ray exclude = 
  mapMaybe (firstIntersection ray) (filter (`notElem` exclude) (geom scene))

rayTraceImage :: Scene -> Size -> Point -> Color
rayTraceImage scene size (x, y) =
  pixelColor size scene (viewer scene) (Point2D x y)
