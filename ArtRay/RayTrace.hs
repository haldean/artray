module ArtRay.RayTrace where

import Data.Maybe
import Data.List
import Data.Vect.Double
import Debug.Trace
import Graphics.GD
import ArtRay.Geometry
import ArtRay.Primitives

-- | Get the color for a pixel in the scene
pixelColor :: Size -> Scene -> Viewer -> Point2D -> ColorTriple
-- | Find the color at a pixel defined by the point
pixelColor size scene viewer (Point2D ix iy) =
  pixelColor size scene viewer (toRelPoint size (Point2D ix iy))

-- | Find the color at a pixel defined by a relative point (scalar multiples of
-- u and v).
pixelColor size scene viewer (RelPoint2D hu hv) =
  meanColor $ map (colorAtRay scene 0) rays
  where rays = pointToRay scene viewer (RelPoint2D hu hv)

-- | Finds the color at a point on a primitive, using the provided material.
-- Both the primitive and the material must be supplied, as some primitives may
-- have multiple materials that need to be calculated. The stack depth of ray
-- tracing is tracked but currently is unused and unlimited.
colorFor :: Scene             
            -- ^ The scene we're operating within
            -> Primitive      
            -- ^ The shape to determine the color for
            -> Material       
            -- ^ The material to examine (not necessarily the result of (material shape)
            -> Vec3           
            -- ^ The incident vector of the ray
            -> Vec3           
            -- ^ The location of intersection
            -> Int            
            -- ^ The stack depth
            -> ColorTriple    
            -- ^ The color at that point
colorFor scene shape mat direction location depth = 
  case mat of
    NullMaterial -> (0, 0, 0)

    ColorMaterial _ -> basecolor mat

    ReflectiveMaterial basemat reflectivity -> 
      weightedCombine reflectivity reflectColor baseColor
      where ray = Ray (direction `reflectAbout` normal shape location) location
            reflectColor = colorAtRay' scene (depth + 1) ray [shape]
            baseColor = colorFor scene shape basemat direction location depth 
 
    PhongMaterial spec diff amb exp ->
      sumLight ((combine amb (glambient scene)) : 
            (phongLight scene shape mat ray norm `map` lights scene))
      where ray  = Ray direction location
            norm = normal shape location

    TransparentMaterial base cmodel refindex ->
      com throughcolor basecolor
      where ray  = Ray (refractVector refindex (normal shape location) direction) location
            throughcolor = colorAtRay' scene (depth + 1) ray [shape]
            basecolor = colorFor scene shape base direction location depth
            com = case cmodel of
                    WeightSum opacity -> weightedCombine opacity
                    FlatSum -> sumColor
                    Multiply -> combine

-- | Returns how much occlusion occurs between two points x1 and x2. The shape
-- at x1 must be provided to ensure that it is excluded when calculating
-- intersections. The return value ranges from 0 to 1, where 0 is fully occluded
-- and 1 is not occluded at all. Intermediate values are possible because of
-- semi-transparent material that may be between x1 and x2.
occluded :: Scene -> Primitive -> Vec3 -> Vec3 -> Double
occluded scene shape x1 x2 =
  product
    . map (\(_, _, p) -> transmittance $ material p)
    . filter (\(s, loc, p) -> 0 < s && s < 1) $
      intersectWithScene scene (Ray (x2 &- x1) x1) [shape]

-- | Calculates the illumination of a point on a primitive with a Phong
-- material. 
phongLight :: Scene 
            -> Primitive
            -> Material
            -> Ray
            -- ^ The incoming ray, whose location is the point of intersection
            -> Vec3
            -- ^ The surface normal at the illumination point
            -> Light
            -> ColorTriple
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

-- | Find the color of the first object that intersects a ray
colorAtRay :: Scene -> Int -> Ray -> ColorTriple
colorAtRay scene depth ray = colorAtRay' scene depth ray []

-- | Find the color of the first object that intersects a ray, excluding the
-- provided primitives from the intersection.
colorAtRay' :: Scene -> Int -> Ray -> [Primitive] -> ColorTriple
colorAtRay' scene depth ray exclude =
  let geom = geomAtRay scene ray exclude
  in 
    if isNothing geom 
      then background scene 
      else let (loc, shape) = fromJust geom
           in colorFor scene shape (material shape) (direction ray) loc depth

-- | Order tuples representing intersections, from first to last intersection
orderTuples :: (Double, Vec3, Primitive) -> (Double, Vec3, Primitive) -> Ordering
orderTuples (s1, _, _) (s2, _, _)
  | s1 < s2 = LT
  | s1 == s2 = EQ
  | otherwise = GT

-- | Find the first primitive at a ray
geomAtRay :: Scene -> Ray -> [Primitive] -> Maybe (Vec3, Primitive)
geomAtRay scene ray exclude =
  let intersections = intersectWithScene scene ray exclude
  in  if null intersections then Nothing else
    let (_, point, shape) = minimumBy orderTuples intersections
    in Just (point, shape)

-- | Find all points of intersections of a ray with the scene
intersectWithScene :: Scene -> Ray -> [Primitive] -> [(Double, Vec3, Primitive)]
intersectWithScene scene ray exclude = 
  mapMaybe (firstIntersection ray) (filter (`notElem` exclude) (geom scene))

