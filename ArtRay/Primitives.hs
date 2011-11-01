module ArtRay.Primitives where

import Graphics.GD
import Data.Vect.Double
import Debug.Trace

data Scene = 
  Scene { 
    geom :: [Primitive], 
    background :: ColorTriple,
    globalAmbient :: ColorTriple,
    lights :: [Light],
    viewer :: Viewer
  } deriving (Show, Read)

data Primitive 
  = Sphere { 
    center :: Vec3, radius :: Double, material :: Material }
  | Plane  { 
    pnorm :: Vec3, point :: Vec3, material :: Material }
  | RectPrism {
    corner :: Vec3, x :: Vec3, y :: Vec3, z :: Vec3, material :: Material }
  deriving (Show, Read, Eq)

data Ray = Ray { direction :: Vec3, position :: Vec3 } deriving (Show)

type ColorTriple = (Double, Double, Double)

data Light
  = PhongLight {
    speclight :: ColorTriple,
    difflight :: ColorTriple,
    loclight  :: Vec3
  } deriving (Show, Read)

data CombinationModel
  = WeightSum { weight::Double }
  | FlatSum
  | Multiply
  deriving (Show, Eq, Read)

data Material
  = ColorMaterial {
    basecolor :: ColorTriple
  }

  | ReflectiveMaterial {
    base :: Material,
    reflectivity :: Double
  }

  | PhongMaterial {
    specular :: ColorTriple,
    diffuse :: ColorTriple,
    ambient :: ColorTriple,
    phongexp :: Int
  }

  | TransparentMaterial {
    base :: Material,
    cmodel :: CombinationModel,
    refindex :: Double
  }

  | NullMaterial
  deriving (Show, Read, Eq)

transmittance :: Material -> Double
transmittance (TransparentMaterial _ (WeightSum op) _) = op
transmittance _ = 0

data Viewer = Viewer {
    -- | The location of the viewer
    location :: Vec3,
    -- | A vector pointing horizontally along the image plane
    u :: Vec3,
    -- | A vector pointing vertically along the image plane
    v :: Vec3,
    -- | A vector pointing from the viewer to the center of the image plane
    f :: Vec3
  } deriving (Show, Read)

data Point2D = 
  -- | Describes a point in the image using pixel coordinates
  Point2D Int Int
  -- | Describes a point in the image using offsets from the center, where each
  --   field goes from zero to one.
  | RelPoint2D Double Double
  deriving (Show)

color :: Double -> Double -> Double -> ColorTriple
color r g b = (r, g, b)

colorm :: Double -> Double -> Double -> Material
colorm r g b = ColorMaterial (r, g, b)

toRelPoint :: Size -> Point2D -> Point2D
toRelPoint size (Point2D ix iy) = 
  RelPoint2D ((x - xc) / xc) ((y - yc) / yc)
  where xc = fromIntegral (fst size) / 2
        yc = fromIntegral (snd size) / 2
        x  = fromIntegral ix
        y  = fromIntegral iy

-- |Creates a viewer
view :: Vec3        -- ^ The location of the viewer
     -> Double      -- ^ The field of view of the viewer, in radians
     -> Vec3        -- ^ The vector from viewer to center of image plane
     -> Vec3        -- ^ Unit vector pointing vertically along the image plane
     -> Viewer
view loc fov f v =
  Viewer loc (scalarMul scale (crossprod v f)) (scalarMul scale v) f
  where scale = norm f * tan (fov / 2)

instance Eq Vec3 where
  (Vec3 x1 y1 z1) == (Vec3 x2 y2 z2) = (x1 == x2) && (y1 == y2) && (z1 == z2)

colorFrom :: ColorTriple -> Color
colorFrom (r, g, b) = 
  rgb (round (r * 255)) (round (g * 255)) (round (b * 255))

combine :: ColorTriple -> ColorTriple -> ColorTriple 
combine (r1, g1, b1) (r2, g2, b2) = (r1 * r2, g1 * g2, b1 * b2)

scale :: ColorTriple -> Double -> ColorTriple
scale (r, g, b) s = (s * r, s * g, s * b)

weightedCombine :: Double -> ColorTriple -> ColorTriple -> ColorTriple
weightedCombine w (r1, g1, b1) (r2, g2, b2) =
  (mul w r1 r2, mul w g1 g2, mul w b1 b2)
  where mul w a b = w * a + (1 - w) * b

normalizeColor (r, g, b) = (n r, n g, n b) 
  where n x = max 0 (min 1 x)

sumLight :: [ColorTriple] -> ColorTriple
sumLight = foldl1 sumColor

sumColor :: ColorTriple -> ColorTriple -> ColorTriple
sumColor (r1, g1, b1) (r2, g2, b2) = normalizeColor (r1 + r2, g1 + g2, b1 + b2)

