module ArtRay.Primitives where

import Data.Maybe
import Data.Vect.Double
import Debug.Trace
import Graphics.GD

-- | The root object of any rendered scene in ArtRay
data Scene = 
  Scene { 
    background :: ColorTriple,
    -- ^ The background color of the scene -- displayed whenever a ray doesn't
    -- intersect any geometry.
    options :: [Option],
    -- ^ A list of rendering options 
    geom :: [Primitive], 
    -- ^ The geometry of the scene
    lights :: [Light],
    -- ^ The lights in the scene
    viewer :: Viewer
    -- ^ Camera information
  } deriving (Show, Read)

-- | Rendering-related options for scenes
data Option
  -- | The global ambient light. If provided, this will illuminate all
  -- primitives with nonzero ambient material response at all points.
  = GlobalAmbient ColorTriple

  -- | Antialiasing options. When the Antialiased option is provided with
  -- msaaSamples > 1, ArtRay will use MSAA subpixel sampling to antialias the
  -- image. Note that multiplying msaaSamples by n increases runtime by O(n^2).
  | Antialiased {
    msaaSamples :: Double
  }

  -- | Depth of field options. If this option is not provided, the camera has
  -- infinite depth of field. When this option is provided, the camera simulates
  -- a real camera with focal length and aperture. Aperture is a number, usually
  -- on the order of 1, which controls how tight the depth of field is -- higher
  -- aperture values lead to narrower areas in focus. The focal length option
  -- determines how far away from the camera's location is in focus. The samples
  -- parameter is the square root of the number of rays shot out to determine
  -- the color at each pixel. For simple scenes, this can be as low as 2. The
  -- run time scales as O(samples^2).
  | DepthOfField {
    focalLength :: Double,
    aperture :: Double,
    dofSamples :: Int
  }
  deriving (Show, Read)

data Primitive 
  = Sphere { 
    center :: Vec3, radius :: Double, material :: Material }
  | Plane  { 
    pnorm :: Vec3, point :: Vec3, material :: Material }
  deriving (Show, Read, Eq)

data Ray = Ray { direction :: Vec3, position :: Vec3 } deriving (Show)

type ColorTriple = (Double, Double, Double)

data Light
  = PhongLight {
    speclight :: ColorTriple,
    difflight :: ColorTriple,
    loclight  :: Vec3
  } deriving (Show, Read)

-- | Combination models determine how materials on an object are combined.
data CombinationModel
  -- | Sum the two colors after multiplying the primary by weight and the
  -- secondary by (1-weight). The weighting term should be in [0,1].
  = WeightSum { weight::Double }
  -- | Sum the colors, truncating if any component goes over the maximum.
  | FlatSum
  -- | Take the product of the two colors. This creates weird effects which you
  -- almost certainly do not want, but it's interesting to look at.
  | Multiply
  deriving (Show, Eq, Read)

data Material
  -- | A material where every point on the material has the same color.
  = ColorMaterial {
    basecolor :: ColorTriple
  }

  -- | A material which reflects rays. Optionally, the material can have a base
  -- material, whose color is blended with the reflections using the
  -- reflectivity parameter. 
  -- TODO: Use a combination model here.
  | ReflectiveMaterial {
    base :: Material,
    reflectivity :: Double
  }

  -- | A Phong-reflectance based material.
  | PhongMaterial {
    specular :: ColorTriple,
    diffuse :: ColorTriple,
    ambient :: ColorTriple,
    phongexp :: Int
  }

  -- | A transparent material with a refractive index. This can be blended with
  -- a base material using the provided combination model.
  | TransparentMaterial {
    base :: Material,
    cmodel :: CombinationModel,
    refindex :: Double
  }

  -- | A material that doesn't exist. This can be convenient for a placeholder
  -- when you don't want to provide an actual material (like as the base
  -- material of a TransparentMaterial that is fully transparent).
  | NullMaterial
  deriving (Show, Read, Eq)

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
  -- | Describes a point in the image using pixel coordinates, allowing for
  -- fractional pixels for subpixel sampling.
  Point2D Double Double
  -- | Describes a point in the image using offsets from the center, where each
  -- field goes from zero to one.
  | RelPoint2D Double Double
  deriving (Show)

-- | Returns the transmittance (1 minus the opacity) of any material.
transmittance :: Material -> Double
transmittance (TransparentMaterial _ (WeightSum op) _) = op
transmittance _ = 0

-- | Returns the global ambient of the scene
glambient :: Scene -> ColorTriple
glambient scene =
  case mapMaybe
        (\opt -> case opt of GlobalAmbient t -> Just t; otherwise -> Nothing)
        (options scene) of
    amb:_ -> amb
    otherwise -> (0, 0, 0)

-- | Returns the number of subpixels used for MSAA, or Nothing if no
-- antialiasing is to be performed.
subpixels :: Scene -> Maybe Double
subpixels scene =
  case mapMaybe
        (\opt -> case opt of Antialiased t -> Just t; otherwise -> Nothing)
        (options scene) of
    subp:_ -> Just subp
    otherwise -> Nothing

-- | Returns the DepthOfField option, or nothing if no depth of field is to be
-- used.
dofinfo :: Scene -> Maybe Option
dofinfo scene =
  case mapMaybe
        (\opt -> case opt of 
            dof@(DepthOfField _ _ _) -> Just dof
            otherwise -> Nothing)
        (options scene) of
    dofs:_ -> Just dofs
    otherwise -> Nothing

-- | Returns True if depth of field is to be simulated, and false otherwise.
dofenabled :: Scene -> Bool
dofenabled = isJust . dofinfo

-- | Returns the number of samples to use for calculating depth of field. This
-- will throw an exception if depth of field is not enabled for the provided 
-- scene.
dofsamples :: Scene -> Int
dofsamples = dofSamples . fromJust . dofinfo

-- | Returns the focal length of the camera. This will throw an exception if 
-- depth of field is not enabled for the provided scene.
dofdepth :: Scene -> Double
dofdepth = focalLength . fromJust . dofinfo

-- | Returns the aperture size of the camera. This will throw an exception if 
-- depth of field is not enabled for the provided scene.
dofaperture :: Scene -> Double
dofaperture = aperture . fromJust . dofinfo

-- | Convenience method for turning a pair of integers into a Point2D.
p2d :: Int -> Int -> Point2D
p2d x y = Point2D (fromIntegral x) (fromIntegral y)

-- | Convenience method for creating colors
color :: Double -> Double -> Double -> ColorTriple
color r g b = (r, g, b)

-- | Convenience method for creating ColorMaterials.
colorm :: Double -> Double -> Double -> Material
colorm r g b = ColorMaterial (r, g, b)

-- | Converts pixel locations into relative locations in [-1,1], where -1
-- corresponds to the left or bottom edge, and 1 corresponds to the right or top
-- edge.
toRelPoint :: Size -> Point2D -> Point2D
toRelPoint size (Point2D x y) = 
  RelPoint2D ((x - xc) / xc) ((y - yc) / yc)
  where xc = fromIntegral (fst size) / 2
        yc = fromIntegral (snd size) / 2

-- | Creates a viewer
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

-- | Convert an ArtRay color to a GD color.
colorFrom :: ColorTriple -> Color
colorFrom (r, g, b) = 
  rgb (round (r * 255)) (round (g * 255)) (round (b * 255))

-- | Take the product of two colors
combine :: ColorTriple -> ColorTriple -> ColorTriple 
combine (r1, g1, b1) (r2, g2, b2) = (r1 * r2, g1 * g2, b1 * b2)

-- | Multiply a color by a scalar
scale :: ColorTriple -> Double -> ColorTriple
scale (r, g, b) s = (s * r, s * g, s * b)

-- | Weight two colors with the provided weighting factor. The first color is
-- multiplied by the weight, the second by one minus the weight.
weightedCombine :: Double -> ColorTriple -> ColorTriple -> ColorTriple
weightedCombine w (r1, g1, b1) (r2, g2, b2) =
  (mul w r1 r2, mul w g1 g2, mul w b1 b2)
  where mul w a b = w * a + (1 - w) * b

-- | Ensure that each component of the color is in [0, 1].
normalizeColor (r, g, b) = (n r, n g, n b) 
  where n x = max 0 (min 1 x)

-- | Take the sum of a list of colors
sumLight :: [ColorTriple] -> ColorTriple
sumLight = foldl1 sumColor

-- | Take the sum of a pair of colors, truncating if it results in a value
-- outside [0, 1].
sumColor :: ColorTriple -> ColorTriple -> ColorTriple
sumColor (r1, g1, b1) (r2, g2, b2) = normalizeColor (r1 + r2, g1 + g2, b1 + b2)

-- | Take the sum of a pair of colors without normalizing.
sumColor' :: ColorTriple -> ColorTriple -> ColorTriple
sumColor' (r1, g1, b1) (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)

-- | Find the mean value of a list of colors.
meanColor :: [ColorTriple] -> ColorTriple
meanColor colors = div $ foldl1 sumColor' colors
  where l = fromIntegral $ length colors
        div (r, g, b) = (r / l, g / l, b / l)
