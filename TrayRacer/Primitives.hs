module TrayRacer.Primitives where

import Graphics.GD
import Data.Vect.Double
import Debug.Trace

data Primitive = 
  Sphere { center :: Vec3, radius :: Double, material :: Material }
  deriving (Show)
data Ray = Ray { direction :: Vec3, position :: Vec3 } deriving (Show)
data Material = ColorMaterial Double Double Double deriving (Show)

materialColor :: Material -> Color
materialColor (ColorMaterial r g b) = 
  rgb (truncate (255 * r)) (truncate (255 * g)) (truncate (255 * b))

data Point2D = 
  -- | Describes a point in the image using pixel coordinates
  Point2D Int Int
  -- | Describes a point in the image using offsets from the center, where each
  --   field goes from zero to one.
  | RelPoint2D Double Double
  deriving (Show)

toRelPoint :: Size -> Point2D -> Point2D
toRelPoint size (Point2D ix iy) = 
  RelPoint2D ((x - xc) / xc) ((y - yc) / yc)
  where xc = (fromIntegral (fst size)) / 2
        yc = (fromIntegral (snd size)) / 2
        x  = (fromIntegral ix)
        y  = (fromIntegral iy)

data Viewer = Viewer {
    -- | The location of the viewer
    location :: Vec3,
    -- | A vector pointing horizontally along the image plane
    u :: Vec3,
    -- | A vector pointing vertically along the image plane
    v :: Vec3,
    -- | A vector pointing from the viewer to the center of the image plane
    f :: Vec3
  } deriving (Show)

-- |Creates a viewer
view :: Vec3        -- ^ The location of the viewer
     -> Double      -- ^ The field of view of the viewer, in radians
     -> Vec3        -- ^ The vector from viewer to center of image plane
     -> Vec3        -- ^ Unit vector pointing vertically along the image plane
     -> Viewer
view loc fov f v =
  Viewer loc (scalarMul scale (crossprod v f)) (scalarMul scale v) f
  where scale = norm f * tan (fov / 2)

data Scene = Scene { geom :: [Primitive] } deriving (Show)

