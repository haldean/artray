module ArtRay.Render where

import Data.Vect.Double
import ArtRay.Primitives
import ArtRay.RayTrace
import Graphics.GD
import ArtRay.Geometry

-- | Raytrace an image without antialiasing
rayTraceImage :: Scene -> Size -> Point2D -> ColorTriple
rayTraceImage scene size point@(Point2D x y) =
  pixelColor size scene (viewer scene) point

-- | Raytrace an image with antialiasing by subpixel sampling
rayTraceImage' :: Scene -> Size -> Point -> ColorTriple
rayTraceImage' scene size (x, y) =
  meanColor $ map (rayTraceImage scene size)
    [Point2D (fromIntegral x + x') (fromIntegral y + y')
     | x' <- [(-0.5),inc..0.5], y' <- [(-0.5),inc..0.5]]
  where inc = (-0.5) + (1 / (fromIntegral $ subpixels scene))

applyToImage :: (Size -> Point -> ColorTriple) -> Image -> IO()
applyToImage imFunc im =
  do 
    size <- imageSize im
    sequence_ [setPixel (i,j) (colorFrom $ imFunc size (i,j)) im
               | i <- [0..(fst size)], j <- [0..(snd size)]]

render :: Scene -> Int -> String -> IO()
render scene size outfile =
  do
    im <- newImage (size, size)
    applyToImage (rayTraceImage' scene) im
    savePngFile outfile im
