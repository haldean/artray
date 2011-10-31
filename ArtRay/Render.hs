module ArtRay.Render where

import Data.Vect.Double
import ArtRay.Primitives
import ArtRay.RayTrace
import Graphics.GD
import ArtRay.Geometry

applyToImage :: (Size -> Point -> Color) -> Image -> IO()
applyToImage imFunc im =
  do 
    size <- imageSize im
    sequence_ [setPixel (i,j) (imFunc size (i,j)) im | i <- [0..(fst size)], j <- [0..(snd size)]]

render :: Scene -> Viewer -> Int -> String -> IO()
render scene viewer size outfile =
  do
    im <- newImage (size, size)
    applyToImage (rayTraceImage scene viewer) im
    savePngFile outfile im
