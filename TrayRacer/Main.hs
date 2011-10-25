module TrayRacer.Main where

import Data.Vect.Double
import TrayRacer.Primitives
import TrayRacer.RayTrace
import Graphics.GD
import TrayRacer.Geometry
import System (getArgs)

applyToImage :: (Size -> Point -> Color) -> Image -> IO()
applyToImage imFunc im =
  do 
    size <- imageSize im
    sequence_ [setPixel (i,j) (imFunc size (i,j)) im | i <- [0..(fst size)], j <- [0..(snd size)]]

scene =
  (Scene 
    [(Sphere (Vec3 2 2 0) 1 (ColorMaterial 0 255 0)),
     (Sphere (Vec3 4 0 0) 1 (ReflectiveMaterial (ColorMaterial 255 0 0) 0.5))]
    (ColorMaterial 0 0 0))

viewer = view (Vec3 0 0 0) (pi / 2) (Vec3 1 0 0) (Vec3 0 0 1)

main = getArgs >>= \args -> 
  if null args then putStrLn "Must supply output file name"
  else do
    im <- newImage (300, 300)
    applyToImage (rayTraceImage scene viewer) im
    savePngFile (head args) im
