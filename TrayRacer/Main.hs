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

main = getArgs >>= \args -> 
  let scene  = Scene [(Sphere (Vec3 8 2 2) 3 (ColorMaterial 0 255 0)),
                      (Sphere (Vec3 4 0 0) 1 (ColorMaterial 255 0 0))]
      viewer = view (Vec3 0 0 0) (pi / 2) (Vec3 1 0 0) (Vec3 0 0 1)
  in do
    im <- newImage (400, 600)
    applyToImage (rayTraceImage scene viewer) im
    savePngFile (head args) im
