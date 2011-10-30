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
    [--(Sphere (Vec3 8 (-2) 0) 1 (colorm 0 1 0)),
     --(Sphere (Vec3 8 2 0) 1 (ReflectiveMaterial (color 1 0 0) 0.5))]
     (Sphere (Vec3 8 0 0) 1 
      (PhongMaterial (color 1 1 1) (color 1 0 0) (color 1 0 0) 2))]
    (color 0 0 0)
    (color 0.3 0.3 0.3)
    [(PhongLight (color 0.3 0.3 0.3) (color 0.4 0.4 0.4) (Vec3 0 0 0))])

viewer = view (Vec3 0 0 0) (pi / 4) (Vec3 1 0 0) (Vec3 0 0 1)

render :: Scene -> Viewer -> Int -> String -> IO()
render scene viewer size outfile =
  do
    im <- newImage (size, size)
    applyToImage (rayTraceImage scene viewer) im
    savePngFile outfile im

main = getArgs >>= \args -> 
  if null args then putStrLn "Must supply output file name"
  else render scene viewer 300 (head args)
