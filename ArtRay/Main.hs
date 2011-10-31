import Data.Vect.Double
import ArtRay.Primitives
import ArtRay.Render
import System (getArgs)

scene =
  (Scene
    [--(Sphere (Vec3 8 (-2) 0) 1 (colorm 0 1 0)),
     --(Sphere (Vec3 8 2 0) 1 (ReflectiveMaterial (color 1 0 0) 0.5))]
     (Sphere (Vec3 14 (-4) 0) 3 
      (PhongMaterial (color 1 1 1) (color 0.7 0 0) (color 1 0 0) 4)),
     (Sphere (Vec3 10 0 0) 1 
      (PhongMaterial (color 1 1 1) (color 0 0.7 0) (color 0 1 0) 4)),
     (Plane (Vec3 (-1) 0 0) (Vec3 13 0 0) 
      (PhongMaterial (color 0 0 0) (color 0 0 0.7) (color 0 0 1) 4))]
    (color 0 0 0)
    (color 0.4 0.4 0.4)
    [(PhongLight (color 1 1 1) (color 1 1 1) (Vec3 6 4 0))])

viewer = view (Vec3 0 0 0) (pi / 4) (Vec3 1 (-0.2) 0) (Vec3 0 0 1)

main = getArgs >>= \args -> 
  if null args then putStrLn "Must supply output file name"
  else render scene viewer 400 (head args)
