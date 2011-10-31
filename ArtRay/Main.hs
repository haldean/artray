import Data.Vect.Double
import ArtRay.Primitives
import ArtRay.Render
import System (getArgs)
import System.IO

readScene :: FilePath -> IO Scene
readScene path =
  do
    infile <- openFile path ReadMode
    contents <- hGetContents infile
    return ((read contents)::Scene)

scene = (Scene
    [(Sphere (Vec3 14 (-4) 0) 3 
       (PhongMaterial (1, 1, 1) (0.7, 0, 0) (1, 0, 0) 4)),
     (Sphere (Vec3 10 0 0) 1 
       (PhongMaterial (1, 1, 1) (0, 0.7, 0) (0, 1, 0) 4)),
     (Plane (Vec3 (-1) 0 0) (Vec3 13 0 0) 
       (PhongMaterial (0, 0, 0) (0, 0, 0.7) (0, 0, 1) 4))]
    (0, 0, 0)
    (0.4, 0.4, 0.4)
    [(PhongLight (1, 1, 1) (1, 1, 1) (Vec3 6 4 0))]
    (Viewer (Vec3 0 0 0) (Vec3 0 0.422 0) (Vec3 0 0 0.422) (Vec3 1 (-0.2) 0)))

main = getArgs >>= \args -> 
  if length args /= 2 then putStrLn "Must supply input and output file names"
  else do
    scene <- readScene (head args)
    render scene 400 (args !! 1)
