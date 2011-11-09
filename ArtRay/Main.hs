module Main where

import Data.Vect.Double
import ArtRay.Primitives
import ArtRay.Render
import System (getArgs)
import System.IO

-- | Read a scene from a file
readScene :: FilePath -> IO Scene
readScene path =
  do
    infile <- openFile path ReadMode
    contents <- hGetContents infile
    return (read contents::Scene)

-- | Artificially inflate my Haddock coverage.
main :: IO ()
main = getArgs >>= \args -> 
  if length args /= 2 then putStrLn "Must supply input and output file names"
  else do
    scene <- readScene (head args)
    render scene 400 (args !! 1)
