if [ ! -d "build" ]; then
  mkdir build
fi
ghc ArtRay/Main.hs -o main -odir build/ -hidir build/ -O
