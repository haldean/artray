if [ ! -d "build" ]; then
  mkdir build
fi
ghc ArtRay/Main.hs -o ar -odir build/ -hidir build/ -O
