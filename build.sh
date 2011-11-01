if [ ! -d "build" ]; then
  mkdir build
fi
ghc ArtRay/Main.hs -o artray -odir build/ -hidir build/ -O
