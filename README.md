ArtRay: A Haskell Raytracer
===========================

Running ArtRay
--------------

Compilation is handled by the build.sh script. After running build.sh,
you can execute ArtRay as follows:

    ./artray [input scene] [output image]

Output is always written in PNG format. Input is in ArtRay format, which is
essentially a bunch of nested data constructors. Better docs for this are on
their way -- until they arrive, the samples in scenes/ give some examples as to
how to specify a scene, and all of the primitives available to you can be seen
in ArtRay/Primitives.hs.

Current Features
----------------
* Half-plane primitive
* Phong reflectance
* Shadows, including partial occlusion by transparent materials
* Point light sources
* Basic reflection model
* Basic refraction model

Planned Features:
* Triangle meshes
* Texturing
* Anti-aliasing
