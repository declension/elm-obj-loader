# elm-obj-loader

This is a small library for importing [wavefront .obj][objSpecs] files into your WebGL application.

**Caution!**
This is currently not useful without `indexedTriangles`, which will be available in [elm-community/webgl 2.0.0][webgl2.0].

# Limitations
Only a small subset of the .obj file specification is supported.

The easiest is to export/convert your model from Blender or another 3D content creation tool as an .obj file.

Make sure you tick the right options:   
TODO: img

Completely supported face types are those with vertex position, normals and optionally vertex texture coordinates. (`v`, `vn`, `vt`)

Currently there is partial support for models without vertex normals (no `vn`).   
Smooth groups are ignored (`g n`).   
Vertex normals are calculated for flat shading, but they are currently slightly wrong. Sorry -.-

Only tris and quads are supported. (Tris might load slightly faster)   
No free-form curves/surfaces.    
No material library (mtllib) support.   

# Demo/Examples
[Suzanne from Blender][loaderDemo] / [src](/examples).



[webgl2.0]: https://github.com/elm-community/webgl/tree/2.0
[loaderDemo]: https://zinggi.github.io/randomDemos/webgl/objLoader.html
[objSpecs]: http://www.martinreddy.net/gfx/3d/OBJ.spec
