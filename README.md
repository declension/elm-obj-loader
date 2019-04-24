elm-obj-loader
==============

This is an Elm library for importing simple [Wavefront .obj][objSpecs] files into your WebGL application.

It is currently Elm 0.19, though [the previous incarnation](https://package.elm-lang.org/packages/Zinggi/elm-obj-loader/) works for 0.18.


Examples
--------
 * [Model viewer][modelViewer]
 * [Suzanne from Blender][suzanne]

See the [demo source](/examples).


Usage
-----
* Export / convert your model from Blender or other 3D content creation tool as an `.obj` file.
* The default export options from Blender work fine.
* Make sure you keep _Write Normals_ selected.


Limitations
-----------
 
Completely supported face types are those with vertex position, normals and optionally vertex texture coordinates. (`v`, `vn`, `vt`)
Only a small subset of the specification is supported though - specifically:

* Your model needs vertex normals (`vn`).
* Smooth groups are ignored (`s n`).  
* Only tris and quads are supported. (Tris might load slightly faster)    
* No free-form curves / surfaces.    
* No material library ([mtllib](http://people.sc.fsu.edu/~jburkardt/data/mtl/mtl.html)) support.   
* No negative indexing.   



[suzanne]: https://zinggi.github.io/randomDemos/webgl/objLoader_simple.html
[modelViewer]: https://zinggi.github.io/randomDemos/webgl/objLoader_modelViewer.html
[objSpecs]: http://www.martinreddy.net/gfx/3d/OBJ.spec
