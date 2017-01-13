# elm-obj-loader

This is a small library for importing [wavefront .obj][objSpecs] files into your WebGL application.

# Demo/Examples
 * [Model viewer][modelViewer]
 * [Suzanne from Blender][suzanne]

[Src](/examples)

# Limitations
Only a small subset of the .obj file specification is supported.

The easiest is to export/convert your model from Blender or another 3D content creation tool as an .obj file.

The default export options from blender work fine. Make sure you keep 'Write Normals' selected.

Completely supported face types are those with vertex position, normals and optionally vertex texture coordinates. (`v`, `vn`, `vt`)

Smooth groups are ignored (`s n`).  
Your model needs vertex normals (`vn`).  

Only tris and quads are supported. (Tris might load slightly faster)    
No free-form curves/surfaces.    
No material library (mtllib) support.   
No negative indexing.   





[suzanne]: https://zinggi.github.io/randomDemos/webgl/objLoader_simple.html
[modelViewer]: https://zinggi.github.io/randomDemos/webgl/objLoader_modelViewer.html
[objSpecs]: http://www.martinreddy.net/gfx/3d/OBJ.spec
