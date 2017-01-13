module OBJ.Types exposing (..)

{-|
These are the types used by the obj loader.

@docs ObjFile, Mesh, MeshWith

--
@docs Vertex, VertexWithTexture, VertexWithTextureAndTangent


-}

import Dict exposing (Dict)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)


{-| A .obj file is optionally divided into different groups/objects.
Each group/object is optionally made up of different meshes, each with it's own material.

So the keys of this dictionary are:

    Dict GroupNameOrObjectName (Dict MaterialName Mesh)

If no name is specified in the input file, "__default__" will be used instead.
-}
type alias ObjFile =
    Dict String (Dict String Mesh)


{-|
A `Mesh` loaded by the obj loader is a record with a list of vertices and a list of indices.
Depending on the mesh type and the loading options you get a different kind of mesh.
They differ on what information a vertex contains.

These meshes are meant to be used with `WebGL.indexedTriangles mesh.vertices mesh.indices`.
-}
type Mesh
    = WithoutTexture (MeshWith Vertex)
    | WithTexture (MeshWith VertexWithTexture)
    | WithTextureAndTangent (MeshWith VertexWithTextureAndTangent)


{-|
-}
type alias MeshWith a =
    { vertices : List a
    , indices : List ( Int, Int, Int )
    }


{-| -}
type alias Vertex =
    { position : Vec3, normal : Vec3 }


{-| -}
type alias VertexWithTexture =
    { position : Vec3, texCoord : Vec2, normal : Vec3 }


{-|
The `tangent` is a vector pointing tangential to the object surface, in the direction of the `u` texture coordinate.
This is needed for doing tangent space normal mapping.
The 4th component is either 1 or -1 and has to be used to get the bitangent in the glsl shader,
e.g: `vec3 bitangent = cross(normal, tangent.xyz) * tangent.w`

more info here:
https://web.archive.org/web/20160409104130/http://www.terathon.com/code/tangent.html
-}
type alias VertexWithTextureAndTangent =
    { position : Vec3, texCoord : Vec2, normal : Vec3, tangent : Vec4 }
