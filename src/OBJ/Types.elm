module OBJ.Types exposing (..)

import Math.Vector3 exposing (Vec3)
import Math.Vector2 exposing (Vec2)


type Mesh
    = WithoutTexture (MeshWith { pos : Vec3, norm : Vec3 })
    | WithTexture (MeshWith { pos : Vec3, norm : Vec3, coord : Vec2 })


type alias MeshWith a =
    { vertices : List a
    , indices : List Int3
    }


type
    Line
    -- v 1 3 4
    = V Vec3
      -- vt 2 4
    | Vt Vec2
      -- vn 3 3 1
    | Vn Vec3
      -- f 1 2 4
      -- f 1/3 2/3 1/7
      -- f 1/2/3 7/4/2 8/12/90
      -- f 4//8 4//1 6//2
    | F Face
      -- steteful stuff
    | Object String
    | Group String
    | Smooth String
    | MtlLib String
    | UseMtl String


type Face
    = FVertex ( Int, Int, Int )
    | FVertex4 ( Int, Int, Int, Int )
    | FVertexTexture ( Int2, Int2, Int2 )
    | FVertexTexture4 ( Int2, Int2, Int2, Int2 )
    | FVertexTextureNormal ( Int3, Int3, Int3 )
    | FVertexTextureNormal4 ( Int3, Int3, Int3, Int3 )
    | FVertexNormal ( Int2, Int2, Int2 )
    | FVertexNormal4 ( Int2, Int2, Int2, Int2 )


type alias Int2 =
    ( Int, Int )


type alias Int3 =
    ( Int, Int, Int )


type Group
    = GV { faces : List Int3 }
    | GVT { faces : List ( Int2, Int2, Int2 ) }
    | GVTN { faces : List ( Int3, Int3, Int3 ) }
    | GVN { faces : List ( Int2, Int2, Int2 ) }
