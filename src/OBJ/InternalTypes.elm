module OBJ.InternalTypes exposing (..)

import Math.Vector3 exposing (Vec3)
import Math.Vector2 exposing (Vec2)
import Array.Hamt as Array exposing (Array)
import OBJ.Types exposing (..)


--
-- {- DEBUG -}
--
-- import Native.Time
--
--
-- debugNow : () -> Float
-- debugNow =
--     Native.Time.now
--
--
-- time : String -> (() -> a) -> a
-- time name f =
--     let
--         a =
--             debugNow ()
--     in
--         f ()
--             |> (\r -> log (name ++ ": " ++ toString (debugNow () - a) ++ " ms") r)


log s a =
    let
        _ =
            Debug.log s ()
    in
        a


type MeshT
    = WithoutTextureT (MeshWith Vertex)
    | WithTextureT (MeshWith VertexWithTexture)
    | WithTextureAndTangentT (MeshWithT VertexWithTextureAndTangentT)


type alias MeshWithT a =
    { vertices : Array a
    , indices : List Int3
    }


type alias VertexWithTextureAndTangentT =
    { position : Vec3, texCoord : Vec2, normal : Vec3, sdir : Vec3, tdir : Vec3 }


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
    = FVertexTextureNormal (ThreeOrFour Int3)
    | FVertexNormal (ThreeOrFour Int2)


type FaceTriangle
    = FTVertexTextureNormal ( Int3, Int3, Int3 )
    | FTVertexNormal ( Int2, Int2, Int2 )


type ThreeOrFour a
    = Three ( a, a, a )
    | Four ( a, a, a, a )


type Group
    = GV { faces : List Int3 }
    | GVT { faces : List ( Int2, Int2, Int2 ) }
    | GVTN { faces : List ( Int3, Int3, Int3 ) }
    | GVN { faces : List ( Int2, Int2, Int2 ) }


type alias Int2 =
    ( Int, Int )


type alias Int3 =
    ( Int, Int, Int )
