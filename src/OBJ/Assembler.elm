module OBJ.Assembler exposing (..)

import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import OBJ.Types exposing (..)


-- TODO:
-- To make things easier, I could just treat "g .." and "o .." the same way.
-- Or just ignore grouping and only group things if they use a different material.


compile lines =
    compileHelper emptyCompileState lines
        |> addCurrentMesh
        |> addCurrentGroup
        |> .groups



--        |> toMeshes
-- emptyCompileState : CompileState


emptyCompileState =
    { -- this is a Dict (GroupName/String) Group
      -- it's the final output of this algorithm
      --  Group = Dict (MtlName/String) Mesh
      groups = Dict.empty
    , currentGroupName = "__default__"
    , currentMaterialName = "__default__"
    , currentMesh = Nothing
    , currentGroup = Dict.empty
    , vs = Array.empty
    , vts = Array.empty
    , vns = Array.empty
    , currentIndex = 0
    , knownVertexTextures = Dict.empty
    , knownVertex = Dict.empty
    }


compileHelper state lines =
    case lines of
        [] ->
            state

        l :: ls ->
            compileHelper (insertLine l state) ls


{-|
this 'inserts' a line into the state.
This means it manipulates the current state to reflect state changing commands
and buils meshes on the fly.
-}
insertLine line state =
    case line of
        Object s ->
            -- "o .."" statements are ignored,
            -- because the specs doesn't give it any meaningful meaning.
            state

        MtlLib s ->
            -- MtlLib statements are ignored,
            -- as I don't plan to support loading .mtl files
            state

        Group s ->
            addCurrentGroup state
                |> (\st -> { st | currentGroupName = s })

        Smooth s ->
            -- smooth groups are ignored.
            -- I tried to calculate these, but failed,
            -- since doing them correctly is more tricky than you might think:
            -- http://www.bytehazard.com/articles/vertnorm.html
            -- { state | activeSmoothGroup = s }
            state

        UseMtl s ->
            addCurrentMesh state
                |> (\st -> { st | currentMaterialName = s })

        V v ->
            { state | vs = Array.push v state.vs }

        Vt v ->
            { state | vts = Array.push v state.vts }

        Vn v ->
            { state | vns = Array.push v state.vns }

        F f ->
            triangulateFace f
                |> List.foldr addFace state


triangulateFace f =
    case f of
        FVertexTextureNormal a ->
            triangulate a |> List.map FTVertexTextureNormal

        FVertexNormal a ->
            triangulate a |> List.map FTVertexNormal


addCurrentMesh state =
    -- this adds the current mesh, to the current group.
    -- We can also normalize all values here that need normalizing
    case state.currentMesh of
        Just m ->
            { state
                | currentGroup = Dict.insert state.currentMaterialName (finalizeMesh m) state.currentGroup
                , currentMesh = Nothing
                , knownVertexTextures =
                    Dict.empty
                    -- , knownVertexTexturesTangents = Dict.empty
                , knownVertex = Dict.empty
            }

        _ ->
            state


finalizeMesh mesh =
    case mesh of
        WithTextureT m ->
            WithTexture m

        WithoutTextureT m ->
            WithoutTexture m

        _ ->
            Debug.crash "TODO"


addCurrentGroup state =
    if Dict.isEmpty state.currentGroup then
        state
    else
        { state
            | groups = Dict.insert state.currentGroupName state.currentGroup state.groups
            , currentGroup = Dict.empty
        }


addFace f state =
    -- this function should add a single face to the currentMesh
    -- for this it needs a dictionary containing the already known vertices,
    -- indexed using the v/vn etc. indices together with the smooth group
    -- flat shading needs to use a unique index for the smooth group to avoid sharing vertices.
    case state.currentMesh of
        Nothing ->
            -- we dont have a mesh yet, create one based on the type of the face
            addFaceToMesh f (createMesh f) { state | currentIndex = 0 }

        Just m ->
            addFaceToMesh f m state


addFaceToMesh f mesh ({ vs, vts, vns, currentIndex } as state) =
    -- add a face to the mesh
    case ( f, mesh ) of
        ( FTVertexTextureNormal ( v1, v2, v3 ), WithTextureT m ) ->
            let
                ( newState, newVs, newIs ) =
                    applyForFace getOrInsertVTN ( v1, v2, v3 ) state

                newMesh =
                    WithTextureT { m | indices = newIs :: m.indices, vertices = m.vertices ++ newVs }
            in
                { newState | currentMesh = Just newMesh }

        ( FTVertexNormal ( v1, v2, v3 ), WithoutTextureT m ) ->
            let
                ( newState, newVs, newIs ) =
                    applyForFace getOrInsertVN ( v1, v2, v3 ) state

                newMesh =
                    WithoutTextureT { m | indices = newIs :: m.indices, vertices = m.vertices ++ newVs }
            in
                { newState | currentMesh = Just newMesh }

        _ ->
            Debug.crash "todo"


applyForFace f ( i1, i2, i3 ) s_0 =
    let
        ( s_1, vs_1, i_1 ) =
            f i1 s_0

        ( s_2, vs_2, i_2 ) =
            f i2 s_1

        ( s_3, vs_3, i_3 ) =
            f i3 s_2
    in
        ( s_3, vs_1 ++ vs_2 ++ vs_3, ( i_3, i_2, i_1 ) )


getOrInsertVTN index ({ vs, vts, vns, knownVertexTextures, currentIndex } as state) =
    case Dict.get index knownVertexTextures of
        Just i ->
            ( state, [], i )

        Nothing ->
            case get3 index vs vts vns of
                Just ( p, t, n ) ->
                    ( { state
                        | knownVertexTextures = Dict.insert index currentIndex knownVertexTextures
                        , currentIndex = currentIndex + 1
                      }
                    , [ VertexWithTexture p t n ]
                    , currentIndex
                    )

                Nothing ->
                    ( state, [], -42 )
                        |> log ("index " ++ toString index ++ " out of bounds!\nThis should never happen with a well formed file")


getOrInsertVN index ({ vs, vns, knownVertex, currentIndex } as state) =
    case Dict.get index knownVertex of
        Just i ->
            ( state, [], i )

        Nothing ->
            case get2 index vs vns of
                Just ( p, n ) ->
                    ( { state
                        | knownVertex = Dict.insert index currentIndex knownVertex
                        , currentIndex = currentIndex + 1
                      }
                    , [ Vertex p n ]
                    , currentIndex
                    )

                Nothing ->
                    ( state, [], -42 )
                        |> log ("index " ++ toString index ++ " out of bounds!\nThis should never happen with a well formed file")


fst2 ( a, b, c ) =
    ( a, b )


arrayUpdate i f a =
    case Array.get i a of
        Just e ->
            Array.set i (f e) a

        _ ->
            a


triangulate threeOrFour =
    case threeOrFour of
        Three t ->
            [ t ]

        Four ( a, b, c, d ) ->
            [ ( a, b, c ), ( d, a, c ) ]


createMesh f =
    let
        emptyMesh =
            { vertices = [], indices = [] }
    in
        case f of
            FTVertexTextureNormal _ ->
                -- TODO: if with or without tangent should depend on the user config
                WithTextureT emptyMesh

            FTVertexNormal _ ->
                WithoutTextureT emptyMesh



--
-- Some helpers:
--


log s a =
    let
        _ =
            Debug.log s ()
    in
        a


get3 ( a, b, c ) a1 a2 a3 =
    case ( Array.get (a - 1) a1, Array.get (b - 1) a2, Array.get (c - 1) a3 ) of
        ( Just a_, Just b_, Just c_ ) ->
            Just ( a_, b_, c_ )

        _ ->
            Nothing


get2 ( a, b ) a1 a2 =
    case ( Array.get (a - 1) a1, Array.get (b - 1) a2 ) of
        ( Just a_, Just b_ ) ->
            Just ( a_, b_ )

        _ ->
            Nothing
