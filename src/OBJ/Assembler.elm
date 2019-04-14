module OBJ.Assembler exposing (addCurrentGroup, addCurrentMesh, addFace, addFaceToMesh, applyForFace, applyForFaceA, arrayUpdate, compile, compileHelper, createMesh, emptyCompileState, finalizeMesh, fst2, get2, get3, getFaceTangent, getOrInsertVN, getOrInsertVTN, getOrInsertVTNT, insertLine, t3map, triangulate, triangulateFace, updateArray)

import Array exposing (Array)
import Debug exposing (toString)
import Dict exposing (Dict)
import Math.Vector2 as V2 exposing (Vec2)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Vector4 as V4 exposing (Vec4, vec4)
import OBJ.InternalTypes exposing (..)
import OBJ.Types exposing (..)


compile config lines =
    compileHelper (emptyCompileState config) lines
        |> addCurrentMesh
        |> addCurrentGroup
        |> .groups


emptyCompileState config =
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
    , knownVertexTexturesTangents = Dict.empty
    , knownVertex = Dict.empty
    , config = config
    }


compileHelper state lines =
    case lines of
        [] ->
            state

        l :: ls ->
            compileHelper (insertLine l state) ls


{-| This 'inserts' a line into the state.
This means it manipulates the current state to reflect state changing commands
and builds meshes on the fly.
-}
insertLine line state =
    case line of
        Object s ->
            -- even though the specs doesn't give it any meaningful meaning,
            -- I treat is exactly like a group statement.
            -- This is because blender uses o instead of g per default.
            addCurrentGroup state
                |> (\st -> { st | currentGroupName = s })

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
    -- Adds the current mesh to the current group.
    -- We also normalize all values here that need normalizing
    case state.currentMesh of
        Just m ->
            { state
                | currentGroup = Dict.insert state.currentMaterialName (finalizeMesh m) state.currentGroup
                , currentMesh = Nothing
                , knownVertexTextures = Dict.empty
                , knownVertexTexturesTangents = Dict.empty
                , knownVertex = Dict.empty
            }

        _ ->
            state


finalizeMesh : MeshT -> Mesh
finalizeMesh mesh =
    case mesh of
        WithTextureT m ->
            WithTexture m

        WithoutTextureT m ->
            WithoutTexture m

        WithTextureAndTangentT m ->
            WithTextureAndTangent { indices = m.indices, vertices = Array.foldr reducer [] m.vertices }


reducer : VertexWithTextureAndTangentT -> List VertexWithTextureAndTangent -> List VertexWithTextureAndTangent
reducer { position, texCoord, normal, sdir, tdir } acc =
    let
        -- handedness:
        -- https://web.archive.org/web/20160409104130/http://www.terathon.com/code/tangent.html
        w =
            if V3.dot (V3.cross normal sdir) tdir < 0 then
                -1

            else
                1

        { x, y, z } =
            -- I have not seen this anywhere, but I added it because I sometimes got (0,0,0)
            if V3.lengthSquared sdir /= 0 then
                V3.toRecord <| V3.normalize (V3.sub sdir (V3.scale (V3.dot normal sdir) normal))

            else
                V3.toRecord <| V3.cross (V3.normalize (V3.sub tdir (V3.scale (V3.dot normal tdir) normal))) normal
    in
    { position = position
    , texCoord = texCoord
    , normal = normal
    , tangent = vec4 x y z w
    }
        :: acc


addCurrentGroup state =
    if Dict.isEmpty state.currentGroup then
        state

    else
        { state
            | groups = Dict.insert state.currentGroupName state.currentGroup state.groups
            , currentGroup = Dict.empty
        }


addFace f state =
    -- this function adds a single face to the currentMesh
    -- for this it needs a dictionary containing the already known vertices,
    -- indexed using the v/vn etc.
    case state.currentMesh of
        Nothing ->
            -- we dont have a mesh yet, create one based on the type of the face
            addFaceToMesh f (createMesh state.config.withTangents f) { state | currentIndex = 0 }

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

        ( FTVertexTextureNormal ( v1, v2, v3 ), WithTextureAndTangentT m ) ->
            let
                tangents =
                    getFaceTangent ( v1, v2, v3 ) state

                ( newState, newVs, newIs ) =
                    applyForFaceA (getOrInsertVTNT tangents) ( v1, v2, v3 ) state

                newMesh =
                    WithTextureAndTangentT { m | indices = newIs :: m.indices, vertices = Array.append m.vertices newVs }
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
            -- TODO: lift this error into a Result type
            Debug.log "ERROR: mixed face types in the model!" <|
                { config = { withTangents = False }
                , currentMesh = Nothing
                , currentGroup = Dict.empty
                , currentGroupName = "(error)"
                , currentMaterialName = "(error)"
                , vs = Array.empty
                , vts = Array.empty
                , vns = Array.empty
                , groups = Dict.empty
                , currentIndex = 0
                , knownVertexTextures = Dict.empty
                , knownVertexTexturesTangents = Dict.empty
                , knownVertex = Dict.empty
                }


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


applyForFaceA f ( i1, i2, i3 ) s_0 =
    let
        ( s_1, vs_1, i_1 ) =
            f i1 s_0

        ( s_2, vs_2, i_2 ) =
            f i2 s_1

        ( s_3, vs_3, i_3 ) =
            f i3 s_2
    in
    ( s_3, Array.append (Array.append vs_1 vs_2) vs_3, ( i_3, i_2, i_1 ) )


getFaceTangent (( ( pi1, ti1, ni1 ), ( pi2, ti2, ni2 ), ( pi3, ti3, ni3 ) ) as index) { vs, vts, vns } =
    -- This is from here:
    -- https://web.archive.org/web/20160409104130/http://www.terathon.com/code/tangent.html
    -- But since the reference doesn't mention what to do in case the denominator is 0,
    -- This is probably not correct.
    case ( get3 ( pi1, pi2, pi3 ) vs vs vs, get3 ( ti1, ti2, ti3 ) vts vts vts ) of
        ( Just ( v1, v2, v3 ), Just ( w1, w2, w3 ) ) ->
            let
                ( vv1, vv2, vv3 ) =
                    t3map V3.toRecord ( v1, v2, v3 )

                ( ww1, ww2, ww3 ) =
                    t3map V2.toRecord ( w1, w2, w3 )

                ( ( x1, x2 ), ( y1, y2 ), ( z1, z2 ) ) =
                    ( ( vv2.x - vv1.x, vv3.x - vv1.x )
                    , ( vv2.y - vv1.y, vv3.y - vv1.y )
                    , ( vv2.z - vv1.z, vv3.z - vv1.z )
                    )

                ( ( s1, s2 ), ( t1, t2 ) ) =
                    ( ( ww2.x - ww1.x, ww3.x - ww1.x ), ( ww2.y - ww1.y, ww3.y - ww1.y ) )

                denom =
                    s1 * t2 - s2 * t1

                r =
                    if abs denom <= 0.000001 then
                        0.1

                    else
                        1 / denom

                sdir =
                    vec3 ((t2 * x1 - t1 * x2) * r) ((t2 * y1 - t1 * y2) * r) ((t2 * z1 - t1 * z2) * r)

                tdir =
                    vec3 ((s1 * x2 - s2 * x1) * r) ((s1 * y2 - s2 * y1) * r) ((s1 * z2 - s2 * z1) * r)
            in
            ( sdir, tdir )

        _ ->
            -- TODO: lift this error into a Result type
            ( vec3 1 1 1, vec3 1 1 1 )
                |> log ("index " ++ toString index ++ " out of bounds!\nThis should never happen with a well formed file")


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
                    -- TODO: lift this error into a Result type
                    ( state, [], -42 )
                        |> log ("index " ++ toString index ++ " out of bounds!\nThis should never happen with a well formed file")


getOrInsertVTNT ( s_dir, t_dir ) index ({ vs, vts, vns, knownVertexTexturesTangents, currentIndex } as state) =
    case Dict.get index knownVertexTexturesTangents of
        Just i ->
            case state.currentMesh of
                Just (WithTextureAndTangentT m) ->
                    ( { state
                        | currentMesh =
                            Just
                                (WithTextureAndTangentT
                                    { m
                                        | vertices =
                                            updateArray i
                                                (\({ sdir, tdir } as v) ->
                                                    { v | sdir = V3.add sdir s_dir, tdir = V3.add tdir t_dir }
                                                )
                                                m.vertices
                                    }
                                )
                      }
                    , Array.empty
                    , i
                    )

                _ ->
                    -- should never happen
                    ( state, Array.empty, i )

        Nothing ->
            case get3 index vs vts vns of
                Just ( p, t, n ) ->
                    ( { state
                        | knownVertexTexturesTangents = Dict.insert index currentIndex knownVertexTexturesTangents
                        , currentIndex = currentIndex + 1
                      }
                    , Array.fromList [ VertexWithTextureAndTangentT p t n s_dir t_dir ]
                    , currentIndex
                    )

                Nothing ->
                    -- TODO: lift this error into a Result type
                    ( state, Array.empty, -42 )
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
                    -- TODO: lift this error into a Result type
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
        Three { a, b, c } ->
            [ ( a, b, c ) ]

        Four { a, b, c, d } ->
            [ ( a, b, c ), ( d, a, c ) ]


createMesh withTangents f =
    let
        emptyMeshT =
            { vertices = Array.empty, indices = [] }

        emptyMesh =
            { vertices = [], indices = [] }
    in
    case f of
        FTVertexTextureNormal _ ->
            if withTangents then
                WithTextureAndTangentT emptyMeshT

            else
                WithTextureT emptyMesh

        FTVertexNormal _ ->
            WithoutTextureT emptyMesh



--
-- Some helpers:
--


t3map f ( a, b, c ) =
    ( f a, f b, f c )


updateArray i f a =
    case Array.get i a of
        Just v ->
            Array.set i (f v) a

        Nothing ->
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
