module OBJ.Assembler exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import OBJ.Types exposing (..)


-- TODO:
-- flat shading cannot share vertices!
-- the current flat shading must be wrong, as it tries to share vertices!
--
-- deal with smooth lightning:
-- create a dict of smooth groups.
-- a smooth group is a dict of vertex indices and their face normal.
-- face normals are added together.
-- at the end, all added face normals will be normalized
--
--
-- idea: when adding a face, don't add the indices, add the actuall data.
-- then in a second step missing data can be easily added,
-- then do the indexing based on the actual data.
--
-- To make things easier, I could just treat "g .." and "o .." the same way.
-- Or just ignore grouping and only group things if they use a different material.
--
-- type alias CompileState =
--     { currentGroup : Maybe Group
--     , currentGroupName : String
--     , currentMaterialName : String
--     , groups : Dict.Dict String Group
--     , mtlLib : Maybe String
--     , objectName : Maybe String
--     , vns : Array Vec3
--     , vs : Array Vec3
--     , vts : Array Vec2
--     }


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
    , activeSmoothGroop = "off"
    , currentMesh = Nothing
    , currentGroup = Dict.empty
    , vs = Array.empty
    , vts = Array.empty
    , vns = Array.empty
    , currentIndex = 0
    , knownVertexTextures = Dict.empty
    , knownVertex = Dict.empty
    }



-- compileHelper : CompileState -> List Line -> CompileState


compileHelper state lines =
    case lines of
        [] ->
            state

        l :: ls ->
            compileHelper (insertLine l state) ls



-- insertLine : Line -> CompileState -> CompileState


{-|
this 'inserts' a line into the state.
This means it manipulates the current state to reflect state changing commands
and buils meshes on the fly.
-}
insertLine line state =
    case line of
        Object s ->
            -- { state | objectName = Just s }
            -- "o .."" statements are ignored,
            -- because the specs doesn't give it any meaningful meaning.
            state

        MtlLib s ->
            -- { state | mtlLib = Just s }
            -- MtlLib statements are ignored,
            -- as I don't plan to support loading .mtl files
            state

        Group s ->
            addCurrentGroup state
                |> (\st -> { st | currentGroupName = s })

        Smooth s ->
            { state | activeSmoothGroop = s }

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
        FVertex a ->
            triangulate a |> List.map FTVertex

        FVertexTexture a ->
            triangulate a |> List.map FTVertexTexture

        FVertexTextureNormal a ->
            triangulate a |> List.map FTVertexTextureNormal

        FVertexNormal a ->
            triangulate a |> List.map FTVertexNormal


addCurrentMesh state =
    -- this should add the current mesh, to the current group.
    -- We can also normalize all values here that need normalizing
    case state.currentMesh of
        Just m ->
            { state
                | currentGroup = Dict.insert state.currentMaterialName m state.currentGroup
                , currentMesh = Nothing
                , knownVertexTextures =
                    Dict.empty
                    -- , knownVertexTexturesTangents = Dict.empty
                , knownVertex = Dict.empty
            }

        _ ->
            state



-- addCurrentGroup : CompileState -> CompileState


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
            -- { state | currentMesh = Just (addFaceToMesh state f (createMesh f)) }
            addFaceToMesh f (createMesh f) { state | currentIndex = 0 }

        Just m ->
            -- { state | currentMesh = Just (addFaceToMesh state f m) }
            addFaceToMesh f m state


addFaceToMesh f mesh ({ vs, vts, vns, currentIndex } as state) =
    -- add a face to the mesh
    case ( f, mesh ) of
        ( FTVertexTextureNormal ( v1, v2, v3 ), WithTexture m ) ->
            let
                ( newState, newVs, newIs ) =
                    applyForFace getOrInsertVTN ( v1, v2, v3 ) state

                newMesh =
                    WithTexture { m | indices = newIs :: m.indices, vertices = m.vertices ++ newVs }
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
            let
                ( p, t, n ) =
                    unsafeGet3 index vs vts vns

                v =
                    VertexWithTexture p t n
            in
                ( { state
                    | knownVertexTextures = Dict.insert index currentIndex knownVertexTextures
                    , currentIndex = currentIndex + 1
                  }
                , [ v ]
                , currentIndex
                )


getFaceNormal v1 v2 v3 =
    V3.normalize (V3.cross (V3.sub v1 v2) (V3.sub v3 v2))


triangulate threeOrFour =
    case threeOrFour of
        Three t ->
            [ t ]

        Four ( a, b, c, d ) ->
            [ ( a, b, c ), ( a, c, d ) ]


createMesh f =
    let
        emptyMesh =
            { vertices = [], indices = [] }
    in
        case f of
            FTVertex _ ->
                WithoutTexture emptyMesh

            FTVertexTexture _ ->
                -- TODO: if with or without tangent should depend on the user config
                WithTexture emptyMesh

            FTVertexTextureNormal _ ->
                WithTexture emptyMesh

            FTVertexNormal _ ->
                WithoutTexture emptyMesh



-- {-| This function adds a face to the current faces list.
--     Since there are 4*2 different face types and 4 + 1 different Mesh types,
--     this function has 4*2*2 branches.
--     (The meshes only add two different branches per face type)
-- -}
-- addFace f state =
--     case ( state.currentGroup, f ) of
--         ( Just (GV ({ faces } as g)), FVertex v ) ->
--             { state | currentGroup = Just (GV { g | faces = v :: faces }) }
--
--         ( Just (GV ({ faces } as g)), FVertex4 ( a, b, c, d ) ) ->
--             { state | currentGroup = Just (GV { g | faces = ( a, b, c ) :: ( c, d, a ) :: faces }) }
--
--         ( Nothing, FVertex v ) ->
--             { state | currentGroup = Just (GV { faces = [ v ] }) }
--
--         ( Nothing, FVertex4 ( a, b, c, d ) ) ->
--             { state | currentGroup = Just (GV { faces = [ ( a, b, c ), ( c, d, a ) ] }) }
--
--         ( Just (GVN ({ faces } as g)), FVertexNormal v ) ->
--             { state | currentGroup = Just (GVN { g | faces = v :: faces }) }
--
--         ( Just (GVN ({ faces } as g)), FVertexNormal4 ( a, b, c, d ) ) ->
--             { state | currentGroup = Just (GVN { g | faces = ( a, b, c ) :: ( c, d, a ) :: faces }) }
--
--         ( Nothing, FVertexNormal v ) ->
--             { state | currentGroup = Just (GVN { faces = [ v ] }) }
--
--         ( Nothing, FVertexNormal4 ( a, b, c, d ) ) ->
--             { state | currentGroup = Just (GVN { faces = [ ( a, b, c ), ( c, d, a ) ] }) }
--
--         ( Just (GVT ({ faces } as g)), FVertexTexture v ) ->
--             { state | currentGroup = Just (GVT { g | faces = v :: faces }) }
--
--         ( Just (GVT ({ faces } as g)), FVertexTexture4 ( a, b, c, d ) ) ->
--             { state | currentGroup = Just (GVT { g | faces = ( a, b, c ) :: ( c, d, a ) :: faces }) }
--
--         ( Nothing, FVertexTexture v ) ->
--             { state | currentGroup = Just (GVT { faces = [ v ] }) }
--
--         ( Nothing, FVertexTexture4 ( a, b, c, d ) ) ->
--             { state | currentGroup = Just (GVT { faces = [ ( a, b, c ), ( c, d, a ) ] }) }
--
--         ( Just (GVTN ({ faces } as g)), FVertexTextureNormal v ) ->
--             { state | currentGroup = Just (GVTN { g | faces = v :: faces }) }
--
--         ( Just (GVTN ({ faces } as g)), FVertexTextureNormal4 ( a, b, c, d ) ) ->
--             { state | currentGroup = Just (GVTN { g | faces = ( a, b, c ) :: ( c, d, a ) :: faces }) }
--
--         ( Nothing, FVertexTextureNormal v ) ->
--             { state | currentGroup = Just (GVTN { faces = [ v ] }) }
--
--         ( Nothing, FVertexTextureNormal4 ( a, b, c, d ) ) ->
--             { state | currentGroup = Just (GVTN { faces = [ ( a, b, c ), ( c, d, a ) ] }) }
--
--         ( mesh, face ) ->
--             Debug.crash
--                 ("Faces should all be the same type!\nMesh, face pair:\n\n"
--                     ++ toString ( state.currentGroup, face )
--                 )
--
-- {-| (v1, v2, v3) are assumed to be given in counter clock direction
-- -}
-- getNormalTo : ( Vec3, Vec3, Vec3 ) -> Vec3
-- getNormalTo ( v1, v2, v3 ) =
--     V3.normalize (V3.cross (V3.sub v1 v2) (V3.sub v3 v2))


log s a =
    let
        _ =
            Debug.log s ()
    in
        a


unsafeGet3 : ( Int, Int, Int ) -> Array a -> Array b -> Array c -> ( a, b, c )
unsafeGet3 ( a, b, c ) a1 a2 a3 =
    case ( Array.get (a - 1) a1, Array.get (b - 1) a2, Array.get (c - 1) a3 ) of
        ( Just a_, Just b_, Just c_ ) ->
            ( a_, b_, c_ )

        _ ->
            Debug.crash "index out of bounds!"



--
-- get3 : ( Int, Int, Int ) -> Array a -> Maybe ( a, a, a )
-- get3 ( a, b, c ) array =
--     case ( Array.get a array, Array.get b array, Array.get c array ) of
--         ( Just a, Just b, Just c ) ->
--             Just ( a, b, c )
--
--         _ ->
--             Nothing
--
--
-- toMeshes : CompileState -> Dict String Mesh
-- toMeshes state =
--     Dict.map
--         (\groupName group ->
--             case group of
--                 GV { faces } ->
--                     WithoutTexture (makeMeshV state.vs faces)
--                         |> log "GV"
--
--                 GVT { faces } ->
--                     WithTexture (makeMeshVT state.vs state.vts faces)
--                         |> log "GVT"
--
--                 GVN { faces } ->
--                     WithoutTexture (makeMeshVN state.vs state.vns faces)
--                         |> log "GVN"
--
--                 GVTN { faces } ->
--                     WithTexture (makeMeshVNT state.vs state.vns state.vts faces)
--                         |> log "GVTN"
--         )
--         state.groups
--
--
-- makeMeshV : Array Vec3 -> List Int3 -> MeshWith Vertex
-- makeMeshV vs fs =
--     makeMeshVHelper vs fs { verts = [], is = [], i = 0 }
--         |> (\{ is, verts } -> { vertices = verts, indices = is })
--
--
-- type alias TempMesh a =
--     { i : Int
--     , is : List Int3
--     , verts : List a
--     }
--
--
-- makeMeshVHelper : Array Vec3 -> List Int3 -> TempMesh Vertex -> TempMesh Vertex
-- makeMeshVHelper vs faces state =
--     case faces of
--         ( v1, v2, v3 ) :: fs ->
--             case ( Array.get (v1 - 1) vs, Array.get (v2 - 1) vs, Array.get (v3 - 1) vs ) of
--                 ( Just a, Just b, Just c ) ->
--                     let
--                         norm =
--                             getNormalTo ( c, b, a )
--                     in
--                         makeMeshVHelper vs
--                             fs
--                             { state
--                                 | verts = state.verts ++ [ { pos = a, norm = norm }, { pos = b, norm = norm }, { pos = c, norm = norm } ]
--                                 , is = ( state.i + 2, state.i + 1, state.i ) :: state.is
--                                 , i = state.i + 3
--                             }
--
--                 _ ->
--                     Debug.crash
--                         ("Index out of bound: "
--                             ++ toString ( v1, v2, v3 )
--                             ++ "\n\n"
--                             ++ "vs: "
--                             ++ toString (Array.length vs)
--                         )
--
--         [] ->
--             state
--
--
-- makeMeshVN : Array Vec3 -> Array Vec3 -> List ( Int2, Int2, Int2 ) -> MeshWith Vertex
-- makeMeshVN vs vns fs =
--     makeMeshVNHelper vs vns fs { verts = [], is = [], i = 0, knowns = Dict.empty }
--         |> (\{ is, verts } -> { vertices = verts, indices = is })
--
--
-- type alias TempMeshWithDict i a =
--     { i : Int
--     , is : List Int3
--     , verts : List a
--     , knowns : Dict i ( Int, a )
--     }
--
--
-- makeMeshVNHelper :
--     Array Vec3
--     -> Array Vec3
--     -> List ( Int2, Int2, Int2 )
--     -> TempMeshWithDict Int2 Vertex
--     -> TempMeshWithDict Int2 Vertex
-- makeMeshVNHelper vs vns faces state =
--     case faces of
--         ( ( v1, vn1 ), ( v2, vn2 ), ( v3, vn3 ) ) :: fs ->
--             let
--                 ( newState, newIs, newVerts ) =
--                     getOrUpdateCoordsVN vs vns [ ( v1, vn1 ), ( v2, vn2 ), ( v3, vn3 ) ] state
--             in
--                 makeMeshVNHelper vs
--                     vns
--                     fs
--                     { newState
--                         | verts = state.verts ++ newVerts
--                         , is = newIs :: state.is
--                     }
--
--         [] ->
--             state
--
--
-- makeMeshVNT :
--     Array Vec3
--     -> Array Vec3
--     -> Array Vec2
--     -> List ( Int3, Int3, Int3 )
--     -> MeshWith VertexWithTexture
-- makeMeshVNT vs vns vts fs =
--     makeMeshVNTHelper vs vns vts fs { verts = [], is = [], i = 0, knowns = Dict.empty }
--         |> (\{ is, verts } -> { vertices = verts, indices = is })
--
--
-- makeMeshVNTHelper :
--     Array Vec3
--     -> Array Vec3
--     -> Array Vec2
--     -> List ( Int3, Int3, Int3 )
--     -> TempMeshWithDict Int3 VertexWithTexture
--     -> TempMeshWithDict Int3 VertexWithTexture
-- makeMeshVNTHelper vs vns vts faces state =
--     case faces of
--         ( ( v1, vt1, vn1 ), ( v2, vt2, vn2 ), ( v3, vt3, vn3 ) ) :: fs ->
--             let
--                 ( newState, newIs, newVerts ) =
--                     getOrUpdateCoordsNT vs vns vts [ ( v1, vt1, vn1 ), ( v2, vt2, vn2 ), ( v3, vt3, vn3 ) ] state
--             in
--                 makeMeshVNTHelper vs
--                     vns
--                     vts
--                     fs
--                     { newState
--                         | verts = state.verts ++ newVerts
--                         , is = newIs :: state.is
--                     }
--
--         [] ->
--             state
--
--
-- makeMeshVT :
--     Array Vec3
--     -> Array Vec2
--     -> List ( Int2, Int2, Int2 )
--     -> MeshWith VertexWithTexture
-- makeMeshVT vs vts fs =
--     makeMeshVTHelper vs vts fs { verts = [], is = [], i = 0, knowns = Dict.empty }
--         |> (\{ is, verts } -> { vertices = verts, indices = is })
--
--
-- type alias TempMeshVT =
--     { i : Int
--     , is : List Int3
--     , verts : List VertexWithTexture
--     , knowns : Dict Int2 ( Int, { coord : Vec2, pos : Vec3 } )
--     }
--
--
-- makeMeshVTHelper :
--     Array Vec3
--     -> Array Vec2
--     -> List ( Int2, Int2, Int2 )
--     -> TempMeshVT
--     -> TempMeshVT
-- makeMeshVTHelper vs vts faces state =
--     case faces of
--         ( ( v1, vt1 ), ( v2, vt2 ), ( v3, vt3 ) ) :: fs ->
--             let
--                 ( newState, newIs, newVerts ) =
--                     getOrUpdateCoordsT vs vts [ ( v1, vt1 ), ( v2, vt2 ), ( v3, vt3 ) ] state
--             in
--                 case ( Array.get (v1 - 1) vs, Array.get (v2 - 1) vs, Array.get (v3 - 1) vs ) of
--                     ( Just a, Just b, Just c ) ->
--                         makeMeshVTHelper vs
--                             vts
--                             fs
--                             { newState
--                                 | verts =
--                                     state.verts
--                                         ++ (List.map
--                                                 (\{ pos, coord } ->
--                                                     { pos = pos, coord = coord, norm = getNormalTo ( c, b, a ) }
--                                                 )
--                                                 newVerts
--                                            )
--                                 , is = newIs :: state.is
--                             }
--
--                     _ ->
--                         Debug.crash "Index out of bound"
--
--         [] ->
--             state
--
--
--
--
-- getOrUpdateCoordsVN vs vns coords state =
--     getOrUpdateCoordsHelperVN vs vns coords ( state, [], [] )
--         |> (\( s, is, verts ) ->
--                 case is of
--                     [ a, b, c ] ->
--                         ( s, ( a, b, c ), verts )
--
--                     _ ->
--                         Debug.crash "This can't happen"
--            )
--
--
-- getOrUpdateCoordsNT vs vns vts coords state =
--     getOrUpdateCoordsNTHelper vs vns vts coords ( state, [], [] )
--         |> (\( s, is, verts ) ->
--                 case is of
--                     [ a, b, c ] ->
--                         ( s, ( a, b, c ), verts )
--
--                     _ ->
--                         Debug.crash "This can't happen"
--            )
--
--
-- getOrUpdateCoordsT vs vts coords state =
--     getOrUpdateCoordsTHelper vs vts coords ( state, [], [] )
--         |> (\( s, is, verts ) ->
--                 case is of
--                     [ a, b, c ] ->
--                         ( s, ( a, b, c ), verts )
--
--                     _ ->
--                         Debug.crash "This can't happen"
--            )
--
--
-- getOrUpdateCoordsNTHelper vs vns vts coords ( state, is, verts ) =
--     case coords of
--         [] ->
--             ( state, is, verts )
--
--         ( c, ct, cn ) :: cs ->
--             case Dict.get ( c, cn, ct ) state.knowns of
--                 Just ( i, v ) ->
--                     getOrUpdateCoordsNTHelper vs vns vts cs ( state, i :: is, verts )
--
--                 Nothing ->
--                     case ( Array.get (c - 1) vs, Array.get (cn - 1) vns, Array.get (ct - 1) vts ) of
--                         ( Just v, Just n, Just t ) ->
--                             getOrUpdateCoordsNTHelper vs
--                                 vns
--                                 vts
--                                 cs
--                                 ( { state
--                                     | knowns = Dict.insert ( c, cn, ct ) ( state.i, { pos = v, norm = n, coord = t } ) state.knowns
--                                     , i = state.i + 1
--                                   }
--                                 , state.i :: is
--                                 , verts ++ [ { pos = v, norm = n, coord = t } ]
--                                 )
--
--                         _ ->
--                             Debug.crash ("Index out of bounds!\n\n" ++ toString (Array.length vns))
--
--
-- getOrUpdateCoordsTHelper vs vts coords ( state, is, verts ) =
--     case coords of
--         [] ->
--             ( state, is, verts )
--
--         ( c, ct ) :: cs ->
--             case Dict.get ( c, ct ) state.knowns of
--                 Just ( i, v ) ->
--                     getOrUpdateCoordsTHelper vs vts cs ( state, i :: is, verts )
--
--                 Nothing ->
--                     case ( Array.get (c - 1) vs, Array.get (ct - 1) vts ) of
--                         ( Just v, Just t ) ->
--                             getOrUpdateCoordsTHelper vs
--                                 vts
--                                 cs
--                                 ( { state
--                                     | knowns = Dict.insert ( c, ct ) ( state.i, { pos = v, coord = t } ) state.knowns
--                                     , i = state.i + 1
--                                   }
--                                 , state.i :: is
--                                 , verts ++ [ { pos = v, coord = t } ]
--                                 )
--
--                         _ ->
--                             Debug.crash "Index out of bounds!"
--
--
-- getOrUpdateCoordsHelperVN vs vns coords ( state, is, verts ) =
--     case coords of
--         [] ->
--             ( state, is, verts )
--
--         ( c, cn ) :: cs ->
--             case Dict.get ( c, cn ) state.knowns of
--                 Just ( i, v ) ->
--                     getOrUpdateCoordsHelperVN vs vns cs ( state, i :: is, verts )
--
--                 Nothing ->
--                     case ( Array.get (c - 1) vs, Array.get (cn - 1) vns ) of
--                         ( Just v, Just n ) ->
--                             getOrUpdateCoordsHelperVN vs
--                                 vns
--                                 cs
--                                 ( { state
--                                     | knowns = Dict.insert ( c, cn ) ( state.i, { pos = v, norm = n } ) state.knowns
--                                     , i = state.i + 1
--                                   }
--                                 , state.i :: is
--                                 , { pos = v, norm = n } :: verts
--                                 )
--
--                         _ ->
--                             Debug.crash "Index out of bounds!"
