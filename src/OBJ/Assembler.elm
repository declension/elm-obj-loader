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


type alias CompileState =
    { currentGroup : Maybe Group
    , currentGroupName : String
    , groups : Dict.Dict String Group
    , mtlLib : Maybe String
    , objectName : Maybe String
    , vns : Array Vec3
    , vs : Array Vec3
    , vts : Array Vec2
    }


compile : List Line -> Dict.Dict String Mesh
compile lines =
    compileHelper emptyCompileState lines
        |> addCurrentGroup
        |> toMeshes


emptyCompileState : CompileState
emptyCompileState =
    { groups = Dict.empty
    , currentGroupName = "default"
    , objectName = Nothing
    , mtlLib = Nothing
    , currentGroup = Nothing
    , vs = Array.empty
    , vts = Array.empty
    , vns = Array.empty
    }


log s a =
    let
        _ =
            Debug.log s ()
    in
        a


toMeshes : CompileState -> Dict String Mesh
toMeshes state =
    Dict.map
        (\groupName group ->
            case group of
                GV { faces } ->
                    WithoutTexture (makeMeshV state.vs faces)
                        |> log "GV"

                GVT { faces } ->
                    WithTexture (makeMeshVT state.vs state.vts faces)
                        |> log "GVT"

                GVN { faces } ->
                    WithoutTexture (makeMeshVN state.vs state.vns faces)
                        |> log "GVN"

                GVTN { faces } ->
                    WithTexture (makeMeshVNT state.vs state.vns state.vts faces)
                        |> log "GVTN"
        )
        state.groups


makeMeshV : Array Vec3 -> List Int3 -> MeshWith Vertex
makeMeshV vs fs =
    makeMeshVHelper vs fs { verts = [], is = [], i = 0 }
        |> (\{ is, verts } -> { vertices = verts, indices = is })


type alias TempMesh a =
    { i : Int
    , is : List Int3
    , verts : List a
    }


makeMeshVHelper : Array Vec3 -> List Int3 -> TempMesh Vertex -> TempMesh Vertex
makeMeshVHelper vs faces state =
    case faces of
        ( v1, v2, v3 ) :: fs ->
            case ( Array.get (v1 - 1) vs, Array.get (v2 - 1) vs, Array.get (v3 - 1) vs ) of
                ( Just a, Just b, Just c ) ->
                    let
                        norm =
                            getNormalTo ( c, b, a )
                    in
                        makeMeshVHelper vs
                            fs
                            { state
                                | verts = state.verts ++ [ { pos = a, norm = norm }, { pos = b, norm = norm }, { pos = c, norm = norm } ]
                                , is = ( state.i + 2, state.i + 1, state.i ) :: state.is
                                , i = state.i + 3
                            }

                _ ->
                    Debug.crash
                        ("Index out of bound: "
                            ++ toString ( v1, v2, v3 )
                            ++ "\n\n"
                            ++ "vs: "
                            ++ toString (Array.length vs)
                        )

        [] ->
            state


makeMeshVN : Array Vec3 -> Array Vec3 -> List ( Int2, Int2, Int2 ) -> MeshWith Vertex
makeMeshVN vs vns fs =
    makeMeshVNHelper vs vns fs { verts = [], is = [], i = 0, knowns = Dict.empty }
        |> (\{ is, verts } -> { vertices = verts, indices = is })


type alias TempMeshWithDict i a =
    { i : Int
    , is : List Int3
    , verts : List a
    , knowns : Dict i ( Int, a )
    }


makeMeshVNHelper :
    Array Vec3
    -> Array Vec3
    -> List ( Int2, Int2, Int2 )
    -> TempMeshWithDict Int2 Vertex
    -> TempMeshWithDict Int2 Vertex
makeMeshVNHelper vs vns faces state =
    case faces of
        ( ( v1, vn1 ), ( v2, vn2 ), ( v3, vn3 ) ) :: fs ->
            let
                ( newState, newIs, newVerts ) =
                    getOrUpdateCoordsVN vs vns [ ( v1, vn1 ), ( v2, vn2 ), ( v3, vn3 ) ] state
            in
                makeMeshVNHelper vs
                    vns
                    fs
                    { newState
                        | verts = state.verts ++ newVerts
                        , is = newIs :: state.is
                    }

        [] ->
            state


makeMeshVNT :
    Array Vec3
    -> Array Vec3
    -> Array Vec2
    -> List ( Int3, Int3, Int3 )
    -> MeshWith VertexWithTexture
makeMeshVNT vs vns vts fs =
    makeMeshVNTHelper vs vns vts fs { verts = [], is = [], i = 0, knowns = Dict.empty }
        |> (\{ is, verts } -> { vertices = verts, indices = is })


makeMeshVNTHelper :
    Array Vec3
    -> Array Vec3
    -> Array Vec2
    -> List ( Int3, Int3, Int3 )
    -> TempMeshWithDict Int3 VertexWithTexture
    -> TempMeshWithDict Int3 VertexWithTexture
makeMeshVNTHelper vs vns vts faces state =
    case faces of
        ( ( v1, vt1, vn1 ), ( v2, vt2, vn2 ), ( v3, vt3, vn3 ) ) :: fs ->
            let
                ( newState, newIs, newVerts ) =
                    getOrUpdateCoordsNT vs vns vts [ ( v1, vt1, vn1 ), ( v2, vt2, vn2 ), ( v3, vt3, vn3 ) ] state
            in
                makeMeshVNTHelper vs
                    vns
                    vts
                    fs
                    { newState
                        | verts = state.verts ++ newVerts
                        , is = newIs :: state.is
                    }

        [] ->
            state


makeMeshVT :
    Array Vec3
    -> Array Vec2
    -> List ( Int2, Int2, Int2 )
    -> MeshWith VertexWithTexture
makeMeshVT vs vts fs =
    makeMeshVTHelper vs vts fs { verts = [], is = [], i = 0, knowns = Dict.empty }
        |> (\{ is, verts } -> { vertices = verts, indices = is })


type alias TempMeshVT =
    { i : Int
    , is : List Int3
    , verts : List VertexWithTexture
    , knowns : Dict Int2 ( Int, { coord : Vec2, pos : Vec3 } )
    }


makeMeshVTHelper :
    Array Vec3
    -> Array Vec2
    -> List ( Int2, Int2, Int2 )
    -> TempMeshVT
    -> TempMeshVT
makeMeshVTHelper vs vts faces state =
    case faces of
        ( ( v1, vt1 ), ( v2, vt2 ), ( v3, vt3 ) ) :: fs ->
            let
                ( newState, newIs, newVerts ) =
                    getOrUpdateCoordsT vs vts [ ( v1, vt1 ), ( v2, vt2 ), ( v3, vt3 ) ] state
            in
                case ( Array.get (v1 - 1) vs, Array.get (v2 - 1) vs, Array.get (v3 - 1) vs ) of
                    ( Just a, Just b, Just c ) ->
                        makeMeshVTHelper vs
                            vts
                            fs
                            { newState
                                | verts =
                                    state.verts
                                        ++ (List.map
                                                (\{ pos, coord } ->
                                                    { pos = pos, coord = coord, norm = getNormalTo ( c, b, a ) }
                                                )
                                                newVerts
                                           )
                                , is = newIs :: state.is
                            }

                    _ ->
                        Debug.crash "Index out of bound"

        [] ->
            state


get3 : ( Int, Int, Int ) -> Array a -> Maybe ( a, a, a )
get3 ( a, b, c ) array =
    case ( Array.get a array, Array.get b array, Array.get c array ) of
        ( Just a, Just b, Just c ) ->
            Just ( a, b, c )

        _ ->
            Nothing


getOrUpdateCoordsVN vs vns coords state =
    getOrUpdateCoordsHelperVN vs vns coords ( state, [], [] )
        |> (\( s, is, verts ) ->
                case is of
                    [ a, b, c ] ->
                        ( s, ( a, b, c ), verts )

                    _ ->
                        Debug.crash "This can't happen"
           )


getOrUpdateCoordsNT vs vns vts coords state =
    getOrUpdateCoordsNTHelper vs vns vts coords ( state, [], [] )
        |> (\( s, is, verts ) ->
                case is of
                    [ a, b, c ] ->
                        ( s, ( a, b, c ), verts )

                    _ ->
                        Debug.crash "This can't happen"
           )


getOrUpdateCoordsT vs vts coords state =
    getOrUpdateCoordsTHelper vs vts coords ( state, [], [] )
        |> (\( s, is, verts ) ->
                case is of
                    [ a, b, c ] ->
                        ( s, ( a, b, c ), verts )

                    _ ->
                        Debug.crash "This can't happen"
           )


getOrUpdateCoordsNTHelper vs vns vts coords ( state, is, verts ) =
    case coords of
        [] ->
            ( state, is, verts )

        ( c, ct, cn ) :: cs ->
            case Dict.get ( c, cn, ct ) state.knowns of
                Just ( i, v ) ->
                    getOrUpdateCoordsNTHelper vs vns vts cs ( state, i :: is, verts )

                Nothing ->
                    case ( Array.get (c - 1) vs, Array.get (cn - 1) vns, Array.get (ct - 1) vts ) of
                        ( Just v, Just n, Just t ) ->
                            getOrUpdateCoordsNTHelper vs
                                vns
                                vts
                                cs
                                ( { state
                                    | knowns = Dict.insert ( c, cn, ct ) ( state.i, { pos = v, norm = n, coord = t } ) state.knowns
                                    , i = state.i + 1
                                  }
                                , state.i :: is
                                , verts ++ [ { pos = v, norm = n, coord = t } ]
                                )

                        _ ->
                            Debug.crash ("Index out of bounds!\n\n" ++ toString (Array.length vns))


getOrUpdateCoordsTHelper vs vts coords ( state, is, verts ) =
    case coords of
        [] ->
            ( state, is, verts )

        ( c, ct ) :: cs ->
            case Dict.get ( c, ct ) state.knowns of
                Just ( i, v ) ->
                    getOrUpdateCoordsTHelper vs vts cs ( state, i :: is, verts )

                Nothing ->
                    case ( Array.get (c - 1) vs, Array.get (ct - 1) vts ) of
                        ( Just v, Just t ) ->
                            getOrUpdateCoordsTHelper vs
                                vts
                                cs
                                ( { state
                                    | knowns = Dict.insert ( c, ct ) ( state.i, { pos = v, coord = t } ) state.knowns
                                    , i = state.i + 1
                                  }
                                , state.i :: is
                                , verts ++ [ { pos = v, coord = t } ]
                                )

                        _ ->
                            Debug.crash "Index out of bounds!"


getOrUpdateCoordsHelperVN vs vns coords ( state, is, verts ) =
    case coords of
        [] ->
            ( state, is, verts )

        ( c, cn ) :: cs ->
            case Dict.get ( c, cn ) state.knowns of
                Just ( i, v ) ->
                    getOrUpdateCoordsHelperVN vs vns cs ( state, i :: is, verts )

                Nothing ->
                    case ( Array.get (c - 1) vs, Array.get (cn - 1) vns ) of
                        ( Just v, Just n ) ->
                            getOrUpdateCoordsHelperVN vs
                                vns
                                cs
                                ( { state
                                    | knowns = Dict.insert ( c, cn ) ( state.i, { pos = v, norm = n } ) state.knowns
                                    , i = state.i + 1
                                  }
                                , state.i :: is
                                , { pos = v, norm = n } :: verts
                                )

                        _ ->
                            Debug.crash "Index out of bounds!"


{-| (v1, v2, v3) are assumed to be given in counter clock direction
-}
getNormalTo : ( Vec3, Vec3, Vec3 ) -> Vec3
getNormalTo ( v1, v2, v3 ) =
    V3.normalize (V3.cross (V3.sub v1 v2) (V3.sub v3 v2))


addCurrentGroup : CompileState -> CompileState
addCurrentGroup state =
    let
        _ =
            Debug.log "(vs, vns, vts)" ( Array.length state.vs, Array.length state.vns, Array.length state.vts )
    in
        case state.currentGroup of
            Just g ->
                { state
                    | groups = Dict.insert state.currentGroupName g state.groups
                    , currentGroup = Nothing
                }

            Nothing ->
                state


compileHelper : CompileState -> List Line -> CompileState
compileHelper state lines =
    case lines of
        [] ->
            state

        l :: ls ->
            compileHelper (insertLine l state) ls


insertLine : Line -> CompileState -> CompileState
insertLine line state =
    case line of
        Object s ->
            { state | objectName = Just s }

        MtlLib s ->
            { state | mtlLib = Just s }

        Group s ->
            addCurrentGroup state
                |> (\st -> { st | currentGroupName = s })

        Smooth s ->
            -- is currently ignored
            state

        UseMtl s ->
            -- currently ignored
            state

        V v ->
            { state | vs = Array.push v state.vs }

        Vt v ->
            { state | vts = Array.push v state.vts }

        Vn v ->
            { state | vns = Array.push v state.vns }

        F f ->
            addFace f state


{-| This function adds a face to the current faces list.
    Since there are 4*2 different face types and 4 + 1 different Mesh types,
    this function has 4*2*2 branches.
    (The meshes only add two different branches per face type)
-}
addFace : Face -> { a | currentGroup : Maybe Group } -> { a | currentGroup : Maybe Group }
addFace f state =
    case ( state.currentGroup, f ) of
        ( Just (GV ({ faces } as g)), FVertex v ) ->
            { state | currentGroup = Just (GV { g | faces = v :: faces }) }

        ( Just (GV ({ faces } as g)), FVertex4 ( a, b, c, d ) ) ->
            { state | currentGroup = Just (GV { g | faces = ( a, b, c ) :: ( c, d, a ) :: faces }) }

        ( Nothing, FVertex v ) ->
            { state | currentGroup = Just (GV { faces = [ v ] }) }

        ( Nothing, FVertex4 ( a, b, c, d ) ) ->
            { state | currentGroup = Just (GV { faces = [ ( a, b, c ), ( c, d, a ) ] }) }

        ( Just (GVN ({ faces } as g)), FVertexNormal v ) ->
            { state | currentGroup = Just (GVN { g | faces = v :: faces }) }

        ( Just (GVN ({ faces } as g)), FVertexNormal4 ( a, b, c, d ) ) ->
            { state | currentGroup = Just (GVN { g | faces = ( a, b, c ) :: ( c, d, a ) :: faces }) }

        ( Nothing, FVertexNormal v ) ->
            { state | currentGroup = Just (GVN { faces = [ v ] }) }

        ( Nothing, FVertexNormal4 ( a, b, c, d ) ) ->
            { state | currentGroup = Just (GVN { faces = [ ( a, b, c ), ( c, d, a ) ] }) }

        ( Just (GVT ({ faces } as g)), FVertexTexture v ) ->
            { state | currentGroup = Just (GVT { g | faces = v :: faces }) }

        ( Just (GVT ({ faces } as g)), FVertexTexture4 ( a, b, c, d ) ) ->
            { state | currentGroup = Just (GVT { g | faces = ( a, b, c ) :: ( c, d, a ) :: faces }) }

        ( Nothing, FVertexTexture v ) ->
            { state | currentGroup = Just (GVT { faces = [ v ] }) }

        ( Nothing, FVertexTexture4 ( a, b, c, d ) ) ->
            { state | currentGroup = Just (GVT { faces = [ ( a, b, c ), ( c, d, a ) ] }) }

        ( Just (GVTN ({ faces } as g)), FVertexTextureNormal v ) ->
            { state | currentGroup = Just (GVTN { g | faces = v :: faces }) }

        ( Just (GVTN ({ faces } as g)), FVertexTextureNormal4 ( a, b, c, d ) ) ->
            { state | currentGroup = Just (GVTN { g | faces = ( a, b, c ) :: ( c, d, a ) :: faces }) }

        ( Nothing, FVertexTextureNormal v ) ->
            { state | currentGroup = Just (GVTN { faces = [ v ] }) }

        ( Nothing, FVertexTextureNormal4 ( a, b, c, d ) ) ->
            { state | currentGroup = Just (GVTN { faces = [ ( a, b, c ), ( c, d, a ) ] }) }

        ( mesh, face ) ->
            Debug.crash
                ("Faces should all be the same type!\nMesh, face pair:\n\n"
                    ++ toString ( state.currentGroup, face )
                )
