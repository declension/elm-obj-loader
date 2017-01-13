module OBJ exposing (..)

{-|
# .obj file loader

## From URL
All these methods take an URL as the first parameter.

### Single model
Use the methods from here if you know whats in your files
and if they only contain a single object with a single material.
These are just provided for convenience.

@docs loadMeshWithoutTexture, loadMesh, loadMeshWithTangent

### General
Use these methods if you don't know what kind of files you'll get or
if your files contain multiple groups or materials.

@docs loadObjFile, loadObjFileWith, Settings, defaultSettings

## From String
@docs parseObjStringWith
-}

import Dict exposing (Dict)
import Http
import OBJ.Assembler exposing (compile)
import OBJ.Parser exposing (parse)
import OBJ.Types exposing (Mesh, ObjFile)
import Task


--

import OBJ.Assembler exposing (compile)
import OBJ.Parser exposing (parse)
import OBJ.Types exposing (..)


{-|
Load a model that doesn't have texture coordinates.
TODO: needs testing
-}
loadMeshWithoutTexture : String -> (Result String (MeshWith Vertex) -> msg) -> Cmd msg
loadMeshWithoutTexture url msg =
    loadObjFile url
        (\res ->
            case res of
                Ok f ->
                    case (Dict.values f |> List.map Dict.values) of
                        [ [ WithoutTexture m ] ] ->
                            msg (Ok m)

                        _ ->
                            msg (Err "file loaded correctely, but there were more than one model.")

                Err e ->
                    msg (Err e)
        )


{-|
Load a model from an URL, expecting texture coordinates.
-}
loadMesh : String -> (Result String (MeshWith VertexWithTexture) -> msg) -> Cmd msg
loadMesh url msg =
    loadObjFile url
        (\res ->
            case res of
                Ok f ->
                    case (Dict.values f |> List.map Dict.values) of
                        [ [ WithTexture m ] ] ->
                            msg (Ok m)

                        _ ->
                            msg (Err "file loaded correctely, but there were more than one model.")

                Err e ->
                    msg (Err e)
        )


{-|
Load a model with texture coordinate and calculate vertex tangents.
This is needed if you want to do tangent space normal mapping.
-}
loadMeshWithTangent : String -> (Result String (MeshWith VertexWithTextureAndTangent) -> msg) -> Cmd msg
loadMeshWithTangent url msg =
    loadObjFile url
        (\res ->
            case res of
                Ok f ->
                    case (Dict.values f |> List.map Dict.values) of
                        [ [ WithTextureAndTangent m ] ] ->
                            msg (Ok m)

                        _ ->
                            msg (Err "file loaded correctely, but there were more than one model.")

                Err e ->
                    msg (Err e)
        )


{-|
Load a .obj file from an URL

    loadObjFile url ObjFileLoaded
-}
loadObjFile : String -> (Result String ObjFile -> msg) -> Cmd msg
loadObjFile =
    loadObjFileWith defaultSettings


{-|
withTangents : If true, vertex tangents will be calculated for meshes with texture coordinates.
This is needed if you want to do tangent space normal mapping.
-}
type alias Settings =
    { withTangents : Bool }


{-| -}
defaultSettings : Settings
defaultSettings =
    { withTangents = False }


{-| -}
loadObjFileWith : Settings -> String -> (Result String ObjFile -> msg) -> Cmd msg
loadObjFileWith settings url msg =
    Http.toTask (Http.getString url)
        |> Task.andThen
            (\s ->
                parseObjStringWith settings s |> Task.succeed
            )
        |> Task.onError (\e -> Task.succeed (Err ("failed to load: " ++ toString e)))
        |> Task.attempt
            (\r ->
                case r of
                    Ok (Ok m) ->
                        msg (Ok m)

                    Ok (Err e) ->
                        msg (Err e)

                    Err e ->
                        msg (Err e)
            )


{-|
Same as `loadObjFile`, but works on a string.
-}
parseObjStringWith : Settings -> String -> Result String ObjFile
parseObjStringWith config input =
    parse input
        |> Result.map (compile config)
