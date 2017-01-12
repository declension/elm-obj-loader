module OBJ exposing (parseObjString, parseObjStringWith, loadObjFileWith, Settings)

import Dict exposing (Dict)


--

import OBJ.Assembler exposing (compile)
import OBJ.Parser exposing (parse)
import OBJ.Types exposing (Mesh)
import Task
import Http


type alias Settings =
    { withTangents : Bool }


parseObjStringWith config input =
    parse input
        |> Result.map (compile config)


parseObjString =
    parseObjStringWith { withTangents = False }


loadObjFileWith : Settings -> String -> (String -> Result String (Dict String (Dict String Mesh)) -> msg) -> Cmd msg
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
                        msg url (Ok m)

                    Ok (Err e) ->
                        msg url (Err e)

                    Err e ->
                        msg url (Err e)
            )
