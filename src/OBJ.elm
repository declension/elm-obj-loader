module OBJ exposing (load, loadWith)

import Dict exposing (Dict)


--

import OBJ.Assembler exposing (compile)
import OBJ.Parser exposing (parse)
import OBJ.Types exposing (Mesh)


loadWith config input =
    parse input
        |> Result.map (compile config)


load =
    loadWith { withTangents = False }
