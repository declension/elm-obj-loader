module OBJ exposing (load)

import Dict exposing (Dict)


--

import OBJ.Assembler exposing (compile)
import OBJ.Parser exposing (parse)
import OBJ.Types exposing (Mesh)


load : String -> Result String (Dict String Mesh)
load input =
    parse input
        |> Result.map compile
