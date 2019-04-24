module ParserTests exposing (suite)

import Combine exposing (ParseError, ParseOk, Parser)
import Combine.Num
import Dict exposing (Dict)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int)
import Math.Vector2 as Vector2 exposing (Vec2)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Math.Vector4 as Vector4 exposing (Vec4)
import OBJ exposing (parseObjStringWith)
import OBJ.InternalTypes exposing (Line(..))
import OBJ.Parser exposing (fourValues, parseLine, threeValues)
import OBJ.Types exposing (Mesh(..), MeshWith, ObjFile, Vertex, VertexWithTexture, VertexWithTextureAndTangent)
import ObjData exposing (basicShapeExpectedOutput, basicShapeWithTextureAndNormals)
import Test exposing (..)


triplify : a -> a -> a -> List a
triplify a b c =
    [ a, b, c ]


quadify a b c d =
    [ a, b, c, d ]


tripleParser : Parser s (List Int)
tripleParser =
    threeValues triplify Combine.Num.int


quadParser : Parser s (List Int)
quadParser =
    fourValues quadify Combine.Num.int


suite : Test
suite =
    describe "OBJ Parser"
        [ fuzz int "can use Combine's int parser to parse a single Int" <|
            \number ->
                String.fromInt number
                    |> Combine.parse Combine.Num.int
                    |> expectParsedValueEquals number
        , test "has a `threeValues` method that can parse three Ints" <|
            \() ->
                Combine.parse tripleParser "1 -2 3"
                    |> expectParsedValueEquals [ 1, -2, 3 ]
        , test "has a `fourValues` method that can parse four Ints" <|
            \() ->
                Combine.parse quadParser "1 2 -3 4"
                    |> expectParsedValueEquals [ 1, 2, -3, 4 ]
        , test "has a line method that can parse a single line" <|
            \() ->
                parseLine "v 0.1 -0.2 0.3"
                    |> equalsOrFail (V (vec3 0.1 -0.2 0.3))
        , test "can parse an empty string" <|
            \() ->
                parseObjStringWith settings "# Comment\n"
                    |> equalsOrFail Dict.empty
        , test "can parse vertices from a real OBJ-formatted string" <|
            \() ->
                parseObjStringWith settings basicShapeWithTextureAndNormals
                    |> Result.andThen getMesh
                    |> Result.andThen getVertices
                    -- Turn our list of vertices into a list of expectations (based on expectations)
                    |> Result.map (List.map2 vertexWithTextureAndTangentsEqual basicShapeExpectedOutput)
                    |> Result.map (List.foldl andAlso Expect.pass)
                    |> failErrorResults
        ]


vertexWithTextureAndTangentsEqual : VertexWithTextureAndTangent -> VertexWithTextureAndTangent -> Expectation
vertexWithTextureAndTangentsEqual expected actual =
    vec3Within expected.normal actual.normal
        |> andAlso (vec3Within expected.position actual.position)
        |> andAlso (vec4Within expected.tangent actual.tangent)
        |> andAlso (vec2Within expected.texCoord actual.texCoord)


nearEnough =
    Expect.within (Absolute 0.0001)


vec2Within : Vec2 -> Vec2 -> Expectation
vec2Within expected actual =
    let
        e =
            Vector2.toRecord expected

        a =
            Vector2.toRecord actual
    in
    nearEnough e.x a.x
        |> andAlso (nearEnough e.y a.y)


vec3Within : Vec3 -> Vec3 -> Expectation
vec3Within expected actual =
    let
        e =
            Vector3.toRecord expected

        a =
            Vector3.toRecord actual
    in
    nearEnough e.x a.x
        |> andAlso (nearEnough e.y a.y)
        |> andAlso (nearEnough e.z a.z)


vec4Within : Vec4 -> Vec4 -> Expectation
vec4Within expected actual =
    let
        e =
            Vector4.toRecord expected

        a =
            Vector4.toRecord actual
    in
    nearEnough e.x a.x
        |> andAlso (nearEnough e.y a.y)
        |> andAlso (nearEnough e.z a.z)
        |> andAlso (nearEnough e.w a.w)



-- See https://github.com/elm-community/elm-test/issues/214


andAlso : Expect.Expectation -> Expect.Expectation -> Expect.Expectation
andAlso l r =
    Expect.all [ always l, always r ] ()


getVertices : Mesh -> Result String (List VertexWithTextureAndTangent)
getVertices mesh =
    case mesh of
        WithoutTexture _ ->
            Err "WithoutTexture found"

        WithTexture _ ->
            Err "WithTexture found"

        WithTextureAndTangent m ->
            Ok m.vertices


getMesh : ObjFile -> Result String Mesh
getMesh obj =
    Dict.get "__default__" obj
        |> Maybe.andThen (Dict.get "__default__")
        |> Maybe.map Ok
        |> Maybe.withDefault (Err "couldn't find __default__")


settings =
    { withTangents = True }


equalsOrFail : a -> Result String a -> Expectation
equalsOrFail expected result =
    case result of
        Err message ->
            Expect.fail <| "FAILED: " ++ message

        Ok data ->
            Expect.equal data expected


failErrorResults : Result String Expectation -> Expectation
failErrorResults result =
    case result of
        Err message ->
            Expect.fail <| "FAILED: " ++ message

        Ok exp ->
            exp


expectParsedValueEquals expected parseResult =
    parseResult
        |> Result.map (\( state, is, res ) -> res)
        |> (\result ->
                case result of
                    Err ( _, _, messages ) ->
                        Expect.fail <| "FAILED: " ++ String.join "\n" messages

                    Ok data ->
                        Expect.equal data expected
           )
