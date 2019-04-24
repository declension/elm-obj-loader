module OBJ.Parser exposing (betterFloat, canSkip, comment, fVertex, fVertexNormal, fVertexTexture, fVertexTextureNormal, face, file, formatError, fourValues, group, ignoreZ, ignoredLines, int__int, int_int, int_int_int, line, mtllib, objectName, parse, parseLine, parseLineAcc, smooth, spaces, threeOrFourValues, threeValues, toInt, usemtl, vector2, vector3, vertex, vertexNormal, vertexTexture)

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Num exposing (..)
import Json.Decode as JD
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 as V3 exposing (Vec3, toRecord, vec3)
import OBJ.InternalTypes exposing (..)
import Regex



-- TODO: figure out how nice error messages work
--
-- The obj specs:
--  http://www.martinreddy.net/gfx/3d/OBJ.spec


parse : String -> Result String (List Line)
parse input =
    String.split "\n" input
        |> List.foldr parseLineAcc (Ok [])


parseLineAcc : String -> Result String (List Line) -> Result String (List Line)
parseLineAcc line_ acc =
    case acc of
        Ok lines ->
            if canSkip line_ then
                Ok lines

            else
                parseLine line_
                    |> Result.andThen
                        (\l ->
                            Ok (l :: lines)
                        )

        Err e ->
            Err e


canSkip : String -> Bool
canSkip =
    regexMatches "^((\\s*)|(\\s*#.*\\r?))$"


regexMatches : String -> String -> Bool
regexMatches regexStr str =
    Regex.fromString regexStr
        |> Maybe.map (\regex -> Regex.contains regex str)
        |> Maybe.withDefault False


parseLine l =
    case Combine.parse line l of
        Ok ( _, stream, result ) ->
            Ok result

        Err ( _, stream, errors ) ->
            Err (formatError errors stream)


file : Parser s (List Line)
file =
    ignore (many ignoredLines) (sepBy (many1 ignoredLines) line)
        |> ignore (many ignoredLines)
        |> ignore end


ignoredLines : Parser s ()
ignoredLines =
    or (skip eol) (skip comment)


objectName : Parser s String
objectName =
    regex "o[ \t]+" |> keep (regex ".+")


mtllib : Parser s String
mtllib =
    regex "mtllib[ \t]+" |> keep (regex ".+")


group : Parser s String
group =
    or (regex "g[ \t]+" |> keep (regex ".+"))
        (char 'g' |> keep (succeed ""))


smooth : Parser s String
smooth =
    regex "s[ \t]+" |> keep (regex ".+")


usemtl : Parser s String
usemtl =
    regex "usemtl[ \t]+" |> keep (regex ".+")


line : Parser s Line
line =
    keep
        (choice
            [ map V vertex
            , map Vt vertexTexture
            , map Vn vertexNormal
            , map F face
            , map Object objectName
            , map Group group
            , map Smooth smooth
            , map UseMtl usemtl
            , map MtlLib mtllib
            ]
        )
        (regex "[ \t]*")


face : Parser s Face
face =
    regex "f[ \t]+"
        |> keep
            (choice
                [ fVertexTextureNormal
                , fVertexNormal
                , fVertex
                , fVertexTexture
                ]
            )


fVertex : Parser s a
fVertex =
    keep (fail "Models with no precalculated vertex normals are not supported!") (threeOrFourValues int)


fVertexTexture : Parser s a
fVertexTexture =
    keep (fail "Models with no precalculated vertex normals are not supported!") (threeOrFourValues int_int)


fVertexTextureNormal : Parser s Face
fVertexTextureNormal =
    map FVertexTextureNormal <| threeOrFourValues int_int_int


fVertexNormal : Parser s Face
fVertexNormal =
    map FVertexNormal <| threeOrFourValues int__int


threeValues : (a -> a -> a -> b) -> Parser s a -> Parser s b
threeValues tagger parser =
    parser
        |> map tagger
        |> andMap (spaces |> keep parser)
        |> andMap (spaces |> keep parser)


fourValues : (a -> a -> a -> a -> b) -> Parser s a -> Parser s b
fourValues tagger parser =
    parser
        |> map tagger
        |> andMap (spaces |> keep parser)
        |> andMap (spaces |> keep parser)
        |> andMap (spaces |> keep parser)


threeOrFourValues : Parser s a -> Parser s (ThreeOrFour a)
threeOrFourValues elementType =
    or (map Four <| fourValues (\a b c d -> { a = a, b = b, c = c, d = d }) elementType)
        (map Three <| threeValues (\a b c -> { a = a, b = b, c = c }) elementType)


int_int : Parser s ( Int, Int )
int_int =
    int
        |> map tuple2
        |> andMap (string "/" |> keep int)


int_int_int : Parser s ( Int, Int, Int )
int_int_int =
    int
        |> map tuple3
        |> andMap (string "/" |> keep int)
        |> andMap (string "/" |> keep int)


tuple2 : a -> b -> ( a, b )
tuple2 a b =
    ( a, b )


tuple3 : a -> b -> c -> ( a, b, c )
tuple3 a b c =
    ( a, b, c )


int__int : Parser s ( Int, Int )
int__int =
    int
        |> map tuple2
        |> andMap (string "//" |> keep int)


vertexNormal : Parser s Vec3
vertexNormal =
    regex "vn[ \t]+" |> keep (map V3.normalize vector3)


vertexTexture : Parser s Vec2
vertexTexture =
    regex "vt[ \t]+" |> keep (or (map ignoreZ vector3) vector2)


vertex : Parser s Vec3
vertex =
    regex "v[ \t]+" |> keep vector3


comment : Parser s String
comment =
    regex "#" |> keep (regex ".*")


vector3 : Parser s Vec3
vector3 =
    threeValues vec3 betterFloat


spaces : Parser s String
spaces =
    regex "[ \t]+"


vector2 : Parser s Vec2
vector2 =
    betterFloat |> map vec2 |> andMap (spaces |> keep betterFloat)


betterFloat : Parser s Float
betterFloat =
    map parseFloatOrZero <| regex "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"


parseFloatOrZero s =
    Result.withDefault 0 (JD.decodeString JD.float s)


formatError : List String -> InputStream -> String
formatError ms stream =
    let
        location =
            currentLocation stream

        separator =
            "| "

        expectationSeparator =
            "\n  * "

        lineNumberOffset =
            floor (logBase 10 (toFloat location.line)) + 1

        separatorOffset =
            String.length separator

        padding =
            location.column + separatorOffset + 2
    in
    "Parse error around line:\n\n"
        ++ String.fromInt location.line
        ++ separator
        ++ location.source
        ++ "\n"
        ++ String.padLeft padding ' ' "^"
        ++ "\nI expected one of the following:\n"
        ++ expectationSeparator
        ++ String.join expectationSeparator ms


toInt : String -> Int
toInt s =
    Maybe.withDefault 0 <| String.toInt s


ignoreZ : Vec3 -> Vec2
ignoreZ v =
    let
        { x, y, z } =
            toRecord v
    in
    vec2 x y
