module OBJ.Parser exposing (..)

import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Combine exposing (..)
import Combine.Num exposing (..)
import Combine.Char exposing (..)
import OBJ.Types exposing (..)


-- The specs:
--  http://www.martinreddy.net/gfx/3d/OBJ.spec


parse : String -> Result String (List Line)
parse input =
    case
        runParser file () input
    of
        Ok ( state, stream, res ) ->
            Ok res

        Err ( state, stream, errors ) ->
            Err (formatError errors stream)


file : Parser s (List Line)
file =
    (many ignoredLines)
        *> sepBy (many1 ignoredLines)
            line
        <* (many ignoredLines)
        <* end


ignoredLines : Parser s ()
ignoredLines =
    (skip eol) <|> (skip comment)


objectName : Parser s String
objectName =
    regex "o[ \t]+" *> regex ".+"


mtllib : Parser s String
mtllib =
    regex "mtllib[ \t]+" *> regex ".+"


group : Parser s String
group =
    (regex "g[ \t]+" *> regex ".+")
        <|> (char 'g' *> succeed "")


smooth : Parser s String
smooth =
    regex "s[ \t]+" *> regex ".+"


usemtl : Parser s String
usemtl =
    regex "usemtl[ \t]+" *> regex ".+"


line : Parser s Line
line =
    choice
        [ V <$> vertex
        , Vt <$> vertexTexture
        , Vn <$> vertexNormal
        , F <$> face
        , Object <$> objectName
        , Group <$> group
        , Smooth <$> smooth
        , UseMtl <$> usemtl
        , MtlLib <$> mtllib
        ]
        <* regex "[ \t]*"


face : Parser s Face
face =
    regex "f[ \t]+"
        *> choice
            [ fVertex
            , fVertexTexture
            , fVertexTextureNormal
            , fVertexNormal
            ]


fVertex : Parser s Face
fVertex =
    FVertex <$> threeOrFourValues int



-- (FVertex4 <$> (fourValues (,,,) int))
--     <|> (FVertex <$> (threeValues (,,) int))


fVertexTexture : Parser s Face
fVertexTexture =
    FVertexTexture <$> threeOrFourValues int_int



-- (FVertexTexture4 <$> (fourValues (,,,) int_int))
--     <|> (FVertexTexture <$> (threeValues (,,) int_int))


fVertexTextureNormal : Parser s Face
fVertexTextureNormal =
    FVertexTextureNormal <$> threeOrFourValues int_int_int



-- (FVertexTextureNormal4 <$> (fourValues (,,,) int_int_int))
--     <|> (FVertexTextureNormal <$> (threeValues (,,) int_int_int))


fVertexNormal : Parser s Face
fVertexNormal =
    FVertexNormal <$> threeOrFourValues int__int



-- (FVertexNormal4 <$> (fourValues (,,,) int__int))
--     <|> (FVertexNormal <$> (threeValues (,,) int__int))


threeValues : (a -> a -> a -> b) -> Parser s a -> Parser s b
threeValues tagger vtype =
    tagger <$> (vtype) <*> (spaces *> vtype) <*> (spaces *> vtype)


fourValues : (a -> a -> a -> a -> b) -> Parser s a -> Parser s b
fourValues tagger vtype =
    tagger <$> (vtype) <*> (spaces *> vtype) <*> (spaces *> vtype) <*> (spaces *> vtype)


threeOrFourValues : Parser s a -> Parser s (ThreeOrFour a)
threeOrFourValues elements =
    (Four <$> (fourValues (,,,) elements))
        <|> (Three <$> (threeValues (,,) elements))


int_int : Parser s ( Int, Int )
int_int =
    (,) <$> int <*> (string "/" *> int)


int_int_int : Parser s ( Int, Int, Int )
int_int_int =
    (,,) <$> int <*> (string "/" *> int) <*> (string "/" *> int)


int__int : Parser s ( Int, Int )
int__int =
    (,) <$> int <*> (string "//" *> int)


vertexNormal : Parser s Vec3
vertexNormal =
    regex "vn[ \t]+" *> (V3.normalize <$> vector3)


vertexTexture : Parser s Vec2
vertexTexture =
    regex "vt[ \t]+" *> ((ignoreZ <$> vector3) <|> vector2)


vertex : Parser s Vec3
vertex =
    regex "v[ \t]+" *> vector3


comment : Parser s String
comment =
    regex "#" *> regex ".*"


vector3 : Parser s Vec3
vector3 =
    threeValues vec3 betterFloat


spaces : Parser s String
spaces =
    regex "[ \t]+"


vector2 : Parser s Vec2
vector2 =
    vec2 <$> betterFloat <*> (spaces *> betterFloat)


floatOrInt : Parser s Float
floatOrInt =
    float <|> (toFloat <$> (intWithZero <|> int))


intWithZero : Parser s Int
intWithZero =
    (*)
        <$> sign
        <*> (toInt <$> regex "[0-9]*")
        <?> "expected an integer with leading zeros"


betterFloat : Parser s Float
betterFloat =
    floatWithE <|> floatOrInt


floatWithE : Parser s Float
floatWithE =
    (\base exponent -> base * 10 ^ exponent) <$> floatOrInt <*> (string "e" *> floatOrInt)


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
            ++ toString location.line
            ++ separator
            ++ location.source
            ++ "\n"
            ++ String.padLeft padding ' ' "^"
            ++ "\nI expected one of the following:\n"
            ++ expectationSeparator
            ++ String.join expectationSeparator ms


toInt : String -> Int
toInt s =
    String.toInt s |> Result.withDefault 0


ignoreZ : Vec3 -> Vec2
ignoreZ v =
    let
        ( x, y, _ ) =
            V3.toTuple v
    in
        vec2 x y
