module Suzanne exposing (..)

import AnimationFrame
import Dict exposing (Dict)
import Html
import Html.Attributes as Attr
import Html.Events exposing (on)
import Http
import Json.Decode as JD
import Math.Matrix4 as M4 exposing (Mat4)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Task
import WebGL as GL
import WebGL.Texture
import WebGL.Options as GL
import WebGL.Settings exposing (cullFace, depth, depthOptions, front)
import OBJ
import OBJ.Types exposing (Mesh(..))


type alias Model =
    { time : Float
    , mesh : Result String (Dict String (Dict String Mesh))
    , zoom : Float
    , reflectionTexture : Result String GL.Texture
    }


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, initCmd )
        , view = view
        , subscriptions = (\model -> AnimationFrame.diffs Tick)
        , update = update
        }


type Msg
    = Tick Float
    | LoadObj (Result String (Dict String (Dict String Mesh)))
    | Zoom Float
    | TextureLoaded (Result String GL.Texture)


initModel : Model
initModel =
    { mesh = Err "loading ...", time = 0, zoom = 10, reflectionTexture = Err "Loading texture..." }


initCmd : Cmd Msg
initCmd =
    Cmd.batch
        [ loadModel modelUrl LoadObj
        , loadTexture "chavant.jpg" TextureLoaded
        ]


modelUrl =
    -- "suzanne.obj"
    -- "testObjs/elmLogoPositionandNormal.obj"
    -- "testObjs/elmLogoPositionOnly.obj"
    -- "testObjs/elmLogoPositionUVandNormal.obj"
    "testObjs/elmLogoPositionUV.obj"


loadTexture : String -> (Result String GL.Texture -> msg) -> Cmd msg
loadTexture url msg =
    WebGL.Texture.load url
        |> Task.attempt
            (\r ->
                case r of
                    Ok t ->
                        msg (Ok t)

                    Err e ->
                        msg (Err ("Failed to load texture: " ++ toString e))
            )


loadModel url msg =
    Http.toTask (Http.getString url)
        |> Task.andThen
            (\s ->
                OBJ.load s
                    |> Task.succeed
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


renderModel : Model -> GL.Texture -> Mesh -> GL.Renderable
renderModel { zoom, time } texture mesh =
    let
        ( camera, view ) =
            getCamera zoom time

        model =
            M4.makeRotate time (vec3 0 1 0)

        modelView =
            M4.mul view model
    in
        case mesh of
            WithTexture { vertices, indices } ->
                GL.renderWith [ depth depthOptions, cullFace front ]
                    -- phongVert
                    reflectionVert
                    -- phongFrag
                    reflectionFrag
                    (GL.indexedTriangles vertices indices)
                    { camera = camera, mvMat = modelView, texture = texture }

            _ ->
                Debug.crash "expected texture"


getCamera : Float -> Float -> ( Mat4, Mat4 )
getCamera zoom t =
    ( (M4.makePerspective 45 1 0.01 10000)
    , (M4.makeLookAt (vec3 (zoom) (zoom / 2) (zoom)) (vec3 0 0 0) (vec3 0 1 0))
    )


onZoom : Html.Attribute Msg
onZoom =
    on "wheel" (JD.map Zoom (JD.field "deltaY" JD.float))


view : Model -> Html.Html Msg
view model =
    --Html.div [] [ Html.text (toString model.mesh) ]
    case model.mesh of
        Ok m ->
            case model.reflectionTexture of
                Ok t ->
                    GL.toHtmlWith [ GL.antialias, GL.depth 1 ]
                        [ onZoom, Attr.width 400, Attr.height 400 ]
                        (Dict.values m
                            |> List.concatMap Dict.values
                            |> List.map (renderModel model t)
                        )

                Err e ->
                    Html.div [] [ Html.text e ]

        Err s ->
            Html.div [] [ Html.text (toString s) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model | time = model.time + dt / 1000 }, Cmd.none )

        Zoom dy ->
            ( { model | zoom = model.zoom + dy / 100 }, Cmd.none )

        LoadObj mesh ->
            ( { model | mesh = mesh }, Cmd.none )

        TextureLoaded t ->
            ( { model | reflectionTexture = t }, Cmd.none )


{-| A pretty standart phong shader.
-}
phongVert =
    [glsl|
attribute vec3 position;
attribute vec3 normal;
uniform mat4 mvMat;
uniform mat3 nMat;
uniform mat4 camera;
varying vec3 vVertex;
varying vec3 vNormal;

void main()
{
    vec4 pos4 = mvMat * vec4(position, 1.0);
    vNormal = vec3(mvMat * vec4(normal, 0.0));

    vVertex = vec3(pos4);
    gl_Position = camera * pos4;
}

|]


phongFrag =
    [glsl|
precision mediump float;

varying vec3 vVertex;
varying vec3 vNormal;
const vec3 lightPos = 10.0*vec3(1.0, 5.0, 2.0);
const vec3 specularColor = vec3(1.0,1.0,1.0);
const vec3 ambientColor = vec3(0.2,0.1,0.1);
const vec3 diffuseColor = vec3(0.8,0.8,1.0);


const float shininessVal = 200.0;
const float Ka=0.6, Kd=0.6, Ks=0.7;

void main()
{
    vec3 fragColor = vec3(0.7, 0.7, 0.7);

    vec3 N = normalize(vNormal);

    vec3 L = normalize(lightPos - vVertex);

    float lambertian = max(dot(N, L), 0.0);

    float specular = 0.0;

    if (lambertian > 0.0) {
        vec3 R = reflect(-L, N);      // Reflected light vector
        vec3 V = normalize(-vVertex); // Vector to viewer

        // Compute the specular term
        float specAngle = max(dot(R, V), 0.0);
        specular = pow(specAngle, shininessVal);
    }
    gl_FragColor =
        vec4(Ka * ambientColor +
             Kd * lambertian * diffuseColor +
             Ks * specular * specularColor, 1.0);
}

|]


{-|
This shader uses Spherical Environment Mapping (SEM).
Here are some relevant links:
    * [very cool demo](https://www.clicktorelease.com/code/spherical-normal-mapping/#)
    * https://www.clicktorelease.com/blog/creating-spherical-environment-mapping-shader
    * http://www.ozone3d.net/tutorials/glsl_texturing_p04.php
-}
reflectionVert =
    [glsl|

attribute vec3 position;
attribute vec3 normal;
uniform mat4 mvMat;
uniform mat4 camera;
varying vec3 vNormal;

void main()
{
    vec4 vertex4 = mvMat * vec4(position, 1.0);
    vNormal = vec3(mvMat * vec4(normal, 0.0));
    vec3 nm_z = normalize(vec3(vertex4));
    vec3 nm_x = cross(nm_z, vec3(0.0, 1.0, 0.0));
    vec3 nm_y = cross(nm_x, nm_z);
    vNormal = vec3(dot(vNormal, nm_x), dot(vNormal, nm_y), dot(vNormal, nm_z));
    gl_Position = camera * vertex4;
}

|]


reflectionFrag =
    [glsl|
precision mediump float;

uniform sampler2D texture;

varying vec3 vNormal;

void main()
{
    vec2 texCoord = vec2(0.5 * vNormal.x + 0.5, - 0.5 * vNormal.y - 0.5);
    vec4 fragColor = texture2D(texture, texCoord);
    fragColor.a = 1.0;

    gl_FragColor = fragColor;
}

|]
