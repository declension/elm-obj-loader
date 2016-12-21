module ElmLogo exposing (..)

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
import Mouse
import Window


type alias Model =
    { time : Float
    , mesh : Result String (Dict String Mesh)
    , zoom : Float
    , diffText : Result String GL.Texture
    , normText : Result String GL.Texture
    , isDown : Bool
    , lastMousePos : Mouse.Position
    , mouseDelta : MouseDelta
    , windowSize : Window.Size
    }


type alias MouseDelta =
    { x : Float, y : Float }


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, initCmd )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ((if model.isDown then
            [ Mouse.moves MouseMove ]
          else
            []
         )
            ++ [ AnimationFrame.diffs Tick
               , Mouse.downs MouseDown
               , Mouse.ups (\_ -> MouseUp)
               , Window.resizes ResizeWindow
               ]
        )


type Msg
    = Tick Float
    | LoadObj (Result String (Dict String Mesh))
    | Zoom Float
    | MouseMove Mouse.Position
    | MouseDown Mouse.Position
    | MouseUp
    | DiffTextureLoaded (Result String GL.Texture)
    | NormTextureLoaded (Result String GL.Texture)
    | ResizeWindow Window.Size


initModel : Model
initModel =
    { mesh = Err "loading ..."
    , time = 0
    , zoom = 5
    , diffText = Err "Loading texture..."
    , normText = Err "Loading texture..."
    , isDown = False
    , lastMousePos = Mouse.Position 0 0
    , mouseDelta = MouseDelta 0 (pi / 2)
    , windowSize = Window.Size 800 600
    }


initCmd : Cmd Msg
initCmd =
    Cmd.batch
        [ loadModel "elmLogo.obj" LoadObj
        , loadTexture "elmLogoDiffuse.png" DiffTextureLoaded
        , loadTexture "elmLogoNorm.png" NormTextureLoaded
        , Task.perform ResizeWindow Window.size
        ]


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


loadModel : String -> (Result String (Dict String Mesh) -> msg) -> Cmd msg
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


renderModel : Model -> GL.Texture -> GL.Texture -> Mesh -> GL.Renderable
renderModel model textureDiff textureNorm mesh =
    let
        ( camera, view ) =
            getCamera model

        modelM =
            M4.makeRotate 0.2 (vec3 0 1 0)
                |> M4.translate (vec3 -1 0 0)

        modelView =
            M4.mul view modelM
    in
        case mesh of
            WithTexture { vertices, indices } ->
                GL.renderWith [ depth depthOptions, cullFace front ]
                    vert
                    frag
                    (GL.indexedTriangles vertices indices)
                    { camera = camera
                    , mvMat = modelView
                    , textureDiff = textureDiff
                    , textureNorm = textureNorm
                    , time = model.time
                    }

            _ ->
                Debug.crash "I was expecting a model with textures!"


getCamera : Model -> ( Mat4, Mat4 )
getCamera { mouseDelta, zoom, windowSize } =
    let
        ( mx, my ) =
            ( mouseDelta.x, mouseDelta.y )

        aspect =
            toFloat windowSize.width / toFloat windowSize.height
    in
        ( (M4.makePerspective 45 (aspect) 0.01 10000)
        , (M4.makeLookAt (vec3 (zoom * sin -mx * sin my) (-zoom * cos my) (zoom * cos -mx * sin my)) (vec3 0 1 0) (vec3 0 1 0))
        )


onZoom : Html.Attribute Msg
onZoom =
    on "wheel" (JD.map Zoom (JD.field "deltaY" JD.float))


view : Model -> Html.Html Msg
view model =
    --Html.div [] [ Html.text (toString model.mesh) ]
    case ( model.mesh, model.diffText, model.normText ) of
        ( Ok m, Ok td, Ok tn ) ->
            GL.toHtmlWith [ GL.antialias, GL.depth 1 ]
                [ onZoom, Attr.width (model.windowSize.width - 10), Attr.height (model.windowSize.height - 10) ]
                (Dict.values m
                    |> List.map (renderModel model td tn)
                )

        err ->
            Html.div [] [ Html.text (toString err) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model | time = model.time + dt / 1000 }, Cmd.none )

        Zoom dy ->
            ( { model | zoom = max 0.01 (model.zoom + dy / 100) }, Cmd.none )

        LoadObj mesh ->
            ( { model | mesh = mesh }, Cmd.none )

        DiffTextureLoaded t ->
            ( { model | diffText = t }, Cmd.none )

        NormTextureLoaded t ->
            ( { model | normText = t }, Cmd.none )

        MouseDown p ->
            ( { model | isDown = True, lastMousePos = p }, Cmd.none )

        MouseUp ->
            ( { model | isDown = False }, Cmd.none )

        MouseMove p ->
            ( { model | mouseDelta = getDelta p model.lastMousePos model.mouseDelta, lastMousePos = p }, Cmd.none )

        ResizeWindow w ->
            ( { model | windowSize = w }, Cmd.none )


getDelta : Mouse.Position -> Mouse.Position -> MouseDelta -> MouseDelta
getDelta curr lastP delta =
    MouseDelta (toFloat (curr.x - lastP.x) / 100 + delta.x) (clamp 0.01 pi (toFloat (curr.y - lastP.y) / 100 + delta.y))


{-| A pretty standart phong shader.
-}
vert =
    [glsl|
attribute vec3 pos;
attribute vec3 norm;
attribute vec2 coord;

uniform mat4 mvMat;
uniform mat3 nMat;
uniform mat4 camera;
varying vec3 vVertex;
varying vec3 vNormal;
varying vec2 vCoord;


void main()
{
    vec4 pos4 = mvMat * vec4(pos, 1.0);
    vNormal = vec3(mvMat * vec4(norm, 0.0));
    vCoord = coord;
    vVertex = vec3(pos4);
    gl_Position = camera * pos4;
}

|]


{-|
TODO: this shader is very wrong
-}
frag =
    [glsl|
precision mediump float;

uniform sampler2D textureDiff;
uniform sampler2D textureNorm;
uniform float time;
varying vec3 vVertex;
varying vec3 vNormal;
varying vec2 vCoord;

const vec3 lightPos = 1.5*vec3(1.0, 5.0, 5.0);
const vec4 lightColor = vec4(1.2, 1.1, 1.1, 3.0);
const vec4 ambientColor = vec4(1.0, 1.0, 1.0, 0.4);
const vec3 falloff = vec3(0.1, 0.01, 0.1);


void main()
{

    // Extract the normal from the normal map
    vec3 normal = vNormal + normalize(texture2D(textureNorm, vCoord).rgb * 2.0 - 1.0);

    // Determine where the light is positioned (this can be set however you like)
    vec3 light_tmp = vec3(-1.1, 0.5*sin(time*5.0), 0.5) - vec3(vCoord, 0.5);
    vec3 light_pos = normalize(normalize(vec3(1.0, 1.0, 1.0))+ light_tmp);

    // Calculate the lighting diffuse value
    float diffuse = max(dot(normal, light_pos), 0.0);

    vec3 color = diffuse * texture2D(textureDiff, vCoord).rgb;

    // Set the output color of our current pixel
    gl_FragColor = vec4(color, 1.0);
}

|]
