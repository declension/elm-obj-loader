module ElmLogo exposing (..)

import AnimationFrame
import Dict exposing (Dict)
import Html exposing (div, text)
import Html.Attributes as Attr
import Html.Events exposing (on, onInput)
import Http
import Json.Decode as JD
import Math.Matrix4 as M4 exposing (Mat4)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Task
import WebGL as GL
import WebGL.Texture
import WebGL.Settings exposing (cullFace, front)
import WebGL.Settings.DepthTest as DepthTest
import OBJ
import OBJ.Types exposing (Mesh(..))
import Mouse
import Window


type alias Model =
    { time : Float
    , mesh : Result String (Dict String (Dict String Mesh))
    , currentModel : String
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
    | LoadObj String (Result String (Dict String (Dict String Mesh)))
    | Zoom Float
    | MouseMove Mouse.Position
    | MouseDown Mouse.Position
    | MouseUp
    | DiffTextureLoaded (Result String GL.Texture)
    | NormTextureLoaded (Result String GL.Texture)
    | ResizeWindow Window.Size
    | SelectMesh String


initModel : Model
initModel =
    { mesh = Err "loading ..."
    , currentModel = "elmLogo.obj"
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


models =
    [ "elmLogo.obj"
    , "suzanne.obj"
    , "testObjs/elmLogoPositionandNormal.obj"
    , "testObjs/elmLogoPositionOnly.obj"
    , "testObjs/elmLogoPositionUVandNormal.obj" ++ {- OK -} ""
    , "testObjs/elmLogoPositionUV.obj"
    , "testObjs/elmLogoPositionUVTris.obj"
    , "testObjs/testPositionandNormal.obj"
    , "testObjs/testPositionOnly.obj"
    , "testObjs/testPositionUVandNormal.obj" ++ {- OK -} ""
    , "testObjs/testPositionUV.obj"
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


loadModel : String -> (String -> Result String (Dict String (Dict String Mesh)) -> msg) -> Cmd msg
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
                        msg url (Ok m)

                    Ok (Err e) ->
                        msg url (Err e)

                    Err e ->
                        msg url (Err e)
            )


renderModel : Model -> GL.Texture -> GL.Texture -> Mesh -> GL.Entity
renderModel model textureDiff textureNorm mesh =
    let
        ( camera, view ) =
            getCamera model

        modelM =
            M4.makeTranslate (vec3 -1 0 0)

        modelView =
            M4.mul view modelM

        lightPos =
            M4.transform view (vec3 -2 (cos model.time) (sin model.time))
    in
        case mesh of
            WithTexture { vertices, indices } ->
                GL.entityWith [ DepthTest.default, cullFace front ]
                    vert
                    frag
                    (GL.indexedTriangles vertices indices)
                    { camera = camera
                    , mvMat = modelView
                    , textureDiff = textureDiff
                    , textureNorm = textureNorm
                    , time = model.time
                    , light_diffuse = vec3 1.0 1.0 1.0
                    , light_position = lightPos
                    , light_specular = vec3 0.15 0.15 0.15
                    , material_diffuse = vec3 0.5 0.5 0.5
                    , material_specular = vec3 0.15 0.15 0.15
                    , material_shininess = 100.002
                    }

            WithoutTexture { vertices, indices } ->
                GL.entityWith [ DepthTest.default, cullFace front ]
                    vert
                    frag
                    (GL.indexedTriangles vertices indices)
                    { camera = camera
                    , mvMat = modelView
                    , textureDiff = textureDiff
                    , textureNorm = textureNorm
                    , time = model.time
                    , light_diffuse = vec3 1.0 1.0 1.0
                    , light_position = lightPos
                    , light_specular = vec3 0.15 0.15 0.15
                    , material_diffuse = vec3 0.5 0.5 0.5
                    , material_specular = vec3 0.15 0.15 0.15
                    , material_shininess = 100.002
                    }

            _ ->
                Debug.crash "I wasn't expecting a model with tangents!"


getCamera : Model -> ( Mat4, Mat4 )
getCamera { mouseDelta, zoom, windowSize } =
    let
        ( mx, my ) =
            ( mouseDelta.x, mouseDelta.y )

        aspect =
            toFloat windowSize.width / toFloat windowSize.height
    in
        ( (M4.makePerspective 45 (aspect) 0.01 10000)
        , (M4.makeLookAt (vec3 (zoom * sin -mx * sin my) (-zoom * cos my + 1) (zoom * cos -mx * sin my)) (vec3 0 1 0) (vec3 0 1 0))
        )


onZoom : Html.Attribute Msg
onZoom =
    on "wheel" (JD.map Zoom (JD.field "deltaY" JD.float))


view : Model -> Html.Html Msg
view model =
    div []
        [ selectModel model
        , case ( model.mesh, model.diffText, model.normText ) of
            ( Ok m, Ok td, Ok tn ) ->
                GL.toHtmlWith [ GL.antialias, GL.depth 1 ]
                    [ onZoom, Attr.width (model.windowSize.width - 10), Attr.height (model.windowSize.height - 10) ]
                    (Dict.values m
                        |> List.concatMap Dict.values
                        |> List.map (renderModel model td tn)
                    )

            err ->
                Html.div [] [ Html.text (toString err) ]
        ]


selectModel model =
    div []
        [ Html.select [ onInput SelectMesh, Attr.value model.currentModel ] (List.map (\t -> Html.option [ Attr.value t ] [ text t ]) models)
        , text model.currentModel
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model | time = model.time + dt / 1000 }, Cmd.none )

        Zoom dy ->
            ( { model | zoom = max 0.01 (model.zoom + dy / 100) }, Cmd.none )

        SelectMesh m ->
            ( model, loadModel m LoadObj )

        LoadObj url mesh ->
            ( { model | mesh = mesh, currentModel = url }, Cmd.none )

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


{-|
-}
vert =
    [glsl|

attribute vec3 position;
attribute vec3 normal;
//attribute vec2 texCoord;
//varying vec2 Vertex_UV;
varying vec3 Vertex_Normal;
varying vec3 Vertex_LightDir;
varying vec3 Vertex_EyeVec;

uniform mat4 camera;
uniform mat4 mvMat;

uniform vec3 light_position;

void main()
{
    vec4 view_vertex = mvMat * vec4(position, 1.0);
    gl_Position = camera * view_vertex;
    //Vertex_UV = texCoord;
    Vertex_Normal = normal; //(mvMat * vec4(normal, 1.0)).xyz; // TODO: WRONG! needs the normal matrix
    Vertex_LightDir = light_position - view_vertex.xyz;
    Vertex_EyeVec = -view_vertex.xyz;
}

|]


{-|
TODO: this shader is very wrong
-}
frag =
    [glsl|

precision mediump float;

uniform sampler2D textureDiff; // color map
uniform sampler2D textureNorm; // normal map
uniform vec3 light_diffuse;
uniform vec3 material_diffuse;
uniform vec3 light_specular;
uniform vec3 material_specular;
uniform float material_shininess;
//varying vec2 Vertex_UV;
varying vec3 Vertex_Normal;
varying vec3 Vertex_LightDir;
varying vec3 Vertex_EyeVec;

void main()
{
    /*vec2 uv = Vertex_UV;

    vec3 N = normalize(Vertex_Normal);
    vec3 L = normalize(Vertex_LightDir);
    vec3 V = normalize(Vertex_EyeVec);
    vec3 PN = perturb_normal(N, V, uv);

    vec3 diff_color = texture2D(textureDiff, uv).rgb;
    vec3 final_color = vec3(0.2, 0.15, 0.15) * diff_color;

    float lambertTerm = dot(PN, L);
    if (lambertTerm > 0.0) {
        final_color += light_diffuse * material_diffuse * lambertTerm * diff_color;

        vec3 E = normalize(Vertex_EyeVec);
        vec3 R = reflect(-L, PN);
        float specular = pow( max(dot(R, E), 0.0), material_shininess);
        final_color += light_specular * material_specular * specular;
    }*/
    //final_color = PN;
    vec3 final_color = (Vertex_Normal + vec3(1.0))*0.5;
    gl_FragColor = vec4(final_color, 1.0);
}

|]
