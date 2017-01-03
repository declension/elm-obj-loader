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
            M4.makeTranslate (vec3 -1 0 0)

        modelView =
            M4.mul view modelM

        lightPos =
            M4.transform view (vec3 -2 (cos model.time) (sin model.time))
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
                    , light_diffuse = vec3 1.0 1.0 1.0
                    , light_position = lightPos
                    , light_specular = vec3 0.15 0.15 0.15
                    , material_diffuse = vec3 0.5 0.5 0.5
                    , material_specular = vec3 0.15 0.15 0.15
                    , material_shininess = 100.002
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
        , (M4.makeLookAt (vec3 (zoom * sin -mx * sin my) (-zoom * cos my) (zoom * cos -mx * sin my)) (vec3 0 2 0) (vec3 0 1 0))
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


{-|
-}
vert =
    [glsl|

attribute vec3 pos;
attribute vec3 norm;
attribute vec2 coord;
varying vec2 Vertex_UV;
varying vec3 Vertex_Normal;
varying vec3 Vertex_LightDir;
varying vec3 Vertex_EyeVec;

uniform mat4 camera;
uniform mat4 mvMat;

uniform vec3 light_position;

void main()
{
    vec4 view_vertex = mvMat * vec4(pos, 1.0);
    gl_Position = camera * view_vertex;
    Vertex_UV = coord;
    Vertex_Normal = (mvMat * vec4(norm, 1.0)).xyz;
    Vertex_LightDir = light_position - view_vertex.xyz;
    Vertex_EyeVec = -view_vertex.xyz;
}

|]


{-|
TODO: this shader is very wrong
-}
frag =
    [glsl|
//__REPLACE_ME_WITH__(#extension GL_OES_standard_derivatives : enable)

precision mediump float;

uniform sampler2D textureDiff; // color map
uniform sampler2D textureNorm; // normal map
uniform vec3 light_diffuse;
uniform vec3 material_diffuse;
uniform vec3 light_specular;
uniform vec3 material_specular;
uniform float material_shininess;
varying vec2 Vertex_UV;
varying vec3 Vertex_Normal;
varying vec3 Vertex_LightDir;
varying vec3 Vertex_EyeVec;

// http://www.thetenthplanet.de/archives/1180
mat3 cotangent_frame(vec3 N, vec3 p, vec2 uv)
{
    // get edge vectors of the pixel triangle
    vec3 dp1 = dFdx( p );
    vec3 dp2 = dFdy( p );
    vec2 duv1 = dFdx( uv );
    vec2 duv2 = dFdy( uv );

    // solve the linear system
    vec3 dp2perp = cross( dp2, N );
    vec3 dp1perp = cross( N, dp1 );
    vec3 T = dp2perp * duv1.x + dp1perp * duv2.x;
    vec3 B = dp2perp * duv1.y + dp1perp * duv2.y;

    // construct a scale-invariant frame
    float invmax = inversesqrt( max( dot(T,T), dot(B,B) ) );
    return mat3( T * invmax, B * invmax, N );
}

vec3 perturb_normal( vec3 N, vec3 V, vec2 texcoord )
{
    // assume N, the interpolated vertex normal and
    // V, the view vector (vertex to eye)
    vec3 map = texture2D(textureNorm, texcoord ).xyz;
    map = map * 255./127. - 128./127.;
    mat3 TBN = cotangent_frame(N, -V, texcoord);
    return normalize(TBN * map);
}

void main()
{
    vec2 uv = Vertex_UV;

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
    }
    //final_color = PN;
    //final_color = N;
    gl_FragColor = vec4(final_color, 1.0);
}

|]
