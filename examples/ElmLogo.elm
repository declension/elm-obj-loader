module ElmLogo exposing (..)

import AnimationFrame
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Html.Events exposing (on, onInput, onCheck)
import Json.Decode as JD
import Math.Matrix4 as M4 exposing (Mat4)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Task
import WebGL as GL
import WebGL.Texture
import WebGL.Settings exposing (cullFace, front)
import WebGL.Settings.DepthTest as DepthTest
import Mouse
import Window


--

import Shaders
import OBJ
import OBJ.Types exposing (Mesh(..))


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
    , withTangent : Bool
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
    | SetUseTangent Bool


initModel : Model
initModel =
    { mesh = Err "loading ..."
    , currentModel = "meshes/elmLogo.obj"
    , time = 0
    , zoom = 5
    , diffText = Err "Loading texture..."
    , normText = Err "Loading texture..."
    , isDown = False
    , lastMousePos = Mouse.Position 0 0
    , mouseDelta = MouseDelta 0 (pi / 2)
    , windowSize = Window.Size 800 600
    , withTangent = True
    }


models : List String
models =
    [ "meshes/elmLogo.obj"
    , "meshes/suzanneNoUV.obj"
    ]


initCmd : Cmd Msg
initCmd =
    Cmd.batch
        [ loadModel True "meshes/elmLogo.obj"
        , loadTexture "textures/elmLogoDiffuse.png" DiffTextureLoaded
        , loadTexture "textures/elmLogoNorm.png" NormTextureLoaded
        , Task.perform ResizeWindow Window.size
        ]


loadModel : Bool -> String -> Cmd Msg
loadModel withTangents url =
    OBJ.loadObjFileWith { withTangents = withTangents } url (LoadObj url)


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


renderModel : Model -> GL.Texture -> GL.Texture -> Mesh -> GL.Entity
renderModel model textureDiff textureNorm mesh =
    let
        ( camera, view, viewProjection, cameraPos ) =
            getCamera model

        modelM =
            M4.makeTranslate (vec3 -1 0 0)

        lightPos =
            vec3 (0.5 * cos (2 * model.time)) (1 + 0.5 * sin (2 * model.time)) 0.5

        uniforms =
            { camera = camera
            , mvMat = M4.mul view modelM
            , modelViewProjectionMatrix = M4.mul viewProjection modelM
            , modelMatrix = modelM
            , viewPosition = cameraPos
            , textureDiff = textureDiff
            , textureNorm = textureNorm
            , lightPosition = lightPos
            }
    in
        case mesh of
            WithoutTexture { vertices, indices } ->
                renderCullFace Shaders.simpleVert Shaders.simpleFrag (GL.indexedTriangles vertices indices) uniforms

            WithTexture { vertices, indices } ->
                renderCullFace Shaders.noNormalVert Shaders.noNormalFrag (GL.indexedTriangles vertices indices) uniforms

            WithTextureAndTangent { vertices, indices } ->
                renderCullFace Shaders.normalVert Shaders.normalFrag (GL.indexedTriangles vertices indices) uniforms


renderCullFace : GL.Shader a u v -> GL.Shader {} u v -> GL.Mesh a -> u -> GL.Entity
renderCullFace =
    GL.entityWith [ DepthTest.default, cullFace front ]


getCamera : Model -> ( Mat4, Mat4, Mat4, Vec3 )
getCamera { mouseDelta, zoom, windowSize } =
    let
        ( mx, my ) =
            ( mouseDelta.x, mouseDelta.y )

        aspect =
            toFloat windowSize.width / toFloat windowSize.height

        proj =
            M4.makePerspective 45 aspect 0.01 10000

        position =
            vec3 (zoom * sin -mx * sin my) (-zoom * cos my + 1) (zoom * cos -mx * sin my)

        view =
            M4.makeLookAt (position) (vec3 0 1 0) (vec3 0 1 0)
    in
        ( proj, view, M4.mul proj view, position )


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
                    [ onZoom
                    , Attr.width (model.windowSize.width)
                    , Attr.height (model.windowSize.height)
                    , Attr.style [ ( "position", "absolute" ) ]
                    ]
                    (Dict.values m
                        |> List.concatMap Dict.values
                        |> List.map (renderModel model td tn)
                    )

            err ->
                Html.div [] [ Html.text (toString err) ]
        ]


selectModel : Model -> Html Msg
selectModel model =
    div [ Attr.style [ ( "position", "absolute" ), ( "z-index", "2" ), ( "backgroundColor", "white" ) ] ]
        ([ Html.select [ onInput SelectMesh, Attr.value model.currentModel ]
            (List.map (\t -> Html.option [ Attr.value t ] [ text t ]) models)
         ]
            ++ if String.startsWith "meshes/elmLogo" model.currentModel then
                [ text "\twith normal map: "
                , Html.input [ Attr.type_ "checkbox", onCheck SetUseTangent, Attr.checked model.withTangent ] []
                ]
               else
                []
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model | time = model.time + dt / 1000 }, Cmd.none )

        Zoom dy ->
            ( { model | zoom = max 0.01 (model.zoom + dy / 100) }, Cmd.none )

        SelectMesh url ->
            ( model, loadModel model.withTangent url )

        SetUseTangent t ->
            ( { model | withTangent = t }, loadModel t model.currentModel )

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
