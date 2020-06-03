module ModelViewer exposing (CameraInfo, Model, Msg(..), Size, decodeMouse, getCamera, getDelta, initCmd, initModel, loadModel, loadTexture, main, models, onZoom, renderCullFace, renderModel, selectModel, subscriptions, update, view)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onMouseDown, onMouseMove, onMouseUp, onResize)
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Html.Events exposing (on, onCheck, onInput)
import Json.Decode as JD exposing (int)
import Math.Matrix4 as M4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import OBJ
import OBJ.Types exposing (Mesh(..), ObjFile)
import Shaders
import String exposing (fromInt)
import Task
import Time exposing (Posix(..), posixToMillis)
import WebGL as GL
import WebGL.Settings exposing (cullFace, front)
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Texture exposing (Error(..), Texture)


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initModel, initCmd )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- MODEL


type alias Model =
    { time : Float
    , mesh : Result String ObjFile
    , currentModel : String
    , zoom : Float
    , diffText : Result String Texture
    , normText : Result String Texture
    , isDown : Bool
    , lastMousePos : Vec2
    , mouseDelta : Vec2
    , windowSize : Size
    , withTangent : Bool
    }


initModel : Model
initModel =
    { mesh = Err "loading ..."
    , currentModel = "meshes/elmLogo.obj"
    , time = 0
    , zoom = 5
    , diffText = Err "Loading texture..."
    , normText = Err "Loading texture..."
    , isDown = False
    , lastMousePos = vec2 0 0
    , mouseDelta = vec2 0 (pi / 2)
    , windowSize = Size 800 600
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
        ]


loadModel : Bool -> String -> Cmd Msg
loadModel withTangents url =
    OBJ.loadObjFileWith { withTangents = withTangents } url (LoadObj url)



-- UPDATE


type alias Size =
    { width : Int, height : Int }


type Msg
    = Tick Float
    | LoadObj String (Result String (Dict String (Dict String Mesh)))
    | Zoom Float
    | MouseMove Int Int
    | MouseDown Int Int
    | MouseUp
    | DiffTextureLoaded (Result String Texture)
    | NormTextureLoaded (Result String Texture)
    | ResizeWindow Int Int
    | SelectMesh String
    | SetUseTangent Bool


type alias CameraInfo =
    { projection : Mat4, view : Mat4, viewProjection : Mat4, position : Vec3 }


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

        MouseDown x y ->
            ( { model | isDown = True, lastMousePos = vec2 (toFloat x) (toFloat y) }, Cmd.none )

        MouseUp ->
            ( { model | isDown = False }, Cmd.none )

        MouseMove x y ->
            let
                pos =
                    vec2 (toFloat x) (toFloat y)
            in
            ( { model | mouseDelta = getDelta pos model.lastMousePos model.mouseDelta, lastMousePos = pos }, Cmd.none )

        ResizeWindow x y ->
            ( { model | windowSize = { width = x, height = y } }, Cmd.none )



-- VIEW / RENDER


renderModel : Model -> Texture -> Texture -> Mesh -> GL.Entity
renderModel model textureDiff textureNorm mesh =
    let
        camera =
            getCamera model

        modelM =
            M4.makeTranslate (vec3 -1 0 0)

        theta =
            2 * model.time

        lightPos =
            vec3 (0.5 * cos theta) (1 + 0.5 * sin theta) 0.5

        uniforms =
            { camera = camera
            , mvMat = M4.mul camera.view modelM
            , modelViewProjectionMatrix = M4.mul camera.viewProjection modelM
            , modelMatrix = modelM
            , viewPosition = camera.position
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


getCamera : Model -> CameraInfo
getCamera { mouseDelta, zoom, windowSize } =
    let
        ( mx, my ) =
            ( Vec2.getX mouseDelta, Vec2.getY mouseDelta )

        aspect =
            toFloat windowSize.width / toFloat windowSize.height

        proj =
            M4.makePerspective 45 aspect 0.01 10000

        position =
            vec3 (zoom * sin -mx * sin my) (-zoom * cos my + 1) (zoom * cos -mx * sin my)

        view_ =
            M4.makeLookAt position (vec3 0 1 0) (vec3 0 1 0)
    in
    { projection = proj, view = view_, viewProjection = M4.mul proj view_, position = position }


view : Model -> Html.Html Msg
view model =
    div []
        [ selectModel model
        , case ( model.mesh, model.diffText, model.normText ) of
            ( Ok m, Ok td, Ok tn ) ->
                GL.toHtmlWith [ GL.antialias, GL.depth 1 ]
                    [ onZoom
                    , Attr.width model.windowSize.width
                    , Attr.height model.windowSize.height
                    , Attr.style "position" "absolute"
                    ]
                    (Dict.values m
                        |> List.concatMap Dict.values
                        |> List.map (renderModel model td tn)
                    )

            ( Err m, _, _ ) ->
                Html.div [] [ Html.text <| "ERROR with mesh: " ++ m ]

            _ ->
                Html.div [] [ Html.text <| "Non-mesh error." ]
        ]


selectModel : Model -> Html Msg
selectModel model =
    div [ Attr.style "position" "absolute", Attr.style "z-index" "2", Attr.style "backgroundColor" "white" ]
        ([ Html.select [ onInput SelectMesh, Attr.value model.currentModel ]
            (List.map (\t -> Html.option [ Attr.value t ] [ text t ]) models)
         ]
            ++ (if String.startsWith "meshes/elmLogo" model.currentModel then
                    [ text "\twith normal map: "
                    , Html.input [ Attr.type_ "checkbox", onCheck SetUseTangent, Attr.checked model.withTangent ] []
                    ]

                else
                    []
               )
        )



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ((if model.isDown then
            [ onMouseMove (decodeMouse MouseMove) ]

          else
            []
         )
            ++ [ onAnimationFrameDelta Tick
               , onMouseUp (JD.succeed MouseUp)
               , onMouseDown (decodeMouse MouseDown)
               , onResize ResizeWindow
               ]
        )



-- HELPERS


decodeMouse : (Int -> Int -> Msg) -> JD.Decoder Msg
decodeMouse mapper =
    JD.map2 mapper
        (JD.field "clientX" int)
        (JD.field "clientY" int)


onZoom : Html.Attribute Msg
onZoom =
    on "wheel" (JD.map Zoom (JD.field "deltaY" JD.float))


getDelta : Vec2 -> Vec2 -> Vec2 -> Vec2
getDelta curr lastP delta =
    vec2 ((Vec2.getX curr - Vec2.getX lastP) / 100 + Vec2.getX delta)
        ((Vec2.getY curr - Vec2.getY lastP) / 100 + Vec2.getY delta |> clamp 0.01 pi)


loadTexture : String -> (Result String Texture -> msg) -> Cmd msg
loadTexture url msg =
    WebGL.Texture.load url
        |> Task.attempt
            (\r ->
                case r of
                    Ok t ->
                        msg (Ok t)

                    Err LoadError ->
                        msg (Err "Failed to load texture")

                    Err (SizeError w h) ->
                        msg (Err ("Invalid texture size: " ++ fromInt w ++ " x " ++ fromInt h))
            )


renderCullFace : GL.Shader a u v -> GL.Shader {} u v -> GL.Mesh a -> u -> GL.Entity
renderCullFace =
    GL.entityWith [ DepthTest.default, cullFace front ]
