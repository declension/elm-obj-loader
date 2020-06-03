module Suzanne exposing (Model, Msg(..), getCamera, initCmd, initModel, loadTexture, main, onZoom, renderModel, update, view)

import Browser
import Browser.Dom
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Html
import Html.Attributes as Attr
import Html.Events exposing (on)
import Json.Decode as JD
import Math.Matrix4 as M4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import OBJ
import OBJ.Types exposing (MeshWith, VertexWithTexture)
import Shaders exposing (reflectionFrag, reflectionVert)
import String exposing (fromInt)
import Task
import WebGL as GL
import WebGL.Settings exposing (cullFace, front)
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Texture exposing (Error(..), Texture)


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initModel, initCmd )
        , view = view
        , subscriptions =
            always <|
                Sub.batch
                    [ onAnimationFrameDelta Tick
                    , onResize ResizeWindow
                    ]
        , update = update
        }



-- MODEL


type alias Model =
    { time : Float
    , mesh : Result String (MeshWith VertexWithTexture)
    , zoom : Float
    , width : Int
    , height : Int
    , reflectionTexture : Result String Texture
    }


initModel : Model
initModel =
    { mesh = Err "loading ...", time = 0, width = 640, height = 640, zoom = 1.5, reflectionTexture = Err "Loading texture..." }


initCmd : Cmd Msg
initCmd =
    Cmd.batch
        [ OBJ.loadMesh "meshes/suzanne.obj" LoadObj
        , loadTexture "textures/chavant.jpg" TextureLoaded
        , Task.perform
            (\{ viewport } ->
                ResizeWindow (round viewport.width) (round viewport.height)
            )
            Browser.Dom.getViewport
        ]



-- UPDATE


type Msg
    = Tick Float
    | LoadObj (Result String (MeshWith VertexWithTexture))
    | Zoom Float
    | ResizeWindow Int Int
    | TextureLoaded (Result String Texture)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model | time = model.time + dt / 1000 }, Cmd.none )

        Zoom dy ->
            ( { model | zoom = max 1 (model.zoom + dy / 15) }, Cmd.none )

        LoadObj mesh ->
            ( { model | mesh = mesh }, Cmd.none )

        TextureLoaded t ->
            ( { model | reflectionTexture = t }, Cmd.none )

        ResizeWindow w h ->
            ( { model | width = w, height = h }, Cmd.none )



-- VIEW / RENDER


renderModel : Model -> Texture -> MeshWith VertexWithTexture -> GL.Entity
renderModel { zoom, time, width, height } texture { vertices, indices } =
    let
        aspect =
            toFloat width / toFloat height

        ( camera, view_ ) =
            getCamera aspect zoom time

        model =
            M4.makeRotate (0.6 * time) (vec3 0 1 0)

        modelView =
            M4.mul view_ model
    in
    GL.entityWith [ DepthTest.default, cullFace front ]
        reflectionVert
        reflectionFrag
        (GL.indexedTriangles vertices indices)
        { camera = camera, mvMat = modelView, texture = texture }


getCamera : Float -> Float -> Float -> ( Mat4, Mat4 )
getCamera aspect zoom _ =
    let
        pos =
            0.5 + zoom * zoom
    in
    ( M4.makePerspective 45 aspect 0.01 10000
    , M4.makeLookAt (vec3 pos (pos / 2) pos) (vec3 0 0 0) (vec3 0 1 0)
    )


view : Model -> Html.Html Msg
view model =
    case ( model.mesh, model.reflectionTexture ) of
        ( Ok m, Ok t ) ->
            GL.toHtmlWith [ GL.antialias, GL.depth 1 ]
                [ onZoom, Attr.width model.width, Attr.height model.height ]
                [ renderModel model t m ]

        ( Err meshErr, Err reflectionErr ) ->
            Html.pre [] [ Html.text (meshErr ++ "\n\n\n" ++ reflectionErr) ]

        ( Err meshErr, Ok _ ) ->
            Html.pre [] [ Html.text <| "Mesh error: " ++ meshErr ]

        ( Ok _, Err reflectionErr ) ->
            Html.pre [] [ Html.text <| "Reflection error: " ++ reflectionErr ]



-- HELPERS


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


onZoom : Html.Attribute Msg
onZoom =
    on "wheel" (JD.map Zoom (JD.field "deltaY" JD.float))
