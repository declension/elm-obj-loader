module Suzanne exposing (..)

import AnimationFrame
import Dict exposing (Dict)
import Html
import Html.Attributes as Attr
import Html.Events exposing (on)
import Json.Decode as JD
import Math.Matrix4 as M4 exposing (Mat4)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Task
import WebGL as GL
import WebGL.Texture
import WebGL.Settings exposing (cullFace, front)
import WebGL.Settings.DepthTest as DepthTest


--

import OBJ
import OBJ.Types exposing (MeshWith, VertexWithTexture)
import Shaders exposing (reflectionVert, reflectionFrag)


type alias Model =
    { time : Float
    , mesh : Result String (MeshWith VertexWithTexture)
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
    | LoadObj (Result String (MeshWith VertexWithTexture))
    | Zoom Float
    | TextureLoaded (Result String GL.Texture)


initModel : Model
initModel =
    { mesh = Err "loading ...", time = 0, zoom = 10, reflectionTexture = Err "Loading texture..." }


initCmd : Cmd Msg
initCmd =
    Cmd.batch
        [ OBJ.loadMesh "meshes/suzanne.obj" LoadObj
        , loadTexture "textures/chavant.jpg" TextureLoaded
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


renderModel : Model -> GL.Texture -> MeshWith VertexWithTexture -> GL.Entity
renderModel { zoom, time } texture { vertices, indices } =
    let
        ( camera, view ) =
            getCamera zoom time

        model =
            M4.makeRotate time (vec3 0 1 0)

        modelView =
            M4.mul view model
    in
        GL.entityWith [ DepthTest.default, cullFace front ]
            reflectionVert
            reflectionFrag
            (GL.indexedTriangles vertices indices)
            { camera = camera, mvMat = modelView, texture = texture }


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
    case ( model.mesh, model.reflectionTexture ) of
        ( Ok m, Ok t ) ->
            GL.toHtmlWith [ GL.antialias, GL.depth 1 ]
                [ onZoom, Attr.width 400, Attr.height 400 ]
                [ renderModel model t m ]

        ( a, b ) ->
            Html.div [] [ Html.text (toString a ++ "\n\n\n" ++ toString b) ]


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
