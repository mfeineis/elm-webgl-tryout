module Main exposing (main)

import AnimationFrame
import Html exposing (Html)
import Mouse
import Render
import Resources
import Task
import Time exposing (Time)
import WebGL.Texture as Texture exposing (Texture)
import Window


type GameState
    = Running
    | Paused


type Msg
    = Animate Time
    | BackgroundLoaded Texture
    | ChangeGameState GameState
    | MouseMove Mouse.Position
    | TextureError Texture.Error
    | WindowResize Window.Size


type alias Model =
    { background : Maybe Texture
    , elapsed : Time
    , frame : Int
    , gameState : GameState
    , mousePosition : Mouse.Position
    , windowSize : Window.Size
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = Render.view
        }


init : ( Model, Cmd Msg )
init =
    { background = Nothing
    , elapsed = 0
    , frame = 0
    , gameState = Paused
    , mousePosition = Mouse.Position 200 200
    , windowSize = Window.Size 400 400
    }
        |> withCmds
            [ Resources.loadBackground BackgroundLoaded TextureError
            , Task.perform WindowResize Window.size
            ]


subscriptions : Model -> Sub Msg
subscriptions { gameState } =
    Sub.batch
        [ if gameState == Running then
            AnimationFrame.diffs Animate
          else
            Sub.none
        , if gameState == Running then
            Mouse.moves MouseMove
          else
            Sub.none
        , Window.resizes WindowResize
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate elapsed ->
            animate elapsed model |> withoutCmd

        BackgroundLoaded background ->
            { model | background = Just background } |> withoutCmd

        ChangeGameState gameState ->
            { model | gameState = gameState } |> withoutCmd

        MouseMove position ->
            { model | mousePosition = position } |> withoutCmd

        TextureError err ->
            Debug.log "Texture.Error" (model |> withoutCmd)

        WindowResize size ->
            let
                _ =
                    Debug.log "WindowResize" size
            in
            { model | windowSize = size } |> withoutCmd


animate : Time -> Model -> Model
animate elapsed model =
    let
        timeout =
            150

        newElapsed =
            elapsed + model.elapsed
    in
    if newElapsed > timeout then
        { model
            | frame = (model.frame + 1) % 24
            , elapsed = newElapsed - timeout
        }
    else
        { model
            | elapsed = newElapsed
        }



-- Helpers


withCmds : List (Cmd Msg) -> Model -> ( Model, Cmd Msg )
withCmds cmds model =
    ( model, Cmd.batch cmds )


withoutCmd : Model -> ( Model, Cmd Msg )
withoutCmd model =
    ( model, Cmd.none )
