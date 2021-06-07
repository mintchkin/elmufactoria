port module Main exposing (..)

import Array exposing (Array)
import Browser
import Edit
import Element as El exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as JsonD
import Json.Encode as JsonE
import Level exposing (Level)
import Replay
import Tile exposing (Tile(..))
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { level : Level
    , mode : Mode
    , success : Bool
    }


type Mode
    = Editing Edit.Model
    | Replaying Replay.Model


initBoard : Int -> Array Tile
initBoard size =
    let
        width =
            size * 2 - 1
    in
    Array.repeat (width ^ 2) Empty
        |> Array.set (size - 1) Begin
        |> Array.set (width ^ 2 - size) End


init : () -> ( Model, Cmd Msg )
init _ =
    ( loadLevel Level.first
    , requestProgress Level.first
    )



-- PORTS


port requestStorage : JsonE.Value -> Cmd msg


port receiveStorage : (JsonE.Value -> msg) -> Sub msg


saveProgress : Model -> Cmd msg
saveProgress model =
    let
        progressEncoder key grid =
            JsonE.object
                [ ( "op", JsonE.string "Save" )
                , ( "key", JsonE.string key )
                , ( "data", JsonE.array Tile.toJson grid )
                ]
    in
    case model.mode of
        Editing { grid } ->
            requestStorage <| progressEncoder model.level.name grid

        Replaying { grid } ->
            requestStorage <| progressEncoder model.level.name grid


requestProgress : Level -> Cmd msg
requestProgress level =
    let
        request =
            JsonE.object
                [ ( "op", JsonE.string "Load" )
                , ( "key", JsonE.string level.name )
                ]
    in
    requestStorage request


decodeProgress : JsonD.Decoder ( String, Array Tile )
decodeProgress =
    JsonD.map2 Tuple.pair
        (JsonD.field "key" JsonD.string)
        (JsonD.field "data" <| JsonD.array Tile.fromJson)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        modeSubs =
            case model.mode of
                Replaying m ->
                    Sub.map GotReplayMsg (Replay.subscriptions m)

                Editing m ->
                    Sub.map GotEditMsg (Edit.subscriptions m)

        autoSave =
            Time.every 5000 (\_ -> AutoSave)

        loadProgress =
            receiveStorage LoadProgress
    in
    Sub.batch [ autoSave, loadProgress, modeSubs ]



-- UPDATE


type Msg
    = LoadLevel Level
    | AutoSave
    | LoadProgress JsonD.Value
    | Edit
    | Replay
    | GotReplayMsg Replay.Msg
    | GotEditMsg Edit.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadLevel level ->
            ( loadLevel level, Cmd.batch [ saveProgress model, requestProgress level ] )

        AutoSave ->
            ( model, saveProgress model )

        LoadProgress json ->
            case JsonD.decodeValue decodeProgress json of
                Ok ( name, grid ) ->
                    case ( model.level.name == name, model.mode ) of
                        ( True, Replaying _ ) ->
                            ( { model | mode = Replaying <| Replay.init grid model.level }, Cmd.none )

                        ( True, Editing _ ) ->
                            ( { model | mode = Editing <| Edit.init grid }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        Edit ->
            case model.mode of
                Editing _ ->
                    ( model, Cmd.none )

                Replaying { grid } ->
                    ( { model | mode = Editing <| Edit.init grid }, saveProgress model )

        Replay ->
            case model.mode of
                Editing { grid } ->
                    ( { model | mode = Replaying <| Replay.init grid model.level }, saveProgress model )

                Replaying _ ->
                    ( model, Cmd.none )

        GotReplayMsg subMsg ->
            case model.mode of
                Replaying subModel ->
                    let
                        ( m, c ) =
                            Replay.update subMsg subModel
                    in
                    ( { model | mode = Replaying m }, Cmd.map GotReplayMsg c )

                _ ->
                    ( model, Cmd.none )

        GotEditMsg editMsg ->
            case model.mode of
                Editing editModel ->
                    let
                        ( m, c ) =
                            Edit.update editMsg editModel
                    in
                    ( { model | mode = Editing m }, Cmd.map GotEditMsg c )

                _ ->
                    ( model, Cmd.none )



-- VIEW


viewSuccessIndicator : Model -> Element Msg
viewSuccessIndicator model =
    let
        color =
            if model.success then
                rgb 0 1 0

            else
                rgb 1 0 0
    in
    El.el
        [ width (px 100)
        , height fill
        , Background.color color
        ]
        none


viewLevelSelect : Element Msg
viewLevelSelect =
    let
        levelButton number level =
            Input.button
                [ width (px tileSize)
                , height (px tileSize)
                , Border.color (rgb 0 0 0)
                , Border.width 2
                , Font.center
                ]
                { onPress = Just (LoadLevel level)
                , label = text <| String.fromInt (number + 1)
                }
    in
    El.wrappedRow
        [ width fill, spacing 10 ]
        (List.indexedMap levelButton Level.list)



--- REPLAYING ---


viewStartReplayButton : Model -> Element Msg
viewStartReplayButton model =
    let
        button =
            Input.button
                [ width fill
                , height (px tileSize)
                , Border.color (rgb 0 0 0)
                , Border.width 2
                , Font.center
                ]

        label =
            el [ centerX, centerY, Font.size 25, Font.bold ] << text
    in
    case model.mode of
        Editing _ ->
            button { onPress = Just Replay, label = label "Test Solution" }

        Replaying _ ->
            button { onPress = Just Edit, label = label "Back To Editing" }


view : Model -> Html Msg
view model =
    let
        panels =
            case model.mode of
                Replaying replayModel ->
                    Replay.panels GotReplayMsg replayModel

                Editing editModel ->
                    Edit.panels GotEditMsg editModel
    in
    El.layout [ width fill, height fill ]
        (El.column
            [ centerX, alignTop, spacing 20 ]
            [ El.column
                [ spacing 10, padding 10, centerX, Font.bold, Font.size 25, width <| px (tileSize * 8) ]
                [ text "Level Select:"
                , viewLevelSelect
                ]
            , El.paragraph
                [ Font.center, Font.bold, Font.size 40 ]
                [ text model.level.name ]
            , El.row [ width fill, spacing 10 ]
                [ el [ alignTop, width (fill |> minimum (tileSize * 2)), height fill ] panels.viewLeftPanel
                , El.column [ centerX, spacing 10 ]
                    [ panels.viewGrid
                    , viewStartReplayButton model
                    ]
                , el [ alignTop, width (fill |> minimum (tileSize * 2)), height fill ] panels.viewRightPanel
                ]
            , El.paragraph
                [ Font.center ]
                [ text model.level.description ]
            ]
        )



-- HELPERS


loadLevel : Level -> Model
loadLevel level =
    { level = level
    , mode = Editing <| Edit.init (initBoard level.size)
    , success = False
    }



--- CONSTANTS ---


tileSize : number
tileSize =
    50
