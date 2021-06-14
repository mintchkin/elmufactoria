module Main exposing (..)

import Array exposing (Array)
import Browser
import Constants exposing (fontSize, headerSize, subHeadSize, tileSize)
import Edit
import Element as El exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Level exposing (Level)
import Replay
import Tile exposing (Tile(..))


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> loadLevel Level.first
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Model
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


loadLevel : Level -> ( Model, Cmd Msg )
loadLevel level =
    let
        ( m, c ) =
            Edit.init level
    in
    ( Editing m, Cmd.map GotEditMsg c )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Replaying m ->
            Sub.map GotReplayMsg (Replay.subscriptions m)

        Editing m ->
            Sub.map GotEditMsg (Edit.subscriptions m)



-- UPDATE


type Msg
    = LoadLevel Level
    | Edit
    | Replay
    | GotReplayMsg Replay.Msg
    | GotEditMsg Edit.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadLevel level ->
            loadLevel level

        Edit ->
            case model of
                Editing _ ->
                    ( model, Cmd.none )

                Replaying { level } ->
                    let
                        ( m, c ) =
                            Edit.init level
                    in
                    ( Editing m, Cmd.map GotEditMsg c )

        Replay ->
            case model of
                Editing { grid, level } ->
                    ( Replaying <| Replay.init grid level, Cmd.none )

                Replaying _ ->
                    ( model, Cmd.none )

        GotReplayMsg subMsg ->
            case model of
                Replaying subModel ->
                    let
                        ( m, c ) =
                            Replay.update subMsg subModel
                    in
                    ( Replaying m, Cmd.map GotReplayMsg c )

                _ ->
                    ( model, Cmd.none )

        GotEditMsg editMsg ->
            case model of
                Editing editModel ->
                    let
                        ( m, c ) =
                            Edit.update editMsg editModel
                    in
                    ( Editing m, Cmd.map GotEditMsg c )

                _ ->
                    ( model, Cmd.none )



-- VIEW


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


viewModeButton : Model -> Element Msg
viewModeButton model =
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
            el [ centerX, centerY, Font.size subHeadSize, Font.bold ] << text
    in
    case model of
        Editing _ ->
            button { onPress = Just Replay, label = label "Test Solution" }

        Replaying _ ->
            button { onPress = Just Edit, label = label "Back To Editing" }


view : Model -> Html Msg
view model =
    let
        panels =
            case model of
                Replaying replayModel ->
                    Replay.panels GotReplayMsg replayModel

                Editing editModel ->
                    Edit.panels GotEditMsg editModel

        level =
            case model of
                Replaying m ->
                    m.level

                Editing m ->
                    m.level
    in
    El.layout [ width fill, height fill, Font.size fontSize ]
        (El.column
            [ centerX, alignTop, spacing 20 ]
            [ El.column
                [ spacing 10, padding 10, centerX, Font.bold, Font.size subHeadSize, width <| px (tileSize * 8) ]
                [ text "Level Select:"
                , viewLevelSelect
                ]
            , El.paragraph
                [ Font.center, Font.bold, Font.size headerSize ]
                [ text level.name ]
            , El.row [ width fill, spacing 10 ]
                [ el [ alignTop, width (fill |> minimum (tileSize * 2)), height fill ] panels.viewLeftPanel
                , El.column [ centerX, spacing 10 ]
                    [ panels.viewGrid
                    , viewModeButton model
                    ]
                , el [ alignTop, width (fill |> minimum (tileSize * 2)), height fill ] panels.viewRightPanel
                ]
            , El.paragraph
                [ Font.center ]
                [ text level.description ]
            ]
        )
