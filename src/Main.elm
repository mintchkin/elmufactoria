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
import Session exposing (Store)
import Tile exposing (Tile(..))


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> mapUpdate Editing GotEditMsg (Edit.init Level.first)
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


mapUpdate : (model -> Model) -> (msg -> Msg) -> ( model, Cmd msg ) -> ( Model, Cmd Msg )
mapUpdate mapModel mapCmd =
    Tuple.mapBoth mapModel (Cmd.map mapCmd)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        modeSubs =
            case model of
                Replaying m ->
                    Sub.map GotReplayMsg (Replay.subscriptions m)

                Editing m ->
                    Sub.map GotEditMsg (Edit.subscriptions m)
    in
    Sub.batch [ modeSubs, Session.receive GotSession ]



-- UPDATE


type Msg
    = LoadLevel Level
    | Edit
    | Replay
    | GotReplayMsg Replay.Msg
    | GotEditMsg Edit.Msg
    | GotSession Store


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadLevel level ->
            mapUpdate Editing GotEditMsg (Edit.init level)

        Edit ->
            case model of
                Editing _ ->
                    ( model, Cmd.none )

                Replaying { level } ->
                    mapUpdate Editing GotEditMsg (Edit.init level)

        Replay ->
            case model of
                Editing { level } ->
                    mapUpdate Replaying GotReplayMsg (Replay.init level)

                Replaying _ ->
                    ( model, Cmd.none )

        GotReplayMsg subMsg ->
            case model of
                Replaying subModel ->
                    mapUpdate Replaying GotReplayMsg (Replay.update subMsg subModel)

                _ ->
                    ( model, Cmd.none )

        GotEditMsg editMsg ->
            case model of
                Editing editModel ->
                    mapUpdate Editing GotEditMsg (Edit.update editMsg editModel)

                _ ->
                    ( model, Cmd.none )

        GotSession store ->
            let
                getGrid { level } =
                    Session.getGrid <| Session.fromStore level store
            in
            case model of
                Editing editModel ->
                    ( Editing { editModel | grid = getGrid editModel }, Cmd.none )

                Replaying replayModel ->
                    ( Replaying { replayModel | grid = getGrid replayModel }, Cmd.none )



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


view : Model -> Html Msg
view model =
    let
        level =
            case model of
                Replaying m ->
                    m.level

                Editing m ->
                    m.level

        body =
            case model of
                Replaying replayModel ->
                    Replay.view Edit GotReplayMsg replayModel

                Editing editModel ->
                    Edit.view Replay GotEditMsg editModel
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
            , body
            , El.paragraph
                [ Font.center ]
                [ text level.description ]
            ]
        )
