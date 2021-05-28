module Main exposing (..)

import Array exposing (Array(..))
import Browser exposing (..)
import Browser.Events as BE
import Direction as Direction exposing (Direction(..))
import Element as El exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as E
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as D
import Level as Level exposing (Code(..), Level, Outcome(..))
import Robot exposing (Progress(..), Robot, advance, checkSolution)
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
    { mousePos : Position
    , grid : Array Tile
    , level : Level
    , mode : Mode
    , success : Bool
    }


type Mode
    = Editing Tile
    | Replaying (List Robot)


type Position
    = Position Float Float


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
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mode of
        Replaying _ ->
            Sub.batch
                [ BE.onMouseDown <|
                    D.map2 SetPosition
                        (D.field "pageX" D.float)
                        (D.field "pageY" D.float)
                , Time.every 1000 (\_ -> StepReplay)
                ]

        Editing _ ->
            Sub.batch
                [ BE.onMouseMove <|
                    D.map2 SetPosition
                        (D.field "pageX" D.float)
                        (D.field "pageY" D.float)
                , BE.onKeyDown <|
                    D.map PressKey (D.field "key" D.string)
                ]



-- UPDATE


type Msg
    = LoadLevel Level
    | Edit Tile
    | Replay
    | PressKey String
    | SetPosition Float Float
    | SetGridTile Int
    | StepReplay


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadLevel level ->
            ( loadLevel level, Cmd.none )

        Edit tile ->
            ( { model | mode = Editing tile }
            , Cmd.none
            )

        Replay ->
            ( { model | mode = Replaying (Robot.initAll model.level) }
            , Cmd.none
            )

        PressKey key ->
            case model.mode of
                Replaying _ ->
                    ( model, Cmd.none )

                Editing tile ->
                    case key of
                        "Esc" ->
                            ( { model | mode = Editing Empty }, Cmd.none )

                        "Escape" ->
                            ( { model | mode = Editing Empty }, Cmd.none )

                        _ ->
                            case ( tile, Direction.fromKey key ) of
                                ( Track _, Just direction ) ->
                                    ( { model | mode = Editing (Track direction) }, Cmd.none )

                                ( RBSplitter _, Just direction ) ->
                                    ( { model | mode = Editing (RBSplitter direction) }, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

        SetPosition x y ->
            ( { model | mousePos = Position x y }
            , Cmd.none
            )

        -- EDITING ONLY
        SetGridTile index ->
            case model.mode of
                Replaying _ ->
                    ( model, Cmd.none )

                Editing tile ->
                    let
                        replacement =
                            case Array.get index model.grid of
                                Just Begin ->
                                    Begin

                                Just End ->
                                    End

                                _ ->
                                    tile

                        grid =
                            Array.set index replacement model.grid

                        success =
                            checkSolution model.level grid
                    in
                    ( { model | grid = grid, success = success }, Cmd.none )

        -- REPLAYING ONLY
        StepReplay ->
            case model.mode of
                Editing _ ->
                    ( model, Cmd.none )

                Replaying [] ->
                    ( model, Cmd.none )

                Replaying (robot :: rest) ->
                    case advance model.grid robot of
                        Finished _ ->
                            ( { model | mode = Replaying rest }, Cmd.none )

                        Working bot ->
                            ( { model | mode = Replaying (bot :: rest) }, Cmd.none )



-- VIEW
-- CONSTANTS


tileSize : Int
tileSize =
    50


viewBox : List (Attribute Msg) -> Tile -> Element Msg
viewBox attributes tile =
    el
        ([ width (px tileSize)
         , height (px tileSize)
         , Border.color (rgb 0 0 0)
         , Border.width 2
         ]
            ++ attributes
        )
        (Tile.view tile)


viewPalette : Element Msg
viewPalette =
    El.column
        [ El.spacing 10
        , El.alignTop
        ]
        [ viewBox [ E.onClick (Edit (Track Down)) ] (Track Down)
        , viewBox [ E.onClick (Edit (RBSplitter Down)) ] (RBSplitter Down)
        , viewBox [ E.onClick (Edit Empty) ] Empty
        ]


viewBrush : Model -> Element Msg
viewBrush model =
    let
        (Position xpos ypos) =
            model.mousePos
    in
    case model.mode of
        Editing Empty ->
            none

        Editing tile ->
            el
                [ htmlAttribute <| HA.style "position" "fixed"
                , htmlAttribute <| HA.style "left" (toPixels (xpos - (toFloat tileSize / 2)))
                , htmlAttribute <| HA.style "top" (toPixels (ypos - (toFloat tileSize / 2)))
                , htmlAttribute <| HA.style "pointer-events" "none"
                ]
                (viewBox [] tile)

        Replaying _ ->
            none


viewGridCell : Model -> Int -> Tile -> Element Msg
viewGridCell model index tile =
    case model.mode of
        Replaying (robot :: _) ->
            if robot.position == index then
                viewBox [ inFront Robot.view ] tile

            else
                viewBox [] tile

        _ ->
            viewBox [ E.onClick (SetGridTile index) ] tile


viewGrid : Model -> Element Msg
viewGrid model =
    El.column
        []
        (model.grid
            |> Array.indexedMap (viewGridCell model)
            |> Array.toList
            |> squareUp
            |> List.map (row [])
        )


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


playIcon : Element msg
playIcon =
    el [ centerX, centerY, Font.size 40 ] (text "▶︎")


pauseIcon : Element msg
pauseIcon =
    El.row
        [ width fill, spaceEvenly, padding 10 ]
        (List.repeat 2 <|
            el
                [ width (px 10)
                , height (px 25)
                , Background.color (rgb 0 0 0)
                ]
                none
        )


viewReplayControls : Model -> Element Msg
viewReplayControls model =
    let
        button =
            Input.button
                [ width (px tileSize)
                , height (px tileSize)
                , Border.color (rgb 0 0 0)
                , Border.width 2
                , Font.center
                ]

        playPauseButton =
            case model.mode of
                Editing _ ->
                    button { onPress = Just Replay, label = playIcon }

                Replaying _ ->
                    button { onPress = Just (Edit Empty), label = pauseIcon }
    in
    El.column
        [ spacing 10, alignTop ]
        [ playPauseButton ]


view : Model -> Html Msg
view model =
    El.layout [ width fill, height fill ]
        (El.column
            [ centerX, centerY, spacing 20 ]
            [ El.column
                [ spacing 10, padding 10, Font.bold, Font.size 25 ]
                [ text "Level Select:"
                , viewLevelSelect
                ]
            , El.paragraph
                [ Font.center, Font.bold, Font.size 40 ]
                [ text model.level.name ]
            , El.row
                [ padding 10, spacing 10 ]
                [ viewPalette
                , viewGrid model
                , viewSuccessIndicator model
                , viewReplayControls model
                ]
            , El.paragraph
                [ Font.center ]
                [ text model.level.description ]
            , El.el [] (viewBrush model)
            ]
        )



-- HELPERS


toPixels : Float -> String
toPixels value =
    String.fromFloat value ++ "px"


chunk : Int -> List a -> List (List a)
chunk i list =
    case List.take i list of
        [] ->
            []

        group ->
            group :: chunk i (List.drop i list)


squareUp : List a -> List (List a)
squareUp list =
    let
        getSize =
            round << sqrt << toFloat << List.length
    in
    chunk (getSize list) list


loadLevel : Level -> Model
loadLevel level =
    { mousePos = Position 0 0
    , mode = Editing Empty
    , grid = initBoard level.size
    , level = level
    , success = False
    }
