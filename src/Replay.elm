module Replay exposing (Model, Msg, init, subscriptions, update, view)

import Array exposing (Array)
import Constants exposing (subHeadSize, tileSize)
import Element as El exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as HA
import Level exposing (Code(..), Level, Outcome(..))
import Robot exposing (Progress(..), Robot)
import Session
import Tile exposing (Tile)
import Time


type alias Model =
    { grid : Array Tile
    , level : Level
    , speed : Int
    , robots : List Robot
    }


init : Level -> ( Model, Cmd msg )
init level =
    ( { grid = Tile.initBoard level.size
      , level = level
      , speed = 1
      , robots = Robot.initAll level
      }
    , Session.request level
    )



--- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.speed > 0 then
        Time.every (1000 / toFloat model.speed) (\_ -> StepReplay)

    else
        Sub.none



--- UPDATE ---


type Msg
    = PressKey String
    | StepReplay
    | IncreaseSpeed
    | DecreaseSpeed
    | Play
    | Pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressKey _ ->
            ( model, Cmd.none )

        StepReplay ->
            case model.robots of
                [] ->
                    ( model, Cmd.none )

                robot :: rest ->
                    case Robot.advance model.grid robot of
                        Working newRobot ->
                            ( { model | robots = newRobot :: rest }, Cmd.none )

                        Finished _ ->
                            if Robot.checkBot model.level model.grid robot then
                                ( { model | robots = rest }, Cmd.none )

                            else
                                ( { model | robots = [ robot ] }, Cmd.none )

        IncreaseSpeed ->
            ( { model | speed = clamp 1 10 (model.speed + 1) }, Cmd.none )

        DecreaseSpeed ->
            ( { model | speed = clamp 1 10 (model.speed - 1) }, Cmd.none )

        Play ->
            ( { model | speed = 1 }, Cmd.none )

        Pause ->
            ( { model | speed = 0 }, Cmd.none )



--- VIEW ---


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


fastForwardIcon : Element msg
fastForwardIcon =
    el
        [ centerX
        , centerY
        , Font.size 20
        , Font.letterSpacing -3
        ]
        (text "▶▶")


slowDownIcon : Element msg
slowDownIcon =
    el
        [ centerX
        , centerY
        , Font.size 20
        , Font.letterSpacing -3
        ]
        (text "◀◀")


viewReplayControls : Model -> Element Msg
viewReplayControls model =
    let
        control msg icon tip =
            Input.button
                [ width (px tileSize)
                , height (px tileSize)
                , Border.color (rgb 0 0 0)
                , Border.width 2
                , Font.center
                , inFront <| viewTooltip tip
                ]
                { onPress = Just msg
                , label = icon
                }

        playPause =
            if model.speed == 0 then
                control Play playIcon "Play"

            else
                control Pause pauseIcon "Pause"
    in
    El.column
        [ alignRight, alignTop, spacing 10 ]
        [ control DecreaseSpeed slowDownIcon "Slower"
        , playPause
        , control IncreaseSpeed fastForwardIcon "Faster"
        ]


viewCodeDot : Code -> Element msg
viewCodeDot code =
    let
        circle color =
            el
                [ Background.color color
                , Border.rounded tileSize
                , width (px <| tileSize - 20)
                , height (px <| tileSize - 20)
                , centerX
                ]
                none
    in
    case code of
        Blue ->
            circle (rgb 0 0 1)

        Red ->
            circle (rgb 1 0 0)


viewRobotInfoPane : Model -> Element msg
viewRobotInfoPane model =
    let
        dots =
            case model.robots of
                robot :: _ ->
                    List.map viewCodeDot robot.codes

                _ ->
                    []

        gridSize =
            tileSize * (model.level.size * 2 - 1)
    in
    El.column
        [ width (px tileSize)
        , height (fill |> maximum gridSize)
        , clipY
        , spacing 5
        , padding 5
        , Border.color (rgb 0 0 0)
        , Border.width 2
        , inFront <|
            el
                [ Background.gradient { angle = 0, steps = [ rgba 1 1 1 1, rgba 1 1 1 0 ] }
                , width fill
                , height (px 20)
                , alignBottom
                ]
                none
        ]
        dots


viewGrid : Model -> Element msg
viewGrid model =
    let
        indicateFinished robot =
            case Robot.advance model.grid robot of
                Finished _ ->
                    if Robot.checkBot model.level model.grid robot then
                        [ Border.color (rgb 0 1 0), Border.width 4 ]

                    else
                        [ Border.color (rgb 1 0 0), Border.width 4 ]

                Working _ ->
                    []

        indicateBot robot index =
            if robot.position == index then
                inFront Robot.view :: indicateFinished robot

            else
                []
    in
    Tile.viewGrid
        (\index ->
            case model.robots of
                robot :: _ ->
                    indicateBot robot index

                [] ->
                    []
        )
        model.grid


toEditButton : msg -> Element msg
toEditButton toReplay =
    let
        label =
            el [ centerX, centerY, Font.size subHeadSize, Font.bold ] (text "Test Solution")
    in
    Input.button
        [ width fill
        , height (px tileSize)
        , Border.color (rgb 0 0 0)
        , Border.width 2
        , Font.center
        ]
        { onPress = Just toReplay
        , label = label
        }


view : msg -> (Msg -> msg) -> Model -> Element msg
view toEdit mapMsg model =
    let
        leftPanel =
            El.map mapMsg <|
                el [ height fill, width fill ]
                    (viewReplayControls model)

        main =
            El.column [ centerX, spacing 10 ]
                [ El.map mapMsg (viewGrid model)
                , toEditButton toEdit
                ]

        rightPanel =
            El.map mapMsg <|
                el [ width fill, height fill ]
                    (viewRobotInfoPane model)
    in
    El.row
        [ width fill, spacing 10 ]
        [ leftPanel, main, rightPanel ]



--- TOOLTIP ---


leftToolTip : String -> Attribute msg
leftToolTip tip =
    onLeft <|
        El.row
            [ centerY
            , spacing -2
            , mouseOver [ transparent True ]
            , htmlAttribute <| HA.style "pointer-events" "none"
            ]
            [ el
                [ padding 5
                , Background.color (rgb 1 1 1)
                , Border.color (rgb 0 0 0)
                , Border.widthEach { top = 2, right = 0, bottom = 2, left = 2 }
                ]
                (text tip)
            , el
                [ centerY
                , moveLeft 10
                , width (px 24)
                , height (px 24)
                , Border.color (rgb 0 0 0)
                , Border.widthEach
                    { top = 2
                    , right = 2
                    , bottom = 0
                    , left = 0
                    }
                , rotate (degrees 45)
                ]
                none
            ]


viewTooltip : String -> Element msg
viewTooltip tip =
    el
        [ width fill
        , height fill
        , transparent True
        , mouseOver [ transparent False ]
        , leftToolTip tip
        ]
        none
