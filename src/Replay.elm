module Replay exposing (Model, Msg, init, panels, subscriptions, update)

import Array exposing (Array)
import Constants exposing (tileSize)
import Element as El exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as HA
import Level exposing (Code(..), Level, Outcome(..))
import Robot exposing (Progress(..), Robot)
import Tile exposing (Tile)
import Time


type alias Model =
    { grid : Array Tile
    , speed : Int
    , robots : List Robot
    }


init : Array Tile -> Level -> Model
init grid level =
    { grid = grid
    , speed = 1
    , robots = Robot.initAll level
    }



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
                        Finished Failed ->
                            ( { model | robots = [ robot ] }, Cmd.none )

                        Finished _ ->
                            ( { model | robots = rest }, Cmd.none )

                        Working bot ->
                            ( { model | robots = bot :: rest }, Cmd.none )

        IncreaseSpeed ->
            ( { model | speed = clamp 1 10 (model.speed + 1) }, Cmd.none )

        DecreaseSpeed ->
            ( { model | speed = clamp 1 10 (model.speed - 1) }, Cmd.none )

        Play ->
            ( { model | speed = 1 }, Cmd.none )

        Pause ->
            ( { model | speed = 0 }, Cmd.none )



--- VIEW ---


viewTooltip : String -> Element msg
viewTooltip tip =
    let
        toolTip =
            El.row
                [ centerY
                , spacing -2
                , mouseOver [ transparent True ]
                , htmlAttribute <| HA.style "pointer-events" "none"
                ]
                [ el
                    [ padding 5
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
    in
    el
        [ width fill
        , height fill
        , transparent True
        , mouseOver [ transparent False ]
        , onLeft toolTip
        ]
        none


viewBox : List (Attribute msg) -> Tile -> Element msg
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
        [ spacing 10, alignTop, alignRight ]
        [ control DecreaseSpeed slowDownIcon "Slower"
        , playPause
        , control IncreaseSpeed fastForwardIcon "Faster"
        ]


viewRobotInfoPane : Model -> Element msg
viewRobotInfoPane model =
    let
        block code =
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

        blocks =
            case model.robots of
                robot :: _ ->
                    List.map block robot.codes

                _ ->
                    []

        gridSize =
            tileSize * getSize model.grid
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
        blocks


viewLeftPanel : Model -> Element Msg
viewLeftPanel model =
    viewReplayControls model


viewRightPanel : Model -> Element msg
viewRightPanel model =
    viewRobotInfoPane model


viewGrid : Model -> Element msg
viewGrid model =
    let
        indicateFinished robot =
            case Robot.isFinished model.grid robot of
                Finished Failed ->
                    [ Border.color (rgb 1 0 0), Border.width 4 ]

                Finished (Passed _) ->
                    [ Border.color (rgb 0 1 0), Border.width 4 ]

                _ ->
                    []

        indicateBot robot index =
            if robot.position == index then
                inFront Robot.view :: indicateFinished robot

            else
                []

        viewGridCell index tile =
            case model.robots of
                [] ->
                    viewBox [] tile

                robot :: _ ->
                    viewBox (indicateBot robot index) tile
    in
    El.column
        []
        (model.grid
            |> Array.indexedMap viewGridCell
            |> Array.toList
            |> squareUp
            |> List.map (El.row [])
        )


panels :
    (Msg -> msg)
    -> Model
    -> { viewLeftPanel : Element msg, viewRightPanel : Element msg, viewGrid : Element msg }
panels mapMsg model =
    { viewLeftPanel = El.map mapMsg (viewLeftPanel model)
    , viewRightPanel = El.map mapMsg (viewRightPanel model)
    , viewGrid = El.map mapMsg (viewGrid model)
    }



--- HELPERS ---


getSize : Array a -> Int
getSize =
    round << sqrt << toFloat << Array.length


chunk : Int -> List a -> List (List a)
chunk i list =
    case List.take i list of
        [] ->
            []

        group ->
            group :: chunk i (List.drop i list)


squareUp : List a -> List (List a)
squareUp list =
    chunk (getSize <| Array.fromList list) list
