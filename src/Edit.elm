module Edit exposing (..)

import Array exposing (Array)
import Bitwise as Bit
import Browser.Events as BE
import Constants exposing (subHeadSize, tileSize)
import Direction exposing (Direction(..), Rotation(..))
import Element as El exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as E
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Json
import Layout
import Level exposing (Code(..), Level)
import Robot
import Session
import Tile exposing (Tile(..))



--- MODEL ---


type alias Model =
    { mousePos : Position
    , level : Level
    , grid : Array Tile
    , brush : Tile
    , indicator : Indicator
    }


type Indicator
    = Default
    | Thinking Float
    | Result Float


type Position
    = Position Float Float


init : Level -> ( Model, Cmd msg )
init level =
    ( { mousePos = Position 0 0
      , level = level
      , grid = Tile.initBoard level.size
      , brush = Empty
      , indicator = Default
      }
    , Session.request level
    )



--- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        updatePosition =
            Json.map2 SetPosition
                (Json.field "clientX" Json.float)
                (Json.field "clientY" Json.float)

        updateIndicator =
            case model.indicator of
                Default ->
                    Sub.none

                _ ->
                    BE.onAnimationFrameDelta TickIndicator
    in
    Sub.batch
        [ BE.onMouseMove updatePosition
        , BE.onMouseDown updatePosition
        , BE.onKeyDown <| Json.map PressKey (Json.field "key" Json.string)
        , updateIndicator
        ]



--- UPDATE ---


type Msg
    = SetPosition Float Float
    | PressKey String
    | SetGridTile Int
    | SetBrush Tile
    | TickIndicator Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPosition x y ->
            ( { model | mousePos = Position x y }, Cmd.none )

        PressKey key ->
            case ( key, Direction.fromKey key, Tile.fromKey key ) of
                ( "Esc", _, _ ) ->
                    ( { model | brush = Empty }, Cmd.none )

                ( "Escape", _, _ ) ->
                    ( { model | brush = Empty }, Cmd.none )

                ( " ", _, _ ) ->
                    ( { model | brush = Tile.invert model.brush }, Cmd.none )

                ( _, Just direction, _ ) ->
                    ( { model | brush = Tile.direct direction model.brush }, Cmd.none )

                ( _, _, Just tile ) ->
                    ( { model | brush = Tile.matchDirection model.brush tile }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetGridTile index ->
            let
                replacement =
                    case ( Array.get index model.grid, model.brush ) of
                        ( Just Begin, _ ) ->
                            Begin

                        ( Just End, _ ) ->
                            End

                        ( Just (Track fst), Track snd ) ->
                            if fst == snd || fst == Direction.reverse snd then
                                Track snd

                            else if snd == Direction.rotate Clockwise fst then
                                MultiTrack fst Clockwise

                            else
                                MultiTrack fst CounterClock

                        ( Just (MultiTrack first rotation), Track direction ) ->
                            let
                                second =
                                    Direction.rotate rotation first

                                reversed =
                                    Direction.reverse direction
                            in
                            case ( first == reversed, second == reversed, rotation ) of
                                ( True, _, Clockwise ) ->
                                    MultiTrack direction CounterClock

                                ( True, _, CounterClock ) ->
                                    MultiTrack direction Clockwise

                                ( _, True, Clockwise ) ->
                                    MultiTrack direction Clockwise

                                ( _, True, CounterClock ) ->
                                    MultiTrack direction CounterClock

                                _ ->
                                    Track direction

                        _ ->
                            model.brush

                newGrid =
                    Array.set index replacement model.grid
            in
            ( { model | grid = newGrid, indicator = Thinking 3000 }, Session.save model.level newGrid )

        SetBrush tile ->
            ( { model | brush = tile }, Cmd.none )

        TickIndicator delta ->
            case model.indicator of
                Thinking remaining ->
                    if remaining - delta <= 0 then
                        ( { model | indicator = Result 3000 }, Cmd.none )

                    else
                        ( { model | indicator = Thinking (remaining - delta) }, Cmd.none )

                Result remaining ->
                    if remaining - delta <= 0 then
                        ( { model | indicator = Default }, Cmd.none )

                    else
                        ( { model | indicator = Result (remaining - delta) }, Cmd.none )

                Default ->
                    ( model, Cmd.none )



--- VIEW ---


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


viewPalette : Element Msg
viewPalette =
    let
        swatch tip tile =
            viewBox
                [ E.onClick (SetBrush tile)
                , Layout.attachTooltip tip
                ]
                tile
    in
    El.row [ spacing 10, alignRight, alignTop ]
        [ El.column [ spacing 10, alignRight, alignTop ]
            [ swatch "Eraser" Empty
            , swatch "G/Y Splitter" (Splitter Green Down)
            , swatch "G/Y Writer" (Writer Yellow Down)
            ]
        , El.column [ spacing 10, alignRight, alignTop ]
            [ swatch "Track" (Track Down)
            , swatch "R/B Splitter" (Splitter Red Down)
            , swatch "R/B Writer" (Writer Blue Down)
            ]
        ]


viewBrush : Model -> Element msg
viewBrush model =
    let
        (Position xpos ypos) =
            model.mousePos
    in
    case model.brush of
        Empty ->
            none

        tile ->
            el
                [ htmlAttribute <| HA.style "position" "fixed"
                , htmlAttribute <| HA.style "left" (toPixels (xpos - (tileSize / 2)))
                , htmlAttribute <| HA.style "top" (toPixels (ypos - (tileSize / 2)))
                , htmlAttribute <| HA.style "pointer-events" "none"
                ]
                (viewBox [] tile)


viewGrid : Model -> Element Msg
viewGrid model =
    Tile.viewGrid
        (\index ->
            [ E.onMouseDown (SetGridTile index)
            , onDraggingMouseEnter (SetGridTile index)
            ]
        )
        model.grid


toReplayButton : msg -> Model -> Element msg
toReplayButton toReplay model =
    let
        isCorrect =
            Robot.checkSolution model.level model.grid

        label =
            case ( isCorrect, model.indicator ) of
                ( _, Default ) ->
                    el [ centerX, centerY, Font.size subHeadSize, Font.bold ] (text "Release Robots")

                ( _, Thinking ticks ) ->
                    if (round ticks |> modBy 1000) > 500 then
                        el [ centerX, centerY, moveRight 10 ] Robot.view

                    else
                        el [ centerX, centerY, moveLeft 10 ] Robot.view

                ( True, Result _ ) ->
                    el [ centerX, centerY, Font.size subHeadSize, Font.bold ] (text "✔️")

                ( False, Result _ ) ->
                    el [ centerX, centerY, Font.size subHeadSize, Font.bold ] (text "❌")

        background =
            case ( isCorrect, model.indicator ) of
                ( _, Thinking _ ) ->
                    Background.color (rgb 1 1 1)

                ( True, _ ) ->
                    Background.color (rgba 0 1 0 0.1)

                ( False, _ ) ->
                    Background.color (rgba 1 0 0 0.1)
    in
    Input.button
        [ width fill
        , height (px tileSize)
        , Border.color (rgb 0 0 0)
        , Border.width 2
        , Font.center
        , background
        ]
        { onPress = Just toReplay
        , label = label
        }


view : msg -> (Msg -> msg) -> Model -> Element msg
view toReplay mapMsg model =
    let
        panelWidth =
            width (fill |> minimum (tileSize * 2 + 5))

        leftPanel =
            El.map mapMsg <|
                el [ panelWidth, height fill ] viewPalette

        main =
            El.column [ centerX, spacing 10 ]
                [ El.map mapMsg (viewGrid model)
                , toReplayButton toReplay model
                ]

        rightPanel =
            el [ panelWidth, height fill ] none
    in
    El.row
        [ width fill, spacing 10, inFront <| viewBrush model ]
        [ leftPanel, main, rightPanel ]



--- HELPERS ---


toPixels : Float -> String
toPixels value =
    String.fromFloat value ++ "px"


onDraggingMouseEnter : msg -> Attribute msg
onDraggingMouseEnter msg =
    let
        ifDragging m buttons =
            if Bit.and 1 buttons == 1 then
                Json.succeed m

            else
                Json.fail "Not dragging"
    in
    htmlAttribute <|
        HE.on "mouseenter"
            (Json.field "buttons" Json.int |> Json.andThen (ifDragging msg))
