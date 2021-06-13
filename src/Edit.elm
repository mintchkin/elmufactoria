module Edit exposing (..)

import Array exposing (Array)
import Bitwise as Bit
import Browser.Events as BE
import Constants exposing (tileSize)
import Direction exposing (Direction(..), Rotation(..))
import Element as El exposing (..)
import Element.Border as Border
import Element.Events as E
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Json
import Tile exposing (Tile(..))



--- MODEL ---


type alias Model =
    { mousePos : Position
    , grid : Array Tile
    , brush : Tile
    }


type Position
    = Position Float Float


init : Array Tile -> Model
init grid =
    { mousePos = Position 0 0
    , grid = grid
    , brush = Empty
    }



--- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        updatePosition =
            Json.map2 SetPosition
                (Json.field "clientX" Json.float)
                (Json.field "clientY" Json.float)

        onMouseMove =
            case model.brush of
                Empty ->
                    Sub.none

                _ ->
                    BE.onMouseMove updatePosition
    in
    Sub.batch
        [ onMouseMove
        , BE.onMouseDown updatePosition
        , BE.onKeyDown <|
            Json.map PressKey (Json.field "key" Json.string)
        ]



--- UPDATE ---


type Msg
    = SetPosition Float Float
    | PressKey String
    | SetGridTile Int
    | SetBrush Tile


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
            in
            ( { model | grid = Array.set index replacement model.grid }, Cmd.none )

        SetBrush tile ->
            ( { model | brush = tile }, Cmd.none )



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


viewPalette : Element Msg
viewPalette =
    let
        swatch tip tile =
            viewBox
                [ E.onClick (SetBrush tile)
                , inFront (viewTooltip tip)
                ]
                tile
    in
    El.column
        [ spacing 10, alignTop ]
        [ swatch "Track" (Track Down)
        , swatch "R/B Splitter" (RBSplitter Down)
        , swatch "Eraser" Empty
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


viewLeftPanel : Model -> Element Msg
viewLeftPanel _ =
    el [ alignRight ] viewPalette


viewRightPanel : Model -> Element Msg
viewRightPanel _ =
    El.none


viewGrid : Model -> Element Msg
viewGrid model =
    let
        viewGridCell index tile =
            viewBox
                [ E.onMouseDown (SetGridTile index)
                , onDraggingMouseEnter (SetGridTile index)
                ]
                tile
    in
    El.column
        [ inFront (viewBrush model) ]
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
