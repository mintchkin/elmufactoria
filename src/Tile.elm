module Tile exposing
    ( Tile(..)
    , direct
    , fromJson
    , fromKey
    , getDirection
    , initBoard
    , invert
    , matchDirection
    , toJson
    , view
    , viewGrid
    )

import Array exposing (Array)
import Constants exposing (tileSize)
import Direction exposing (Direction(..), Rotation(..))
import Element as El exposing (..)
import Element.Background as Background
import Element.Border as Border
import Json.Decode as JsonD
import Json.Encode as JsonE
import Level exposing (Code(..), codeColor, invertCode)


type Tile
    = Empty
    | Track Direction
    | MultiTrack Direction Rotation
    | Splitter Code Direction
    | Writer Code Direction
    | Begin
    | End



--- VIEW ---


viewSplitter : Code -> Direction -> Element msg
viewSplitter left direction =
    let
        right =
            invertCode left

        indicator attrs =
            el ([ width (px 12), height (px 12), Border.width 1, Border.color (rgba 0 0 0 0.3) ] ++ attrs) none
    in
    El.row
        [ width fill
        , height fill
        , rotate (Direction.toAngle direction)
        ]
        [ indicator [ Background.color (codeColor left), alignLeft ]
        , indicator [ Background.color (rgb 0.3 0.3 0.3), centerX, alignBottom ]
        , indicator [ Background.color (codeColor right), alignRight ]
        ]


viewTrack : Direction -> Element msg
viewTrack direction =
    el
        [ width fill, height fill, rotate (Direction.toAngle direction) ]
        (el
            [ centerX
            , alignBottom
            , width (px 20)
            , height (px 40)
            , Background.color (rgb 0 0 0)
            ]
            none
        )


viewWriter : Code -> Direction -> Element msg
viewWriter code direction =
    let
        arrow =
            el
                [ Border.widthEach
                    { top = 0
                    , right = 4
                    , bottom = 4
                    , left = 0
                    }
                , width (px 15)
                , height (px 15)
                , centerX
                , centerY
                , rotate (degrees 45)
                ]
                none
    in
    column
        [ width fill, height fill, spacing -5, rotate (Direction.toAngle direction) ]
        [ el
            [ Background.color (codeColor code)
            , Border.rounded tileSize
            , Border.width 2
            , width (px <| tileSize - 20)
            , height (px <| tileSize - 20)
            , centerX
            , centerY
            ]
            none
        , arrow
        ]


view : Tile -> Element msg
view tile =
    case tile of
        Empty ->
            El.none

        Begin ->
            El.el [ width fill, height fill, Background.color red ] none

        End ->
            El.el [ width fill, height fill, Background.color blue ] none

        Track direction ->
            viewTrack direction

        MultiTrack fst snd ->
            el
                [ width fill
                , height fill
                , inFront <| viewTrack (Direction.rotate snd fst)
                ]
                (viewTrack fst)

        Splitter left direction ->
            viewSplitter left direction

        Writer code direction ->
            viewWriter code direction


viewGrid : (Int -> List (Attribute msg)) -> Array Tile -> Element msg
viewGrid getCellAttrs grid =
    let
        viewCell index tile =
            el
                ([ width (px tileSize)
                 , height (px tileSize)
                 , Border.color (rgb 0 0 0)
                 , Border.width 2
                 ]
                    ++ getCellAttrs index
                )
                (view tile)
    in
    El.column []
        (Array.toList grid
            |> List.indexedMap viewCell
            |> squareUp
            |> List.map (El.row [])
        )



--- API ---


direct : Direction -> Tile -> Tile
direct direction tile =
    case tile of
        Track _ ->
            Track direction

        MultiTrack _ rotation ->
            MultiTrack direction rotation

        Splitter code _ ->
            Splitter code direction

        Writer code _ ->
            Writer code direction

        other ->
            other


getDirection : Tile -> Maybe Direction
getDirection tile =
    case tile of
        Track direction ->
            Just direction

        MultiTrack direction _ ->
            Just direction

        Splitter _ direction ->
            Just direction

        Writer _ direction ->
            Just direction

        _ ->
            Nothing


matchDirection : Tile -> Tile -> Tile
matchDirection from target =
    case getDirection from of
        Just direction ->
            direct direction target

        _ ->
            target


invert : Tile -> Tile
invert tile =
    case tile of
        Splitter code direction ->
            Splitter (invertCode code) direction

        Writer code direction ->
            Writer (invertCode code) direction

        other ->
            other


fromKey : String -> Maybe Tile
fromKey key =
    case key of
        "1" ->
            Just (Track Down)

        "2" ->
            Just (Splitter Red Down)

        "3" ->
            Just (Writer Blue Down)

        _ ->
            Nothing


toJson : Tile -> JsonE.Value
toJson tile =
    let
        dirEncoder direction =
            case direction of
                Up ->
                    JsonE.string "Up"

                Down ->
                    JsonE.string "Down"

                Left ->
                    JsonE.string "Left"

                Right ->
                    JsonE.string "Right"

        rotEncoder rotation =
            case rotation of
                Clockwise ->
                    JsonE.string "Clockwise"

                CounterClock ->
                    JsonE.string "CounterClock"

        codeEncoder code =
            case code of
                Blue ->
                    JsonE.string "Blue"

                Red ->
                    JsonE.string "Red"

                Green ->
                    JsonE.string "Green"

                Yellow ->
                    JsonE.string "Yellow"
    in
    case tile of
        Empty ->
            JsonE.object
                [ ( "tile", JsonE.string "Empty" )
                ]

        Track dir ->
            JsonE.object
                [ ( "tile", JsonE.string "Track" )
                , ( "dir", dirEncoder dir )
                ]

        MultiTrack dir rot ->
            JsonE.object
                [ ( "tile", JsonE.string "MultiTrack" )
                , ( "dir", dirEncoder dir )
                , ( "rot", rotEncoder rot )
                ]

        Splitter code dir ->
            JsonE.object
                [ ( "tile", JsonE.string "Splitter" )
                , ( "dir", dirEncoder dir )
                , ( "code", codeEncoder code )
                ]

        Writer code dir ->
            JsonE.object
                [ ( "tile", JsonE.string "Writer" )
                , ( "dir", dirEncoder dir )
                , ( "code", codeEncoder code )
                ]

        Begin ->
            JsonE.object
                [ ( "tile", JsonE.string "Begin" )
                ]

        End ->
            JsonE.object
                [ ( "tile", JsonE.string "End" )
                ]


fromJson : JsonD.Decoder Tile
fromJson =
    let
        dirDecoder =
            let
                decode name =
                    case name of
                        "Up" ->
                            JsonD.succeed Up

                        "Down" ->
                            JsonD.succeed Down

                        "Left" ->
                            JsonD.succeed Left

                        "Right" ->
                            JsonD.succeed Right

                        _ ->
                            JsonD.fail "Could not decode direction"
            in
            JsonD.string |> JsonD.andThen decode

        rotDecoder =
            let
                decode name =
                    case name of
                        "Clockwise" ->
                            JsonD.succeed Clockwise

                        "CounterClock" ->
                            JsonD.succeed CounterClock

                        _ ->
                            JsonD.fail "Could not decode rotation"
            in
            JsonD.string |> JsonD.andThen decode

        codeDecoder =
            let
                decode name =
                    case name of
                        "Red" ->
                            JsonD.succeed Red

                        "Blue" ->
                            JsonD.succeed Blue

                        "Green" ->
                            JsonD.succeed Green

                        "Yellow" ->
                            JsonD.succeed Yellow

                        _ ->
                            JsonD.fail "Could not decode code"
            in
            JsonD.string |> JsonD.andThen decode

        decodeTile name =
            case name of
                "Empty" ->
                    JsonD.succeed Empty

                "Track" ->
                    JsonD.map Track
                        (JsonD.field "dir" dirDecoder)

                "MultiTrack" ->
                    JsonD.map2 MultiTrack
                        (JsonD.field "dir" dirDecoder)
                        (JsonD.field "rot" rotDecoder)

                "Splitter" ->
                    JsonD.map2 Splitter
                        (JsonD.field "code" codeDecoder)
                        (JsonD.field "dir" dirDecoder)

                "Writer" ->
                    JsonD.map2 Writer
                        (JsonD.field "code" codeDecoder)
                        (JsonD.field "dir" dirDecoder)

                "Begin" ->
                    JsonD.succeed Begin

                "End" ->
                    JsonD.succeed End

                _ ->
                    JsonD.fail "Could not decode tile"
    in
    JsonD.field "tile" JsonD.string
        |> JsonD.andThen decodeTile


initBoard : Int -> Array Tile
initBoard size =
    let
        width =
            size * 2 - 1
    in
    Array.repeat (width ^ 2) Empty
        |> Array.set (size - 1) Begin
        |> Array.set (width ^ 2 - size) End



--- HELPERS ---


blue : El.Color
blue =
    rgb 0 0 1


red : El.Color
red =
    rgb 1 0 0


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
