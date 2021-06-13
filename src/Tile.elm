module Tile exposing
    ( Tile(..)
    , direct
    , fromJson
    , fromKey
    , getDirection
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


type Tile
    = Empty
    | Track Direction
    | MultiTrack Direction Rotation
    | RBSplitter Direction
    | BRSplitter Direction
    | Begin
    | End



--- VIEW ---


viewSplitter : El.Color -> El.Color -> Direction -> Element msg
viewSplitter left right direction =
    let
        indicator attrs =
            el ([ width (px 10), height (px 10) ] ++ attrs) none
    in
    El.row
        [ width fill
        , height fill
        , rotate (Direction.toAngle direction)
        ]
        [ indicator [ Background.color left, alignLeft ]
        , indicator [ Background.color (rgb 0.3 0.3 0.3), centerX, alignBottom ]
        , indicator [ Background.color right, alignRight ]
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


view : Tile -> Element msg
view tile =
    case tile of
        Empty ->
            El.none

        Begin ->
            El.el [ width fill, height fill, Background.color (rgb 1 0 0) ] none

        End ->
            El.el [ width fill, height fill, Background.color (rgb 0 0 1) ] none

        Track direction ->
            viewTrack direction

        MultiTrack fst snd ->
            el
                [ width fill
                , height fill
                , inFront <| viewTrack (Direction.rotate snd fst)
                ]
                (viewTrack fst)

        RBSplitter direction ->
            viewSplitter red blue direction

        BRSplitter direction ->
            viewSplitter blue red direction


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

        RBSplitter _ ->
            RBSplitter direction

        BRSplitter _ ->
            BRSplitter direction

        other ->
            other


getDirection : Tile -> Maybe Direction
getDirection tile =
    case tile of
        Track direction ->
            Just direction

        MultiTrack direction _ ->
            Just direction

        RBSplitter direction ->
            Just direction

        BRSplitter direction ->
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
        RBSplitter direction ->
            BRSplitter direction

        BRSplitter direction ->
            RBSplitter direction

        other ->
            other


fromKey : String -> Maybe Tile
fromKey key =
    case key of
        "1" ->
            Just (Track Down)

        "2" ->
            Just (RBSplitter Down)

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

        RBSplitter dir ->
            JsonE.object
                [ ( "tile", JsonE.string "RBSplitter" )
                , ( "dir", dirEncoder dir )
                ]

        BRSplitter dir ->
            JsonE.object
                [ ( "tile", JsonE.string "BRSplitter" )
                , ( "dir", dirEncoder dir )
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

                "RBSplitter" ->
                    JsonD.map RBSplitter
                        (JsonD.field "dir" dirDecoder)

                "BRSplitter" ->
                    JsonD.map BRSplitter
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
