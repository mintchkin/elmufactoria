module Tile exposing (..)

import Direction exposing (Direction(..))
import Element as El exposing (..)
import Element.Background as Background


type Tile
    = Empty
    | Track Direction
    | RBSplitter Direction
    | BRSplitter Direction
    | Begin
    | End


viewSplitter : El.Color -> El.Color -> Direction -> Element msg
viewSplitter left right direction =
    let
        indicator attrs =
            el ([ width (px 10), height (px 10) ] ++ attrs) none
    in
    El.row
        [ width fill
        , height fill
        , rotate (Direction.toRotation direction)
        ]
        [ indicator [ Background.color left, alignLeft ]
        , indicator [ Background.color (rgb 0.3 0.3 0.3), centerX, alignBottom ]
        , indicator [ Background.color right, alignRight ]
        ]


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
            El.row
                [ width fill
                , height fill
                , rotate (Direction.toRotation direction)
                ]
                [ el
                    [ centerX
                    , alignBottom
                    , width (px 20)
                    , height (px 40)
                    , Background.color (rgb 0 0 0)
                    ]
                    none
                ]

        RBSplitter direction ->
            viewSplitter red blue direction

        BRSplitter direction ->
            viewSplitter blue red direction



--- CONSTANTS ---


blue : El.Color
blue =
    rgb 0 0 1


red : El.Color
red =
    rgb 1 0 0



--- HELPERS ---


direct : Direction -> Tile -> Tile
direct direction tile =
    case tile of
        Track _ ->
            Track direction

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
