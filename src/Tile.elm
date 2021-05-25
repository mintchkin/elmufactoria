module Tile exposing (..)

import Direction exposing (Direction)
import Element as El exposing (..)
import Element.Background as Background


type Tile
    = Empty
    | Track Direction
    | RBSplitter Direction
    | Begin
    | End


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
            let
                indicator attrs =
                    el ([ width (px 10), height (px 10) ] ++ attrs) none
            in
            El.row
                [ width fill
                , height fill
                , rotate (Direction.toRotation direction)
                ]
                [ indicator [ Background.color (rgb 1 0 0), alignLeft ]
                , indicator [ Background.color (rgb 0.3 0.3 0.3), centerX, alignBottom ]
                , indicator [ Background.color (rgb 0 0 1), alignRight ]
                ]
