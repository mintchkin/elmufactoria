module Layout exposing (attachTooltip)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html.Attributes as HA



--- TOOLTIP ---


leftToolTip : String -> Attribute msg
leftToolTip tip =
    onLeft <|
        row
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


attachTooltip : String -> Attribute msg
attachTooltip tip =
    inFront <|
        el
            [ width fill
            , height fill
            , transparent True
            , mouseOver [ transparent False ]
            , leftToolTip tip
            ]
            none
