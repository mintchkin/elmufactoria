module Layout exposing (attachTooltip)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html.Attributes as HA



--- TOOLTIP ---


leftToolTip : String -> Attribute msg
leftToolTip tip =
    let
        disablePointerEvents =
            htmlAttribute <| HA.style "pointer-events" "none"
    in
    onLeft <|
        el
            [ centerY
            , disablePointerEvents
            , mouseOver [ transparent True ]
            , moveLeft 20
            , padding 5
            , Background.color (rgb 1 1 1)
            , Border.color (rgb 0 0 0)
            , Border.widthEach { top = 2, right = 0, bottom = 2, left = 2 }
            , behindContent <|
                el
                    [ alignRight
                    , centerY
                    , disablePointerEvents
                    , height (px 24)
                    , moveRight 12
                    , rotate (degrees 45)
                    , width (px 24)
                    , Background.color (rgb 1 1 1)
                    , Border.color (rgb 0 0 0)
                    , Border.widthEach { top = 2, right = 2, bottom = 0, left = 0 }
                    ]
                    none
            ]
            (text tip)


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
