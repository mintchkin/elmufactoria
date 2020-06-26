module Main exposing (..)

import Browser exposing (..)
import Browser.Events exposing (onMouseMove)
import Element as El exposing (..)
import Element.Background as B
import Html.Attributes as HA
import Json.Decode as D


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { mousePos : Position
    }


type Position
    = Position Float Float


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mousePos = Position 0 0
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    onMouseMove <|
        D.map2 SetPosition
            (D.field "pageX" D.float)
            (D.field "pageY" D.float)



-- UPDATE


type Msg
    = SetPosition Float Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPosition x y ->
            ( { model | mousePos = Position x y }
            , Cmd.none
            )



-- VIEW


toPixels : Float -> String
toPixels value =
    String.fromFloat value ++ "px"


viewBox : Model -> Element msg
viewBox model =
    let
        (Position xPos yPos) =
            model.mousePos
    in
    el
        [ B.color (rgb 0 0 0)
        , width (px 50)
        , height (px 50)
        , htmlAttribute <| HA.style "position" "absolute"
        , htmlAttribute <| HA.style "left" (toPixels xPos)
        , htmlAttribute <| HA.style "top" (toPixels yPos)
        ]
        none


view : Model -> Document Msg
view model =
    { title = "Elmufactoria"
    , body =
        [ El.layout
            []
            (El.row
                []
                [ viewBox model
                ]
            )
        ]
    }
