module Main exposing (..)

import Browser exposing (..)
import Browser.Events as BE
import Element as El exposing (..)
import Element.Background as B
import Element.Events as E
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as D


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { mousePos : Position
    , dragging : Tile
    }


type Position
    = Position Float Float


type Tile
    = Nothing
    | Box


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mousePos = Position 50 50
      , dragging = Nothing
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragging of
        Nothing ->
            Sub.none

        Box ->
            BE.onMouseMove <|
                D.map2 SetPosition
                    (D.field "pageX" D.float)
                    (D.field "pageY" D.float)



-- UPDATE


type Msg
    = SetPosition Float Float
    | SetDragging Tile


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPosition x y ->
            ( { model | mousePos = Position x y }
            , Cmd.none
            )

        SetDragging tile ->
            ( { model
                | dragging =
                    if model.dragging == tile then
                        Nothing

                    else
                        tile
              }
            , Cmd.none
            )



-- VIEW


toPixels : Float -> String
toPixels value =
    String.fromFloat value ++ "px"


viewBox : Model -> Element Msg
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
        , htmlAttribute <| HA.style "left" (toPixels (xPos - 25))
        , htmlAttribute <| HA.style "top" (toPixels (yPos - 25))
        , E.onClick (SetDragging Box)
        ]
        none


view : Model -> Html Msg
view model =
    El.layout
        []
        (El.row
            []
            [ viewBox model
            ]
        )
