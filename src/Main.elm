module Main exposing (..)

import Array exposing (Array(..))
import Browser exposing (..)
import Browser.Events as BE
import Element as El exposing (..)
import Element.Background as Background
import Element.Border as Border
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
    , grid : List Tile
    }


type Position
    = Position Float Float


type Tile
    = Empty
    | Track
    | RGSplitter


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mousePos = Position 50 50
      , dragging = Empty
      , grid = List.repeat (5 * 5) Empty
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragging of
        Empty ->
            BE.onMouseDown <|
                D.map2 SetPosition
                    (D.field "pageX" D.float)
                    (D.field "pageY" D.float)

        _ ->
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
                        Empty

                    else
                        tile
              }
            , Cmd.none
            )



-- VIEW


toPixels : Float -> String
toPixels value =
    String.fromFloat value ++ "px"


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


viewTile : Tile -> Element Msg
viewTile tile =
    case tile of
        Empty ->
            none

        Track ->
            el
                [ centerX
                , centerY
                , width (px 20)
                , height (px 40)
                , Background.color (rgb 0 0 0)
                ]
                none

        RGSplitter ->
            el
                [ centerX
                , centerY
                , width (px 30)
                , height (px 30)
                , Border.rounded 20
                , Border.width 2
                , Border.color (rgb 0 1 0)
                ]
                none


viewBox : List (Attribute Msg) -> Tile -> Element Msg
viewBox attributes tile =
    el
        ([ width (px 50)
         , height (px 50)
         , Border.color (rgb 0 0 0)
         , Border.width 2
         ]
            ++ attributes
        )
        (viewTile tile)


viewPalette : Element Msg
viewPalette =
    El.column
        [ El.padding 10
        , El.spacing 10
        ]
        [ viewBox [ E.onClick (SetDragging Track) ] Track
        , viewBox [ E.onClick (SetDragging RGSplitter) ] RGSplitter
        ]


viewBrush : Model -> Element Msg
viewBrush model =
    let
        (Position xpos ypos) =
            model.mousePos
    in
    case model.dragging of
        Empty ->
            none

        tile ->
            el
                [ htmlAttribute <| HA.style "position" "absolute"
                , htmlAttribute <| HA.style "left" (toPixels (xpos - 25))
                , htmlAttribute <| HA.style "top" (toPixels (ypos - 25))
                , htmlAttribute <| HA.style "pointer-events" "none"
                ]
                (viewBox [] tile)


viewGrid : Model -> Element Msg
viewGrid model =
    El.column [] <|
        List.map (row []) (squareUp <| List.map (viewBox []) model.grid)


view : Model -> Html Msg
view model =
    El.layout []
        (El.row
            [ spacing 50 ]
            [ viewPalette
            , viewBrush model
            , viewGrid model
            ]
        )
