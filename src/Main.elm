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
    , grid : Array Tile
    }


type Position
    = Position Float Float


type Tile
    = Empty
    | Track Direction
    | RBSplitter


type Direction
    = Up
    | Right
    | Down
    | Left


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mousePos = Position 50 50
      , dragging = Empty
      , grid = Array.repeat (7 * 7) Empty
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
            Sub.batch
                [ BE.onMouseMove <|
                    D.map2 SetPosition
                        (D.field "pageX" D.float)
                        (D.field "pageY" D.float)
                , BE.onKeyDown <|
                    D.map
                        (SetDirection << toDirection)
                        (D.field "key" D.string)
                ]



-- UPDATE


type Msg
    = SetPosition Float Float
    | SetDragging Tile
    | SetGridTile Int
    | SetDirection (Maybe Direction)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPosition x y ->
            ( { model | mousePos = Position x y }
            , Cmd.none
            )

        SetDragging tile ->
            ( { model | dragging = tile }
            , Cmd.none
            )

        SetGridTile index ->
            ( { model
                | grid = Array.set index model.dragging model.grid
              }
            , Cmd.none
            )

        SetDirection maybeDirection ->
            let
                justDirection default =
                    Maybe.withDefault default maybeDirection
            in
            case model.dragging of
                Track direction ->
                    ( { model | dragging = Track (justDirection direction) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- VIEW


viewTile : Tile -> Element Msg
viewTile tile =
    case tile of
        Empty ->
            El.none

        Track direction ->
            El.row
                [ width fill
                , height fill
                , rotate (toRotation direction)
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

        RBSplitter ->
            let
                indicator attrs =
                    el ([ width (px 10), height (px 10) ] ++ attrs) none
            in
            El.row
                [ width fill
                , height fill
                ]
                [ indicator [ Background.color (rgb 1 0 0), alignLeft ]
                , indicator [ Background.color (rgb 0.3 0.3 0.3), centerX, alignBottom ]
                , indicator [ Background.color (rgb 0 0 1), alignRight ]
                ]


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
        , El.alignTop
        ]
        [ viewBox [ E.onClick (SetDragging (Track Down)) ] (Track Down)
        , viewBox [ E.onClick (SetDragging RBSplitter) ] RBSplitter
        , viewBox [ E.onClick (SetDragging Empty) ] Empty
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
                [ htmlAttribute <| HA.style "position" "fixed"
                , htmlAttribute <| HA.style "left" (toPixels (xpos - 25))
                , htmlAttribute <| HA.style "top" (toPixels (ypos - 25))
                , htmlAttribute <| HA.style "pointer-events" "none"
                ]
                (viewBox [] tile)


viewGridRow : List ( Int, Tile ) -> Element Msg
viewGridRow indexedTiles =
    let
        viewCell ( index, tile ) =
            viewBox [ E.onClick (SetGridTile index) ] tile
    in
    indexedTiles
        |> List.map viewCell
        |> row []


viewGrid : Model -> Element Msg
viewGrid model =
    El.column
        [ El.padding 10 ]
        (model.grid
            |> Array.toIndexedList
            |> squareUp
            |> List.map viewGridRow
        )


view : Model -> Html Msg
view model =
    El.layout []
        (El.row
            []
            [ viewPalette
            , viewGrid model
            , viewBrush model
            ]
        )



-- HELPERS


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


toRotation : Direction -> Float
toRotation direction =
    case direction of
        Down ->
            turns 0

        Left ->
            turns 0.25

        Up ->
            turns 0.5

        Right ->
            turns 0.75


toDirection : String -> Maybe Direction
toDirection key =
    case String.uncons key of
        Just ( 'w', "" ) ->
            Just Up

        Just ( 'd', "" ) ->
            Just Right

        Just ( 's', "" ) ->
            Just Down

        Just ( 'a', "" ) ->
            Just Left

        _ ->
            Nothing
