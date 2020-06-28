module Main exposing (..)

import Array exposing (Array(..))
import Browser exposing (..)
import Browser.Events as BE
import Element as El exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as E
import Element.Font as Font
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


levels : List Level
levels =
    [ { name = "Starter Level"
      , description = "Allow all robots to get safely from the start to the end"
      , size = 7
      , criteria = always True
      }
    ]



-- MODEL


type alias Model =
    { mousePos : Position
    , dragging : Tile
    , grid : Array Tile
    , level : Level
    , success : Bool
    }


type alias Level =
    { name : String
    , description : String
    , size : Int
    , criteria : Criteria
    }


type Position
    = Position Float Float


type Tile
    = Empty
    | Track Direction
    | RBSplitter Direction
    | Begin
    | End


type Direction
    = Up
    | Right
    | Down
    | Left


initBoard : Int -> Array Tile
initBoard size =
    let
        begin =
            round (toFloat size / 2) - 1

        end =
            (size ^ 2) - begin - 1
    in
    Array.repeat (size ^ 2) Empty
        |> Array.set begin Begin
        |> Array.set end End


init : () -> ( Model, Cmd Msg )
init _ =
    ( loadLevel <| List.head levels
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
            case Array.get index model.grid of
                Just Begin ->
                    ( model, Cmd.none )

                Just End ->
                    ( model, Cmd.none )

                _ ->
                    let
                        newGrid =
                            Array.set index model.dragging model.grid
                    in
                    ( { model
                        | grid = newGrid
                        , success = checkSolution model.level newGrid
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

                RBSplitter direction ->
                    ( { model | dragging = RBSplitter (justDirection direction) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- VIEW


viewTile : Tile -> Element Msg
viewTile tile =
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

        RBSplitter direction ->
            let
                indicator attrs =
                    el ([ width (px 10), height (px 10) ] ++ attrs) none
            in
            El.row
                [ width fill
                , height fill
                , rotate (toRotation direction)
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
        [ El.spacing 10
        , El.alignTop
        ]
        [ viewBox [ E.onClick (SetDragging (Track Down)) ] (Track Down)
        , viewBox [ E.onClick (SetDragging (RBSplitter Down)) ] (RBSplitter Down)
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
        []
        (model.grid
            |> Array.toIndexedList
            |> squareUp
            |> List.map viewGridRow
        )


viewSuccessIndicator : Model -> Element Msg
viewSuccessIndicator model =
    let
        color =
            if model.success then
                rgb 0 1 0

            else
                rgb 1 0 0
    in
    El.el
        [ width (px 100)
        , height fill
        , Background.color color
        ]
        none


view : Model -> Html Msg
view model =
    El.layout [ width fill, height fill ]
        (El.column
            [ centerX, centerY, spacing 20 ]
            [ El.paragraph
                [ Font.center, Font.bold, Font.size 40 ]
                [ text model.level.name ]
            , El.row
                [ padding 10, spacing 10 ]
                [ viewPalette
                , viewGrid model
                , viewBrush model
                , viewSuccessIndicator model
                ]
            , El.paragraph
                [ Font.center ]
                [ text model.level.description ]
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


loadLevel : Maybe Level -> Model
loadLevel maybeLevel =
    let
        level =
            Maybe.withDefault
                (Level "" "" 0 (always False))
                maybeLevel
    in
    { mousePos = Position 0 0
    , dragging = Empty
    , grid = initBoard level.size
    , level = level
    , success = False
    }



-- SOLUTIONS


type alias Criteria =
    Robot -> Bool


type Robot
    = Robot Int


type Result
    = Passed
    | Failed


type Progress
    = Finished Result
    | Working Tile


checkSolution : Level -> Array Tile -> Bool
checkSolution level solution =
    let
        size =
            round (sqrt (toFloat (Array.length solution)))

        begin =
            round (toFloat size / 2) - 1

        robot =
            Robot begin
    in
    case simulate solution robot of
        Passed ->
            level.criteria robot

        Failed ->
            not (level.criteria robot)


simulate : Array Tile -> Robot -> Result
simulate tiles robot =
    case checkSafety tiles robot of
        Finished result ->
            result

        Working tile ->
            getDirection tile
                |> move tiles robot
                |> Maybe.map (simulate tiles)
                |> Maybe.withDefault Failed


getDirection : Tile -> Direction
getDirection tile =
    case tile of
        RBSplitter direction ->
            direction

        Track direction ->
            direction

        _ ->
            Down


checkSafety : Array Tile -> Robot -> Progress
checkSafety tiles robot =
    let
        (Robot position) =
            robot
    in
    case Array.get position tiles of
        Just End ->
            Finished Passed

        Just Empty ->
            Finished Failed

        Just tile ->
            Working tile

        Nothing ->
            Finished Failed


move : Array Tile -> Robot -> Direction -> Maybe Robot
move tiles robot direction =
    let
        size =
            round (sqrt (toFloat (Array.length tiles)))

        (Robot position) =
            robot
    in
    case direction of
        Up ->
            if (position - size) >= 0 then
                Just <| Robot (position - size)

            else
                Nothing

        Down ->
            if (position + size) < Array.length tiles then
                Just <| Robot (position + size)

            else
                Nothing

        Left ->
            if modBy size position /= 0 then
                Just <| Robot (position - 1)

            else
                Nothing

        Right ->
            if modBy size (position + 1) /= 0 then
                Just <| Robot (position + 1)

            else
                Nothing
