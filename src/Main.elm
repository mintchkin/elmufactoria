module Main exposing (..)

import Array exposing (Array(..))
import Browser exposing (..)
import Browser.Events as BE
import Direction as Direction exposing (Direction(..))
import Element as El exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as E
import Element.Font as Font
import Element.Input as Input
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
      , size = 3
      , criteria = always Passed
      }
    , { name = "Checking ID"
      , description = "Allow only robots whose codes start with Red"
      , size = 4
      , criteria =
            \robot ->
                case List.head robot.codes of
                    Just Red ->
                        Passed

                    _ ->
                        Failed
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


initBoard : Int -> Array Tile
initBoard size =
    let
        width =
            size * 2 - 1
    in
    Array.repeat (width ^ 2) Empty
        |> Array.set (size - 1) Begin
        |> Array.set (width ^ 2 - size) End


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
                        (SetDirection << Direction.fromKey)
                        (D.field "key" D.string)
                ]



-- UPDATE


type Msg
    = SetPosition Float Float
    | SetDragging Tile
    | SetGridTile Int
    | SetDirection (Maybe Direction)
    | LoadLevel Level


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
                        , success = checkSolution model.level newGrid testRobots
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

        LoadLevel level ->
            ( loadLevel (Just level), Cmd.none )



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


viewLevelSelect : Element Msg
viewLevelSelect =
    let
        levelButton number level =
            Input.button
                [ width (px 50)
                , height (px 50)
                , Border.color (rgb 0 0 0)
                , Border.width 2
                , Font.center
                ]
                { onPress = Just (LoadLevel level)
                , label = text <| String.fromInt (number + 1)
                }
    in
    El.wrappedRow
        [ width fill, spacing 10 ]
        (List.indexedMap levelButton levels)


view : Model -> Html Msg
view model =
    El.layout [ width fill, height fill ]
        (El.column
            [ centerX, centerY, spacing 20 ]
            [ El.column
                [ spacing 10, padding 10, Font.bold, Font.size 25 ]
                [ text "Level Select:"
                , viewLevelSelect
                ]
            , El.paragraph
                [ Font.center, Font.bold, Font.size 40 ]
                [ text model.level.name ]
            , El.row
                [ padding 10, spacing 10 ]
                [ viewPalette
                , viewGrid model
                , viewSuccessIndicator model
                ]
            , El.paragraph
                [ Font.center ]
                [ text model.level.description ]
            , El.el [] (viewBrush model)
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


loadLevel : Maybe Level -> Model
loadLevel maybeLevel =
    let
        level =
            Maybe.withDefault
                (Level "" "" 0 (always Failed))
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
    Robot -> Result


type Code
    = Red
    | Blue


type alias Robot =
    { position : Int
    , memories : List Int
    , codes : List Code
    }


type Result
    = Passed
    | Failed


type Progress
    = Finished Result
    | Working Tile


testRobots : List Robot
testRobots =
    [ Robot 0 [] []
    , Robot 0 [] [ Red, Blue ]
    , Robot 0 [] [ Blue, Red ]
    ]


checkSolution : Level -> Array Tile -> List Robot -> Bool
checkSolution level solution robots =
    let
        size =
            round (sqrt (toFloat (Array.length solution)))

        begin =
            round (toFloat size / 2) - 1
    in
    case robots of
        [] ->
            True

        robot :: rest ->
            let
                positionedBot =
                    { robot | position = begin }
            in
            if simulate solution positionedBot == level.criteria positionedBot then
                checkSolution level solution rest

            else
                False


simulate : Array Tile -> Robot -> Result
simulate tiles robot =
    case checkSafety tiles robot of
        Finished result ->
            result

        Working tile ->
            List.head robot.codes
                |> getDirection tile
                |> move tiles { robot | codes = updateCodes tile robot.codes }
                |> Maybe.map (simulate tiles)
                |> Maybe.withDefault Failed


getDirection : Tile -> Maybe Code -> Direction
getDirection tile maybeCode =
    case tile of
        RBSplitter direction ->
            case maybeCode of
                Just Red ->
                    Direction.shiftClockwise direction

                Just Blue ->
                    Direction.flip <| Direction.shiftClockwise direction

                Nothing ->
                    direction

        Track direction ->
            direction

        _ ->
            Down


checkSafety : Array Tile -> Robot -> Progress
checkSafety tiles robot =
    let
        dejavu =
            List.member robot.position robot.memories
    in
    case Array.get robot.position tiles of
        Just End ->
            Finished Passed

        Just Empty ->
            Finished Failed

        Nothing ->
            Finished Failed

        Just tile ->
            if dejavu then
                Finished Failed

            else
                Working tile


updateCodes : Tile -> List Code -> List Code
updateCodes tile codes =
    case tile of
        RBSplitter _ ->
            case codes of
                Red :: rest ->
                    rest

                Blue :: rest ->
                    rest

                _ ->
                    codes

        _ ->
            codes


move : Array Tile -> Robot -> Direction -> Maybe Robot
move tiles robot direction =
    let
        size =
            round (sqrt (toFloat (Array.length tiles)))
    in
    case direction of
        Up ->
            if (robot.position - size) >= 0 then
                Just
                    { robot
                        | position = robot.position - size
                        , memories = robot.position :: robot.memories
                    }

            else
                Nothing

        Down ->
            if (robot.position + size) < Array.length tiles then
                Just
                    { robot
                        | position = robot.position + size
                        , memories = robot.position :: robot.memories
                    }

            else
                Nothing

        Left ->
            if modBy size robot.position /= 0 then
                Just
                    { robot
                        | position = robot.position - 1
                        , memories = robot.position :: robot.memories
                    }

            else
                Nothing

        Right ->
            if modBy size (robot.position + 1) /= 0 then
                Just
                    { robot
                        | position = robot.position + 1
                        , memories = robot.position :: robot.memories
                    }

            else
                Nothing
