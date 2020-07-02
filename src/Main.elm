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
import Level as Level exposing (Code(..), Level, Outcome(..))
import Time


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
    , level : Level
    , success : Bool
    , replayBots : List Robot
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
    ( loadLevel Level.first
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragging of
        Empty ->
            Sub.batch
                [ BE.onMouseDown <|
                    D.map2 SetPosition
                        (D.field "pageX" D.float)
                        (D.field "pageY" D.float)
                , Time.every 1000 (\_ -> StepReplay)
                ]

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
    | Replay
    | StepReplay


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
                        , success = List.all (testSolution model.level newGrid) testCodes
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
            ( loadLevel level, Cmd.none )

        Replay ->
            case model.replayBots of
                [] ->
                    let
                        start =
                            model.level.size - 1
                    in
                    ( { model | replayBots = List.map (Robot [] start) testCodes }
                    , Cmd.none
                    )

                _ ->
                    ( { model | replayBots = [] }, Cmd.none )

        StepReplay ->
            case model.replayBots of
                robot :: rest ->
                    case advance model.grid robot of
                        Finished _ ->
                            ( { model | replayBots = rest }, Cmd.none )

                        Working bot ->
                            ( { model | replayBots = bot :: rest }, Cmd.none )

                [] ->
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


viewBot : Element Msg
viewBot =
    let
        eye =
            el [ Background.color (rgb 0 0 0), width (px 4), height (px 4) ] none
    in
    El.el
        [ centerX
        , centerY
        , width (px 30)
        , height (px 25)
        , Background.color (rgb 0.75 0.75 0.75)
        , padding 8
        ]
        (El.row
            [ width fill, centerX, spaceEvenly ]
            [ eye, eye ]
        )


viewGridCell : Model -> Int -> Tile -> Element Msg
viewGridCell model index tile =
    let
        addRobot =
            List.head model.replayBots
                |> Maybe.map (\robot -> robot.position == index)
                |> Maybe.withDefault False
    in
    if addRobot then
        viewBox [ E.onClick (SetGridTile index), inFront viewBot ] tile

    else
        viewBox [ E.onClick (SetGridTile index) ] tile


viewGrid : Model -> Element Msg
viewGrid model =
    El.column
        []
        (model.grid
            |> Array.indexedMap (viewGridCell model)
            |> Array.toList
            |> squareUp
            |> List.map (row [])
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
        (List.indexedMap levelButton Level.list)


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
                , el [ E.onClick Replay ] (text "Replay")
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


loadLevel : Level -> Model
loadLevel level =
    { mousePos = Position 0 0
    , dragging = Empty
    , grid = initBoard level.size
    , level = level
    , success = False
    , replayBots = []
    }



-- ROBOTS


type alias Robot =
    { memories : List ( Int, List Code )
    , position : Int
    , codes : List Code
    }


type Progress
    = Finished Outcome
    | Working Robot


testCodes : List (List Code)
testCodes =
    [ []
    , [ Red, Blue ]
    , [ Blue, Red ]
    ]


testSolution : Level -> Array Tile -> List Code -> Bool
testSolution level solution codes =
    let
        size =
            round (sqrt (toFloat (Array.length solution)))

        begin =
            round (toFloat size / 2) - 1

        robot =
            Robot [] begin codes
    in
    level.criteria codes (simulate solution robot)


simulate : Array Tile -> Robot -> Outcome
simulate tiles robot =
    case advance tiles robot of
        Working nextBot ->
            simulate tiles nextBot

        Finished outcome ->
            outcome


advance : Array Tile -> Robot -> Progress
advance tiles robot =
    case getDirection tiles robot of
        Just direction ->
            let
                updateBot =
                    remember >> updateCodes tiles >> move tiles direction
            in
            chainProgress updateBot (checkSafety tiles robot)

        Nothing ->
            Finished Failed


getDirection : Array Tile -> Robot -> Maybe Direction
getDirection tiles robot =
    case Array.get robot.position tiles of
        Just Begin ->
            Just Down

        Just (Track direction) ->
            Just direction

        Just (RBSplitter direction) ->
            case robot.codes of
                Red :: _ ->
                    Just (Direction.shiftClockwise direction)

                Blue :: _ ->
                    Just (Direction.flip <| Direction.shiftClockwise direction)

                _ ->
                    Just direction

        _ ->
            Nothing


checkSafety : Array Tile -> Robot -> Progress
checkSafety tiles robot =
    let
        dejavu =
            List.member ( robot.position, robot.codes ) robot.memories
    in
    case Array.get robot.position tiles of
        Just End ->
            Finished (Passed robot.codes)

        Just Empty ->
            Finished Failed

        Nothing ->
            Finished Failed

        Just _ ->
            if dejavu then
                Finished Failed

            else
                Working robot


remember : Robot -> Robot
remember robot =
    { robot | memories = ( robot.position, robot.codes ) :: robot.memories }


updateCodes : Array Tile -> Robot -> Robot
updateCodes tiles robot =
    case Array.get robot.position tiles of
        Just (RBSplitter _) ->
            case robot.codes of
                Red :: rest ->
                    { robot | codes = rest }

                Blue :: rest ->
                    { robot | codes = rest }

                _ ->
                    robot

        _ ->
            robot


chainProgress : (Robot -> Progress) -> Progress -> Progress
chainProgress callback result =
    case result of
        Working robot ->
            callback robot

        finished ->
            finished


move : Array Tile -> Direction -> Robot -> Progress
move tiles direction robot =
    let
        size =
            round (sqrt (toFloat (Array.length tiles)))
    in
    case direction of
        Up ->
            if (robot.position - size) >= 0 then
                Working
                    { robot | position = robot.position - size }

            else
                Finished Failed

        Down ->
            if (robot.position + size) < Array.length tiles then
                Working
                    { robot | position = robot.position + size }

            else
                Finished Failed

        Left ->
            if modBy size robot.position /= 0 then
                Working
                    { robot | position = robot.position - 1 }

            else
                Finished Failed

        Right ->
            if modBy size (robot.position + 1) /= 0 then
                Working
                    { robot | position = robot.position + 1 }

            else
                Finished Failed
