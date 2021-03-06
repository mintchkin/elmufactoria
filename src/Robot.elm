module Robot exposing (Progress(..), Robot, advance, checkBot, checkSolution, initAll, isFinished, view)

import Array exposing (Array)
import Direction exposing (Direction(..), Rotation(..))
import Element as El exposing (..)
import Element.Background as Background
import Level exposing (Code(..), Level, Outcome(..), invertCode)
import Tile exposing (Tile(..))


type alias Robot =
    { memories : List ( Int, List Code )
    , position : Int
    , codes : List Code
    }


type Progress
    = Finished Outcome
    | Working Robot


initAll : Level -> List Robot
initAll level =
    List.map (Robot [] (level.size - 1)) level.tests


checkSolution : Level -> Array Tile -> Bool
checkSolution { criteria, tests, size } solution =
    let
        initRobot =
            Robot [] (size - 1)
    in
    List.all (\codes -> criteria codes <| simulate solution <| initRobot codes) tests


checkBot : Level -> Array Tile -> Robot -> Bool
checkBot level solution robot =
    let
        ( _, initial ) =
            List.head (List.reverse robot.memories)
                |> Maybe.withDefault ( robot.position, robot.codes )
    in
    level.criteria initial (simulate solution robot)


simulate : Array Tile -> Robot -> Outcome
simulate tiles robot =
    case advance tiles robot of
        Working nextBot ->
            simulate tiles nextBot

        Finished outcome ->
            outcome


advance : Array Tile -> Robot -> Progress
advance tiles robot =
    let
        maybeDirection =
            Array.get robot.position tiles |> Maybe.andThen (getDirection robot)
    in
    case maybeDirection of
        Just direction ->
            let
                updateBot =
                    remember >> updateCodes tiles >> move tiles direction
            in
            isFinished tiles robot |> chainProgress updateBot

        Nothing ->
            Finished Failed


getDirection : Robot -> Tile -> Maybe Direction
getDirection robot tile =
    case tile of
        Begin ->
            Just Down

        End ->
            Just Down

        Track direction ->
            Just direction

        MultiTrack first rotation ->
            let
                continue from =
                    if from == first || from == Direction.reverse first then
                        first

                    else
                        Direction.rotate rotation first

                trajectory =
                    case ( robot.position, robot.codes ) :: robot.memories of
                        ( curr, _ ) :: ( prev, _ ) :: _ ->
                            if curr == prev - 1 then
                                Just Left

                            else if curr == prev + 1 then
                                Just Right

                            else if curr < prev then
                                Just Up

                            else
                                Just Down

                        _ ->
                            Nothing
            in
            trajectory |> Maybe.map continue

        Splitter code direction ->
            case robot.codes of
                robotCode :: _ ->
                    if robotCode == code then
                        Just (Direction.rotate Clockwise direction)

                    else if robotCode == invertCode code then
                        Just (Direction.rotate CounterClock direction)

                    else
                        Just direction

                _ ->
                    Just direction

        Writer _ direction ->
            Just direction

        Empty ->
            Nothing


isFinished : Array Tile -> Robot -> Progress
isFinished tiles robot =
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
        Just (Splitter code _) ->
            case robot.codes of
                robotCode :: rest ->
                    if robotCode == code || robotCode == invertCode code then
                        { robot | codes = rest }

                    else
                        robot

                _ ->
                    robot

        Just (Writer code _) ->
            { robot | codes = robot.codes ++ [ code ] }

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

        unless check position =
            if not (check position) then
                Working { robot | position = position }

            else
                Finished Failed
    in
    case direction of
        Up ->
            robot.position - size |> unless (\p -> p < 0)

        Down ->
            robot.position + size |> unless (\p -> p >= Array.length tiles)

        Left ->
            robot.position - 1 |> unless (\p -> modBy size p == (size - 1))

        Right ->
            robot.position + 1 |> unless (\p -> modBy size p == 0)


view : Element msg
view =
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
