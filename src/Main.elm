module Main exposing (main)

import Array
import Basics exposing (cos, sin)
import Dict exposing (Dict)
import GraphicSVG as G
import GraphicSVG.App exposing (NotificationsApp, notificationsApp)
import Set exposing (Set)


type Player
    = Red
    | Black


type TurnPhase
    = MovePawn
    | MovePlatform


type alias Platform =
    ( Int, Int )


type alias Board =
    Set Platform


type alias Pawns =
    Dict Platform Player


type alias Model =
    { currentPlayer : Player
    , turnPhase : TurnPhase
    , board : Board
    , pawns : Dict Platform Player
    , selectedPawn : Maybe Platform
    , selectedPlatform : Maybe Platform
    }


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        Red ->
            Black

        Black ->
            Red


neighbors : Platform -> Set Platform
neighbors ( x, y ) =
    Set.fromList
        [ ( x - 1, y )
        , ( x - 1, y + 1 )
        , ( x, y - 1 )
        , ( x, y + 1 )
        , ( x + 1, y - 1 )
        , ( x + 1, y )
        ]


countNeighboringPlatforms : Board -> Platform -> Int
countNeighboringPlatforms board platform =
    neighbors platform
        |> Set.intersect board
        |> Set.size


pawnIsSelectable : Player -> TurnPhase -> Player -> Bool
pawnIsSelectable currentPlayer turnPhase player =
    case turnPhase of
        MovePawn ->
            case currentPlayer of
                Red ->
                    case player of
                        Red ->
                            True

                        Black ->
                            False

                Black ->
                    case player of
                        Red ->
                            True

                        Black ->
                            False

        MovePlatform ->
            False


platformIsSelectable : Board -> Pawns -> Platform -> Bool
platformIsSelectable board pawns platform =
    countNeighboringPlatforms board platform < 5 && not (Dict.member platform pawns)


isDestination : Board -> Platform -> Platform -> Bool
isDestination board selected platform =
    let
        neighborCount =
            countNeighboringPlatforms (Set.remove selected board) platform
    in
    neighborCount >= 2 && neighborCount < 5


findDestinations : Board -> Platform -> Set Platform
findDestinations board selectedPlatform =
    Set.toList board
        |> List.map neighbors
        |> List.foldl Set.union Set.empty
        |> Set.filter (isDestination board selectedPlatform)


initialBoard : Board
initialBoard =
    Set.fromList
        [ ( 0, 0 )
        , ( 1, 0 )
        , ( 0, 1 )
        , ( 1, 1 )
        , ( -1, 0 )
        , ( 0, -1 )
        , ( -1, -1 )
        , ( 1, -1 )
        , ( -1, 1 )
        , ( 2, 0 )
        , ( -2, 0 )
        , ( 0, -2 )
        , ( 2, -2 )
        , ( -2, 2 )
        , ( 0, 2 )
        , ( -2, 1 )
        , ( -1, 2 )
        , ( 2, -1 )
        , ( 1, -2 )
        ]


zipWithPlayer : Player -> List a -> List ( a, Player )
zipWithPlayer player l =
    List.map (\x -> ( x, player )) l


initialPawns : Dict Platform Player
initialPawns =
    zipWithPlayer Red [ ( 0, -2 ), ( -2, 2 ), ( 2, 0 ) ]
        |> List.append (zipWithPlayer Black [ ( -2, 0 ), ( 0, 2 ), ( 2, -2 ) ])
        |> Dict.fromList


initialModel : Model
initialModel =
    { currentPlayer = Red
    , turnPhase = MovePawn
    , board = initialBoard
    , pawns = initialPawns
    , selectedPawn = Nothing
    , selectedPlatform = Nothing
    }


type Msg
    = SelectPawn Platform
    | ChoosePawnDestination Platform Player Platform
    | SelectPlatform Platform
    | ChoosePlatformDestination Platform Platform


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectPawn platform ->
            { model | selectedPawn = Just platform }

        ChoosePawnDestination platform player destination ->
            let
                newPawns =
                    Dict.remove platform model.pawns
                        |> Dict.insert destination player
            in
            { model | pawns = newPawns }

        SelectPlatform platform ->
            { model | selectedPlatform = Just platform }

        ChoosePlatformDestination selected destination ->
            let
                newBoard =
                    model.board |> Set.remove selected |> Set.insert destination
            in
            { model | selectedPlatform = Nothing, board = newBoard }


placeShape : ( Int, Int ) -> G.Shape Msg -> G.Shape Msg
placeShape ( x, y ) shape =
    shape
        |> G.move ( 100 * (toFloat x + cos (pi / 3) * toFloat y), 100 * sin (pi / 3) * toFloat y )


selectedPlatformShape : G.Shape Msg
selectedPlatformShape =
    G.circle 50 |> G.filled G.orange


platformShape : G.Shape Msg
platformShape =
    G.circle 50
        |> G.filled G.yellow


platformCircleView : Maybe Platform -> Platform -> G.Shape Msg
platformCircleView selectedPlatform platform =
    (case selectedPlatform of
        Nothing ->
            platformShape

        Just selected ->
            if selected == platform then
                selectedPlatformShape

            else
                platformShape
    )
        |> placeShape platform


platformView : Board -> Pawns -> Maybe Platform -> Platform -> G.Shape Msg
platformView board pawns selectedPlatform platform =
    if platformIsSelectable board pawns platform then
        platformCircleView selectedPlatform platform
            |> G.notifyTap (SelectPlatform platform)

    else
        platformCircleView selectedPlatform platform


destinationView : Platform -> Platform -> G.Shape Msg
destinationView selected destination =
    platformCircleView Nothing destination |> G.makeTransparent 0.6 |> G.notifyTap (ChoosePlatformDestination selected destination)


boardView : Board -> Pawns -> Maybe Platform -> List (G.Shape Msg)
boardView board pawns selectedPlatform =
    Set.toList board
        |> List.map (platformView board pawns selectedPlatform)
        |> List.append
            (case selectedPlatform of
                Nothing ->
                    []

                Just selected ->
                    findDestinations board selected
                        |> Set.toList
                        |> List.map (destinationView selected)
            )


color : Player -> Bool -> G.Color
color player selected =
    case player of
        Red ->
            if selected then
                G.pink

            else
                G.red

        Black ->
            if selected then
                G.grey

            else
                G.black


pawnShape : Platform -> Player -> Bool -> G.Shape Msg
pawnShape platform player selected =
    G.circle 40 |> G.filled (color player selected) |> placeShape platform


pawnCircleView selectedPawn platform player =
    case selectedPawn of
        Nothing ->
            pawnShape platform player False

        Just selected ->
            pawnShape platform player (selected == platform)


pawnView : Player -> TurnPhase -> Maybe Platform -> Platform -> Player -> G.Shape Msg
pawnView currentPlayer turnPhase selectedPawn platform player =
    if pawnIsSelectable currentPlayer turnPhase player then
        pawnCircleView selectedPawn platform player
            |> G.notifyTap (SelectPawn platform)

    else
        pawnCircleView selectedPawn platform player


pawnsView : Player -> TurnPhase -> Maybe Platform -> Dict Platform Player -> List (G.Shape Msg)
pawnsView currentPlayer turnPhase selectedPawn pawns =
    Dict.map (pawnView currentPlayer turnPhase selectedPawn) pawns
        |> Dict.values


view : Model -> G.Collage Msg
view model =
    pawnsView model.currentPlayer model.turnPhase model.selectedPawn model.pawns
        |> List.append (boardView model.board model.pawns model.selectedPlatform)
        |> G.collage 1000 1000


main : NotificationsApp Model Msg
main =
    notificationsApp { model = initialModel, view = view, update = update }
