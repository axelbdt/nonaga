module Main exposing (main)

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
    , lastMovedPlatform : Platform
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


directions : Set ( Int, Int )
directions =
    Set.fromList
        [ ( -1, 0 )
        , ( -1, 1 )
        , ( 0, 1 )
        , ( 0, -1 )
        , ( 1, -1 )
        , ( 1, 0 )
        ]


neighbors : Platform -> Set Platform
neighbors ( x, y ) =
    Set.map (\( dx, dy ) -> ( x + dx, y + dy )) directions


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
                            False

                        Black ->
                            True

        _ ->
            False


checkDirection : Board -> Pawns -> Platform -> ( Int, Int ) -> Platform
checkDirection board pawns start direction =
    let
        ( x, y ) =
            start

        checkedPosition =
            ( x + Tuple.first direction, y + Tuple.second direction )
    in
    if Set.member checkedPosition board && not (Dict.member checkedPosition pawns) then
        checkDirection board pawns checkedPosition direction

    else
        start


findPawnDestinations : Board -> Pawns -> Platform -> Set Platform
findPawnDestinations board pawns selectedPawn =
    Set.map (checkDirection board pawns selectedPawn) directions


platformIsSelectable : TurnPhase -> Board -> Pawns -> Platform -> Platform -> Bool
platformIsSelectable turnPhase board pawns lastMovedPlatform platform =
    case turnPhase of
        MovePlatform ->
            countNeighboringPlatforms board platform < 5 && not (Dict.member platform pawns) && platform /= lastMovedPlatform

        _ ->
            False


isPlatformDestination : Board -> Platform -> Platform -> Bool
isPlatformDestination board selected platform =
    let
        neighborCount =
            countNeighboringPlatforms (Set.remove selected board) platform
    in
    neighborCount >= 2 && neighborCount < 5


findPlatformDestinations : Board -> Platform -> Set Platform
findPlatformDestinations board selectedPlatform =
    Set.toList board
        |> List.map neighbors
        |> List.foldl Set.union Set.empty
        |> Set.filter (isPlatformDestination board selectedPlatform)


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
    , lastMovedPlatform = ( 0, 0 )
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
            { model | turnPhase = MovePlatform, pawns = newPawns, selectedPawn = Nothing }

        SelectPlatform platform ->
            { model | selectedPlatform = Just platform }

        ChoosePlatformDestination selected destination ->
            let
                newBoard =
                    model.board |> Set.remove selected |> Set.insert destination
            in
            { model
                | currentPlayer = nextPlayer model.currentPlayer
                , turnPhase = MovePawn
                , lastMovedPlatform = destination
                , selectedPlatform = Nothing
                , board = newBoard
            }


placeShape : ( Int, Int ) -> G.Shape Msg -> G.Shape Msg
placeShape ( x, y ) shape =
    shape
        |> G.move ( 100 * (toFloat x + cos (pi / 3) * toFloat y), 100 * sin (pi / 3) * toFloat y )


platformColor : Bool -> G.Color
platformColor selected =
    if selected then
        G.lightYellow

    else
        G.yellow


platformShape : Bool -> G.Shape Msg
platformShape selected =
    G.circle 50
        |> G.filled (platformColor selected)


platformCircleView : Maybe Platform -> Platform -> G.Shape Msg
platformCircleView selectedPlatform platform =
    (case selectedPlatform of
        Nothing ->
            platformShape False

        Just selected ->
            platformShape (selected == platform)
    )
        |> placeShape platform


platformView : TurnPhase -> Board -> Pawns -> Platform -> Maybe Platform -> Platform -> G.Shape Msg
platformView turnPhase board pawns lastMovedPlatform selectedPlatform platform =
    if platformIsSelectable turnPhase board pawns lastMovedPlatform platform then
        platformCircleView selectedPlatform platform
            |> G.notifyTap (SelectPlatform platform)

    else
        platformCircleView selectedPlatform platform


platformDestinationView : Platform -> Platform -> G.Shape Msg
platformDestinationView selected destination =
    platformCircleView Nothing destination |> G.makeTransparent 0.6 |> G.notifyTap (ChoosePlatformDestination selected destination)


color : Player -> Bool -> G.Color
color player selected =
    case player of
        Red ->
            if selected then
                G.lightRed

            else
                G.red

        Black ->
            if selected then
                G.charcoal

            else
                G.darkCharcoal


pawnShape : Player -> Bool -> G.Shape Msg
pawnShape player selected =
    G.circle 40 |> G.filled (color player selected)


pawnCircleView : Maybe Platform -> Platform -> Player -> G.Shape Msg
pawnCircleView selectedPawn platform player =
    (case selectedPawn of
        Nothing ->
            pawnShape player False

        Just selected ->
            pawnShape player (selected == platform)
    )
        |> placeShape platform


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


pawnDestinationView : Platform -> Player -> Platform -> G.Shape Msg
pawnDestinationView selected player destination =
    pawnCircleView Nothing destination player |> G.makeTransparent 0.6 |> G.notifyTap (ChoosePawnDestination selected player destination)


view : Model -> G.Collage Msg
view model =
    pawnsView model.currentPlayer model.turnPhase model.selectedPawn model.pawns
        |> List.append
            (case model.selectedPawn of
                Nothing ->
                    []

                Just selected ->
                    findPawnDestinations model.board model.pawns selected
                        |> Set.toList
                        |> List.map (pawnDestinationView selected model.currentPlayer)
            )
        |> List.append
            (Set.toList model.board
                |> List.map (platformView model.turnPhase model.board model.pawns model.lastMovedPlatform model.selectedPlatform)
                |> List.append
                    (case model.selectedPlatform of
                        Nothing ->
                            []

                        Just selected ->
                            findPlatformDestinations model.board selected
                                |> Set.toList
                                |> List.map (platformDestinationView selected)
                    )
            )
        |> G.collage 1000 1000


main : NotificationsApp Model Msg
main =
    notificationsApp { model = initialModel, view = view, update = update }
