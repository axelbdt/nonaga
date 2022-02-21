module Main exposing (main)

import Array
import Basics exposing (cos, sin)
import Dict exposing (Dict)
import GraphicSVG as G
import GraphicSVG.App exposing (NotificationsApp, notificationsApp)
import Set exposing (Set, intersect)


type Player
    = Red
    | Black


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        Red ->
            Black

        Black ->
            Red


type alias Platform =
    ( Int, Int )


type alias Board =
    Set Platform


type alias Pawns =
    Dict Platform Player


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


isSelectable : Board -> Pawns -> Platform -> Bool
isSelectable board pawns platform =
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


type alias Model =
    { currentPlayer : Player
    , board : Board
    , selectedPlatform : Maybe Platform
    , pawns : Dict Platform Player
    }


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
    , board = initialBoard
    , selectedPlatform = Nothing
    , pawns = initialPawns
    }


type Msg
    = SelectPlatform Platform
    | ChoosePlatformDestination Platform Platform


update : Msg -> Model -> Model
update msg model =
    case msg of
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
    if isSelectable board pawns platform then
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


color : Player -> G.Color
color player =
    case player of
        Red ->
            G.red

        Black ->
            G.black


pawnView : Platform -> Player -> G.Shape Msg
pawnView platform player =
    G.circle 40 |> G.filled (color player) |> placeShape platform


pawnsView : Dict Platform Player -> List (G.Shape Msg)
pawnsView pawns =
    Dict.map pawnView pawns
        |> Dict.values


view : Model -> G.Collage Msg
view model =
    pawnsView model.pawns
        |> List.append (boardView model.board model.pawns model.selectedPlatform)
        |> G.collage 1000 1000


main : NotificationsApp Model Msg
main =
    notificationsApp { model = initialModel, view = view, update = update }
