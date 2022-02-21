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


isSelectable : Board -> Platform -> Bool
isSelectable board platform =
    countNeighboringPlatforms board platform < 5


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
    , redPawns : Set Platform
    , blackPawns : Set Platform
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


initialRedPawns =
    Set.fromList [ ( 0, -2 ), ( 2, -2 ), ( 2, 0 ) ]


initialBlackPawns =
    Set.fromList [ ( -2, 0 ), ( 0, 2 ), ( 2, -2 ) ]


initialModel : Model
initialModel =
    { currentPlayer = Red
    , board = initialBoard
    , selectedPlatform = Nothing
    , redPawns = initialRedPawns
    , blackPawns = initialBlackPawns
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


platformView : Board -> Maybe Platform -> Platform -> G.Shape Msg
platformView board selectedPlatform platform =
    if isSelectable board platform then
        platformCircleView selectedPlatform platform
            |> G.notifyTap (SelectPlatform platform)

    else
        platformCircleView selectedPlatform platform


destinationView : Platform -> Platform -> G.Shape Msg
destinationView selected destination =
    platformCircleView Nothing destination |> G.makeTransparent 0.6 |> G.notifyTap (ChoosePlatformDestination selected destination)


boardView : Board -> Maybe Platform -> List (G.Shape Msg)
boardView board selectedPlatform =
    Set.toList board
        |> List.map (platformView board selectedPlatform)
        |> List.append
            (case selectedPlatform of
                Nothing ->
                    []

                Just selected ->
                    findDestinations board selected
                        |> Set.toList
                        |> List.map (destinationView selected)
            )


view : Model -> G.Collage Msg
view model =
    boardView model.board model.selectedPlatform
        |> G.collage 1000 1000


main : NotificationsApp Model Msg
main =
    notificationsApp { model = initialModel, view = view, update = update }
