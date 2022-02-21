module Main exposing (main)

import Basics exposing (cos, sin)
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
    }


initialModel : Model
initialModel =
    { currentPlayer = Red
    , board = Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ), ( -1, 0 ), ( 0, -1 ), ( -1, -1 ), ( 1, -1 ), ( -1, 1 ) ]
    , selectedPlatform = Just ( 1, 1 )
    }


type Msg
    = SelectPlatform Platform
    | ChoosePlatformDestination


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectPlatform platform ->
            { model | selectedPlatform = Just platform }

        ChoosePlatformDestination ->
            model


platformCircleView : Bool -> Platform -> G.Shape Msg
platformCircleView selected ( x, y ) =
    (if selected then
        G.circle 50 |> G.filled G.orange

     else
        G.circle 50
            |> G.filled G.yellow
    )
        |> G.move ( 100 * (toFloat x + cos (pi / 3) * toFloat y), 100 * sin (pi / 3) * toFloat y )


platformView : Maybe Platform -> Platform -> G.Shape Msg
platformView selectedPlatform platform =
    (case selectedPlatform of
        Nothing ->
            platformCircleView False platform

        Just selected ->
            if selected == platform then
                platformCircleView True platform

            else
                platformCircleView False platform
    )
        |> G.notifyTap (SelectPlatform platform)


destinationView : Platform -> G.Shape Msg
destinationView destination =
    platformCircleView False destination |> G.makeTransparent 0.6


destinationsView : Set Platform -> List (G.Shape Msg)
destinationsView destinations =
    List.map destinationView (Set.toList destinations)


boardView : Board -> Maybe Platform -> List (G.Shape Msg)
boardView board selectedPlatform =
    Set.toList board
        |> List.map (platformView selectedPlatform)
        |> List.append
            (case selectedPlatform of
                Nothing ->
                    []

                Just selected ->
                    destinationsView (findDestinations board selected)
            )


view : Model -> G.Collage Msg
view model =
    boardView model.board model.selectedPlatform
        |> G.collage 1000 1000


main : NotificationsApp Model Msg
main =
    notificationsApp { model = initialModel, view = view, update = update }
