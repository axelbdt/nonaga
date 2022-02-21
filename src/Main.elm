module Main exposing (main)

import Basics exposing (cos, sin)
import GraphicSVG as G
import GraphicSVG.App exposing (NotificationsApp, notificationsApp)
import Set exposing (Set)


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


type TurnPhase
    = SelectPlatformPhase
    | ChoosePlatformDestinationPhase


type alias Platform =
    ( Int, Int )


type alias Board =
    Set Platform


neighbors : Platform -> Set Platform
neighbors ( x, y ) =
    Set.fromList
        [ ( x - 1, y - 1 )
        , ( x - 1, y )
        , ( x - 1, y + 1 )
        , ( x, y - 1 )
        , ( x, y )
        , ( x, y + 1 )
        , ( x + 1, y - 1 )
        , ( x + 1, y )
        , ( x + 1, y + 1 )
        ]


isDestination : Board -> Platform -> Platform -> Bool
isDestination board selectedPlatform platform =
    not (Set.member platform board)


destinations : Board -> Platform -> Set Platform
destinations board selectedPlatform =
    Set.toList board
        |> List.map neighbors
        |> List.foldl Set.union Set.empty
        |> Set.filter (isDestination board selectedPlatform)


type alias Model =
    { currentPlayer : Player
    , board : Board
    , phase : TurnPhase
    , selectedPlatform : Maybe Platform
    }


initialModel : Model
initialModel =
    { currentPlayer = Red
    , board = Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ), ( -1, 0 ), ( 0, -1 ), ( -1, -1 ), ( 1, -1 ), ( -1, 1 ) ]
    , phase = SelectPlatformPhase
    , selectedPlatform = Nothing
    }


type Msg
    = SelectPlatform Platform
    | ChoosePlatformDestination


update : Msg -> Model -> Model
update msg model =
    case model.phase of
        SelectPlatformPhase ->
            case msg of
                SelectPlatform platform ->
                    { model | selectedPlatform = Just platform }

                ChoosePlatformDestination ->
                    model

        ChoosePlatformDestinationPhase ->
            model


platformCircleView : Maybe Platform -> Platform -> G.Shape Msg
platformCircleView selectedPlatform ( x, y ) =
    (case selectedPlatform of
        Nothing ->
            G.circle 50
                |> G.filled G.yellow

        Just ( selectedX, selectedY ) ->
            if selectedX == x && selectedY == y then
                G.circle 50 |> G.filled G.orange

            else
                G.circle 50
                    |> G.filled G.yellow
    )
        |> G.move ( 100 * (toFloat x + cos (pi / 3) * toFloat y), 100 * sin (pi / 3) * toFloat y )


platformView : TurnPhase -> Maybe Platform -> Platform -> G.Shape Msg
platformView turnPhase selectedPlatform platform =
    if turnPhase == SelectPlatformPhase then
        platformCircleView selectedPlatform platform |> G.notifyTap (SelectPlatform platform)

    else
        platformCircleView selectedPlatform platform


view : Model -> G.Collage Msg
view model =
    Set.toList model.board
        |> List.map (platformView model.phase model.selectedPlatform)
        |> G.collage 1000 1000


main : NotificationsApp Model Msg
main =
    notificationsApp { model = initialModel, view = view, update = update }
