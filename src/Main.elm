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


type alias Model =
    { currentPlayer : Player
    , board : Set Platform
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


platformCircleView : Platform -> G.Shape Msg
platformCircleView ( x, y ) =
    G.circle 50 |> G.filled G.yellow |> G.move ( 100 * (toFloat x + cos (pi / 3) * toFloat y), 100 * sin (pi / 3) * toFloat y )


platformView : TurnPhase -> Platform -> G.Shape Msg
platformView turnPhase ( x, y ) =
    if turnPhase == SelectPlatformPhase then
        platformCircleView ( x, y ) |> G.notifyTap (SelectPlatform ( x, y ))

    else
        platformCircleView ( x, y )


view : Model -> G.Collage Msg
view model =
    G.collage 1000 1000 (Set.toList model.board |> List.map (platformView model.phase))


main : NotificationsApp Model Msg
main =
    notificationsApp { model = initialModel, view = view, update = update }
