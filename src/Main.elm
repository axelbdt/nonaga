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


type Platform
    = Platform


type alias Model =
    { currentPlayer : Player
    , board : Set ( Int, Int )
    }


initialModel : Model
initialModel =
    { currentPlayer = Red
    , board = Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ) ]
    }


type Msg
    = SomeMsg


update : Msg -> Model -> Model
update msg model =
    model


platformView : ( Int, Int ) -> G.Shape Msg
platformView ( x, y ) =
    G.circle 50 |> G.filled G.yellow |> G.move ( 100 * (toFloat x + cos (pi / 3) * toFloat y), 100 * sin (pi / 3) * toFloat y )


view : Model -> G.Collage Msg
view model =
    G.collage 1000 1000 (Set.toList model.board |> List.map platformView)


main : NotificationsApp Model Msg
main =
    notificationsApp { model = initialModel, view = view, update = update }
