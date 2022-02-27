module Main exposing (main)

import Basics exposing (cos, sin)
import GraphicSVG as G
import GraphicSVG.App exposing (NotificationsApp, notificationsApp)
import Set exposing (Set)



-- MODEL


type Player
    = Red
    | Black


type TurnPhase
    = MoveToken
    | MovePlatform


type alias Platform =
    ( Int, Int )


type alias Board =
    Set Platform


type alias Token =
    Platform


type alias TokenSet =
    { player : Player, platforms : Set Platform }


type alias Tokens =
    List TokenSet


type alias Model =
    { currentPlayer : Player
    , turnPhase : TurnPhase
    , board : Board
    , tokens : Tokens
    , lastMovedPlatform : Platform
    , selectedToken : Maybe Token
    , selectedPlatform : Maybe Platform
    }


playerText : Player -> String
playerText player =
    case player of
        Red ->
            "Red"

        Black ->
            "Black"


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        Red ->
            Black

        Black ->
            Red


playerEquals : Player -> Player -> Bool
playerEquals p1 p2 =
    case p1 of
        Red ->
            case p2 of
                Red ->
                    True

                Black ->
                    False

        Black ->
            case p2 of
                Red ->
                    False

                Black ->
                    True


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


noTokenAt : Platform -> Tokens -> Bool
noTokenAt platform tokens =
    tokens
        |> List.all (\ts -> not (Set.member platform ts.platforms))


platformIsSelectable : Board -> Tokens -> Platform -> Platform -> Bool
platformIsSelectable board tokens lastMovedPlatform platform =
    countNeighboringPlatforms board platform < 5 && noTokenAt platform tokens && platform /= lastMovedPlatform


checkDirection : Board -> Tokens -> Platform -> ( Int, Int ) -> Platform
checkDirection board tokens start direction =
    let
        ( x, y ) =
            start

        checkedPosition =
            ( x + Tuple.first direction, y + Tuple.second direction )
    in
    if Set.member checkedPosition board && noTokenAt checkedPosition tokens then
        checkDirection board tokens checkedPosition direction

    else
        start


findTokenDestinations : Board -> Tokens -> Platform -> Set Platform
findTokenDestinations board tokens selectedToken =
    Set.map (checkDirection board tokens selectedToken) directions


moveToken : Platform -> Platform -> TokenSet -> TokenSet
moveToken from to tokenSet =
    if Set.member from tokenSet.platforms then
        { tokenSet
            | platforms =
                tokenSet.platforms
                    |> Set.remove from
                    |> Set.insert to
        }

    else
        tokenSet


checkWinner : Tokens -> Maybe Player
checkWinner tokens =
    case tokens of
        [] ->
            Nothing

        tokenSet :: rest ->
            if
                Set.toList tokenSet.platforms
                    |> List.any (tokenIsWinner tokenSet.platforms)
            then
                Just tokenSet.player

            else
                checkWinner rest


tokenIsWinner : Set Token -> Token -> Bool
tokenIsWinner tokens token =
    tokens
        |> Set.intersect (neighbors token)
        |> Set.size
        |> (==) 2


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


initialTokens : Tokens
initialTokens =
    [ { player = Red
      , platforms =
            Set.fromList
                [ ( 0, -2 )
                , ( -2, 2 )
                , ( 2, 0 )
                ]
      }
    , { player = Black
      , platforms =
            Set.fromList
                [ ( -2, 0 )
                , ( 0, 2 )
                , ( 2, -2 )
                ]
      }
    ]


initialModel : Model
initialModel =
    { currentPlayer = Red
    , turnPhase = MoveToken
    , board = initialBoard
    , tokens = initialTokens
    , lastMovedPlatform = ( 0, 0 )
    , selectedToken = Nothing
    , selectedPlatform = Nothing
    }


type Msg
    = SelectToken Player Token
    | ChooseTokenDestination Token Platform
    | SelectPlatform Platform
    | ChoosePlatformDestination Platform Platform
    | Reset



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectToken player token ->
            case model.turnPhase of
                MoveToken ->
                    if playerEquals model.currentPlayer player then
                        { model | selectedToken = Just token }

                    else
                        model

                MovePlatform ->
                    model

        ChooseTokenDestination from to ->
            let
                newTokens =
                    List.map (moveToken from to) model.tokens
            in
            { model
                | turnPhase = MovePlatform
                , tokens = newTokens
                , selectedToken = Nothing
            }

        SelectPlatform platform ->
            case model.turnPhase of
                MovePlatform ->
                    case checkWinner model.tokens of
                        Nothing ->
                            if platformIsSelectable model.board model.tokens model.lastMovedPlatform platform then
                                { model | selectedPlatform = Just platform }

                            else
                                model

                        Just _ ->
                            model

                MoveToken ->
                    model

        ChoosePlatformDestination from to ->
            let
                newBoard =
                    model.board |> Set.remove from |> Set.insert to
            in
            { model
                | currentPlayer = nextPlayer model.currentPlayer
                , turnPhase = MoveToken
                , lastMovedPlatform = to
                , selectedPlatform = Nothing
                , board = newBoard
            }

        Reset ->
            initialModel



-- VIEW


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


platformView : Maybe Platform -> Platform -> G.Shape Msg
platformView selectedPlatform platform =
    (case selectedPlatform of
        Nothing ->
            platformShape False

        Just selected ->
            platformShape (selected == platform)
    )
        |> placeShape platform
        |> G.notifyTap (SelectPlatform platform)


boardView : Board -> Maybe Platform -> G.Shape Msg
boardView board selectedPlatform =
    Set.toList board
        |> List.map (platformView selectedPlatform)
        |> G.group


platformDestinationView : Platform -> Platform -> G.Shape Msg
platformDestinationView from to =
    platformShape False
        |> placeShape to
        |> G.makeTransparent 0.6
        |> G.notifyTap (ChoosePlatformDestination from to)


tokenColor : Player -> Bool -> G.Color
tokenColor player selected =
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


tokenShape : Player -> Bool -> G.Shape Msg
tokenShape player selected =
    G.circle 40 |> G.filled (tokenColor player selected)


tokenView : Maybe Platform -> Player -> Platform -> G.Shape Msg
tokenView selectedToken player platform =
    (case selectedToken of
        Nothing ->
            tokenShape player False

        Just selected ->
            tokenShape player (selected == platform)
    )
        |> placeShape platform
        |> G.notifyTap (SelectToken player platform)


tokenSetView : Maybe Platform -> TokenSet -> G.Shape Msg
tokenSetView selectedToken tokenSet =
    Set.toList tokenSet.platforms
        |> List.map (tokenView selectedToken tokenSet.player)
        |> G.group


tokensView : Maybe Platform -> Tokens -> G.Shape Msg
tokensView selectedToken tokens =
    List.map (tokenSetView selectedToken) tokens
        |> G.group


tokenDestinationView : Player -> Platform -> Platform -> G.Shape Msg
tokenDestinationView player from to =
    tokenShape player False
        |> placeShape to
        |> G.makeTransparent 0.6
        |> G.notifyTap (ChooseTokenDestination from to)


winnerView : Player -> G.Shape Msg
winnerView player =
    G.group
        [ G.roundedRect 240 120 5 |> G.filled G.white
        , G.text (playerText player ++ " wins!") |> G.centered |> G.size 32 |> G.filled (tokenColor player False) |> G.move ( 0, 16 )
        , G.group
            [ G.roundedRect 96 48 5 |> G.filled (tokenColor player False)
            , G.text "Retry" |> G.centered |> G.size 24 |> G.filled G.white |> G.move ( 0, -8 )
            ]
            |> G.move ( 0, -24 )
            |> G.notifyTap Reset
        ]


view : Model -> G.Collage Msg
view model =
    [ boardView model.board model.selectedPlatform
    , G.group
        (case model.selectedPlatform of
            Nothing ->
                []

            Just selected ->
                findPlatformDestinations model.board selected
                    |> Set.toList
                    |> List.map (platformDestinationView selected)
        )
    , tokensView
        model.selectedToken
        model.tokens
    , G.group
        (case model.selectedToken of
            Nothing ->
                []

            Just selected ->
                findTokenDestinations model.board model.tokens selected
                    |> Set.toList
                    |> List.map (tokenDestinationView model.currentPlayer selected)
        )
    , case checkWinner model.tokens of
        Nothing ->
            G.group []

        Just player ->
            winnerView player
    ]
        |> G.collage 1000 1000


main : NotificationsApp Model Msg
main =
    notificationsApp { model = initialModel, view = view, update = update }
