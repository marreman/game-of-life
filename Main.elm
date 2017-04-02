module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Matrix exposing (Matrix, Location)
import Time exposing (Time)
import Window
import Task


main =
    Html.program
        { init = ( model, getWindowSize )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { started : Bool
    , grid : Matrix Bool
    }


type alias Position =
    ( Int, Int )


model : Model
model =
    { started = False
    , grid = Matrix.square 0 (\_ -> False)
    }


type Msg
    = StartStop
    | Flip Location
    | Tick Time
    | NewWindowSize Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Flip location ->
            { model | grid = Matrix.update location not model.grid } ! []

        StartStop ->
            { model | started = not model.started } ! []

        Tick _ ->
            { model | grid = evolveGrid model.grid } ! []

        NewWindowSize size ->
            let
                rows =
                    size.height // 20

                cols =
                    size.width // 20
            in
                { model | grid = Matrix.matrix rows cols (\_ -> False) } ! []


evolveGrid : Matrix Bool -> Matrix Bool
evolveGrid grid =
    let
        evolve : Location -> Bool -> Bool
        evolve location isAlive =
            getNeighbours location
                |> List.map (\loc -> Matrix.get loc grid)
                |> List.map evaluateCell
                |> List.sum
                |> evaluateResult isAlive

        getNeighbours : Location -> List Location
        getNeighbours ( x, y ) =
            [ Matrix.loc (x - 1) (y - 1)
            , Matrix.loc (x - 1) (y)
            , Matrix.loc (x - 1) (y + 1)
            , Matrix.loc (x) (y + 1)
            , Matrix.loc (x + 1) (y + 1)
            , Matrix.loc (x + 1) (y)
            , Matrix.loc (x + 1) (y - 1)
            , Matrix.loc (x) (y - 1)
            ]

        evaluateCell : Maybe Bool -> Int
        evaluateCell maybeAlive =
            maybeAlive
                |> Maybe.map
                    (\isAlive ->
                        if isAlive then
                            1
                        else
                            0
                    )
                |> Maybe.withDefault 0

        evaluateResult : Bool -> Int -> Bool
        evaluateResult isAlive neighbours =
            if isAlive then
                case neighbours of
                    2 ->
                        True

                    3 ->
                        True

                    _ ->
                        False
            else
                case neighbours of
                    3 ->
                        True

                    _ ->
                        False
    in
        Matrix.mapWithLocation evolve grid


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.started then
        Time.every (Time.second / 10) Tick
    else
        Sub.none


getWindowSize =
    Task.perform NewWindowSize Window.size


view : Model -> Html Msg
view model =
    Html.div []
        [ viewBoard model
        , viewControls model
        ]


viewControls : Model -> Html Msg
viewControls model =
    Html.div [ class "controls" ]
        [ Html.button [ onClick StartStop ]
            [ if model.started then
                text "■"
              else
                text "▶"
            ]
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        cells =
            model.grid
                |> Matrix.mapWithLocation viewCell
                |> Matrix.flatten
    in
        Html.div [ class "board" ] cells


viewCell : Location -> Bool -> Html Msg
viewCell location isAlive =
    div
        [ onClick (Flip location)
        , classList
            [ ( "cell", True )
            , ( "is-alive", isAlive )
            ]
        ]
        []
