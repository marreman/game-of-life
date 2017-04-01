module Main exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Matrix exposing (Matrix, Location)
import Time exposing (Time)


main =
    Html.program
        { init = ( model, Cmd.none )
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
    , grid = Matrix.square 100 (\_ -> False)
    }


type Msg
    = StartStop
    | Flip Location
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Flip location ->
            { model | grid = Matrix.update location not model.grid } ! []

        StartStop ->
            { model | started = not model.started } ! []

        Tick _ ->
            { model | grid = evolveGrid model.grid } ! []


evolveGrid : Matrix Bool -> Matrix Bool
evolveGrid grid =
    let
        evolve : Location -> Bool -> Bool
        evolve location _ =
            getNeighbours location
                |> List.map (\loc -> Matrix.get loc grid)
                |> List.map evaluateCell
                |> List.sum
                |> evaluateResult

        getNeighbours : Location -> List Location
        getNeighbours ( x, y ) =
            [ Matrix.loc (x + 1) (y + 0)
            , Matrix.loc (x + 1) (y + 1)
            , Matrix.loc (x + 0) (y + 1)
            , Matrix.loc (x - 1) (y + 1)
            , Matrix.loc (x - 1) (y + 0)
            , Matrix.loc (x - 1) (y - 1)
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

        evaluateResult : Int -> Bool
        evaluateResult numberOfNeighbours =
            case numberOfNeighbours of
                3 ->
                    True

                _ ->
                    False
    in
        Matrix.mapWithLocation evolve grid


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.started then
        Time.every Time.second Tick
    else
        Sub.none


view : Model -> Svg Msg
view model =
    Html.div []
        [ viewControls
        , viewGrid model.grid
        ]


viewControls : Html Msg
viewControls =
    Html.div []
        [ Html.button [ onClick StartStop ] [ Html.text "play" ] ]


viewGrid : Matrix Bool -> Svg Msg
viewGrid grid =
    let
        squares =
            grid
                |> Matrix.mapWithLocation square
                |> Matrix.flatten
    in
        svg
            [ y "100"
            , width "600"
            , height "600"
            , viewBox ("0 0 10 10")
            ]
            squares


square : Location -> Bool -> Svg Msg
square location isAlive =
    rect
        [ onClick <| Flip location
        , x (toString <| Matrix.col location)
        , y (toString <| Matrix.row location)
        , width "1"
        , height "1"
        , strokeWidth "0.01"
        , stroke "black"
        , fill <|
            if isAlive then
                "black"
            else
                "white"
        ]
        [ text (toString location) ]
