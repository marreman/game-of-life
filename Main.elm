module Main exposing (..)

import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Matrix exposing (Matrix, Location)


main =
    Html.beginnerProgram
        { model = model
        , update = update
        , view = view
        }


type alias Model =
    { grid : Matrix Bool }


type alias Position =
    ( Int, Int )


model : Model
model =
    { grid = Matrix.matrix 10 10 (\_ -> False) }


type Msg
    = Flip Location


update : Msg -> Model -> Model
update msg model =
    case msg of
        Flip location ->
            { model | grid = Matrix.update location not model.grid }


view : Model -> Svg Msg
view model =
    let
        squares =
            model.grid
                |> Matrix.mapWithLocation square
                |> Matrix.flatten
    in
        svg
            [ width "600"
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
        , strokeWidth "0.05"
        , stroke "black"
        , fill <|
            if isAlive then
                "black"
            else
                "white"
        ]
        []
