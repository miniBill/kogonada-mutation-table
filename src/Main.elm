module Main exposing (main)

import Browser
import Html exposing (Html, button, div, li, text, ul)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Random.Extra



{-

   the rules are as follows:
   roll 1d4 to decide the amount of mutations
   mutation types:
   a: 1d400
   b: 1d100+400
   c: 1d200+400

   results:
   1: a
   2: a,b
   3: a,b,c
   4: 2a, b, c
-}


type alias Model =
    List Int


type Msg
    = Roll
    | Rolled (List Int)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( [], Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        ]
        [ button [ onClick Roll ] [ text "Roll" ]
        , if List.isEmpty model then
            text ""

          else
            ul []
                (li []
                    [ text ("1d4 roll: " ++ String.fromInt (List.length model))
                    ]
                    :: List.indexedMap viewRoll model
                )
        ]


viewRoll : Int -> Int -> Html msg
viewRoll index result =
    let
        label : String
        label =
            case index of
                0 ->
                    "1st"

                1 ->
                    "2nd"

                2 ->
                    "3rd"

                3 ->
                    "4th"

                _ ->
                    "nth"
    in
    li [] [ text (label ++ " roll: " ++ String.fromInt result) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, roll )

        Rolled result ->
            ( result, Cmd.none )


roll : Cmd Msg
roll =
    let
        a : Random.Generator Int
        a =
            Random.int 1 400

        b : Random.Generator Int
        b =
            Random.int 401 500

        c : Random.Generator Int
        c =
            Random.int 401 600
    in
    Random.int 1 4
        |> Random.andThen
            (\f ->
                (case f of
                    1 ->
                        [ a ]

                    2 ->
                        [ a, b ]

                    3 ->
                        [ a, b, c ]

                    4 ->
                        [ a, a, b, c ]

                    _ ->
                        []
                )
                    |> Random.Extra.combine
            )
        |> Random.generate Rolled


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
