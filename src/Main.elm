module Main exposing (main, update)

import Browser
import Danok exposing (Model, bruto2neto, initModel, minBruto, minNeto, neto2bruto)
import Views exposing (Msg(..), view)


main : Platform.Program () Model Msg
main =
    Browser.sandbox
        { init = initModel
        , view = view
        , update = update
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Bruto amount ->
            let
                fAmount =
                    Maybe.withDefault 0 (String.toInt amount)
            in
            if fAmount >= minBruto then
                bruto2neto fAmount

            else
                { model | bruto = fAmount }

        Neto amount ->
            let
                fAmount =
                    Maybe.withDefault 0 (String.toInt amount)
            in
            if fAmount >= minNeto then
                neto2bruto fAmount

            else
                { model | neto = fAmount }
