module Main exposing (Danoci, Model, Msg(..), Pridonesi, bold, bruto2neto, containerStyle, details, initModel, inputFields, licnoOsloboduvanje, main, maxOsnovica, minBruto, minNeto, minOsnovica, neto2bruto, od, presmetajDanoci, presmetajPridonesi, procentiDanoci, procentiPridonesi, referentnaVrednost, ribbon, rowStyle, splitter, sumaDanoci, sumaPridonesi, td, update, view)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

referentnaVrednost : Int
referentnaVrednost =
    34079


licnoOsloboduvanje : Int
licnoOsloboduvanje =
    8000


minNeto : Int
minNeto =
    12000


minBruto : Int
minBruto =
    17040


maxOsnovica : Int
maxOsnovica =
    referentnaVrednost * 16


minOsnovica : Int
minOsnovica =
    round (toFloat referentnaVrednost / 2)


od : Float -> Int -> Int
od x =
    toFloat >> (*) x >> round



-- Даноци (за сега само еден)


type alias Danoci number =
    { 
     pdd : number, -- персонален данок на добивка
     pdd1 : number -- персонален данок на добивка
    }


procentiDanoci : Danoci Float
procentiDanoci =
    { 
        pdd = 0.1,
        pdd1 = 0.18
    }


presmetajDanoci : Int -> Danoci Float -> Danoci Int
presmetajDanoci osnovica d =
    { 
        pdd = if osnovica<90000 then osnovica |> od d.pdd else 90000 |> od d.pdd,
        pdd1 = if osnovica > 90000 then
            (osnovica-90000) |> od d.pdd1 else 0 
    }


sumaDanoci : Danoci Int -> Int
sumaDanoci d =
    d.pdd



-- Константни коефициенти


type alias Pridonesi number =
    { penzisko : number
    , zdravstveno : number
    , pridones : number
    , boluvanje : number
    }


procentiPridonesi : Pridonesi Float
procentiPridonesi =
    { penzisko = 0.184
    , zdravstveno = 0.074
    , pridones = 0.012
    , boluvanje = 0.005
    }


presmetajPridonesi : Int -> Pridonesi Float -> Pridonesi Int
presmetajPridonesi bruto p =
    { penzisko = bruto |> od p.penzisko
    , zdravstveno = bruto |> od p.zdravstveno
    , pridones = bruto |> od p.pridones
    , boluvanje = bruto |> od p.boluvanje
    }


sumaPridonesi : Pridonesi number -> number
sumaPridonesi p =
    [ p.penzisko, p.zdravstveno, p.pridones, p.boluvanje ]
        |> List.sum



-- Главни функции за конверзија од бруто во нето и обратно


bruto2neto : Int -> Model
bruto2neto bruto =
    let
        pridonesi =
            presmetajPridonesi bruto procentiPridonesi

        vkupnoPridonesi =
            sumaPridonesi pridonesi

        pddOsnovica =
            bruto - vkupnoPridonesi - licnoOsloboduvanje

        danoci =
            presmetajDanoci pddOsnovica procentiDanoci

        vkupnoDanoci =
            sumaDanoci danoci

        neto =
            bruto - vkupnoPridonesi - vkupnoDanoci
    in
    { bruto = bruto
    , neto = neto
    , pridonesi = pridonesi
    , danoci = danoci
    , pddOsnovica = pddOsnovica
    , vkupnoDavacki = vkupnoPridonesi + vkupnoDanoci
    , vkupnoPridonesi = vkupnoPridonesi
    , brutoMinusPridonesi = bruto - vkupnoPridonesi
    }


neto2bruto : Int -> Model
neto2bruto neto =
    let
        vkupnoPridonesi =
            sumaPridonesi procentiPridonesi

        -- ова фејла (помалку)
        p =
            procentiDanoci.pdd * 100

        danok =
            (toFloat (neto - licnoOsloboduvanje) * p) / (100 - p)

        bruto =
            (toFloat neto + danok) / (1 - vkupnoPridonesi)
    in
    bruto2neto (floor bruto)


main : Platform.Program () Model Msg
main =
    Browser.sandbox
        { init = initModel
        , view = view
        , update = update
        }


type alias Model =
    { bruto : Int
    , neto : Int
    , pridonesi : Pridonesi Int
    , danoci : Danoci Int
    , pddOsnovica : Int
    , vkupnoDavacki : Int
    , vkupnoPridonesi : Int
    , brutoMinusPridonesi : Int
    }


initModel : Model
initModel =
    { bruto = 0
    , neto = 0
    , pridonesi = presmetajPridonesi 0 procentiPridonesi
    , danoci = presmetajDanoci 0 procentiDanoci
    , pddOsnovica = 0
    , vkupnoDavacki = 0
    , vkupnoPridonesi = 0
    , brutoMinusPridonesi = 0
    }


type Msg
    = Bruto String
    | Neto String


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



-- VIEWS AND STYLES


containerStyle : List (Attribute msg)
containerStyle =
    [ style "width" "600px"
    , style "left" "50%"
    , style "margin-left" "-300px"
    , style "position" "absolute"
    , style "vertical-align" "center"
    , style "font" "0.8em sans-serif"
    ]


rowStyle : List (Attribute msg)
rowStyle =
    [ style "border-bottom" "1px solid #afafaf"
    , style "padding" "15px"
    ]


bold : List (Attribute msg)
bold =
    [ style "font-weight" "bold"
    ]


ribbon : Html Msg
ribbon =
    a [ href "https://github.com/skopjehacklab/kalkulator.ot.mk" ]
        [ img
            [ style "position" "absolute"
            , style "top" "0"
            , style "left" "0"
            , style "border" "0"
            , src "https://s3.amazonaws.com/github/ribbons/forkme_left_orange_ff7600.png"
            , alt "Fork me on GitHub"
            ]
            []
        ]


splitter : List (Attribute msg)
splitter =
    [ style "margin-bottom" "30px"
    , style "border-bottom" "5px solid #afafaf"
    , style "border-left" "5px solid #afafaf"
    , style "border-right" "5px solid #afafaf"
    , style "padding" "30px 25px"
    , style "background-color" "#fffcda"
    ]


td : String -> Html Msg
td txt =
    Html.td rowStyle [ text txt ]


view : Model -> Html Msg
view model =
    div []
        [ div [] [ ribbon ]
        , div containerStyle
            [ inputFields model
            , details model
            ]
        ]


inputFields : Model -> Html Msg
inputFields model =
    table splitter
        [ tr []
            [ th [] [ text "Бруто" ]
            , th [] [ text "Нето" ]
            ]
        , tr []
            [ Html.td [] [ input [ type_ "text", placeholder "Бруто", onInput Bruto, value (String.fromInt model.bruto), style "width" "250px" ] [] ]
            , Html.td [] [ input [ type_ "text", placeholder "Нето", onInput Neto, value (String.fromInt model.neto), style "width" "250px" ] [] ]
            ]
        ]


details : Model -> Html Msg
details model =
    div [ style "margin" "0 0 50px 0" ]
        [ table []
            [ tr bold
                [ td "Бруто"
                , td ""
                , td (String.fromInt model.bruto)
                , td "МКД"
                ]
            , tr []
                [ td "Придонеси за задолжително ПИО"
                , td (String.fromFloat (procentiPridonesi.penzisko * 100) ++ "%")
                , td (String.fromInt model.pridonesi.penzisko)
                , td "МКД"
                ]
            , tr []
                [ td "Придонеси за задолжително здравствено осигурување"
                , td (String.fromFloat (procentiPridonesi.zdravstveno * 100) ++ "%")
                , td (String.fromInt model.pridonesi.zdravstveno)
                , td "МКД"
                ]
            , tr []
                [ td "Придонеси за вработување"
                , td (String.fromFloat (procentiPridonesi.pridones * 100) ++ "%")
                , td (String.fromInt model.pridonesi.pridones)
                , td "МКД"
                ]
            , tr []
                [ td "Дополнителен придонес за задолжително осигурување во случај повреда или професионално заболување"
                , td (String.fromFloat (procentiPridonesi.boluvanje * 100) ++ "%")
                , td (String.fromInt model.pridonesi.boluvanje)
                , td "МКД"
                ]
            , tr bold
                [ td "Вкупно придонеси"
                , td ""
                , td (String.fromInt model.vkupnoPridonesi)
                , td "МКД"
                ]
            , tr []
                [ td "Бруто плата намалена за придонеси"
                , td ""
                , td (String.fromInt model.brutoMinusPridonesi)
                , td "МКД"
                ]
            , tr []
                [ td "Лично ослободување"
                , td ""
                , td (String.fromInt licnoOsloboduvanje)
                , td "МКД"
                ]
            , tr []
                [ td "Даночна основа за пресметка на персонален данок на доход"
                , td ""
                , td (String.fromInt model.pddOsnovica)
                , td "МКД"
                ]
            , tr []
                [ td "Данок на Личен доход (ДЛД)"
                , td (String.fromFloat (toFloat (round ((toFloat(model.danoci.pdd + model.danoci.pdd1)/toFloat(model.pddOsnovica)) * 10000)) / 100) ++ "%")
                , td (String.fromInt (model.danoci.pdd + model.danoci.pdd1))
                , td "МКД"
                ]
            , tr []
                [ td "Вкупно придонеси и данок"
                , td ""
                , td (String.fromInt model.vkupnoDavacki)
                , td "МКД"
                ]
            , tr bold
                [ td "Нето"
                , td ""
                , td (String.fromInt model.neto)
                , td "МКД"
                ]
            ]
        ]
