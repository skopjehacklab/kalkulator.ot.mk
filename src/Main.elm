module Main exposing (Danoci, Model, Msg(..), Pridonesi, bold, bruto2neto, containerStyle, details, initModel, inputFields, inputStyle, licnoOsloboduvanje, main, maxOsnovica, minBruto, minNeto, minOsnovica, neto2bruto, od, presmetajDanoci, presmetajPridonesi, procentiDanoci, procentiPridonesi, referentnaVrednost, ribbon, rowStyle, splitter, sumaDanoci, sumaPridonesi, td, update, view)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (alt, href, placeholder, src, style, type_, value)
import Html.Events exposing (onInput)
import Round


maxSafeInt : Int
maxSafeInt =
    2 ^ 53 - 1


maxBrutoNetoOdnos : Int
maxBrutoNetoOdnos =
    10


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
    referentnaVrednost // 2


limit : Int
limit =
    90000


od : Float -> Int -> Int
od x =
    toFloat >> (*) x >> round


type alias Danoci number =
    { dld10 : number -- данок на личен доход од 10%
    , dld18 : number -- данок на личен доход од 18%
    }


procentiDanoci : Danoci Float
procentiDanoci =
    { dld10 = 0.1
    , dld18 = 0.18
    }


presmetajDanoci : Int -> Danoci Float -> Danoci Int
presmetajDanoci osnova d =
    { dld10 = min limit osnova |> od d.dld10
    , dld18 = max 0 (osnova - limit) |> od d.dld18
    }


sumaDanoci : Danoci number -> number
sumaDanoci d =
    [ d.dld10, d.dld18 ]
        |> List.sum



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

        dldOsnova =
            bruto - vkupnoPridonesi - licnoOsloboduvanje

        dldOsnova10 =
            min limit dldOsnova

        dldOsnova18 =
            max 0 (dldOsnova - limit)

        danoci =
            presmetajDanoci dldOsnova procentiDanoci

        vkupnoDanoci =
            sumaDanoci danoci

        neto =
            bruto - vkupnoPridonesi - vkupnoDanoci
    in
    { bruto = bruto
    , neto = neto
    , pridonesi = pridonesi
    , danoci = danoci
    , dldOsnova10 = dldOsnova10
    , dldOsnova18 = dldOsnova18
    , vkupnoDavacki = vkupnoPridonesi + vkupnoDanoci
    , vkupnoPridonesi = vkupnoPridonesi
    , brutoMinusPridonesi = bruto - vkupnoPridonesi
    }


findBruto : Int -> Int
findBruto netoVal =
    let
        val =
            clamp minNeto maxSafeInt netoVal
    in
    binSearch val val (val * maxBrutoNetoOdnos)


binSearch : Int -> Int -> Int -> Int
binSearch searchValue lo hi =
    let
        mid =
            lo + floor (toFloat (hi - lo) / 2)

        value =
            (bruto2neto mid).neto
    in
    if searchValue < value then
        binSearch searchValue lo (mid - 1)

    else if searchValue > value then
        binSearch searchValue (mid + 1) hi

    else
        mid


neto2bruto : Int -> Model
neto2bruto val =
    bruto2neto (findBruto val)


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
    , dldOsnova10 : Int
    , dldOsnova18 : Int
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
    , dldOsnova10 = 0
    , dldOsnova18 = 0
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
    , style "width" "600px"
    ]


inputStyle : List (Attribute msg)
inputStyle =
    [ style "box-sizing" "border-box"
    , style "line-height" "1.25"
    , style "padding" ".5rem .75rem"
    , style "background-clip" "padding-box"
    , style "width" "250px"
    , style "border-radius" ".25rem"
    , style "border" "1px solid rgba(0,0,0,.15)"
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
            [ Html.td [] [ input ([ type_ "number", placeholder "Бруто", onInput Bruto, value (String.fromInt model.bruto) ] ++ inputStyle) [] ]
            , Html.td [] [ input ([ type_ "number", placeholder "Нето", onInput Neto, value (String.fromInt model.neto) ] ++ inputStyle) [] ]
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
                , td (Round.round 2 (procentiPridonesi.penzisko * 100) ++ "%")
                , td (String.fromInt model.pridonesi.penzisko)
                , td "МКД"
                ]
            , tr []
                [ td "Придонеси за задолжително здравствено осигурување"
                , td (Round.round 2 (procentiPridonesi.zdravstveno * 100) ++ "%")
                , td (String.fromInt model.pridonesi.zdravstveno)
                , td "МКД"
                ]
            , tr []
                [ td "Придонеси за вработување"
                , td (Round.round 2 (procentiPridonesi.pridones * 100) ++ "%")
                , td (String.fromInt model.pridonesi.pridones)
                , td "МКД"
                ]
            , tr []
                [ td "Дополнителен придонес за задолжително осигурување во случај повреда или професионално заболување"
                , td (Round.round 2 (procentiPridonesi.boluvanje * 100) ++ "%")
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
                [ td "Даночна основа за пресметка на данок на личен доход со 10% (за даночна основа под 90.000 денари)"
                , td ""
                , td (String.fromInt model.dldOsnova10)
                , td "МКД"
                ]
            , tr []
                [ td "Данок на личен доход"
                , td (Round.round 2 (procentiDanoci.dld10 * 100) ++ "%")
                , td (String.fromInt model.danoci.dld10)
                , td "МКД"
                ]
            , tr []
                [ td "Даночна основа за пресметка на данок на личен доход со 18% (за даночна основа над 90.000 денари)"
                , td ""
                , td (String.fromInt model.dldOsnova18)
                , td "МКД"
                ]
            , tr []
                [ td "Данок на личен доход"
                , td (Round.round 2 (procentiDanoci.dld18 * 100) ++ "%")
                , td (String.fromInt model.danoci.dld18)
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
