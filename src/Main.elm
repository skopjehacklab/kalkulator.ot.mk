module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


rounds : Float -> Int
rounds x =
    round x


referentnaVrednost : Int
referentnaVrednost =
    34079


licnoOsloboduvanje : Int
licnoOsloboduvanje =
    7531


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
    rounds ((toFloat referentnaVrednost) / 2)



-- Даноци (за сега само еден)


type alias Danoci number =
    { pdd : number -- персонален данок на добивка
    }


procentiDanoci : Danoci Float
procentiDanoci =
    { pdd = 0.1
    }


presmetajDanoci : Int -> Danoci Float -> Danoci Int
presmetajDanoci osnovica d =
    { pdd = rounds ((toFloat osnovica) * d.pdd)
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
    { penzisko = 0.18
    , zdravstveno = 0.073
    , pridones = 0.012
    , boluvanje = 0.005
    }


presmetajPridonesi : Int -> Pridonesi Float -> Pridonesi Int
presmetajPridonesi bruto p =
    { penzisko = rounds (toFloat bruto * p.penzisko)
    , zdravstveno = rounds (toFloat bruto * p.zdravstveno)
    , pridones = rounds (toFloat bruto * p.pridones)
    , boluvanje = rounds (toFloat bruto * p.boluvanje)
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
        vkupnoPridonesiProcenti =
            sumaPridonesi procentiPridonesi

        -- ова фејла
        bruto =
            (toFloat neto - ((toFloat licnoOsloboduvanje) * procentiDanoci.pdd)) / (((1 - vkupnoPridonesiProcenti) - procentiDanoci.pdd) + (vkupnoPridonesiProcenti * procentiDanoci.pdd))
    in
        bruto2neto (rounds bruto)


main : Platform.Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Bruto amount ->
            let
                fAmount =
                    Result.withDefault 0 (String.toInt amount)
            in
                if fAmount >= minBruto then
                    ( bruto2neto fAmount, Cmd.none )
                else
                    ( { model | bruto = fAmount }, Cmd.none )

        Neto amount ->
            let
                fAmount =
                    Result.withDefault 0 (String.toInt amount)
            in
                if fAmount >= minNeto then
                    ( neto2bruto fAmount, Cmd.none )
                else
                    ( { model | neto = fAmount }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEWS AND STYLES


containerStyle : Attribute msg
containerStyle =
    style
        [ ( "width", "600px" )
        , ( "left", "50%" )
        , ( "margin-left", "-300px" )
        , ( "position", "absolute" )
        , ( "vertical-align", "center" )
        , ( "font", "0.8em sans-serif" )
        ]


rowStyle : Attribute msg
rowStyle =
    style
        [ ( "border-bottom", "1px solid #afafaf" )
        , ( "padding", "15px" )
        ]


bold : Attribute msg
bold =
    style
        [ ( "font-weight", "bold" )
        ]


ribbon : Html Msg
ribbon =
    a [ href "https://github.com/skopjehacklab/kalkulator.ot.mk" ]
        [ img
            [ style
                [ ( "position", "absolute" )
                , ( "top", "0" )
                , ( "left", "0" )
                , ( "border", "0" )
                ]
            , src "https://s3.amazonaws.com/github/ribbons/forkme_left_orange_ff7600.png"
            , alt "Fork me on GitHub"
            ]
            []
        ]


splitter : Attribute msg
splitter =
    style
        [ ( "margin-bottom", "30px" )
        , ( "border-bottom", "5px solid #afafaf" )
        , ( "border-left", "5px solid #afafaf" )
        , ( "border-right", "5px solid #afafaf" )
        , ( "padding", "30px 25px" )
        , ( "background-color", "#fffcda" )
        ]


td : String -> Html Msg
td txt =
    Html.td [ rowStyle ] [ text txt ]


view : Model -> Html Msg
view model =
    div []
        [ div [] [ ribbon ]
        , div [ containerStyle ]
            [ inputFields model
            , details model
            ]
        ]


inputFields : Model -> Html Msg
inputFields model =
    table [ splitter ]
        [ tr []
            [ th [] [ text "Бруто" ]
            , th [] [ text "Нето" ]
            ]
        , tr []
            [ Html.td [] [ input [ type_ "text", placeholder "Бруто", onInput Bruto, value (toString model.bruto), style [ ( "width", "250px" ) ] ] [] ]
            , Html.td [] [ input [ type_ "text", placeholder "Нето", onInput Neto, value (toString model.neto), style [ ( "width", "250px" ) ] ] [] ]
            ]
        ]


details : Model -> Html Msg
details model =
    div [ style [ ( "margin", "0 0 50px 0" ) ] ]
        [ table []
            [ tr [ bold ]
                [ td "Бруто"
                , td ""
                , td (toString model.bruto)
                , td "МКД"
                ]
            , tr []
                [ td "Придонеси за задолжително ПИО"
                , td (toString (procentiPridonesi.penzisko * 100) ++ "%")
                , td (toString model.pridonesi.penzisko)
                , td "МКД"
                ]
            , tr []
                [ td "Придонеси за задолжително здравствено осигурување"
                , td (toString (procentiPridonesi.zdravstveno * 100) ++ "%")
                , td (toString model.pridonesi.zdravstveno)
                , td "МКД"
                ]
            , tr []
                [ td "Придонеси за вработување"
                , td (toString (procentiPridonesi.pridones * 100) ++ "%")
                , td (toString model.pridonesi.pridones)
                , td "МКД"
                ]
            , tr []
                [ td "Дополнителен придонес за задолжително осигурување во случај повреда или професионално заболување"
                , td (toString (procentiPridonesi.boluvanje * 100) ++ "%")
                , td (toString model.pridonesi.boluvanje)
                , td "МКД"
                ]
            , tr [ bold ]
                [ td "Вкупно придонеси"
                , td ""
                , td (toString model.vkupnoPridonesi)
                , td "МКД"
                ]
            , tr []
                [ td "Бруто плата намалена за придонеси"
                , td ""
                , td (toString model.brutoMinusPridonesi)
                , td "МКД"
                ]
            , tr []
                [ td "Лично ослободување"
                , td ""
                , td (toString licnoOsloboduvanje)
                , td "МКД"
                ]
            , tr []
                [ td "Даночна основа за пресметка на персонален данок на доход"
                , td ""
                , td (toString model.pddOsnovica)
                , td "МКД"
                ]
            , tr []
                [ td "Персонален данок на доход (ПДД)"
                , td (toString (procentiDanoci.pdd * 100) ++ "%")
                , td (toString model.danoci.pdd)
                , td "МКД"
                ]
            , tr []
                [ td "Вкупно придонеси и данок"
                , td ""
                , td (toString model.vkupnoDavacki)
                , td "МКД"
                ]
            , tr [ bold ]
                [ td "Нето"
                , td ""
                , td (toString model.neto)
                , td "МКД"
                ]
            ]
        ]
