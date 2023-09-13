module Views exposing (Msg(..), view)

import Danok exposing (Model, licnoOsloboduvanje, procentiDanoci, procentiPridonesi)
import Html exposing (..)
import Html.Attributes exposing (align, alt, href, placeholder, rel, src, style, target, title, type_, value)
import Html.Events exposing (onInput)
import Round


type Msg
    = Bruto String
    | Neto String


containerStyle : List (Attribute msg)
containerStyle =
    [ style "width" "600px"
    , style "left" "50%"
    , style "margin-left" "-300px"
    , style "position" "absolute"
    , style "vertical-align" "center"
    , style "font" "0.8em sans-serif"
    ]


bold : List (Attribute msg)
bold =
    [ style "font-weight" "bold"
    ]


ribbon : Html Msg
ribbon =
    div [ style "position" "absolute"
        , style "top" "42px"
        , style "width" "200px"
        , style "padding" "1px 0"
        , style "background" "#000"
        , style "color" "#eee"
        , style "-moz-box-shadow" "0 0 10px rgba(0,0,0,0.5)"
        , style "-webkit-box-shadow" "0 0 10px rgba(0,0,0,0.5)"
        , style "box-shadow" "0 0 10px rgba(0,0,0,0.5)"
        , style "left" "-42px"
        , style "-moz-transform" "rotate(-45deg)"
        , style "-webkit-transform" "rotate(-45deg)"
        , style "-o-transform" "rotate(-45deg)"
        , style "-ms-transform" "rotate(-45deg)"
        , style "transform" "rotate(-45deg)"
        , style "background" "#0a0"
        ]
        [ a [ href "https://github.com/skopjehacklab/kalkulator.ot.mk"
            , style "display" "block"
            , style "padding" "1px 0"
            , style "height" "24px"
            , style "line-height" "24px"
            , style "color" "inherit"
            , style "text-align" "center"
            , style "text-decoration" "none"
            , style "font-family" "'Cabin Condensed', sans-serif"
            , style "font-size" "16px"
            , style "font-weight" "500"
            , style "border" "1px solid rgba(255,255,255,0.3)"
            , style "-moz-text-shadow" "0 0 10px rgba(0,0,0,0.31)"
            , style "-webkit-text-shadow" "0 0 10px rgba(0,0,0,0.31)"
            , style "text-shadow" "0 0 10px rgba(0,0,0,0.31)"
            ]
            [ text "Fork me on GitHub" ]
        ]


pdfLinkTxt : String
pdfLinkTxt =
    "Закон за данокот на личен доход"


pdfLinkStyle : List (Attribute msg)
pdfLinkStyle =
    [ style "float" "right"
    , style "padding" "10px"
    , style "font-size" "12px"
    , style "background-color" "#fffcda"
    , style "color" "#000000"
    ]


pdfLink : Html Msg
pdfLink =
    a
        (pdfLinkStyle
            ++ [ href "http://www.ujp.gov.mk/mk/regulativa/opis/425", title pdfLinkTxt, target "_blank", rel "noopener" ]
        )
        [ text pdfLinkTxt ]


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


rowStyle : List (Attribute msg)
rowStyle =
    [ style "border-bottom" "1px solid #afafaf"
    , style "padding" "15px"
    ]


td : String -> Html Msg
td txt =
    Html.td (align "right" :: rowStyle) [ text txt ]


tdLeft : String -> Html Msg
tdLeft txt =
    Html.td rowStyle [ text txt ]


inputFields : Model -> Html Msg
inputFields model =
    table splitter
        [ tr []
            [ th [] [ text "Бруто" ]
            , th [] [ text "Нето" ]
            ]
        , tr []
            [ Html.td [] [ input ([ title "Бруто, износ кој ја вклучува чистата плата што ја добива работникот (нето-плата) заедно со сите јавни давачки (даноци и придонеси), во бруто-платата се вклучени надоместоците кои ги добиваат вработените за храна и за превоз", type_ "number", placeholder "Бруто", onInput Bruto, value (String.fromInt model.bruto) ] ++ inputStyle) [] ]
            , Html.td [] [ input ([ title "Нето, чистата плата што ја добива работникот на својата трансакциска сметка", type_ "number", placeholder "Нето", onInput Neto, value (String.fromInt model.neto) ] ++ inputStyle) [] ]
            ]
        ]


infoIcon : Html Msg
infoIcon =
    span
        [ style "display" "inline-block"
        , style "width" "16px"
        , style "height" "16px"
        , style "text-align" "center"
        , style "border-radius" "50%"
        , style "background" "#9898ea"
        , style "color" "#fff"
        , style "margin-right" "5px"
        , style "user-select" "none"
        ]
        [ i [] [ text "i" ] ]


details : Model -> Html Msg
details model =
    div [ style "margin" "0 0 50px 0" ]
        [ table []
            [ tr bold
                [ tdLeft "Бруто"
                , td ""
                , td (String.fromInt model.bruto)
                , td "МКД"
                ]
            , tr []
                [ tdLeft "Придонеси за задолжително ПИО"
                , td (Round.round 2 (procentiPridonesi.penzisko * 100) ++ "%")
                , td (String.fromInt model.pridonesi.penzisko)
                , td "МКД"
                ]
            , tr []
                [ tdLeft "Придонеси за задолжително здравствено осигурување"
                , td (Round.round 2 (procentiPridonesi.zdravstveno * 100) ++ "%")
                , td (String.fromInt model.pridonesi.zdravstveno)
                , td "МКД"
                ]
            , tr []
                [ tdLeft "Придонес за осигурување во случај на невработеност"
                , td (Round.round 2 (procentiPridonesi.nevrabotenost * 100) ++ "%")
                , td (String.fromInt model.pridonesi.nevrabotenost)
                , td "МКД"
                ]
            , tr []
                [ tdLeft "Дополнителен придонес за задолжително осигурување во случај повреда или професионално заболување"
                , td (Round.round 2 (procentiPridonesi.boluvanje * 100) ++ "%")
                , td (String.fromInt model.pridonesi.boluvanje)
                , td "МКД"
                ]
            , tr bold
                [ tdLeft "Вкупно придонеси"
                , td ""
                , td (String.fromInt model.vkupnoPridonesi)
                , td "МКД"
                ]
            , tr []
                [ tdLeft "Бруто плата намалена за придонеси"
                , td ""
                , td (String.fromInt model.brutoMinusPridonesi)
                , td "МКД"
                ]
            , tr []
                [ tdLeft "Лично ослободување"
                , td ""
                , td (String.fromInt licnoOsloboduvanje)
                , td "МКД"
                ]
            , tr []
                [ tdLeft "Даночна основа за пресметка на данок на личен доход"
                , td ""
                , td (String.fromInt model.dldOsnova10)
                , td "МКД"
                ]
            , tr []
                [ tdLeft "Данок на личен доход"
                , td (Round.round 2 (procentiDanoci.dld10 * 100) ++ "%")
                , td (String.fromInt model.danoci.dld10)
                , td "МКД"
                ]
            , tr []
                [ tdLeft "Вкупно придонеси и данок"
                , td ""
                , td (String.fromInt model.vkupnoDavacki)
                , td "МКД"
                ]
            , tr bold
                [ tdLeft "Нето"
                , td ""
                , td (String.fromInt model.neto)
                , td "МКД"
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [] [ ribbon ]
        , div [] [ pdfLink ]
        , div containerStyle
            [ inputFields model
            , details model
            ]
        ]
