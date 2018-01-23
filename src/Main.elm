import Html exposing (Html, beginnerProgram, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
  Html.beginnerProgram
    { model  = model
    , view   = view
    , update = update
    }


-- референтни вредности
referentnaVrednost = 32877
licnoOsloboduvanje = 7456

-- ограничувања
minNeto     = 12000
minBruto    = 17300
maxOsnovica = referentnaVrednost * 12
minOsnovica = referentnaVrednost / 2

-- Даноци (за сега само еден)
type alias Danoci =
  { pdd : Float -- персонален данок на добивка
  }

procentiDanoci : Danoci
procentiDanoci =
  { pdd = 0.10
  }

presmetajDanoci : Float -> Danoci -> Danoci
presmetajDanoci osnovica d =
  { pdd = osnovica * d.pdd
  }

-- Константни коефициенти
type alias Pridonesi =
  { penzisko    : Float
  , zdravstveno : Float
  , pridones    : Float
  , boluvanje   : Float
  }

procentiPridonesi : Pridonesi
procentiPridonesi =
  { penzisko    = 0.18
  , zdravstveno = 0.073
  , pridones    = 0.012
  , boluvanje   = 0.005
  }

presmetajPridonesi : Float -> Pridonesi -> Pridonesi
presmetajPridonesi bruto p =
  { penzisko    = bruto * p.penzisko
  , zdravstveno = bruto * p.zdravstveno
  , pridones    = bruto * p.pridones
  , boluvanje   = bruto * p.boluvanje
  }

pridonesiToList : Pridonesi -> List Float
pridonesiToList p =
  [ p.penzisko
  , p.zdravstveno
  , p.pridones
  , p.boluvanje
  ]

sumaPridonesi : Pridonesi -> Float
sumaPridonesi p =
  List.sum (pridonesiToList p)

vkupnoPridonesiProcenti : Float
vkupnoPridonesiProcenti = sumaPridonesi procentiPridonesi



bruto2neto : Float -> Pridonesi -> Model
bruto2neto bruto procenti =
  let
    pridonesi = presmetajPridonesi bruto procentiPridonesi
    vkupnoPridonesi = sumaPridonesi pridonesi
    pddOsnovica = bruto - vkupnoPridonesi - licnoOsloboduvanje
    danoci = presmetajDanoci pddOsnovica procentiDanoci
    neto = bruto - vkupnoPridonesi - danoci.pdd
  in
    { bruto = bruto
    , neto = neto
    , pridonesi = pridonesi
    , danoci = danoci
    , pddOsnovica = pddOsnovica
    , vkupnoDavacki = vkupnoPridonesi + danoci.pdd
    , vkupnoPridonesi = vkupnoPridonesi
    , brutoMinusPridonesi = bruto - vkupnoPridonesi
    }

neto2bruto : Float -> Pridonesi -> Model
neto2bruto neto procenti =
  let
    pdd = ((neto - licnoOsloboduvanje) * procentiDanoci.pdd) / (1 - procentiDanoci.pdd)
    bruto = (neto + pdd) / (1 - vkupnoPridonesiProcenti)
  in
    bruto2neto bruto procentiPridonesi


type alias Model =
  { bruto : Float
  , neto : Float
  , pridonesi : Pridonesi
  , danoci : Danoci
  , pddOsnovica : Float
  , vkupnoDavacki : Float
  , vkupnoPridonesi : Float
  , brutoMinusPridonesi : Float
  }

model : Model
model =
  { bruto = 0
  , neto = 0
  , pridonesi = procentiPridonesi
  , danoci = procentiDanoci
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
      bruto2neto (Result.withDefault 0 (String.toFloat amount)) procentiPridonesi
    Neto amount ->
      neto2bruto (Result.withDefault 0 (String.toFloat amount)) procentiPridonesi


view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Бруто", onInput Bruto, value (toString (round model.bruto)) ] []
    , input [ type_ "text", placeholder "Нето", onInput Neto, value (toString (round model.neto)) ] []
    , details model
    ]

details : Model -> Html msg
details model =
  div []
    [ div [] [ text ("Придонеси за задолжително ПИО: " ++ toString (round model.pridonesi.penzisko)) ]
    , div [] [ text ("Придонеси за задолжително здравствено осигурување: " ++ toString (round model.pridonesi.zdravstveno)) ]
    , div [] [ text ("Придонес за вработување: " ++ toString (round model.pridonesi.pridones)) ]
    , div [] [ text ("Дополнителен придонес за задолжително осигурување во случај повреда или професионално заболување: " ++ toString (round model.pridonesi.boluvanje)) ]
    , div [] [ text ("Вкупно придонеси: " ++ toString (round model.vkupnoPridonesi)) ]
    , div [] [ text ("Бруто плата намалена за придонеси: " ++ toString (round model.brutoMinusPridonesi)) ]
    , div [] [ text ("Лично ослободување: " ++ toString (round licnoOsloboduvanje)) ]
    , div [] [ text ("Даночна основа за пресметка на персонален данок на доход: " ++ toString (round model.pddOsnovica)) ]
    , div [] [ text ("Вкупно придонеси и данок: " ++ toString (round model.vkupnoDavacki)) ]
    , div [] [ text ("Нето: " ++ toString (round model.neto)) ]
    ]
