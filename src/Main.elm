import Round
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


-- референтни вредности
referentnaVrednost = 34079.0
licnoOsloboduvanje = 7531.0

rounds : Float -> Float
rounds x = Round.roundNum 0 x

-- ограничувања
minNeto     = 12000.0
minBruto    = 17130.0
maxOsnovica = referentnaVrednost * 16
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
  { pdd = rounds (osnovica * d.pdd)
  }

sumaDanoci : Danoci -> Float
sumaDanoci d =
  d.pdd

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
  { penzisko    = rounds (bruto * p.penzisko)
  , zdravstveno = rounds (bruto * p.zdravstveno)
  , pridones    = rounds (bruto * p.pridones)
  , boluvanje   = rounds (bruto * p.boluvanje)
  }

sumaPridonesi : Pridonesi -> Float
sumaPridonesi p =
  p.penzisko + p.zdravstveno + p.pridones + p.boluvanje

vkupnoPridonesiProcenti : Float
vkupnoPridonesiProcenti = sumaPridonesi procentiPridonesi



-- Главни функции за конверзија од бруто во нето и обратно

bruto2neto : Float -> Model
bruto2neto bruto =
  let
    pridonesi = presmetajPridonesi bruto procentiPridonesi
    vkupnoPridonesi = sumaPridonesi pridonesi
    pddOsnovica = bruto - vkupnoPridonesi - licnoOsloboduvanje
    danoci = presmetajDanoci pddOsnovica procentiDanoci
    vkupnoDanoci = sumaDanoci danoci
    neto = bruto - vkupnoPridonesi - vkupnoDanoci
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

neto2bruto : Float -> Model
neto2bruto neto =
  let
    bruto = (neto - licnoOsloboduvanje * procentiDanoci.pdd) / (((1 - vkupnoPridonesiProcenti) - procentiDanoci.pdd) + (vkupnoPridonesiProcenti * procentiDanoci.pdd))
  in
    bruto2neto (rounds bruto)



-- имплементација на стандардните Elm ствари: main, model, update, view

main =
  Html.beginnerProgram
    { model  = model
    , view   = view
    , update = update
    }


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
      let
        fAmount = Result.withDefault 0 (String.toFloat amount)
      in
        if fAmount >= minBruto
        then bruto2neto fAmount
        else { model | bruto = fAmount }
    Neto amount ->
      let
        fAmount = Result.withDefault 0 (String.toFloat amount)
      in
        if fAmount >= minNeto
        then neto2bruto fAmount
        else { model | neto = fAmount }


view : Model -> Html Msg
view model =
  div []
    [ inputFields model
    , details model
    ]


inputFields : Model -> Html Msg
inputFields model =
  div []
    [ label []
      [ span [] [ text "Бруто : " ]
      , input [ type_ "text", placeholder "Бруто", onInput Bruto, value (toString model.bruto) ] []
      ]
    , label []
      [ span [] [ text "Нето : " ]
      , input [ type_ "text", placeholder "Нето", onInput Neto, value (toString model.neto) ] []
      ]
    ]

details : Model -> Html msg
details model =
  div []
    [ table []
      [ tr []
          [ td [] [ text "Придонеси за задолжително ПИО" ]
          , td [] [ text (toString (procentiPridonesi.penzisko * 100) ++ "%") ]
          , td [] [ text (toString model.pridonesi.penzisko) ]
          , td [] [ text "МКД" ]
          ]
      , tr []
          [ td [] [ text "Придонеси за задолжително здравствено осигурување" ]
          , td [] [ text (toString (procentiPridonesi.zdravstveno * 100) ++ "%") ]
          , td [] [ text (toString model.pridonesi.zdravstveno) ]
          , td [] [ text "МКД" ]
          ]
      , tr []
          [ td [] [ text "Придонеси за вработување" ]
          , td [] [ text (toString (procentiPridonesi.pridones * 100) ++ "%") ]
          , td [] [ text (toString model.pridonesi.pridones) ]
          , td [] [ text "МКД" ]
          ]
      , tr []
          [ td [] [ text "Дополнителен придонес за задолжително осигурување во случај повреда или професионално заболување" ]
          , td [] [ text (toString (procentiPridonesi.boluvanje * 100) ++ "%") ]
          , td [] [ text (toString model.pridonesi.boluvanje) ]
          , td [] [ text "МКД" ]
          ]
      , tr []
          [ td [] [ text "Вкупно придонеси" ]
          , td [] []
          , td [] [ text (toString model.vkupnoPridonesi) ]
          , td [] [ text "МКД" ]
          ]
      , tr []
          [ td [] [ text "Бруто плата намалена за придонеси" ]
          , td [] []
          , td [] [ text (toString model.brutoMinusPridonesi) ]
          , td [] [ text "МКД" ]
          ]
      , tr []
          [ td [] [ text "Лично ослободување" ]
          , td [] []
          , td [] [ text (toString licnoOsloboduvanje) ]
          , td [] [ text "МКД" ]
          ]
      , tr []
          [ td [] [ text "Даночна основа за пресметка на персонален данок на доход" ]
          , td [] []
          , td [] [ text (toString model.pddOsnovica) ]
          , td [] [ text "МКД" ]
          ]
      , tr []
          [ td [] [ text "Персонален данок на доход (ПДД)" ]
          , td [] [ text (toString (procentiDanoci.pdd * 100) ++ "%") ]
          , td [] [ text (toString model.danoci.pdd) ]
          , td [] [ text "МКД" ]
          ]
      , tr []
          [ td [] [ text "Вкупно придонеси и данок" ]
          , td [] []
          , td [] [ text (toString model.vkupnoDavacki) ]
          , td [] [ text "МКД" ]
          ]
      , tr []
          [ td [] [ text "Нето" ]
          , td [] []
          , td [] [ text (toString model.neto) ]
          , td [] [ text "МКД" ]
          ]
      ]
    ]
