import Html exposing (Html, beginnerProgram, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
  Html.beginnerProgram
    { model  = model
    , view   = view
    , update = update
    }

-- Ограничувања и референтни вредности
referentna_vrednost       = 32877
danocno_osloboduvanje     = 7531
min_neto_plata            = 12000
max_osnovica_za_pridonesi = referentna_vrednost * 12
min_osnovica_za_pridonesi = referentna_vrednost / 2
min_bruto_plata           = referentna_vrednost / 2 -- може да се случи да е хаце

-- Константни коефициенти
type alias Davacki =
  { penzisko    : Float
  , zdravstveno : Float
  , pridones    : Float
  , boluvanje   : Float
  , personalen  : Float
  }

procenti : Davacki
procenti =
  { penzisko    = 0.18
  , zdravstveno = 0.073
  , pridones    = 0.012
  , boluvanje   = 0.005
  , personalen  = 0.10
  }

presmetajDavacki : Float -> Davacki -> Davacki
presmetajDavacki amount d =
  { penzisko    = amount * d.penzisko
  , zdravstveno = amount * d.zdravstveno
  , pridones    = amount * d.pridones
  , boluvanje   = amount * d.boluvanje
  , personalen  = amount * d.personalen
  }

toList : Davacki -> List Float
toList d =
  [ d.penzisko
  , d.zdravstveno
  , d.pridones
  , d.boluvanje
  , d.personalen
  ]

suma : Davacki -> Float
suma d =
  List.sum (toList d)

neto2bruto : Float -> Davacki -> Float
neto2bruto amount procenti =
  amount / (1 - suma procenti)

bruto2neto : Float -> Davacki -> Float
bruto2neto amount procenti =
  amount * (1 - (suma procenti))

type alias Model =
  { bruto   : Float
  , neto    : Float
  , davacki : Davacki
  }

model : Model
model =
  { bruto = 0
  , neto  = 0
  , davacki = procenti
  }


type Msg
  = Bruto String
  | Neto String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Bruto amount ->
      let
        bruto = Result.withDefault 0 (String.toFloat amount)
        neto = bruto2neto bruto procenti
        davacki = presmetajDavacki bruto procenti
      in
        { model | bruto = bruto, neto = neto, davacki = davacki }
    Neto amount ->
      let
        neto = Result.withDefault 0 (String.toFloat amount)
        bruto = neto2bruto neto procenti
        davacki = presmetajDavacki bruto procenti
      in
        { model | bruto = bruto, neto = neto, davacki = davacki }


view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Бруто", onInput Bruto, value (toString model.bruto) ] []
    , input [ type_ "text", placeholder "Нето", onInput Neto, value (toString model.neto) ] []
    , details model
    ]

details : Model -> Html msg
details model =
  div []
    [ div [] [ text ("Пензиско: " ++ toString model.davacki.penzisko) ]
    , div [] [ text ("Здравствено: " ++ toString model.davacki.zdravstveno) ]
    , div [] [ text ("Придонес: " ++ toString model.davacki.pridones) ]
    , div [] [ text ("Боледување: " ++ toString model.davacki.boluvanje) ]
    , div [] [ text ("Персонален: " ++ toString model.davacki.personalen) ]
    ]
