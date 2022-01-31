module Danok exposing (Danoci, Model, bruto2neto, initModel, licnoOsloboduvanje, minBruto, minNeto, neto2bruto, procentiDanoci, procentiPridonesi)


type alias Model =
    { bruto : Int
    , neto : Int
    , pridonesi : Pridonesi Int
    , danoci : Danoci Int
    , dldOsnova10 : Int
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
    , vkupnoDavacki = 0
    , vkupnoPridonesi = 0
    , brutoMinusPridonesi = 0
    }


maxSafeInt : Int
maxSafeInt =
    2 ^ 53 - 1


maxBrutoNetoOdnos : Int
maxBrutoNetoOdnos =
    10


prosecnaPlata : Int
prosecnaPlata =
    43509


licnoOsloboduvanje : Int
licnoOsloboduvanje =
    8438


minNeto : Int
minNeto =
    15229


minBruto : Int
minBruto =
    22146


maxOsnovica : Int
maxOsnovica =
    prosecnaPlata * 16


minOsnovica : Int
minOsnovica =
    prosecnaPlata // 2


od : Float -> Int -> Int
od x =
    toFloat >> (*) x >> round


type alias Danoci number =
    { dld10 : number -- данок на личен доход од 10%
    }


procentiDanoci : Danoci Float
procentiDanoci =
    { dld10 = 0.1
    }


presmetajDanoci : Int -> Danoci Float -> Danoci Int
presmetajDanoci osnova d =
    { dld10 = osnova |> od d.dld10
    }


sumaDanoci : Danoci number -> number
sumaDanoci d =
    d.dld10


type alias Pridonesi number =
    { penzisko : number
    , zdravstveno : number
    , pridones : number
    , boluvanje : number
    }


procentiPridonesi : Pridonesi Float
procentiPridonesi =
    { penzisko = 0.188
    , zdravstveno = 0.075
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
        osnovica = min bruto maxOsnovica

        pridonesi =
            presmetajPridonesi osnovica procentiPridonesi

        vkupnoPridonesi =
            sumaPridonesi pridonesi

        dldOsnova =
            bruto - vkupnoPridonesi - licnoOsloboduvanje

        dldOsnova10 =
            dldOsnova

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
