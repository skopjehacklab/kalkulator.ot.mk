module Danok exposing (Danoci, Model, bruto2neto, initModel, licnoOsloboduvanje, minBruto, minNeto, neto2bruto, procentiDanoci, procentiPridonesi)


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
    12165


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
