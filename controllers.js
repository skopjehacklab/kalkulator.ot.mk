function Calculator($scope, $location, $timeout) {

    // коефицинети
    $scope.k = {
       penzisko   : 0.18,
       zdravstveno: 0.073,
       pridones   : 0.012,
       boluvanje  : 0.005,
       personalen : 0.10,
    }

    $scope.total_davacki_bez_personalen_koficent = $scope.k.penzisko + $scope.k.zdravstveno + $scope.k.pridones + $scope.k.boluvanje;

    $scope.danocno_osloboduvanje = 7531.0;
    $scope.referentna_vrednost = 34079.0;
    $scope.max_osnovica_za_pridonesi = $scope.referentna_vrednost * 16;
    $scope.min_osnovica_za_pridonesi = $scope.referentna_vrednost / 2;
    $scope.min_neto_plata = 12000.0;
    $scope.min_bruto_plata = 17130.0;

    var calculate = function (bruto) {
        var osnovica_za_pridonesi = bruto;

        if (bruto > $scope.max_osnovica_za_pridonesi) {
           osnovica_za_pridonesi = $scope.max_osnovica_za_pridonesi;
        } else
        if (bruto < $scope.min_osnovica_za_pridonesi) {
           osnovica_za_pridonesi = $scope.min_osnovica_za_pridonesi;
        }

        var penzisko    = Math.round(osnovica_za_pridonesi * $scope.k.penzisko);
        var zdravstveno = Math.round(osnovica_za_pridonesi * $scope.k.zdravstveno);
        var pridones    = Math.round(osnovica_za_pridonesi * $scope.k.pridones);
        var boluvanje   = Math.round(osnovica_za_pridonesi * $scope.k.boluvanje);
        var pridonesi   = penzisko + zdravstveno + pridones + boluvanje;

        $scope.penzisko    = penzisko;
        $scope.zdravstveno = zdravstveno;
        $scope.pridones    = pridones;
        $scope.boluvanje   = boluvanje;

        $scope.bruto_minus_pridonesi = bruto - pridonesi;
        var osnovica_za_danok = $scope.bruto_minus_pridonesi - $scope.danocno_osloboduvanje;
        osnovica_za_danok = osnovica_za_danok > 0 ? osnovica_za_danok: 0;
        var personalec        = Math.round(osnovica_za_danok * $scope.k.personalen);
        var davacki           = personalec + pridonesi;
        var neto              = bruto - davacki;

        $scope.personalec = personalec;
        $scope.osnovica_za_danok = osnovica_za_danok;
        $scope.pridonesi = pridonesi;

        return neto;
    }

    var neto2bruto = function (neto) {
        var danok = ((neto - $scope.danocno_osloboduvanje) * $scope.k.personalen) / (1 - $scope.k.personalen)
        var bruto = (neto + danok) / (1 - $scope.total_davacki_bez_personalen_koficent)
        return Math.round(bruto);
    }

    $scope.bruto_change = function() {
        var bruto = parseFloat($scope.bruto.toString());
        $scope.neto = calculate(bruto);
        if ($scope.neto < $scope.min_neto_plata) {
           $scope.myForm.bruto.$error.min = true;
           ['penzisko', 'zdravstveno', 'pridones', 'boluvanje', 'bruto_minus_pridonesi',
              'personalec', 'osnovica_za_danok', 'pridonesi', 'neto'].forEach(function (property) {
              delete $scope[property];
           });
           return;
        }
        $scope.myForm.bruto.$error.min = false;
    }

    $scope.neto_change = function() {
        var neto = parseFloat($scope.neto.toString());  // cheap way to clean the input
        if (neto < $scope.min_neto_plata) {
           $scope.myForm.neto.$error.min = true;
           return;
        }
        $scope.myForm.neto.$error.min = false;
        var bruto = neto2bruto(neto);
        $scope.bruto = bruto;
        calculate(bruto);
    }
    $scope.bruto = parseFloat($location.absUrl().split('?')[1]) || undefined;
    // view is not ready yet, so delay bruto_change
    if ($scope.bruto) $timeout(function() {
        $scope.bruto_change();
    }, 1);
}
