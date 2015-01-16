function Calculator($scope, $location, $timeout) {

    // коефицинети
    $scope.k = {
       penzisko   : 0.18,
       zdravstveno: 0.073,
       pridones   : 0.012,
       boluvanje  : 0.005,
       personalen : 0.10
    }

    $scope.danocno_osloboduvanje = 7320;
    $scope.referentna_vrednost = 30595;
    $scope.max_osnovica_za_pridonesi = $scope.referentna_vrednost * 6;
    $scope.min_osnovica_za_pridonesi = Math.round($scope.referentna_vrednost / 2);
    $scope.min_neto_plata = 8050;
    $scope.min_bruto_plata = 12268; // fixme: треба да се пресмета не да се фиксира

    var calculate = function (bruto) {
        var osnovica_za_pridonesi = bruto;

        if (bruto > $scope.max_osnovica_za_pridonesi) {
           osnovica_za_pridonesi = $scope.max_osnovica_za_pridonesi;
        } else
        if (bruto < $scope.min_osnovica_za_pridonesi) {
           osnovica_za_pridonesi = $scope.min_osnovica_za_pridonesi;
        }

        var penzisko    = $scope.penzisko    = Math.round(osnovica_za_pridonesi * $scope.k.penzisko);
        var zdravstveno = $scope.zdravstveno = Math.round(osnovica_za_pridonesi * $scope.k.zdravstveno);
        var pridones    = $scope.pridones    = Math.round(osnovica_za_pridonesi * $scope.k.pridones);
        var boluvanje   = $scope.boluvanje   = Math.round(osnovica_za_pridonesi * $scope.k.boluvanje);
        var pridonesi   = penzisko + zdravstveno + pridones + boluvanje;

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
        // still wrong
        var pridonesi = $scope.k.penzisko + $scope.k.zdravstveno +
                        $scope.k.pridones + $scope.k.boluvanje;
        var k = (1 - pridonesi) * (1 - $scope.k.personalen);
        var bruto = (neto - $scope.danocno_osloboduvanje / 10) / k;
        return bruto;
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
