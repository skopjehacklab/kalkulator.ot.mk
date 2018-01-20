function Calculator($scope, $location, $timeout) {

    // коефицинети
    $scope.k = {
       penzisko   : 0.18,
       zdravstveno: 0.073,
       pridones   : 0.012,
       boluvanje  : 0.005,
       personalen : 0.10,
       neto_to_bruto_konficent: 0.657   // n = b * 73% - (b * 7.3% - du / 10) => b = (n - du / 10) / 0.657       
    }

    $scope.total_davacki_bez_personalen_koficent = $scope.k.penzisko + $scope.k.zdravstveno + $scope.k.pridones + $scope.k.boluvanje;
    $scope.k.neto_to_bruto_konficent = 1 - ($scope.total_davacki_bez_personalen_koficent) - (1 - ($scope.total_davacki_bez_personalen_koficent)) * $scope.k.personalen;

    $scope.danocno_osloboduvanje = 7531;
    $scope.referentna_vrednost = 32877;
    $scope.max_osnovica_za_pridonesi = $scope.referentna_vrednost * 12;
    $scope.min_osnovica_za_pridonesi = Math.round($scope.referentna_vrednost / 2);
    $scope.min_neto_plata = 9590;
    $scope.min_bruto_plata = 16134; // 50% од 32.268

    var calculate = function (bruto) {
        var osnovica_za_pridonesi = bruto;

        if (bruto > $scope.max_osnovica_za_pridonesi) {
           osnovica_za_pridonesi = $scope.max_osnovica_za_pridonesi;
        } else
        if (bruto < $scope.min_osnovica_za_pridonesi) {
           osnovica_za_pridonesi = $scope.min_osnovica_za_pridonesi;
        }

        var penzisko    = osnovica_za_pridonesi * $scope.k.penzisko;
        var zdravstveno = osnovica_za_pridonesi * $scope.k.zdravstveno;
        var pridones    = osnovica_za_pridonesi * $scope.k.pridones;
        var boluvanje   = osnovica_za_pridonesi * $scope.k.boluvanje;
        var pridonesi   = penzisko + zdravstveno + pridones + boluvanje;

        $scope.penzisko    = Math.round(penzisko);   
        $scope.zdravstveno = Math.round(zdravstveno);
        $scope.pridones    = Math.round(pridones);  
        $scope.boluvanje   = Math.round(boluvanje);

        $scope.bruto_minus_pridonesi = bruto - pridonesi;
        var osnovica_za_danok = $scope.bruto_minus_pridonesi - $scope.danocno_osloboduvanje;
        osnovica_za_danok = osnovica_za_danok > 0 ? osnovica_za_danok: 0;
        var personalec        = osnovica_za_danok * $scope.k.personalen;
        var davacki           = personalec + pridonesi;
        var neto              = bruto - davacki;

        $scope.personalec = Math.round(personalec);
        $scope.osnovica_za_danok = osnovica_za_danok;
        $scope.pridonesi = pridonesi;

        return neto;
    }

    var neto2bruto = function (neto) {
        var bruto = (neto - $scope.danocno_osloboduvanje / 10) / $scope.k.neto_to_bruto_konficent;
        
        var personalec = (bruto * (1 - $scope.total_davacki_bez_personalen_koficent) - $scope.danocno_osloboduvanje) * $scope.k.personalen;

        var osnovica_za_pridonesi = bruto;
        if (bruto > $scope.max_osnovica_za_pridonesi) {
            osnovica_za_pridonesi = $scope.max_osnovica_za_pridonesi;
        } else
        if (bruto < $scope.min_osnovica_za_pridonesi) {
                osnovica_za_pridonesi = $scope.min_osnovica_za_pridonesi;
        }
        var penzisko = osnovica_za_pridonesi * $scope.k.penzisko;
        var zdravstveno = osnovica_za_pridonesi * $scope.k.zdravstveno;
        var pridones = osnovica_za_pridonesi * $scope.k.pridones;
        var boluvanje = osnovica_za_pridonesi * $scope.k.boluvanje;
        bruto = neto + personalec + penzisko + zdravstveno + pridones + boluvanje;

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
