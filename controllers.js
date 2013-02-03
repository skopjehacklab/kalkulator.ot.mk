function Calculator($scope) {

    // коефицинети
    $scope.k = {
       penzisko   : 0.18,
       zdravstveno: 0.073,
       pridones   : 0.012,
       boluvanje  : 0.005,
       personalen : 0.10
    }

    $scope.danocno_osloboduvanje = 7269;
    $scope.referentna_vrednost = 30595;
    $scope.max_osnovica_za_pridonesi = $scope.referentna_vrednost * 6;
    $scope.min_osnovica_za_pridonesi = Math.ceil($scope.referentna_vrednost / 2);
    $scope.min_neto_plata = 8050;

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
        var zaboluvanje = osnovica_za_pridonesi * $scope.k.boluvanje;
        var pridonesi   = penzisko + zdravstveno + pridones + zaboluvanje;

        var osnovica_za_danok = bruto - $scope.danocno_osloboduvanje - pridonesi;
        osnovica_za_danok = osnovica_za_danok > 0 ? osnovica_za_danok: 0;
        var personalec        = osnovica_za_danok * $scope.k.personalen;
        var davacki           = personalec + pridonesi;
        var neto              = bruto - davacki;

        $scope.personalec = personalec;
        $scope.osnovica_za_danok = osnovica_za_danok;
        $scope.pridonesi = pridonesi;

        return neto;
    }

    var neto2bruto = function (neto) {
        var pridonesi = $scope.k.penzisko + $scope.k.zdravstveno +
                        $scope.k.pridones + $scope.k.boluvanje;
        var k = (1 - pridonesi) * (1 - $scope.k.personalen);
        var bruto = (neto - $scope.danocno_osloboduvanje / 10) / k;
        return bruto;
    }

    $scope.bruto_change = function() {
        var bruto = parseFloat($scope.bruto.toString());
        $scope.neto = calculate(bruto);
    }

    $scope.neto_change = function() {
        var neto = parseFloat($scope.neto.toString());  // cheap way to clean the input
        var bruto = neto2bruto(neto);
        $scope.bruto = bruto;
        calculate(bruto);
    }

}
