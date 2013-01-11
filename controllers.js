function Calculator($scope) {

    // коефицинети
    var ppenzisko = 0.18;
    var pzdravstveno = 0.073;
    var ppridones = 0.012;
    var pzaboluvanje = 0.005;
    var ppersonalen = 0.10;

    var danocno_osloboduvanje = 7470;
    var max_osnovica_za_pridonesi = 183546;
    var min_osnovica_za_pridonesi = 15295.50;

    var calculate = function (bruto) {
        var osnovica_za_pridonesi = bruto;

        if (bruto > max_osnovica_za_pridonesi) {
           osnovica_za_pridonesi = max_osnovica_za_pridonesi;
        } else if (bruto < min_osnovica_za_pridonesi) {
           osnovica_za_pridonesi = min_osnovica_za_pridonesi;
        }

        var penzisko    = osnovica_za_pridonesi * ppenzisko;
        var zdravstveno = osnovica_za_pridonesi * pzdravstveno;
        var pridones    = osnovica_za_pridonesi * ppridones;
        var zaboluvanje = osnovica_za_pridonesi * pzaboluvanje;
        var pridonesi   = penzisko + zdravstveno + pridones + zaboluvanje;

        var osnovica_za_danok = bruto - danocno_osloboduvanje - pridonesi;
        var personalen        = osnovica_za_danok * ppersonalen;
        var davacki           = personalen + pridonesi;
        var neto              = bruto - davacki;

        var vals = Array()
        vals.push({ name: 'Бруто плата',                            value: bruto.toFixed(2) });
        vals.push({ name: 'Придонес за пензискo осигурување', p: ppenzisko*100+'%',         value: penzisko.toFixed(2) });
        vals.push({ name: 'Придонес за здравствено осигурување', p: pzdravstveno*100+'%',   value: zdravstveno.toFixed(2) });
        vals.push({ name: 'Придонес за вработување', p: ppridones*100 + '%',                value: pridones.toFixed(2) });
        vals.push({ name: 'Придонес за професионално заболување', p: pzaboluvanje*100+'%',  value: zaboluvanje.toFixed(2) });
        vals.push({ name: 'Вкупно придонеси',                       value: pridonesi.toFixed(2) });
        vals.push({ name: 'Бруто - придонеси',                      value: (bruto - pridonesi).toFixed(2) });
        vals.push({ name: 'Даночно ослободување', p: danocno_osloboduvanje, value: danocno_osloboduvanje.toFixed(2) });
        vals.push({ name: 'Даночна основа',                         value: osnovica_za_danok.toFixed(2) });
        vals.push({ name: 'Персонален данок', p: ppersonalen*100+'%',       value: personalen.toFixed(2) });
        vals.push({ name: 'Вкупно придонеси и данок',               value: davacki.toFixed(2) });
        vals.push({ name: 'Нето плата',                             value: neto.toFixed(2) });

        return {neto: neto, vals: vals};
    }

    var neto2bruto = function (neto) {
        var pridonesi = ppenzisko + pzdravstveno + ppridones + pzaboluvanje;
        var k = (1 - pridonesi) * (1 - ppersonalen);
        var bruto = (neto - danocno_osloboduvanje / 10) / k;
        return bruto;
    }

    $scope.getBrutoVals = function() {
        var bruto = parseFloat($scope.bruto.toString());
        var result = calculate(bruto);
        $scope.vals =  result.vals;
        $scope.neto =  result.neto;
    }

    $scope.getNetoVals = function() {
        var bruto = neto2bruto(parseFloat($scope.neto.toString()));
        $scope.vals = calculate(bruto).vals;
        $scope.bruto = bruto.toFixed(2);
    }

}
