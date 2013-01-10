function Calculator() {

    var ppenzisko = 0.18;
    var pzdravstveno = 0.073;
    var ppridones = 0.012;
    var pzaboluvanje = 0.005;
    var ppersonalen = 0.10;
    var osloboduvanje = 7269;

    var calculate = function (bruto) {
        var penzisko    = bruto * ppenzisko;
        var zdravstveno = bruto * pzdravstveno;
        var pridones    = bruto * ppridones;
        var zaboluvanje = bruto * pzaboluvanje;
        var pridonesi = penzisko + zdravstveno + pridones + zaboluvanje;
        var osnova = bruto - osloboduvanje - pridonesi;
        var personalen = osnova * ppersonalen;
        var davacki = personalen + pridonesi;
        var neto = bruto - davacki;
        vals = Array()
        vals.push({ name: 'Бруто плата',                            value: bruto.toFixed(2) });
        vals.push({ name: 'Придонес за пензискo осигурување', p: ppenzisko*100+'%',         value: penzisko.toFixed(2) });
        vals.push({ name: 'Придонес за здравствено осигурување', p: pzdravstveno*100+'%',   value: zdravstveno.toFixed(2) });
        vals.push({ name: 'Придонес за вработување', p: ppridones*100 + '%',                value: pridones.toFixed(2) });
        vals.push({ name: 'Придонес за професионално заболување', p: pzaboluvanje*100+'%',  value: zaboluvanje.toFixed(2) });
        vals.push({ name: 'Вкупно придонеси',                       value: pridonesi.toFixed(2) });
        vals.push({ name: 'Бруто - придонеси',                      value: (bruto - pridonesi).toFixed(2) });
        vals.push({ name: 'Даночно ослободување', p: osloboduvanje,                         value: osloboduvanje.toFixed(2) });
        vals.push({ name: 'Даночна основа',                         value: osnova.toFixed(2) });
        vals.push({ name: 'Персонален данок', p: ppersonalen*100+'%',                       value: personalen.toFixed(2) });
        vals.push({ name: 'Вкупно придонеси и данок',               value: davacki.toFixed(2) });
        vals.push({ name: 'Нето плата',                             value: neto.toFixed(2) });
        return vals;
    }

    var neto2bruto = function (neto) {
        var pridonesi = ppenzisko + pzdravstveno + ppridones + pzaboluvanje;
        var k = (1 - pridonesi) * (1 - ppersonalen);
        var bruto = (neto - osloboduvanje / 10) / k;
        return bruto;
    }

    this.getBrutoVals = function() {
        var bruto = parseFloat(this.bruto.toString());
        this.vals = calculate(bruto);
        this.neto = vals.slice(-1)[0].value;
    }

    this.getNetoVals = function() {
        var bruto = neto2bruto(parseFloat(this.neto.toString()));
        this.vals = calculate(bruto);
        this.bruto = bruto.toFixed(2);
    }

}
