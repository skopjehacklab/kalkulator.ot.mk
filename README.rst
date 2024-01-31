=============
plata.m.ie.mk
=============

Апликација за пресметка на нето и бруто плата, придонеси и данок на
личен доход.

Апликацијата е направена од хобисти, кои не се поврзани со УЈП или други
институции на државата, и не даваме никакви гаранции дека пресметките се
точни. Доколку сакате да бидете сигурни, најдобро е сами да си проверите
според `важечките законските одредби <http://www.ujp.gov.mk/mk/javnost/soopstenija/pogledni/945>`_
(податоци за 2019).

Апликацијата е јавно достапна на `plata.m.ie.mk <https://plata.m.ie.mk/>`_.

Развој
------

Апликацијата е направена со `elm 0.19 <https://elm-lang.org/>`_, посетете го нивниот сајт за да дознаете
повеќе за овој програмски јазик. Но, на кратко:

* апликацијата нема сервер, сѐ се случува во browser
* за локална работа, може да користите `elm reactor`
* за пуштање во продукција: `elm make src/Main.elm --output=dist/app.js --optimize`
* во нашиов случај, користиме `github actions <.github/workflows/>`_ кој откога ќе направи build на апликацијата, го закачува во gh-pages branch-от
  на истово repo.

.. image:: https://github.com/skopjehacklab/kalkulator.ot.mk/workflows/Build%20and%20Deploy/badge.svg
    :target: https://github.com/skopjehacklab/kalkulator.ot.mk/actions
