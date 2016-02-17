scala-utils
==============

Yleiskäyttöisiä scala-kirjastoja Opetushallituksen verkkokehitykseen

## Kehitys

* JDK 1.8, mutta target-version 1.7 (koska scala 2.11)
* Jokainen alimoduuli julkaisee oman jar-pakettinsa
* Alimoduulit mahdollisimman yksinkertaisina: [SRP](https://en.wikipedia.org/wiki/Single_responsibility_principle)
* Alimoduulilla oma versionumeronsa, nosta jos teet rikkovia muutoksia alimoduliin
* Lisää uudet java-luokat omiin alimoduuleihinsa, varsinkin jos liittyy kiinteästi olemassaolevaan pakettiin
  * jos teet muutoksia scala-utils alimoduulissa olevaan luokkaan niin:
    1. tee scala-utils:sta ensin release versio (poista sen versionumerosta SNAPSHOT pääte ja pushaa)
    2. päivitä uusi SNAPHSHOT versio
    3. tee uusi alimoduuli ja *siirrä* luokka sinne
    4. tee muutokset
    5. vaihda käyttävä projekti riippumaan uudesta alimoduulista ja tarvittaessa yhä scala-utilsin uudesta versiosta
* Rootin versionumeroa ei pitäisi olla tarvetta muokata
* OPH:n Bamboo ajaa "mvn clean deploy"-komennon mikä buildaa jar-paketit ja asentaa ne artifactoryyn

## Komentoja

    mvn test
