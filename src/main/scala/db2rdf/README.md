Proces om diamant rdf in elkaar te sleutelen
============================================

* Schema diamant bevat informatie over de synonymdefinities
* Dat wordt in elkaar gezet in project Hilex/diamant uit de door Boukje bewerkte tabellen 
  in diamant_ruw van de svowdb06
  * Zie aldaar voor de details (importSynonymDefinitions.scala)
* In Hilex moet je het schema diamant herimporteren
* Er moet een query worden gedraaid om veld wdb in de tabel data.documents aan te vullen

  ```update data.documents set wdb='MNW' from data.token_attestations a where a.wdb='MNW'  and data.documents.wdb is null and a.document_id=data.documents.document_id;```

* Verder moet je een query draaien om sense labels te laden
  * Dat is nu wel gedaan maar gaat opnieuw moete als rehierarchisering aangepast wordt
* Verder: schema serpens ook overnemen in Hilex!
* Vergeet niet dat voor de sense-attestaties informatie uit de tabel diamant.sense_attestations nodig is
 (anders heeft MNW helemaal geen sense attestaties!)
    * Die tabel lijkt overigens voor het MNW niet compleet te zijn en moet dus uit de woordenboeken 
    opnieuw worden gemaakt! (zie weer Hilex/diamant, senseAttestation.scala)
    * We hebben nu een tabel diamant.sense_attestations_reloaded direct uit de woordenboekdata
      * todo documenteer diamant schema en schoon op wat niet meer gebruikt wordt
    * Wat deden we precies met attestatie en subbetekenissen? Niet toevoegen toch?

* We maken afzonderlijke nq.gz dumps
  * poslist
     * distincte pos uit data.lemmata
  * lemmata
     * tabel data.lemmata
  * wordforms
     * tabellen data.lemmata, data.analyzed_wordforms, data.wordforms
  * senses 
     * tabel diamant.senses
  * attestations
     * voor de woordvormattestaties:  
     * voor de senseattestaties: 
  * ezels
  * serpens
* How maken we een jena database uit de dumps?
    
    
Overige zaken
=============

* In aanvullingsdelen zijn senses gemist (peetjes gebleven) Zoek bijvoorbeeld naar A009723.eg.extra.1, en bestandje
gemiste_citaten_niet_in_sense in Hilex/data
* In de query: voor pos liever geen eindslash, dus 
  ```<http://rdf.ivdnt.org/lexica/diamant/entry/MNW/74555> <http://universaldependencies.org/u/pos> <http://universaldependencies.org/u/pos/ADJ> <http://rdf.ivdnt.org/lexica/diamant/v1.5/>```
 