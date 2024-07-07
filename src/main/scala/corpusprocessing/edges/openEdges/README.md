# Edges dataconversie


## Data

Paden zijn nu hard coded in de scripts. Deze instellingen staan in Settings.scala

* De input data directory is `/mnt/Projecten/Corpora/Historische_Corpora/EDGeS_historical_bible_corpus/OpenEDGeS/OpenEDGeS_v1.01/`
* De TEI target directory is `/mnt/Projecten/Corpora/Historische_Corpora/EDGeS_historical_bible_corpus/TEI/`

## Tools
* Deze kluwen corpusprocessingtooltjes
* DutchTagger voor het tokenizeren van de TEI
* `fast_align` voor het toevoegen van wordalignment
## Stappen
* Conversie naar TEI met verse alignment (output naar `/tmp/Bible`): (duurt ongeveer 25 minuten op mijn PC thuis)
```bash 
java -Xmx20g corpusprocessing.edges.openEdges.dehelezooi
```
* Tokenizeren (met de oude java tagger)
```bash
cd /mnt/Projecten/Corpora/CorporaTools/DutchTagger
bash tokenize.sh $indir $outdir
```
* Word id's aanpassen om globaal uniek te zijn: run object `makeWordIdsGloballyUnique`. Input in teiDir + `tokenized`, Output komt in teiDir + `ids-fixed` terecht
* Word alignment toevoegen met fast align: run `corpusprocessing.edges.openEdges.addWordAlignmentForAll`. 
Doe dit bij voorkeur op de rekenserver. 
