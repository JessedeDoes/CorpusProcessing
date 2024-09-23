
# XPath-queries voor dialectconstructies


De example-based search van gretel zal voor sommige dialect constructies niet goed werken omdat Alpino de gebruikersinvoer 
niet goed parseert.  

https://hackmd.io/@amghysel/r1kMS8cC9

## "en" als negatie

`//node[./node[@word="en" and @rel="mod"]]`


## Pseudodirecte rede


## 2. Subjectsverschijnselen

### 2.1 subjectverdubbeling

_Ik heb ik ik dat niet gezegd._

`//node[count(./node[@rel='su']) > 1]`



### 2.1 subject in objectvorm

_omdat hem peinsde dat dat zijn kindje was._

`//node[@rel="su" and @word="hem"]`

`//node[@rel="su" and @naamval="obl"]`


### 2.3 Presentatief 'het'

Deze constructie wordt noch in de Lassy-handleiding noch in de CGN-beschrijving besproken. 
In het GCND zullen we dit soort het zoals presentatief er behandelen, en het dus het dependentielabel 'MOD' te geven.
Adjectieven met "geen".

Vindbaar met:

`//node[@rel='mod' and word='het']`

De resultaten zijn soms een beetje verwarrend

## 3. Uitbreidingen van de zin: TAG en SAT

#### 3.1.1 Left dislocation (topicalisatie)

_Jan, die ken ik niet_

Lijkt goed te gaan in Alpino.

Herkenbaar aan rel=sat en cat=np of pos="noun"

`//node[@rel='sat' and (@cat='np' or @pos='noun')]`

#### 3.1.2. Tussenwerpsels en aansporingen

* _zo, dat was plezant._

* _natuurlijk, moeilijk is het niet._

* _kom, ik ga er maar vandoor._

* _jongens, ik vertrek nu._

Met dit soort structuren kan Alpino doorgaans vlotjes om; preprocessing is dan ook niet nodig.

Geanalyseerd met tag en nucl

#### 3.1.3. Hanging Topic / Hangend Topic / Nominativus Pendens:

_**mijn vent** wist **hij** ook niet wat dat was en nu komt ..._

er staat steeds een naamwoordgroep in de eerste positie, die later in de zin door een persoonlijk voornaamwoord (hij, het, zij, hem, haar) wordt opgenomen

`//node[@rel='tag' and (@cat='np' or @pos='noun')]`

#### 3.1.4. Inversieloos V-later-dan-2 / V>2 / Noninverted V3

_zeg **als je nu trouwt** het zijn altijd voort kosten._

`//node[@rel="tag" and @cat="cp"]`

Mogelijk ook: `//node[@rel="tag" and @cat="pp"]`

_**in de zomer** t e klaar tot sn avonds t negenen_

### 3.2 Ingebedde dislocaties

_Wat vindt u der eigenlijk van dat zulke zinnen dat die zo geanalyseerd worden?_

Die gaan weet met sat

### 3.3 ja/nee het/ik/…

* _Bwa nee het jong_
* _ja **ja ze** het is heel juist_


`//node[@rel='tag'][node[@rel='mwp' and @pt='tsw'] and node[@rel='mwp' and @pos='pron']]`

### 3.4 V2-bijzinnen - pseudodirecte rede

Het is toch geen waar, etc

* _hij zei hij weet het niet_
* _ik zeg gisterenavond , ik moet de auto binnensteken ut tut tut ._
* _ik zeg , steek hem binnen ._

(Laatste met sv1, verschil met smain niet zo duidelijk?)


* Inleidende matrixzin (hij zei):

    Dependentielabel (rel): tag
    Categorielabel (cat): smain

Pseudodirecte rede - V2-bijzin (hij weet het niet):

    Depentielabel (rel): nucl
    Categorielabel (cat): smain (of – bij werkwoordsinitiële zinnen – sv1)

`//node[./node[@rel='tag' and @cat='smain'] and node[@rel='nucl' and (@cat='smain' or @cat='sv1')]]`


NB: Alpino parset directe en pseudodirecte redes doorgaans automatisch juist als je een komma toevoegt tussen de matrixzin en de V2-bijzin.


### 3.5 Intercalaties/parentheses/interpositio

Let op: afwijking van lassy: In het GCND kiezen we ervoor parentheses het dependentielabel TAG te geven en op hetzelfde niveau als de hoofdzin onder te brengen . 


`//node[node[@rel="tag"][.//node[@pos="verb"]]] and node[@cat="smain"]]`

Geeft zinnetjes met tag (@rel='tag' and @cat='smain') zou ook al wat zijn.

`node[@rel="tag" and @cat='smain'][../node[@cat='smain' and @rel='nucl']/@begin < @begin]`

Dit werkt niet....


## 4. Complementizer-fenomenen

### 4.1 Afwijkende comparatieve voegwoorden (of, als, gelijk als, gelijk of dat)

Bijvoorbeeld voor "of"

* _maar het scheelt meer **of de helft** ._
* _dat is veel langer **als dat** ik ik ben ._

Voor 'of' bijvoorbeeld:

`//node[@rel='obcomp'][./node[@rel='cmp' and @word='of']]`

Meerwoordige voegwoordelijke combinaties:

`//node[@rel='obcomp'][./node[@rel='cmp' and @cat='mwu']]`

### 4.2 Directe rede ingeleid door van


* _ja die zeggen van , als we daar in de tranchée en zaten ..._

Vindbaar met: 

`//node[@rel="vc"  and @cat="svan"]`

Bijvoorbeeld beperkt tot combinatie met "zeggen"

`//node[node[@rel="hd" and @lemma="zeggen"] and node[@rel="vc"  and @cat="svan"]]`


### 4.3 Expletief dat

#### Type 1: na onderschikkend voegwoord
* Ik weet niet of dat hij komt.
* Om het te zeggen gelijk of dat het is: …
* ik was getrouwd sinds dat hij nog bij het leger was

`//node[@cat='cp']/node[@rel='cmp' and @cat='mwu'][./node[@word="dat"]]`

#### Type 2: na vraagwoord
* Ik weet niet wie dat er komt.
* we gaan weer moeten de tijd afwachten wat dat er allemaal gaat voorvallen

`//node[@word="wie" and @rel="whd"][following-sibling::node[./node[@word="dat" and @pt="vg"]]]`

#### Type 3: na betrekkelijk voornaamwoord

* De mens die dat jou moet helpen, zal vloeken.

#### Type 4: na vraagwoord + of (zeldzaam in Vlaanderen, cf. Lassy-handleiding)

* Zijn er meer mogelijkheden dan wat of dat je nu hebt?

