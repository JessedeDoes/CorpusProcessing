
# Dialectconstructies die moeilijk zijn voor Alpino


De example-based search van gretel zal voor sommige dialect constructies niet goed werken omdat Alpino de gebruikersinvoer 
niet goed parseert.  

https://hackmd.io/@amghysel/r1kMS8cC9

## "en" als negatie

`//node[./node[@word="en" and @rel="mod"]]`


## Pseudodirecte rede

_hij zei hij weet het niet_

met tag en nucl

Directe rede met "van"

//node[@rel="vc"  and @cat="svan"]
bijvoorbeeld met zeggen: //node[node[@rel="hd" and @lemma="zeggen"] and node[@rel="vc"  and @cat="svan"]]


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

# Zooi
Het is toch geen waar, etc
