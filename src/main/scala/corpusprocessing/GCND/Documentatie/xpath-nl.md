###### Cheat Sheet

Basis XPath-syntaxis:
* / - Selecteert vanaf de root node.
* // - Selecteert nodes overal in het document.
* . - Vertegenwoordigt de "huidige node".
* .. - Vertegenwoordigt de ouder van de huidige node.

Selectors:

* element - Selecteert alle elementen met de gegeven naam. (Meestal `node`)
* @attribute - Selecteert de waarde van het opgegeven attribuut.
* `*`  Selecteert alle elementen.
* text() - Selecteert de tekst binnen een element. _(Zelden nuttig in Gretel)_
* [predicate] - Voegt een voorwaarde toe om nodes te filteren.

Predicaten:
* [@name='waarde'] - Selecteert nodes met de opgegeven attribuutwaarde.
* [position()] - Selecteert nodes op basis van hun positie.
* [last()] - Selecteert de laatste node van een gegeven type.
* [contains(@attribute, 'waarde')] - Selecteert nodes met attribuutwaarden die 'waarde' bevatten.
* [not(predicate)] - Ontkent een voorwaarde.

Assen:
* ancestor:: - Selecteert alle voorouders.
* ancestor-or-self:: - Selecteert voorouders en de huidige node.
* child:: - Selecteert alle kinderen.
* descendant:: - Selecteert alle afstammelingen.
* descendant-or-self:: - Selecteert afstammelingen en de huidige node.
* following:: - Selecteert alle volgende nodes.
* following-sibling:: - Selecteert volgende broers/zussen.
* parent:: - Selecteert de ontmiddelijke parent node.
* preceding:: - Selecteert alle voorgaande nodes.
* preceding-sibling:: - Selecteert voorgaande broers/zussen.
* self:: - Selecteert de huidige node.

Operatoren
* = - Gelijk aan.
* != - Niet gelijk aan.
* < - Minder dan.
* <= - Minder dan of gelijk aan.
* `>` - Groter dan.
* `>=` - Groter dan of gelijk aan.
* and - Logisch EN.
* or - Logisch OF.
* not - Logisch NIET.

Functies (Voorbeelden):

* name() - Geeft de naam van de huidige node terug. _(Zelden nuttig in Gretel, is bijna altijd 'node')_
* count(nodes) - Geeft het aantal nodes in de node-set terug.
* concat(string1, string2) - Voegt twee strings samen. _(Zelden nuttig in Gretel)_
* substring(string, start, length) - Geeft een substring terug.
* contains(string, substr) - Controleert of een string een substring bevat.
* matches(string, pattern) - Controleert of een string aan een reguliere expressie voldoet.
* normalize-space(string) - Verwijdert voor- en achterliggende witruimtes en comprimeert spaties.

Voorbeelden:

* //node - Selecteert alle nodes
* `//*[@pt='ww']` - Selecteert elementen met het attribuut pt gelijk aan 'ww'.
* //node/node[position()=1] - Selecteert alle nodes die eerste element binnen hun parent zijn
* //node[@cat='pp']/node[@cat='np'] - Selecteert noun phrases direct binnen een propositional phrase 
* //node[matches(@word,'.*end$')] - Selecteert woorden die op 'end' eindigen
