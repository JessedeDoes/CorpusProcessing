package rdf

import scala.xml.{Elem, XML}

object addDiagramsToHTML extends App
{

  val stuff = <div>
    <head>The basic attestation model</head>
    <ontology source="data/Diamant/diamant.fss"></ontology>
    <example>
      :lemma_78332 a ontolex:LexicalEntry .
      :lemma_78332 ontolex:canonicalForm :lemmavorm_78332 .
      :lemma_78332 rdfs:label "kat"@nl .
      :lemma_78332 rdf:type ontolex:Word .
      :lemmavorm_78332 rdf:type ontolex:Form .
      :lemmavorm_78332 ontolex:writtenRepresentation "kat"@nl .
      :lemmavorm_78332 ontolex:phoneticRepresentation "*k ɑ t"@nl-ReadSpeaker-fonipa .
      :lemma_78332 ontolex:sense :sensebet1.0_78333 .
      :sensebet1.0_78333 lemon:definition :sensebet1.0_78333_definition .
      :sensebet1.0_78333 int:senseOrder 1 .

      :sensebet1.0_78333_definition lemon:value "klein huisdier dat miauwt en spint en dat gehouden wordt als gezelschapsdier of voor het vangen van muizen en ratten; huiskat; poes"@nl .
      :sensebet1.0_78333_definition int:definitionComplement "Zowel in toepassing op dieren van het vrouwelijk als van het mannelijk geslacht."@nl .

      :lemma_78332 ontolex:sense :sensebet1.1_195644 .
      :sensebet1.1_195644 lemon:definition :sensebet1.1_195644_definition .
      :sensebet1.1_195644 int:subsenseOrder 1 .

      :sensebet1.1_195644_definition lemon:value "beeld of afbeelding van een kat, bv. in de kunst"@nl .

      :relation_sensebet1.1_195644 rdf:type vartrans:SenseRelation .
      :relation_sensebet1.1_195644 vartrans:source :sense_bet1.0_78333  .
      :relation_sensebet1.1_195644 vartrans:target :sense_bet1.1_195644  .
      :relation_sensebet1.1_195644 vartrans:category :metonymie  .

      :lemma_78332 ontolex:sense :sensebet2.0_78334 .
      :sensebet2.0_78334 lemon:definition :sensebet2.0_78334_definition .
      :sensebet2.0_78334 int:senseOrder 2 .
      :sensebet2.0_78334_definition lemon:value "roofdier dat behoort tot de familie van de katachtigen (Felidae), bv. een tijger, leeuw of huiskat; katachtige"@nl .
      :sensebet2.0_78334_definition int:definitionComplement "Vaak in het meervoud."@nl .
      :lemma_78332 ontolex:sense :sensebet3.0_78335 .
      :sensebet3.0_78335 lemon:definition :sensebet3.0_78335_definition .
      :sensebet3.0_78335 int:senseOrder 3 .
      :sensebet3.0_78335_definition lemon:value "bitse, vinnige vrouw; bits of vinnig meisje"@nl .
      :lemma_78332 ontolex:sense :sensebet4.0_78336 .
      :sensebet4.0_78336 lemon:definition :sensebet4.0_78336_definition .
      :sensebet4.0_78336 int:senseOrder 4 .
      :sensebet4.0_78336_definition lemon:value "bitse opmerking; snauw"@nl .
      :lemma_78332 ontolex:sense :sensebet5.0_204707 .
      :sensebet5.0_204707 lemon:definition :sensebet5.0_204707_definition .
      :sensebet5.0_204707 int:senseOrder 5 .
      :sensebet5.0_204707_definition lemon:value "middeleeuws belegeringswerktuig in de vorm van een lage, langwerpige houten constructie op rollen, die de aanvallers tegen een vestingmuur plaatsten om beschut en ongehinderd een bres in de muur te slaan, een muur te ondergraven of een vestinggracht te dempen"@nl .
      :sensebet5.0_204707_definition int:definitionComplement "Alleen in historiserend taalgebruik."@nl .
      :lemma_78332 ontolex:sense :sensebet6.0_204722 .
      :sensebet6.0_204722 lemon:definition :sensebet6.0_204722_definition .
      :sensebet6.0_204722 int:senseOrder 6 .
      :sensebet6.0_204722_definition lemon:value "hoog boven de wal uitstekende opstellingsplaats voor geschut, op een bastion of elders in een verdedigingswerk"@nl .
      :sensebet6.0_204722_definition int:definitionComplement "Alleen in historiserend taalgebruik."@nl .
      :lemma_78332 ontolex:sense :sensebet7.0_204817 .
      :sensebet7.0_204817 lemon:definition :sensebet7.0_204817_definition .
      :sensebet7.0_204817 int:senseOrder 7 .
      :sensebet7.0_204817_definition lemon:value "toestel waaraan een takel hangt en dat langs een geleidingsbaan wordt voortbewogen om lasten tussen vaste punten te vervoeren; loopkat"@nl .
      :lemma_78332 ontolex:sense :sensebet8.0_204831 .
      :sensebet8.0_204831 lemon:definition :sensebet8.0_204831_definition .
      :sensebet8.0_204831 int:senseOrder 8 .
      :sensebet8.0_204831_definition lemon:value "gesel van touwen met geknoopte uiteinden waarmee men vroeger misdadige matrozen of slaven strafte; geselkat"@nl .
      :sensebet8.0_204831_definition int:definitionComplement "Alleen in historiserend taalgebruik en meestal in de verbinding kat met de negen (of zeven) staarten."@nl .

    </example>
  </div>

  val document = if (args.size > 0) XML.load(args(0)) else stuff
  val outputDocument = if (args.size > 0) args(0).replaceAll(".html$", ".diagrams.html") else "aap.html"
  val imageDirectory = if (args.size > 0) {
    val dir = new java.io.File(args(0)).getParentFile
    val subdir = new java.io.File(dir.getAbsolutePath + "/" + "images")
    if (!subdir.isDirectory) subdir.mkdir()
    subdir
  } else {
    val d = new java.io.File("./temp")
    if (!d.isDirectory) d.mkdir()
    d
  }

  val xstuff = diagrams.processOntologies(diagrams.processExamples(document, imageDirectory).asInstanceOf[Elem],imageDirectory)
  //Console.err.println(xstuff)
  XML.save(outputDocument, xstuff, "UTF-8")
}
