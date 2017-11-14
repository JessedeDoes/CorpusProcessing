package sat
import Proposition._
object Mapping {

  val basicClauses = And(
    l("pos=INTJ") → l("cgn=TSW"),
    l("pos=NOUN") →  l("cgn=ZNW"),
    l("pos=PROPN") → (l("cgn=SPEC") ∧ l("cgn=deeleigen")),
    l("pos=VERB") ∧ l("mood=Part") → (l("cgn=deelwoord") ∧ l("cgn=WW")),
    l("mood=Part") →l("pos=VERB")
  )

  def mapToCGN(p:Proposition):Proposition =
  {
    val cgnTags:Set[String] = basicClauses.varsIn.filter(s => s.startsWith("cgn"))

    val translation = cgnTags.filter(t => {
      val lit = Literal(t)
      val extended = And(basicClauses, ¬(lit), p)
      !extended.isSatisfiable
    })
    val b = translation.map(Literal(_)).toList
    And(b: _*)
  }

  def main(args: Array[String]) =
  {
    val p  = l("mood=Part")
    val q = mapToCGN(p)
    println(s"$p => $q")
  }
}

object CHN
{
  val featureMapping = Array(Array("pos", "ADJ", "pos", "AA"), Array("pos", "ADJ", "pos", "ADJ"), Array("pos", "ADV", "pos", "ADV"), Array("pos", "ADV", "pos", "AA", "position", "oth|pred"), Array("pos", "INTJ", "pos", "INT"), Array("pos", "NOUN", "pos", "NOU-C"), Array("pos", "PROPN", "pos", "NOU-P"), Array("pos", "VERB", "pos", "VRB"), Array("pos", "ADP", "pos", "ADP"), Array("pos", "AUX", "pos", "VRB"), // HM
    Array("pos", "DET", "pos", "ART"), // HM; alleen bij historische corpora
    Array("pos", "DET", "pos", "PD", "position", "prenom"), //
    Array("pos", "PRON", "pos", "PD", "position", "pron"), // HM, zo krijg je ook de determiners
    Array("pos", "NUM", "pos", "NUM"), Array("pos", "CCONJ", "pos", "CONJ", "type", "coor"), Array("pos", "SCONJ", "pos", "CONJ", "type", "sub"), Array("pos", "PUNCT", "pos", "RES"), // HM hebben we niet
    Array("pos", "SYM", "pos", "RES"), Array("pos", "X", "pos", "RES"), // adp features
    Array("AdpType", "Prep", "type", "pre"), Array("AdpType", "Post", "type", "post"), // nominal features
    Array("Number", "Plur", "number", "pl"), Array("Number", "Sing", "number", "sg"), Array("Gender", "Fem", "gender", "f"), Array("Gender", "Masc", "gender", "m"), Array("Gender", "Neut", "gender", "n"), Array("Gender", "Com", "gender", "f", "gender", "m"), // HM, not implemented in regex or otherwise
    // adjective (formal=infl-e)? Hoe doe je dat in UD??)
    Array("Degree", "Pos", "degree", "pos"), Array("Degree", "Cmp", "degree", "comp"), Array("Degree", "Sup", "degree", "sup"), //{"Degree", "Dim"
    Array("Position", "Postnom", "position", "postnom"), Array("Position", "Prenom", "position", "prenom"), Array("Position", "Free", "position", "oth|pred"), Array("Position", "Nom", "position", "oth|pred"), Array("Position", "Nom", "position", "pron"), Array("Case", "Gen", "case", "gen"), // {"Definiteness", "Def", "formal", "infl-e"}, ?? For instance german adjectives in UD?
    // numeral
    Array("NumType", "Card", "type", "card"), Array("NumType", "Ord", "type", "ord"), // verbal features
    Array("Mood", "Ind", "finiteness", "fin"), Array("Mood", "Imp", "finiteness", "fin"), Array("Mood", "Sub", "finiteness", "fin"), Array("VerbForm", "Fin", "finiteness", "fin"), Array("VerbForm", "Inf", "finiteness", "inf"), Array("VerbForm", "Inf", "finiteness", "ger|inf"), Array("VerbForm", "Part", "finiteness", "part"), Array("Person", "1", "person", "1"), // TODO dit zit niet in CHN. Waarom niet?? te behoudend. Aanpassen na taggeroptimalisatie
    Array("Person", "2", "person", "2"), Array("Person", "3", "person", "3"), Array("Tense", "Past", "tense", "past"), Array("Tense", "Pres", "tense", "pres"), // pronoun / determiner / article
    // UD heeft: Art (ldiwoord)	Dem	(aanwijzend) Emp (nadruk)	Exc (uitroepend)	Ind	(onbepaald) Int	Neg	Prs (persoonlijk)	Rcp (reciprocal)	Rel (betrekkelijk)	Tot (collectief: iedereen enzo)
    Array("PronType", "Art", "subtype", "art-def"), Array("PronType", "Art", "subtype", "art-indef"), Array("PronType", "Dem", "type", "dem"), Array("PronType", "Dem", "type", "d-p"), Array("PronType", "Prs", "type", "pers"), Array("PronType", "Rel", "type", "rel"), Array("PronType", "Rel", "type", "w-p"), // is dat zo?
    Array("PronType", "Rel", "type", "d-p"), Array("PronType", "Rcp", "type", "recip"), Array("PronType", "Ind", "type", "indef"), Array("PronType", "Int", "type", "w-p"), Array("PronType", "Tot", "type", "indef", "lemma", "iedereen|ieder|al|alles"), Array("Poss", "Yes", "type", "poss"), Array("Reflex", "Yes", "type", "refl")) // hoe zit het nou ook alweer precies met de w-p's en d-p's. Bleuh...

  def main(args: Array[String]) =
  {
    val z = CGNMapping.translate(featureMapping)
  }
}

object CGNMapping
{
  def translate(mapping: Array[Array[String]]) =
  {
    val clauses:Array[Proposition] = mapping.map(a =>
    {
      val l = s"ud:${a(0)}=${a(1)}"
      val r = (2 until a.length by 2).map(i => Literal(s"cgn:${a(i)}=${a(i+1)}"))
      Implies(Literal(l), And(r: _*))
    })
    val byLeftHand = clauses.groupBy({case Implies(lhs, _) => lhs})
      .mapValues(l => l.map({case Implies(_,rhs) => rhs}))
      .mapValues(_.distinct)
      .mapValues(l => Or(l: _*))
    println(byLeftHand.mkString(",\n"))
    val andz = byLeftHand.map({case (k,v) => k → v}).toList
    And(andz: _*)
  }

  // lazy val mappingPropositionOld =  translate(cgnMapping)// Literal("aap")// translate(cgnMapping)
  lazy val mappingProposition = PropositionParser.parseFile("data/mapping.sorted").get

  // meeste tijd lijkt in de vertaling naar dimacs via CNF te zitten
  // dat kan worden beperkt door de mapping vooraf te compileren naar dimacs

  def time[A](p: ()=>A):A =
  {
    val t0 = System.currentTimeMillis()
    val a = p()
    val t1 = System.currentTimeMillis()
    //Console.err.println(t1-t0)
    a
  }

  def mapToTagset(p:Proposition, tagSet: Set[String]):Proposition =
  {
    // println(mappingProposition)
    val d0 = mappingProposition.dimacs.size

    val b = time( () => {
      val translation = tagSet.filter(t => {
        val lit = Literal(t)
        val extension = And(¬(lit), p)
        //val extended = And(mappingProposition, ¬(lit), p)
       // val b0 = !extended.isSatisfiable
        val b1 = !mappingProposition.isConsistentWith(extension)
        //if (b1 != b0)
         // Console.err.println("Gadsie!")
        b1
      })
      translation.map(Literal(_)).toList })

    And(b: _*)
  }

  val cgnFeatureSet = mappingProposition.varsIn.filter(s => s.startsWith("cgn:"))
  val udFeatureSet = mappingProposition.varsIn.filter(s => s.startsWith("ud:"))

  def main(args: Array[String]) =
  {
    //translate(cgnMapping)
    //(0 to 1).foreach(i => println(mapToTagset(PropositionParser.parse("ud:VerbForm=Fin ∧ ud:Tense=Past").get, cgnTagSet)))
    //(0 to 1).foreach(i => println(mapToTagset(PropositionParser.parse("cgn:pos=WW ∧ cgn:feat.persoon=1").get, udTagSet)))
    cgnFeatureSet.toList.sorted.foreach(s => {
      val p = Literal(s)
      println(s"$s => ${mapToTagset(p, udFeatureSet)}")
    })
  }
}
