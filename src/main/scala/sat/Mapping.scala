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

object CGNMapping
{
  val featureMapping1 = Array(Array("pos", "ADJ", "pos", "ADJ"), Array("pos", "ADV", "pos", "BW"), Array("pos", "INTJ", "pos", "TSW"), Array("pos", "NOUN", "pos", "N", "feat.ntype", "soort"), Array("pos", "PROPN", "pos", "N", "feat.ntype", "eigen"), // spec(deeleigen nooit te vinden zo....
    Array("pos", "PROPN", "pos", "SPEC", "feat.spectype", "deeleigen"), Array("pos", "VERB", "pos", "WW"), Array("pos", "ADP", "pos", "VZ"), Array("pos", "AUX", "pos", "WW"), // HM
    Array("pos", "DET", "pos", "LID"), Array("pos", "DET", "pos", "VNW", "feat.pdtype", "det"), Array("pos", "PRON", "pos", "VNW", "feat.pdtype", "pron"), Array("pos", "NUM", "pos", "TW"), // {"pos", "PART", "", ""}, // HM
    Array("pos", "CCONJ", "pos", "VG", "feat.conjtype", "neven"), Array("pos", "SCONJ", "pos", "VG", "feat.conjtype", "onder"), Array("pos", "PUNCT", "pos", "LET"), // HM
    Array("pos", "SYM", "pos", "SPEC"), // opzoeken
    Array("pos", "X", "pos", "SPEC"), // AdpType
    Array("AdpType", "Comprep", "feat.vztype", "versm"), Array("AdpType", "Prep", "feat.vztype", "init"), Array("AdpType", "Post", "feat.vztype", "fin"), // nominal features
    Array("Number", "Plur", "feat.getal", "mv"), Array("Number", "Sing", "feat.getal", "ev"), Array("Gender", "Fem", "feat.genus", "fem"), Array("Gender", "Fem", "feat.npagr", "evf"), Array("Gender", "Fem", "feat.persoon", "2v|3v"), Array("Gender", "Masc", "feat.genus", "masc"), Array("Gender", "Masc", "feat.persoon", "3m"), // ? evmo, wel of niet doen?
    Array("Gender", "Neut", "feat.genus", "onz"), Array("Gender", "Neut", "feat.npagr", "evon"), Array("Gender", "Neut", "feat.graad", "dim", "pos", "N"), Array("Gender", "Com", "feat.genus", "zijd"), // adjective
    Array("Degree", "Pos", "feat.graad", "basis"), Array("Degree", "Cmp", "feat.graad", "comp"), Array("Degree", "Sup", "feat.graad", "sup"), Array("Degree", "Dim", "feat.graad", "dim"), Array("Position", "Nom", "feat.positie", "nom"), Array("Position", "Postnom", "feat.positie", "postnom"), Array("Position", "Prenom", "feat.positie", "prenom"), Array("Position", "Free", "feat.positie", "vrij"), //
    Array("??", "??", "feat.buiging", "met-e"), // en wat doen we het de -s? (Ook in CHN tagging een probleem)
    Array("Case", "Gen", "feat.naamval", "gen", "pos", "ADJ|N|VNW"), Array("Case", "Acc", "feat.naamval", "obl"), Array("Case", "Dat", "feat.naamval", "dat"), Array("Case", "Nom", "feat.naamval", "nomin"), // numeral
    Array("NumType", "Card", "feat.numtype", "hoofd"), Array("NumType", "Ord", "feat.numtype", "rang"), // verbal features
    Array("Aspect", "Imp", "feat.wvorm", "od"), Array("Aspect", "Perf", "feat.wvorm", "vd"), Array("Mood", "Ind", "feat.wvorm", "pv", "feat.pvtijd", "verl"), Array("Mood", "Ind", "feat.wvorm", "pv", "feat.pvtijd", "tgw", "feat.persoon", "1|3|3p|3v|3m|3o"), Array("Mood", "Imp,Ind", "feat.wvorm", "pv", "feat.pvtijd", "tgw"), //{"Mood", "Imp", "feat.wvorm", "pv"},
    Array("Mood", "Sub", "feat.wvorm", "pv", "feat.pvtijd", "conj"), Array("VerbForm", "Fin", "feat.wvorm", "pv"), Array("VerbForm", "Inf", "feat.wvorm", "inf"), Array("VerbForm", "Part", "feat.wvorm", "od"), Array("VerbForm", "Part", "feat.wvorm", "vd"), Array("Person", "1", "feat.persoon", "1"), Array("Person", "2", "feat.persoon", "2|2v|2b"), Array("Person", "3", "feat.persoon", "3|3p|3v|3m|3o"), //{"Person", "1", "person", "1"},
    //{"Person", "2", "person", "2"},
    //{"Person", "3", "feat.pvagr", "met-t"},
    Array("Tense", "Past", "feat.pvtijd", "verl"), // Maar: NIET als het een deelwoord is Dus deze manier van converteren werkt niet; je hebt ook nog condities nodig
    Array("Tense", "Pres", "feat.pvtijd", "tgw"), Array("Tense", "Past", "feat.wvorm", "vd"), // eigenlijk niet goed? in UD geen past maar perf?
    Array("Tense", "Pres", "feat.wvorm", "od"), Array("Polite", "Inf", "feat.persoon", "2v"), Array("Polite", "Pol", "lemma", "u"), // pronoun / determiner / article
    // UD heeft: Art (ldiwoord)	Dem	(aanwijzend) Emp (nadruk)	Exc (uitroepend)	Ind	(onbepaald) Int	Neg	Prs (persoonlijk)	Rcp (reciprocal)	Rel (betrekkelijk)	Tot (collectief: iedereen enzo)
    Array("Definite", "Def", "feat.lwtype", "bep"), Array("Definite", "Ind", "feat.lwtype", "onbep"), Array("Definite", "Def", "feat.vwtype", "bep"), Array("Definite", "Ind", "feat.vwtype", "onbep"), Array("PronType", "Art", "pos", "LID"), Array("PronType", "Exc", "feat.vwtype", "excl"), // misnomer: zou vnwtype moeten zijn?
    Array("PronType", "Dem", "feat.vwtype", "aanw"), // in welk feat zit PronType????
    Array("PronType", "Prs", "feat.vwtype", "pers"), Array("PronType", "Rel", "feat.vwtype", "betr"), Array("PronType", "Int", "feat.vwtype", "vrag"), // wanneer precies vb? Alleen bij wie/wat enz?
    Array("PronType", "Rel", "feat.vwtype", "betr"), Array("PronType", "Rcp", "feat.vwtype", "recip"), Array("PronType", "Ind", "feat.lwtype", "onbep"), Array("PronType", "Ind", "feat.vwtype", "onbep"), Array("PronType", "Tot", "feat.lwtype", "onbep", "lemma", "iedereen|ieder|al|alles|elk|elke"), Array("PronType", "Neg,Tot", "feat.lwtype", "onbep", "lemma", "geen"), // ?
    Array("Poss", "Yes", "feat.vwtype", "bez"), Array("Reflex", "Yes", "feat.vwtype", "refl"), Array("Reflex", "Yes", "feat.vwtype", "pr"), // ??
    Array("Variant", "Long", "feat.status", "vol"), Array("Variant", "Short", "feat.status", "red"))
}
