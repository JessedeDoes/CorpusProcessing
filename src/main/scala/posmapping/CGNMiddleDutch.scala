package posmapping

import propositional.Proposition


/*
N(soort,ev,form=0)
N(soort,ev,form=-e)
N(soort,ev,form=-s/-th)
N(soort,ev,form=-t)
N(soort,ev,form=-n)
N(soort,ev,form=-r/-re)
N(soort,ev,form=-a)
N(soort,ev,form=other)
 */

object CGNMiddleDutch
{
  val substitutions = List(
    "form=0" -> "zonder",
    "form=-e" -> "met-e",
    "form=-s/-th" -> "met-s-of-th",
    "form=-n" -> "met-n",
    "form=-t"-> "met-t",
    "form=-r.-re"-> "met-r-of-re",
    "form=-a" -> "met-a",
    "form=other"-> "overig",

    ",probleem=true" -> "",
    "probleem=true(,?)" -> "",
    "type=hk" -> "hulp-of-koppel",
    "imper"-> "imp",
    "quality" -> "kwal",
    "refl/recip" -> "refl-of-recip",
    "[a-z]+=" -> ""
  )


  def apply(s: String, pr: (String,String)) = s.replaceAll(pr._1,pr._2)
  def preprocess(s: String):String = substitutions.foldLeft(s)(apply)

  //Console.err.println("eek!!")
  def partitions = Map(
    "buiging"  -> (Set("met-e", "met-s", "zonder") ++
            Set("met-s-of-th", "met-t", "met-n", "met-r-of-re", "met-a", "overig")),
    "getal-n"  -> Set("mv-n", "zonder-n"),

    "lwtype"   -> Set("bep", "onbep"),
    "conjtype" -> Set("neven", "onder",  /**/   "expl", "verg", "neg", "betr", "kwal"),
    "ntype"    -> Set("eigen", "soort"), // /**/ "onduidelijk"),
    /**/ "wwtype"  -> Set("hoofd", "hulp-of-koppel"), // hk
    "numtype"  -> Set("hoofd", "rang",   /**/  "onbep"), // "onbep" ??
    "vztype"   -> Set("init", "fin", "versm"),
    "pdtype"   -> Set("adv-pron", "det", "grad", "pron"),
    "vwtype"   -> Set("refl", "aanw", "betr", "bez", "excl",
      "onbep", "pers", "pr", "recip", "vb", "vrag",   /**/     "refl-of-recip"),

    /**/ "bwtype"   -> Set("alg", "aanw", "betr", "vrag", "onbep", "herv", "neg"),


    "getal"    -> Set("ev", "mv", "getal"),
    "getal-n"  -> Set("getal-n", "mv-n", "ev-n", "zonder-n"),
    "pvagr"    -> Set("met-t", "ev", "mv"),
    "pvtijd"   -> Set("tgw", "verl", "conj", "imp"), // "imper" --> "imp"
    // /**/ "pvmodus"  -> Set("imper"), // vooralsnog niet nodig, wel voor onw!
    "status"   -> Set("nadr", "red", "vol",  /**/    "ellips"),

    "graad"    -> Set("basis", "comp", "dim", "sup"),

    "positie"  -> Set("nom", "postnom", "prenom", "vrij"),
    "genus"    -> Set("fem", "genus", "masc", "onz", "zijd"),
    "naamval"  -> Set("bijz", "dat", "gen", "nomin", "obl", "stan"),
    "persoon"  -> Set("1", "2", "2b", "2v", "3", "3m", "3o", "3p", "3v", "persoon"),
    "npagr"    -> Set("agr", "agr3", "evf", "evmo", "evon", "evz", "mv", "rest", "rest3"),
    "wvorm"    -> Set("inf", "od", "pv", "vd",  /**/   "part"), // part als je niet weet of het vd of od is

    "spectype" -> Set("deeleigen", "vreemd", "afk", "afgebr", "symb", "meta", "onverst"),
    "variatie" -> Set("dial") // officiele naam??
  )


  def additions2partitions = Map(
    "buiging" -> Set("met-s-of-th", "met-t", "met-n", "met-r-of-re", "met-a", "overig"),
    "conjtype" -> Set("expl", "verg", "neg", "betr", "kwal"),
    "wwtype"  -> Set("hoofd", "hulp-of-koppel"),
    "numtype" -> Set("onbep"),
    "vwtype" -> Set("refl-of-recip"),
    "bwtype"   -> Set("alg", "aanw", "betr", "vrag", "onbep", "herv", "neg"),
    "status" -> Set("ellips"),
    "wvorm" -> Set("part")
  )

  val p1 = CGNTagset.partitions.map({case (k,v) => (k, v ++ additions2partitions.getOrElse(k, Set())) }) ++ additions2partitions.filter(x =>
  !CGNTagset.partitions.contains(x._1))

  //Console.err.println(partitions == p1)
  //Console.err.println(partitions.keySet.filter(k => partitions(k) != p1(k)))

  def pos2partitions_add = Map(
    "N" -> List("buiging"),
    "ADJ" -> List("getal", "buiging"),
    "WW" -> List("wwtype", "buiging"),
    "NUM" -> List("buiging"),
    "VNW" -> List("buiging"),
    "LID" -> List("buiging"),
    "VZ" -> List("buiging"),
    "VG" -> List("buiging"),
    "BW" -> List("bwtype", "status", "pdtype", "buiging"),

    "TSW"  ->List("buiging"),
    "TW"  -> /**/ List("buiging"),
    "SPEC"  -> /**/ List("buiging"),
  )

  val pp1 = CGNTagset.pos2partitions.map({case (k,v) => (k, v ++ pos2partitions_add.getOrElse(k, List())) }) ++ pos2partitions_add.filter(x =>
    !CGNTagset.pos2partitions.contains(x._1))

  //println(pp1 == pos2partitions)
  //Console.err.println(pp1.keySet.filter(k => !pos2partitions.contains(k)))
  //System.exit(0)
  def pos2partitions = List( // fix volgorde....
    "N"    -> List("ntype", "getal", "graad", "genus", "naamval",    /**/     "buiging"),
    "ADJ"  -> List("positie", "graad", "buiging", "getal-n", "naamval",  /**/  "getal", "buiging"),
    "WW"   -> List("wvorm", "pvtijd", "pvagr", "positie", "buiging", "getal-n",
      /**/ /* "pvmodus", */ "wwtype", "buiging"),
    "NUM"  -> List("numtype", "positie", "graad", "getal-n", "naamval",  /**/      "buiging"),
    "VNW"  -> List("vwtype", "pdtype", "naamval", "status", "persoon",
      "getal", "genus", "positie",  "buiging", "npagr", "getal-n", "graad", /**/     "buiging"),
    "LID"  -> List("lwtype", "naamval", "npagr",  /**/   "buiging"),
    "VZ"   -> List("vztype",  /**/   "buiging"),
    "VG"   -> List("conjtype",   /**/  "buiging"),
    "BW"   -> /**/ List("bwtype", "status",  "pdtype", "buiging"),
    "TSW"  -> /**/ List("buiging"),
    "TW"  -> List("numtype", "graad", "positie", /**/   "buiging"),
    "SPEC" -> List("spectype",   /**/ "buiging")
  ).toMap
}

object CGMMiddleDutchTagset extends TagSet("mnl", CGNLite.posTags, CGNMiddleDutch.partitions, CGNMiddleDutch.pos2partitions)
{
  override def fromProposition(p:Proposition, posIsSpecial: Boolean = false):Tag = fromPropositionCGN(p, posIsSpecial)
  override def fromString(t: String): Tag = CGNTag(CGNMiddleDutch.preprocess(t))
}

case class CGNMiddleDutchTag(tag: String) extends CGNStyleTag(tag, CGMMiddleDutchTagset)

object testje
{
  val tagset = "data/tagset-CorpGysseling-CRM_allTags.overzichtje.csv"
  def main(args:Array[String]) =
  {
    scala.io.Source.fromFile(tagset).getLines.toStream.map(l => l.split("\t").toList).filter(_.size >= 5).foreach(l => {
      val t = CGNMiddleDutch.preprocess(l(4))
      val t1 = CGNMiddleDutchTag(t)
      //Console.err.println(l(4))
      //Console.err.flush()
      Console.err.println(l(4) + ", " + t + " --> " + t1.proposition)
      println(s"${l(0)}\t$t1")
    })
  }

}