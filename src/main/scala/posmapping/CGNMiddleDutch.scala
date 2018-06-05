package posmapping


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
  //Console.err.println("eek!!")
  def partitions = Map(
    "buiging"  -> (Set("met-e", "met-s", "zonder") ++ Set("met-s-of-th", "met-t", "met-n", "met-r-of-re", "met-a", "overig")),
    "getal-n"  -> Set("mv-n", "zonder-n"),
    "lwtype"   -> Set("bep", "onbep"),
    "conjtype" -> Set("neven", "onder"),
    "ntype"    -> Set("eigen", "soort"),
    "numtype"  -> Set("hoofd", "rang"),
    "getal"    -> Set("ev", "mv", "getal"),
    "getal-n"  -> Set("getal-n", "mv-n", "ev-n", "zonder-n"),
    "pvagr"    -> Set("met-t", "ev", "mv"),
    "pvtijd"   -> Set("tgw", "verl", "conj", "imp"),
    "status"   -> Set("nadr", "red", "vol"),
    "vztype"   -> Set("init", "fin", "versm"),
    "graad"    -> Set("basis", "comp", "dim", "sup"),
    "pdtype"   -> Set("adv-pron", "det", "grad", "pron"),
    "positie"  -> Set("nom", "postnom", "prenom", "vrij"),
    "genus"    -> Set("fem", "genus", "masc", "onz", "zijd"),
    "naamval"  -> Set("bijz", "dat", "gen", "nomin", "obl", "stan"),
    "persoon"  -> Set("1", "2", "2b", "2v", "3", "3m", "3o", "3p", "3v", "persoon"),
    "npagr"    -> Set("agr", "agr3", "evf", "evmo", "evon", "evz", "mv", "rest", "rest3"),
    "wvorm"    -> Set("inf", "od", "pv", "vd"),
    "vwtype"   -> Set("refl", "aanw", "betr", "bez", "excl", "onbep", "pers", "pr", "recip", "vb", "vrag"),
    "spectype" -> Set("deeleigen", "vreemd", "afk", "afgebr", "symb", "meta", "onverst"),
    "variatie" -> Set("dial") // officiele naam??
  )

  def pos2partitions = List( // fix volgorde....
    "N"    -> List("ntype", "getal", "graad", "genus", "naamval",         "buiging"),
    "ADJ"  -> List("positie", "graad", "buiging", "getal-n", "naamval"),
    "WW"   -> List("wvorm", "pvtijd", "pvagr", "positie", "buiging", "getal-n"),
    "NUM"  -> List("numtype", "positie", "graad", "getal-n", "naamval"),
    "VNW"  -> List("vwtype", "pdtype", "naamval", "status", "persoon",
      "getal", "genus", "positie",  "buiging", "npagr", "getal-n", "graad"),
    "LID"  -> List("lwtype", "naamval", "npagr"),
    "VZ"   -> List("vztype"),
    "VG"   -> List("conjtype"),
    "BW"   -> List(),
    "TSW"  -> List(),
    "TW"  -> List("numtype", "graad", "positie"),
    "SPEC" -> List("spectype")
  ).toMap
}
class CGNMiddleDutch {

}
