package corpusprocessing.clariah_training_corpora.patch_scripts


import corpusprocessing.corpusutils.toTSV.elemToTsv
import database.DatabaseUtilities.Select

import scala.xml._
import utils.PostProcessXML
import utils.ProcessFolder
/*
   - Lemmatisering van scheidbare delen werkwoord
   - Lemmatisering van gesubstantiveerde infinitieven
 */

object setAtt {
  def setAttribute(e: Elem, prefix: String, name: String, value: String): Elem = {
    val a = e.attributes.filter(_.key != name).append(new PrefixedAttribute(prefix, name, value, Null))
    e.copy(attributes = a)
  }

  def setAttribute(e: Elem, name: String, value: String): Elem = {
    val a = e.attributes.filter(_.key != name).append(new UnprefixedAttribute(name, value, Null))
    e.copy(attributes = a)
  }

}
object patchSomeLemmatizationBugs {

  import setAtt._
  val testFile = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Corpus19Tagged/aa__001biog11_01.meta.xml"
  lazy val gigantHilexDB = new database.Database(database.Configuration("x", "svowdb16.ivdnt.loc","gigant_hilex_candidate", "dba", "vercingetorix"))
  lazy val molex = new database.Database(database.Configuration("y", "svowdb16.ivdnt.loc","gig_pro", "dba", "vercingetorix"))
  lazy val molexVerbs: Set[String] = molex.slurp(Select(r => r.getString("modern_lemma"), "data.lemmata where lemma_gigpos ~ 'VRB'")).toSet
  lazy val molexLemmata : Set[String] = molex.slurp(Select(r => r.getString("modern_lemma"), "data.lemmata")).toSet
  lazy val lexNames = Map(molexLemmata -> "molex", hilexLemmata -> "hilex")
  lazy val hilexLemmata : Set[String] = gigantHilexDB.slurp(Select(r => r.getString("modern_lemma"), "data.lemmata")).toSet
  lazy val molexVerbForms: Set[String] = molex.slurp(Select(r => r.getString("wordform"), "data.lemmata_en_paradigma_view where wordform_gigpos ~ 'VRB'")).toSet
  lazy val molexSeparableParts: Set[String] = molex.slurp(Select(
    r => r.getString("wordform"),
    "data.lemmata_en_paradigma_view where lemma_gigpos ~ 'VRB.*sep' and wordform ~ ' '")).map(w => w.replaceAll("^.* ", "")).toSet

  def findBetterLemma(word: String, pos: String, lemma: String): String = {
    // println(s"$word $pos --> $lemma")
    if (pos.equals("NOU-C(number=sg)") && molexVerbs.contains(word.toLowerCase())) {
      word
    }  else if (pos.startsWith("VRB") && molexSeparableParts.contains(word.toLowerCase()) && !molexVerbForms.contains(word.toLowerCase())) {
      word
    } else if (pos.startsWith("NOU-P") && !(lemma.toLowerCase() + "s" == word.toLowerCase())) {
      // Maar als lemma = wordform -s mag ie blijven
      if (word.substring(0,1).toLowerCase() == word.substring(0,1)) word else word.substring(0,1) + word.substring(1).toLowerCase
    } else if (pos.toLowerCase.contains("abbr") || pos.startsWith("RES"))
      word // word
    else lemma

    // misschien nog voor Nou-P lemma=woordform zetten
  }

  def patchLemma(e: Elem)  = {
    val pos = (e \ "@pos").text
    val lemma  = (e \ "@lemma").text
    val word = e.text.trim
    val lNew  = findBetterLemma(word,pos,lemma)
    val lexica = lexNames.filter(_._1.contains(lNew)).map(_._2).mkString(",")

    if (lNew != lemma) {
      println(s"$word $pos $lemma --> $lNew")
    }
    setAttribute
        (setAttribute(e, "lemma", s"$lNew"),
        "lemmaLexicon",
          if (lexica.isEmpty) "unknown" else lexica)

  }
  def patchLemmata(d: Elem)  = {
    PostProcessXML.updateElement(d, _.label=="w",patchLemma)
  }


  def handle(i: String, o: String)  = {
    val d = patchLemmata(XML.loadFile(i))
    XML.save(o, d)
  }
  def main(args: Array[String]) : Unit  = {
    //println(molexSeparableParts)
    // val d1 = patchLemmata(XML.load(testFile))
    // elemToTsv(d1)
    implicit def f(x: String) = new java.io.File(x)
    ProcessFolder.processFolder("/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Corpus19Tagged/", "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/LemmaPatch/", handle)
  }
}
