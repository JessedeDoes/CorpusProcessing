package corpusprocessing.clariah_training_corpora.patch_scripts

import utils.PostProcessXML._
import utils.ProcessFolder

import java.io.File
import scala.xml._

object reduceUnclears {

  val inDir = fixBotchedOrder.orderFixedDir
  val fixedDir = "/mnt/Projecten/Corpora/Historische_Corpora/TrainingCorpora/PostProcessed_LessUnclear"

  def fixAnnot(w:  String, lemma: String, pos: String): (String, String) = {
    val mainPos = pos.replaceAll("\\(.*", "")
    val newPos = (mainPos match {
      case "NOU-C" => pos.replaceAll("number=uncl", "number=sg")
      case "ADV" => pos.replaceAll("type=uncl",
        if (Set("er","daar").contains(lemma)) "type=pron" else "type=reg"
      )
      case "VRB" =>  {
        if (w.endsWith("de")) {
          // Console.err.println(s"$w $pos")
        }
        pos.replaceAll("tense=uncl", "tense=pres")
      }
      case "NUM" if w.matches("[0-9]+")  => pos.replaceAll("representation=uncl", "representation=dig")
      case "NUM" if w.matches("[A-Za-z]+") => pos.replaceAll("representation=uncl", "representation=let")
      case _ => pos
    }).replaceAll("\\(\\)", "")
    //if (newPos != pos) {Console.err.println(s"($w,$lemma,$pos) ==> $newPos")}
    (lemma, newPos)
  }

  def splitX(s: String, r: String) =
    s.replaceAll(s"($r)", "#$1###")
      .split("###").toList
      .map{ x=> val a = x.split("#"); if (a.length  > 1) a(0) -> Some(a(1)) else a(0) -> None }

  def fixWord(w: Elem) : Elem = {
    val lemma = ( w\ "@lemma").text
    val pos = (w \ "@pos").text
    val word = w.text.trim

    val lSplit = splitX(lemma, "[|+]")
    val pSplit = splitX(pos, "[|+]")

    if (lSplit.map(_._2) != pSplit.map(_._2)) {
      // Console.err.println(s"!!!Inconsistentie: $lemma || $pos")
    }

    val lp0 = pSplit.zip(lemma.split("[|+]"))
    //val lp : Seq[(String, String)] = lp0.map{ case ((p,s),l) => { fixAnnot(word, l, p); (l,p)} -> s}.map{ case ((l,p), s) => (l +s.getOrElse(""), p + s.getOrElse(""))}
    val lp = pSplit.map{case (p,s) => fixAnnot(word, lemma, p) -> s}.map{ case ((l,p), s) => (l +s.getOrElse(""), p + s.getOrElse(""))}
    val l1 = lp.map(_._1).mkString("")
    val p1 = lp.map(_._2).mkString("")
    if (lp.length > 1) {
      // Console.err.println(s"\nPAS OP $lp0 ===> $lp")
    }

    // if (p1 != pos) Console.err.println(s"#${lp.size} $word, $lemma, $pos => ($l1, $p1) ///// $lp")

    w.copy(attributes = w.attributes.filter(a => !Set("lemma","pos").contains(a.key))
      .append(new UnprefixedAttribute("lemma", lemma, Null)).append(new UnprefixedAttribute("pos", p1, Null)))
  }

  def fixDocje(d: Elem) : Elem = {
    updateElement(d, _.label == "w", fixWord)
  }

  def main(args: Array[String]): Unit = {
    ProcessFolder.processFolder(new File(inDir), new File(fixedDir), {case (i,o) =>
      if (i.endsWith(".xml")) {
        val inDoc = XML.load(i)
        val outDoc = fixDocje(inDoc)
        val outTSV = o.replaceAll(".xml$", ".tsv")
        XML.save(o, outDoc, "UTF-8")
      }
    })
    //val x = fixDocje(bartje)
  }
}

/*
-	Graag de unclear-waardes vervangen door een defaultwaarde. Hierbij nog mijn opmerkingen uit een eerdere mail:
Wat betreft de unclears (met oog op een scriptje en/of defaulttoewijzing) zijn er volgens mij vier gevallen waarin de defaultwaarde kan worden aangepast:
1. NOU-C: number=uncl kan in de regel vervangen worden door number=sg (eventueel niet voor woorden op –en of –s, maar misschien voert dat te ver)
2. ADV: type=uncl kan in de regel vervangen worden door type=reg (eventueel een uitzondering maken voor gevallen da(a|e)r., er., hier., wa(a|e)r. waar het meestal gaat om type=pron)
3. VRB: tense=uncl kan in de regel vervangen worden door tense=pres (eventueel een uitzondering maken voor vormen die eindigen op –te, -ten, -de of ¬–den, hoewel er dan wel een (kleine?) foutmarge is bij vormen op een slot-n)
4. NUM: representation=uncl is een lastige. Misschien als defaultwaarde dig (tenzij er letters in staan dan let)? Maar dan gaat het fout in een tekst met veel Romeinse cijfers. Misschien toch maar dig doen en accepteren dat er (mogelijk) nog het nodige te corrigeren valt.
-	Bij lemmata waarbij de PoS-tag RES(type=uncl) of RES(type=for) is, mag het lemma worden veranderd in een liggend streepje. Cf. TDN

-	Loze haakjes bij kenmerkloze NOU-P’s en INT’s mogen (moeten?) worden weggehaald in de analyses. Je kan ze ook niet toekennen omdat er geen toegestane tag INT() of NOU-P() is
Opmerking Katrien 12-01-2022 (uit Teams): Ik denk dat woorden zonder features ingeladen hadden moeten worden zonder loze haakjes. Ik zou in ieder geval geen haakjes zelf toevoegen.
-	Er zijn nog enkele gevallen van dollartekens in de typetabel (één in de 16e-eeuwse set en twee in de 19e-eeuwse set, zie ook de screenshot) waar er geen match is in de worktable. Moeten deze types gewoon verwijderd worden uit HiLex of moet er verder nog goed naar de data gekeken worden en moeten er eventuele aanpassingen worden gedaan? Ik denk dat dit de laatste ‘loze rijen’ zijn.


 */