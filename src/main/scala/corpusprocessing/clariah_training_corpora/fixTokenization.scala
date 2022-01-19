package corpusprocessing.clariah_training_corpora
import utils.{PostProcessXML, ProcessFolder}

import java.io.{File, PrintWriter}
import scala.xml._

object fixTokenization {

  val inDir = "/mnt/Projecten/Corpora/Historische_Corpora/TrainingCorpora/Mapped2TDN/"
  val fixedDir = "/mnt/Projecten/Corpora/Historische_Corpora/TrainingCorpora/Fixed/"
  val bartje = XML.loadFile("/mnt/Projecten/Corpora/Historische_Corpora/TrainingCorpora/Mapped2TDN/NederlabEvaluation15/enge022vand01_01.tok.frogmodernized.sampled.tei.exported.xml")

  def getId(n: Node):String = n.attributes.filter(a => a.prefixedKey.endsWith(":id") || a.key=="id").map(_.value.toString).headOption.getOrElse("zeperd")

  def groupBy(tags: Seq[Node], f: Node=>String, grouper: Seq[Node] => Node) =
     {
       val g0  = tags.zipWithIndex.groupBy({case (n,i) => f(n)}).toList.sortBy(_._2.head._2).map({case (s,l) => (s, l.sortBy(_._2).map(_._1))}) // .mapValues(l => l.sortBy(_._2).map(_._1))
       g0.map({case (s,l) =>  grouper(l)})
     }

   def fix(s: Elem) = {
     def f(n: Node) =  (n \ "@n").headOption.map(_.text).getOrElse(getId(n))
     val groupedChild = groupBy(s.child.filter(_.isInstanceOf[Elem]), f, g => if (g.size==1) g.head else <group>{g}</group>)
     s.copy(child=groupedChild)
   }

  /*
              <w pos="NOU-C(number=uncl)+VRB(finiteness=fin,tense=uncl)" ctag="N+WW_PV"
              lemma="vuur+beschutten" gloss="+" time="2018-04-10T16:33:12" resp="tom" mform="+" type="N" xml:id="cast005cons01_01.TEI.2.text.body.div.p.13479.s.1.w.9">vierBeschudt</w>

   */

  def peuterLos(w: Elem, splitOn: String="__")  =  {
    val pos = (w \ "@pos").text
    val txt = w.text

    val (txt1,pat) = {
      if (txt.matches(".*([a-z])([A-Z]).*")) (txt.replaceAll("([a-z])([A-Z])", "$1__$2"),1)
      else if (txt.matches("^([,.:?])([0-9]).*")) (txt.replaceAll("^([,.:?])([0-9])", "$1__$2"),2)
      else if (txt.matches(".*[a-z][0-9]+$")) (txt.replaceAll("([a-z])([0-9]+)$", "$1__$2"), 3)
      else (txt,0)
    }


    //if (pat == 2 && txt1.contains("__")) Console.err.println("p" + pat + ":" + txt1  + " "  + w)

    if (txt1.contains(splitOn)) {

      val posParts = pos.split("\\+").toList
      val lemParts = (w \ "@lemma").text.split("\\+").toList
      val wordParts = txt1.split(splitOn).toList
      val canSplit = Set(posParts, lemParts,wordParts).map(_.size).size == 1
      val (posPartsF, lemPartsF) = if (Set(1,3).contains(pat) && posParts.size > 1 &&  !canSplit && posParts.size == 3 && lemParts.size == 3 && wordParts.size == 2)
        {
          Console.err.println(s"!!!! Cannot split $pat $txt1  in $w, but will try...")
          val posParts_new = Seq(posParts(0), s"${posParts(1)}+${posParts(2)}")
          val lemParts_new = Seq(lemParts(0), s"${lemParts(1)}+${lemParts(2)}")
          (posParts_new, lemParts_new)
        } else (posParts, lemParts)

      val replacement = if (Set(1,3).contains(pat) && posParts.size > 1 &&  Set(posPartsF, lemPartsF,wordParts).map(_.size).size == 1) {
        posPartsF.indices.toList.map(i => {
          val id = getId(w) + "." + i
          <w xml:id={id} resp={s"postprocess:$pat"} type={(w \ "@type").text} n={(w \ "@n").text} pos={posPartsF(i)} lemma={lemPartsF(i)} ctag={(w \ "@ctag").text}>{wordParts(i)}</w>}
        ).map(w => if (w.text.matches("^[,.:?]$")) w.copy(label="pc") else w)
      } else if (pat == 2) {
        val id = getId(w)
        Seq(<pc xml:id ={s"$id.pre"} resp={s"postprocess-2:$pat"}>{wordParts(0)}</pc>, w.copy(child=Text(wordParts(1))) )
      } else w

      if (w != replacement) {
        println(
          s"""\n#### $txt1 $pat
             |Vervang ${w}
             |Door:
             |${replacement.mkString("\n")}""".stripMargin)
      }
      replacement
    } else w
  }

  def fixDocje(doc: Elem) = {
    val d0 = PostProcessXML.updateElement(doc, _.label=="s" , fix)

    val d1 = PostProcessXML.updateElement5(d0, _.label=="group", g => {
      if (g.child.size == 2 && g.child.head.text.endsWith("-")) {
        val w0 = g.child.head.asInstanceOf[Elem]
        val w1 = g.child.tail.head.asInstanceOf[Elem]
        // Console.err.println(g)
        w0.copy(
          attributes = w0.attributes.filter(_.key != "type")
            .append(new UnprefixedAttribute("org", w0.text + w1.text, Null)),
          child = Text(w0.text.replaceAll("-$", "") + w1.text))
      } else g.child
    })

    val d2 = PostProcessXML.updateElement(d1.asInstanceOf[Elem], _.label == "w", w => {
      if (w.text == "|") w.copy(label = "pc") else w
    })

    val d3 = PostProcessXML.updateElement5(d2, _.label=="w", n => peuterLos(n,"__"))
    d3.asInstanceOf[Elem]
  }

  def makeTSV(doc: Elem, tsv: String): Unit = {
    val pw = new PrintWriter(tsv)
    doc.descendant.filter(x => Set("pc", "w").contains(x.label)).zipWithIndex.foreach({case (n,i) => {
      val id = getId(n)
      val docid = id.replaceAll(".TEI.2.*", "")
      val l = List(i, n.text, (n \ "@lemma").text, (n \ "@pos").text, n.label, id, docid)
      pw.println(l.mkString("\t"))
     }}
    )
    pw.close()
  }

  def main(args: Array[String]): Unit = {
    ProcessFolder.processFolder(new File(inDir), new File(fixedDir), {case (i,o) =>
      if (i.endsWith(".xml")) {
        val inDoc = XML.load(i)
        val outDoc = fixDocje(inDoc)
        val outTSV = o.replaceAll(".xml$", ".tsv")
        XML.save(o, outDoc, "UTF-8")
        makeTSV(outDoc, outTSV)
      }
    })
    //val x = fixDocje(bartje)
  }
}


/*
Per set:
-	15e-eeuwse set: Deze set bevat twee teksten en één van de teksten (Van den proprieteyten der dinghen) bevat veel afbreekstreepjes waar je de woorden liever aaneen hebt
 (zie bijv. http://cobalt.dev.inl.loc/php/moreContext.php?sDatabase=NederlabEvaluation15Play&iDocumentId=2&sDocumentTitle=/mnt/Projecten/Impact/testlocatie/uploadedDocuments2/zipExtractDir3/enge022vand01_01.tok.frogmodernized.sampled.tei.xml_tokenized.tab&iDocumentSize=0&iSearchOffset=35685&sWordForm=al-&iSearchWordLength=5&iWindowWidth=222222220&iOrigWindowWidth=220 en zoek op -).
  Daarnaast komt er 33x een sluisteken voor, maar die wil je liever niet tussen je tokens hebben zitten.
  @Katrien/@Jesse, is het wenselijk/mogelijk om deze eruit te halen en/of moeten we deze maar taggen als RES (met lemma -)? @Katrien,
  hebben we nu een optie om iets NIET te taggen (bijv. als er allerhande leestekens los in zouden staan)?

-	16e-eeuwse set: bij de getallen is vaak de komma/punt ervoor bij genomen in het token zelf, is daar iets aan te doen (eraf te slopen)?
  Ook zitten cijfers en/of woorden soms aan elkaar vast (bijv. een paginanummer vastgeplakt aan een woord/cijfer of een woord aan het einde
  van een regel vastgeplakt aan een ander woord dat met een hoofdletter begint). Is daar iets aan te doen?

-	17e-eeuwse set: voor zover ik kan zien, zijn er amper ‘gewenste aanpassingen’.
  Misschien enkele gevallen waarbij een cijfer achter een woord is geplakt (hooguit enkele tientallen keren).
Wel nog een vraagje (zie bijgevoegde png): willen we bij dit soort symbolen altijd het SYMBOOL als lemma geven? Want misschien worden deze niet (altijd) ondersteund? Kan volgens mij als (derde/tweede) set in CoBaLTje.
-	18e-eeuwse set: voor zover ik kan zien, geen aanpassingen. Kan volgens mij als (eerste) set in CoBaLTje
-	19e-eeuwse set: los van deze zin (Bevolking : West-Vlaenderen542000Oost-Vlaenderen661000Antwerpen380000Braband ( Nyvel uitgez . ) 377000 Limburg307000 _________ 2,267,000 Provintien , waer de volkstael waelsch is . Bevolking : Arrondissement Nyvel97000 Henegauwen530000 Namen180000 Luik314000 Gedeelte Luxemburg (1)127500 _________ 1,248,500), waar de (door mij) dikgedrukte tokens eigenlijk los geschreven moeten worden,
zijn er geen aanpassingen nodig. Kan volgens mij als (tweede/derde) set in CoBaLTje

 */