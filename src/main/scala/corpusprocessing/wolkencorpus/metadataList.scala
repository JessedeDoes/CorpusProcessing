package corpusprocessing.wolkencorpus

import utils.ProcessFolder

import java.io.{File, PrintWriter}
import scala.xml._

/*
Hoe zie je of er stukjes zijn?

In CLVN:             <biblScope>
              <span inst="#INT_datum_460"/>
            </biblScope>

In Gysseling:             <biblScope>
              <span to="#hand.d7262e60" from="#hand.d7262e25"/>
            </biblScope>

 */

object metadataList {
  val xmlns = "http://www.w3.org/XML/1998/namespace"

  def metaProps(b: Node) = {
    (b \\ "interpGrp")
      .flatMap(g =>  (g \\ "interp").map(i => (g \ "@type").text -> i.text))
      .filter(_._2.nonEmpty)
      .groupBy(_._1)
      .mapValues(l => l.map(_._2))
  }

  def p(metaProperties: Map[String, Seq[String]], n: String) = metaProperties.getOrElse(n, Seq()).mkString(";").replaceAll("\\s+", " ")
  def getLowestLevelX(metaProperties: Map[String, Seq[String]], n: String) =
    Set(0,1,2).map(i => n.replaceAll("#", i.toString)).intersect(metaProperties.keySet).toList.sorted.headOption.map(k => p(metaProperties, k)).getOrElse("_").replaceAll("\\s+", " ")

  def printMeta(f: File, pw: PrintWriter)(d: Elem)  = {

    val inlMetadata: Option[Node] = (d \\ "listBibl").find(x => x.attributes.filter(a => a.key.endsWith("id")).map(_.value.text).mkString(";").contains("inlMetadata"))

    val metaProperties: Map[String, Seq[String]] = inlMetadata.map(b => metaProps(b)).getOrElse(Map())
    def p(n: String) = metaProperties.getOrElse(n, Seq()).mkString(";").replaceAll("\\s+", " ")

    val pid = p("pid")

    val allDates = ((d \ "teiHeader") \\ "bibl").map(
      b => {
        val props = metaProps(b)
        val date = getLowestLevelX(props, "witnessYearLevel#_from")

        //val frag = b.toString.replaceAll("\\s+", " ").substring(0,50)
        //Console.err.println(s"$pid $frag $date")
        date
      }
    ).filter(_.matches("[0-9]{4}")).toSet

    //Console.err.println(allDates)
    val heeftSubjes = allDates.size > 1

    val metaXML = inlMetadata.map(_.toString().replaceAll("\\s+", " ")).getOrElse("")



    val level = List(1,2).filter(i => metaProperties.exists(_._1.contains(s"evel$i"))).headOption.getOrElse(-1)

    def getLowestLevel(n: String) =
      Set(1,2).map(i => n.replaceAll("#", i.toString)).intersect(metaProperties.keySet).toList.sorted.headOption.map(k => p(k)).getOrElse("_").replaceAll("\\s+", " ")

    val wyf = getLowestLevel("witnessYearLevel#_from")
    val wyt = getLowestLevel("witnessYearLevel#_to")
    val tyf = getLowestLevel("textYearLevel#_from")
    val tyt = getLowestLevel("textYearLevel#_to")
    val pyf = getLowestLevel("pubYearLevel#_from")
    val pyt = getLowestLevel("pubYearLevel#_to")
    val title = getLowestLevel("titleLevel#")
    val author = getLowestLevel("authorLevel#")

    val line = s"${f.getCanonicalPath}\t$pid\t${p("corpusProvenance")}\t${p("sourceID")}\t$heeftSubjes\t$allDates\t$level\t$title\t$author\t$wyf\t$wyt\t$tyf\t$tyt\t$pyf\t$pyt\t$metaXML"

    if (heeftSubjes) {
      //Console.err.println("Subjes " + allDates + " " + line)
    }
    pw.println(line)
    pw.flush()
    //val wyf = p("witnessYear")
  }

  val dbnl = "/mnt/DiskStation/homes/jesse/work/DBNL/MetMeta2021"
  val gysseling = "/mnt/Projecten/Corpora/Historische_Corpora/Nederlab/Wolkencorpus/CorpusGysseling"
  // DBNL: /mnt/DiskStation/homes/jesse/work/DBNL/MetMeta2021
  // wolkjes: /mnt/Projecten/Corpora/Historische_Corpora/Nederlab/Wolkencorpus
  def main(args: Array[String]): Unit = {
    val pw = new PrintWriter("/tmp/wolkencorpus.tsv")
    ProcessFolder.processFolder(new File(args(0)), f => printMeta(f, pw)(XML.loadFile(f)))
    pw.close()
  }
}
