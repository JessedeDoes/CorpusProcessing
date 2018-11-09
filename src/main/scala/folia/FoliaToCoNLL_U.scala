package folia

import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import folia.TestSimplify._
import posmapping.{CGNTagset, Tag}

import scala.xml._

object FoliaToCoNLL_U {
  def getId(n: Node):String = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).head

  def cgnParse(t: String):Tag = CGNTagset.fromString(t)
  val posMapper = FoliaMapPosTags(cgnParse, toUD)



  def printWord(w: Node) =
  {
    val word = (w \ "t").text
    val lemma = ((w \\ "lemma").head \ "@class").text
    val pos = ((w \ "pos").head \ "@class").text
    val mainPos = pos.replaceAll("[(].*", "")
    val features = pos.replaceAll(".*[(]", "").replaceAll("[()]","").replaceAll("[|]", "_").replaceAll(",", "|")
    s"$word\t$lemma\t$mainPos\t${if (features.nonEmpty) features else "_"}"
  }

  def padToTen(s: String) =
  {
    val tabjes = s.count(_ == '\t')

    val padding = (tabjes to 9).map(x => "\t_").mkString
    s + padding
  }
  //   <w lemmaCheck="weg" sense="r_n-42716" xml:id="WR-P-E-I-0000000006.p.39.s.2.w.8">
  def printLine(s: Node): List[String] =
  {
     val id = getId(s)
     val wordsWithSenses = (s \\ "w").zipWithIndex.map({case (e,i) => (e,i+1)}).filter({ case (w,i) => (w \ "@sense").nonEmpty})
     val sense_lines = wordsWithSenses.map({
       case (w,i) =>
         {
           val wid = getId(w)
           val sense = (w \ "@sense").text
           s"# semcor_sense $i : $wid : $sense"
         }
     }).toList
     val tokens = (s \\ "w").map(printWord).zipWithIndex.map({case (l,i) => (i+1).toString + "\t" + l }).toList.map(padToTen)
     s"# sent_id = $id" ::  (sense_lines ++ tokens) ++ List("\n")
  }

  def toCoNLL(f: Elem): Seq[String] =
  {
    val mapped = posMapper.updatePoS(f)
    val lines = (mapped \\ "s").flatMap(printLine)
    lines
  }

  def toCoNLL(fileName: String):Seq[String] =
  {
    val in = new FileInputStream(fileName)
    val zin = if (fileName.endsWith("gz")) new GZIPInputStream(in) else in
    toCoNLL(XML.load(zin))
  }

  val sourceDir = "/mnt/Projecten/Taalbank/CL-SE-data/Corpora/DutchSemCor/SentencesFromSonarWithAnnotation/Corrected"
  def main(args: Array[String]): Unit = {
    toCoNLL(sourceDir + "/" + "sonarSemcor.out.279335.xml.gz").foreach(println)
  }
}
