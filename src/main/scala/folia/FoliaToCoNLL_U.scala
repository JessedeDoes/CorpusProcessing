package folia

import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import folia.TestSimplify._
import posmapping.{CGNTagset, Tag}
import utils.{PostProcessXML, ProcessFolder}
import scala.util.{Try,Success,Failure}

import scala.xml._

object FoliaToCoNLL_U {
  def getId(n: Node):String = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).head

  def cgnParse(t: String):Tag = {

    Try (CGNTagset.fromString(t.replaceAll("enof",""))) match {
      case Success(tag) => tag
      case _ => {
        Console.err.println(s"Bad tag CGN $t")
        CGNTagset.fromString("SPEC")
      }
    }
  }

  val posMapper = FoliaMapPosTags(cgnParse, toUD)


  def printWord(w: Node) =
  {
    val word = (w \ "t").text
    val lemma = ((w \\ "lemma").filter(x => (x \ "@class").text != "_").headOption.map(x =>  (x \ "@class").text)).getOrElse("word")
    val pos = ((w \ "pos").head \ "@class").text
    val originalPoS = (((w \ "pos").filter(x => !(x \ "@set").text.contains("UD" ))).head \ "@class").text
    val mainPos = pos.replaceAll("[(].*", "").replaceAll("UNK", "X")
    val features = pos.replaceAll(".*[(]", "").replaceAll("[()]","").replaceAll("[|]", "_").replaceAll(",", "|")
    s"$word\t$lemma\t$mainPos\t$originalPoS\t${if (features.nonEmpty) features else "_"}"
  }

  def padToTen(s: String) =
  {
    val tabjes = s.count(_ == '\t')

    val padding = (tabjes until 9).map(x => "\t_").mkString
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
           s"# semcor_sense for token $i: $wid => $sense"
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

  def doOneFile(in: String, out:String): Unit = {
    Try ({
      val o = new java.io.PrintWriter(out.replaceAll(".xml.gz$", ".conll-u.txt").replaceAll("sonarSemcor.out", "INT-dutch-elexis-semtest"))
      toCoNLL(in).foreach(l => o.write(l + "\n"))
      o.close()
    }) match {
      case Success(value) =>
      case Failure(exception) => exception.printStackTrace() ; Console.err.println(s"Error in file $in: ${exception.getMessage}")
    }
  }
  def testje = toCoNLL(sourceDir + "/" + "sonarSemcor.out.279335.xml.gz").foreach(println)

  val sourceDir = "/mnt/Projecten/Taalbank/CL-SE-data/Corpora/DutchSemCor/SentencesFromSonarWithAnnotation/Corrected"
  val targetDir = "/mnt/Projecten/Taalbank/CL-SE-data/Corpora/DutchSemCor/SentencesFromSonarWithAnnotation/CoNLL-X/"
  def main(args: Array[String]): Unit = {
    import java.io.File
    ProcessFolder.processFolder(new File(sourceDir), new File(targetDir), doOneFile )
  }
}
