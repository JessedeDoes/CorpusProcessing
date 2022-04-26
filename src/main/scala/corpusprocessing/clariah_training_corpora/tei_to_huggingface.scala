package corpusprocessing.clariah_training_corpora
import scala.xml._
import fixTokenization.getId

import java.io.{File, PrintWriter}
import utils.{PostProcessXML, ProcessFolder}

// {"id":"0","tokens":["@paulwalk","It","'s","the","view","from","where","I","'m","living","for","two","weeks",".","Empire","State","Building","=","ESB",".","Pretty","bad","storm","here","last","evening","."],"ner_tags":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,8,8,0,7,0,0,0,0,0,0,0,0]}
import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write


case class Sentence(
                   id: String,
                   tokens: List[String],
                   tags: List[String]
                   )

object to_huggingface {
  implicit val formats = DefaultFormats

  def sentence(s: Node) = {
    val tokenElements = s.descendant.toList.filter(n => Set("w", "pc").contains(n.label))
    val tokens = tokenElements.map(_.text.trim)
    val tags = tokenElements.map(x => (x \ "@pos").headOption.getOrElse(x \ "@type").text.trim)
    Sentence("",tokens,tags)
  }

  def Nodes2JSON(d: Seq[Elem], fout: String): Unit = {
    val pw = new PrintWriter(fout)

    val s0 = (d \\ "s").toList.map(sentence)
    val s1 = s0.zipWithIndex.map({case (s,i) => s.copy(id=i.toString)})
    val jsons = s1.map(s => write(s))

    jsons.foreach(pw.println)
    pw.close()
  }

  def toJSON(f: Seq[String], fout: String): Unit = Nodes2JSON(f.map(x => XML.load(x)), fout)

  val openDBNL = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Tagged/"
  lazy val ideeen = new java.io.File(openDBNL).listFiles().filter(_.getName.contains("mult")).toSeq.map(_.getCanonicalPath)
  val example = "data/20220421_cobalt/CobaltServeExport/docpid_1.xml"

  def main(args: Array[String]): Unit = {
    val bestandje = if (args.isEmpty) ideeen else args.toSeq
    print(bestandje)
    toJSON(bestandje, "/tmp/out.json")
  }
}
