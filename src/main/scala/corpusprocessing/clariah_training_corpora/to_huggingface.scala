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

  def toJSON(d: Elem, fout: String): Unit = {
    val s0 = (d \\ "s").toList.map(sentence)
    val s1 = s0.zipWithIndex.map({case (s,i) => s.copy(id=i.toString)})
    val jsons = s1.map(s => write(s))
    val pw = new PrintWriter(fout)
    jsons.foreach(pw.println)
    pw.close()
  }

  def toJSON(f: String, fout: String): Unit = toJSON(XML.load(f), fout)

  val example = "data/20220421_cobalt/CobaltServeExport/docpid_1.xml"
  def main(args: Array[String]): Unit = {
    toJSON(example, "/tmp/out.json")
  }
}
