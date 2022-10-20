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
  val chunkSize=300
  implicit val formats = DefaultFormats

  def sentence(s: Node) = {
    val tokenElements = s.descendant.toList.filter(n => Set("w", "pc").contains(n.label))
    val tokens = tokenElements.map(x => if ( (x \\ "seg").nonEmpty) (x \\ "seg").text  else x.text.trim)
    val tags = tokenElements.map(x => (x \ "@pos").headOption.getOrElse(x \ "@type").text.trim)
    Sentence("",tokens,tags)
  }

  def Nodes2JSON(d: Iterator[Elem], fout: String): Unit = {
    val pw = new PrintWriter(fout)

    val esjes: Iterator[Node] = d.flatMap(

      x => {
        if ((x \\ "s").nonEmpty)
        (x \\ "s").iterator
        else
          {
            val chunks = (x \\ "w").grouped(chunkSize).toList.map(chunk => {
              <s>{chunk}</s>
            })
            chunks.iterator
          }
      }
    )
    val sentences: Iterator[Sentence] = esjes.map(sentence)

    //val words = (d \\ "w").size
    //println("Sentences:" + s0.size  + " Words: " + words)
    val s1 = sentences.zipWithIndex.map({case (s,i) => s.copy(id=i.toString)})
    val jsons = s1.map(s => write(s))

    jsons.foreach(x => {Console.err.println(x); pw.println(x)})
    pw.close()
  }

  def toJSON(f: Seq[String], fout: String): Unit = Nodes2JSON(f.iterator.map(x => XML.load(x)), fout)

  val openDBNL = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Tagged/"
  lazy val ideeen: Seq[String] = new java.io.File(openDBNL).listFiles().filter(_.getName.contains("mult")).toSeq.map(_.getCanonicalPath)
  val example = "data/20220421_cobalt/CobaltServeExport/docpid_1.xml"

  val BaB = "/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuit/2.7CHN/" // ai heeft geen zinnen..... Willekeurig aanmaken?
  val dbnl19 = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Selectie19"

  def main(args: Array[String]): Unit = {
    val filesToProcess = if (args.isEmpty) ideeen else args.toSeq.flatMap(x => {
      val f = new java.io.File(x)
      if (f.isFile) Seq(f.getCanonicalPath) else f.listFiles.toSeq.map(_.getCanonicalPath)
    }
    )
    println(filesToProcess)
    toJSON(filesToProcess, "/tmp/out.json")
  }
}
