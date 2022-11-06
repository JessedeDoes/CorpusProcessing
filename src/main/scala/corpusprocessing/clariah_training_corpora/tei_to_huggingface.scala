package corpusprocessing.clariah_training_corpora
import scala.xml._
import fixTokenization.getId

import java.io.{File, PrintWriter}
import utils.{PostProcessXML, ProcessFolder}

import java.util.zip.GZIPOutputStream

// {"id":"0","tokens":["@paulwalk","It","'s","the","view","from","where","I","'m","living","for","two","weeks",".","Empire","State","Building","=","ESB",".","Pretty","bad","storm","here","last","evening","."],"ner_tags":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,8,8,0,7,0,0,0,0,0,0,0,0]}
import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write


case class Sentence(
                   id: String,
                   tokens: List[String],
                   tags: List[String],
                   lemmata: List[String],
                   xml_ids: List[String]  = List(),
                   relevances: List[String]  = List(),
                   file: String = "unknown" // pas op andere dingen hebben dit niet
                   )

object to_huggingface {
  val sentence_element="q"
  val chunkSize=300
  implicit val formats = DefaultFormats

  def sentence(s: Node, f: String) = {

    def getN(n: Node) =  (n \ "@n").text

    val tokenElements = s.descendant.toList.filter(n => Set("w", "pc").contains(n.label))
    val indexedTokenElements = tokenElements.zipWithIndex
    val tokens = tokenElements.map(x => if ( (x \\ "seg").nonEmpty) (x \\ "seg").text  else x.text.trim)
    val tags = tokenElements.map(x => (x \ "@pos").headOption.getOrElse(x \ "@type").text.trim)
    val lemmata = tokenElements.map(x => (x \ "@lemma").headOption.map(_.text.trim).getOrElse(""))
    val relevances =  tokenElements.map(x => (x \ "@sense-id").nonEmpty).map(x => if (x) "yes" else "no")
    //System.err.println(relevances)
    val xml_ids =  tokenElements.map(x => getId(x))

    def enhancePos(w: Node, i: Int) = {
      val p =  (w \ "@pos").headOption.getOrElse(w \ "@type").text.trim
      if ((w \ "@type").text=="multiw") {
        println(w)
        val n = getN(w)
        val hasPrev = indexedTokenElements.exists({case (w,i1) => getN(w) == n && i1 < i })
        val hasNext = indexedTokenElements.exists({case (w,i1) => getN(w) == n && i1 > i })

        val bio =
          (hasPrev,hasNext) match {
            case (true,true) => "i"
            case (true,false) => "f"
            case (false,true) => "b"
            case _ => "o"
          }
        val t = p + "_" + bio
        println(t)
        t
      } else p
    }
    val enhancedTags = indexedTokenElements.map({case (x,y) => enhancePos(x,y)})

    Sentence("",tokens, enhancedTags, lemmata, xml_ids, file=f,relevances=relevances)
  }

  def Nodes2JSON(d: Iterator[(String, Elem)], fout: String, sentence_element:String=sentence_element): Unit = {
    val pw = new PrintWriter(new GZIPOutputStream(new java.io.FileOutputStream(fout)))

    val esjes: Iterator[(String, Node)] = d.flatMap({
      case (f: String, x: Node) => {
        if ((x \\ sentence_element).nonEmpty)
          (x \\ sentence_element).iterator.map(s => f -> s)
        else {
          val chunks = (x \\ "w").grouped(chunkSize).toList.map(chunk => {
            <s>
              {chunk}
            </s>
          })
          chunks.iterator.map(c => f -> c)
        }
      }
    })

    val sentences: Iterator[Sentence] = esjes.map({case (f,s) => sentence(s,f)})

    //val words = (d \\ "w").size
    //println("Sentences:" + s0.size  + " Words: " + words)
    val s1 = sentences.zipWithIndex.map({case (s,i) => s.copy(id=i.toString)})
    val jsons = s1.map(s => write(s))

    jsons.foreach(x => { pw.println(x)})
    pw.close()
  }

  def toJSON(f: Seq[String], fout: String): Unit = Nodes2JSON(f.iterator.map(x => x -> XML.load(x)), fout)

  val openDBNL = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Tagged/"
  lazy val ideeen: Seq[String] = new java.io.File(openDBNL).listFiles().filter(_.getName.contains("mult")).toSeq.map(_.getCanonicalPath)
  val example = "data/20220421_cobalt/CobaltServeExport/docpid_1.xml"

  val BaB = "/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuit/2.7CHN/" // ai heeft geen zinnen..... Willekeurig aanmaken?
  val dbnl19 = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Selectie19"
  val dbnl18 = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Selectie18"
  val gtbCit = "/mnt/Projecten/Corpora/Historische_Corpora/Wolkencorpus/GTB/CitatenTDN2/Refurbished/"

  def main(args: Array[String]): Unit = {
    val filesToProcess: Seq[String] = if (args.isEmpty) ideeen else args.toSeq.flatMap(x => {
      val f = new java.io.File(x)
      if (f.isFile) Seq(f.getCanonicalPath) else f.listFiles.toSeq.map(_.getCanonicalPath)
     })

    println(filesToProcess)
    toJSON(filesToProcess, "/tmp/out.json.gz")
  }
}
