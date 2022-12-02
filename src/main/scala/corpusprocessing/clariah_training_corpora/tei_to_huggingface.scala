package corpusprocessing.clariah_training_corpora
import scala.xml._
import fixTokenization.{bartje, getId}

import java.io.{File, PrintWriter}
import utils.{PostProcessXML, ProcessFolder}

import java.util.zip.GZIPOutputStream

// {"id":"0","tokens":["@paulwalk","It","'s","the","view","from","where","I","'m","living","for","two","weeks",".","Empire","State","Building","=","ESB",".","Pretty","bad","storm","here","last","evening","."],"ner_tags":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,8,8,0,7,0,0,0,0,0,0,0,0]}
import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write



trait tei_to_huggingface_trait {

  case class Sentence(
                       id: String,
                       tokens: List[String],
                       tags: List[String],
                       lemmata: List[String],
                       xml_ids: List[String]  = List(),
                       relevances: List[String]  = List(),
                       hilex_pos : List[String]  = List(),
                       file: String = "unknown" // pas op andere dingen hebben dit niet
                     )

  val sentence_element="q"
  val pos_attribute = "@pos"
  val chunkSize= 50
  val test_sample_rate = 0.2
  val split_test_train_on_document_level = false
  val output_folder= "/tmp"
  val output_prefix = "tei_to_huggingface"

  implicit val formats = DefaultFormats

  def sentence(s: Node, f: String) = {

    def getN(n: Node) =  (n \ "@n").text

    val tokenElements = s.descendant.toList.filter(n => Set("w", "pc").contains(n.label))
    val indexedTokenElements = tokenElements.zipWithIndex
    val tokens = tokenElements.map(x => if ( (x \\ "seg").nonEmpty) (x \\ "seg").text  else x.text.trim)
    val tags = tokenElements.map(x => (x \ pos_attribute).headOption.getOrElse(x \ "@type").text.trim)
    val lemmata = tokenElements.map(x => (x \ "@lemma").headOption.map(_.text.trim).getOrElse(""))
    val relevances =  tokenElements.map(x => (x \ "@sense-id").nonEmpty).map(x => if (x) "yes" else "no")
    val hilex_pos = tokenElements.map(x => (x \ "@hilex-pos").headOption.map(_.text.trim).getOrElse("unk"))
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

    Sentence("",tokens, enhancedTags, lemmata, xml_ids, file=f,relevances=relevances,hilex_pos=hilex_pos)
  }

  def decentSentence(s: Sentence, b: Boolean)  = true


  def Nodes2JSON(d: Iterator[(String, Elem)], fout: String, sentence_element:String=sentence_element): Unit =
  {
    val pwTrain = new PrintWriter(new GZIPOutputStream(new java.io.FileOutputStream(fout + ".train.json.gz")))
    val pwTest = new PrintWriter(new GZIPOutputStream(new java.io.FileOutputStream(fout  + ".test.json.gz")))

    val esjes: Iterator[(String, Node, Boolean)] = d.flatMap({

      case (f: String, x: Node) => {
        val doc_in_test = Math.random() < test_sample_rate
        if ((x \\ sentence_element).nonEmpty)
          (x \\ sentence_element).iterator.map(s => (f,s,doc_in_test))
        else {
          val chunks = (x \\ "w").grouped(chunkSize).toList.map(chunk => {
            <s>
              {chunk}
            </s>
          })
          chunks.iterator.map(c =>  (f,c,doc_in_test))
        }
      }
    })

    val sentences: Iterator[(Sentence,Boolean)] = esjes.map({case (f,s,is_test_doc) => sentence(s,f) -> is_test_doc}).filter({case (s,is_test_doc) => decentSentence(s,is_test_doc)})

    val sampled: Iterator[(Sentence, Boolean)] = sample(sentences)

    //val words = (d \\ "w").size
    //println("Sentences:" + s0.size  + " Words: " + words)

    val s1: Iterator[(Sentence, Boolean)] = sampled.zipWithIndex.map({case ((s,b),i) => s.copy(id=i.toString) -> b})
    val jsons: Iterator[(String, Boolean)] = s1.map({case (s,b) => write(s) -> b})

    jsons.foreach({case (json,b) => if (b) pwTest.println(json) else pwTrain.println(json)})
    pwTest.close()
    pwTrain.close()
  }


  def always_sampled(s: Sentence) = true

  def sample(sentences: Iterator[(Sentence,Boolean)], sample_rate: Double = 0.05, rate_test_train: Double = test_sample_rate): Iterator[(Sentence, Boolean)] = {

    def  selected(s: Sentence) = (Math.random() < sample_rate) || always_sampled(s)

    sentences.filter({ case (s,b) => selected(s)}).map({ case (s,b) => {
      if (split_test_train_on_document_level && b || (!split_test_train_on_document_level && Math.random() < rate_test_train)) (s, true) else (s,false)
    }})
  }


  def toJSON(f: Seq[String], fout: String, preprocess: Elem=>Elem = x => x): Unit = Nodes2JSON(f.iterator.map(x => x -> preprocess(XML.load(x))), fout)

  val openDBNL = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Tagged/"
  lazy val ideeen: Seq[String] = new java.io.File(openDBNL).listFiles().filter(_.getName.contains("mult")).toSeq.map(_.getCanonicalPath)

  val example = "data/20220421_cobalt/CobaltServeExport/docpid_1.xml"

  val BaB = "/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuit/2.7CHN/" // ai heeft geen zinnen..... Willekeurig aanmaken?
  val dbnl19 = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Selectie19"
  val dbnl18 = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Selectie18"


  val default_dataset = ideeen
  def default_process(e: Elem)  = e

  val default_folder = "hadjememaar"
  def main(args0: Array[String]): Unit = {

    val args = if (args0.size > 0) args0 else Array(default_folder)

    val filesToProcess: Seq[String] = args.toSeq.flatMap(x => {
      val f = new java.io.File(x)
      if (f.isFile) Seq(f.getCanonicalPath) else f.listFiles.toSeq.map(_.getCanonicalPath)
     })

    println(filesToProcess)
    toJSON(filesToProcess, output_folder + "/" + output_prefix, preprocess = default_process)
  }
}

object tei_to_huggingface extends tei_to_huggingface_trait {
}

object gtbcit_to_huggingface extends tei_to_huggingface_trait {
  val gtbCit = "/mnt/Projecten/Corpora/Historische_Corpora/Wolkencorpus/GTB/CitatenTDN2/Refurbished/"

  override  def always_sampled(s: Sentence) = s.hilex_pos.indices.exists(i => s.relevances(i)   == "yes" && s.hilex_pos(i).matches(".*(PD|CON|ADP|NUM|INT)"))

  def setPos(w: Elem, p:String) = w.copy(attributes =  w.attributes.append(new UnprefixedAttribute("hilex-pos", p, Null)))

  override def decentSentence(s: Sentence, b: Boolean)  = s.hilex_pos.exists(x => x != "unk")

  def propagateHilexPos(d: Elem): Elem = {
    PostProcessXML.updateElement(d,_.label=="cit", cit =>  {
      val pos = (cit \ "@pos").text
      // System.err.println(pos)
      PostProcessXML.updateElement(cit,_.label=="w", w => setPos(w,pos))
    })
  }

  override def default_process(e: Elem): Elem = propagateHilexPos(e)

}

object ofr_to_huggingface extends tei_to_huggingface_trait {
  override val pos_attribute = "@type"
}

object bab_to_huggingface extends tei_to_huggingface_trait {
  override val split_test_train_on_document_level: Boolean = true
  override val output_prefix: String = "bab"
  override val default_folder = "/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuit/2.8TDN/"
}


