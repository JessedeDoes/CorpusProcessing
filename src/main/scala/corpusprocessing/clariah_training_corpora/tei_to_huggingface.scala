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
  trait Sentence {
    def toTSV() : String = {
      this match {
        case s:BasicSentence => {
          s"#### ${s.file} ####\n" +
          s.tokens.indices.map(i => List(s.tokens(i), s.tags(i), s.lemmata(i)).mkString("\t")).mkString("\n")
        }
        case s:PimpedSentence => {
          s"#### ${s.file} ####\n" +
            s.tokens.indices.map(i => List(s.tokens(i), s.tags(i), s.lemmata(i)).mkString("\t")).mkString("\n")
        }
      }
    }
    def file: String = "unknown"
  }
  case class PimpedSentence(
                       id: String,
                       tokens: List[String],
                       tags: List[String],
                       lemmata: List[String],
                       xml_ids: List[String]  = List(),
                       relevances: List[String]  = List(),
                       hilex_pos : List[String]  = List(),
                       override val file: String = "unknown",
                       partition: String = "unknown"// pas op andere dingen hebben dit niet
                     ) extends Sentence

  case class BasicSentence(
                             id: String,
                             tokens: List[String],
                             tags: List[String],
                             lemmata: List[String],
                             xml_ids: List[String] = List(),
                             override val  file: String = "unknown",
                           ) extends Sentence
  val sentence_element="q"
  val pos_attribute = "@pos"


  val chunkSize= 50

  type Partition = String
  val TRAIN: Partition = "train"
  val DEV: Partition = "dev"
  val TEST: Partition = "test"

  val partitions = List("train", "dev", "test")

  val p_train = 0.75
  val p_dev = 0.125

  val split_test_train_on_document_level = false
  val output_folder= "/tmp"
  val output_prefix = "tei_to_huggingface"
  val enhance : Boolean = false
  val addStuff: Boolean = false

  implicit val formats = DefaultFormats

  def sentence(s: Node, f: String): Sentence = {

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
    val partition = (s \ "@ana").headOption.map(_.text.replaceAll("#","")).getOrElse("unknown")

    // println(s.asInstanceOf[Elem].copy(child=Seq()))
    // println(s.attributes.toString + "->" + partition)
    val r = if (addStuff)
      PimpedSentence("",tokens, if (enhance) enhancedTags else tags, lemmata, xml_ids, file=f, relevances=relevances,hilex_pos=hilex_pos,partition = partition)
    else BasicSentence("",tokens, if (enhance) enhancedTags else tags, lemmata, xml_ids, file=f)
    r
  }

  def decentSentence(s: Sentence, b: Partition)  = true

  def pickPartition(): Partition = {
    val r = Math.random()
    val partition = if (r < p_train)
      TRAIN
    else if (r < p_train + p_dev)
      DEV
    else TEST
    Console.err.println(s"$r -> $partition ($p_train, $p_dev)")
    partition
  }
  def Nodes2JSON(documents: Iterator[(String, Elem)], outputPrefix: String,  sentence_element:String=sentence_element): Unit =
  {

    Console.err.println(s"Output to: $outputPrefix")

    val printWritersJSON: Map[Partition, PrintWriter] = partitions.map(p => p ->
      new PrintWriter(new GZIPOutputStream(new java.io.FileOutputStream(outputPrefix + s".$p.json.gz")))).toMap
    val printWritersTSV: Map[Partition, PrintWriter] = partitions.map(p => p ->
      new PrintWriter(new GZIPOutputStream(new java.io.FileOutputStream(outputPrefix + s".$p.tsv.gz")))).toMap
    val printWritersDocumentPartition: Map[Partition, PrintWriter]  = partitions.map(p => p ->
      new PrintWriter(new GZIPOutputStream(new java.io.FileOutputStream(outputPrefix + s".$p.filenames.gz")))).toMap

    val partitioned_s_elements: Iterator[(String, Node, Partition)] = documents.flatMap({

      case (f: String, x: Node) => {
        val documentPartition = pickPartition()
        if ((x \\ sentence_element).nonEmpty)
          (x \\ sentence_element).iterator.map(s => (f,s,documentPartition))
        else {
          val chunks = (x \\ "w").grouped(chunkSize).toList.map(chunk => {
            <s ana="#chunk">
              {chunk}
            </s>
          })
          chunks.iterator.map(c =>  (f, c, documentPartition))
        }
      }
    })

    val sentences: Iterator[(Sentence,Partition)] = partitioned_s_elements.map({case (f,s,documentPartition) => sentence(s,f) -> documentPartition}).filter({case (s,documentPartition) => decentSentence(s,documentPartition)})

    val sampled: Iterator[(Sentence, Partition)] = sample(sentences)

    //val words = (d \\ "w").size
    //println("Sentences:" + s0.size  + " Words: " + words)

    val s1: Iterator[(Sentence, Partition)] = sampled.zipWithIndex.map({
      case ((s:PimpedSentence,b),i) => s.copy(id=i.toString) -> b
      case ((s:BasicSentence,b),i) => s.copy(id=i.toString) -> b
    })

    val jsons: Iterator[(String, String, Partition, String)] = s1.map({case (s: Sentence,b) => (write(s), s.toTSV(), b, s.file)})

    val partitionedDocuments: Set[(String, Partition)] = jsons.map({ case (json, tsv, b, f) =>
      val pwJSON = printWritersJSON(b)
      val pwTSV = printWritersTSV(b)

      pwTSV.println("")
      pwTSV.println(tsv)
      pwJSON.println(json)
      f -> b
    }).toSet

    if (this.split_test_train_on_document_level) {
      partitionedDocuments.foreach({case (f,p) =>
        val pw = printWritersDocumentPartition(p)
        pw.println(f)
      })
    }

    printWritersJSON.values.foreach(_.close())
    printWritersTSV.values.foreach(_.close())
    printWritersDocumentPartition.values.foreach(_.close())
  }

  def always_sampled(s: Sentence) = true

  def sample(sentences: Iterator[(Sentence,Partition)], sample_rate: Double = 0.05, rate_test_train: Double = p_train): Iterator[(Sentence, Partition)] = {

    def  selected(s: Sentence) = (Math.random() < sample_rate) || always_sampled(s)

    sentences.filter({ case (s,b) => selected(s)}).map({ case (s,p) => {
      if (split_test_train_on_document_level) {
        (s,p)
      } else {
        val p1 = pickPartition()
        (s,p1)
      }
    }})
  }

  def toJSON(filenames: Seq[String], fout: String, preprocess: Elem=>Elem = x => x): Unit = Nodes2JSON(filenames.iterator.map(x => x -> preprocess(XML.load(x))), fout)

  val openDBNL = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Tagged/"
  lazy val ideeen: Seq[String] = new java.io.File(openDBNL).listFiles().filter(_.getName.contains("mult")).toSeq.map(_.getCanonicalPath)

  val example = "data/20220421_cobalt/CobaltServeExport/docpid_1.xml"

  val BaB = "/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuit/2.7CHN/" // ai heeft geen zinnen..... Willekeurig aanmaken?
  val dbnl19 = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Selectie19"
  val dbnl18 = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Selectie18"


  val default_dataset = ideeen
  def default_process(e: Elem)  = e

  val default_folder = "hadjememaar"

  val max_files = Integer.MAX_VALUE

  def main(args0: Array[String]): Unit = {

    val args = if (args0.size > 0) args0 else Array(default_folder)

    val filesToProcess: Seq[String] = args.toSeq.flatMap(x => {
      val f = new java.io.File(x)
      if (f.isFile) Seq(f.getCanonicalPath) else f.listFiles.toSeq.map(_.getCanonicalPath)
     })

    println(filesToProcess)
    toJSON(filesToProcess.take(max_files), output_folder + "/" + output_prefix, preprocess = default_process)
  }
}

object tei_to_huggingface extends tei_to_huggingface_trait {
}


object clariah_15 extends  tei_to_huggingface_trait {
  override val default_folder = "../nephomant/data/nederval/15/CobaltServeExport/"
  override val output_folder = "/tmp/"
  override val output_prefix = "nederval_15"
}


object clariah_16 extends  tei_to_huggingface_trait {
  override val default_folder = "../nephomant/data/nederval/16/CobaltServeExport/"
  override val output_folder = "/tmp/"
  override val output_prefix = "nederval_16"
}

object clariah_17 extends  tei_to_huggingface_trait {
  override val default_folder = "../nephomant/data/nederval/17_thomas/"
  override val output_folder = "/tmp/"
  override val output_prefix = "nederval_17"
}
object clariah_18 extends  tei_to_huggingface_trait {
  override val default_folder = "../nephomant/data/nederval/18_thomas/"
  override val output_folder = "/tmp/"
  override val output_prefix = "nederval_18"
}

object clariah_19 extends  tei_to_huggingface_trait {
  override val default_folder = "../nephomant/data/nederval/19_thomas/"
  override val output_folder = "/tmp/"
  override val output_prefix = "nederval_19"
}

object clariah_several {
  def main(args: Array[String])  = {
    List(clariah_15, clariah_16, clariah_17, clariah_18, clariah_19).foreach(_.main(Array()))
  }
}

object gtbcit_to_huggingface extends tei_to_huggingface_trait {
  val gtbCit = "/mnt/Projecten/Corpora/Historische_Corpora/Wolkencorpus/GTB/CitatenTDN2/Refurbished/"

  override  def always_sampled(s1: Sentence) = {
    val s = s1.asInstanceOf[PimpedSentence]
    s.hilex_pos.indices.exists(i => s.relevances(i) == "yes" && s.hilex_pos(i).matches(".*(PD|CON|ADP|NUM|INT)"))
  }

  def setPos(w: Elem, p:String) = w.copy(attributes =  w.attributes.append(new UnprefixedAttribute("hilex-pos", p, Null)))

  override def decentSentence(s: Sentence, b: Partition)  = s.asInstanceOf[PimpedSentence].hilex_pos.exists(x => x != "unk")

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
  override   val default_folder = "/mnt/Projecten/Corpora/Historische_Corpora/OudFries/RitaVdPoel/corpusfiles/"
  override val split_test_train_on_document_level = true
  override val output_prefix = "ofr"
  override def decentSentence(s: Sentence, b: Partition)  =  {
    val tags =  s.asInstanceOf[BasicSentence].tags
    tags.count(_.nonEmpty) > 0.7 * tags.size
  }
}

object bab_to_huggingface extends tei_to_huggingface_trait {
  override val split_test_train_on_document_level: Boolean = true
  override val output_prefix: String = "bab"
  override val default_folder = "/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuit/2.8TDN/"
}

object gysseling_to_hugginface extends tei_to_huggingface_trait {
  override val split_test_train_on_document_level: Boolean = true
  override val output_prefix: String = "gys"
  override val max_files: Int = Integer.MAX_VALUE // 500
  override val output_folder: String = "/mnt/Projecten/Corpora/TrainingDataForTools/Gysseling/All/"
  override val default_folder = "/mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/TeIndexeren/2020_07_31/"
}


object crm_to_huggingface extends tei_to_huggingface_trait {
  override val split_test_train_on_document_level: Boolean = true
  override val output_prefix: String = "CRM"
  override val max_files: Int = Integer.MAX_VALUE // 500
  override val output_folder: String = "/mnt/Projecten/Corpora/TrainingDataForTools/CRM/All/"
  override val default_folder = "/mnt/Projecten/Corpora/Historische_Corpora/CRM/TEI-tagmapped/"
}


