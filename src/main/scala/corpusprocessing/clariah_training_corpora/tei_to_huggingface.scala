package corpusprocessing.clariah_training_corpora
import corpusprocessing.clariah_training_corpora.crm_to_huggingface.training_subsets

import scala.xml._
import fixTokenization.{bartje, getId}
import posmapping.TagsetDiachroonNederlands

import java.io.{File, PrintWriter}
import utils.{PostProcessXML, ProcessFolder}

import java.util.zip.GZIPOutputStream
import scala.util.Random

// {"id":"0","tokens":["@paulwalk","It","'s","the","view","from","where","I","'m","living","for","two","weeks",".","Empire","State","Building","=","ESB",".","Pretty","bad","storm","here","last","evening","."],"ner_tags":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,8,8,0,7,0,0,0,0,0,0,0,0]}
import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write


case class Partition(part: String, portion: Int) {
  lazy val prefix: String = if (part == TRAIN.part && training_subsets > 1 && portion > 0) s"$part.$portion" else part
}

object TRAIN extends Partition("train", -1)
object DEV extends Partition("dev", -1)
object TEST extends  Partition("test", -1)
trait tei_to_huggingface_trait {
  trait Sentence {
    def id:Option[String] = None
    def toTSV(addSourceLine: Boolean=false) : String = {

      this match {
        case s:BasicSentence => {
          val sourceLine =  if (addSourceLine) s"#### ${s.file} ####\n" else ""
          sourceLine +
          s.tokens.indices.map(i => List(s.tokens(i), s.tags(i), s.lemmata(i)).mkString("\t")).mkString("\n")
        }
        case s:PimpedSentence => {
          val sourceLine =  if (addSourceLine) s"#### ${s.file} ####\n" else ""
          sourceLine +
            s.tokens.indices.map(i => List(s.tokens(i), s.tags(i), s.lemmata(i)).mkString("\t")).mkString("\n")
        }
      }
    }
    def file: String = "unknown"
  }
  case class PimpedSentence(
                       override val id: Option[String],
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
                             override val  id: Option[String],
                             tokens: List[String],
                             tags: List[String],
                             lemmata: List[String],
                             xml_ids: List[String] = List(),
                             override val  file: String = "unknown",
                           ) extends Sentence
  val sentence_element="q"
  val pos_attribute = "@pos"


  val chunkSize = 50




  lazy val partitions = if (training_subsets <= 1) List(TRAIN, DEV,  TEST) else (0 to training_subsets).map(i => TRAIN.copy(portion = i)).toList ++ List(DEV, TEST)



  val p_train = 0.75
  val p_dev = 0.125

  val split_test_train_on_document_level = false
  val training_subsets : Int = 1
  lazy val output_folder= "/tmp"
  lazy val output_prefix = "tei_to_huggingface"
  val enhance : Boolean = false
  val addStuff: Boolean = false

  implicit val formats = DefaultFormats

  def getId(n: Node): Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).headOption

  def tagMapping(s: String)  = s

  def transformToken(w: Node) = w

  def sentence(s: Node, f: String): Sentence = {

    def getN(n: Node) =  (n \ "@n").text

    val id: Option[String]  = getId(s)

    // Console.err.println(s"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ID: $id for $s") ((x \ "choice").nonEmpty)

    val tokenElements = s.descendant.toList.filter(n => Set("w", "pc").contains(n.label)).map(transformToken)
    val indexedTokenElements = tokenElements.zipWithIndex
    val tokens =
      tokenElements.map(x =>
        if ( (x \\ "seg").nonEmpty) (x \\ "seg").text.replaceAll("\\s+", " ")
        else if ((x \ "choice").nonEmpty) (x \ "choice" \ "sic").text
        else x.text.trim)
    val tags = tokenElements.map(x => (x \ pos_attribute).headOption.getOrElse(x \ "@type").text.trim)
    val lemmata = tokenElements.map(x => (x \ "@lemma").headOption.map(_.text.trim).getOrElse(""))
    val relevances =  tokenElements.map(x => (x \ "@sense-id").nonEmpty).map(x => if (x) "yes" else "no")
    val hilex_pos = tokenElements.map(x => (x \ "@hilex-pos").headOption.map(_.text.trim).getOrElse("unk"))

    //System.err.println(relevances)

    val xml_ids =  tokenElements.map(x => getId(x).getOrElse("no_id_found"))

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
      PimpedSentence(id,tokens, if (enhance) enhancedTags else tags.map(tagMapping), lemmata, xml_ids, file=f, relevances=relevances,hilex_pos=hilex_pos,partition = partition)
    else BasicSentence(id,tokens, if (enhance) enhancedTags else tags.map(tagMapping), lemmata, xml_ids, file=f)
    r
  }

  def decentSentence(s: Sentence, b: Partition)  = true

  val maxje = 100
  def pickPartition(f: Option[String]): Partition = {
    val r = Math.random()
    val portion = Math.floor(Math.random()* this.training_subsets).toInt

    val partition = if (r < p_train)
      TRAIN
    else if (r < p_train + p_dev)
      DEV
    else TEST

    val p1 = if (partition.part == TRAIN.part && this.training_subsets > 1) partition.copy(portion = portion) else partition
    // Console.err.println(s"$r -> $p1 ($p_train, $p_dev)")
    p1
  }

  def processDocuments(documents: Iterator[(String, Elem)], outputPrefix: String, sentence_element:String=sentence_element): Unit =
  {

    Console.err.println(s"Output to: $outputPrefix")

    val printWritersJSON: Map[Partition, PrintWriter] = partitions.map(p => p ->
      new PrintWriter(new GZIPOutputStream(new java.io.FileOutputStream(outputPrefix + s".${p.prefix}.json.gz")))).toMap
    val printWritersTSV: Map[Partition, PrintWriter] = partitions.map(p => p ->
      new PrintWriter(new GZIPOutputStream(new java.io.FileOutputStream(outputPrefix + s".${p.prefix}.tsv.gz")))).toMap
    val printWritersDocumentPartition: Map[Partition, PrintWriter]  = partitions.map(p => p ->
      new PrintWriter(new GZIPOutputStream(new java.io.FileOutputStream(outputPrefix + s".${p.prefix}.filenames.gz")))).toMap

    val partitioned_s_elements: Iterator[(String, Node, Partition)] = documents.flatMap({

      case (f: String, x: Node) => {
        val documentPartition = pickPartition(Some(f))
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
      case ((s:PimpedSentence,b),i) => s.copy(id=Some(s.id.getOrElse(i.toString))) -> b
      case ((s:BasicSentence,b),i) => s.copy(id=Some(s.id.getOrElse(i.toString))) -> b
    })

    val sentencesForExport: Iterator[(String, String, Partition, String, String)] = s1.map({case (s: Sentence,b) => (write(s), s.toTSV(), b, s.file, s.id.getOrElse("no_id_found"))})

    val partitionedSentences: Set[(String, Partition, String)] = sentencesForExport.map({ case (json, tsv, b, f, id) =>
      val pwJSON = printWritersJSON(b)
      val pwTSV = printWritersTSV(b)

      pwTSV.println("")
      pwTSV.println(tsv)
      pwJSON.println(json)

      (f,b,id)
    }).toSet


    if (this.split_test_train_on_document_level) {
      val partitionedDocuments = partitionedSentences.map({case (a,b,c) => (a,b)})
        partitionedDocuments.foreach({case (f,p) =>
        val pw = printWritersDocumentPartition(p)
        pw.println(f)
      })
    } else {
      partitionedSentences.foreach({
        case (f, p, id) =>
          val pw = printWritersDocumentPartition(p)
          pw.println(s"$f\t$id")
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
        val p1 = pickPartition(None)
        (s,p1)
      }
    }})
  }

  def preprocess(x: Elem)  = x
  def makeTrainingMaterialSplit(filenames: Seq[String], outputPrefix: String, preprocess: Elem=>Elem = preprocess): Unit = processDocuments(filenames.iterator.map(x => x -> preprocess(XML.load(x))), outputPrefix)

  val openDBNL = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Tagged/"
  lazy val ideeen: Seq[String] = new java.io.File(openDBNL).listFiles().filter(_.getName.contains("mult")).toSeq.map(_.getCanonicalPath)

  val example = "data/20220421_cobalt/CobaltServeExport/docpid_1.xml"

  val BaB = "/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuit/2.7CHN/" // ai heeft geen zinnen..... Willekeurig aanmaken?
  val dbnl19 = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Selectie19"
  val dbnl18 = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Selectie18"


  val default_dataset = ideeen
  def default_process(e: Elem)  = e

  lazy val default_folder = "hadjememaar"

  val max_files = Integer.MAX_VALUE

  def main(args0: Array[String]): Unit = {

    val args = if (args0.size > 0) args0 else Array(default_folder)

    val filesToProcess: Seq[String] = args.toSeq.flatMap(x => {
      val f = new java.io.File(x)
      if (f.isFile) Seq(f.getCanonicalPath) else f.listFiles.toSeq.map(_.getCanonicalPath)
     })

    println(filesToProcess)
    val maybeShuffled = if (this.training_subsets > 1) Random.shuffle(filesToProcess) else filesToProcess
    makeTrainingMaterialSplit(maybeShuffled.take(max_files), output_folder + "/" + output_prefix, preprocess = preprocess)
  }
}

object tei_to_huggingface extends tei_to_huggingface_trait {
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
  override   lazy val default_folder = "/mnt/Projecten/Corpora/Historische_Corpora/OudFries/RitaVdPoel/corpusfiles/"
  override val split_test_train_on_document_level = true
  override lazy val output_prefix = "ofr"
  override def decentSentence(s: Sentence, b: Partition)  =  {
    val tags =  s.asInstanceOf[BasicSentence].tags
    tags.count(_.nonEmpty) > 0.7 * tags.size
  }
}
//
object onw_to_huggingface extends tei_to_huggingface_trait {
  override val pos_attribute = "@pos"
  override   lazy val default_folder = "/mnt/Projecten/Corpora/Historische_Corpora/ONW/ONW-januari-2022/"
  override val split_test_train_on_document_level = false
  override lazy val output_prefix = "onw"
  override def decentSentence(s: Sentence, b: Partition)  =  {
    val tags =  s.asInstanceOf[BasicSentence].tags
    tags.count(t => t.nonEmpty && !t.contains("RES")) > 0.6 * tags.size
  }
}
object bab_to_huggingface extends tei_to_huggingface_trait {
  override val split_test_train_on_document_level: Boolean = true
  override lazy val output_prefix: String = "bab"
  override val max_files: Int = Integer.MAX_VALUE
  override val training_subsets: Int = 10
  override lazy val output_folder = "/mnt/Projecten/Corpora/TrainingDataForTools/BaB/All/"  + "test_train" + (if (training_subsets > 1) "/partitioned/" else "")
  new File(output_folder).mkdir()
  override lazy val default_folder = "/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuit/2.8TDN/"
}







object crm_to_huggingface extends tei_to_huggingface_trait {
  override val split_test_train_on_document_level: Boolean = true
  override lazy val output_prefix: String = "CRM"
  override val max_files: Int = Integer.MAX_VALUE // 500
  override val training_subsets: Int = 10
  override lazy val output_folder: String = "/mnt/Projecten/Corpora/TrainingDataForTools/CRM/All/" + "/" + "test_train" + (if (training_subsets > 1) "/partitioned/" else "")
  //override val output_folder: String = "/mnt/Projecten/Corpora/TrainingDataForTools/CRM/All/"
  override lazy val default_folder = "/mnt/Projecten/Corpora/Historische_Corpora/CRM/TEI-tagmapped/"
}


