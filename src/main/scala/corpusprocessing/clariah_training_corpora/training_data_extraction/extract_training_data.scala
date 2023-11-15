package corpusprocessing.clariah_training_corpora.training_data_extraction

import corpusprocessing.clariah_training_corpora.training_data_extraction.crm_to_huggingface.training_subsets
import utils.PostProcessXML

import java.io.{File, PrintWriter}
import java.util.zip.GZIPOutputStream
import scala.util.Random
import scala.xml._

// {"id":"0","tokens":["@paulwalk","It","'s","the","view","from","where","I","'m","living","for","two","weeks",".","Empire","State","Building","=","ESB",".","Pretty","bad","storm","here","last","evening","."],"ner_tags":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,8,8,0,7,0,0,0,0,0,0,0,0]}
import org.json4s._
import org.json4s.jackson.Serialization.write
import Sentence._

case class Partition(part: String, portion: Int) {
  lazy val prefix: String = if (part == TRAIN.part && training_subsets > 1 && portion >= 0) s"$part.$portion" else part
}

object TRAIN extends Partition("train", -1)
object DEV extends Partition("dev", -1)
object TEST extends  Partition("test", -1)
trait extract_training_data_trait {

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

  def tagMapping(s: String): String = s

  def transformToken(w: Node): Node = w




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

    val sentences: Iterator[(Sentence,Partition)] = partitioned_s_elements.map({case (f,s,documentPartition) => sentence(s,f,this) -> documentPartition}).filter({case (s,documentPartition) => decentSentence(s,documentPartition)})

    val sampled: Iterator[(Sentence, Partition)] = sample(sentences)

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

  def preprocess(x: Elem): Elem = x
  def makeTrainingMaterialSplit(filenames: Seq[String], outputPrefix: String, preprocess: Elem=>Elem = preprocess): Unit = processDocuments(filenames.iterator.map(x => x -> preprocess(XML.load(x))), outputPrefix)


  val default_dataset: Seq[String] = Seq()

  lazy val default_folder = "hadjememaar"

  val max_files = Integer.MAX_VALUE

  def main(args0: Array[String]): Unit = {

    val args = if (args0.size > 0) args0 else Array(default_folder)

    val filesToProcess: Seq[String] = args.toSeq.flatMap(x => {
      val f = new java.io.File(x)
      if (f.isFile) Seq(f.getCanonicalPath) else f.listFiles.toSeq.map(_.getCanonicalPath)
     })

    // println(filesToProcess)
    val maybeShuffled = if (this.training_subsets > 1) Random.shuffle(filesToProcess) else filesToProcess
    makeTrainingMaterialSplit(maybeShuffled.take(max_files), output_folder + "/" + output_prefix, preprocess = preprocess)
  }
}




