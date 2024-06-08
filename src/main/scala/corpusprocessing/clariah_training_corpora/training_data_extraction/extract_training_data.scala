package corpusprocessing.clariah_training_corpora.training_data_extraction

import corpusprocessing.clariah_training_corpora.training_data_extraction.specific.crm_to_huggingface.training_subsets
import utils.PostProcessXML

import java.io.{File, PrintWriter}
import java.util.zip.GZIPOutputStream
import scala.util.Random
import scala.xml._

// {"id":"0","tokens":["@paulwalk","It","'s","the","view","from","where","I","'m","living","for","two","weeks",".","Empire","State","Building","=","ESB",".","Pretty","bad","storm","here","last","evening","."],"ner_tags":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,8,8,0,7,0,0,0,0,0,0,0,0]}
import org.json4s._
import org.json4s.jackson.Serialization.write
import Sentence._
import java.nio.file.{Files, Path}
case class Partition(part: String, portion: Int = -1) {
  lazy val prefix: String = if (part == TRAIN.part && training_subsets > 1 && portion >= 0) s"$part.$portion" else part
}

object TRAIN extends Partition("train", -1)
object DEV extends Partition("dev", -1)
object TEST extends  Partition("test", -1)
trait TrainingDataExtraction {

  val sentence_element="q"
  val pos_attribute = "@pos"
  val chunkSize = 50
  lazy val partitions = if (training_subsets <= 1) List(TRAIN, DEV,  TEST) else (0 to training_subsets).map(i => TRAIN.copy(portion = i)).toList ++ List(DEV, TEST)
  val p_train = 0.75
  val p_dev = 0.125
  val split_test_train_on_document_level = false
  val clean_brackets = false
  val training_subsets : Int = 1
  lazy val output_folder = "/tmp"
  lazy val output_prefix = "tei_to_huggingface"
  val enhance : Boolean = false
  val addStuff: Boolean = false
  val shuffleDocuments: Boolean = false

  implicit val formats = DefaultFormats

  def getId(n: Node): Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).headOption

  def tagMapping(s: String): String = s

  def transformToken(w: Node): Node = w

  def decentSentence(s: Sentence)  = true

  val maxje = 100

  def pickPartition(fileId: Option[String]=None, sentenceId: Option[String]=None): Partition = {
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

  def processDocuments(documents: Iterator[(String, Elem)], outputPrefix: String, sentence_element:String=sentence_element)  =
  {

    Console.err.println(s"Output to: $outputPrefix")

    val printWritersJSON: Map[Partition, PrintWriter] = partitions.map(p => p ->
      new PrintWriter(new GZIPOutputStream(new java.io.FileOutputStream(outputPrefix + s".${p.prefix}.json.gz")))).toMap
    val printWritersTSV: Map[Partition, PrintWriter] = partitions.map(p => p ->
      new PrintWriter(new GZIPOutputStream(new java.io.FileOutputStream(outputPrefix + s".${p.prefix}.tsv.gz")))).toMap
    val printWritersDocumentPartition: Map[Partition, PrintWriter]  = partitions.map(p => p ->
      new PrintWriter(new GZIPOutputStream(new java.io.FileOutputStream(outputPrefix + s".${p.prefix}.filenames.gz")))).toMap

    val sentences = documents.flatMap({
      case (f: String, x: Node) => {
        val documentPartition = Some(pickPartition(Some(f)))

        // println(s"$f $documentPartition")

        val chunkElements =
          if ((x \\ sentence_element).nonEmpty)
          (x \\ sentence_element)
          else x.descendant
            .filter(d => Set("w", "pc").contains(d.label))
            .grouped(chunkSize).toList.map(chunk => {
            <s ana="#chunk">{chunk}</s>
          })

        chunkElements.iterator
          .map(s => sentence(s, f, extractor=this, partition = documentPartition))
          .filter(decentSentence)
      }
    }).filter(!_.isEmpty)

    val sampledSentences: Iterator[Sentence] = sampleAndChooseSentencePartitions(sentences)

    val sentencesWithIds: Iterator[Sentence] = sampledSentences.zipWithIndex.map({
      case (s:BasicSentence ,i) => s.copy(sentenceId=Some(s.sentenceId.getOrElse(i.toString)))
    })


    case class SentenceSerializations(s: Sentence) {
      lazy val json = write(s)
      lazy val tsv = s.toTSV()
    }

    val sentencesForExport: Iterator[(String, String, Partition, String, String)] = sentencesWithIds.map(s=> (write(s), s.toTSV(), s.partition.get,s.fileId, s.sentenceId.getOrElse("no_id_found")))

    val partitionedSentences: Set[(String, Partition, String)] = {
      val printed = sentencesForExport.map({ case (json, tsv, b, f, id) =>
        val pwJSON = printWritersJSON(b)
        val pwTSV = printWritersTSV(b)

        pwTSV.println("")
        pwTSV.println(tsv)
        pwJSON.println(json)

        (f, b, id)
      })
      printed.toSet
    }


    val partitionInformation: Set[(String, (String, Option[String]))] =
      if (this.split_test_train_on_document_level) {
      val partitionedDocuments = partitionedSentences.map({case (a,b,c) => (a,b)})
        partitionedDocuments.map({case (f,p) =>
          val pw = printWritersDocumentPartition(p)
          pw.println(f)
          p.prefix -> (f,None.asInstanceOf[Option[String]])
      })
    } else {
      partitionedSentences.map({
        case (f, p, id) =>
          val pw = printWritersDocumentPartition(p)
          pw.println(s"$f\t$id")
          p.prefix -> (f,Some(id).asInstanceOf[Option[String]])
      })
    }

    val infoAsJSON = TrainingDataInfo.info2Json(partitionInformation)

    val jsonInfoWriter = new PrintWriter(outputPrefix + s".partitionInformation.json")

    jsonInfoWriter.println(infoAsJSON)
    jsonInfoWriter.close()

    printWritersJSON.values.foreach(_.close())
    printWritersTSV.values.foreach(_.close())
    printWritersDocumentPartition.values.foreach(_.close())

    TrainingDataInfo.info2Object(partitionInformation, sentence_element)
  }

  def always_sampled(s: Sentence) = true

  def sampleAndChooseSentencePartitions(sentences: Iterator[Sentence], sample_rate: Double = 0.05): Iterator[Sentence] = {
    def  inSample(s: Sentence) = (Math.random() < sample_rate) || always_sampled(s)

    sentences.filter(s => inSample(s))
      .map(s => {
      if (split_test_train_on_document_level) {
        s
      } else {
        val p1 = pickPartition(Some(s.fileId), s.sentenceId)
        s.asInstanceOf[BasicSentence].copy(partition = Some(p1))
      }
    })
  }

  def preprocess(x: Elem): Elem = x

  def loadXML(fileName: String): Elem = preprocess(XML.load(fileName))

  def makeTrainingMaterialAndPartition(filenames: Seq[String], outputPrefix: String, preprocess: Elem=>Elem = preprocess): Unit =
    processDocuments(filenames.iterator.filter(_.contains("xml")).map(x => x -> loadXML(x)), outputPrefix)

  def makeTrainingMaterialAndPartitionFromPaths(paths: Seq[Path], outputPrefix: String, preprocess: Elem => Elem = preprocess): TrainingDataInfo =
    {
       val iterator = (if (shuffleDocuments) Random.shuffle(paths) else paths).iterator.map(p => {
        val c = p.getNameCount
        val lastPart = p.getName(c-1).toString
        val inStream = Files.newInputStream(p)
        val doc = XML.load(inStream)
        lastPart -> doc
      })
      processDocuments(iterator, outputPrefix)
    }
    // processDocuments(filenames.iterator.filter(_.contains("xml")).map(x => x -> loadXML(x)), outputPrefix)

  val default_dataset: Seq[String] = Seq()

  lazy val default_input_folder = "hadjememaar"

  val max_files = Integer.MAX_VALUE

  def main(args0: Array[String]): Unit = {

    val args = if (args0.size > 0) args0 else Array(default_input_folder)

    println(args.toList)

    val filesToProcess: Seq[String] = args.toSeq.flatMap(x => {
      val f = new java.io.File(x)
      if (f.isFile) Seq(f.getCanonicalPath) else f.listFiles.toSeq.map(_.getCanonicalPath)
     })

    // println(filesToProcess)
    val maybeShuffled = if (this.training_subsets > 1) Random.shuffle(filesToProcess) else filesToProcess
    makeTrainingMaterialAndPartition(maybeShuffled.take(max_files), output_folder + "/" + output_prefix, preprocess = preprocess)
  }
}

case class PrepartitionedTrainingDataExtraction(info: TrainingDataInfo) extends TrainingDataExtraction {
  override def pickPartition(fileId: Option[String], sentenceId: Option[String]): Partition = info.pickPartition(fileId,sentenceId)
}

object gcnd_folia extends TrainingDataExtraction {
  override val split_test_train_on_document_level = true
  override lazy val output_prefix: String = "gcnd"
  override val sentence_element: String = "s"
  override lazy val output_folder = "/tmp/gcnd_test"
  override def preprocess(x: Elem): Elem = {
    val y = folia.FoliaToRudimentaryTEI(x => x).convert(x).asInstanceOf[Elem]
    y
  }

  override lazy val default_input_folder: String = "/home/jesse/workspace/XmlToRdf/data/GCND/Folia/WithAlpino"

}


