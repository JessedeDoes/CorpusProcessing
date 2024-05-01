package corpusprocessing.clariah_training_corpora.training_data_extraction
import org.json4s.{DefaultFormats, Formats}
import org.json4s.jackson.Serialization.read
import org.json4s.jackson.Serialization.write
import org.json4s.jackson.Serialization.writePretty

import scala.collection.immutable
/*
{

}
 */

case class DocumentInfo(sourceFileName: String, sentenceIds: Option[Set[String]]  = None) {

}
case class PartitionInfo(documents: Set[DocumentInfo]) {

}

case class TrainingDataInfo(partitions: Map[String, PartitionInfo]) {

}

object TrainingDataInfo {
  implicit lazy val serializationFormats: Formats = DefaultFormats
  def write(info: TrainingDataInfo): String = writePretty(info)// org.json4s.jackson.Serialization.write(info)

  def opt(x: Set[String]) : Option[Set[String]] = if (x.isEmpty) None else Some(x)
  def convertInfo(partitionInformation: Set[(String, (String, Option[String]))] ): TrainingDataInfo = {
    val pInfos: Map[String, PartitionInfo] = partitionInformation.groupBy(_._1).mapValues(_.map(_._2)).map{ case (p,l) => {
      val l1: Set[(String, Option[String])] = l
      val byDoc: Set[DocumentInfo] = l1.groupBy(_._1).mapValues(_.map(_._2).filter(_.nonEmpty).map(_.get)).map({case (x,y) => DocumentInfo(x,opt(y))}).toSet
      p -> PartitionInfo(byDoc)
    }}
    TrainingDataInfo(pInfos)
  }

  def info2Json(partitionInformation: Set[(String, (String, Option[String]))]): String = {
    TrainingDataInfo.write(convertInfo(partitionInformation))
  }
  def read(info: String): TrainingDataInfo = org.json4s.jackson.Serialization.read[TrainingDataInfo](info)
}
