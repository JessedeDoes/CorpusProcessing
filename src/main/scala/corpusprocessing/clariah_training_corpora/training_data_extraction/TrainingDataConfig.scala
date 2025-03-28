package corpusprocessing.clariah_training_corpora.training_data_extraction
import org.json4s.{DefaultFormats, Formats}
import org.json4s.jackson.Serialization.read
import org.json4s.jackson.Serialization.write
import org.json4s.jackson.Serialization.writePretty

import java.io.PrintWriter
import scala.collection.immutable
/*
{

}
 */

case class DocumentInfo(sourceFileName: String, sentenceIds: Option[Set[String]]  = None, legacyName: Option[String]= None) {

}
case class PartitionInfo(documents: Set[DocumentInfo]) {

}

case class TrainingDataInfo(partitions: Map[String, PartitionInfo], sentenceElement: String = "s") {

  lazy val isSentenceLevel = partitions.values.exists(p => p.documents.exists(d => d.sentenceIds.nonEmpty))
  lazy val file2partitionName: Map[String, String] = partitions.flatMap({ case (p,i) =>
    i.documents.map(_.sourceFileName -> p)
  })

  lazy val filePlusDoc2partitionName: Map[(String, String), String] = partitions.flatMap({ case (p, i) =>
    i.documents.flatMap(d => {
      val fid = d.sourceFileName
      d.sentenceIds.getOrElse(Set()).map(sid => (fid,sid) -> p)
    })
  })

  def pickPartition(fileId: Option[String]=None, sentenceId: Option[String]=None): Partition = {
    val docPartition = Partition(file2partitionName(fileId.get))
    val partition = {
     if (sentenceId==None)
       docPartition
     else if (filePlusDoc2partitionName.contains(fileId.get -> sentenceId.get))
       Partition(filePlusDoc2partitionName(fileId.get -> sentenceId.get))
     else
       docPartition
    }
    // Console.err.println(s"Choosing partition for $fileId, $sentenceId: $partition")
    partition
  }
}

case class TrainingDataInfos(downloadedDataDir: String, extractedDataDir: String, trainingDataInfos: Map[String, TrainingDataInfo])

object TrainingDataInfos {
  implicit lazy val serializationFormats: Formats = DefaultFormats

  def write(info: TrainingDataInfos): String = writePretty(info)

  def writeToFile(info: TrainingDataInfos, fileName: String)  = {
    val p = new PrintWriter(fileName)
    p.write(write(info))
    p.close()
  }//
  def read(info: String): TrainingDataInfos = org.json4s.jackson.Serialization.read[TrainingDataInfos](info)

  def readFromFile(path: String)  = {
    val json = io.Source.fromFile(path).getLines().mkString("\n")
    read(json)
  }
}

object TrainingDataInfo {
  implicit lazy val serializationFormats: Formats = DefaultFormats
  def write(info: TrainingDataInfo): String = writePretty(info)// org.json4s.jackson.Serialization.write(info)

  def opt(x: Set[String]) : Option[Set[String]] = if (x.isEmpty) None else Some(x)
  def convertInfo(partitionInformation: Set[(String, (String, Option[String]))], sentenceElement: String="s" ): TrainingDataInfo = {
    val pInfos: Map[String, PartitionInfo] = partitionInformation.groupBy(_._1).mapValues(_.map(_._2)).map{ case (p,l) => {
      val l1: Set[(String, Option[String])] = l
      val byDoc: Set[DocumentInfo] = l1.groupBy(_._1).mapValues(_.map(_._2).filter(_.nonEmpty).map(_.get)).map({case (x,y) => DocumentInfo(x,opt(y))}).toSet
      p -> PartitionInfo(byDoc)
    }}
    TrainingDataInfo(pInfos, sentenceElement)
  }

  def info2Object(partitionInformation: Set[(String, (String, Option[String]))], sentenceElement: String="s"): TrainingDataInfo = {
    convertInfo(partitionInformation, sentenceElement)
  }

  def info2Json(partitionInformation: Set[(String, (String, Option[String]))], sentenceElement: String="s"): String = {
    TrainingDataInfo.write(convertInfo(partitionInformation, sentenceElement))
  }
  def read(info: String): TrainingDataInfo = org.json4s.jackson.Serialization.read[TrainingDataInfo](info)

  def compareConfigs(c1: TrainingDataInfo, c2: TrainingDataInfo) = {
    val k1 = c1.partitions.keySet
    val k2 = c2.partitions.keySet
    if (c1.sentenceElement != c2.sentenceElement) {
      println(s"Sentence elements: ${c1.sentenceElement} != ${c2.sentenceElement}" )
    }
    if (k1 != k2) {
       println(s"different partition keySets, ${k1} ${k2} ")
       false
     } else {
      val c2SubsetOfc1 =  k1.map(k => {
        val d1 = c1.partitions(k).documents
        val d2 = c2.partitions(k).documents
        val d = d1 diff d2

        println(s"Boom for $k!: ${d.size}, old:${d1.size}, new:${d2.size}:  ${d}")
        d2 subsetOf d1
      }).reduce( (b1,b2) => b1 && b2)
      c2SubsetOfc1
    }
  }
}

