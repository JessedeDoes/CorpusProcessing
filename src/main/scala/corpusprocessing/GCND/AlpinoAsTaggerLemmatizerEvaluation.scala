package corpusprocessing.GCND

import java.io.{File,PrintWriter}
import collection.JavaConverters._
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy._
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.{AlpinoSentence, UdToken}

import scala.util.Random
import scala.xml._
object AlpinoAsTaggerLemmatizerEvaluation {
  val atHome = true

  val toGood = new PrintWriter("/tmp/toGood.txt")
  val baseDir = if (atHome) "/home/jesse/Downloads/alpino_treebank/" else "/mnt/Projecten/Hercules/Corpus-ZNL-Dialecten/GCNDAlpinoParsesWithCorrection/alpino_treebank/"
  val original = baseDir + "original"
  val processed = baseDir + "processed"

  lazy val originalFiles = findXMLFilesIn(new File(original))
  lazy val processedFiles = findXMLFilesIn(new File(processed))

  lazy val orgMap = originalFiles.map(f => f.getName -> f).toMap
  lazy val procMap = processedFiles.map(f => f.getName -> f).toMap
  lazy val hasBoth = orgMap.keySet intersect procMap.keySet
  def findXMLFilesIn(f: File): List[File] = {
    if (f.isFile && f.getName.endsWith(".xml")) List(f) else {
      if (f.isDirectory)
      f.listFiles.flatMap(findXMLFilesIn).toList
      else List()
    }
  }


  val max = Integer.MAX_VALUE

  def conspicuouslyGood(m: Map[String, (Int, Int)]): Boolean =  !m.keySet.isEmpty &&
    m("CGN tag")._1 == 0 &&
    m("Lemma")._1 == 0 &&
    m("Dependency relation label (Alpino relation mapped to flat dependencies)")._1 == 0  &&
    m("LAS (labeled attachment score)")._1 == 0 &&
    m("Lemma")._2 >= 12

  def evalSentence(org: AlpinoSentence, processed: AlpinoSentence, fields: Map[String,UdToken => String]): Map[String, (Int, Int)] = {
     val tokenPairs = org.connlTokens.zip(processed.connlTokens)
     val n = tokenPairs.size

     val m = fields.map({case (name, field) =>
       val errors = tokenPairs.count({case (t1,t2) => field(t1) != field(t2)})
       name -> (errors,n)
     })

    if (conspicuouslyGood(m)) {
      toGood.println(processed.toCONLL())
      toGood.flush()
    }
    m
  }

  val fields : List[(String,UdToken => String)] = List(
    "Lassy main pos tag" -> (_.UPOS),
    "CGN tag" -> (_.XPOS),
    "Lemma" -> (_.LEMMA),
    "Dependency relation label (Alpino relation mapped to flat dependencies)" -> (_.DEPREL),
    "UAS (unlabeled attachment score)" -> (x => x.HEAD),
    "LAS (labeled attachment score)" -> (x => x.DEPREL + ":" + x.HEAD)
  )

  def pad(x: String, n: Int) = {
    x + " " * (n - x.size)
  }

  val sampleSize = Integer.MAX_VALUE
  def main(args: Array[String]) = {
     val sample: Seq[String] = Random.shuffle(hasBoth.toList).take(sampleSize)

     val scores: Seq[Map[String, (Int, Int)]] = sample.map(x => {
        // println(s"#### $x #####")
        try {
         val org = AlpinoSentence(XML.loadFile(orgMap(x)))
         val processed =  AlpinoSentence(XML.loadFile(procMap(x)))
          evalSentence(org,processed, fields.toMap)
        }
        catch {
          case e: Exception =>
            e.printStackTrace()
            Map[String, (Int, Int)]()
         }
       })

      val failures = scores.filter(_.keySet.isEmpty)
      val toGoodToBeTrue = scores.filter(conspicuouslyGood)

      val maxFieldLen = fields.map(_._1).map(_.length).max
      val nTokens = scores.filter(_.nonEmpty).map(m => m(fields.head._1)._2).sum
      val nSentences = scores.size

      println(s"Sentences: $nSentences\nTokens: $nTokens\nFailed to evaluate:  ${failures.size}\nTo good to be true (No lem, pos or deprel error, sentence length at least 12): ${toGoodToBeTrue.size}\n")

      fields.map(_._1).foreach(k => {
         val nTokens = scores.filter(m => m.contains(k)).map(m => m(k)._2).sum
         val nErrors = scores.filter(m => m.contains(k)).map(m => m(k)._1).sum
         val score = 1 - nErrors / nTokens.toDouble
         println(s"${pad(k,maxFieldLen)}\t$nErrors\t$score")
      })
    }
}
