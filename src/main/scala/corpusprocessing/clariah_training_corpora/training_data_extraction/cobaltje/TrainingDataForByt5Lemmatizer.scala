package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje

import java.io.{File, FileInputStream, PrintWriter}
import java.util.zip.GZIPInputStream


case class Token(word: String, pos: String, lemma: String)

import Settings._
object TrainingDataForByt5Lemmatizer {

  val proportion = 80



  lazy val lemAll = lemDataDir + "lem.all.tsv"
  lazy val lemTrain = lemDataDir + "lem.train.tsv"
  lazy val lemTest = lemDataDir + "lem.test.tsv"

  def parseToken(s: String): Option[Token] = {
    val f = s.split("\\t")
    if (f.size == 3) Some(Token(f(0), f(1), f(2))) else None
  }


  def findAllFilesIn(f: File): List[File] = {
    val l = f.listFiles()
    val l1 = if (l == null) List[File]() else l.toList
    List(f) ++
      l1.flatMap(x => findAllFilesIn(x))
  }

  lazy val trainingFiles = findAllFilesIn(new File(posTaggingDataDir)).filter(_.getName.endsWith("train.tsv.gz"))

  lazy val trainingLines = trainingFiles.iterator.flatMap(f => {
    val x = io.Source.fromInputStream(new GZIPInputStream(new FileInputStream(f))).getLines()
    x
  })

  lazy val trainingTokens = trainingLines.map(parseToken).filter(_.nonEmpty).map(_.get)

  lazy val groups: Map[Token, Int] = trainingTokens.toStream.groupBy(identity).mapValues(_.size)

  def printGroups(groups: Map[Token,Int], f: String) = {
    val out = new PrintWriter(f)
    out.println("word\tpos\tlemma\tfreq")
    groups.foreach(g => {
      val t = g._1
      val props = List(t.word, t.pos, t.lemma, g._2)
      out.println(props.mkString("\t"))
    })
    out.close()
  }
  def main(args: Array[String]) = {
    // trainingFiles.foreach(println)

    println(groups.size)
    printGroups(groups, lemAll)
    val train = groups.take(proportion * groups.size / 100)
    val test = groups.drop(proportion * groups.size / 100)
    printGroups(train,lemTrain)
    printGroups(test, lemTest)
  }
}
