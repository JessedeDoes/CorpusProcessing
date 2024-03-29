package utils

//import DatabaseUtilities._
import java.io.{BufferedReader, InputStreamReader}

import serpens.ColumnConfiguration

import scala.collection.JavaConverters._


/**
  * Created by jesse on 6/9/17.
  * */


object BasicTextProfiler extends SimpleTextProfiler()

class SimpleTextProfiler()
{

  case class Woordje(lemma:String, pos:String, id:String, wordform: String)

  val lexiconPath = "/dutchCorpusBasedDictionary_1.1.tf.txt"
  val lexiconPathAsFile = "src/main/resources/dutchCorpusBasedDictionary_1.1.tf.txt"

  lazy val in = new BufferedReader(new InputStreamReader(getClass.getResourceAsStream(lexiconPath)))

  def getResourceLines(): Stream[String] =
  {
    val f0 = new java.io.File(lexiconPathAsFile)
    if (f0.exists) {
      val l0 = scala.io.Source.fromFile(lexiconPathAsFile).getLines().toStream
      l0
    }
    else
      in.lines().iterator().asScala.toStream
  }

  def readLine(line: String):(String,Int) = { val c = line.split("\\s+"); c(0) -> c(1).toInt }

  private val frequencyList = getResourceLines.map(readLine).toStream
  private lazy val frequencyMap = frequencyList.toMap
  private lazy val sumOfCounts = frequencyMap.values.sum
  private lazy val lowerCasedMap = frequencyList.groupBy(_._1.toLowerCase).mapValues(l => l.map(_._2).sum).toMap

  def ocrLexiconHas() : String => Boolean =
  {
    s => lowerCasedMap.contains(s)
  }

  lazy val isKnownWord:String => Boolean = ocrLexiconHas()

  def nonWord(t:Tokenizer.Token):Boolean = t.token.matches("^[0-9\\.,;]+$")

  def profile(text: String): Map[String,Any] =
  {
     val tokens = Tokenizer.tokenize(text)

     val n = tokens.size
     val nKnown = tokens.count(x => isKnownWord(x.token.toLowerCase) && !nonWord(x))
     val nn = tokens.count(nonWord) // getallen etc doen niet mee
     val nUc = tokens.count(t => t.token.matches("^[A-Z].*"))
     val p = nKnown / (n - nn ).asInstanceOf[Double]
     val lang:String = "nl" // LanguageIdentification.detectLanguage(text) // TODO fix this

     Map("numberOfTokens" -> n,
       "numberInLexicon" -> nKnown,
       "lexiconCoverage" -> p,
       "numberNumerical" -> nn,
       "numberUppercase" -> nUc,
       "lang" -> lang)
  }

  def processTSV(fileName: String, c: ColumnConfiguration):Unit =
  {
    val lines = scala.io.Source.fromFile(fileName).getLines.toStream.map(l => l.split("\\t", -1).toList).map(
      r => { val text = r(c.textColumn); val prof = profile(text); r ++ List(prof("lang"), prof("lexiconCoverage")) }
    ).map(_.mkString("\t"))
    lines.foreach(println)
  }

  def processTSV(fileName: String, textColumn: Int):Unit = 
  {
     val c = ColumnConfiguration(null, textColumn=textColumn, -1)
     processTSV(fileName,c)
  }
}

object SimpleTextProfiler
{
  def main(args:Array[String]):Unit =
  {
    val sts  = new SimpleTextProfiler()
    sts.processTSV(args(0), args(1).toInt)
  }
}
