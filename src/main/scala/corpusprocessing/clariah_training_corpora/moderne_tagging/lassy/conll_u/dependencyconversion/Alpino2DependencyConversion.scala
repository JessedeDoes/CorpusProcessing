package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.dependencyconversion

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.dependencyconversion.Alpino2FlatDependencies.{garrulous, max}
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.{AlpinoSentence, ConversionRules}

import java.io.{File, PrintWriter}
import scala.xml.XML
import scala.sys.process._

object Alpino2DependencyConversion {
  def transform(location: String, output: String = "/tmp/test.conll.txt", rules: ConversionRules = ConversionToFlatLassyRules, referenceSentences: List[String] = List()) = {

    val refSet = referenceSentences.toSet

    val stime = System.currentTimeMillis()
    lazy val lines: Stream[String] = Seq("find", location, "-name", "*.xml") #| Seq("head", s"-$max") lineStream

    val conllOutput = new PrintWriter(output)

    val sentenceMap: Map[String, String] = lines.flatMap(l => {

      val sentence_id = new File(l).getName.replaceAll(".xml$", "")
      val x = XML.load(l)
      val sentence = AlpinoSentence(x, Some(sentence_id), Some(l), rules)


      if (garrulous) {
        println(s"###############  $l #####################")
        sentence.constituentsAndHeads.foreach({ case (x, y) => println(s"${x.indent} ${x.cat}/${x.rel} [${x.text}]  ----> ${y.map(x => x.word + ":" + x.betterRel + ":" + x.wordNumber).getOrElse("-")}") })
        println(s"### pure dependencies for ${sentence.sentid} ###")
        println(sentence.toCONLL())
      }

      if (sentence.dependencyParseIsValid) {
        if (referenceSentences.nonEmpty && refSet.contains(sentence.text.trim.replaceAll("\\s+", ""))) {
          println(s"Youpie ${sentence.text}")
          List((sentence.text.trim.replaceAll("\\s+", ""), sentence.toCONLL()))
        } else if (referenceSentences.isEmpty) {
          conllOutput.println(sentence.toCONLL())
          conllOutput.flush()
          conllOutput.println()
          List[(String, String)]()
        } else List[(String, String)]()
      } else {
        Console.err.println(s"Bummer, invalid dependency parse for ${sentence.sentid} !")
        List[(String, String)]()
      }
      // transferInfo(x)
    }).toMap


    if (sentenceMap.nonEmpty) {
      referenceSentences.foreach(s => {
        if (sentenceMap.contains(s)) {
          conllOutput.println(sentenceMap(s))
          conllOutput.println()
          conllOutput.flush()
        }
      })
    }

    val etime = System.currentTimeMillis()

    println((etime - stime) / 1000.0)
  }
}
