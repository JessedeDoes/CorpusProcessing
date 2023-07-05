package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.CONLL.parse
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.grouping
import org.json4s._
import org.json4s.jackson.Serialization.write

import java.io.PrintWriter
import scala.xml._
import sys.process._
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.grouping.groupWithFirst

import scala.collection.immutable
object CONLL {
  def parseFile(f: java.io.File): Seq[UdSentence] = {
    val lines = io.Source.fromFile(f).getLines.toStream
    val language = f.getName.replaceAll("_.*", "")
    parse(lines, language)
  }

  def convertAndParseFiles(dir: String, language: String="Dutch")  = {
    val lines: Stream[String] = Seq("find", dir, "-name", "*.xml") #| Seq("/home/jesse/go/bin/alud")  lineStream;
    parse(lines, language)
  }

  def convertAndParseFilesAndKeepAlpinoLink(dir: String, language: String = "Dutch", max: Int = 100) = {

    val lines: Stream[String] = Seq("find", dir, "-name", "*.xml") #| Seq("head", s"-$max") #| Seq("/home/jesse/go/bin/alud") lineStream;
    val fileNames = Seq("find", dir, "-name", "*.xml")  lineStream;
    val alpinos = fileNames.take(max).map(f => {
      val id = new java.io.File(f).getName.replaceAll(".xml$","")
      id -> XML.load(f)
    }).toMap
    val sentences = parse(lines, language).map(s => s.sent_id -> s).toMap
    sentences.filter({case (id, udSentence) => alpinos.contains(id)}).map({case (id, udSentence) => {
      val found = alpinos.contains(udSentence.sent_id)
      // println(udSentence.sent_id + " " + found)
      val alpinoSentence = AlpinoSentence(alpinos(id))
      AlpinoWithUD(alpinoSentence, udSentence)
    }})
  }
  def parse(lines: Stream[String], language:String="Dutch"): Seq[UdSentence] = {

    val grouped = groupWithFirst[String](lines, x => x.startsWith("# sent_id =") || x.startsWith("# S-ID")) // S-ID in Japanese KTC

    val sentences = grouped.flatMap(g => {
      val sent_id = g.find(x => x.startsWith("# sent_id") || x.startsWith("# S-ID")).map(_.replaceAll(".*[=:]", "").replaceAll("\\s+", ""))

      sent_id.map(x => {
        val text = g.find(_.startsWith("# text")).map(_.replaceAll(".*=", "").trim)
        val tokens = g.filter(_.matches("[0-9].*")).map(_.split("\\t").toList).map(l => UdToken(l(0), l(1), l(2), l(3), l(4), l(5), l(6), l(7), l(8), l(9), x, language))
        val sentence = UdSentence(x, language, tokens)
        sentence
      })
    })
    sentences
  }

  val testData = "/home/jesse/workspace/alud/data"
  val lassyAll = "/mnt/Projecten/Corpora/TrainingDataForTools/LassyKlein/LassySmall//Treebank/"
  def main(args: Array[String]): Unit  = {
    val pw = new PrintWriter("/tmp/patched.txt")
    convertAndParseFilesAndKeepAlpinoLink(lassyAll,max = 3000).foreach(x => {
      val patched = x.enrichUD;
      if (patched.tokens.exists(t => Set("obl:me", "obl:arg").contains(t.DEPREL))) {
        println(patched.conll)
        pw.println("\n" + patched.conll)
        pw.flush()
      }
    })
    print("YouHoo ready")
    pw.close()
    System.exit(0)
  }
}
