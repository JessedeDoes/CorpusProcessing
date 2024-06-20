package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.CONLL.{parse, parseFile, toTEI}
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.grouping
import org.json4s._
import org.json4s.jackson.Serialization.write

import java.io.{File, PrintWriter}
import scala.xml._
import sys.process._
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.grouping.groupWithFirst

import scala.collection.immutable
import scala.util.Random
object CONLL {
  def parseFile(f: java.io.File): Seq[UdSentence] = {
    val lines = io.Source.fromFile(f).getLines.toStream
    val language = f.getName.replaceAll("_.*", "")
    parse(lines, language).filter(_.isValid()).map(s => s.copy(sent_id = f.getName + "." + s.sent_id))
  }

  def toTEI(f: java.io.File, setName: String = "Unknown", maxSentLength:Int = Integer.MAX_VALUE): Elem = {
    val sentences = CONLL.parseFile(f)
      .filter(_.tokens.size <= maxSentLength)
      .zipWithIndex.map({case (s,i) => s.copy(sent_id=s"sentence_$i")})
      .map(_.asTEI())

    val nTokens = sentences.map(x => (x \\ "w").size).sum
    val langCode  = f.getName.replaceAll("_.*", "")
    val langName = setName.replaceAll("-.*", "").replaceAll("_", " ")

    val id = f.getName
    <TEI xml:id={id} xmlns="http://www.tei-c.org/ns/1.0">
        <teiHeader>
          <fileDesc>
            <titleStmt>
              <title type="main">{f.getName}</title>
              <title type="sub">{setName}</title>
              <respStmt>
                <resp>Conversion to TEI</resp>
                <name>Instituut voor de Nederlandse Taal</name>
              </respStmt>
            </titleStmt>
            <extent>
              <!--These numbers do not reflect the size of the sample!-->
              <measure quantity={sentences.size.toString} unit="sentences" xml:lang={langCode}/>
              <measure quantity={nTokens.toString} unit="words" xml:lang="en"/>
            </extent>

            <publicationStmt>
              <publisher>Instituut voor de Nederlandse Taal</publisher>
              <pubPlace>Leiden</pubPlace>
              <date>2023</date>
              <availability>
                <p>Via online corpus application: https://corpora.ato2.ivdnt.org</p>
              </availability>
            </publicationStmt>
          </fileDesc>
          <sourceDesc>
           <listBibl type="intMetadata">
            <bibl type="intMetadata">

            </bibl>
          </listBibl>
          </sourceDesc>
          <encodingDesc>
            <langUsage>
              <language ident={langCode} usage={nTokens.toString}>{langName}</language>
            </langUsage>
          </encodingDesc>
        </teiHeader>

        <text>

        <body><div><p>{sentences}</p></div></body>
      </text>
    </TEI>
  }

  def parseFile(f: String): Seq[UdSentence]  = {
    parseFile(new File(f))
  }

  def splitMaterial(f: java.io.File, dev_p: Double = 0.1, test_p: Double=0.1) = {

    val dev = new File(f.getCanonicalPath.replaceAll("\\.([^.]*$)", ".dev.$1"))
    val test = new File(f.getCanonicalPath.replaceAll("\\.([^.]*$)", ".test.$1"))
    val train = new File(f.getCanonicalPath.replaceAll("\\.([^.]*$)", ".train.$1"))

    println(s"$dev $train $test")

    val sentences = Random.shuffle(parseFile(f))
    val devN = Math.round(sentences.size * dev_p).toInt
    val testN = Math.round(sentences.size * test_p).toInt


    val devSet = sentences.take(devN)
    val testSet = sentences.drop(devN).take(testN)
    val trainingSet = sentences.drop(devN + testN)

    println(s"$dev,$devN,${devSet.size}\n$train,${trainingSet.size}\n$test,$testN,${testSet.size}")
    def save(f: File, s:Seq[UdSentence]) = {
      println(s"saving to $f")
      val p = new PrintWriter(f)
      s.foreach(u => p.println(u.toCONLL(rebase=false)))
      p.close()
    }

    save(dev,devSet)
    save(test,testSet)
    save(train,trainingSet)
  }

  def convertAndParseFiles(dir: String, language: String="Dutch")  = {
    val lines: Stream[String] = Seq("find", dir, "-name", "*.xml") #| Seq("/home/jesse/go/bin/alud", "-e")  lineStream;
    parse(lines, language)
  }

  def convertAndParseFilesAndKeepAlpinoLink(dir: String, language: String = "Dutch", max: Int = 100) = {

    val lines: Stream[String] = Seq("find", dir, "-name", "*.xml") #| Seq("head", s"-$max") #| Seq("/home/jesse/go/bin/alud", "-e") lineStream;
    val fileNames = Seq("find", dir, "-name", "*.xml")  lineStream;
    val alpinos = fileNames.take(max).zipWithIndex.map({ case (f,i) => {
      if (i % 500 == 0) Console.err.println(s"..$i..")
      val id = new java.io.File(f).getName.replaceAll(".xml$","")
      id -> XML.load(f)
    }}).toMap
    val sentences: Map[String, UdSentence] = parse(lines, language).map(s => s.sent_id -> s).toMap
    sentences.filter({case (id, udSentence) => alpinos.contains(id)}).map({case (id, udSentence) => {
      val found = alpinos.contains(udSentence.sent_id)
      // println(udSentence.sent_id + " " + found)
      val alpinoSentence = AlpinoSentence(alpinos(id))
      AlpinoWithUD(alpinoSentence, udSentence)
    }})
  }

  def startsLine(x: String)  = x.startsWith("# sent_id =") || x.startsWith("# S-ID")
  def startsLineSource(x: String)  = x.startsWith("# source =")
  def isMeta(x: String)  = x.startsWith("# ") && x.count(_ == "\t") < 5
  def parse(lines: Stream[String], language:String="Dutch"): Seq[UdSentence] = {
    // val l1 = lines.zipWithIndex

    // lazy val grouped: Seq[Seq[String]] = groupWithFirst[(String,Int)](l1, {case (x,i) => isMeta(x) && (i==0 || !isMeta(lines(i-1)))}).map(l => l.map(_._1))

    val grouped: Seq[Seq[String]] = groupWithFirst[String](lines, x => isMeta(x), previousMustNotBeIt=true) // S-ID in Japanese KTC

    val sentences = grouped.flatMap(g => {
      val sent_id: Option[String] = g.find(x => x.startsWith("# sent_id") || x.startsWith("# S-ID")).map(_.replaceAll(".*[=:]", "").replaceAll("\\s+", ""))
      lazy val headers = g.filter(isMeta)
      // println(headers)
      sent_id.map(x => {
        val text = g.find(_.startsWith("# text")).map(_.replaceAll(".*=", "").trim)
        val tokens = g.filter(_.matches("^[0-9].*")).map(_.split("\\t",-1).toList)
          .filter(l => { if (l.size < 10) Console.err.println(s"Nope: $sent_id: $l");  l.size >= 10})
          .map(l => UdToken(l(0), l(1), l(2), l(3), l(4), l(5), l(6), l(7), l(8), l(9), x, language))

        val sentence = UdSentence(x, language, tokens, lines=g)
        sentence
      })
    })
    sentences
  }

  def main(args: Array[String])  = {
    val in = args(0)
    val sentences = parseFile(in)
    val out = new PrintWriter(in.replaceAll("conllu", "fixed.conllu"))
    sentences.filter(_.isValid()).foreach(s => {
      // println("")
      out.print(s.toCONLL(rebase = false))
    })
    out.println()
    out.flush()
    //p.foreach(println)
  }

}

object CONLL2TEI {

  val corpusDir = "/mnt/Projecten/Corpora/UDCorpora/All/ud-treebanks-v2.14/"

  val xmlDir = "/media/jesse/Data/Corpora/UD/ShortSentences/"
  lazy val pretty = new scala.xml.PrettyPrinter(300, 4)
  val maxFiles = Int.MaxValue
  val maxSentLength = 12

  var tokens = 0
  def findAllConllIn(path: String): Set[File] = {
    val f = new File(path)
    if (f.isFile && f.getName.endsWith(".conllu")) {
      Set(f)
    } else if (f.isFile()) {
      Set()
    } else {
      val sub = f.listFiles().map(_.getCanonicalPath).toSet

      sub.flatMap(x => { findAllConllIn(x)} )
    }
  }

  def main(args: Array[String]) = {
    val in = args.headOption.getOrElse(corpusDir)
    val f = new File(in)
    if (f.isFile) {
      val tei = toTEI(f)
      val pw = new PrintWriter(s"/tmp/${f.getName.replaceAll("[.][^.]*$", "")}.xml")
      pw.println(tei)
      pw.close()
    } else {
      val files = findAllConllIn(in)
      files.take(maxFiles).foreach(f => {

        val setName = f.getParentFile.getName.replaceAll("UD_", "")
        val tei = toTEI(f, setName,maxSentLength=maxSentLength)
        val nTokens = (tei \\ "w").size
        tokens = tokens + nTokens

        val underscores = (tei \\ "w").filter(_.text == "_").size

        val hasContent =  4 * underscores < nTokens
        if (hasContent) {
          val pw = new PrintWriter(s"$xmlDir/${f.getName.replaceAll("[.][^.]*$", "")}.xml")
          pw.println(pretty.format(tei))
          pw.close()
        } else {
          Console.err.println(s"Corpus $f has too many void tokens: $underscores of $nTokens")
        }

        println(s"Converted:  ${f.getName}, $nTokens tokens, total $tokens now")
      })
    }

    //p.foreach(println)
  }
}

object Split {
  def main(args: Array[String]) = {
     CONLL.splitMaterial(new File(args(0)))
  }
}
