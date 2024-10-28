package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

import java.io.{File, PrintWriter}
import scala.xml.{Elem, XML}

object LassyXML2FlatDependencies {

  import sys.process._

  val max = Integer.MAX_VALUE // 100000
  val lassyAllAtHome = "/media/jesse/Data/Corpora/LassySmall/Treebank/"
  val lassyAtWork = "/mnt/Projecten/Corpora/TrainingDataForTools/LassyKlein/LassySmall/Treebank/"
  val lassy = List(lassyAllAtHome, lassyAtWork).find(x => new File(x).exists())


  // complex voorbeeld coindexeringen: https://paqu.let.rug.nl:8068/tree?db=lassysmall&names=true&mwu=false&arch=/net/corpora/paqu/lassyklein.dact&file=WR-P-P-I-0000000126.p.12.s.4.xml&global=true&marknodes=&ud1=&ud2=
  def transferInfo(x: Elem) = {
    val leafs = (x \\ "node").filter(x => x.child.isEmpty)
    val groupedByBegin = leafs.groupBy(x => (x \ "@begin").text -> (x \ "@end").text)
    groupedByBegin.foreach({ case (b, n) => if (n.size > 1) {
      println("Colocated nodes!")
      n.foreach(println)
      // System.exit(1)
    }
    })
  }
  /*
  <node begin="49" end="50" genus="masc" getal="ev" id="88" index="3" lemma="hem" naamval="obl" pdtype="pron" persoon="3" pos="pron" postag="VNW(pers,pron,obl,vol,3,ev,masc)" pt="vnw" rel="obj2" root="hem" status="vol" vwtype="pers" word="hem"/>
  <node begin="49" end="50" id="93" index="3" rel="su"/>
   */


  val garrulous = false

  def transform(location: String, output: String = "/tmp/test.conll.txt", rules: ConversionRules = ConversionToFlatLassyRules) = {

    val stime = System.currentTimeMillis()
    lazy val lines: Stream[String] = Seq("find", location, "-name", "*.xml") #| Seq("head", s"-$max") lineStream

    val conllOutput = new PrintWriter(output)

    lines.foreach(l => {

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
        conllOutput.println(sentence.toCONLL())
        conllOutput.flush()
      } else {
        Console.err.println(s"Bummer, invalid dependency parse for ${sentence.sentid} !")
      }
      // transferInfo(x)
    })

    val etime = System.currentTimeMillis()

    println((etime - stime) / 1000.0)
  }
  def main(args: Array[String]) = {
    val location = args.headOption.getOrElse(lassy.getOrElse("nope"))
    transform(location)
  }
}

