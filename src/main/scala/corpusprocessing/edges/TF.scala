package corpusprocessing.edges
import java.io.PrintWriter

import utils.ProcessFolder

import scala.xml._
import java.io.File

object TF {


  val dir = "/mnt/Projecten/Corpora/Historische_Corpora/EDGeS_historical_bible_corpus//WithMetadata/"

  val files = ProcessFolder.filesIn(new java.io.File(dir))

  def bibleName(f: File) = f.getParentFile.getName.replaceAll("se_bijbel","")
    .replaceAll("_xml_dbnl","")

  val bibles = files.map(bibleName)

  Console.err.println(bibles.sorted)

  case class Wordform(form: String, lemma: String, pos: String, file: String) {
    def lem = Lemma(lemma, pos)
  }

  case class Lemma(form: String, pos: String) {
    override def toString = s"$form\t$pos"
  }

  case class LemmaInfo(
                      lemma: Lemma,
                      wordforms: Seq[(Wordform,  Int)]
                      ) {
    def count = wordforms.map(_._2).sum
    def counts = wordforms.groupBy(_._1.file).mapValues(_.map(_._2).sum)

    def countInfo = bibles.sorted.map(x =>
      x -> counts.getOrElse(x, 0)).map({ case (x,y) => s"$y" }).mkString("\t")
    def wfInfo = wordforms.groupBy(_._1.form).map(
      { case (f, s) => WordformInfo(f,s, this)})

    lazy val  wfString = wfInfo.map(_.toString).mkString("; ")
    override def toString = s"$lemma\t$count\t$countInfo\t$wfString"

  }

  case class WordformInfo(
                        form: String,
                        wordforms: Seq[(Wordform,  Int)],
                        lemma: LemmaInfo
                      ) {
    def count = wordforms.map(_._2).sum
    def counts = wordforms.groupBy(_._1.file).mapValues(_.map(_._2).sum)

    override def toString = s"[$form $count]"
  }

  def tfOneFile(f: java.io.File) =
    {
      val d = XML.loadFile(f)
      val tokens = (d \\ "w").map(w => {
        val pos = (w \\ "@type").text
        val wf = w.text.replaceAll("\\s+", "").trim
        val l = (w \\ "@lemma").text
        Wordform(wf, if (l.isEmpty) wf else l,pos, bibleName(f)
      )})
      tokens.groupBy(identity).mapValues(_.size).toList
    }

  lazy val stuff: List[LemmaInfo] =
    files.par.flatMap(tfOneFile).toList.groupBy(_._1.lem).toList.map({case (l,x) => LemmaInfo(l,x)})
      .sortBy(_.lemma.form)

  def main(args: Array[String]): Unit = {
    val p = new PrintWriter("/tmp/edges.tf.txt")
    stuff.filter(x => x.count > 9 && x.lemma.form.length > 3  && x.lemma.form.matches("^[a-z]+")).foreach(s => p.println(s))
    p.close()
  }
}
