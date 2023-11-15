package corpusprocessing.clariah_training_corpora.training_data_extraction.gysseling

import java.io.PrintWriter
import scala.xml._
object GysselingLexTrainingData {
  val max = Int.MaxValue
  case class Word(word: String, pos: String, lemma: String) {
    def compatible(w: Word) = w.word == word && w.lemma == lemma

    def posCompatible(w: Word) = w.pos.startsWith(this.pos)
  }
  def word(x: Node) = {
    val txt = (if ((x \ "seg").nonEmpty) (x \ "seg").text else x.text).replaceAll("\\s+", " ")
    Word(txt, (x \ "@pos").text.replaceAll("\\s+", ""), (x \ "@lemma").text.replaceAll("\\s+", " "))
  }

  val partition = "dev"
  val fileList = s"/mnt/Scratch/jesse/gys.$partition.filenames"

  lazy val words: Iterator[Node] = io.Source.fromFile(fileList).getLines().take(max).toStream.iterator.flatMap(l => {
    val doc= XML.load(l)
    (doc \\ "w")
  })

  lazy val filtered: Iterator[Node] = words.filter(w => (w \ "@corresp").isEmpty && ! (w \ "seg").text.contains(" "))

  def collect() = {
    val out = new PrintWriter("/tmp/tf.txt")
    filtered.map(word).toStream.groupBy(identity).mapValues(_.size).toList.sortBy(-1 * _._2).foreach({ case (w,n) =>
      out.println(s"${w.word}\t${w.pos}\t${w.lemma}\t$n")
    })
    out.close()
  }

  def main(args: Array[String])  = {
    collect()
  }
}
