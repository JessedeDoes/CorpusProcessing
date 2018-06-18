package brievenalsbuit
import scala.xml._
import utils.PostProcessXML._
import java.io.File
import utils.ProcessFolder
import scala.io.Source._

object tagList {

  def windowed[T](n:Int, s: Stream[T]):Stream[List[T]] =
  {
    if (n==0)
      s match
      {
        case h #::t =>   (List.empty  #:: Stream.empty).asInstanceOf[Stream[List[T]]]
        case _ => Stream.empty
      }
    else s match
    {
      case h #::t =>
        val z = windowed(n-1,t);
        val zh = if (z.isEmpty) List(h) else (h :: z.head);
        zh #:: windowed(n,t);
      case _ => Stream.empty
    }
  }



  case class Word(word: String, lemma: String, tag: String)

  def wordsInFile(f: File):Stream[Word] = (XML.loadFile(f) \\ "w").map(w => Word(w.text.trim, (w \ "@lemma").text, (w \ "@pos").text.trim )).toStream

  def wordsInAllFiles(d: File) = ProcessFolder.processFolder(d, wordsInFile ).flatten

  val contextSize = 9
  val center = 4

  def allWithContext(d: File) = ProcessFolder.processFolder(d, f => windowed(contextSize, wordsInFile(f)) ).flatten
  def main(args: Array[String]): Unit = {


    val tagFrequency = wordsInAllFiles(bab.output).groupBy(_.tag).mapValues(_.size)

    val allExamples  = wordsInAllFiles(bab.output).groupBy(_.tag).mapValues(scala.util.Random.shuffle(_).toSet.take(10))
    allExamples.toList.sortBy(_._1).foreach({case (k,v) => println(s"$k (${tagFrequency(k)})\t${v.map(_.word).mkString("; ")}") })

    //allWithContext(bab.output).foreach(println)༽༽༼ ༽ ⟅ ⟆
    val allExamplesWithContext = allWithContext(bab.output).filter(_.size == contextSize).groupBy(_(center).tag).mapValues(scala.util.Random.shuffle(_).toSet.take(5))

    def marked(l: Seq[Word]) = l.zipWithIndex.map({case (word,i) => val w = word.word; val l = word.lemma; if (i == center) s"⟅$w⟿$l⟆" else w}).mkString(" ")
    allExamplesWithContext.toList.sortBy(_._1).foreach({case (k,v) => println(s"$k (${tagFrequency(k)})\n${v.map(marked).mkString("\n")}") })
  }

}
