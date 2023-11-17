package corpusprocessing.corpusutils
import scala.xml._

object toTSV {
  def word(x: Node) = {
    val txt = (if ((x \ "seg").nonEmpty) (x \ "seg").text else x.text).replaceAll("\\s+", " ")
    Word(txt, (x \ "@pos").text.replaceAll("\\s+", ""), (x \ "@lemma").text.replaceAll("\\s+", " "))
  }


  case class Word(word: String, pos: String, lemma: String) {
    def compatible(w: Word) = w.word == word && w.lemma == lemma

    def posCompatible(w: Word) = w.pos.startsWith(this.pos)
  }

  def toTsv(f: String) = {
    val d = XML.load(f)
    if ((d \\ "s").nonEmpty) {
      (d \\ "s").foreach(s => {
        val tokens = s.descendant.filter(x => Set("w", "pc").contains(x.label))
        tokens.foreach(t => t match {
          case e: Elem if (e.label == "w") =>
            val w = word(e)
            println(s"${w.word}\t${w.pos}\t${w.lemma}")
          case x => println(x.text)
        })
        println("\n")
      })
    } else {
      val tokens = d.descendant.filter(x => Set("w", "pc").contains(x.label))
      tokens.foreach(t => t match {
        case e: Elem if (e.label == "w") =>
          val w = word(e)
          println(s"${w.word}\t${w.pos}\t${w.lemma}")
        case x => println(x.text.trim.replaceAll("\\s+", " "))
      })
    }
  }




  def main(args: Array[String])  =
    {
      
      args.foreach(toTsv)
    }
}
