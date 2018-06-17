package eindhoven

import scala.xml._
import Eindhoven._
import java.io.File
import java.nio.file._
import posmapping.ProcessFolder

object klussen {
  case class Kwic(f: File, w: Node, s: Node)
  {

    import java.nio.file.Paths

    val base  = Paths.get(Eindhoven.outputDir).toAbsolutePath

    val relative = base.relativize(Paths.get(f.getCanonicalPath))
    val theWord = word(w.asInstanceOf[Elem])

    def kwic = (s.descendant.filter(x => x.label == "w" || x.label == "pc")).map(w1 => if (w1 == w) <b>{w1.text.replaceAll("\\s+", " ").trim}</b>.toString else {w1.text.replaceAll("\\s+", " ").trim}).mkString(" ")

    def toString(g: Word => String) = s"$relative\t${getId(w).getOrElse("NOID")}\t${g(theWord)}\t${theWord.pos}\t$kwic"
  }


  def extract(file: File, f: Word => Boolean, g: Word => String) = {
    //Console.err.println(file)
    val d = XML.loadFile(file)

    val sentences = (d \\ "s").filter(s => (s \\ "w").exists(w => f(word(w))))
    val kwics = sentences.flatMap(s =>
    {
      val wordz = (s \\ "w").filter(w => f(word(w)))
      wordz.map(Kwic(file,_,s))
    })
    kwics.foreach(x => println(x.toString(g)))
    kwics
  }

  def word(w: Node): Word = Word(w.text, (w \ "@lemma").text, (w \ "@pos").text)

  def maakKlus(dir: File, f: Word => Boolean, g: Word => String) = {
    ProcessFolder.processFolder(dir, file => extract(file,f,g))
  }
}

object zezijklus {
  def main (args: Array[String] ): Unit = {
    klussen.maakKlus(new File(outputDir),
      w => Set("ze","zij").contains(w.word.toLowerCase) && w.pos.matches("VNW.*pers.*nomin.*"),
      w => if (w.pos.contains("fem")) "true" else "false")
  }
}

object adjectiefklus {
  def main (args: Array[String] ): Unit = {
    klussen.maakKlus(new File(outputDir),
      w => w.pos.matches("ADJ.*(x-|\\|).*"),
      w => if (w.pos.contains("vrij")) "true" else "false")
  }
}
