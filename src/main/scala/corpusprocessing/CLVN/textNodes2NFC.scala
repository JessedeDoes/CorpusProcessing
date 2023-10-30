package corpusprocessing.CLVN

import java.text.Normalizer._
import java.text.Normalizer
import java.io.File
import scala.xml._

object textNodes2NFC {
  def norm(s: String) = normalize(s, Normalizer.Form.NFC)

  def recodeXML(e: Elem): Elem = {
    e.copy(child = e.child.map(
      {
        case e1: Elem => recodeXML(e1)
        case t: Text => {
          val s1 = t.toString
          val s2 = norm(s1)
          // if (!s1.equals(s2)) Console.err.println(s"$s1 --> $s2")
          Text(s2)
        }
        case n: Node => n
      })
    )
  }

  def recode(f1: String, f2: String) {
    Console.err.println(f1)
    val fw = new java.io.FileWriter(f2)

    // scala.io.Source.fromFile(f,"utf8").getLines.foreach(l => fw.write(norm(l) + "\n"))
    val recoded = recodeXML(XML.load(f1))
    fw.write(recoded.toString)
    fw.close()

  }


  def main(args: Array[String]) = {
    //  println(args(0))
    utils.ProcessFolder.processFolder(new File(args(0)), new File(args(1)), recode)
    //   scala.io.Source.fromFile(args(0)).getLines.foreach( s => norm(s).foreach(c => { println(s"$c ${Integer.toString(c.toInt,16)}") }))
  }
}

/*

Form	Description
Normalization Form D (NFD)	Canonical Decomposition
Normalization Form C (NFC)	Canonical Decomposition, followed by Canonical Composition
Normalization Form KD (NFKD)	Compatibility Decomposition
Normalization Form KC (NFKC)	Compatibility Decomposition,
followed by Canonical Composition
*/
