package corpusprocessing.clariah_training_corpora.enhanceBaB

import java.io.File
import scala.xml._
import utils.PostProcessXML
object preprocess {
  val babPath = "/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuit/2.8TDN/"
  val outputPath = "/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuit/SelectionForEnhancementWaddeBlijfEffeAf/"

  lazy val files = new File(babPath).listFiles.toList

  val maxTokens = 60000
  var t = 0
  lazy val selection = util.Random.shuffle(files).iterator.takeWhile(f => {
    val d = XML.loadFile(f)
    val nWords = (d \\ "w").size
    t = t + nWords
    t < maxTokens
  })

  val chunk_size = 50

  def chunkIn(e: Elem) = {
    val tokenGroups = e.child.filter(x => Set("w", "pc").contains(x.label)).grouped(chunk_size)
    e.copy(child = tokenGroups.map(g => <s>{g}</s>).toSeq)
  }

  def fakeSentences(d: Elem)  = {
    val f = PostProcessXML.updateElement(d, x => x.child.exists(y => Set("w", "pc").contains(y.label)), chunkIn)
    val s0 = (d \\ "w").size + (d \\ "pc").size
    val s1 = (f \\ "w").size + (f \\ "pc").size
    println(s"$s0 $s1")
    f
  }

  def tweakW(w: Elem)  = {
    val w0 = w.copy(child = w.child.filter(x => x.label != "fs"))
    val pos = (w \ "@pos").text
    val neLabeling = pos.split("\\+").map(p => {
      if (p.contains("NOU-P")) p else "O"
    }).mkString("+")
    w0.copy(attributes =  w.attributes.append(new UnprefixedAttribute("ana", neLabeling, Null)))
  }

  def noFeats(d: Elem)  = {
    PostProcessXML.updateElement(d, x => Set("w", "pc").contains(x.label), w => tweakW(w))
  }

  def changeRoot(d: Elem) = {
    <TEI xmlns="http://www.tei-c.org/ns/1.0">{d.child}</TEI>
  }

  def main(args: Array[String]) = {
    selection.foreach(f => {
      val d = XML.loadFile(f)
      val d1 = changeRoot(noFeats(fakeSentences(d)))
      val out = outputPath + f.getName
      println(out)
      XML.save(out, d1)
      //println(d1)
    })
  }
}
