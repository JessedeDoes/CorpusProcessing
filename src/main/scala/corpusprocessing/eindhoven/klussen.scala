package corpusprocessing.eindhoven

import scala.xml._
import Eindhoven._
import java.io.File
import java.nio.file._

import utils.{PostProcessXML, ProcessFolder}

object klussen {

  val baseXML  = Paths.get(Eindhoven.xmlDir).toAbsolutePath

  def relative(f:File) = baseXML.relativize(Paths.get(f.getCanonicalPath)).toString

  case class Kwic(f: File, w: Node, s: Node, h:Option[Word => Boolean]=None)
  {

    import java.nio.file.Paths

    val base  = Paths.get(Eindhoven.outputDir).toAbsolutePath

    val relative = base.relativize(Paths.get(f.getCanonicalPath))
    val theWord = word(w.asInstanceOf[Elem])

    def kwic = (s.descendant.filter(x => x.label == "w" || x.label == "pc")).map(w1 =>
      {
        val strippedText = w1.text.replaceAll("\\s+", " ").trim
        if (w1 == w) <b>{strippedText}</b>.toString else
          if (h.isDefined && h.get(word(w1))) <span style="color: red">{strippedText}</span> else
          strippedText}).mkString(" ")

    def toString(g: Word => String) = s"$relative\t${getId(w).getOrElse("NOID")}\t${g(theWord)}\t${theWord.pos}\t$kwic"
  }


  def extract(file: File, f: Word => Boolean, g: Word => String, h:Option[Word => Boolean] = None) = {
    //Console.err.println(file)
    val d = XML.loadFile(file)

    val sentences = (d \\ "s").filter(s => (s \\ "w").exists(w => f(word(w))))
    val kwics = sentences.flatMap(s =>
    {
      val wordz = (s \\ "w").filter(w => f(word(w)))
      wordz.map(Kwic(file,_,s,h))
    })
    kwics.foreach(x => println(x.toString(g)))
    kwics
  }

  def parseBoolean(s: String) =  s.toLowerCase.contains("t")

  def word(w: Node): Word = Word(w.text, (w \ "@lemma").text, (w \ "@pos").text)

  def maakKlus(dir: File, f: Word => Boolean, g: Word => String, h: Option[Word => Boolean]=None) = {
    ProcessFolder.processFolder(dir, file => extract(file,f,g,h))
  }

  case class adjInfo(fileName: String, wordId: String, isAttributief:Boolean, comment: String)
  {
    def key =(fileName, wordId)


  }

  case class zeZij(fileName: String, wordId: String, isSingular:Boolean, comment: String)
  {
    def key = (fileName, wordId)
    Console.err.println(this)
  }

  lazy val adjectiefKlus:Stream[adjInfo] = io.Source.fromFile("data/Eindhoven/adjectief.txt").getLines.toStream.map(l => l.split("\\t").toList).map(
    l => adjInfo(l(0), l(1), parseBoolean(l(2)), l(5)))

  lazy val adjMap = adjectiefKlus.groupBy(_.key).mapValues(_.head)


  lazy val zezijKlus:Stream[zeZij] = io.Source.fromFile("data/Eindhoven/zezij.txt").getLines.toStream.map(l => l.split("\\t").toList).map(
    l => zeZij(l(0), l(1), parseBoolean(l(2)), l(5)))

  lazy val zezijMap = zezijKlus.groupBy(_.key).mapValues(_.head)

  def doWord(f: String, w:Elem): Elem = {
    val info:Option[adjInfo] = getId(w).flatMap(id => adjMap.get( (f,id) ))
    val zinfo:Option[zeZij] = getId(w).flatMap(id => zezijMap.get( (f,id) ))
    val pos = (w \ "@pos").text

    // Console.err.println(f + "." + getId(w))

    val newPos1 = if (info.isDefined)
      {


        if (info.get.isAttributief)
          {
            val newVal = if (info.get.comment.toLowerCase.contains("postp")) "postnom" else "prenom"
            //val posNew = Eindhoven.replaceFeature(pos, "x-vrij", newVal)
            val posNew = Eindhoven.replaceFeature(pos.replaceAll("x-vrij|x-prenom|prenom.e-[a-z]+", newVal), "e-pred", "").replaceAll(",,",",")

            Console.err.println(s"############# ${info.get} $pos -> $newVal -> $posNew")
            posNew
          } else
        {
          val newVal = "vrij"
          val posNew = pos.replaceAll("prenom.e-pred","vrij,e-pred").replaceAll("x-vrij|x-prenom|prenom.e-[a-z]+", "vrij").replaceAll(",,",",")
          Console.err.println(s"############# ${info.get} $pos -> $newVal -> $posNew")
          posNew
        }
        // Console.err.println(info + " " + w)
        //System.exit(1)
      } else pos


    //VNW(pers,pron,nomin,vol) -> true -> VNW(pers,pron,nomin,vol)
    //VNW(pers,pron,nomin,red) -> true -> VNW(pers,pron,nomin,red)

    // Console.err.println(f + "." + getId(w))
    val newPos2:String = if (zinfo.isDefined)
    {
      //System.exit(1)
      val pos = newPos1

      if (zinfo.get.isSingular)
      {
        val posNew = pos.replaceAll("x-mv", "ev,fem").replaceAll("x-ev","ev").replaceAll("x-fem","fem")
        //val posNew = Eindhoven.replaceFeature(pos, "x-vrij", newVal)
        // val posNew = Eindhoven.replaceFeature(pos.replaceAll("x-vrij|x-prenom|prenom.e-[a-z]+", newVal), "e-pred", "").replaceAll(",,",",")


        Console.err.println(s"############# ${zinfo.get} $pos -> ${zinfo.get.isSingular} -> $posNew")
        if (posNew == "VNW(pers,pron,nomin,vol)" || posNew=="VNW(pers,pron,nomin,red)") posNew.replaceAll("\\)", ",ev,fem)") else posNew
      } else
      {

        val posNew = pos.replaceAll("x-ev,x-fem", "mv").replaceAll("x-ev","mv").replaceAll("x-fem","").replaceAll("x-mv","mv").replaceAll(",,",",")
        Console.err.println(s"############# ${zinfo.get} $pos -> ${zinfo.get.isSingular} -> $posNew")
        if (posNew == "VNW(pers,pron,nomin,vol)" || posNew=="VNW(pers,pron,nomin,red)") posNew.replaceAll("\\)", ",mv)") else posNew
      }
      // Console.err.println(info + " " + w)
      //System.exit(1)
    } else newPos1

    Eindhoven.replaceAttribute(w, "pos", _ => newPos2)
  }

  def nabewerking(d: Elem, f: String) =
  {
    PostProcessXML.updateElement(d, _.label=="w", w => doWord(f,w))
  }
}

object zezijklus {

  def interessant(w: Word) = Set("ze","zij").contains(w.word.toLowerCase) && w.pos.matches("VNW.*pers.*nomin.*")
  def main (args: Array[String] ): Unit = {
    klussen.maakKlus(new File(outputDir),
      w => interessant(w),
      w => if (w.pos.contains("ev")) "true" else "false",
      Some(w => w.pos.matches("WW.*pv.*")))
  }
}

object adjectiefklus {
  def main (args: Array[String] ): Unit = {
    klussen.maakKlus(new File(outputDir),
      w => w.pos.matches("ADJ.*(x-|\\|).*"),
      w => if (w.pos.contains("vrij") || w.pos.contains("prenom|e-pred")) "false" else "true",
      Some(w => w.pos.matches("N.*")))
  }
}
