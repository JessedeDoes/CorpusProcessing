package CRM

import scala.xml._
import utils.ProcessFolder

object allPids {
  def main(args: Array[String]): Unit = {
    def it(s: String) = s

    val allFields = ProcessFolder.processFolder(new java.io.File(args(0)), doFile).flatten.toSet.toList.sortBy(it)
    allFields.foreach(println)
  }

  def doFile(f: java.io.File): List[String] = (XML.loadFile(f) \\ "interpGrp")
    .filter(i => (i \\ "@type").text == "pid")
    .map(i => (i \ "interp").text.toString())
    .toList
}

case class TaggedWord(id: String, gigantTag: String, cgnTag: String, morfcode: String, word: String, lemma: String) {
  lazy val codes = morfcode.split("\\+").toList
  lazy val lemmata = lemma.split("\\+").toList
  lazy val possen = gigantTag.split("\\+").toList
  lazy val groupId = codes.find(_.contains("{")).map(_.replaceAll(".*\\{", "").replaceAll("[}*#]", ""))
  lazy val starryIndex = codes.zipWithIndex.find(_._1.contains("{")).map(_._2)
  lazy val starryCode = starryIndex.map(codes(_))
  lazy val starryLemma = starryIndex.map(lemmata(_))
  lazy val starryPos = starryIndex.map(possen(_))

  def tagjes = (cgnTag, morfcode, gigantTag)
}


// <w msd="WW(hulp-of-koppel,pv,tgw,met-t)" pos="VRB(type=auxiliary,finiteness=finite,tense=present,inflection=t)" type="213" lemma="hebben" xml:id="w.33228">
object allTags {

  def main(args: Array[String]): Unit = {
    def it(s: String) = s

    val allWords = ProcessFolder.processFolder(new java.io.File(args(0)), doFile).flatten
    val grouped = allWords.groupBy(w => w.tagjes)
    val withExamples = grouped.mapValues(l => (l.size, l.map(_.word).toSet.take(5))).toList.sortBy(_._1._1)
    withExamples.foreach(
      e => {
        val p = e._1
        val pp = s"${p._1}\t${p._2}\t${p._3}"
        println(s"$pp\t[${e._2._1}]\t${e._2._2.mkString(", ")}")
      }
    )
    //allFields.foreach(println)
  }

  def doFile(f: java.io.File): List[TaggedWord] = {
    Console.err.println(f);
    (XML.loadFile(f) \\ "w").map(w2W).toList
  }

  def w2W(i: Node) = {
    val word: String = i.child.filter(x => !(x.label == "fs")).text.trim.replaceAll("\\s+", " ")
    val id = i.attributes.filter(a => a.key == "id" || a.key == "n").head.value.text

    TaggedWord(
      id,
      (i \ "@pos" ++ i \ "@function").text,
      (i \ "@msd").text,
      (i \ "@type").text,
      word,
      (i \ "@lemma").text)
  }
}

object GysSettings {
  val atHome = true
  val gysAtHome = "/mnt/DiskStation/homes/jesse/work/Gys/"
  val gysAtWork = "/mnt/Projecten/Nederlab/corpusbouw/TEI/CorpusGysseling/"
  val gysDir = if (atHome) gysAtHome else gysAtWork
  val gysInput = gysDir + "klaar-voor-Jesse-en-Mathieu/"
}

object sterretjeMatje {

  import allTags._

  val particleTF = io.Source.fromFile("data/Gys/particles.txt").getLines.map(l => l.split("\\t")).map(l => l(1) -> l(0).toInt).toMap
  val particles = particleTF.keySet

  def groupId(m: String) = m.replaceAll(".*\\{", "").replaceAll("[}*#]", "")

  def main(args: Array[String]): Unit = {
    def it(s: String) = s

    val allWords = ProcessFolder.processFolder(new java.io.File(GysSettings.gysInput), doFile).flatten.filter(_.morfcode.contains("{"))

    val groups = allWords.groupBy(m => m.groupId)

    groups.foreach({ case (id, g) =>

      classify(g).zipWithIndex.foreach({ case (x, i) => val y = x._2; println(s"[$i]\t${x._1}\t${y._1}\t${y._2}\t${y._3}") })
    })
  }

  def classify(l: Seq[TaggedWord]) = {
    val h = l.head

    val code = h.starryCode.get
    val lemma = h.starryLemma.get
    val pos = h.starryPos.get

    val plakLemma = l.map(_.starryLemma.get).mkString("|").replaceAll("-", "")

    if (pos.contains("ADP")) {
      l.zipWithIndex.map({case (w,i) =>
          val feature = if (i == 0) "deel-vz" else  "deel-vz"
          w.id -> (feature, w.word, plakLemma)
      })
    } else if (pos.contains("CON")) {
      l.map(w => w.id -> ("deel-vg", w.word, plakLemma))
    } else if (pos.matches("NOU.*proper.*")) {
      l.map(w => w.id -> ("deel-eigen", w.word, plakLemma)) // zou spec(deeleigen) kunnen worden
    } else if (pos.matches("NOU.*common.*")) {
      l.map(w => w.id -> ("deel-znw", w.word, plakLemma))
    } else if (pos.contains("VRB")) {
      val particle = l.find(w => particles.contains(w.starryLemma.get.replaceAll("-", "")))
      val otherParts = particle.map(p => l.diff(Seq(p)))

      if (particle.isDefined) {
        val lemma = (particle.get.starryLemma.get + "|" + otherParts.get.map(w => w.starryLemma.get).mkString("")).replaceAll("-", "")
        List(particle.get.id -> ("bw-deel-ww", particle.get.word, lemma)) ++ otherParts.getOrElse(List()).map(w => w.id -> ("hoofddeel-ww", w.word, lemma))
      }
      else {
        Console.err.println("WW UNCLEAR: " + l.map(w => s"${w.morfcode}:${w.lemma}").mkString(" "))
        l.map(w => w.id -> ("deel", w.word, plakLemma))
      }
    } else if (pos.matches("ADV.*pron.*")) {
      val pronominalPart = l.find(_.starryLemma.get.matches("-?(HIER|DAAR|WAAR|ER|ALDAAR)-?"))
      val otherParts = pronominalPart.map(p => l.diff(Seq(p)))

      if (pronominalPart.isDefined) {
        val lemma = (pronominalPart.get.starryLemma.get + "|" + otherParts.get.map(w => w.starryLemma.get).mkString("")).replaceAll("-", "")
        List(pronominalPart.get.id -> ("hoofddeel-bw", pronominalPart.get.word, lemma)) ++ otherParts.getOrElse(List()).map(w => w.id -> ("vz-deel-bw", w.word, lemma))
      }
      else {
        Console.err.println("VNWBW UNCLEAR: " + l.map(w => s"${w.morfcode}:${w.lemma}").mkString(" "))
        l.map(w => w.id -> ("deel", w.word, plakLemma))
      }
    } else if (pos.matches("ADV.*")) {
      l.map(w => w.id -> ("deel-bw", w.word, plakLemma)) // zou spec(deeleigen) kunnen worden
    }
    else {
      Console.err.println("OTHER + " + l.map(w => s"${w.morfcode}:${w.lemma}").mkString(" "))
      l.map(w => w.id -> ("deel", w.word, plakLemma))
    }
  }
}
