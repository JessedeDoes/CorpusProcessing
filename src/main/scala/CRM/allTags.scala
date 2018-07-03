package CRM

import scala.xml._
import utils.ProcessFolder

object allPids
{
  def doFile(f: java.io.File):List[String] = (XML.loadFile(f) \\ "interpGrp")
    .filter(i => (i \\ "@type").text == "pid")
    .map(i => (i \ "interp").text.toString())
    .toList

  def main(args: Array[String]): Unit = {
    def it(s: String) =s
    val allFields = ProcessFolder.processFolder( new java.io.File(args(0)), doFile).flatten.toSet.toList.sortBy(it)
    allFields.foreach(println)
  }
}

case class TaggedWord(gigantTag: String, cgnTag: String, morfcode: String, word: String, lemma: String)
{
  def tagjes = (cgnTag, morfcode, gigantTag)
  lazy val codes = morfcode.split("\\+").toList
  lazy val lemmata = lemma.split("\\+").toList
  lazy val possen = gigantTag.split("\\+").toList
  lazy val groupId = codes.find(_.contains("{")).map(_.replaceAll(".*\\{","").replaceAll("[}*#]",""))
  lazy val starryIndex = codes.zipWithIndex.find(_._1.contains("{")).map(_._2)
  lazy val starryCode = starryIndex.map(codes(_))
  lazy val starryLemma = starryIndex.map(lemmata(_))
  lazy val starryPos = starryIndex.map(possen(_))
}


// <w msd="WW(hulp-of-koppel,pv,tgw,met-t)" pos="VRB(type=auxiliary,finiteness=finite,tense=present,inflection=t)" type="213" lemma="hebben" xml:id="w.33228">
object allTags {

  def w2W(i : Node) = {
    val word:String = i.child.filter(x => !(x.label == "fs")).text.trim
    TaggedWord(
      (i \ "@pos" ++ i \ "@function").text ,
      (i \ "@msd").text,
      (i \ "@type").text,
      word,
      (i \ "@lemma").text)
  }

  def doFile(f: java.io.File):List[TaggedWord] = { Console.err.println(f); (XML.loadFile(f) \\ "w").map(w2W).toList }

  def main(args: Array[String]): Unit = {
    def it(s: String) =s
    val allWords = ProcessFolder.processFolder( new java.io.File(args(0)), doFile).flatten
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
}

object GysSettings
{
  val atHome = true
  val gysAtHome = "/mnt/DiskStation/homes/jesse/work/Gys/"
  val gysAtWork = "/mnt/Projecten/Nederlab/corpusbouw/TEI/CorpusGysseling/"
  val gysDir = if (atHome) gysAtHome else gysAtWork
  val gysInput = gysDir + "klaar-voor-Jesse-en-Mathieu/"
}

object sterretjeMatje
{
  import allTags._

  def groupId(m: String) = m.replaceAll(".*\\{","").replaceAll("[}*#]","")

  def classify(l: Seq[TaggedWord]) =
  {
    val h = l.head

    val code = h.starryCode.get
    val lemma = h.starryLemma.get
    val pos = h.starryPos.get

    if (pos.contains("WW"))
      {

      }
    else if (pos.matches("ADV.*pron.*"))
      {

      }
  }
  def main(args: Array[String]): Unit = {
    def it(s: String) =s

    val allWords = ProcessFolder.processFolder( new java.io.File(GysSettings.gysInput), doFile).flatten.filter(_.morfcode.contains("{"))

    val groups = allWords.groupBy(m => m.groupId)

    groups.foreach({case (id,g) => println(g.map(w => s"${w.morfcode}:${w.lemma}").mkString(" "))})
  }
}
