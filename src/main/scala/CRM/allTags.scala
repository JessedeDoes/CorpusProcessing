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
}


// <w msd="WW(hulp-of-koppel,pv,tgw,met-t)" pos="VRB(type=auxiliary,finiteness=finite,tense=present,inflection=t)" type="213" lemma="hebben" xml:id="w.33228">
object allTags {

  def w2W(i : Node) = {
    val word:String = i.child.filter(x => !(x.label == "fs")).text.trim
    TaggedWord(
      (i \ "@pos").text,
      (i \ "@msd").text,
      (i \ "@type").text,
      word,
      (i \ "@lemma").text)
  }

  def doFile(f: java.io.File):List[TaggedWord] = (XML.loadFile(f) \\ "w").map(w2W).toList

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
