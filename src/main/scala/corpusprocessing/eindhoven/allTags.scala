package corpusprocessing.eindhoven

import java.io.FileWriter

import corpusprocessing.eindhoven.Eindhoven.{files, hvhKort, hvhLang, vuPos}

import scala.xml.XML

object allTags {

  case class TaggedWord(word: String, kort: Option[String], lang: Option[String], vu: Option[String]) {
    override def toString() = s"$word,${vu.getOrElse("_")},${kort.getOrElse("_")},${lang.getOrElse("_")}"
  }

  def listAllTags() = files.flatMap(f => (XML.loadFile(f) \\ "w").map(n => TaggedWord(n.text, hvhKort(n), hvhLang(n), vuPos(n))))

  def vert() = listAllTags().foreach({ case TaggedWord(w, k, l, v) =>
    println(s"$w\t${k.getOrElse("_")}\t${l.getOrElse("_")}\t${v.getOrElse("_")}")
  })

  def byVuTag() = listAllTags().filter(w => w.vu.isDefined && w.vu.get.length == 3).groupBy(_.vu).mapValues(l => scala.util.Random.shuffle(l).toSet)

  //.map({ case (w,k,l,v) =>  s"$w\t${k}\t${l.getOrElse("_")}\t${v.getOrElse("_")}" }
  // )

  def byKorteTag = listAllTags().groupBy(_.kort).mapValues(l => scala.util.Random.shuffle(l).toSet)

  def splitLangetags: Stream[TaggedWord] = listAllTags().filter(_.lang.isDefined).flatMap(w => w.lang.getOrElse("").split("\\|").toList.map(l
  => w.copy(lang = Some(l.replaceAll("\\{.*?\\}", "").replaceAll("\\[.*?\\]", "").replaceAll("MTU.[0-9]*_L[0-9]+_", "")))))

  // splitLangetags.foreach(println)

  def byLangeTag = splitLangetags.groupBy(_.lang).mapValues(l => scala.util.Random.shuffle(l).toSet)


  def main(args: Array[String]) = {
    val files = List("vu", "kort", "lang").map(s => new FileWriter("/tmp/" + s + ".taginfo.txt"))
    val (vuFile, kortFile, langFile) = (files(0), files(1), files(2))

    byVuTag.toList.sortBy(_._1).foreach(
      {
        case (v, w) if (v.isDefined && v.get.length < 4) => {
          val allekortjes = w.filter(_.kort.isDefined).map(_.kort.get).toSet.mkString(",")
          vuFile.write(s"${v.get}\t${w.size}\t${w.take(5).map(_.word).mkString("  ")}\n")
        }

        case _ =>
      }
    )

    byKorteTag.toList.sortBy(_._1).foreach(
      {
        case (v, w) => {
          kortFile.write(s"${v.getOrElse("_")}\t${w.size}\t${w.take(5).map(_.toString).mkString("  ")}\n")
        }

        case _ =>
      }
    )

    byLangeTag.toList.sortBy(_._1).foreach(
      {
        case (v, w) => {
          langFile.write(s"${v.getOrElse("_")}\t${w.size}\t${w.take(5).map(_.toString).mkString("  ")}\n")
        }

        case _ =>
      }
    )
    files.foreach(_.close())

  }
}
