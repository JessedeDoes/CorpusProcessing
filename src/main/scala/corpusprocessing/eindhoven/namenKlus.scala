package corpusprocessing.eindhoven

import java.io.File

import corpusprocessing.eindhoven.Eindhoven.getId

import scala.xml.{Elem, XML}

object namenKlus {
  val xmlDir = Eindhoven.outputDir

  def extractNames(d: Elem): Seq[(String, String)] = (d \\ "name").map(n => {
    val txt = (n \\ "w").map(_.text)
    val t1 = txt.mkString("").trim.toLowerCase
    val t2 = txt.mkString(" ")
    t1 -> t2
  })

  def extractNameParts(d: Elem) = (d \\ "w").filter(x => (x \ "@type").text.startsWith("01")).map(
    x => {
      val txt = x.text.trim
      val t1 = txt.toLowerCase
      t1 -> txt
    }
  ) ++ extractNames(d)


  val allFiles = utils.ProcessFolder.processFolder(new File(xmlDir), identity).toSet.filter(_.isFile)
  val hansLoos = allFiles.filter(f => (Set("camb", "cgtl1", "cgtl2").contains(f.getParentFile().getName)))
  val hanzig = allFiles.diff(hansLoos)

  lazy val resolvedNames: Map[String, Set[String]] = hanzig.toStream.map(f => extractNameParts(XML.loadFile(f))).flatten.groupBy(_._1).
    mapValues(_.map(_._2).toSet)

  val unresolvedNames = hansLoos.flatMap(
    f => {
      val shortPath = f.getParentFile.getName + "/" + f.getName
      val d = XML.loadFile(f)
      (d \\ "w").filter(x => (x \ "@type").text.startsWith("01")).map((shortPath, _))
    }
  )

  def main(args: Array[String]): Unit = {
    //hansLoos.foreach(println)
    unresolvedNames.foreach(
      { case (f, n) => {
        val t = n.text
        val id = getId(n).getOrElse("?")
        val t1 = n.text.replaceAll("-", "")
        val candidates = resolvedNames.getOrElse(t1, Set.empty).mkString("|")
        println(s"$f $id $t -> $candidates")
      }
      }
    )
  }
}