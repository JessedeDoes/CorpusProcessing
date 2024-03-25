package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy

import corpusprocessing.eindhoven.Eindhoven.getId

import scala.xml._
import java.io.{File, PrintWriter}
object ANSexamples {
  val ansDIR = new File("/home/jesse/workspace/ANS/")
  val example = new File("/mnt/b79b34af-5025-4515-93b8-30c09d3c9245/home/jesse/workspace/ANS/04/04/body.xml")
  val out = new PrintWriter("/tmp/ans.examples.txt")

  lazy val tocFiles = ansDIR.listFiles().filter(_.getName.matches("toc[0-9]+.xml"))
  lazy val tocLines = tocFiles.flatMap(f => io.Source.fromFile(f, "iso-8859-1").getLines()).filter(_.contains("<toc"))

  lazy val tocMap = tocLines.map(l => {
    val d = XML.loadString(l)
    val id = "ans." + (d \ "@url").text.replaceAll("/", ".")
    val title = d.text
    id -> title
  } )

  lazy val tocMapEnhanced = tocMap.map({case (id,t) => {
    val ancestors = tocMap.filter({case (id1,t1) => {
      id.startsWith(id1)
    }}).sortBy(_._1.size).map(_._2).mkString(" // ")
    id -> ancestors
  }}).toMap

  def loadFile(f: File)  = {
    val content = scala.io.Source.fromFile(f,"ISO-8859-1").getLines.mkString("\n").replaceAll("<!DOCTYPE.*?>", "")

    XML.loadString(content)
  }

  def doExample(e: Node, f:File)  = {
    val n = (e \ "@ori").text
    val id = (e \ "@id").text.replaceAll("/", ".")

    val path = (e \ "@url").text.replaceAll("/", ".")
    val text = e.text.trim.replaceAll("\\s+", " ")
    if (id.nonEmpty) {
       out.println(s"$path\t$n\t$id\t$text\t${f.getCanonicalPath}")
       out.flush()
    }
    (id.nonEmpty -> <cit id={s"ans.$path.$id"} n={n}>{text}</cit>)
  }

  def doFile(f: File)  = {
    // println(f.getCanonicalPath)
    val doc = loadFile(f)
    val examples = (doc \\ "x")
    examples.map(e => doExample(e,f)).filter(_._1).map(_._2)
  }

  def doDir(f: File): NodeSeq  = {
    val files = f.listFiles().filter(_.getName.endsWith(".xml")).toSeq
    val divs = files.sortBy(_.getCanonicalPath).map(f => {
      try {
        if (f.getCanonicalPath.matches(".*ANS/[0-9].*")) {
          val cits = doFile(f)
          if (cits.nonEmpty) {
            val id = (cits.head \ "@id").text.replaceAll(".[^.]*$", "")
            <div id={id}><head>{tocMapEnhanced.getOrElse(id, "")}</head>{cits}</div>
          } else <div/>
        } else
        {<div/>}
      }
      catch  {
        case e: Exception => Console.err.println(s"${f.getCanonicalPath}: ${e.getMessage}")
            <div/>
      }
    })
    val subDirs = f.listFiles().filter(_.isDirectory)
    (divs ++ subDirs.flatMap(doDir)).filter(d => (d \\ "cit").nonEmpty).sortBy(d => getId((d \\ "cit").head))
  }

  def main(args: Array[String])  = {

    tocMap.foreach(println)
    val chapters = ansDIR.listFiles.filter(_.getName.matches("[0-9]+"))
    chapters.foreach(c => {
      val chapterNumber = c.getName
      println(chapterNumber)
      val title = chapterNumber + " " + tocMapEnhanced.getOrElse("ans."  + chapterNumber, "title not found")
      val divs = doDir(c)
      val n = c.getName
      val doc =
        <TEI xmlns="http://www.tei-c.org/ns/1.0" xml:id={"ans." + chapterNumber}>
          <text>
            <body>
              <title>{title}</title>
              {divs}
            </body>
          </text>
        </TEI>
      val pretty = new PrettyPrinter(100, 4)
      val f = pretty.format(doc)
      val xmlOut = s"/tmp/ans.$n.examples.xml"
      val p = new PrintWriter(xmlOut)
      p.println(f)
      p.close()
    })
    //doFile(example)
  }
}
