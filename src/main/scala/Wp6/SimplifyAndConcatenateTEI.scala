package Wp6
import scala.xml._
import utils.PostProcessXML._
import java.io.File

import utils.PostProcessXML

import scala.collection.immutable
import scala.util.matching.Regex._
object SimplifyAndConcatenateTEI {

  // file:///home/jesse/workspace/xml2rdf/data/Missiven/7/toc.xml

  val XMLDirectory = "data/Missiven"
  val inputDirectory = "/data/CLARIAH/WP6/generalemissiven/generalemissiven/"
  val outputDirectory = "/data/CLARIAH/WP6/generalemissiven/generalemissiven/Simplified/"

  val tocjes = new File(XMLDirectory).listFiles.filter(_.getName.matches("[0-9]+"))
    .map(f => (f.getName().toInt ->  new File(f.getCanonicalPath() + "/" + "toc.xml")))

  case class TocItem(volume: Int, pageNumber: String, title: String)
  {
    lazy val page = pageNumber.toInt
    lazy val date = title.replaceAll(".*, *", "")
    lazy val authors = title.replaceAll(",[^,]*$","")

    def toXML = <bibl><page>{page}</page><author>{authors}</author><date>{date}</date></bibl>
  }

  lazy val tocItems = tocjes.flatMap(
    { case (n, f) => (XML.loadFile(f) \\ "item").map(
      item => TocItem(n, (item \\ "page").text, (item \\ "title").text)
    )}
  ).filter(_.pageNumber.matches("[0-9]+")).groupBy(_.volume)



  def findTocItem(v: Int, p: Int) =
  {
    val bestMatch = tocItems(v).filter(_.page <= p).lastOption
    bestMatch.getOrElse(TocItem(0, "0", "no match"))
  }

  def italicity(e: Elem) = {
    val c1 = ((e \\ "hi").filter(x => (x \ "@rend").text.contains("italic")) \\ "w").size
    val c2 = ((e \\ "hi").filter(x => !(x \ "@rend").text.contains("italic")) \\ "w").size
    if (c1 + c2 == 0) 0.0 else c1 / (c1 + c2).asInstanceOf[Double]
  }

  def uppercaseity(e: Node) = {
    val t = e.text
    val u = t.count(_.isUpper)
    val l = t.count(_.isLower)
    if (u + l == 0) 0.0 else (u / (u+l).asInstanceOf[Double])
  }

  def parseRend(r: String): Map[String, String] = {
    //Console.err.println(r)
    r.split("\\s*;\\s*").filter(x => x.trim.nonEmpty && x.contains(":")).map(
      pv => {
        val a = pv.split("\\s*:\\s*")
        a(0) -> a(1)
      }
    ).toMap
  }

  def css(m: Map[String,String]) = m.map({case (k,v) => s"$k: $v"}).mkString("; ")

  def simplifyOne(p: Elem): NodeSeq = {
        val r = p.label match {
          case "w" => Text(p.text + " ")
          case "editionStmt" => Seq()
          case "sourceDoc" => Seq()
          case "pb" => if ((p \ "@unit").text == "external") p else Seq()

          case _ =>
            val ital = if (p.label == "p") italicity(p) else 0
            val rend = css(parseRend( (p \ "@rend").text).filter(_._1 != "font-family"))

            val newAtts = if (rend.nonEmpty) p.attributes.filter(_.key != "rend")
              .append(new UnprefixedAttribute("rend", rend, Null)) else p.attributes
            val c = p.child.flatMap({
              case e: Elem => simplifyOne(e)
              case x => x
            } )
            val label = if (ital > 0.7) "summary" else p.label
            if (p.label == "ab" && (p \\ "w").nonEmpty)
              c else
            p.copy(child=c, label=label, attributes = newAtts.filter(_.key != "facs"))
        }
    r
  }

  def simplifyHis(e: Elem) = {
    val rends = (e \ "hi").map(_ \ "@rend").map(_.text).groupBy(x => x).mapValues(_.size)
    val mfr = rends.toList.sortBy(_._2).last._1
    System.err.println(s"Renditions: ${rends.size} : $rends")
    if (true)
      {
        val m1 = parseRend(mfr)
        val m2 = parseRend((e \ "@rend").text)
        val newRend = css(m1 ++ m2)
        val newChild = e.child.flatMap(c => c match {
          case h:Elem if h.label == "hi" && ((h \ "@rend").text == mfr)=> h.child
          case _ => Seq(c)}
        )
        val newAtts = e.attributes.filter(_.key != "rend")
          .append(new UnprefixedAttribute("rend", newRend, Null))
        e.copy(child = newChild, attributes = newAtts)
      } else e
  }

  def simplify(d: Elem):Elem = {
    val d1 = updateElement2(d, e=> true, simplifyOne).head.asInstanceOf[Elem]
    val d2 = updateElement(d1, e => (e \ "hi").nonEmpty, simplifyHis)
    d2
  }

  lazy val allFiles = new File(inputDirectory).listFiles().filter(_.getName.endsWith(".xml")).toStream

  def volumeNumber(f: File) = {
    val r = "_([0-9]+)_".r
    val z: Seq[String] = r.findAllIn(f.getName).toList
    //Console.err.println(z + " " + f.getName)
    z(0).replaceAll("_","").toInt
  }

  def pageNumber(f: File) : Int = {
    val r = "content_pagina-(.*)-image.tei.xml".r
    val z: Seq[String] = r.findAllIn(f.getName).toList
    //Console.err.println(z + " " + f.getName)
    val n = z(0).replaceAll("content_pagina-|-image.*","")
    if (n.matches("[0-9]+")) n.toInt else -1
  }

  def concatenate(n: NodeSeq): Elem = {
    <TEI>
      <text>
        <body>
        {n}
        </body>
      </text>
    </TEI>
  }

  def concat(n: (Elem,Elem)): NodeSeq = n._1 ++ n._2

  def plakFiles(): Unit = {

  }

  def getId(n: Node): Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).headOption

  case class Box(uly: Int, ulx: Int, lry: Int, lrx: Int)

  def zoneX(p: Elem, b: Node) = {
    val facs = (b \ "@facs").text.replaceAll("#","")
    //println(facs)
    val zone = p.descendant.filter(z => z.label=="zone" && getId(z).get == facs).head
    def ai(s: String): Int = (zone \ s).text.toInt
    Box(ai("@uly"), ai("@ulx"), ai("@lry"), ai("@lrx"))
  }

  def zone(p: Elem, b: Node) = {
    if (b.label == "p")
      zoneX(p, (b \\ "lb").head)
    else
      zoneX(p, b)
  }

  def loadAndInsertPageNumber(v:Integer)(f: File) = {
    val n = pageNumber(f)
    Console.err.println("#### Page "  + n)
    val x = XML.loadFile(f)
    def insertPB(b: Elem) = b.copy(child = Seq(<pb unit='external' n={n.toString}/>) ++ b.child)
    val peetjes = (x \\ "p").zipWithIndex
    val candidateHeaders = peetjes.filter({case (p,i) => i <= 2 && uppercaseity(p) >=  0.8}).map(_._1)

    if (candidateHeaders.nonEmpty)
      {
        // Console.err.println("\n####### HEADER\n" + candidateHeaders.head)
      }

    val candidateKopregels1 = (x \\ "ab").take(2).filter(ab => {
      val b = zone(x,ab)
      b.uly < 300 && b.lry < 600
    })

    val candidateKopregels2 = (x \\ "p").take(2).filter(ab => {
      if ((ab \\ "lb").isEmpty) false else {
        val b = zone(x, ab)
        b.uly < 250
      }
    })

    val candidateKopregels = if (candidateKopregels1.nonEmpty) candidateKopregels1 else candidateKopregels2

    candidateKopregels.foreach(kr => Console.err.println(kr.text + " " + zone(x,kr)))

    val p1 = PostProcessXML.updateElement(x, _.label=="body", insertPB)
    val p2 = PostProcessXML.updateElement3(p1, x => x.label=="p" || x.label=="ab",
      p => if (candidateKopregels.contains(p))
        p.copy(label="running-header") else p)

    val p3 = PostProcessXML.updateElement(p2, _.label=="p",
      p => if (candidateHeaders.contains(p)) p.copy(label="head") else p)
    (p3, n)
  }

  lazy val volumes: Map[Int, Elem] =
    allFiles.filter(f => volumeNumber(f) ==6).groupBy(volumeNumber)
      .mapValues(l => l.sortBy(pageNumber).filter(pageNumber(_) >= 0 ))
      .map({ case (n,l) =>
        {
          val byToc =
            l.map(loadAndInsertPageNumber(n)).groupBy({case (e,k) => findTocItem(n,k)}).toList.sortBy(_._1.page).map(
              {case (t, l) => <div>{t.toXML}{l.flatMap(x => (x._1 \\ "body").map(_.child))}</div>}
            ).toSeq
          n -> concatenate(byToc.map(simplify))}})

  def main(args: Array[String]): Unit = {
    volumes.foreach({ case (n, v) =>
      XML.save(outputDirectory + s"missiven-v$n.xml", v)
    })
  }
}
