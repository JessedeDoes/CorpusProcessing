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
  val rootAtHome = "/data/CLARIAH/WP6/generalemissiven/generalemissiven/"

  val inputDirectoryatHome = "/data/CLARIAH/WP6/generalemissiven/generalemissiven/"
  val inputDirectoryatWork = "/mnt/Nederlab/Corpusdata/2-PreTEI/generalemissiven/"

  val outputDirectoryatHome = "/data/CLARIAH/WP6/generalemissiven/generalemissiven/Simplified/"
  val outputDirectoryAtWork = "/mnt/Nederlab/Corpusdata/5-TEI/generalemissiven/"

  val inputs = List(inputDirectoryatHome, inputDirectoryatWork)
  val outputs = List(outputDirectoryatHome, outputDirectoryAtWork)

  val inputDirectory = inputs.filter(new File(_).exists()).head
  val outputDirectory = outputs.filter(new File(_).exists()).head

  println(s"$inputDirectory $outputDirectory")

  val tocjes = new File(XMLDirectory).listFiles.filter(_.getName.matches("[0-9]+"))
    .map(f => (f.getName().toInt ->  new File(f.getCanonicalPath() + "/" + "toc.xml")))

  case class TocItem(volume: Int, pageNumber: String, title: String)
  {
    lazy val page = pageNumber.toInt
    lazy val date = title.replaceAll(".*, *", "")
    lazy val author = title.replaceAll(",[^,]*$","")
    lazy val authors = author.split("\\s*,\\s*")

    def toXML = <bibl><volume>{volume}</volume><page>{page}</page><author>{author}</author><date>{date}</date><title>{title}</title></bibl>

    def toTEI = <listBibl><bibl>
      <interpGrp inst={inst} type="titleLevel1"><interp>{title}</interp></interpGrp>
      <interpGrp inst={inst} type="dateLevel1"><interp>{date}</interp></interpGrp>
      <interpGrp inst={inst} type="authorLevel1">{authors.map(z => <interp>{z}</interp>)}</interpGrp>
      <interpGrp inst={inst} type="pid"><interp>{pid}</interp></interpGrp>
    </bibl></listBibl>

    def uuid():String =
    {
      val source = this.toString
      val bytes = source.getBytes("UTF-8")
      java.util.UUID.nameUUIDFromBytes(bytes).toString
    }

    def pid() = s"INT_${uuid()}"

    def inst  = s"#$pid"
  }

  def tocItemFromBibl(b: Elem) =  {
    val volume = (b \ "volume").text.toInt
    val pageNumber = (b \ "page").text
    val title =  (b \ "title").text
    TocItem(volume, pageNumber, title)
  }


  def uuidForVolume(n: Int) = {
    val source = "INT-generalemissiven-deeltje" + n
    val bytes = source.getBytes("UTF-8")
    java.util.UUID.nameUUIDFromBytes(bytes).toString
  }

  lazy val tocItems: Map[Int, Array[TocItem]] = tocjes.flatMap(
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

  def simplifyOneElement(p: Elem): NodeSeq = {
        val r = p.label match {
          case "w" => Text(p.text + (if ((p \ "@type").text == "last") "" else " "))
          case "editionStmt" => Seq()
          case "sourceDoc" => Seq()
          case "pb" => if ((p \ "@unit").text == "external") p else Seq()

          case _ =>
            val ital = if (p.label == "p") italicity(p) else 0

            val c = p.child.flatMap({
              case e: Elem => simplifyOneElement(e)
              case x => x
            } )

            val label = if (ital > 0.7) "note" else p.label
            val newAtts = if (label == "note") p.attributes
              .append(new UnprefixedAttribute("place", "inline", Null))
              .append(new UnprefixedAttribute("type", "editorial-summary", Null))
              .append(new UnprefixedAttribute("resp", "editor", Null)) else p.attributes

              if (p.label == "ab" && (p \\ "w").nonEmpty)
              c else
            p.copy(child=c, label=label, attributes = newAtts.filter(_.key != "facs"))
        }
    r
  }

  def simplifyRendition(e: Elem) = {
    val rends = (e \ "hi").map(_ \ "@rend").map(_.text).groupBy(x => x).mapValues(_.size)
    val mfr = rends.toList.sortBy(_._2).last._1

    //System.err.println(s"Renditions: ${rends.size} : $rends")

    if (true)
      {
        val m1 = parseRend(mfr).filter(_._1 != "font-family")
        val m2 = parseRend((e \ "@rend").text).filter(_._1 != "font-family")
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

  def findLastWordIn(p: Elem) = {
    if  ((p \\ "w").nonEmpty) {
      val lastWord = (p \\ "w").last
      updateElement(p, _.label=="w", w => (if (w==lastWord) w.copy(attributes = w.attributes.append(new UnprefixedAttribute("type", "last", Null))) else w))
    } else p
  }

  def simplifyPreTEI(d: Elem):Elem = {
    // whitespace handling

    val d0 = updateElement(d, _.label == "p", findLastWordIn)
    val d1 = updateElement2(d0, e=> true, simplifyOneElement).head.asInstanceOf[Elem]
    val d2 = updateElement(d1, e => (e \ "hi").nonEmpty, simplifyRendition)
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

  def concatenatePages(n: NodeSeq): Elem = {
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
        p.copy(label="fw", attributes=
        p.attributes.append(new UnprefixedAttribute("type", "head", Null)).append(new UnprefixedAttribute("place", "top", Null) )) else p)
    val p3 = PostProcessXML.updateElement(p2, _.label=="p",
      p => if (candidateHeaders.contains(p)) p.copy(label="head") else p)
    (p3, n)
  }

  def createHeader(v: Int, d: Elem) = {
    val allTocItems = (d \\ "bibl").map(b => tocItemFromBibl(b.asInstanceOf[Elem]))

    val header = <teiHeader>
      <fileDesc>
        <titleStmt>
          <title>Tweede boek, deel I</title>
        </titleStmt>
        <publicationStmt>
          <p>
            <date></date>
            <idno type="sourceID">missiven:vol{v}</idno>
            <idno type="pid">INT_{uuidForVolume(v)}</idno>
          </p>
        </publicationStmt>
        <notesStmt>
          <note/>
        </notesStmt>
        <sourceDesc>
          <listBibl xml:id="inlMetadata">
            <bibl>
            </bibl>
          </listBibl>
          {allTocItems.map(t => t.toTEI)}
        </sourceDesc>
      </fileDesc>
    </teiHeader>

    def fixDivje(d: Node) = {
      val t = tocItemFromBibl((d \ "bibl").head.asInstanceOf[Elem])
      <div xml:id={t.pid()}>
        {d.child.filter(_.label != "bibl")}
      </div>
    }
    val d1 = updateElement(d, _.label == "div", fixDivje)

    <TEI>
      {header}
      {d1.child}
    </TEI>
  }
  val doAll = true

  lazy val volumes: Map[Int, Elem] =
    allFiles.filter(f => doAll || volumeNumber(f) ==6).groupBy(volumeNumber)
      .mapValues(l => l.sortBy(pageNumber).filter(pageNumber(_) >= 0 ))
      .map({ case (n,l) =>
        {
          val byToc =
            l.map(loadAndInsertPageNumber(n)).groupBy({case (e,k) => findTocItem(n,k)}).toList.sortBy(_._1.page).map(
              {case (t, l) => <div>{t.toXML}{l.flatMap(x => (x._1 \\ "body").map(_.child))}</div>}
            ).toSeq
          n -> createHeader(n, concatenatePages(byToc.map(simplifyPreTEI)))}})

  def main(args: Array[String]): Unit = {
    tocItems(6).foreach(println)

    volumes.foreach({ case (n, v) =>
      XML.save(outputDirectory + s"missiven-v$n.xml", v,"utf-8")
    })
  }
}
