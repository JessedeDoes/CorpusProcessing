package Wp6
import scala.xml._
import utils.PostProcessXML._
import java.io.File

import utils.PostProcessXML

import scala.collection.immutable
import scala.util.matching.Regex._
object SimplifyAndConcatenateTEI {

  // file:///home/jesse/workspace/xml2rdf/data/Missiven/7/toc.xml


  import Settings._

  val tocjes = new File(XMLDirectory).listFiles.filter(_.getName.matches("[0-9]+"))
    .map(f => (f.getName().toInt ->  new File(f.getCanonicalPath() + "/" + "toc.xml")))

  case class TocItem(volume: Int, pageNumber: String, title: String, level: Int)
  {
    lazy val page = pageNumber.toInt
    lazy val date = title.replaceAll(".*, *", "")
    lazy val author = title.replaceAll(",[^,]*$","")
    lazy val authors = author.split("\\s*,\\s*")

    def toXML = <bibl><level>{level}</level><volume>{volume}</volume><page>{page}</page><author>{author}</author><date>{date}</date><title>{title}</title></bibl>

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

  def tocItemFromBibl(b: Elem) =  { //
    val volume = (b \ "volume").text.toInt
    val pageNumber = (b \ "page").text
    val title =  (b \ "title").text
    val level = (b \ "level").text.toInt
    TocItem(volume, pageNumber, title, level)
  }


  def uuidForVolume(n: Int) = {
    val source = "INT-generalemissiven-deeltje" + n
    val bytes = source.getBytes("UTF-8")
    java.util.UUID.nameUUIDFromBytes(bytes).toString
  }

  lazy val tocItems: Map[Int, Array[TocItem]] = tocjes.flatMap(
    { case (n, f) => (XML.loadFile(f) \\ "item").map(
      item => TocItem(n, (item \\ "page").text, (item \\ "title").text, (item \ "@level").text.toInt)
    )}
  ).filter(_.pageNumber.matches("[0-9]+"))
    .groupBy(_.volume)
    .mapValues(l => l.sortBy(x => 10000 * x.page + x.level)) // kies liever een level 1 item (als laatste)



  def findTocItem(v: Int, p: Int) =
  {
    val bestMatch = tocItems(v).filter(_.page <= p).lastOption
    bestMatch.getOrElse(TocItem(0, "0", "no match", 0))
  }

  def simplifyOneElement(p: Elem): NodeSeq = {
        val r = p.label match {
          case "w" => Text(p.text + (if ((p \ "@type").text == "last") "" else ""))
          case "editionStmt" => Seq()
          case "sourceDoc" => Seq()
          case "pb" => if ((p \ "@unit").text == "external") p else Seq()
          case "fw" => p.copy(child = Text(p.text.trim))
          case _ =>
            val ital = if (p.label == "p") PageStructure.italicity(p) else 0

            val c = p.child.flatMap({
              case e: Elem => simplifyOneElement(e)
              case x => x
            } )

            val convertToNote = ital > 0.7
            val label = if (convertToNote) "note" else p.label
            val newAtts = if (convertToNote) p.attributes
              .append(new UnprefixedAttribute("place", "inline", Null))
              .append(new UnprefixedAttribute("type", "editorial-summary", Null))
              .append(new UnprefixedAttribute("resp", "editor", Null)) else p.attributes

            if (p.label == "div") (if ((p \\ "w").nonEmpty) c else Seq())
               else p.copy(child=c, label=label, attributes = newAtts.filter(_.key != "facs"))
        }
    r
  }

  def simplifyRendition(e: Elem) = {
    val rends = (e \ "hi").map(_ \ "@rend").map(_.text).groupBy(x => x).mapValues(_.size)
    val mfr = rends.toList.sortBy(_._2).last._1

    //System.err.println(s"Renditions: ${rends.size} : $rends")

    if (true)
      {
        val m1 = PageStructure.parseRend(mfr).filter(_._1 != "font-family")
        val m2 =  PageStructure.parseRend((e \ "@rend").text).filter(_._1 != "font-family")
        val newRend =  PageStructure.css(m1 ++ m2)

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


  def loadAndInsertPageNumber(v:Integer)(f: File) = {
    val n = pageNumber(f)
    Console.err.println("#### Page "  + n)
    val page = XML.loadFile(f)
    def insertPB(b: Elem) = b.copy(child = Seq(<pb unit='external' n={n.toString}/>) ++ b.child)
    val p1 = PostProcessXML.updateElement(page, _.label=="body", insertPB)
    val p3: Elem = simplifyPreTEI(MissivenPageStructure(PageStructure.zone(page), p1).basicPageStructuring())
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
    val d1 = updateElement(d, x => x.label == "div" && (x \ "@type").text == "missive", fixDivje)

    <TEI>
      {header}
      {d1.child}
    </TEI>
  }
  val doAll = false

  lazy val volumes: Map[Int, Elem] =
    allFiles.filter(f => doAll || volumeNumber(f) ==6).groupBy(volumeNumber)
      .mapValues(l => l.sortBy(pageNumber).filter(pageNumber(_) >= 0 ))
      .map({ case (volumeNr, l) => {
        val byToc: immutable.Seq[Elem] =
          l.map(loadAndInsertPageNumber(volumeNr)).groupBy({ case (e, k) => findTocItem(volumeNr, k) })
            .toList.sortBy(_._1.page).map(
            { case (t, l) => <div type="missive">
              {t.toXML}{l.flatMap(x => (x._1 \\ "body").map(_.child))}
            </div>
            }
          ).toSeq
        volumeNr -> createHeader(volumeNr, concatenatePages(byToc))
      }
      })

  def main(args: Array[String]): Unit = {
    tocItems(6).foreach(println)

    volumes.foreach({ case (n, v) =>
      XML.save(outputDirectory + s"missiven-v$n.xml", v,"utf-8")
    })
  }
}
