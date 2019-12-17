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


  lazy val volumes: Map[Int, Elem] =
    allFiles.filter(f => doAll || volumeNumber(f) ==1).groupBy(volumeNumber)
      .mapValues(l => l.sortBy(pageNumber).filter(pageNumber(_) >= 0 ))
      .map({ case (volumeNr, l) => {
        val byToc: immutable.Seq[Elem] =
          l.map(loadAndInsertPageNumber(volumeNr)).groupBy({ case (e, k) => MissivenMetadata.findTocItem(volumeNr, k) })
            .toList.sortBy(_._1.page).map(
            { case (t, l) => <div type="missive">
              {t.toXML}{l.flatMap(x => (x._1 \\ "body").map(_.child))}
            </div>
            }
          ).toSeq
        volumeNr -> MissivenMetadata.addHeaderForVolume(volumeNr, concatenatePages(byToc))
      }
      })

  def splitVolume(volumeNumber: Int, volume: Node): Unit = {
    val dir = outputDirectory + s"$volumeNumber"
    new File(dir).mkdir()

    val divjes = (volume \\ "div")
    divjes.foreach(
      div => {
        val id = PageStructure.getId(div)
        if (id.nonEmpty)
          {
            Console.err.println(s"id=$id")
            val bibl = ((volume \ "teiHeader") \\ "bibl").find(x => (x \ "@inst").text == "#" + id.get)
            if (bibl.nonEmpty) {
              Console.err.println(s"bibl=${bibl.get}")
              val header = MissivenMetadata.createHeaderForMissive(volumeNumber, bibl.get)
              val tei = <TEI xml:id={id.get} id={id.get}>
                {header}
                <text>
                  <body>
                    {div}
                  </body>
                </text>
              </TEI>
              XML.save(dir + "/" + s"${id.get}.xml", tei, "utf-8")
            }
          }
      }
    )
  }

  def main(args: Array[String]): Unit = {
    MissivenMetadata.tocItemsPerVolume(6).foreach(println)

    volumes.par.foreach({ case (n, v) =>
        XML.save(outputDirectory + s"missiven-v$n.xml", v,"utf-8")
        splitVolume(n, v)
    })
  }
}
