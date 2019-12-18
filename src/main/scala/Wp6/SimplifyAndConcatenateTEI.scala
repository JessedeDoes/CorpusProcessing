package Wp6
import scala.xml._
import utils.PostProcessXML._
import java.io.{File, PrintWriter}

import utils.PostProcessXML

import scala.collection.immutable
import scala.util.matching.Regex._


object SimplifyAndConcatenateTEI {

  // file:///home/jesse/workspace/xml2rdf/data/Missiven/7/toc.xml


  import Settings._



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
    //Console.err.println("#### Page "  + n)
    val page = XML.loadFile(f)
    def insertPB(b: Elem) = b.copy(child = Seq(<pb unit='external' n={n.toString}/>) ++ b.child)
    val p1 = PostProcessXML.updateElement(page, _.label=="body", insertPB)
    val p3: Elem = PageStructure.simplifyPreTEI(MissivenPageStructure(PageStructure.zone(page), p1).basicPageStructuring())
    (p3, n)
  }


  // skip pages with just empty missiven headers

  val skipPages = List(1 -> 3, 1->16, 1->20, 1 -> 27, 1 -> 97, 1 -> 98, 1 -> 118, 1->244)

  def skipPage(v: Int, n: Int) = skipPages.contains((v,n))

  lazy val volumes: Map[Int, Elem] =
    allFiles.filter(f => doAll || volumeNumber(f) == Settings.theOneToProcess).groupBy(volumeNumber)
      .mapValues(l => l.sortBy(pageNumber).filter(pageNumber(_) >= 0))
      .map({ case (volumeNr, l) => {
        val byToc: immutable.Seq[Elem] =
          l.map(loadAndInsertPageNumber(volumeNr)).filter({case (e,k) => !skipPage(volumeNr,k)}).groupBy({ case (e, k) => MissivenMetadata.findTocItem(volumeNr, k) })
            .toList.sortBy(_._1.page).map(
            { case (t, l) => <div type="missive">
              {t.toXML}{l.flatMap(x => (x._1 \\ "body").map(_.child))}
            </div>
            }
          ).toSeq
        volumeNr -> MissivenMetadata.addHeaderForVolume(volumeNr, concatenatePages(byToc))
      }})

  def assignIds(e: Elem, prefix: String, k: Int): Elem =
  {
    val myId = s"$prefix.${e.label}.$k"

    val children: Map[String, Seq[((Node, Int), Int)]] = e.child.toList.zipWithIndex.filter(_._1.isInstanceOf[Elem]).groupBy(_._1.label).mapValues(_.zipWithIndex)

    val indexMapping: Map[Int, Int] = children.values.flatMap(x => x).map( {case ((n,i),j) => i -> j} ).toMap


    val newChildren: Seq[Node] = e.child.zipWithIndex.map({
      case (e1: Elem, i) => val k1 = indexMapping(i); assignIds(e1, myId, k1+1)
      case (x, y) => x
    })
    val newAtts = if (e.label == "TEI") e.attributes else  e.attributes.filter(_.key != "id").append(new PrefixedAttribute("xml", "id", myId, Null))
    e.copy(child=newChildren, attributes = newAtts)
  }

  def cleanupTEI(tei: Elem, pid: String) = {
    val e0 = PostProcessXML.updateElement3(tei, e => true, e => {
      e.copy(attributes = e.attributes.filter(a => a.key != "inst" && !(e.label == "pb" && a.key=="unit")))
    })
    assignIds(e0, pid, 1)
  }

  def splitVolume(volumeNumber: Int, volume: Node): Unit = {
    val dir = outputDirectory + s"split/$volumeNumber"
    new File(dir).mkdir()

    val divjes = (volume \\ "div")
    divjes.foreach(
      div => {
        val id = PageStructure.getId(div)
        if (id.nonEmpty)
          {
            //Console.err.println(s"id=$id")
            val bibl = ((volume \ "teiHeader") \\ "bibl").find(x => (x \ "@inst").text == "#" + id.get)
            if (bibl.nonEmpty) {
              //Console.err.println(s"bibl=${bibl.get}")
              val header = MissivenMetadata.createHeaderForMissive(volumeNumber, bibl.get)
              val tei = <TEI xmlns="http://www.tei-c.org/ns/1.0" xml:id={id.get}>
                {header}
                <text>
                  <body>
                    {div}
                  </body>
                </text>
              </TEI>
              XML.save(dir + "/" + s"${id.get}.xml", cleanupTEI(tei, id.get), "utf-8")
            }
          }
      }
    )
  }

  def main(args: Array[String]): Unit = {
    // MissivenMetadata.tocItemsPerVolume(6).foreach(println)
    println(assignIds(<TEI xmlns="aapje"><div><p></p><p></p></div></TEI>, "xx", 1))
    val u = new PrintWriter("/tmp/unused_tocitems.txt")
    volumes.par.foreach({ case (n, v) =>
        val z = MissivenMetadata.unusedTocItemsForVolume(n,v)
        u.println(s"Unused items for volume $n: ${z.size}")
        z.foreach(t => u.println("Unused: " + t.pid + " " + t.pageNumber + " " + t.title))
        XML.save(outputDirectory + s"pervolume/missiven-v$n.xml", v,"utf-8")
        splitVolume(n, v)
    })
    u.close()
  }
}
