package zeeuws

import java.net.{URI, URL}
import scala.xml._
import java.nio.charset.CodingErrorAction
import scala.collection.immutable
import scala.io.Codec


case class Competition(n: Node) {

}

case class Skater(n: Node, name: String, club: String) {
  //println("######")
  //println((n \\ "td").map(_.text.trim).filter(_.nonEmpty).mkString("\n"))

  def pr(m: Int) = {
    val prrow = n.descendant.filter(x => (x \ "td").exists(td => td.text.trim == s"$m meter"))
    if (prrow.nonEmpty) {
      // println(prrow)
      val cells = (prrow \ "td").toList

      val i = cells.indices.find(i => cells(i).text.trim == s"$m meter").get
      // println(cells(i))
      if (cells.size > i+2)
          Some(cells(i + 1).text + cells(i + 2).text)
      else None
    } else None
  }

  def PRs = shortrackonline.distances.map(pr)

  lazy val PR500: String =  { val z  = PRs(3).getOrElse("9:").replaceAll("^([0-9]{2})[.:]","0:$1."); println(z); z }

  //println(PR500)


  def htmlRow =           <tr>
    <td>
      {name}
    </td> <td>
      {if (club.trim().replaceAll("[^A-Za-z]", "").equals("IHCL")) <b>{club}</b> else club}
    </td>{prCells}
  </tr>

  def prCells = PRs.map(x => <td>
    {x.getOrElse("-")}
  </td>)
}

object shortrackonline {
  implicit val codec = Codec("UTF-8")
  codec.onMalformedInput(CodingErrorAction.REPLACE)
  codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

  def dinges(): Unit = {
    val src = Source.fromFile("aapje")
  }

  def getSkater(url: String, name: String, club: String) = {
    //Console.err.println("Try skater: " + url)
    val lines  = io.Source.fromURL(new URL(url)).getLines()
    //Console.err.println(lines.toList)
    val content = lines.mkString("\n")
    val contentX = HTML.parse(content)
    Skater(contentX, name, club)
  }

  val lewis = "http://www.shorttrackonline.info/skaterbio.php?id=STNED12802201301"
  val regiof = "data/regiofinale.html"
  val gent = "data/gent_deelnemers.html"
  val hutspot = "data/hutspot.html"
  val zoetermeer = "data/zoetermeer.html"
  val groningen = "data/groningen.html"
  val alkmaar = "data/alkmaar.html"
  val finaleAlkmaar = "data/regioFinaleAlkmaar.html"
  val denhaag = "data/regio_denhaag.html"
  val hasselt = "data/hasselt.html"
  val regio_utrecht = "data/regio_utrecht.html"
  val regio_dordrecht = "data/regiodordrecht.html"
  val ar2023 = "data/ar2023.html"
  val pretty = new scala.xml.PrettyPrinter(300, 4)
  val distances = List(222, 333, 444, 500, 777, 1000, 1500)

  def main(args: Array[String]): Unit = {

    val fAll: Skater => Boolean = x => true;
    val fIHCL: Skater => Boolean = x => x.club.toLowerCase().contains("ihcl");

    val tasks = List("/tmp/allSkaters.html" -> fAll, "/tmp/ihclOny.html" -> fIHCL)

    tasks.foreach({ case (fileName, filter) =>
      val regio = HTML.parse(io.Source.fromFile(ar2023).getLines().mkString("\n"))
      // println(pretty.format(regio))
      val sections = (regio \\ "h3").map(_.text)
      //println(sections)
      val tablez = sections.map(s => {
        val parent = regio.descendant.filter(x => ((x \ "h3").exists(_.text == s))).head
        val childElems = parent.child.filter(_.isInstanceOf[Elem])
        val i = childElems.indices.find(i => childElems(i).text == s).get
        val next = childElems(i + 1)
        //println(s)

        val skaters: immutable.Seq[Skater] = (next \\ "tr").flatMap(tr => {
          val name = (tr \ "td") (3).text
          val club = (tr \ "td") (4).text.trim
          (tr.descendant.filter(x => (x \ "@href").nonEmpty)
            .map(x => getSkater((x \ "@href").text, name, club))
            )
        }).sortBy(s => s.PR500)

        //val ihclSkaters = skaters.filter(x => x.club.toLowerCase().contains("ihcl"))
        val infos = skaters.filter(filter).map(skater => {
          skater.htmlRow
        })
        println(s + ": " + infos.size)
        <div>
          <h3>
            {s}
          </h3>
          <table>
            <tr>
              {(List("naam", "club") ++ distances).map(x => <th>
              {x}
            </th>)}
            </tr>{infos}
          </table>
        </div>
      })
      val xml = <html>
        <head>
          <style type="text/css">
            tr {{ height: 13pt }}
            table, tr, td, th {{ border: 1px solid black;
            padding: 3px;
            border-collapse: collapse; }}
            body {{ font-family : calibri; font-size: 11px }}
          </style>
        </head> <body>
          {tablez}
        </body>
      </html>
      XML.save(fileName, xml)
    })
    // val z = getSkater(lewis)
    // println(z.PRs)
  }
}
