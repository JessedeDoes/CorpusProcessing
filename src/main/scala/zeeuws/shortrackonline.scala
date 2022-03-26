package zeeuws

import java.net.{URI, URL}
import scala.xml._

case class Competition(n: Node) {

}

case class Skater(n: Node) {
  def pr(m: Int) = {
    val prrow = n.descendant.filter(x => (x \ "td").exists(td => td.text == s"$m meter"))
    if (prrow.nonEmpty) {
      val cells = (prrow \ "td").toList
      val i = cells.indices.find(i => cells(i).text == s"$m meter").get
      Some(cells(i + 1).text + cells(i + 2).text)
    } else None
  }

  def PRs = shortrackonline.distances.map(pr)

  def prCells = PRs.map(x => <td>
    {x.getOrElse("-")}
  </td>)
}

object shortrackonline {
  def getSkater(url: String) = {
    val content = io.Source.fromURL(new URL(url)).getLines().mkString("\n")
    val contentX = HTML.parse(content)
    Skater(contentX)
  }

  val lewis = "http://www.shorttrackonline.info/skaterbio.php?id=STNED12802201301"
  val regiof = "data/regiofinale.html"

  val pretty = new scala.xml.PrettyPrinter(300, 4)
  val distances = List(222, 333, 444, 500, 777, 1000, 1500)

  def main(args: Array[String]): Unit = {
    val regio = HTML.parse(io.Source.fromFile(regiof).getLines().mkString("\n"))
    // println(pretty.format(regio))
    val sections = (regio \\ "h3").map(_.text)
    //println(sections)
    val tablez = sections.map(s => {
      val parent = regio.descendant.filter(x => ((x \ "h3").exists(_.text == s))).head
      val childElems = parent.child.filter(_.isInstanceOf[Elem])
      val i = childElems.indices.find(i => childElems(i).text == s).get
      val next = childElems(i + 1)
      println(s)

      val infos = (next \\ "tr").map(tr => {
        val name = (tr \ "td") (3).text
        val club = (tr \ "td") (4).text.trim
        (tr.descendant.filter(x => (x \ "@href").nonEmpty).map(x => {
          val skater = getSkater((x \ "@href").text)
          //println(s"$name/$club  -->  ${skater.PRs}")
          <tr>
            <td>
              {name}
            </td> <td>
            {club}
          </td>{skater.prCells}
          </tr>
        }
        ))
      }
      )
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
    XML.save("/tmp/out.html", xml)
    // val z = getSkater(lewis)
    // println(z.PRs)
  }
}
