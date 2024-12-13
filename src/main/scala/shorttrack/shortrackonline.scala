package shorttrack

import utils.HTML

import java.net.URL
import java.nio.charset.CodingErrorAction
import scala.collection.immutable
import scala.io.Codec
import scala.xml._


case class Competition(n: Node) {

}

case class Skater(n: Node, name: String, club: String, number: String) {
  //println("######")
  //println((n \\ "td").map(_.text.trim).filter(_.nonEmpty).mkString("\n"))
  val pretty = new scala.xml.PrettyPrinter(100,2)
  def pr(m: Int) = {
    val prrow = n.descendant.filter(x => (x \ "td").exists(td => td.text.trim == s"$m meter"))
    if (prrow.nonEmpty) {
      // println(prrow)
      val cells = (prrow \ "td").toList

      val i = cells.indices.find(i => cells(i).text.trim == s"$m meter").get
      // println(cells(i))
      if (cells.size > i+2) {
        val r = Some(cells(i + 1).text + cells(i + 2).text)
        Console.err.println(s"PR for $name, distance=$m = $r");
        r
      } else None
    } else {
      if (m==500) {
        Console.err.println(s"!!!!!!!!!!!!No PR row for $name, distance=$m");
        // Console.err.println(pretty.format(n))
        //System.exit(1)
      }
      None }
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

  def getSkater(url: String, name: String, club: String, number: String) = {
    Console.err.println("Try skater: " + url)
    val lines  = io.Source.fromURL(new URL(url.replaceAll("http","https"))).getLines()
    // Console.err.println(lines.toList)
    val content = lines.mkString("\n")
    val contentX = HTML.parse(content)
    Skater(contentX, name, club, number)
  }

  val pretty = new scala.xml.PrettyPrinter(300, 4)
  val distances = List(222, 333, 444, 500, 777, 1000, 1500)

  def text(x: String)  = XML.loadString(s"<e>$x</e>").child
  lazy val nbsp = text("&#160;")
  def times(n: Int, node: NodeSeq) = (0 to n-1).flatMap(i => node)
  def main(args: Array[String]): Unit = {
    val htmlToProcess = args.headOption.getOrElse(Settings.inputHTML)
    val fAll: Skater => Boolean = x => true;
    val fIHCL: Skater => Boolean = x => x.club.toLowerCase().contains("ihcl");
    val fHVHW: Skater => Boolean = x => x.club.toLowerCase().contains("hvhw");
    val fIJA: Skater => Boolean = x => x.club.toLowerCase().contains("ija");
    val fDKIJV: Skater => Boolean = x => x.club.toLowerCase().contains("dkijv");

    val fFocus: Skater => Boolean = x=> fIHCL(x) || fHVHW(x) || fIJA(x)
    val tasks = List("/tmp/ihclOnly.html" -> fIHCL,
      "/tmp/hvhwOnly.html" -> fHVHW,
      "/tmp/ijAOnly.html" -> fIJA,
      "/tmp/dkijvOnly.html" -> fDKIJV,
      "/tmp/fSomeClubs.html" -> fFocus,
      "/tmp/allSkaters.html" -> fAll)

    tasks.foreach({ case (fileName, filter) =>
      val regio = HTML.parse(io.Source.fromFile(htmlToProcess).getLines().mkString("\n"))
      // println(pretty.format(regio))
      val sections = (regio \\ "h3").map(_.text)
      //println(sections)
      var allSkaters = List[Skater]()
      val tablez = sections.map(s => {
        val parent = regio.descendant.filter(x => ((x \ "h3").exists(_.text == s))).head
        val childElems = parent.child.filter(_.isInstanceOf[Elem])
        val i = childElems.indices.find(i => childElems(i).text == s).get
        val next = childElems(i + 1)
        //println(s)

        val skaters: immutable.Seq[Skater] = (next \\ "tr").flatMap(tr => {
          val number = (tr \ "td") (1).text
          val name = (tr \ "td") (3).text
          val club = (tr \ "td") (4).text.trim.replaceAll("^[A-Za-z0-9]", "")
          (tr.descendant.filter(x => (x \ "@href").nonEmpty)
            .map(x => getSkater((x \ "@href").text, name, club, number))
            )
        }).sortBy(s => s.PR500)

        //val ihclSkaters = skaters.filter(x => x.club.toLowerCase().contains("ihcl"))
        val infos = skaters.filter(filter).map(skater => {
          skater.htmlRow
        })
        allSkaters = allSkaters ++ skaters.filter(filter)
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
            border-collapse: collapse;
            font-size: 12pt;
            }}
            body {{ font-family : calibri; font-size: 11px }}
          </style>
        </head> <body>

          <div>
            <head>PR's</head>
          {tablez}
          </div>{if (fileName.toLowerCase.contains("ihcl") || fileName.toLowerCase.contains("hvhw") || fileName.toLowerCase.contains("ija")   || fileName.toLowerCase.contains("some")  )
            <div>
              <head>Resultaten per schaatser</head>{allSkaters.grouped(2).map(g => {
              <table>
                <tr>
                  {g.map(s => <td>
                  <div style="font-size:12pt">
                    <h3>
                      {s.number}<span> </span>{s.name}
                    </h3>
                    <table style="border-collapse: collapse; border-style:solid; border-width:1pt">
                      {(0 to 2).map(i => <tr>
                      <td width="2em">
                        {times(3, nbsp)}
                      </td> <td width="16em">
                        {times(25, nbsp)}
                      </td> <td width="20em">
                        {times(35, nbsp)}
                      </td>
                    </tr>)}
                    </table>
                  </div>
                </td>)}
                </tr>
              </table>
            })}
            </div>}
        </body>
      </html>
      XML.save(fileName, xml)
    })
    // val z = getSkater(lewis)
    // println(z.PRs)
  }
}
