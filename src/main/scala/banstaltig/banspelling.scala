package banstaltig

import database._
import zeeuws.HTML

import java.io.PrintWriter
import scala.util.{Failure, Success, Try}
import scala.xml._

object banspelling {
  lazy val molex = new database.Database(Configuration("x", "svowdb16.ivdnt.loc","gig_pro", "dba", "vercingetorix"))
  val table = "data.external_links"
  def checkze() =  molex.iterator(molex.allRecords(table)).filter(r =>   r("resource_name") == "Banlijst").toList.sortBy(r => r("modern_lemma")).map(r => {
    println(s"")
    val modern_lemma = r("modern_lemma")
    val lemma_id = r("lemma_id")
    val resource_name = r("resource_name")
    val resource_lemma = r("resource_lemma")
    val resource_lemma_id= r("resource_lemma_id")


      val norm = resource_lemma.replaceAll(" ", "-").toLowerCase()
      val link = s"https://namen.taalunie.org/content/$resource_lemma_id/$norm"

      Console.err.println(s"Checking $link")
      // println(link)
      val kopjes = Try(HTML.parse(io.Source.fromURL(link).getLines().mkString("\n"))) match {
        case Success(x) => x.descendant.filter(_.label.toLowerCase.matches("^h1.*"))
        case _ =>
          Console.err.println(s"Failure for $link")
          List()
      }
      // println(kopjes)
     val title_found = kopjes.map(_.text).mkString("")
     List(lemma_id, modern_lemma, resource_lemma, link, kopjes.nonEmpty, title_found, title_found == resource_lemma, title_found == modern_lemma)
     // println(regio)
  })

  def main(args: Array[String])  =  {

    val out = new PrintWriter("/tmp/checkban.tsv")
    checkze().foreach(x => out.println(x.mkString("\t")))
    out.close()
  }
}


/*

https://namen.taalunie.org/content/43201/heilig-roomse-rijk
modern_lemma       |       lemma_gigpos        | lemma_id | linked | gedrukt | online | log | resource_type | resource_name |        resource_lemma         | resource_lemma_id | resource_extra
-------------------------+---------------------------+----------+--------+---------+--------+-----+---------------+---------------+-------------------------------+-------------------+----------------
Oost-Afrikaans Hoogland | NOU-P(gender=n,number=sg) |   931031 | t      | f       | f      |     | external      | Banlijst      | Oost‑Afrikaans Hoogland       |             43560 | {nid}
Centraal-Azië           | NOU-P(gender=n,number=sg) |   931246 | t      | f       | f      |     | external      | Banlijst      | Centraal‑Azië                 |             43570 | {nid}
Noord-Equatoriale Rug   | NOU-P(gender=m,number=sg) |   933188 | t      | f       | f      |     | external      | Banlijst      | Noord‑Equatoriale Rug         |             43559 | {nid}
Voor-Indië              | NOU-P(gender=n,number=sg) |   933399 | t      | f       | f      |     | external      | Banlijst      | Voor‑Indië                    |             43585 | {nid}

 */