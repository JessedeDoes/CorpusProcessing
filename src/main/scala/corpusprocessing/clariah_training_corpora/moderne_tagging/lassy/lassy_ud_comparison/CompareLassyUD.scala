package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy_ud_comparison

import basex.QueryExample.runQuery

import java.net.URLEncoder
import scala.collection.immutable
import scala.xml._
import scala.util.{Try,Success,Failure}
/*
http://svotmc10.ivdnt.loc/corpus-frontend/lassy-groter/search/hits?first=0&number=100&patt=${}&interface=%7B%22form%22%3A%22search%22%2C%22patternMode%22%3A%22expert%22%7D
 */
object CompareLassyUD {

  val spodFile = "doc/Syntax_queries - SPOD.tsv"
  val spodLines = io.Source.fromFile(spodFile).getLines().map(_.split("\t").toList)
  val spodFields: Map[String, Int] = "id      label   categorie       omschrijving    xpath   link_paqu       UD_query        opmerking       Lassy UD".split("\\s+").zipWithIndex.toMap
  val spodjes = spodLines.map(l => spodFields.mapValues(i => if (l.size > i) l(i) else "")).map(Spodje).drop(1)

  case class Spodje(m: Map[String, String])  {
     val xpath = PaQu_macros.expand(m("xpath")).trim.replaceAll("^//node", "node")
     val udQuery = m("UD_query")
     
     def compare() = if (xpath.trim.nonEmpty && udQuery.trim.nonEmpty) {
       Console.err.println(s"Comparing: $xpath --- $udQuery")
       compareQueries(xpath, udQuery)
     }
  }
  def checkSpodjes = {
    spodjes.foreach(_.compare)
  }


  val pretty =new scala.xml.PrettyPrinter(300, 4)

  object Lassy {
    // eerst basexserver -c PASSWORD en dan admin als password
    def makeQuery(part: String)  = s"""for $$a in //alpino_ds[.//$part]
                                     |             return
                                     |                <result>
                                     |                 {$$a//sentence}
                                     |                  <snippet>{$$a//$part//node[./@word]/ud}</snippet>
                                     |                  <snappet>{for $$w in $$a//$part//node[./@word] return <w>{$$w/@word}</w>}</snappet>
                                     |                </result>""".stripMargin

    def lassyQuery(part: String)  =  {
      runQuery("Enhanced", part, "/tmp/out.xml")
      XML.load("/tmp/out.xml")
    }
/*
  <sentence sentid="dpc-bal-001236-nl-sen.p.10.s.3">Bonaire , Curaçao , Sint Maarten , Sint Eustatius , Saba en Nederland hebben samen gekozen voor nieuwe staatkundige verhoudingen , waar Aruba medewerking aan verleent .</sentence>
 */
    def testQuery(query: String = "node[@rel='pc']"): Set[(String, String)] = {
      val q = makeQuery(query)
      val r = lassyQuery(q)
      val sents = (r \\ "sentence").map(s => (s \ "@sentid").text -> s.text)
      sents.toSet
    }
  }


  object UD {

    val portion_size = 10000
    def lassyUDQueryURL(q: String, start: Int = 0, num: Int = 20): String = s"http://svotmc10.ivdnt.loc/blacklab-server/lassy-groter/hits?first=$start&number=$num&patt=${URLEncoder.encode(q, "UTF-8")}" +
      s"&interface=%7B%22form%22%3A%22search%22%2C%22patternMode%22%3A%22expert%22%7D&outputformat=xml&context=s"

    def queryUD(q: String, start: Int = 0, num: Int = 20): Elem = {
      val url = lassyUDQueryURL(q, start, num)
      val results: Elem = Try(XML.load(url)) match {
        case Success(x) => x
        case Failure(exception) => <exception>
          {exception}
        </exception>
      }
      Console.err.println(s"start: $start, numberOfHits: ${nResults(results)}")
      (results \\ "text").take(2).foreach(x => Console.err.println("... " + x.text))
      results
    }


    def nResults(r: Elem) = Try((r \\ "numberOfHits").text.toInt) match {
      case Success(k) => k
      case _ => 0
    }

    def queryUDAll(q: String, portion: Int = portion_size): Seq[Elem] = {
      Stream.from(0, portion).map(i => {
        queryUD(q, start = i, num = portion)
      }).takeWhile(x => (x \\ "hit").nonEmpty).toList
    }

    def sentenceIds(results: Elem): immutable.Seq[(String, String)] = (results \\ "hit" \\ "inlineTag").filter(i => (i \ "name").text == "s").map(i => (i \\ "sent_id").text -> (i \\ "text").text)

    def sentenceIds(q: String): Set[(String, String)] = queryUDAll(q).flatMap(x => sentenceIds(x)).toSet
  }
  import UD._
  def compareQueries(lassyQuery: String, udQuery: String)  = {
    val lassySents: Set[(String, String)] = Lassy.testQuery(lassyQuery)
    val q =
      """[pos="VERB"]*
        |      --obl|advcl--> [pos="VERB"]
        |         --mark--> [lemma="te"];
        |         !--mark-->[lemma="om"]""".stripMargin

    val LassyUdSents = sentenceIds(udQuery)
    val ids = LassyUdSents.map(_._1)
    val lassyWelUdNiet = lassySents.filter(s => !ids.contains(s._1))
    println(lassyWelUdNiet.size)
    lassyWelUdNiet.foreach(println)
    println(s"${lassyWelUdNiet.size} Lassy:${lassySents.size} UD:${LassyUdSents.size}")
  }

  def main(args: Array[String]): Unit  = {
    checkSpodjes
     compareQueries("node[@rel='pc']", "[] --obl:arg--> []")
  }
}
