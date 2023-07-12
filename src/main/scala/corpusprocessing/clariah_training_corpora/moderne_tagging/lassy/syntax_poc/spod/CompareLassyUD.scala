package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc.spod

import basex.QueryExample.runQuery

import java.net.URLEncoder
import scala.collection.immutable
import scala.xml._
import scala.util.{Try,Success,Failure}
/*
http://svotmc10.ivdnt.loc/corpus-frontend/lassy-groter/search/hits?first=0&number=100&patt=${}&interface=%7B%22form%22%3A%22search%22%2C%22patternMode%22%3A%22expert%22%7D
 */
object CompareLassyUD {
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
      runQuery("LassyEnhanced", part, "/tmp/out.xml")
      XML.load("/tmp/out.xml")
    }
/*
  <sentence sentid="dpc-bal-001236-nl-sen.p.10.s.3">Bonaire , Curaçao , Sint Maarten , Sint Eustatius , Saba en Nederland hebben samen gekozen voor nieuwe staatkundige verhoudingen , waar Aruba medewerking aan verleent .</sentence>
 */
    def testPC(): Set[(String, String)] = {
      val q = makeQuery("node[@rel='pc']")
      val r = lassyQuery(q)
      val sents = (r \\ "sentence").map(s => (s \ "@sentid").text -> s.text)
      sents.toSet
    }
  }


  object UD {
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

    def queryUDAll(q: String, portion: Int = 500): Seq[Elem] = {
      Stream.from(0, portion).map(i => {
        queryUD(q, start = i, num = portion)
      }).takeWhile(x => (x \\ "hit").nonEmpty).toList
    }

    def sentenceIds(results: Elem): immutable.Seq[(String, String)] = (results \\ "hit" \\ "inlineTag").filter(i => (i \ "name").text == "s").map(i => (i \\ "sent_id").text -> (i \\ "text").text)

    def sentenceIds(q: String): Set[(String, String)] = queryUDAll(q).flatMap(x => sentenceIds(x)).toSet
  }

  import UD._
  def main(args: Array[String]): Unit  = {

    val lassySents: Set[(String, String)] = Lassy.testPC()
    val q = """[pos="VERB"]*
              |      --obl|advcl--> [pos="VERB"]
              |         --mark--> [lemma="te"];
              |         !--mark-->[lemma="om"]""".stripMargin
    val LassyUdSents = sentenceIds("[] --obl:arg--> []")
    val ids = LassyUdSents.map(_._1)
    val lassyWelUdNiet = lassySents.filter(s => !ids.contains(s._1))
    println(lassyWelUdNiet.size)
    lassyWelUdNiet.foreach(println)
    println(s"${lassyWelUdNiet.size} Lassy:${lassySents.size} UD:${LassyUdSents.size}")
  }
}
