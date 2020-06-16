package corpusprocessing.onw

import java.io.PrintWriter

import database.{Configuration, Database}
import database.DatabaseUtilities.Select

import scala.collection.immutable
import scala.xml._
import scala.util.{Success, Try}


case class Article(dbid: String,  articleText: String, phase: String, lemma: String)
{
  lazy val articleWithFixedDoctype: String =
    articleText.replaceAll("N:/ONW/Werkstation/Schema/entities.dtd", "/mnt/Projecten/ONW/Werkstation/Schema/entities.dtd")

  lazy val articleElement: Elem = Try[Elem](XML.loadString(articleWithFixedDoctype)) match {
    case Success(x) => x
    case x: Any =>
        Console.err.println(x)
        Console.err.println(articleText)
        <bummer/>
  }

  lazy val xmlid = (articleElement \ "@ID").text

  lazy val senses: NodeSeq = articleElement \\ "Betekenis"
  lazy val onwLemma: String = (articleElement \\ "Lemma_ONW").text
  lazy val mbLemma: String = (articleElement \\ "Lemma_MB").text

  def norm(s: String): String = s.replaceAll("\\s+", " ").trim
  def normText(n: NodeSeq): String = if (n.isEmpty) "" else norm(n.text)
  lazy val definitions: immutable.Seq[String] = senses.map(s =>
    s"$phase\t$dbid\t${normText(s \ "Nr")}\t$onwLemma\t$mbLemma\t${normText(s \\ "Definitie" \ "Tekst")}\t${normText(s \\ "Definitie" \ "vertalingEngels")}\t${normText(s \\ "Definitie" \ "vertalingDuits")}")
}

object onwBetekenissen {
   val onwConfig = Configuration(name="onw", server="onw-db.inl.loc", user="onwapp",
     password="kjwthw", database = "ONW_Woordenboek", driver = "mysql")
   val ONWdb = new Database(onwConfig)
   val query = Select(r =>  Article(
     r.getString("id"),
     r.getString("artikel", Some("CONVERT(CAST(artikel as BINARY) USING utf8) as artikel")),
     r.getString("artikel_fase"),
     r.getString("lemma")),
   "artikelen")

  lazy val articles: Stream[Article] = ONWdb.stream(query)

  def main(args: Array[String]): Unit = {
    val allDefinitions = articles.flatMap(_.definitions)
    val issues = allDefinitions.groupBy(d => d.split("\\t", -1).slice(1,3).toList).filter(_._2.size > 1)
    issues.flatMap(_._2).foreach(Console.err.println(_))
    val out = new PrintWriter("/tmp/onwbet.out")
    allDefinitions.foreach(out.println)
    out.close()
  }
}




// mysql -honw-db.inl.loc -uonwapp -pkjwthw ONW_Woordenboek
//my $Stmt = $db->prepare ("select artikel,id,artikel_fase,laatst_bewerkt, lemma from artikelen_kopie") or die "$DBI::errstr\n";


