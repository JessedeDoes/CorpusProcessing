package corpusprocessing.gekaapte_brieven
import java.io.PrintWriter
import scala.xml._
import Settings._
import database.DatabaseUtilities._
import scala.util.{Success, Try}
object gekaapte_brieven {

  def pieterNaarDB(): Unit = {
     preparation.foreach(briefdb.runStatement(_))
     val f: ((Int, String)) => Seq[briefdb.Binding] =  { case (id, xml) => Seq(briefdb.Binding("id",id), briefdb.Binding("xml", xml)) }
     val b  = briefdb.QueryBatch[(Int,String)]("insert into nederlab_xml (id,xml) values (:id, :xml)", f)
     b.insert(Article.nederDivs.toStream)
  }

  val n_to_export = Integer.MAX_VALUE
  def main(args: Array[String])  = {


    //
    //pieterNaarDB()
    briefdb.runStatement(makeArticleTable)
    briefdb.iterator(briefdb.allRecords(articleTable)).map(x => Article(x)).take(n_to_export).foreach(x => {
      XML.save(exportDataTo + x.m("brief_id") + ".xml", x.prettyXML)
      println(x.m("brief_id"))
    } )
  }
}
