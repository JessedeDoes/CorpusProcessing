package corpusprocessing.gekaapte_brieven
import java.io.PrintWriter
import scala.xml._
import Settings._

import scala.util.{Success, Try}
object gekaapte_brieven {

  def main(args: Array[String])  = {
    briefdb.runStatement(makeView)
    briefdb.runStatement(makeArticleTable)

    briefdb.iterator(briefdb.allRecords(articleTable)).map(x => Article(x)).take(3).foreach(x => println(x.xml))
  }
}
