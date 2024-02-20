package dictionaries.anw

import database.Database
import database.DatabaseUtilities.Select

object Settings {

  val anwServer = "172.16.4.27" // anw-db.inl.loc
  val anwTestConfig =  database.Configuration("anw_test", anwServer, "anwwdb_dev", "anwapp", "oaspid", "mariadb")
  lazy val anwTest = new Database(anwTestConfig)

  val anwConfig =  database.Configuration("anw_db", anwServer, "anwwdb", "anwapp", "oaspid", "mariadb")
  lazy val anwDB = new Database(anwConfig)

  val max = 5 // Integer.MAX_VALUE;

  lazy val query = Select(r => Article(r.getString("tekst")), s"artikel order by rand() limit $max")
  lazy val queryTxt = Select(r => r.getString("tekst"), s"artikel order by rand() limit $max")
  lazy val testArticles = anwTest.stream(query)
  lazy val articles = anwDB.stream(query)

  def main(args: Array[String])  = {
    val textTxt = anwDB.slurp(queryTxt)
    textTxt.foreach(println)
  }
}
