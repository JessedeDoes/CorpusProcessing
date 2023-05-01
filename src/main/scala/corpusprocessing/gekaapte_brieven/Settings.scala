package corpusprocessing.gekaapte_brieven
import database.DatabaseUtilities.{Diamond, Select}

object Settings {
  val krantenconfig = new database.Configuration(
    name = "krantjes",
    server = "svowdb20.ivdnt.loc",
    database = "brieven_site",
    user = "postgres",
    password = "inl")

  val briefdb = new database.Database(krantenconfig)
  val makeView = "create view bd as select id as brid, xml from brief_data "
  val makeArticleTable = "create temporary view with_data as select * from brieven_monster_view, bd where bd.brid=brieven_monster_view.brief_id"
  val articleTable = "brieven_monster_view" // with_data"





}