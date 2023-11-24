package corpusprocessing.gekaapte_brieven
import database.DatabaseUtilities.{Diamond, Select}

object Settings {
  val nederlabXML = "/mnt/Projecten/Corpora/Historische_Corpora/Nederlab/gekaaptebrieven/"
  val exportDataTo = "/mnt/Projecten/Corpora/Historische_Corpora/GekaapteBrieven2021/Export/Untagged/"

  val krantenconfig = new database.Configuration(
    name = "krantjes",
    server = "svowdb20.ivdnt.loc",
    database = "gekaapte_brieven_v0",
    user = "postgres",
    password = "inl")

  val briefdb = new database.Database(krantenconfig)
  val makePieterTable = "create table if not exists nederlab_xml (id integer, xml text, metadata text)"
  val pieterLeeg = "delete from nederlab_xml"
  val dropView = "drop view if exists bd cascade"
  val makeView = "create view bd as select id as brid, xml from nederlab_xml "
  val makeArticleTable = "create temporary view with_data as select * from brieven_monster_view, bd where bd.brid=brieven_monster_view.brief_id"

  val preparation = List(makeArticleTable, pieterLeeg, makePieterTable, dropView, makeView, makeArticleTable)

  val articleTable = "with_data"
  /*
  create view monster_columns as SELECT column_name, data_type
    FROM information_schema.columns
     where table_name   = 'brieven_monster_view'
       ;
 create table monster_field_info as select monster_columns.*, '' as description, false as exported, '' as int_metadata_field, '' as comment from monster_columns;
   */

}