package corpusprocessing.gekaapte_brieven
import database.DatabaseUtilities.{Diamond, Select}

object Settings {
  val useXMLFromOldDatabase = true
  val makePretty = false
  val nederlabXLSX = "/mnt/Projecten/Corpora/Historische_Corpora/GekaapteBrieven2021/Database-maart2021-onlineappl/database dump gekaapte_brieven en excels maart 2021/gekaapte_brieven/xslx"
  val nederlabXML = "/mnt/Projecten/Corpora/Historische_Corpora/Nederlab/gekaaptebrieven/"
  val exportDataTo = "/mnt/Projecten/Corpora/Historische_Corpora/GekaapteBrieven2021/Export/Untagged/"
  val extraXMLFromExcel = "/mnt/Projecten/Corpora/Historische_Corpora/GekaapteBrieven2021/ExcelConverted/"
  val brief_data_table = "with_data_plus"


  val brieven_db_config = new database.Configuration(
    name = "gekaapte_briefjes",
    server = "svowdb20.ivdnt.loc",
    database = "brieven_site",
    user = "postgres",
    password = "inl")

  val briefdb = new database.Database(brieven_db_config)
  val makePieterTable = "create table if not exists nederlab_xml (id integer, xml text, metadata text)"
  val pieterLeeg = "delete from nederlab_xml"
  val dropView = "drop view if exists bd cascade"
  val makeView = "create view bd as select id as brid, xml from nederlab_xml "
  val makeArticleTable_deprecated = "create temporary view with_data as select * from brieven_monster_view, bd where bd.brid=brieven_monster_view.brief_id"

  val group_id_with_singletons = "group_id_with_singletons"
  val makeArticleTable =
    s"""create  temporary view with_data
       |as select brieven_monster_view.*, bd.xml,
       |case when "groepID_INT"='' then 'singleton_' || brieven_monster_view.brief_id else  "groepID_INT" end as $group_id_with_singletons
       |from brieven_monster_view, bd
       |where  bd.brid=brieven_monster_view.brief_id""".stripMargin


  val makeArticleTable_2 =
  s"""create  temporary view with_data_2
      |as select
      |   brieven_monster_view.*,
      |   archive_info.date as datering_archive, $brief_data_table.xml, $brief_data_table.excel,
      |   case when "groepID_INT"='' then 'singleton_' || brieven_monster_view.brief_id else  "groepID_INT" end as $group_id_with_singletons
      |from brieven_monster_view, $brief_data_table, archive_info
      |where $brief_data_table.id=brieven_monster_view.brief_id
      |      and brieven_monster_view.brief_id=archive_info.brief_id""".stripMargin

  val preparation_for_nederlab_import = List(makeArticleTable, pieterLeeg, makePieterTable, dropView, makeView, makeArticleTable)

  val articleTable = if (useXMLFromOldDatabase) "with_data_2" else "with_data"


  /*
  create view monster_columns as SELECT column_name, data_type
    FROM information_schema.columns
     where table_name   = 'brieven_monster_view'
       ;

       create view monster_useful_columns as SELECT column_name, data_type
      FROM information_schema.columns
       where table_name   = 'brieven_monster_reduced'
         ;
 create table monster_field_info as select monster_columns.*, '' as description, false as exported, '' as int_metadata_field, '' as comment from monster_columns;
   */

}