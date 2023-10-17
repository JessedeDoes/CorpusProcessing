package zeeuws.xml2db

import database.Configuration
import utils.PostProcessXML._

import scala.xml._

object zeeuws_xml_to_db {
   import Classes._
   import Mappings._

   val filename = "/mnt/Projecten/Hercules/DSDD/WZD/Data/definitieve bestanden/selectie_werkfile_definitief_met_var.xml"
   lazy val doc = XML.load(filename)
   lazy val postprocessedDoc = PostProcess.postprocess(doc)


   val c = new Configuration(name="zeelandia", server="svowdb20.ivdnt.loc", user="postgres", password="inl", database = "hercules")
   lazy val db = new database.Database(c)



   import PostProcess._
  def extractFromEntries(): Unit = {
    val entries = (postprocessedDoc \\ "entry").map(Entry)
    val senses = entries.flatMap(_.senses) // .flatMap(_.splitMe)
    val lemmata = senses.map(_.lemma)
    val attestations = senses.flatMap(_.attestations)
    val responses = attestations.map(_.response)
    val keywords = senses.flatMap(_.keywords)

    db.runStatement("set schema 'wzd'")

    insert[Lemma](db,"wzd.wzd_lemmata", fieldMappingLemmata, lemmata)

    // keywords Keyword(lemma_id: String, keyword: String, kw_id: String)

    insert[Keyword](db,"wzd.wzd_keywords", fieldMappingKeywords, keywords)

    // responses   case class Response(keyword_id: String, keyword: String, place: String, isRegion: Boolean)

    insert[Response](db,"wzd.wzd_responses", fieldMappingResponses, responses)

    db.runStatement(
    """create view the_join
        | as select wzd_lemmata.lemma, wzd_lemmata.definition, wzd_lemmata.lemma_id, wzd_keywords.keyword, wzd_keywords.keyword_id, wzd_responses.place,wzd_responses.isregion
        | from  wzd_lemmata, wzd_keywords, wzd_responses
        | where wzd_lemmata.lemma_id=wzd_keywords.lemma_id and wzd_keywords.keyword_id=wzd_responses.keyword_id;""".stripMargin)
  }

  def doList(l: List[String]) = l.foreach(db.runStatement(_))

  def integratie() = {


    db.runStatement("set schema 'wzd'")

    val queries_create = List(
      "create table if not exists wzd.dsdd_lemmata (like integratie.lemmata including all)",
      "create table if not exists  wzd.dsdd_keywords (like integratie.keywords including all)",
      "create table if not exists  wzd.dsdd_union_table (like integratie.union_table including all)",
      "drop table if exists wzd.dsdd_concept_list",
      "create table if not exists  wzd.dsdd_concept_list (like integratie.concept_list including all)",
      "insert into wzd.dsdd_concept_list select * from integratie.concept_list",
      "alter table wzd.dsdd_keywords add column lemma_number integer",
      "alter table wzd.dsdd_keywords add column keyword_normalized text",
      "update wzd_lemmata set lemma_id='WZD.' || lemma_id",
      "update wzd_keywords set keyword_id='WZD.' ||  keyword_id",
      "update wzd_keywords set lemma_id='WZD.' || lemma_id",
      "update wzd_responses set keyword_id='WZD.' || keyword_id",
      "alter table wzd_keywords add column keyword_number serial primary key"
    )

    doList(queries_create)

    val queries_empty = List("delete from dsdd_lemmata", "delete from dsdd_keywords", "delete from dsdd_union_table")

    doList(queries_empty)

    val queries_insert = List(
      "insert into dsdd_lemmata (dictionary, lemma_id, lemma, definition) select distinct 'WZD',  cast(lemma_id as text), lemma, definition from wzd_lemmata",

      """insert into dsdd_keywords (dictionary, lemma_id, keyword_id, keyword, keyword_org, keyword_normalized)
        |select distinct 'WZD', lemma_id, keyword_number, keyword, keyword,keyword_an from wzd_keywords""".stripMargin,

      "update dsdd_keywords set lemma=wzd_lemmata.lemma from wzd_lemmata where  wzd_lemmata.lemma_id = dsdd_keywords.lemma_id;",

      """insert into dsdd_union_table
        (dictionary, lemma, keyword, lemma_id, location_place, location_area)
        select
        'WZD', lemma, keyword, lemma_id,
         case when isregion='false' then place else '' end,
         case when isregion='true' then place else '' end
         from the_join
        """
    )
    doList(queries_insert)
  }


  def main(args: Array[String]) = {

    println("Gemarkeerd als regio:")
    (postprocessedDoc \\ "region").map(_.text).groupBy(x=>x).mapValues(_.size).toList.sorted.foreach(println)

    println("Niet gemarkeerd als regio:")
    (postprocessedDoc \\ "placeName").map(_.text).groupBy(x=>x).mapValues(_.size).toList.sorted.foreach(println)

    XML.save("/mnt/Projecten/Hercules/DSDD/WZD/Data/definitieve bestanden/selectie_werkfile_definitief_met_var_en_regiomarkering.xml", postprocessedDoc)

    extractFromEntries()
    integratie()

    // println(postprocessedDoc)
  }
}