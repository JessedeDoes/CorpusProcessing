package banstaltig
import database._
object banProcessing {

  def valueClause(fieldName: String) : String = s"""(select entity_id, '$fieldName' as name, cast(field_${fieldName}_value as text) as value,
                    null as target from field_data_field_$fieldName)"""

  def bodyClause(fieldName: String) : String = s"""(select entity_id, '$fieldName' as name, cast(${fieldName}_value as text) as value, null as target from field_data_$fieldName)"""

  def targetClause(fieldName: String) : String = s"""(select entity_id, '$fieldName' as name, null as value, field_${fieldName}_target_id as target from field_data_field_$fieldName)"""



  val clauses = List("continent","endoniem","hoort_bij", "land","synoniemen","zie_ook").map(targetClause) ++
    List("body").map(bodyClause) ++
    List("taal","afkorting","indeling","iso_3166_alpha_2", "iso_3166_alpha_3",
      "iso_3166_numeriek", "map_embed", "opties", "taal", "transcriptie_nl","transcriptie_vl","voertaal","voertalen").map(valueClause)


  val query = s"create temporary table all_fields as ${clauses.mkString( " union ")};"



  val q1 = "drop table if exists bandata.overview; create table bandata.overview as select n1.*, node.title as target_title from (select node.title, node.type, node.language, all_fields.* from node, all_fields where node.nid=all_fields.entity_id) n1 left join node on n1.target=node.nid;"

  val q2 = "drop table if exists bandata.grouped_overview; create table bandata.grouped_overview as select entity_id,max(type) as type,max(language) as language ,max(title) as title, array_agg(name || ':' || value) filter (where value is not null) as values,  array_agg(name || ':' || target || ':' || target_title) filter (where target is not null) as linked_values from bandata.overview group by entity_id;"

  val q3 = "select * from bandata.grouped_overview;";

  val q4 = "alter table bandata.grouped_overview add column is_endoniem boolean default false; update bandata.grouped_overview set is_endoniem=true from bandata.overview where bandata.grouped_overview.entity_id=bandata.overview.target and bandata.overview.name='endoniem';"

  def main(args: Array[String]) = {
    println(query)
    println(q1)
    println(q2)
    println(q3)
    println(q4)
  }

  /*

  field_continent_target_id       int(10) unsigned        NO      MUL     NULL
  field_endoniem_target_id        int(10) unsigned        NO      MUL     NULL
  field_hoort_bij_target_id       int(10) unsigned        NO      MUL     NULL
  field_land_target_id    int(10) unsigned        NO      MUL     NULL
  field_synoniemen_target_id      int(10) unsigned        NO      MUL     NULL
  field_zie_ook_target_id int(10) unsigned        NO      MUL     NULL


  Values hebben we bij:

  body_value      longtext        YES             NULL
  comment_body_value      longtext        YES             NULL
  field_afkorting_value   varchar(5)      YES             NULL
  field_indeling_value    varchar(255)    YES     MUL     NULL
  field_iso_3166_alpha_2_value    varchar(2)      YES             NULL
  field_iso_3166_alpha_3_value    varchar(3)      YES             NULL
  field_iso_3166_numeriek_value   int(11) YES             NULL
  field_map_embed_value   longtext        YES             NULL
  field_opties_value      varchar(255)    YES     MUL     NULL
  field_taal_value        varchar(12)     YES     MUL     NULL
  field_transcriptie_nl_value     varchar(255)    YES             NULL
  field_transcriptie_vl_value     varchar(255)    YES             NULL
  field_voertaal_value    varchar(12)     YES     MUL     NULL
  field_voertalen_value   varchar(12)     YES     MUL     NULL

  */

}
