package corpusprocessing.kranten.oud.dubbelestukjes

import corpusprocessing.kranten.oud.dubbelestukjes.OverlapChecking.N

object eenPaarQueries {

  lazy val text_field = "article_text_ontdubbeld"
  lazy val text_field_org = "article_text"
  lazy val overlap_base_name = "overlap_postfix_filtered"
  val check_article_overlap_query =
    s"""create temporary view groupie as
     select kb_article_id,
     array_agg($text_field_org order by record_id) as articles,
     array_agg(record_id order by record_id) as record_ids,
     array_agg(subheader_int order by record_id) as subheaders
     from articles_int
     where kb_issue in (select kb_issue from issues_kb_fixed where not dubbel_mag_weg)
     group by kb_article_id;"""

  val drop_overlap_table = s"drop table ${overlap_base_name}_$N cascade"

  val create_grouped_view =
    s"""create view ${overlap_base_name}_${N}_grouped as select
       |kb_article_id,
       |id1,
       |max(text1),
       |string_agg(text2 ,'<hr>' order by matchOrder),
       |array_agg(id2)
       |from ${overlap_base_name}_$N group by kb_article_id, id1, text1_org""".stripMargin

  val create_overlap_table =
    s"""create table ${overlap_base_name}_$N (kb_article_id text, id1 text, id2 text, n text, example text,
       |text1 text, text2 text,
       |text1_org text, text2_org text,
       |subheader1 text, subheader2 text, matchOrder integer)""".stripMargin

  val addtext1 = s"update ${overlap_base_name}_$N set text1_org = articles_int.$text_field_org , subheader1 = subheader_int from articles_int where cast(record_id as text) = id1"
  val addtext2 = s"update ${overlap_base_name}_$N set text2_org = articles_int.$text_field_org , subheader2 = subheader_int from articles_int where cast(record_id as text) = id2"
}
