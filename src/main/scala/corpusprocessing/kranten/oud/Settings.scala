package corpusprocessing.kranten.oud

import database.DatabaseUtilities.Select

object Settings {
  val krantenconfig = new database.Configuration(
    name = "krantjes",
    server = "svowdb20.ivdnt.loc",
    database = "kranten_metadatabewerking_2021",
    user = "postgres",
    password = "inl")

  val krantendb = new database.Database(krantenconfig)

  val sillyQuery = """
                    create temporary table normalized_subheaders as select record_id, string_agg(word, ' ' order by tellertje)
    as normalized_subheader from subheader_words group by record_id;
    create temporary view with_normalized_subheader as
      select articles_int.*, normalized_subheaders.normalized_subheader from
      articles_int left join normalized_subheaders on articles_int.record_id = normalized_subheaders.record_id;
    """

  krantendb.runStatement(sillyQuery)

  lazy val articles = krantendb.iterator(
    Select(r => Article(r.getString("record_id"),
      r.getString("kb_article_id"),
      r.getString("kb_issue"),
      r.getString("subissue"),
      r.getString("kb_page"),
      r.getString("colophon"),
      r.getString("issue_date"),
      r.getString("paper_title"),
      r.getString("land"),
      r.getString("plaats"),
      r.getString("tekstsoort"),
      r.getStringNonNull("header").replaceAll("\\s+", " "),
      r.getStringNonNull("subheader").replaceAll("\\s+", " ").replaceAll("\u00a0"," "),
      r.getString("article_text"),
      r.getStringNonNull("normalized_subheader")
    ), "with_normalized_subheader")
  )
}
