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
  val article_table = "articles_int"
  val sillyQuery = s"""
                    create temporary table normalized_subheaders as select record_id, string_agg(word, ' ' order by tellertje)
    as normalized_subheader from subheader_words group by record_id;
    create temporary view with_normalized_subheader as
      select $article_table.*, normalized_subheaders.normalized_subheader from
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
      r.getString("plaats").replaceAll("\\s+", " ").trim,
      r.getString("tekstsoort"),
      r.getStringNonNull("header").replaceAll("\\s+", " "),
      r.getStringNonNull("subheader").replaceAll("\\s+", " ").replaceAll("\u00a0"," "),
      r.getString("article_text"),
      r.getStringNonNull("normalized_subheader")
    ), "with_normalized_subheader")
  )
}

/*
Ontdubbeling: update issues_kb_fixed set dubbel_mag_weg=true from issues_kb_fixed i
where i.double_group_id= issues_kb_fixed.double_group_id and (i.text_length > issues_kb_fixed.text_length or (i.text_length = issues_kb_fixed.text_length and i.tellertje > issues_kb_fixed.tellertje));


Hoi Jesse en Katrien,

Tijdens een van m'n zoekacties in het Couranten Corpus viel mij op dat ik helemaal geen resultaten vond voor 1626, 1627, 1638 en 1680. Ik zie in het tabblad 'Expert' dat het niet aan mijn zoekactie lag: voor deze jaartallen staan er geen kranten in het corpus. Voor 1638 ontbreken inderdaad ook kranten in Delpher, maar voor de andere drie jaargangen bevat Delpher wel kranten: 39 uit 1626, 52 uit 1627 en 44 uit 1680. Kan het zijn dat er een vinkje uitstaat, waardoor deze jaargangen, net als 1618, niet in het corpus terecht zijn gekomen?

Hartelijke groeten,
Machteld

 */
