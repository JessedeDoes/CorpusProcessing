package corpusprocessing.kranten.oud

import database.DatabaseUtilities.Select
import scala.collection.JavaConverters._

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

  val exportQuery_old =
    """\"Krantenmetadata17eeeuwdefintieveversie1-22021nw\" a,
      | (select kb_page, kb_issue, subissue from pages_kb) p,
      | (select kb_issue, subissue, datum_issue, colophon_int,  to_char(datum_issue, 'Day') as weekday from issues_kb ) i
      | where
      |   a.kb_page=p.kb_page and p.kb_issue=i.kb_issue and p.subissue=i.subissue"""

  val exportQuery1664 = "articles_int where cast(issue_date as text) ~ '1664'"
  val exportQuery = "articles_int"
  val exportQuery_geenDubbel = " (select articles_int.* from articles_int, issues_kb_fixed where articles_int.kb_issue=issues_kb_fixed.kb_issue and not dubbel_mag_weg) x" // deze wordt nu gebruik
  val exportQuery_geenDubbelMetWatMeer = " (select articles_int_more.* from articles_int_more, issues_kb_fixed where articles_int_more.kb_issue=issues_kb_fixed.kb_issue and not dubbel_mag_weg) x"
}

/*
Ontdubbeling: update issues_kb_fixed set dubbel_mag_weg=true from issues_kb_fixed i
where i.double_group_id= issues_kb_fixed.double_group_id and (i.text_length > issues_kb_fixed.text_length or (i.text_length = issues_kb_fixed.text_length and i.tellertje > issues_kb_fixed.tellertje));


Hoi Jesse en Katrien,

Tijdens een van m'n zoekacties in het Couranten Corpus viel mij op dat ik helemaal geen resultaten vond voor 1626, 1627, 1638 en 1680. Ik zie in het tabblad 'Expert' dat het niet aan mijn zoekactie lag: voor deze jaartallen staan er geen kranten in het corpus. Voor 1638 ontbreken inderdaad ook kranten in Delpher, maar voor de andere drie jaargangen bevat Delpher wel kranten: 39 uit 1626, 52 uit 1627 en 44 uit 1680. Kan het zijn dat er een vinkje uitstaat, waardoor deze jaargangen, net als 1618, niet in het corpus terecht zijn gekomen?

Hartelijke groeten,
Machteld

paper_title              | issue_date
---------------------------------------+------------
Opregte Leydse courant                | 1698-08-11
Oprechte Haerlemsche courant          | 1697-09-12
Amsterdamse courant                   | 1696-05-01
Oprechte Haerlemsche courant          | 1693-07-23
Oprechte Haerlemsche courant          | 1693-05-09
Oprechte Haerlemsche courant          | 1693-02-19
Amsterdamse courant                   | 1692-06-17
Amsterdamse courant                   | 1692-06-14
Amsterdamse courant                   | 1692-05-31
Oprechte Haerlemsche courant          | 1688-12-23
Amsterdamse courant                   | 1672-08-20
Ordinarisse middel-weeckse courante   | 1665-08-11
Ordinaris dingsdaeghse courante       | 1650-07-19
Courante uyt Italien, Duytslandt, &c. | 1621-12-03
Tĳdinghe uyt verscheyde quartieren    | 1619-06-22
Tĳdinghe uyt verscheyde quartieren    | 1619-02-10
Courante uyt Italien, Duytslandt, &c. | 1618-11-30
Courante uyt Italien, Duytslandt, &c. | 1618-11-23
Courante uyt Italien, Duytslandt, &c. | 1618-11-16
Courante uyt Italien, Duytslandt, &c. | 1618-06-22
Courante uyt Italien, Duytslandt, &c. | 1618-06-15


 */
