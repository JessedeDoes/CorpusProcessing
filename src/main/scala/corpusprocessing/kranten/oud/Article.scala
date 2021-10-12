package corpusprocessing.kranten.oud


case class Article(record_id: String, kb_id: String, issue: String, subissue: String, page: String, colophon: String, date: String, paper: String, country: String, ccity: String,
                   tekstsoort: String, header: String, subheader: String, text: String, normalized_subheader: String="") {

  lazy val city = if (ccity == "Den Haag") "'s-Gravenhage" else ccity

}


/*
kranten_metadatabewerking_2021=# \d articles_int;
              Table "public.articles_int"
    Column     | Type | Collation | Nullable | Default
---------------+------+-----------+----------+---------
 record_id     | text |           | not null |
 kb_article_id | text |           |          |
 kb_issue      | text |           |          |
 subissue      | text |           |          |
 kb_page       | text |           |          |
 colophon      | text |           |          |
 issue_date    | date |           |          |
 paper_title   | text |           |          |
 land          | text |           |          |
 plaats        | text |           |          |
 tekstsoort    | text |           |          |
 header        | text |           |          |
 subheader     | text |           |          |
 article_text  | text |           |          |
Indexes:
    "articles_int_pkey" PRIMARY KEY, btree (record_id)
 */