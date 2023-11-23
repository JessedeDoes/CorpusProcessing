create table materialized_checkviews.banlinks (lemma_id integer, modern_lemma text, resource_lemma text, resource_lemma_id integer, link text, page_exists boolean, page_title text, page_title_equals_resource_lemma boolean, page_title_equals_modern_lemma boolean);
\copy materialized_checkviews.banlinks from '/tmp/checkban.tsv'
