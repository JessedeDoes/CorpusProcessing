start transaction;
drop table if exists articles_int_extra;

create table articles_int_extra (
        record_id text primary key,
        kb_article_id text,
        kb_issue text,
        subissue text,
        kb_page text,
        colophon text,
        issue_date date,
        paper_title text,
        land text,
        plaats text,
        tekstsoort text,
        header text,
        subheader text,
        article_text text
);

insert into articles_int_extra
select
  a.record_id,
  a."article_scannr_KB",
  a.kb_issue,
  p.subissue,
  a.kb_page,
  trim(i.colophon_int),
  i.datum_issue,
  i.paper_title_int,
  a.land_nw,
  a.plaats_nw,
  case
     when a.tekstsoort_int is not null then lower(a.tekstsoort_int)
     else lower(a.tekstsoort_nw)
  end,
  trim(a.header_int),
  trim(a.subheader_int),
  trim(a.article_text_int)
from
   "Krantenmetadata17eeeuwdefintieveversie1-22021nw" a,
   pages_kb p,
   issues_kb_fixed i
where
  not kan_weg and
  a.article_text_int is not null and
  (not trim(a.article_text_int) ~ '^\s*$') and
  a.kb_page=p.kb_page and
  p.kb_issue=i.kb_issue and
  p.subissue=i.subissue and
  i.issue_handled and (not i.wegermee) and i.datum_issue is not null and not (i.dubbel_mag_weg)
  and not (record_id in (select record_id from commentaar_bewerker where kan_weg or issue_mismatch))
  ;

delete from articles_int_extra where record_id in (select record_id from articles_int);
select paper_title, issue_date, length(article_text) from articles_int_extra order by issue_date desc;
select count(*) from articles_int_extra;
rollback;
-- select count(*) from  "Krantenmetadata17eeeuwdefintieveversie1-22021nw";