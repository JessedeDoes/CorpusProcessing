start transaction;
drop table if exists colophons.colophons cascade;
drop view if exists articles_int_more_more;
create table colophons.colophons (like articles_int_more);
insert into colophons.colophons
(
  paper_title,
  kb_issue,
  subissue,
  header_int,
  issue_date,
  tekstsoort_int,
  article_text,
  record_id
)
select
  paper_title_int,
  kb_issue,
  subissue,
  'Colofon',
  datum_issue,
  'colofon',
  colophon_int,
  500000 + tellertje
from issues_kb_fixed;

create view articles_int_more_more as (select * from articles_int_more) union (select * from colophons.colophons where article_text is not null and article_text != '');
commit;

