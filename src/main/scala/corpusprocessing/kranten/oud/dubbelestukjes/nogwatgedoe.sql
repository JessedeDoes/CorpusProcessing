
drop view oud_met_overlap if exists cascade;
drop view if exists nieuw_met_overlap cascade;
create view oud_met_overlap as select * from oude_artikeldump where article_id in (select kb_article_id from overlappings_8);



create view oud_met_overlap_distinct as select distinct article_id, cast(n1 as integer) as n, article from oud_met_overlap;

create view nieuw_met_overlap as select
 "article_scannr_KB" as article_id,
 id as n,
 "article_text_CS" as article
from "Krantenmetadata17eeeuwdefintieveversie1-22021nw" where "article_scannr_KB" in (select kb_article_id from overlappings_8);
create view nieuw_met_overlap_distinct as select distinct article_id, cast(n as integer) as n, article from nieuw_met_overlap;


drop view if exists oud_met_overlap_geplakt;
create view oud_met_overlap_geplakt as select article_id, string_agg(article, '<hr/>' order by n) from oud_met_overlap_distinct group by article_id;
select * from oud_met_overlap_geplakt;

drop view if exists length_comparison_2;
create view length_comparison_2 as select
  "article_scannr_KB",
  "id",
  length(string_agg(distinct "article_text_CS",'')) as length_cs,
  sum(length("article_text_int")) as length_int
from  "Krantenmetadata17eeeuwdefintieveversie1-22021nw" group by "article_scannr_KB", "id";

select count(*) from length_comparison_2 where length_cs + 500 < length_int and "article_scannr_KB" in (select kb_article_id from overlappings_8);

