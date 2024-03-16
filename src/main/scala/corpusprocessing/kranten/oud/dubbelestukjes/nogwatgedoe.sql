
drop view oud_met_overlap cascade;

create view oud_met_overlap as select * from oude_artikeldump where article_id in (select kb_article_id from overlappings_8);

create view oud_met_overlap_distinct as select distinct article_id, cast(n1 as integer) as n, article from oud_met_overlap;

drop view if exists oud_met_overlap_geplakt;
create view oud_met_overlap_geplakt as select article_id, string_agg(article, '<hr/>' order by n) from oud_met_overlap_distinct group by article_id;

select * from oud_met_overlap_geplakt;
