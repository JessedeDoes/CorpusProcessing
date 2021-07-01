set schema 'wolkencorpus';
update wolken_all set witness_year_from = null where not witness_year_from ~ '^[0-9]{4}$';
update wolken_all set witness_year_to = null where not witness_year_to ~ '^[0-9]{4}$';
update wolken_all set text_year_to = null where not text_year_to ~ '^[0-9]{4}$';
update wolken_all set text_year_from = null where not text_year_from ~ '^[0-9]{4}$';
update wolken_all set pub_year_from = null where not pub_year_from ~ '^[0-9]{4}$';
update wolken_all set pub_year_to = null where not pub_year_to ~ '^[0-9]{4}$';

alter table wolken_all alter column witness_year_from type integer using witness_year_from::integer;
alter table wolken_all alter column witness_year_to type integer using witness_year_to::integer;
alter table wolken_all alter column text_year_to type integer using text_year_to::integer;
alter table wolken_all alter column text_year_from type integer using text_year_from::integer;
alter table wolken_all alter column pub_year_from type integer using pub_year_from::integer;
alter table wolken_all alter column pub_year_to type integer using pub_year_to::integer;

alter table wolken_all add column genre text[];
update wolken_all set genre=xpath('//*[(local-name()="interpGrp") and (contains(@type,"genre"))]/*[local-name()="interp"]/text()', bibl);
update wolken_all set genre=array_sort_unique(genre)
