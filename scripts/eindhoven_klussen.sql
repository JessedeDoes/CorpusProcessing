drop table if exists zezij;
create table zezij (file text, word_id text, mark boolean, pos text, kwic text);
 
\cd 'lexit';
\copy zezij from 'zezij.txt' with delimiter E'\t';
alter table zezij add column comment text;
alter table zezij add column tellertje serial primary key;

drop table if exists adjectief;
create table adjectief (file text, word_id text, mark boolean, pos text, kwic text);

\cd 'lexit';
\copy adjectief from 'adjectiefklus.txt' with delimiter E'\t';
alter table adjectief add column comment text;
alter table adjectief add column tellertje serial primary key;

