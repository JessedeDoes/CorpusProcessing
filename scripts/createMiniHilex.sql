create table lemmata as select * from data.lemmata where persistent_id='M030758';
create table analyzed_wordforms as select a.* from data.analyzed_wordforms a, lemmata l  where l.persistent_id='M030758' and a.lemma_id=l.lemma_id;
create table wordforms as select * from data.wordforms where wordform_id in (select wordform_id from analyzed_wordforms);
create table token_attestations as select * from data.token_attestations where analyzed_wordform_id in (select analyzed_wordform_id from analyzed_wordforms);
create table documents as select * from data.documents where document_id in (select document_id from token_attestations);
create table senses as select * from diamant.senses where lemma_id='M030758';
create table sense_attestations_reloaded as select * from diamant.sense_attestations_reloaded where sense_id in (select persistent_id from senses);
create table synonym_definitions as select * from diamant.synonym_definitions where sense_id in (select persistent_id from senses);
