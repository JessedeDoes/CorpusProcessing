create temporary table dimpjes as select lemma_id, modern_lemma from data.lemmata where cast(verkleinwoord as text) ~ '[A-Z]';

select distinct wordform from data.lemmata_en_paradigma_view where lemma_id in (select lemma_id from dimpjes);

