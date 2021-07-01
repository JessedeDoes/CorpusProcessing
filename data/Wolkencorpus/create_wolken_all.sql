drop table wolkencorpus.wolken_all;

CREATE TABLE wolkencorpus.wolken_all (
    source_file text,
    pid text,
    corpus text,
    source_id text,
    has_parts boolean,
    date_set text,
    level text,
    title text,
    authors text,
    witness_year_from text,
    witness_year_to text,
    text_year_from text,
    text_year_to text,
    pub_year_from text,
    pub_year_to text,
    bibl xml
);

