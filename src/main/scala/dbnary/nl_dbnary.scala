package dbnary
import database.{Configuration, Database}
import rdf.rdf2db.{createSchema, insertToTable}
import rdf.readRDF.{parseToStatements, parseWithHandler}
import org.openrdf.model.Statement

object nl_dbnary {
  val wiktionary = "/mnt/Projecten/Taalbank/Definitiebank/Data/Dbnary/nl_dbnary_ontolex.ttl"

  val dbConfig = Configuration(name="dbnary", server = "svowdb20.ivdnt.loc", database = "wiktionary", user = "postgres", password = "inl")
  lazy val db = new Database(dbConfig)

  val setje = new collection.mutable.HashSet[Statement]()

  /* create table senses as select t1.subject as entry, t2.subject as sense, t3.object as definition from triples t1,
  triples t2, triples t3 where t1.predicate ~ '#sense' and t2.predicate ~ 'definition' and t3.predicate ~ 'value' and t1.object=t2.subject and t2.object=t3.subject;
  create table senses_distinct as select distinct * from senses;
  create table forms as select t1.subject as entry, t2.object as form from triples t1, triples t2 where t1.predicate ~ 'canonicalForm' and t2.predicate ~ 'written' and t1.object=t2.subject;
 alter table senses_distinct add column lemma text;

  update senses_distinct set lemma=forms.form from forms where senses_distinct.entry=forms.entry;
alter table senses_distinct add column pos text;
update senses_distinct set pos=pos.pos from pos where senses_distinct.entry=pos.entry;
update senses_distinct set pos=regexp_replace(pos,'.*#','');
create table predicates as select predicate, sum(1) as count from triples group by predicate;



create table distinct_pos as select pos, sum(1) as count, '' as mapping from pos group by pos;
create table molex_wiki as select * from (select
   lemma_id, modern_lemma, lemma_gigpos, pos, d.definition,d.sense
   from data.lemmata l left join senses_distinct d on d.lemma=l.modern_lemma) x
   where definition is not null;

    create table molex_wiki_pos_ok as select molex_wiki.* from molex_wiki, distinct_pos where molex_wiki.pos=distinct_pos.pos and molex_wiki.lemma_gigpos ~ distinct_pos.mapping and distinct_pos.mapping != '';
     create table molex_wiki_distinct as select lemma_id, modern_lemma, lemma_gigpos, array_agg(distinct definition) as definitions from molex_wiki_pos_ok group by lemma_id, modern_lemma, lemma_gigpos;
  */

  def main(args: Array[String]): Unit = {
    var N = 0
    val doInsert = true

    parseWithHandler(wiktionary, s => {
      if (!s.getPredicate.toString.contains("kaiko")) N  = N + 1;
      if (setje.size > 50000) {
        Console.err.println("Inserting setje from ... " + setje.head)
        insertToTable(db, "triples", setje.toStream, scratch = false)
        setje.clear()
      }
      if (N % 50000 == 0) {
        Console.err.println(N)
      }
      if (doInsert && !s.getPredicate.toString.contains("kaiko"))  setje.add(s)
      if (false && Set("ontolex", "definition", "skos:example", "lexinfo", "rdf:value").exists(s.getPredicate.toString.contains(_))) {
        Console.err.println(s)
      }
    })
    if (setje.size > 0) {
      Console.err.println("Inserting last portion of setje from ... " + setje.head)
      insertToTable(db, "triples", setje.toStream, scratch = false)
      setje.clear()
    }
    Console.err.println(s"Aantal triples: $N")
  }
}
