package rdf

import database.{Configuration, Database}
import database.DatabaseUtilities.Select

import scala.collection.immutable
import scala.xml._
import java.text.Normalizer

import utils.EditDistance
object evoke {

  val evoke_config = Configuration("x", "svowdb02","evoke_rdf", "postgres", "inl")
  lazy val evoke_db = new Database(evoke_config)

  case class Sense (resource: String, category: String, label: String)


  val bosworth_toller = XML.loadFile("/data/workspace/btc_anglo_saxon/db/oe_bt.xml")


  lazy val bwMap: Map[String, immutable.Seq[Node]] = (bosworth_toller \\ "entry").flatMap(e =>
    Seq((e \ "@key").text -> e)
  ++ (e \\ ("headwords")).map(h => h.text.replaceAll("-","") -> e)).groupBy(_._1).mapValues(_.map(_._2))


  def approxiMatch(s: String) = {
    bwMap.minBy({case (w,n) => utils.EditDistance.distance(w,s)})._2
  }

  import java.text.Normalizer

  def removeAccents(str: String): String = Normalizer.normalize(str, Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
  def findBTEntries(d: Database): Unit = {
    val senses = d.slurp(Select(r => Sense(r.getString("resource"), r.getString("category"), r.getString("preflabel")), "schema.senses"))
    var missed = 0
    senses.foreach(s => {
      val label = s.label
      val label1 = removeAccents(label).replaceAll("æ", "ae").replaceAll("þ", "th")
      val entries: Set[Node] = Set(label1).flatMap(l => bwMap.getOrElse(l, approxiMatch(l)))
      if (entries.isEmpty) missed = missed+1
      val entryKeys = entries.map(e => (e \ "@key").text)
      val bestDistance = Set(label1).flatMap(l => entryKeys.map(k => EditDistance.distance(k,l))).min
      val firstSense = (entries.head \\ "def").head.text.replaceAll("\\s+", " ").replaceAll("^.*?xxxxxx[.]", "").replaceAll(":--.*", "")
      val firstSenseSubst = firstSense.substring(0, Math.min(50,firstSense.length))
      println(s"${s.label} ${entries.size} $entryKeys ($bestDistance)  [[$firstSenseSubst]]")
    })
    Console.err.println(s"Missed: $missed in ${senses.size}")
  }

  def evokeSpecificProcessing(d: Database, tableName: String)  = {
    val statements = s"""set schema 'schema';
                        |create table hierarchy as select * from $tableName where predicate ~ 'skos.core.broader';
                        |drop table if exists concepts;
                        |create table concepts as select resource from resource_type where type ~ 'LexicalConcept';
                        |alter table concepts add column level integer default null;
                        |alter table concepts add column prefLabel text default null;
                        |alter table concepts add column parent text default null;
                        |alter table concepts add column parentLabel text default null;
                        |alter table concepts add column notation text default null;
                        |update concepts set prefLabel=$tableName.object from $tableName where predicate ~ 'prefLabel' and concepts.resource = $tableName.subject;
                        |update concepts set parent=$tableName.object from $tableName where predicate ~ 'skos.core.broader' and concepts.resource = $tableName.subject;
                        |update concepts set parentLabel=c1.prefLabel from concepts c1 where c1.resource=concepts.parent;
                        |update concepts set notation=$tableName.object from $tableName where predicate ~ 'skos.core.notation' and concepts.resource = $tableName.subject;
                        |update concepts set level=0 where resource in (select subject from $tableName where predicate ~'topConceptOf');
                        |alter table concepts add column ancestors text[] default null;
                        |update concepts set ancestors = array[resource];
                        |alter table concepts add column ancestor_text text default '';
                        |update concepts set ancestor_text = '[' || preflabel || ']';
                        |update concepts set ancestors = array[resource, parent] where parent is not null;
                        |alter table concepts add column senses text[];
                        |alter table concepts add primary key (resource);
                        |update concepts set senses=s.labels from (select category, array_agg(preflabel) labels from senses group by category) s where s.category=concepts.resource;
                        |""".stripMargin.split(";") ++
      (0 to 20).map(i =>
        s"update concepts set level=${i+1} where resource in (select subject from hierarchy where object in (select resource from concepts where level=$i))")  ++
      (1 to 20).map(i => s"update concepts set ancestors = concepts.ancestors || p.ancestors from concepts p where p.resource = concepts.parent and concepts.level=$i") ++
      (1 to 20).map(i => s"update concepts set ancestor_text = concepts.ancestor_text || ' ← ' || p.ancestor_text from concepts p where p.resource = concepts.parent and concepts.level=$i") ++ Seq(
      "update concepts set prefLabel = repeat('→ ',level) || prefLabel;")
    statements.foreach(s => {
      println(s)
      d.runStatement(s)
    })
  }

  def main(args: Array[String]): Unit = {
    findBTEntries(evoke_db)
  }
}
