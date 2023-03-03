package diamant
import database.Database
import database.Configuration
import database.DatabaseUtilities.Select
case class Synonym(id: String, indexwoord: String, synonym: String, group_id: String)
case class SynConcept(synonym: String, groups: String, count: Int)

object non_unicity {
  val gigantHilexDB = new database.Database(Configuration("x", "svowdb16.ivdnt.loc","gigant_hilex_candidate", "dba", "vercingetorix"))
  val q1 = Select(r => Synonym(r.getString("medpilot_id") , r.getString("indexwoord"), r.getString("synonym"), r.getString("group_id")), "medpilot.medpilot_all_synonyms")
  val q2 = Select(r => SynConcept(r.getString("synonym"), r.getString("groups"), r.getInt("aantal")), "medpilot.grouped_by_synonym")
  lazy val synConcepts = gigantHilexDB.slurp(q2)
  lazy val all_concepts = synConcepts.flatMap(sc => sc.groups.split("] \\[").map(g => g.replaceAll("\\[|\\]", ""))).map(x => s"[$x]").toSet
  lazy val all_syn_concept_pairs = synConcepts.flatMap(sc => sc.groups.split("] \\[").map(g => g.replaceAll("\\[|\\]", ""))).map(x => s"[$x]").toSet
  def main(args: Array[String]): Unit = {
    //synConcepts.foreach(println)
    //all_concepts.foreach(println)
    val nonU_list = all_concepts.toList.map(c => {
      val terms: Set[String] = synConcepts.filter(sc => sc.groups.contains(c)).map(_.synonym).toSet
      val nonU: Int = synConcepts.filter(sc => terms.contains(sc.synonym)).map(sc => if (sc.groups.contains(c)) sc.count-1 else sc.count).sum
      c -> nonU
    }).sortBy(-1 * _._2)
    val average = nonU_list.map(_._2).sum / (nonU_list.size.asInstanceOf[Double])
    println(average)
    nonU_list.take(30).foreach({case (c,count) => println(s"$c\t$count")})
  }
}


/*

View "medpilot.grouped_by_synonym"
Column  |  Type   | Collation | Nullable | Default
---------+---------+-----------+----------+---------
synonym | text    |           |          |
groups  | text    |           |          |
aantal  | integer |           |          |
            Table "medpilot.medpilot_all_synonyms"
      Column      |   Type    | Collation | Nullable | Default
------------------+-----------+-----------+----------+---------
 medpilot_id      | text      |           |          |
 indexwoord       | text      |           |          |
 wdb              | text      |           |          |
 synonym          | text      |           |          |
 match_direction  | text      |           |          |
 uitleg           | text      |           |          |
 sense_id         | text      |           |          |
 definition       | text      |           |          |
 min_year_to      | integer   |           |          |
 dates            | integer[] |           |          |
 indexwoord_match | boolean   |           |          |
 nl_match         | boolean   |           |          |
 be_match         | boolean   |           |          |
 medical_match    | boolean   |           |          |
 syn_match        | boolean   |           |          |
 group_id         | integer   |           |          |




*/