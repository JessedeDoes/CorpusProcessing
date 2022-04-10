package corpusprocessing.wnt_definities
import database.DatabaseUtilities.Select
import database._
import utils.{EditDistance, Tokenizer}
object moderniseer {
  val molex = new database.Database(Configuration("x", "svowdb16.ivdnt.loc","gig_pro", "dba", "vercingetorix"))
  val definitie_db = new database.Database(Configuration("y", "svowdb16.ivdnt.loc","gigant_hilex_candidate", "dba", "vercingetorix"))
  lazy val molex_wordforms = molex.slurp(Select(r => r.getString("wordform"), "data.lemmata_en_paradigma_view where lem_keurmerk and wf_keurmerk")).toSet
  lazy val definities = definitie_db.slurp(Select(r =>  (r.getString("id"), r.getString("persistent_id"), r.getString("definitie")), //  r.getBoolean("af") +  ": " + r.getString("portie") + "/" +
    "definities.sample_worktable where type='Modern'  order by portie, af desc"))

  val replacements = List(
    "(e(e)|a(a)|e(e)|o(o)|u(u))([bcdfghjklmnpqrstvwxz][aeiou])" -> "$2$3$4$5$6$7",
    "qu" -> "kw",
    "ae" -> "e",
    "ph" -> "f",
    "sch$" -> "s",
    "(.)sch" -> "$1s",
    "c" -> "k",
    "eau" -> "o",
    "n$" -> "" // GEVAARLIJK
  )

  def blaasop(V: Set[String]): Set[String] = {
    val stap = V.flatMap(w => replacements.map({case (p,r) => w.replaceAll(p,r)})).toSet
    if ((stap diff V).nonEmpty) {
      blaasop(V ++ stap)
    } else V
  }

  def candidates(w: String): Set[String] = {
    if (molex_wordforms.contains(w)) Set() else blaasop(Set(w)).filter(x => molex_wordforms.contains(x))
  }

  def spelZinOm(s: String) = {
    val tokens = Tokenizer.tokenize(s)
    tokens.map(t => {
      val cands = candidates(t.token)
      if (cands.nonEmpty)
        s"${t.leading}<b>[${t.token}->${cands.toList.sortBy(EditDistance.distance(_,t.token)).take(1).mkString("|")}]</b>${t.trailing}" else s"${t.leading}${t.token}${t.trailing}"
    }).mkString(" ").replaceAll(" den ", " <b>[den->de]</b> ")
  }

  def main(args: Array[String]): Unit = {
    println(spelZinOm("geeven maar qualiteit auteau"))
    val s= definities.map{case (i,s,d) => (i, s, spelZinOm(d))}
    //s.filter(true || _._2.contains("->")).foreach(println)
    definitie_db.runStatement("drop table definities.auto")
    definitie_db.runStatement("create table definities.auto (id integer, persistent_id text, definitie text, er_is_iets boolean default false)")
    val b = definitie_db.QueryBatch[(String,String, String)]("insert into definities.auto (id, persistent_id, definitie) VALUES(cast(:id as integer), :persistent_id, :definitie)",
      l => Seq(definitie_db.Binding("id", l._1), definitie_db.Binding("persistent_id", l._2), definitie_db.Binding("definitie", l._3)))

    b.insert(s)
    definitie_db.runStatement("update definities.auto set er_is_iets=true where definitie ~* '<b'")
  }
}
