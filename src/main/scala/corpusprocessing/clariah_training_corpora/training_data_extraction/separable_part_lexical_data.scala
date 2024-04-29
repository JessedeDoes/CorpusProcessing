package corpusprocessing.clariah_training_corpora.training_data_extraction
import database.DatabaseUtilities.Select
import database._

import java.io.File
object separable_part_lexical_data {
  lazy val hilex_db = new database.Database(Configuration("y", "svowdb16.ivdnt.loc","gigant_hilex_candidate", "dba", "vercingetorix"))

  lazy val hilexWordforms: Map[String, Set[String]] =
    hilex_db.slurp(Select(r => (r.getString("wordform"), r.getString("lemma_part_of_speech")),
      "data.lemmata_and_paradigma where wordform is not null and lemma_part_of_speech is not null and lemma_part_of_speech!=''"))
      .groupBy(_._1.toLowerCase)
      .mapValues(l => l.map(_._2.toLowerCase()).toSet)

  //  r.getBoolean("af") +  ": " + r.getString("portie") + "/" +

  val dir = "/mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2024/Sep/"
  lazy val bestandjes = new File(dir).listFiles().filter(_.getName.endsWith(".txt"))
  case class Word(word: String, pos: String, lemma: String)
  lazy val tokens: Seq[Word] = io.Source.fromFile(dir + "tokens.tsv").getLines.map(l => l.split("\\t")).map(a => Word(a(0), a(1), a(2))).toList

  val posMap: Map[String, Set[String]] =
    bestandjes.flatMap(f => {
     val pos = f.getName.replaceAll(".txt$", "")
      val words = io.Source.fromFile(f).getLines().toList
      words.map(w => w.replaceAll("\\t.*", "").toLowerCase -> pos)
   }).groupBy(_._1)
     .mapValues(l => l.map(_._2).toSet) ++ hilexWordforms

  def getPos(w: String): Set[String] = posMap.getOrElse(w.toLowerCase(), Set[String]())
  def checkTokens: Unit = tokens.foreach(t => {
    val tags = posMap.getOrElse(t.word.toLowerCase, Set())
    if (tags.isEmpty)
      println(s"$t --> $tags")
  })

  def main(args: Array[String])  = {
    checkTokens
  }
}
