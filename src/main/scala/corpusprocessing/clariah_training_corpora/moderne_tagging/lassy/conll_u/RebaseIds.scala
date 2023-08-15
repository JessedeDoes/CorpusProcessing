package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

import java.io.{File, PrintWriter}
object RebaseIds {
  def main(args: Array[String]) = {
    val file = "/home/jesse/workspace/UD_DUM-CG/bloeme_frag.conllu"
    val pw = new PrintWriter(file.replaceAll(".conllu", ".rebase.conllu"))
    val parsed = CONLL.parseFile(new File(file))
    parsed.foreach(s => pw.println(s.toCONLL()))
    pw.close()
  }
}
