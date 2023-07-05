package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

import java.io.PrintWriter
import CONLL.convertAndParseFilesAndKeepAlpinoLink
case class AlpinoWithUD(alpino: AlpinoSentence, ud: UdSentence) {
   lazy val obls = ud.tokens.find(_.DEPREL == "obl")

  def checkOblArg(t: UdToken): UdToken = {
    val id = t.ID
    val node = alpino.wordMap.get(id)
    if (t.DEPREL == "obl" && node.exists(n => n.rel == "pc" || n.parent.exists(_.rel == "pc") || n.parent.flatMap(_.parent).exists(_.rel=="pc"))) {
      t.copy(DEPREL = "obl:arg")
    } else t
  }

  def checkME(t: UdToken): UdToken = {
    val id = t.ID
    val node = alpino.wordMap.get(id)
    if (t.DEPREL == "obl" && node.exists(n => n.rel == "me" || n.parent.exists(_.rel == "me") || n.parent.flatMap(_.parent).exists(_.rel=="me"))) {
      t.copy(DEPREL = "obl:me")
    } else t
  }


  def checkLD(t: UdToken): UdToken = {
    val id = t.ID
    val node = alpino.wordMap.get(id)
    if (t.DEPREL == "obl" && node.exists(n => n.rel == "ld" || n.parent.exists(_.rel == "ld") || n.parent.flatMap(_.parent).exists(_.rel=="ld"))) {
      t.copy(DEPREL = "obl:ld")
    } else t
  }
  def testAligment() = {
    ud.tokens.foreach(t => {
      val at = alpino.wordMap.get(t.ID)
      println(s"${t.ID} ${t.FORM} ---- $at")
    })
  }

  def checkToken(t: UdToken) =  {
    checkLD(checkME(checkOblArg(t)))
  }

  def enrichUD = ud.copy(tokens = ud.tokens.map(checkToken)).patchLines()
  def lookAtObls() = obls.foreach(checkOblArg)


}

object AlpinoWithUD {
  val testData = "/home/jesse/workspace/alud/data"
  val lassyAll = "/mnt/Projecten/Corpora/TrainingDataForTools/LassyKlein/LassySmall//Treebank/"
  val lassyAllAtHome = "/media/jesse/Data/Corpora/LassySmall/Treebank/"

  def main(args: Array[String]): Unit = {
    val pw = new PrintWriter("/tmp/patched.txt")
    convertAndParseFilesAndKeepAlpinoLink(lassyAllAtHome, max = 1000000).foreach(x => {
      val patched = x.enrichUD;
      if (true || patched.tokens.exists(t => Set("obl:me", "obl:arg", "obl:ld").contains(t.DEPREL))) {
        //println(patched.conll)
        pw.println("\n\n" + patched.lines.mkString("\n"))
        pw.flush()
      }
    })
    print("YouHoo ready")
    pw.close()
    System.exit(0)
  }
}
