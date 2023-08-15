package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

import java.io.PrintWriter
import scala.collection.immutable
import scala.xml._
object TEI2CONLLU {

  val basicMapping = Map(
    "NOU-C" -> "NOUN",
    "VRB" -> "VERB",
    "CONJ" -> "SCONJ",
    "PD" -> "PRON",
    "ADV" -> "ADV",
    "ADJ" -> "ADJ",
    "ADP" -> "ADP",
    "NUM" -> "NUM",
    "INT" -> "INTJ",
    "NOU-P" -> "PROPN",
    "AA" -> "ADJ",
    "RES" -> "X"
  )

  def getPos(xpos: String) = {
    val mainpos = xpos.replaceAll("\\(.*", "")
    basicMapping.getOrElse(mainpos, "X")
  }

  val separator = "\\+"
   def w2tok(w: Node): Seq[UdToken] =  {
     val lemmata = (w \ "@lemma").text.split(separator).toSeq
     val pos = (w \ "@pos").text.split(separator).toSeq
     val content = (w \ "seg").text
     if (lemmata.size > 1 && lemmata.size == pos.size)
       {
          val subtokens: Seq[UdToken] = lemmata.zip(pos).map{case (l,p) =>  {
            val mainpos = pos(0).replaceAll("\\(.*", "")
            val upos =  basicMapping.getOrElse(mainpos, "X")
            UdToken(FORM=l,LEMMA=l, UPOS=upos,XPOS = p)
          }}
          val multitoken =UdToken(FORM=content, subtokens=subtokens)
          multitoken +: subtokens
       } else
       {
         val mainpos = pos(0).replaceAll("\\(.*", "")
         Seq(UdToken(FORM=content,LEMMA=lemmata.mkString(separator),UPOS = basicMapping.getOrElse(mainpos, "X"), XPOS = pos.mkString(separator)))
       }
   }

  def process(d: Elem): Seq[UdToken] = {
    val tokenList = (d \\ "w").flatMap(w2tok)
    val withTempIds = tokenList.zipWithIndex.map { case (t, i) => t.copy(
      ID = i.toString,
      subtokens = t.subtokens.zipWithIndex.map{case (ts,j) => ts.copy(ID=(i+j+1).toString)}
    )}

    val newIdMap = withTempIds.filter(_.subtokens.isEmpty).zipWithIndex.map { case (t, i) => t.ID -> (i+1).toString }.toMap

    val withIds = withTempIds.map(t => {
      if (t.subtokens.isEmpty) {
        t.copy(ID = newIdMap(t.ID))
      } else t.copy(ID = t.subtokens.map(ts => newIdMap(ts.ID)).mkString("-"))
    })
    withIds
  }

  var sentenceNumber = 1

  def createId(): String = {
    val current = "s" + sentenceNumber
    sentenceNumber = sentenceNumber + 1
    current
  }
  def processWithSentences(d : Elem)  = {
    if ((d \\ "s").isEmpty) process(d).map(_.toCONLL()) else {
      (d \\ "s").flatMap(s => {
        val tokens = process(s.asInstanceOf[Elem])
        val header =
          s"""# sent_id = ${createId()}
             |# text = ${tokens.map(_.FORM).mkString(" ")}""".stripMargin
        Seq("\n", header) ++ tokens.map(_.toCONLL())
      })
    }
  }


  def main(args: Array[String]): Unit = {
    val crmpje  =  "/mnt/Projecten/Corpora/Historische_Corpora/CRM/TEI-tagmapped/Meertens-CRM-1-1.fdc2030f-f46d-3fdf-bd1f-9a4822e29643.xml"
    val bloeme = "/mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/TeIndexeren/2020_07_31/3000.tei.xml"
    val d = XML.load(bloeme)
    val pw = new PrintWriter("/tmp/test.conll")
    processWithSentences(d).foreach(x => pw.println(x))
    pw.close()
  }
}
