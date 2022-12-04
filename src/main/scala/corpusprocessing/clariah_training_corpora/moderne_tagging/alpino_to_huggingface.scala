package corpusprocessing.clariah_training_corpora.moderne_tagging
import scala.xml._
import grouping._
import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write


import java.io.PrintWriter

case class Sentence(
                     id: String,
                     tokens: List[String],
                     tags: List[String],
                     lemmata: List[String],
                     xml_ids: List[String]  = List(),
                     relevances: List[String]  = List(),
                     hilex_pos : List[String]  = List(),
                     file: String = "unknown" // pas op andere dingen hebben dit niet
                   )

case class UdToken(ID: String, FORM: String, LEMMA: String, UPOS: String, XPOS: String, FEATS: String, HEAD: String, DEPREL: String, DEPS: String, MISC: String, sent_id: String, language: String) {
  lazy val tokenId = s"$sent_id.$ID.$language"


  def toXML(sent_id: String, language: String) = {
    val feats = if (FEATS != "_") "|" + FEATS else ""
    <w xml:id={tokenId} pos={UPOS} msd={s"UPosTag=$UPOS$feats"} lemma={LEMMA} ana={s"xpos=$XPOS|misc=$MISC"}>{FORM}</w>
  }
}

case class UdSentence(sent_id: String, language: String, tokens: Seq[UdToken]) {
  def toXML() = <s xml:lang={language} n={sent_id} xml:id={s"$sent_id.$language"}>{tokens.map(_.toXML(sent_id, language))}{linkGrp}</s>
  lazy val n2id = tokens.map(t => t.ID -> t.tokenId).toMap

  lazy val links: Seq[(String, String, String)] = tokens.map(t => {
    val id = n2id(t.ID)
    (t.DEPREL, id,  n2id.getOrElse(t.HEAD, id))
  })

  def head(t: UdToken) = tokens.find(_.ID == t.HEAD)

  def xpos_converted(t: UdToken) = (t.UPOS, t.DEPREL, head(t)) match {
    case ("ADP", "compound:prt", h) => h.map(_.XPOS).getOrElse(t.XPOS) // enzovoorts ...
    case _ => t.XPOS
  }

  lazy val linkGrp = <linkGrp>{links.map(t => <link ana={"ud-syn:" + t._1} target={s"#${t._3} #${t._2}"}/>)}</linkGrp>

  lazy val sent: Sentence = Sentence("", tokens.map(_.FORM).toList, tokens.map(_.XPOS).toList, tokens.map(_.LEMMA).toList) // todo add lemmata
}





object alpino_to_huggingface {

  implicit val formats = DefaultFormats

  def parseFile(f: java.io.File) = {
    val lines = io.Source.fromFile(f).getLines.toStream
    val language = f.getName.replaceAll("_.*","")
    val grouped = groupWithFirst[String](lines, x=> x.startsWith("# sent_id ="))

    val sentences = grouped.flatMap(g => {
      val sent_id = g.find(_.startsWith("# sent_id")).map(_.replaceAll(".*=","").replaceAll("\\s+",""))

      sent_id.map(x => {
        val text =  g.find(_.startsWith("# text")).map(_.replaceAll(".*=","").trim)
        val tokens = g.filter(_.matches("[0-9].*")).map(_.split("\\t").toList).map(l => UdToken(l(0),l(1),l(2),l(3),l(4),l(5),l(6),l(7),l(8),l(9), x, language))
        val sentence = UdSentence(x, language, tokens)
        sentence })
    })
    sentences
  }

  def makeTEI(f: java.io.File, sentences: Seq[UdSentence]) =
    <TEI xmlns="http://www.tei-c.org/ns/1.0" xml:id={f.getName}><text><body><div><ab>{sentences.map(_.toXML())}</ab></div></body></text></TEI>


  def doit(allFiles: Seq[java.io.File]): Unit = {

    val corpus = <teiCorpus xmlns="http://www.tei-c.org/ns/1.0">{allFiles.map(parseFile)}</teiCorpus>
    import java.io.PrintWriter
    def prettyScala(pw: PrintWriter, e: Elem): Unit = {
      val pretty = new scala.xml.PrettyPrinter(Integer.MAX_VALUE, 4)
      pw.write(pretty.format(e))
      pw.close()
    }
    prettyScala(new PrintWriter("corpus.xml"), corpus)
  }

  lazy val allFiles: Seq[java.io.File] = new java.io.File(".").listFiles.filter(_.isDirectory()).toList.flatMap(_.listFiles.toList).filter(_.getName.matches("[a-z][a-z]_pud-ud-test.conllu"))

  def main(args: Array[String]): Unit = {
    val f = new java.io.File(args(0))
    val sents: Seq[Sentence] = parseFile(f).map(_.sent)
    val s1 = sents.zipWithIndex.map({ case (s, i) => s.copy(id = i.toString) })
    val jsons = s1.map(s => write(s))
    val pw = new PrintWriter("/tmp/huggie.json")
    jsons.foreach(pw.println)
    pw.close()
  }
}


/*
 # newdoc id = n01001
 # sent_id = n01001011
 # text = 「米国でデジタルへの移行が大いに進んでいる一方で、権力の平和的な移行は進んでいない」と書いたブログを、オバマ大統領特別補佐コリ・シューマンが月曜日に投稿した。
 # text_en = “While much of the digital transition is unprecedented in the United States, the peaceful transition of power is not,” Obama special assistant Kori Schulman wrote in a blog post Monday.
 1       「      「      PUNCT   補助記号-括弧開 _       2       punct   _       BunsetuPositionType=CONT|LUWBILabel=B|LUWPOS=補助記号-括弧開|SpaceAfter=No|UniDicLemma=「
 2       米国    米国    PROPN   名詞-固有名詞-地名-国   _       10      obl     _       BunsetuPositionType=SEM_HEAD|LUWBILabel=B|LUWPOS=名詞-固有名詞-地名-国|SpaceAfter=No|UniDicLemma=米国
 3       で      で      ADP     助詞-格助詞     _       2       case    _       BunsetuPositionType=SYN_HEAD|LUWBILabel=B|LUWPOS=助詞-格助詞|SpaceAfter=No|UniDicLemma=で
 4       デジタル        デジタル        NOUN    名詞-普通名詞-形状詞可能        _       7       nmod    _       BunsetuPositionType=SEM_HEAD|LUWBILabel=B|LUWPOS=名詞-普通名詞-一般|SpaceAfter=No|UniDicLemma=デジタル



     ID: Word index, integer starting at 1 for each new sentence; may be a range for multiword tokens; may be a decimal number for empty nodes (decimal numbers can be lower than 1 but must be greater than 0).
     FORM: Word form or punctuation symbol.
     LEMMA: Lemma or stem of word form.
     UPOS: Universal part-of-speech tag.
     XPOS: Language-specific part-of-speech tag; underscore if not available.
     FEATS: List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.
     HEAD: Head of the current word, which is either a value of ID or zero (0).
     DEPREL: Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.
     DEPS: Enhanced dependency graph in the form of a list of head-deprel pairs.
     MISC: Any other annotation.
 */
