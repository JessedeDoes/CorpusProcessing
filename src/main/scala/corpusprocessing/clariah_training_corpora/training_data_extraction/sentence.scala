package corpusprocessing.clariah_training_corpora.training_data_extraction

import scala.xml._

trait Sentence {
  def sentenceId: Option[String] = None

  def toTSV(addSourceLine: Boolean = false, addGroupIds: Boolean = true): String = {

    this match {
      case s: BasicSentence => {
        val sourceLine = if (addSourceLine) s"#### ${s.fileId} ####\n" else ""
        sourceLine +
          s.tokens.indices.map(i => List(s.tokens(i), s.tags(i), s.lemmata(i), s.token_group_ids(i).getOrElse("")).mkString("\t")).mkString("\n")
      }
    }
  }

  def fileId: String = "unknown"

  def partition: Option[Partition] = None
}



case class BasicSentence(
                          override val sentenceId: Option[String],
                          tokens: List[String],
                          tags: List[String],
                          lemmata: List[String],
                          xml_ids: List[String] = List(),
                          token_group_ids: List[Option[String]]  = List(),
                          override val fileId: String = "unknown",
                          node: Option[Node] = None, // hebben we die echt nodig?
                          override val partition: Option[Partition] = None
                        ) extends Sentence

object Sentence {

  def getId(n: Node): Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).headOption

  def getWord(x: Node)  = if ((x \\ "seg").nonEmpty) (x \\ "seg").text.replaceAll("\\s+", " ")
    else if ((x \ "choice").nonEmpty) (x \ "choice" \ "sic").text
    else x.text.trim

  def sentence(s: Node, f: String, extractor: TrainingDataExtraction, partition: Option[Partition] = None): Sentence = {

    def getJoin_n(n: Node) = (n \ "join" \  "@n").text

    val id: Option[String] = getId(s)

    // Console.err.println(s"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ID: $id for $s") ((x \ "choice").nonEmpty)

    val tokenElements = s.descendant.toList.filter(n => Set("w", "pc").contains(n.label)).map(extractor.transformToken)
    val indexedTokenElements = tokenElements.zipWithIndex
    val tokens = tokenElements.map(getWord)
    val tags = tokenElements.map(x => (x \ extractor.pos_attribute).headOption.getOrElse(x \ "@type").text.trim)
    val lemmata = tokenElements.map(x =>  {
      val w = getWord(x)
      val looksLikePunct = w.matches("(\\p{P})+")
      if (x.label == "pc") "" else  {
        val lem = (x \ "@lemma").headOption.map(_.text.trim)
        if (lem.nonEmpty && lem.get.nonEmpty && looksLikePunct) {
          Console.err.println("HMMMMMMMMMM " + x)
        }
        lem.getOrElse("") }
    } )

    val relevances = tokenElements.map(x => (x \ "@sense-id").nonEmpty).map(x => if (x) "yes" else "no")
    val hilex_pos = tokenElements.map(x => (x \ "@hilex-pos").headOption.map(_.text.trim).getOrElse("unk"))

    //System.err.println(relevances)
    // println(s)
    val xml_ids = tokenElements.map(x => getId(x).getOrElse("no_id_found"))

    // alternatief: doe alleen de delene van een scheidbaar werkwoord

    def enhancePos(w: Node, i: Int) = {
      val p = (w \ "@pos").headOption.getOrElse(w \ "@type").text.trim
      val word = getWord(w)
      if ((w \\ "join").nonEmpty) {

        val n = getJoin_n(w)
        val hasPrev = indexedTokenElements.exists({ case (w, i1) => getJoin_n(w) == n && i1 < i })
        val hasNext = indexedTokenElements.exists({ case (w, i1) => getJoin_n(w) == n && i1 > i })

        val bio =
          (hasPrev, hasNext) match {
            case (true, true) => "i"
            case (true, false) => "f"
            case (false, true) => "b"
            case _ => "o" // this should not happen.....
          }

        val t = p + "_" + bio + {
          val lexicalPos = separable_part_lexical_data.getPos(word)
          if ( (lexicalPos intersect Set("adv", "adp")).nonEmpty) "A" else ""
        }

        t
      } else p
    }

    lazy val tokenGroupIds = tokenElements.map(w => {
      val n = getJoin_n(w)
      if (n.isEmpty) None else Some(n)
    })

    val enhancedTags = indexedTokenElements.map({ case (x, y) => enhancePos(x, y) })
    //val partition = (s \ "@ana").headOption.map(_.text.replaceAll("#", "")).getOrElse("unknown")

    val r =
      BasicSentence(id,
        tokens,
      if (extractor.enhance) enhancedTags else tags.map(extractor.tagMapping),
      lemmata,
        xml_ids,
        tokenGroupIds,
        fileId = f,
        None, // s,
        partition=partition)
    r
  }
}