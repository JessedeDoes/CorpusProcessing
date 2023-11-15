package corpusprocessing.clariah_training_corpora.training_data_extraction

import scala.xml._

trait Sentence {
  def id: Option[String] = None

  def toTSV(addSourceLine: Boolean = false): String = {

    this match {
      case s: BasicSentence => {
        val sourceLine = if (addSourceLine) s"#### ${s.file} ####\n" else ""
        sourceLine +
          s.tokens.indices.map(i => List(s.tokens(i), s.tags(i), s.lemmata(i)).mkString("\t")).mkString("\n")
      }
      case s: PimpedSentence => {
        val sourceLine = if (addSourceLine) s"#### ${s.file} ####\n" else ""
        sourceLine +
          s.tokens.indices.map(i => List(s.tokens(i), s.tags(i), s.lemmata(i)).mkString("\t")).mkString("\n")
      }
    }
  }

  def file: String = "unknown"
}

case class PimpedSentence(
                           override val id: Option[String],
                           tokens: List[String],
                           tags: List[String],
                           lemmata: List[String],
                           xml_ids: List[String] = List(),
                           relevances: List[String] = List(),
                           hilex_pos: List[String] = List(),
                           override val file: String = "unknown",
                           partition: String = "unknown" // pas op andere dingen hebben dit niet
                         ) extends Sentence

case class BasicSentence(
                          override val id: Option[String],
                          tokens: List[String],
                          tags: List[String],
                          lemmata: List[String],
                          xml_ids: List[String] = List(),
                          override val file: String = "unknown",
                        ) extends Sentence

object Sentence {

  def getId(n: Node): Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).headOption

  def sentence(s: Node, f: String, extractor: extract_training_data_trait): Sentence = {

    def getN(n: Node) = (n \ "@n").text

    val id: Option[String] = getId(s)

    // Console.err.println(s"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ID: $id for $s") ((x \ "choice").nonEmpty)

    val tokenElements = s.descendant.toList.filter(n => Set("w", "pc").contains(n.label)).map(extractor.transformToken)
    val indexedTokenElements = tokenElements.zipWithIndex
    val tokens =
      tokenElements.map(x =>
        if ((x \\ "seg").nonEmpty) (x \\ "seg").text.replaceAll("\\s+", " ")
        else if ((x \ "choice").nonEmpty) (x \ "choice" \ "sic").text
        else x.text.trim)
    val tags = tokenElements.map(x => (x \ extractor.pos_attribute).headOption.getOrElse(x \ "@type").text.trim)
    val lemmata = tokenElements.map(x => (x \ "@lemma").headOption.map(_.text.trim).getOrElse(""))
    val relevances = tokenElements.map(x => (x \ "@sense-id").nonEmpty).map(x => if (x) "yes" else "no")
    val hilex_pos = tokenElements.map(x => (x \ "@hilex-pos").headOption.map(_.text.trim).getOrElse("unk"))

    //System.err.println(relevances)

    val xml_ids = tokenElements.map(x => getId(x).getOrElse("no_id_found"))

    def enhancePos(w: Node, i: Int) = {
      val p = (w \ "@pos").headOption.getOrElse(w \ "@type").text.trim
      if ((w \ "@type").text == "multiw") {
        println(w)
        val n = getN(w)
        val hasPrev = indexedTokenElements.exists({ case (w, i1) => getN(w) == n && i1 < i })
        val hasNext = indexedTokenElements.exists({ case (w, i1) => getN(w) == n && i1 > i })

        val bio =
          (hasPrev, hasNext) match {
            case (true, true) => "i"
            case (true, false) => "f"
            case (false, true) => "b"
            case _ => "o"
          }
        val t = p + "_" + bio
        println(t)
        t
      } else p
    }

    val enhancedTags = indexedTokenElements.map({ case (x, y) => enhancePos(x, y) })
    val partition = (s \ "@ana").headOption.map(_.text.replaceAll("#", "")).getOrElse("unknown")

    // println(s.asInstanceOf[Elem].copy(child=Seq()))
    // println(s.attributes.toString + "->" + partition)

    val r = if (extractor.addStuff)
      PimpedSentence(id, tokens, if (extractor.enhance) enhancedTags else tags.map(extractor.tagMapping), lemmata, xml_ids, file = f, relevances = relevances, hilex_pos = hilex_pos, partition = partition)
    else BasicSentence(id, tokens, if (extractor.enhance) enhancedTags else tags.map(extractor.tagMapping), lemmata, xml_ids, file = f)
    r
  }
}