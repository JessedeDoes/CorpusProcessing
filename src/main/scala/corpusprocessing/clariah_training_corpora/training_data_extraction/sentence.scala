package corpusprocessing.clariah_training_corpora.training_data_extraction

import scala.xml._
import java.io.PrintWriter
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

  def isEmpty: Boolean = ???
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
                        ) extends Sentence {
  override def isEmpty: Boolean =  tokens.isEmpty
}

object Sentence {

  var discardIfStrict = 0
  val logje = new PrintWriter("/tmp/quotationCleaning.log.txt")
  def log(x: String) = { logje.println(x); logje.flush() }
  def getId(n: Node): Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).headOption

  def getWord(x: Node)  =
    if ((x \\ "seg").nonEmpty) (x \\ "seg").text.replaceAll("\\s+", " ").trim
    else if ((x \ "choice").nonEmpty) (x \ "choice" \ "sic").text.trim
    else x.text.trim

  def noPos(p: Node) = (p \ "@pos").text.isEmpty

  def removeStuffBetweenBracketsIfUntagged(tokenElements: Seq[Node]) = {
    val indexedTokenElements = tokenElements.zipWithIndex

    val startBrackets: Seq[(Node,Int)] = indexedTokenElements.filter(x => getWord(x._1).contains("("))
    val bracketPairs: Seq[((Node, Int), (Node, Int))] = startBrackets.map({case (s,i) => {
      val nextBracket: Option[(Node, Int)] = startBrackets.find{case (n,j) => j > i}
      val endBracket: Option[(Node, Int)] = indexedTokenElements.drop(i).find{case (n,j) => getWord(n).contains(")") && !nextBracket.exists{case (_,k) => k < j }}
      (s,i) -> endBracket
    }}).filter(_._2.nonEmpty).map{case (s,e) => (s,e.get)}


    val sentence = tokenElements.map(getWord).mkString(" ")
    val removeIt = bracketPairs.map({case ((s,i), (e,j)) =>
       val slice = tokenElements.slice(i,j+1)
       val part = slice.map(getWord).mkString(" ")
       val posjes = slice.map(x => (x \ "@pos").text)
       val noPos = slice.filter(p => (p \ "@pos").text.isEmpty)
       val emptyness = noPos.size / slice.filter(_.label != "pc").size.toDouble
       val removable = emptyness > 0 // 0.66
       // println(s"Bracketed part [$emptyness, remove=$removable] found: $part  in $sentence")
      (removable,i,j+1)
    }).filter(_._1)

    val untaggedLastWords: Seq[Int] = indexedTokenElements
      .filter(_._1.label=="w")
      .filter{case (w,i) => !indexedTokenElements.exists{case (w1,i1) =>  i1 >= i && !noPos(w1)}}.map(_._2)

    untaggedLastWords.filter(i => !removeIt.exists{case (r,s,e) => i >= s && i < e}).foreach(k => println(s"Untagged last Token: ${getWord(tokenElements(k))}"))

    val cleaner = indexedTokenElements.filter{case (n,i) => !removeIt.exists({case (r,s,e) => r && s <= i && i < e}) && !untaggedLastWords.contains(i)}.map(_._1)
    val cleanedSentence = cleaner.map(getWord).mkString(" ")

    val nopjes = cleaner.filter(x => x.label=="w" && noPos(x))
    val nopPart = nopjes.map(getWord).mkString(" ")


    if (cleaner.size < tokenElements.size || nopjes.nonEmpty) {
      log("########################")
      log(s"Quotation: ${tokenElements.map(getWord).mkString(" ")} ")
      if (nopjes.nonEmpty) {
        discardIfStrict += nopjes.size
        log(s"!!!Nopjes (total $discardIfStrict tokens): $nopPart")
      }
      log(s"Cleaned to: ||$cleanedSentence||\n\n")
    }
    cleaner.toList -> nopPart.isEmpty
  }

  def sentence(s: Node, f: String, extractor: TrainingDataExtraction, partition: Option[Partition] = None): Sentence = {

    def getJoin_n(n: Node) = (n \ "join" \  "@n").text

    val id: Option[String] = getId(s)

    // Console.err.println(s"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ID: $id for $s") ((x \ "choice").nonEmpty)

    val tokenElements: List[Node] =  {
      val all = s.descendant.toList.filter(n => Set("w", "pc").contains(n.label)).map(extractor.transformToken)
      if (extractor.clean_brackets)  { val (cleaned, ok) = removeStuffBetweenBracketsIfUntagged(all)
        if (ok) cleaned else List()
      } else all
    }



    val indexedTokenElements = tokenElements.zipWithIndex
    val tokens: List[String] = tokenElements.map(getWord).toList
    val tags = tokenElements.map(x => (x \ extractor.pos_attribute).headOption.getOrElse(x \ "@type").text.trim).map(t => {
      if (t.toLowerCase().matches("^let([()]*)$")) "PC" else t
    })

    val lemmata = tokenElements.map(x =>  {
      val w = getWord(x)
      val looksLikePunct = w.matches("(\\p{P})+")
      if (x.label == "pc") "" else  {
        val lem = (x \ "@lemma").headOption.map(_.text.trim)
        if (lem.nonEmpty && lem.get.nonEmpty && looksLikePunct) {
          // Console.err.println("HMMMMMMMMMM " + x)
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
      } else {
        if (p.toLowerCase().matches("^let([()]*)$")) "PC" else p
      }
    }

    lazy val tokenGroupIds = tokenElements.map(w => {
      val n = getJoin_n(w)
      if (n.isEmpty) None else Some(n)
    })

    val enhancedTags = indexedTokenElements.map({ case (x, y) => enhancePos(x, y) }).toList
    //val partition = (s \ "@ana").headOption.map(_.text.replaceAll("#", "")).getOrElse("unknown")

    val r =
      BasicSentence(id,
        tokens,
      if (extractor.enhance) enhancedTags else tags.toList.map(extractor.tagMapping),
      lemmata.toList,
        xml_ids,
        tokenGroupIds,
        fileId = f,
        None, // s,
        partition=partition)
    r
  }
}