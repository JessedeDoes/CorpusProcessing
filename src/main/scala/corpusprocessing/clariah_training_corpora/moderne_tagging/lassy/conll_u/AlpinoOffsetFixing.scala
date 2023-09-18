package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

import corpusprocessing.GCND.AlpinoAnnotation

import scala.xml._
import utils.PostProcessXML

object AlpinoOffsetFixing {
  def fixOffsets(alpino: Elem,a: AlpinoAnnotation): Elem = {

    val beginIndexSet: Set[String] =
      (alpino \\ "node")
        .filter(x => x.child.isEmpty)
        .flatMap(n => List((n \ "@begin").text))
        .toSet

    val beginIndexMap = beginIndexSet
      .toList
      .sortBy(x => x.toInt)
      .zipWithIndex.toMap.mapValues(_.toString)

    def newBeginOfWord(w: Node): String = {
      val b = (w \ "@begin").text
      beginIndexMap.getOrElse(b, s"index_not_defined:$b for $w")
    }

    if (true) {
      Console.err.println(s"###### Index mapping for ${a.sentenceId} !!!!!!!!")
      Console.err.println(beginIndexSet.toList.sortBy(_.toInt))
      beginIndexMap.toList.sortBy(_._1.toInt).foreach(z => Console.err.println(z))
    }

    def replaceOffsets(n: Node, b: Any, e: Any): Elem = {
      val bOld = (n \ "@begin").text
      val eOld = (n \ "@end").text
      n.asInstanceOf[Elem].copy(attributes = n.attributes.filter(a => !Set("beginx", "endx").contains(a.key))
        .append(new UnprefixedAttribute("newBegin", b.toString, Null))
        .append(new UnprefixedAttribute("newEnd", e.toString, Null))
        .append(new UnprefixedAttribute("originalOffsets", s"$bOld--$eOld", Null)))
    }

    def replaceFinalOffsets(n: Node): Elem = {
      val bNew = (n \ "@newBegin").text
      val eNew = (n \ "@newEnd").text
      n.asInstanceOf[Elem].copy(attributes = n.attributes.filter(a => !Set("begin", "end", "newBegin", "newEnd", "isWord").contains(a.key))
        .append(new UnprefixedAttribute("begin", bNew.toString, Null))
        .append(new UnprefixedAttribute("end",eNew.toString, Null))
        )
    }

    def fixNode(n: Elem): Elem = {
      if (n.child.isEmpty) {
        val b = (n \ "@begin").text
        val newBegin = beginIndexMap.getOrElse(b, "bummertje")
        val newEnd = newBegin.toInt + 1
        replaceOffsets(n, newBegin, newEnd)
      } else {
        val (b, e) = {
          val below = (n \\ "node").filter(_.child.isEmpty)
          val b = if (below.nonEmpty) below.map(w => newBeginOfWord(w).toInt).min
          val e = if (below.nonEmpty) below.map(w => newBeginOfWord(w).toInt).max + 1
          (b, e)
        }
        replaceOffsets(n, b, e)
      }
    }

    val d = PostProcessXML.updateElement3(alpino, x => x.label == "node", fixNode)
    PostProcessXML.updateElement3(d, _.label=="node", replaceFinalOffsets)
  }
}
