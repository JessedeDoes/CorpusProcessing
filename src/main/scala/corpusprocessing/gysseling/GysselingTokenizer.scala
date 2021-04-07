package corpusprocessing.gysseling

import utils.Tokenizer

import scala.util.Try
import scala.xml._

object GysselingTokenizer {
  def makeGroupx[T](s: Seq[T], currentGroup:List[T], f: T=>Boolean):Stream[List[T]] =
  {
    if (s.isEmpty) Stream(currentGroup)
    else if (f(s.head))
      Stream.cons(currentGroup, makeGroupx(s.tail, List(s.head), f))
    else
      makeGroupx(s.tail, currentGroup :+ s.head, f)
  }

  def makeGroup[T](s: Seq[T], f: T=>Boolean):Seq[List[T]] =
  {
    makeGroupx[T](s, List.empty, f).filter(_.nonEmpty)
  }

  def removeUselessWhite(n: Seq[(Node,Int)]) =
    n.filter({ case (x,i) =>  !(x.isInstanceOf[Text] && x.text.trim.isEmpty  &&  (i==0 || i == n.size-1)) }).map(
      {
        case (x:Text,i) if (i==0 || i == n.size-1) => (Text(x.text.trim),i)
        case (x,i) => (x,i)}
    )

  def tokenizeOne(w: Elem): NodeSeq = {
    val seg = (w \ "seg")
    if (seg.isEmpty ||
      seg.text.contains("[") ||
      seg.text.contains("]") ||
      (w \ "@pos").text.matches(".*NUM.*") ||
      (w \ "@pos").text.matches(".*PD.*") && w.text.trim.toLowerCase.matches("^([.]?)[ij]([.]?)"))
      Seq(w)
    else {
      val theseg = (w \\ "seg").head.asInstanceOf[Elem]
      val brun = theseg.text.contains("brun")
      if (theseg.child.exists(_.isInstanceOf[Elem])) {
        if (brun) println(s"Tokenizing $seg")
        val z: Seq[(Int, Option[Elem], Node)] = theseg.child.zipWithIndex.toSeq.flatMap(
          { case (n, i) if (i ==0 && n.isInstanceOf[Text]) => {
            val t = Tokenizer.tokenizeOne(n.text)
            if (t.leading.nonEmpty)
              Seq(
                (-1, Some(<pc type="pre">{t.leading}</pc>), Text(t.leading)),
                (0, None, Text(t.token)),
                (0, None, Text(t.trailing))
              )
            else Seq((0, None, n))
          }
          case (n,i) if (i == theseg.child.size -1 && n.isInstanceOf[Text]) => {
            val t = Tokenizer.tokenizeOne(n.text)
            if (t.trailing.nonEmpty)
              Seq(
                (0, None, Text(t.leading)),
                (0, None, Text(t.token)),
                (1, Some(<pc type="post">{t.trailing}</pc>), Text(t.trailing))
              )
            else Seq((0, None, n))
          }
          case (n,i) => Seq((0, None, n))
          }
        )
        if (brun) println(z)
        if (!z.exists(_._1 != 0)) Seq(w) else
        {
          val newSeg = theseg.copy(child = z.filter(_._1 == 0).map(_._3))
          val w1 = w.copy(child = w.child.map(
            {
              case e:Elem if e.label == "seg" => newSeg
              case x => x
            }
          ))
          val pref = z.filter(x => x._1 == -1).map(_._2.get)
          val post = z.filter(x => x._1 == 1).map(_._2.get)
          val r = pref ++ Seq(w1) ++ post
          if (brun) println(w1.text)
          if (brun) println(r)
          r
        }
      } else {
        val token = (w \\ "seg").text.replaceAll("\\s+", " ")
        val t = Tokenizer.tokenizeOne(token)
        val leading = if (t.leading.nonEmpty) Some(<pc type="pre">
          {t.leading}
        </pc>) else None
        val trailing = if (t.trailing.nonEmpty) Some(<pc type="post">
          {t.trailing}
        </pc>) else None
        val word = if (t.token.nonEmpty) Some(w.copy(child = w.child.map(
          { case e: Elem if e.label == "seg" => <seg type="orth">{t.token}</seg>
          case x => x
          }
        ))) else None
        //val x = brievenalsbuit.
        Seq(leading, word, trailing).filter(_.nonEmpty).map(_.get)
      }
    }
  }

  def brack2supplied(w: Elem) = {
    val seg = (w \ "seg")
    if (seg.isEmpty) w else {
      val theseg = (w \\ "seg").head.asInstanceOf[Elem]

      val childXML: String = theseg.child.map(x => x match {
        case t: Text => t.text.replaceAll("\\[", "<supplied>").replaceAll("\\]", "</supplied>")
        case _ => x
      }).mkString("")

      val newSeg = Try(XML.loadString("<seg type='orth'>" + childXML + "</seg>")) match {
        case scala.util.Success(x) => x
        case _ => theseg
      }

      if (theseg.text.contains("["))
      {
        // Console.err.println(s"$theseg => $childXML => $newSeg")
      }

      w.copy(child = w.child.map(
        {
          case e:Elem if e.label == "seg" => newSeg
          case x => x
        }
      ))
    }
  }

  def wrapContentInSeg(w: Elem): Elem =
  {
    val children = w.child.zipWithIndex

    val groupedChildren = makeGroup[(Node,Int)](children, {case (c,i) => !(c.isInstanceOf[Text] || c.label == "hi" || c.label == "supplied" || c.label == "expan" || c.label=="c")})

    val newChildren = groupedChildren.flatMap(
      g => {

        if (g.map(_._1.text).mkString.trim.nonEmpty && g.forall( {case (c,i) => c.isInstanceOf[Text] || c.label == "hi" || c.label == "supplied" || c.label == "expan" || c.label == "c"})) {

          val contents: Seq[Node] = removeUselessWhite(g).map(_._1)
          val ctext:String = contents.text.trim.replaceAll("\\s+", " ")

          <seg type='orth'>{contents}</seg>

        }
        else g.map(_._1)
      }
    )
    w.copy(child = newChildren)
  }
}
