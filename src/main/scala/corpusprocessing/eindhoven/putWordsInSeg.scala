package corpusprocessing.eindhoven

import scala.xml._

import utils.PostProcessXML._

object putWordsInSeg {

  def removeUselessWhite(n: Seq[(Node,Int)]) =
    n.filter({ case (x,i) =>  !(x.isInstanceOf[Text] && x.text.trim.isEmpty  &&  (i==0 || i == n.size-1)) }).map(
      {
        case (x:Text,i) if (i==0 || i == n.size-1) => (Text(x.text.trim),i)
        case (x,i) => (x,i)}
    )


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

  def doit(in: String, out:String): Unit = {
    val d = XML.load(in)
    val d1 = updateElement(d, _.label == "w", w => wrapContentInSeg(w))
    XML.save(out,d1, "UTF-8")
  }

  def main(args: Array[String]): Unit = {
    utils.ProcessFolder.processFolder(new java.io.File("/mnt/Projecten/corpora/Historische_Corpora/Eindhoven/WordsNotInSeg"),
      new java.io.File("/mnt/Projecten/corpora/Historische_Corpora/Eindhoven/TeIndexeren"), doit)
  }
}
