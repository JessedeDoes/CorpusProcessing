package corpusprocessing.MNL
import scala.xml._
import utils.PostProcessXML._
import scala.util.matching.Regex._
import scala.util.{Try,Success,Failure}
object SquareBracketThing {
  val brackz = "(\\[|\\])".r
  val open = 0
  val close = 1
  val openTag  = "<hi rend='square_bracket'>"
  val closeTag = "</hi>"
  case class BracketInfo(positions: List[(Int,Int)], closed: Boolean)


  def bracketInfo(isClosed: Boolean, n: Node): (NodeSeq, Boolean) = {
    n match {
      case t: Text =>
        val b0 = if (isClosed) Seq[(Int,Int)]() else Seq(0 -> open)
        val bracks = b0 ++
          (brackz.findAllMatchIn(t.text).map(m =>
          { val b = m.toString()
            val p = m.start
            val s = if (b == "[") open else close
            p -> s
          }))

          val closed = if(bracks.isEmpty) isClosed else (bracks.last._2 == close)

          val hackedText = // speciaal geval: eindigt op '['. Dat niet afsluiten ?
            (if (isClosed) "" else openTag) +
            t.text.replaceAll("\\[", openTag).replaceAll("\\]", closeTag) +
              (if (!closed) closeTag else "")
          //println(hackedText)

        val hackXML = Try(XML.loadString("<x>" + hackedText + "</x>").child) match {
            case Success(z) => z
            case Failure(_) => t
          }
        (hackXML, closed)
      case e: Elem =>  {
        def prolong(b: Boolean, nz: Seq[Node], n: Node) = {
          val (n1,c) = bracketInfo(b, n)
          c  -> (nz ++ n1)
        }
        val z: (Boolean, Seq[Node]) =
          e.child
            .foldLeft(isClosed -> Seq[Node]())({case ((b0,n0), n) => prolong(b0,n0,n)})
        e.copy(child = z._2) -> z._1 }
        case n1: Node => n1 -> isClosed
    }
  }




   def processDocument(d: Elem) = {
     val wrapped = updateElement2(d, _.label == "text",
       x => bracketInfo(true,x)._1)
     wrapped.asInstanceOf[Elem]
   }

  def testDoc = <TEI><text>Hallo [men<hi>ee</hi>r]</text></TEI>

  def main(args: Array[String]): Unit = {
    println(processDocument(testDoc))
  }
}
