package clariah.standoffmarkup
import scala.collection.immutable
import scala.xml._

trait Annotation {
  val begin : Int
  val length : Int
  val end : Int = begin + length
  val typ: String
  val value: Any
  val content: String
}

case class NafToken(node: Node, text: String) extends Annotation
{
  val begin = (node \ "@offset").text.toInt
  val length = (node \ "@length").text.toInt
  override val end =  begin + length
  val id = (node \ "@id").text
  val typ = "token"
  val content = text.substring(begin,end)
  override val value = node.text
  override def toString: String = s"$typ $begin:$end $id ($content)"
}


case class TextUnit(node: Node, text: String) extends Annotation
{
  val begin = (node \ "@offset").text.toInt
  val length = (node \ "@length").text.toInt
  override val end =  begin + length
  val id = (node \ "@id").text
  val typ = node.label
  val content = text.substring(begin,end)
  override val value = node.label
  override def toString: String = s"$typ $begin:$end $id {$content}"
}

case class NAF(document: Elem) {
  lazy val rawText = (document \ "raw").text
  lazy val tokens = ((document \ "text") \ "wf").map(NafToken(_,rawText)).toList
  lazy val textUnits = (document \\ "tunit").map(TextUnit(_,rawText))

  lazy val tUnitMap = textUnits.map(x => x.id ->x).toMap

  // println(tokens)
  def getTextUnit(id: String) = tUnitMap.get(id)

  def tokensIn(tu: TextUnit): immutable.Seq[NafToken] = tokens.filter(
    t => t.begin >= tu.begin && t.end <= tu.end
  )
}

object NAF {
}
