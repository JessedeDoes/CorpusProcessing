package corpusprocessing.CLVN

import corpusprocessing.metadata.jamaarCLVN

import scala.xml._
import utils.PostProcessXML._
import utils.ProcessFolder

import java.io.PrintWriter
object PostProcessForCobaltje {
  def breakAfterLB(lb: Elem)  = Seq(lb, Text("\n"))

  def splitKomma(e: Elem) = e.copy(child = e.child.map{
    case t: Text => Text(t.text.replaceAll(",([^ ])", ", $1"))
    case x => x
  })

  def addToExpan(e: Elem)  =  {
    p.println(e.text)
    e.copy(label="expan", attributes = e.attributes.append(new UnprefixedAttribute("ana", "#add2expan", Null)))
  }

  def addToExpanIn(e: Elem) = {
      e.copy(child = e.child.map{
        case x: Elem  if
          x.label=="add" &&
            !staatLosIn(e,x) &&
            !(x.text.replaceAll("\\s+", " ")contains(" ")) &&
            ((x \ "@place").isEmpty || (x \ "@place").text == "inline") =>
          p1.println(e)
          addToExpan(x)
        case z => z
      })
  }

  lazy val p = new PrintWriter("/tmp/expan.extra.txt")
  lazy val p1 = new PrintWriter("/tmp/expan.extra.context.txt")

  def staatLosIn(p: Elem, c: Elem): Boolean = {
    var before = ""
    var after = ""
    var seen = false
    p.child.foreach(x => {
      if (x == c) {
        seen = true
      } else {
        if (seen) after  = after + x.text
        else before = before + x.text
      }
    })
    (before.isEmpty || before.replaceAll("\\s+", " ").endsWith(" ")) && (after.isEmpty || after.replaceAll("\\s+ ", " ").startsWith(" "))
  }

  def fix(d:  Elem)  =
    {
      val d1 = updateElement5(d, _.label == "lb", breakAfterLB).asInstanceOf[Elem]
      val d2 = updateElement5(d1, x => true, splitKomma).asInstanceOf[Elem]
      val d3 = updateElement3(d2,
        x => true, addToExpanIn)
      d3
    }

  def process(in: String, out: String)  = {
    val d = XML.load(in)
    val d1 = fix(d)
    val d2 = jamaarCLVN.fix(d1)
    XML.save(out, d2)
  }
  import java.io.File
  val in = new File("/media/jesse/Data/Corpora/CLVN/TEI")
  val out = new File("/media/jesse/Data/Corpora/CLVN/Patched")
  def main(args: Array[String]) = {

    ProcessFolder.processFolder(in,out,process)
    p.close()
    p1.close()
  }
}
