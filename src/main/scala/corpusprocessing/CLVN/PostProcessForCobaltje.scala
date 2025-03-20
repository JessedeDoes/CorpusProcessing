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

  def putAllInSeg(w: Elem) = {
    val child = w.child.zipWithIndex
    val segOption = child.filter(_._1.label == "seg").headOption
    segOption.map(seg0 => {
      val seg = seg0.asInstanceOf[(Elem, Int)]
      val beforeSeg = child.filter(c => c._2 < seg._2).map(_._1)
      val afterSeg = child.filter(c => c._2 > seg._2).map(_._1)
      val newSeg = seg._1.copy(child = beforeSeg ++ seg._1.child ++ afterSeg)
      if (beforeSeg.nonEmpty || afterSeg.nonEmpty) {
        // println(s"$seg => $newSeg")
      }
      w.copy(child=newSeg)
    }).getOrElse(w.copy(child= <seg>{w.child}</seg> ))
  }

  def addToExpanIn(e: Elem) = {
      val newChildren  = e.child.map{
        case x: Elem  if
          x.label=="add" &&
            !staatLosIn(e,x) &&
            !(x.text.replaceAll("\\s+", " ")contains(" ")) &&
            ((x \ "@place").isEmpty || (x \ "@place").text == "inline") =>
          p1.println(e)
          addToExpan(x)
        case z => z
      }
      val expannetjes = newChildren.zipWithIndex
        .filter({case (x,i) => x.label == "expan" && (x \ "@ana").text == "#add2expan"}) .map(_._2).toSet

      val trimmed = newChildren.zipWithIndex.map{case (x,i) => {
         x match {
           case Text(t) if expannetjes.contains(i-1) && expannetjes.contains(i+1) => Text(t.trim)
           case Text(t) if expannetjes.contains(i-1) => Text(t.replaceAll("^\\s+", ""))
           case Text(t) if expannetjes.contains(i+1) => Text(t.replaceAll("\\s+$", ""))
           case _ => x
      }}}

    val r:Elem =   e.copy(child = trimmed)
     if (expannetjes.nonEmpty) { println(CLVNUtils.deTokenizeDoc(r)) }
      r



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
    val d3 = updateElement5(d2, _.label == "w", putAllInSeg).asInstanceOf[Elem]
    XML.save(out, d3)
  }

  import java.io.File
  val in = new File(Settings.CLVN)
  val out = new File(Settings.CLNVPatched)

  def main(args: Array[String]) = {

    ProcessFolder.processFolder(in,out,process)
    p.close()
    p1.close()
  }
}


/*
<w pos="" xml:id="w.93" lemma="voorgaand"><seg>voorgaen</seg><expan>de</expan></w>


 */