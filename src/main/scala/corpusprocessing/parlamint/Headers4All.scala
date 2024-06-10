package corpusprocessing.parlamint

import scala.xml._
import utils.PostProcessXML

object setAtt {
  def setAttribute(e: Elem, prefix: String, name: String, value: String): Elem = {
    val a = e.attributes.filter(_.key != name).append(new PrefixedAttribute(prefix, name, value, Null))
    e.copy(attributes = a)
  }

  def setAttribute(e: Elem, name: String, value: String): Elem = {
    val a = e.attributes.filter(_.key != name).append(new UnprefixedAttribute(name, value, Null))
    e.copy(attributes = a)
  }

}

object Headers4All {
   val baseDir = "/media/jesse/Data/ParlaMint/CorpusData/" // "/mnt/Projecten/Corpora/0-Ruwmateriaal/FederaalParlement_v1/linguistic_annotation/complete_corpus_parlamint_v1_trankit/"
   val toDir = baseDir + "headers4all/"
   val mainFile = baseDir + "ParlaMint-BE.ana.xml"

  val parentDir = "../" //  s"file:///$baseDir/"
  val max = Integer.MAX_VALUE
   implicit def f(x: String)  = new java.io.File(x)

  def expandInclude(d: Elem)  = {
    PostProcessXML.updateElement(d, _.label=="include", i => {
      val path = toDir + (i \ "@href").text
      val includedDoc = XML.load(path)
      includedDoc
    })
  }
  def fixFile(in: String, outDir:String) = {
     val d= XML.load(in)
     val includes = (d \ "include").toList

     val d0 = PostProcessXML.updateElement(d, _.label=="teiHeader",
       h => PostProcessXML.updateElement(h, _.label=="include", i => {
         val href = (i \ "@href").text
         println(i)
         val i1 = setAtt.setAttribute(i,"href", parentDir + href)
         println(i1)
         i1
       }))

     //println(d0 \ "teiHeader")
     util.Random.shuffle(includes).take(max).foreach(i => {
       val href = (i \ "@href").text
       val d1 = d0.copy(child = d0.child.filter(_.label!= "include"))
       val d2 = d1.copy(child = d1.child ++ <xi:include href={parentDir  + href}  xmlns:xi="http://www.w3.org/2001/XInclude"/>)
       val d3 = expandInclude(d2)
       XML.save(toDir + new java.io.File(href).getName, d3)
     })
   }

  def main(args: Array[String])  = {
    new java.io.File(toDir).listFiles().foreach(_.delete())
    fixFile(mainFile, toDir)
  }
}
