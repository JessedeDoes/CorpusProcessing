package folia
import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import folia.TestSimplify._
import posmapping.{CGNTagset, Tag}
import utils.{PostProcessXML, ProcessFolder}
import scala.util.{Try,Success,Failure}
import FoliaToCoNLL_U.cgnParse
import java.io.File
import scala.xml._
/*
                     <pos set="http://rdf.ivdnt.org/pos/cgn-eindhoven"
                          class="ADJ(vrij,basis,zonder)"
                          head="ADJ">
                        <feat subset="positie" class="vrij"/>
                        <feat subset="graad" class="basis"/>
                        <feat subset="buiging" class="zonder"/>
                     </pos>

 */
object MakePosFeaturesExplicit {

  def makePosFeatures(e: Elem): Elem =
  {
    val p = (e \ "@class").text
    val t = cgnParse(p)
    val featureTags = t.features.map(f => <feat class={f.value} subset={f.name}/>)
    e.copy(child = e.child ++ featureTags)
  }

  def updateDocument(d: Elem) =
  {
    PostProcessXML.updateElement(d, _.label=="pos", makePosFeatures)
  }

  def doOneFile(fIn: String, fout: String): Unit =
  {
    val d1 = updateDocument(XML.load(fIn))
    XML.save(fout, d1, "UTF-8")
  }

  def main(args: Array[String]): Unit = {
    val (sourceDir,targetDir) = (args(0), args(1))

    ProcessFolder.processFolder(new File(sourceDir), new File(targetDir), doOneFile )
  }
}
