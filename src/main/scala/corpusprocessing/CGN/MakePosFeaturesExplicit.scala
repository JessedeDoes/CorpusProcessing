package corpusprocessing.CGN

import java.io.File

import folia.FoliaToCoNLL_U.cgnParse
import folia.entityReplacement
import utils.{PostProcessXML, ProcessFolder}

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
    val head = t.pos
    val featureTags = t.features.filter(_.name != "pos").map(f => <feat subset={f.name} class={f.value}/>)
    e.copy(child = e.child ++ featureTags, attributes=e.attributes.append(new UnprefixedAttribute("head", head, Null)))
  }

  def updateDocument(d: Elem) =
  {
    val d1 = PostProcessXML.updateElement(d, _.label=="pos", makePosFeatures)
    PostProcessXML.updateElement3(d1, x => true, unescapeCGN)
  }

  def replaceAttributeValue(a: MetaData, v: String): Attribute =
  {
    a match {
      case p: PrefixedAttribute => new PrefixedAttribute(pre = p.pre, key=a.key, v, Null)
      case u: UnprefixedAttribute => new UnprefixedAttribute(a.key, v,Null)
      case x: Any => new UnprefixedAttribute(a.key, v,Null)
    }
  }

  def unescapeCGN(e: Elem): Elem =
  {
    val newChildren = e.child.map({
      case t: Text => Text(entityReplacement.replace(t.text))
      case x:Any => x })

    val newMeta = e.attributes.filter(x => false)

    val newAttributes =e.attributes.map(a => replaceAttributeValue(a, entityReplacement.replace(a.value.text)))

    val zz = newAttributes.foldLeft(newMeta)({case (z,a) => z.append(a)})

    e.copy(child = newChildren, attributes =  zz)
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
