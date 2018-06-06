package posmapping
import scala.xml._
import java.io.File

object mapMiddelnederlandseTags {

  def updateElement(e: Elem, condition: Elem=>Boolean, f: Elem => Elem):Elem =
  {
    if (condition(e))
      f(e)
    else
      e.copy(child = e.child.map({
        {
          case e1: Elem => updateElement(e1,condition,f)
          case n:Node => n
        }
      }))
  }

  val tagMapping  = scala.io.Source.fromFile("data/getalletjes2cgn.txt").getLines().toStream
    .map(s => s.split("\\t")).map(x => x(0) -> x(1)).toMap

  val morfcodeAttribuut = "@pos"
  val posAttribuut = "msd"

  def updateTag(e: Elem):Elem =
  {
    val morfcodes = (e \\ morfcodeAttribuut).text.split("\\+")
    val cgnTag = morfcodes.map(tagMapping).mkString("+")
    val newAttribute = new UnprefixedAttribute(posAttribuut, cgnTag, Null)
    e.copy(attributes = e.attributes.filter(a => a.key != posAttribuut).append(newAttribute))
  }

  def fixEm(d: Elem):Elem = updateElement(d, _.label=="w", updateTag)

  def fixFile(in: String, out:String) = XML.save(out, fixEm(XML.load(in)),  enc="UTF-8")
  def main(args: Array[String]) = ProcessFolder.processFolder(new File(args(0)), new File(args(1)), fixFile)
}


object ProcessFolder {
  def processFolder(input: File, outputFolder: File, base: (String,String) => Unit): Unit =
  {
    if (!outputFolder.exists())
      outputFolder.mkdir()

    if (input.isDirectory)
    {
      input.listFiles().foreach(f =>
      {
        if (f.isFile && f.getName.endsWith(".xml"))
          processFolder(f, outputFolder, base)
        else if (f.isDirectory)
          processFolder(f, new File(outputFolder + "/" + f.getName), base)
      })
    } else if (input.isFile)
    {
      val outFile = outputFolder + "/" + input.getName()
      base(input.getCanonicalPath, outFile)
    }
  }
}
