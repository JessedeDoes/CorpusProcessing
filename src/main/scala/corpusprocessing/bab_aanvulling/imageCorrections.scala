package corpusprocessing.bab_aanvulling
import scala.xml._
import Settings._
import utils.PostProcessXML
import java.io.{File, PrintWriter}

import sys.process._
import utils.PostProcessXML._

import scala.collection.immutable

object imageCorrections {

  implicit def lift(k: Int): Seq[String] => Int = s => k

  type ifun = Seq[String] => Int

  type fetching = Seq[String] => String


  def length(s: Seq[String]) = s.size
  def last(s: Seq[String]) = s.size -1
  def penultimate(s: Seq[String]) = s.size -2

  def patch(from: ifun, to: ifun, overwrite: Boolean = false): Seq[String] => Seq[String] = inSeq => {
    val x = inSeq(from(inSeq))
    inSeq.patch(to(inSeq), Seq(x) , if (overwrite) 1 else 0)
  }

  def imageFile(f: String) = Settings.imageMap(f).getCanonicalPath

  def sourceFilesAt(baseDir: String): Stream[String] = {
    val cmd = Seq("find", baseDir, "-name", "*.scala", "-type", "f")
    cmd.lineStream
  }

  def swap(from: ifun, to: ifun): Seq[String] => Seq[String]= {
    inSeq => {
      val i0 = inSeq(from(inSeq))
      val i1 = inSeq(to(inSeq))
      inSeq.patch(from(inSeq), Seq(i1), 1).patch(to(inSeq), Seq(i0),1)
    }
  }

  def remove(index: ifun): Seq[String] => Seq[String]= {
    inSeq => {
      val i0 = index(inSeq)
      inSeq.patch(i0,Seq(),1)
    }
  }

  def doRotate(inFile: String, outFile: String, angle: Int): Unit =
  {
    val a = if (angle < 0) 360 - angle else angle
    if (!(new java.io.File(correctedImageDirectory + outFile).exists())) {
      val cmd = Seq("convert", "-rotate", a.toString, inFile, correctedImageDirectory + outFile)
      val res = cmd.lineStream
    }
  }

  def ∀(domain: Seq[String] => Seq[Int], action: Int => Seq[String] => Seq[String]): Seq[String] => Seq[String] = inSeq => {
    val d = domain(inSeq)
    val actions = d.map(action)
    actions.foldLeft(inSeq)( {case (l,f) => f(l)} )
  }


  def rotate(from: ifun, to: ifun, angle: Int, overwrite: Boolean = false): Seq[String] => Seq[String]= {
    inSeq => {
      val inFile = imageFile(inSeq(from(inSeq)))
      val outFile = inSeq(from(inSeq)).replaceAll("\\.([^.]*)$", s".rot_$angle.$$1")
      doRotate(inFile,outFile,angle)
      // res.foreach(println)
      val r: Seq[String] = inSeq.patch(to(inSeq), Seq(outFile), if (overwrite) 1 else 0).asInstanceOf[Seq[String]]
      //println(s"Rotation applied: r=$r")
      r
    }
  }

  def pick(k: Int) : Seq[String] => String = inSeq => { println(s"pick $k from $inSeq"); inSeq(k) }

  implicit def liftInt(k: Int) : fetching = pick(k)
  implicit def liftString(s: String) : fetching = l => s

  def assign(l: Seq[fetching]): Seq[String] => Seq[String] = inSeq => l.map(f => f(inSeq))

  def rotAll(angle: Int): Seq[String] =>  Seq[String] = inSeq =>  {
    inSeq.map(s => {
      val inFile = imageFile(s)
      val outFile = s.replaceAll("\\.([^.]*)$", s".rot_$angle.$$1")
      doRotate(inFile,outFile,angle)
      outFile
    })
  }

  def rot(k: Int, angle: Int): Seq[String] => String = {
    inSeq => {
      val inFile = imageFile(inSeq(k))
      val outFile = inSeq(k).replaceAll("\\.([^.]*)$", s".rot_$angle.$$1")
      doRotate(inFile,outFile,angle)
      // res.foreach(println)
      outFile
    }
  }

  import java.io.FileInputStream
  import java.io.FileOutputStream
  import java.io.IOException

  @throws[IOException]
  private def copyFileUsingStream(source: File, dest: File): Unit = {
    try {
      val is = new java.io.FileInputStream(source)
      val os = new java.io.FileOutputStream(dest)
      val buffer = new Array[Byte](1024)
      var length:Int = 0
      while ( {length = is.read(buffer); length > 0 } ) os.write(buffer, 0, length)
      is.close
      os.close
    } finally {

    }
  }
  //??? //  convert -rotate "90" in.jpg out.jpg
  case class babDocumentPatch(document: Elem, l: Seq[Seq[String] => Seq[String]]) {
    lazy val pid = getPid(document)
    lazy val textFileName: String = (document \\ "div" \ "@n").head.text
    lazy val textFile = Settings.textFileMap(textFileName)

    lazy val images: Seq[String] = ((document \\ "interpGrp").filter(att("type", "images")) \\ "interp").map(x => (x \ "@value").text)

    println(s"######\nIn: $pid $images")

    lazy val movedPatchedImages = {
      val targetDir = correctedImageDirectory + pid + "/"


      val targetText = new File(targetDir + textFileName + ".txt")

      if (!(new File(targetDir)).exists()) {
        (new File(targetDir)).mkdir()
      }

      copyFileUsingStream(textFile, targetText)



      val pw = new PrintWriter(targetDir + "images.txt")
      patchedImageList.foreach(pw.println(_))
      pw.close()

      patchedImageList.map(i => {
        val source = new File(correctedImageDirectory + i)
        if (source.exists()) {
          val target = new File(targetDir + i.replaceAll(".*/", ""))
          source.renameTo(target)
        }
        if (i.contains("rot_")) pid + "/" + i.replaceAll(".*/", "") else i
      }
      )
    }

    lazy val htmlSnippet = <div style="border-styled:solid"><h3>{pid}</h3>{movedPatchedImages.map(i =>
      {
        val i0 = if (i.contains("rot_")) i.replaceAll(".*/","") else s"file://${Settings.babDir}$i"
        <div><h3>{i}</h3><img width="800" src={i0}></img></div>
      })}</div>

    lazy val patchedImageList: Seq[String] = l.foldLeft(images)( {case (l,f) => f(l)} )

    lazy val patchedDocument = {

      println(s"Out: $pid -> $patchedImageList")
      var patchedDoc = PostProcessXML.updateElement(document, e => e.label == "interpGrp" && (e \ "@type").text == "images", e => {
        <interpGrp type="images">{patchedImageList.map(i => <interp value={i}/>)}</interpGrp>
      })
      movedPatchedImages
      patchedDoc
    }
  }

  val actions = imageActions.actions

  val showAll = true
  def main(args: Array[String]): Unit = {
    val list = new PrintWriter(correctedImageDirectory + "list.html")

    imageActions.actions.foreach({case (id, l) =>
      val xml = Settings.processedFileMap.get(id)
      if (xml.nonEmpty) xml.foreach(x => if (l.nonEmpty || showAll) {
        val d = babDocumentPatch(x, l)
        val patchedDoc = d.patchedDocument
        new File(Settings.correctedImageDirectory + id).mkdir()
        XML.save(Settings.correctedImageDirectory + id + "/" + id + ".xml", patchedDoc, "utf-8")
        val view = new PrintWriter(Settings.correctedImageDirectory + id + "/view_images.html" )
        view.println(d.htmlSnippet)
        val snip = (<div><a href={id + "/view_images.html"}>{s"$id: ${d.textFileName}"}</a><br/></div>).toString
        println(snip)
        list.println(snip)
        view.close()
      }) else {
        println(s"Key $id not found!")
      }
    })

    list.close()
  }
}
