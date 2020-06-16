package corpusprocessing.onw
import scala.xml._
import java.io.File

import utils.PostProcessXML._

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object splitONWTagsInFeatures {
  //val tagset = CHNStyleTags



  val nietgevonden = "/home/jesse/workspace/Hilex/" + "data/ONW/niet_gevonden_in_online_versie.txt"

  def readTSV(f: String): Stream[Map[String, String]] =
  {
    val lines = io.Source.fromFile(f).getLines.map(_.split("\t",-1).toList).toStream

    val header = lines.head.map(_.trim.replaceAll("\\s+","").replaceAll(".*taxonID","taxonID"))
    //println(header.head)
    val rows = lines.map(
      l => l.indices.map(i => header(i) -> l(i)).toMap
    )
    //rows.foreach(r => println(r.keySet))
    //rows.filter(_.keySet.contains("taxonID")).foreach(m => println(m.get("taxonID")))
    rows
  }

  val badIds = readTSV(nietgevonden).map(_("id")).toSet


  val articlePatch = Map(1854 -> 5285, 4979 -> 5315,  2375->2376, 4987-> 2977, 5047 -> 5617,  4609 -> 1323, 5047 -> 3208)

  def patchArticle(f: Elem) = {
    val id = f.text.trim
    val patchedId = Try (
      {
        val i = id.toInt
        val pid = articlePatch.getOrElse(i, i)
        if (badIds.contains(pid.toString)) {
          //Console.err.println(s"Dropping link $f")
          None
        }
        else Some(pid)
      }
    ) match {
      case Success(x) => x
      case _ => {
        //Console.err.println(s"Dropping link $f")
        None
      }
    }

    patchedId.map(p => <f name="article">{p}</f>).getOrElse(Seq())
  }

  def patchArticleLinks(d: Elem) = updateElement2(d, e=> e.label == "f" && (e \ "@name").text == "article", patchArticle).head.asInstanceOf[Elem]

  /*
  1854	not_found_in_db  5285
4979	not_found_in_db  5315
4979	not_found_in_db 5315
4979	not_found_in_db 5315
4979	not_found_in_db 5315
4979	not_found_in_db 5315
4979	not_found_in_db 5315
2375	not_found_in_db 2376
4555	not_found_in_db  4555
4987	not_found_in_db  2977
5047	not_found_in_db  5617
4609	not_found_in_db 1323

   */


  def Ѧ(n:String, v: String) = new UnprefixedAttribute(n,v,Null)

  def replaceAtt(m: MetaData, name: String, value: String): MetaData = m.filter(a => a.key != name).append(Ѧ(name, value))

  def padYear(s: String) = {
    val s1 = s.trim
    if (s1.matches("^[0-9]{1,4}$")) {
      val s2 = (0 until (4 - s.length)).map(x => "0").mkString("") + s1
      //Console.err.println(s"$s1 --> $s2")
      s2
    }
    else s1
  }

  def padYears(grp: Elem) = {
     val r = grp.copy(child = grp.child.map(cleanText(padYear)))
     // if ((r \ "@type").text.toLowerCase().contains("year"))  Console.err.println(r)
    r
  }

  def splitFS(fs: Elem, lemma: String): Seq[Elem] =
  {
    val fs1 = fs.copy(
      attributes = replaceAtt(fs.attributes, "type", "chnpos"),
      child = fs.child.flatMap({
        case e: Elem if e.label == "f" && (e \ "@name").text.startsWith("pos.")  => {
          val newName = (e \ "@name").text.replaceAll("^pos.","")
          e.copy(attributes = replaceAtt(e.attributes, "name", newName)) }
        case _ => Seq()
      }) ++ Seq(<f name="lemma">{lemma}</f>)
    )
    Seq(fs,fs1)
  }

  /*
  \p{P} or \p{Punctuation}: any kind of punctuation character.
\p{Pd} or \p{Dash_Punctuation}: any kind of hyphen or dash.
\p{Ps} or \p{Open_Punctuation}: any kind of opening bracket.
\p{Pe} or \p{Close_Punctuation}: any kind of closing bracket.
\p{Pi} or \p{Initial_Punctuation}: any kind of opening quote.
\p{Pf} or \p{Final_Punctuation}: any kind of closing quote.
\p{Pc} or \p{Connector_Punctuation}: a punctuation character such as an underscore that
   */

  def punctuationType(w1: String)  =
  {
    val word = w1.trim
    val forceSpaceBefore = word.matches("^\\p{Ps}") || word.matches("^\\p{Pi}")
    val noSpaceBefore = word.matches("\\p{Pe}$") || word.matches("\\p{Pf}$")  || word.matches("^[,:.;!?]")
    val spaceAfter = noSpaceBefore
    val typ = if (noSpaceBefore) "post" else if (forceSpaceBefore) "pre" else "floating"
    typ
  }

  def updateTag(w: Elem):NodeSeq =
  {
    val word = (w \ "seg" ++ w \ "choice" \ "sic").text

    if (word.trim.matches("^\\p{P}+$")) {
      val typ = punctuationType(word)
      return <pc type={typ}>{word}</pc>
    }

    val lemma = (w \ "@lemma").text
    val lemmata = lemma.split("\\+").toSeq
    val fsjes = (w \ "fs").zip(lemmata).flatMap(
      {case (fs, l) =>  splitFS(fs.asInstanceOf[Elem], l)}
    )
    val msd = (w \ "@msd").text
    val multimainpos = msd.replaceAll("\\(.*?\\)", "")
    val multimainattribute = new UnprefixedAttribute("groupingMainPos", multimainpos, Null)
    val newChild = w.child.filter(_.label != "fs") ++ fsjes
    // Console.err.println(newChild)

    if (word.contains("##"))
      {
        val z: Regex = new scala.util.matching.Regex("##([0-9a-z]+)")
        val n = z.findFirstMatchIn(word).get.group(1)
        Settings.makePB(n)
      } else {
      val dinges = w.descendant.filter(_.isInstanceOf[Text]).map(_.text).mkString(" ")
      (if (dinges.contains("##"))
      {
        Console.err.println("Page number found in: " + w)
        val z: Regex = new scala.util.matching.Regex("##([0-9a-z]+)")
        val n = z.findFirstMatchIn(dinges).get.group(1)
        Console.err.println("Page number is " + n)
          Seq(Settings.makePB(n))
      } else Seq()) ++ Seq(w.copy(child = newChild, attributes=w.attributes.append(multimainattribute)))
    }
  }

  val noNo: String = new Character(133).toString

  def cleanText(f: String => String)(n: Node): Node =
  {
    n match {
      case e: Elem => {
        e.copy(child = e.child.map(cleanText(f)))
      }
      case t: Text => Text(f(t.text))
      case _ => n
    }
  }

  def updateTags(d: Elem):Node =
  {
    updateElement5(d, _.label=="w", updateTag).head
  }

  def doFile(f1: String, f2: String) =
  {
    val d = XML.load(f1)
    val d1 = updateTags(d)
    val d2 = updateElement(d1.asInstanceOf[Elem], x => x.label=="interpGrp" && (x \ "@type").text.toLowerCase.contains("year"), padYears )
    val d3 = patchArticleLinks(d2)
    XML.save(f2,cleanText(s => s.replaceAll(noNo, " ").replaceAll("p?##[0-9a-z]+",""))(d3.asInstanceOf[Elem]),"UTF-8")
  }

  def main(args: Array[String]): Unit = {
    utils.ProcessFolder.processFolder(new File(Settings.tagsplitSourceDir), new File(Settings.tagsplitTargetDir), doFile)
  }
}

/*
anbranton          AANBERNEN     anabrennen      zw.ww.trans.    1854      not_found_in_db  5285
anderes               ANDER andar (I)              znw.      4979      not_found_in_db  5315
andera ANDER andar (I)              znw.      4979      not_found_in_db 5315
ander    ANDER andar (I)              znw.      4979      not_found_in_db 5315
anderem             ANDER andar (I)              znw.      4979      not_found_in_db 5315
anderen              ANDER andar (I)              znw.      4979      not_found_in_db 5315
andere ANDER andar (I)              znw.      4979      not_found_in_db 5315
einander             EEN+ANDER      ēn (III)|andar (I)              onbep.lidw.+znw.          4979      not_found_in_db
giscot    GESCHOT            giskot (I)              znw.o.  2375      not_found_in_db 2376
glidir      GLIJDERE             glīderī   bnw.     906; 4555             not_found_in_db  4555
cathendol           KATENTOL                          znw.      ?             not_found_in_db
kachdol                KATENTOL                          znw.      ?             not_found_in_db
catertol                KATENTOL                          znw.      ?             not_found_in_db
lespart  LESPART                              znw.      ?             not_found_in_db
macarelli             MAKREEL            makril   znw.      ?             not_found_in_db
mikilo    MEKEL  mikilo    bw.        4987      not_found_in_db  2977
reep      REEP      rēp         znw.      5047      not_found_in_db  5617
rep         REEP      rēp         znw.      5047      not_found_in_db
*ren      REN       ren         znw.      847         not_found_in_db ??
nixxîtx  WIJTEN                wītan (II)             st.ww.  4609      not_found_in_db 132
 */