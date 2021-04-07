package corpusprocessing.bab_aanvulling
import corpusprocessing.brievenalsbuit.Settings.discardSubElemsOfWord
import corpusprocessing.brievenalsbuit.bab.veiligePunten
import corpusprocessing.gysseling.GysselingTokenizer
import utils.{PostProcessXML, Tokenizer}

import scala.xml._
import util.{Failure, Success, Try}

object sillyTokenizer {
  def tokenize(w: Elem) : Seq[Node] = {
    val word = w.text
    val t = Tokenizer.tokenizeOne(word)
    if ((word.contains("[") || word.contains("]")) || (t.leading.isEmpty && t.trailing.isEmpty))
      Seq(w) else Try({
      val s1 = w.toString.replace(t.trailing + "</w>", "</w>" + <pc>{t.trailing}</pc>)
      val s2 = s1.replaceAll("(<w[^<>]*>)" + t.leading.replace(".","\\."), s"<pc>${t.leading}</pc>$$1").replaceAll("<pc></pc>", "")
      val s3 = XML.loadString("<x>" + s2 + "</x>").child;
      s3
    })  match {
      case Success(x) => x
      case _ =>  println(s"|{${t.leading}|${t.token}|${t.trailing}| $w"); Seq(w)
    }
  }
}

object fixAbbr {

  // &lt;hyph&gt;genoeg&lt;-&gt;saam&lt;/hyph&gt;"
  def gysTokenize(w: Elem): Seq[Node] = {

    val check = w.text.trim.replaceAll("\\s+", " ")
    val w1 = w.copy(child = <seg>{w.child}</seg>)
    val w1t = GysselingTokenizer.tokenizeOne(w1)
    val doeIets = w1t.size > 1

    val w2 = w1t.flatMap(x => {
      x match {
          case e:Elem if e.label == "w" =>
            val w1 = e.copy(child = e.child.flatMap({
              case s:Elem if s.label == "seg" => s.child
              case x => Seq(x)
            }))
            sillyTokenizer.tokenize(w1)
        case pc: Elem if pc.label == "pc" => pc.copy(child = Text(pc.text.trim))
        case y => y
      }
    })
    val check1 = w2.text.trim.replaceAll("\\s+", " ")
    if (check1 != check)
      {
        println(s"Klopt geen hout: $check --> $check1")
        System.exit(1)
      }
    if (w.text.contains(".") && (w \\ "hyph").nonEmpty) println(s"\n\n$w1 ==> (Gysseling) $w1t ==> (hier) $w2")
    w2
  }

  def tokenize(w: Elem):Seq[Node] =
  {
    //System.err.println("tokenize: " + w)
    val word = w.text
    val t = Tokenizer.tokenizeOne(word)

    val nonTriv = (t.token.nonEmpty  && (t.leading.nonEmpty || t.trailing.nonEmpty))

    val lemma = (w \ "@lemma").text

    val safe = true // (!t.trailing.startsWith(".") || veiligePunten.contains(word,lemma)) && (discardSubElemsOfWord || seg.child.filter(_.isInstanceOf[Elem]).isEmpty)
    if (!safe || !nonTriv)
      w
    else
    {
      val r =
        (if (t.leading.nonEmpty) Seq(<pc>{t.leading}</pc>) else Seq()) ++
        w.copy(child = Seq(Text(t.token))) ++
        (if (t.trailing.nonEmpty) Seq(<pc>{t.trailing}</pc>) else Seq())
      r
    }
  }

  def resetTextToOriginal(w: Elem): Elem = {
    val original = (w \ "@original").text
    if (original.nonEmpty && original.contains("<")) {
      val newNode = Try(XML.loadString("<w>" + original.replaceAll("<->","<lb type='hyphen'/>") + "</w>")) match {
        case Success(x) => w.copy(child = x.child)
        case _ => w
      }
      //if (original.contains("hyph")) Console.err.println(newNode);
      newNode
    } else w
  }


  def removeBreakAfterHyphen(wParent: Elem) = {
    val children= wParent.child.zipWithIndex

    def hyphenBefore(i: Int): Boolean= if (i > 0) {
      val (w1,_) = children(i-1)
      if (w1.isInstanceOf[Text] && w1.text.trim.isEmpty && i > 1) {
        val (w2,_) = children(i-2)
        (w2.label == "w" &&  (w2 \\ "hyph").nonEmpty)
      } else {
        (w1.label == "w" &&  (w1 \\ "hyph").nonEmpty)
      }
    } else false

    val fixed = children.flatMap{
      case (e:Elem,i) if (e.label == "lb" && hyphenBefore(i)) =>  {
        // Console.err.println("Hyphen before me!!" + e + children(i-2) + " .. " + children(i-1))
        Seq(<c> </c>)
      }
      case  (x,i) => Seq(x)
    }
    wParent.copy(child = fixed)
  }

  val testW = <w original="&lt;abbr&gt;ver&lt;/abbr&gt;staen">verstaen</w>

  def fixDoc(d: Elem) = {

    val d1 = PostProcessXML.updateElement(d, _.label=="w", resetTextToOriginal)
    val d2 = PostProcessXML.updateElement(d1, x => (x \ "w").nonEmpty, removeBreakAfterHyphen)
    val dt = PostProcessXML.updateElement5(d2, _.label == "w",  gysTokenize).asInstanceOf[Elem] //
    dt
  }

  def fixAllDocs = new java.io.File(Settings.xmlAfterImageCorrections).listFiles().foreach(
    f => {
      val fixed = fixDoc(XML.loadFile(f))
      val newFile = Settings.abbrCorrectDir + f.getName()
      XML.save(newFile, fixed, "UTF-8")
    }
  )

  def main(args: Array[String]): Unit = {
    // println(fixW(testW))
    fixAllDocs
  }
}
