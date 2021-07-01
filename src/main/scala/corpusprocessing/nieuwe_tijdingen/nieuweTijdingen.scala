package corpusprocessing.nieuwe_tijdingen

import java.io.{File,PrintWriter}
import scala.util.{Failure, Success, Try}
import scala.xml._
object nieuweTijdingen {
  val dir = "/mnt/Projecten/Corpora/Historische_Corpora/NieuweTijdinghen/Teksten/TXT/"

  val files = new File(dir).listFiles.filter(_.getName.endsWith(".txt"))

  val logje = new PrintWriter("/mnt/Projecten/Corpora/Historische_Corpora/NieuweTijdinghen/Teksten/wellformedness.log")

  def processBlock(b: String): String = {
    val bt = b.trim;

    val first = !bt.contains("凹")

    val leadingWhite = if (first) "" else bt.replaceAll("(?s)凹.*","").replaceAll("凸","").replaceAll("[凹凸]","")

    val txt = bt.replaceAll("(?s).*凹", "")
      .trim
      .replaceAll("\\*([a-zA-Z.]{1,2})\\*", "<expan>$1</expan>")
      .replaceAll("\\*([^*]{3,})\\*","<emph>$1</emph>")

    if (txt.matches("(?s)<.*>")) {
      return leadingWhite + txt;
    }

    return s"$leadingWhite<p>$txt</p>"
  }

  def processFile(txt: File): Unit = {
    val slurpie = io.Source.fromFile(txt).getLines.mkString("\n")


    val withPages = slurpie
      .replaceAll("\u00a0"," ")
      .replaceAll("(\\[\\])?\\{#[^{}]*\\}","")
      .replaceAll("\\{((\\\\)+)", "{/")
      .replaceAll("\\{pa?g?\\s*\\.?\\s*([0-9]+)\\.?\\s*\\.?}", "<pb n='$1'/>")

    val pseudo = withPages
        .replaceAll("\\{ill\\}", "<figure/>")
        .replaceAll("\\{([A-Za-z]+[0-9]*)\\}","<$1>")
        .replaceAll("\\{/([A-Za-z]+[0-9]*)\\}","</$1>")
        .replaceAll("<h1>([^<>]*)<h1>", "<h1>$1</h1>")
        .replaceAll("&", "&amp;")


    //Console.err.println(txt.getName)


   if (pseudo.contains("{p.")) {

   }


    val p1 = pseudo
      .replaceAll("(\\s*\n\\s*\n\\s*)", "口凸$1凹")
      .split("口")
      .map(processBlock)
      .mkString("")



    val p2 =
      """<TEI>
        |  <teiHeader>
        |  </teiHeader>
        |  <text>
        |  """.stripMargin + p1 +
        """
          |</text>
          |</TEI>""".stripMargin

    val test = Try(XML.loadString(p2)) match {
      case Success(x) =>
        val save = new PrintWriter(s"$dir/../PseudoXML/Success/" + txt.getName.replaceAll(".txt$", ".xml"))
        save.print(p2)
        save.close()
        <success fileName={txt.getName}/>
      case Failure(e) =>
        val save = new PrintWriter(s"$dir/../PseudoXML/Failure/" + txt.getName.replaceAll(".txt$", ".xml"))
        save.print(p2)
        save.close()
        logje.println(s"${txt.getName}\t$e")
        <bummer fileName={txt.getName}>{e}</bummer>
    }


    logje.flush()
    println(test)
  }

  def main(args: Array[String]): Unit = {
    files.foreach(processFile)
    logje.close()
  }
}
