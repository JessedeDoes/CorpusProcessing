package dictionaries.anw

import java.io.PrintWriter

object dumpCompleteXML {

  def main(args: Array[String]): Unit = {

    val pw = new PrintWriter("/tmp/anw_dummp.xml")
    Settings.articles.foreach(a => pw.println(a.article))
    pw.close()
  }
}
