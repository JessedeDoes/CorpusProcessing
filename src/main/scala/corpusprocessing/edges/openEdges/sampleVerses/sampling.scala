package corpusprocessing.edges.openEdges.sampleVerses
import corpusprocessing.edges.openEdges.Settings.{en_nl, teiDir, tokenizedContentDir}
import corpusprocessing.clariah_training_corpora.patch_scripts.fixTokenization.getId
import scala.xml._
import java.io.File
import utils.PostProcessXML._
object sampling {
    val maxBooks = Integer.MAX_VALUE
    val maxTokens = 5000
    lazy val allFiles = new File(tokenizedContentDir).listFiles().filter(x => x.getName.endsWith(".xml") && x.getName.startsWith("nl") ).toList
    def bible(f: File) = f.getName.replaceAll("\\.[^.]+\\.xml$", "")
    def lineNumber(verse: Node) = (getId(verse).replaceAll(".*\\.", "")).toInt

    lazy val bibles: Map[String, List[File]] = allFiles.groupBy(bible).mapValues(_.take(maxBooks))

    def bibleVerses(bible:  String): List[(String, Node)] = bibles(bible).flatMap(f => {
        val name = f.getCanonicalPath
        val x = XML.loadFile(f)
        (x \\ "ab").map(ab => name -> ab)
    })

    def selectVerses(bible: String, maxTokens: Int=sampling.maxTokens)  = {
        val verses = bibleVerses(bible)
        val shuffled: Seq[(String, Node)] = util.Random.shuffle(verses)
        var cumul: Int = 0
        lazy val selection: Seq[(String, Node)] = shuffled.takeWhile({case (b, verse: Node)  =>
            cumul = cumul + (verse \\ "w").size
            cumul <= maxTokens
        })
        selection
    }

    def main(args: Array[String])  = {
        val sampleDir = teiDir + "/Samples/"

        bibles.foreach({case (b, files) =>
           val sample: Map[String, Seq[Node]] = selectVerses(b).groupBy(_._1).mapValues(_.map(_._2))
            // dit toch liever per book doen

            {
                sample.foreach { case (f, verses) =>
                    val doc = XML.load(f)
                    val header = doc \\ "teiHeader"
                    val doc1 = updateElement(doc, _.label == "body", x => <body><div>
                        {verses.sortBy(lineNumber)}
                    </div></body>)
                    val saveTo =  sampleDir + ""  + new File(f).getName
                   println(saveTo)
                   XML.save(saveTo, doc1)
                }
            }
        })
    }
}
