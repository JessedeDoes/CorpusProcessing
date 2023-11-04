package corpusprocessing.edges.openEdges
import utils.ProcessFolder
import java.io.File
object Settings {
   val baseDirAtWork = "/mnt/Projecten/Corpora/Historische_Corpora/EDGeS_historical_bible_corpus/OpenEDGeS/OpenEDGeS/"
   val baseDirAtHome = "/media/jesse/Data/Corpora/Edges/OpenEDGeS/"
   val baseDir = baseDirAtHome
   val teiDir = "/media/jesse/Data/Corpora/Edges/TEI"
   val nl_nl = baseDir + "Alignments/nl-nl/"
   val en_nl = baseDir + "Alignments/en-nl/"
   val staten_canisius = nl_nl + "nl_1637_Staten-nl_1939_Canisius.tsv"
   val staten_darby = en_nl + "en_1890_Darby-nl_1637_Staten.tsv"
   val darby_canisius = en_nl + "en_1890_Darby-nl_1939_Canisius.tsv"

   val genesis = "/media/jesse/Data/Corpora/Edges/TEI/alignments/Gen.xml"
   val zeph = "/media/jesse/Data/Corpora/Edges/TEI/alignments/Zeph.xml"

   lazy val allFiles = ProcessFolder.filesIn(new File(Settings.baseDir + "/Alignments")).map(_.getCanonicalPath).filter(_.endsWith(".tsv"))
   lazy val files_en_nl = allFiles.filter(x => x.contains("nl-nl") || x.contains("en-nl") || x.contains("en-en"))


   lazy val complete_corpus = BibleCorpus(baseDir, allFiles.toSet)
   lazy val subcorpus_en_nl = BibleCorpus(baseDir, files_en_nl.toSet)

   lazy val smaller_sample = BibleCorpus(baseDir, Set( staten_darby, staten_canisius, darby_canisius))
}
