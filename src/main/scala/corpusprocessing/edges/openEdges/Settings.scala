package corpusprocessing.edges.openEdges

import utils.ProcessFolder

import java.io.File


object Defaults {
   lazy val projectRoot: String =
   {
      val rekenserver = new File("/media/proj/").exists();
      val projecten = if (rekenserver) "/media/proj/" else "/mnt/Projecten/";
      s"${projecten}/Corpora/Historische_Corpora/EDGeS_historical_bible_corpus/"
   }


   val sourceDir: String = "OpenEDGeS/OpenEDGeS_v1.01/"


   val teiDir: String = projectRoot + "TEI"
}
case class EdgeSettings(projectRoot: String = Defaults.projectRoot,
                      sourceDir: String = Defaults.sourceDir,
                      teiDir: String = Defaults.teiDir)
object Settings extends EdgeSettings() {

   val fullPathtoSourceData = projectRoot  + "/"  + sourceDir
   val outputTEIDir =  teiDir

   val verseAlignedTEIDir = outputTEIDir + "/alignments/"


   val tokenizedContentDir = teiDir + "/ids-fixed/"
   val wordLinkedContentDir = teiDir + "/inline-word-alignment/"

   new File(wordLinkedContentDir).mkdir()

   val nl_nl = fullPathtoSourceData + "/Alignments/nl-nl/"
   val en_nl = fullPathtoSourceData + "/Alignments/en-nl/"
   val staten_canisius = nl_nl + "/nl_1637_Staten-nl_1939_Canisius.tsv"
   val staten_darby = en_nl + "/en_1890_Darby-nl_1637_Staten.tsv"
   val darby_canisius = en_nl + "/en_1890_Darby-nl_1939_Canisius.tsv"

   val genesis = s"$teiDir/alignments/Gen.xml"
   val zeph = s"$teiDir/alignments/Zeph.xml"

   lazy val allAlignmentFiles = ProcessFolder.filesIn(new File(Settings.fullPathtoSourceData + "/Alignments")).map(_.getCanonicalPath).filter(_.endsWith(".tsv"))
   lazy val files_en_nl = allAlignmentFiles.filter(x => x.contains("nl-nl") || x.contains("en-nl") || x.contains("en-en"))


   lazy val complete_corpus = BibleCorpus(fullPathtoSourceData, allAlignmentFiles.toSet)
   lazy val subcorpus_en_nl = BibleCorpus(fullPathtoSourceData, files_en_nl.toSet)
   lazy val smaller_sample =  BibleCorpus(fullPathtoSourceData, Set(staten_darby, staten_canisius, darby_canisius))
}
