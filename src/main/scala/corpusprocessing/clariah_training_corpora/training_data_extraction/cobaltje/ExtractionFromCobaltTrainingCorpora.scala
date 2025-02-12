package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje

import java.io.{File, PrintWriter}
import OldCobaltSettings._
import corpusprocessing.clariah_training_corpora.training_data_extraction.{TrainingDataInfo, TrainingDataInfos}

object Rename {
  val renaming: Map[String,String] =Map(
    "evaluation_set_15" -> "dbnl-excerpts-15",
    "evaluation_set_16" -> "dbnl-excerpts-16",
    "evaluation_set_17" -> "dbnl-excerpts-17",
    "evaluation_set_18" -> "dbnl-excerpts-18",
    "evaluation_set_19" -> "dbnl-excerpts-19",
    "clvn_selectie_met_wat_minder_duits" -> "clvn",
    "courantenselectie" -> "couranten",
    "gtbcit_14_fromscratch" -> "dictionary-quotations-14",
    "gtbcit_mnw_15" -> "dictionary-quotations-15",
    "gtbcit_punct_16" -> "dictionary-quotations-16",
    "gtbcit_punct_17" -> "dictionary-quotations-17",
    "wnt_citaten_18" -> "dictionary-quotations-18",
    "wnt_citaten_19" -> "dictionary-quotations-19",
    "bab_enhanced_hoofdlettertest" -> "letters-as-loot",
    "eee1729763457532" -> "kranten_19",
    "gys1732186374071" -> "gysseling_ambtelijk"
  )
}

import Rename._

object ExtractionFromCobaltTrainingCorpora {

  def extract(cobaltExportSettings: CobaltExportSettings): Unit = {
    val extractedSets = new File(cobaltExportSettings.directoryWithCobaltExports).listFiles()
      .filter(_.getName.endsWith(".zip"))

      //.filter(_.getName.contains("gtbcit_mnw_15"))
      .map(f => {
        //val datasetName = f.getName().replaceAll("^cobalt_export_", "").replaceAll(".zip", "")
        val datasetNameOrg = f.getName().replaceAll("^cobalt_export_", "").replaceAll(".zip", "")
        val datasetName = renaming.getOrElse(datasetNameOrg, datasetNameOrg)
        val dirAsDir = new File(cobaltExportSettings.createdTrainingDataDirectory + "/" + datasetName)
        // dirAsDir.delete()
        dirAsDir.mkdir()
        val outputPrefix = cobaltExportSettings.createdTrainingDataDirectory + "/" + datasetName + "/" + datasetName

        val e = ExtractionFromCobaltExport(f.getCanonicalPath, outputPrefix,
          sentenceElement = if (datasetName.contains("cit")) "q" else "s")
        datasetName -> e.extract()
      }).toMap

    val infos = TrainingDataInfos(cobaltExportSettings.directoryWithCobaltExports, cobaltExportSettings.createdTrainingDataDirectory, extractedSets)
    val w = new PrintWriter(cobaltExportSettings.createdTrainingDataDirectory + "/"  + "cobaltSets.json")
    w.println(TrainingDataInfos.write(infos))
    w.close()
  }

  def main(args: Array[String]) = {
    extract(LancelotSettings)
  }
}



object doBoth { // dit werkt niet vanwege
  def main(args: Array[String]) = {
    ExtractionFromCobaltTrainingCorpora.main(Array())
    ExtractionFromCobaltTrainingCorporaWithConfig.main(Array())
  }
}

/*
Failure opening /mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2024_2/download/cobalt_export_wnt_citaten_19.zip
java.nio.file.FileSystemAlreadyExistsException
  at com.sun.nio.zipfs.ZipFileSystemProvider.newFileSystem(ZipFileSystemProvider.java:113)
  at java.nio.file.FileSystems.newFileSystem(FileSystems.java:326)
  at java.nio.file.FileSystems.newFileSystem(FileSystems.java:276)
  at utils.zipUtils$.getRootPath(zip.scala:23)
  at utils.zipUtils$.find(zip.scala:29)
 */
