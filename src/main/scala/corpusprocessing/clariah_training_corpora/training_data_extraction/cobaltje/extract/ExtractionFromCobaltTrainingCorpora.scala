package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje.extract

import corpusprocessing.clariah_training_corpora.training_data_extraction.TrainingDataInfos
import corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje.Rename.renaming
import corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje.{LancelotExportSettings, LancelotSettings}

import java.io.{File, PrintWriter}

object ExtractionFromCobaltTrainingCorpora {

  def extract(cobaltExportSettings: LancelotExportSettings): Unit = {
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
