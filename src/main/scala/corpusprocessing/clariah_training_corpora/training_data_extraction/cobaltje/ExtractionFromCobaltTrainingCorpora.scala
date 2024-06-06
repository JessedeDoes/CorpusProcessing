package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje

import java.io.{File, PrintWriter}
import Settings._
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
  )
}

import Rename._

object ExtractionFromCobaltTrainingCorpora {

  def main(args: Array[String]) = {

    val extractedSets = new File(directoryWithCobaltExports).listFiles()
      .filter(_.getName.endsWith(".zip"))

      //.filter(_.getName.contains("gtbcit_mnw_15"))
      .map(f => {
        val datasetName = f.getName().replaceAll("^cobalt_export_", "").replaceAll(".zip", "")
        val dirAsDir = new File(createdTrainingDataDirectory + "/" + datasetName)
        // dirAsDir.delete()
        dirAsDir.mkdir()
        val outputPrefix = createdTrainingDataDirectory + "/" + datasetName + "/" + datasetName

        val e = ExtractionFromCobaltExport(f.getCanonicalPath, outputPrefix,
          sentenceElement = if (datasetName.contains("cit")) "q" else "s")
        datasetName -> e.extract()
      }).toMap

      val infos = TrainingDataInfos(directoryWithCobaltExports, createdTrainingDataDirectory, extractedSets)
      val w = new PrintWriter(createdTrainingDataDirectory + "/"  + "cobaltSets.json")
      w.println(TrainingDataInfos.write(infos))
      w.close()
  }
}

object ExtractionFromCobaltTrainingCorporaWithConfig {


  val jsonLocation = "data/cobaltExtraction/cobaltSets.met14.oldNames.json" //  "/mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2024_2/training-data-2/cobaltSets.json"
  val info = TrainingDataInfos.readFromFile(jsonLocation)

  def extract(enhanceTags: Boolean = false, extractTo:String = info.extractedDataDir.replaceAll("/$", "") + "_enhanced_tags"): Unit = {
    //print(renaming)


    new File(extractTo).mkdir()


    val extractedSets  = new File(info.downloadedDataDir).listFiles()
      .filter(_.getName.endsWith(".zip"))
      //.filter(_.getName.contains("evaluation_set"))
      //.filter(_.getName.contains("gtbcit_mnw_15"))
      .map(f => {
        val datasetNameOrg = f.getName().replaceAll("^cobalt_export_", "").replaceAll(".zip", "")
        val datasetName = renaming.getOrElse(datasetNameOrg, datasetNameOrg)
        val datasetConfig = info.trainingDataInfos.get(datasetNameOrg)
        val dirAsDir = new File(extractTo + "/" + datasetName)
        val isQuotationCorpus = (datasetName.contains("cit") || datasetName.contains("quotation"))
        // dirAsDir.delete()
        dirAsDir.mkdir()

        val outputPrefix = extractTo + "/" + datasetName + "/" + datasetName

        val e = ExtractionFromCobaltExport(f.getCanonicalPath, outputPrefix,
          sentenceElement = datasetConfig.map(_.sentenceElement).getOrElse(if (isQuotationCorpus) "q" else "s"),
          cleanBrackets = isQuotationCorpus,
          enhanceTags = enhanceTags, // dan wordt dus alles wel anders.......
          info = datasetConfig
        )
        val newConfig = e.extract()
        if (newConfig != datasetConfig) {
          Console.err.println(s"Hm, het is niet hetzelfde voor $datasetName")
        }
        datasetName -> newConfig
      }).toMap

    val infos = TrainingDataInfos(directoryWithCobaltExports, createdTrainingDataDirectory, extractedSets)
    val w = new PrintWriter(createdTrainingDataDirectory + "/" + "cobaltSets.json")
    w.println(TrainingDataInfos.write(infos))
    w.close()

  }
  def main(args: Array[String]) = {
    extract(enhanceTags = true)
    //extract(false, info.extractedDataDir.replaceAll("/$", "") + "_unenhanced_tags")
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
