package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje

import corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje.Rename.renaming
import corpusprocessing.clariah_training_corpora.training_data_extraction.{TrainingDataInfo, TrainingDataInfos}

import java.io.{File, PrintWriter}

// dit moet anders - alleen de partities uit de oude json lezen, maar gewoon de datapaden uit de cobalt export settings nemen
object ExtractionFromCobaltTrainingCorporaWithConfigOld {

  val jsonLocation = "data/cobaltExtraction/cobaltSets.met14.oldNames.json" //  "/mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2024_2/training-data-2/cobaltSets.json"
  val info = TrainingDataInfos.readFromFile(jsonLocation)

  def extract(cobaltExportSettings: CobaltExportSettings, enhanceTags: Boolean = false,
              extractTo: String = info.extractedDataDir.replaceAll("/$", "") + "_enhanced_tags"): Unit = {
    //print(renaming)


    new File(extractTo).mkdir()

    val extractedSets = new File(info.downloadedDataDir).listFiles()
      .filter(_.getName.endsWith(".zip"))
      //.filter(_.getName.contains("evaluation_set"))
      //.filter(_.getName.contains("gtbcit_mnw_15"))
      .map(f => {
        val datasetNameOrg = f.getName().replaceAll("^cobalt_export_", "").replaceAll(".zip", "")
        val datasetName = renaming.getOrElse(datasetNameOrg, datasetNameOrg)
        val datasetConfig: Option[TrainingDataInfo] = info.trainingDataInfos.get(datasetNameOrg)
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

    val infos = TrainingDataInfos(cobaltExportSettings.directoryWithCobaltExports, cobaltExportSettings.createdTrainingDataDirectory, extractedSets)
    val w = new PrintWriter(cobaltExportSettings.createdTrainingDataDirectory + "/" + "cobaltSets.json")
    w.println(TrainingDataInfos.write(infos))
    w.close()
  }

  def main(args: Array[String]) = {
    //extract(enhanceTags = true)
    extract(LancelotSettings, false, info.extractedDataDir.replaceAll("/$", "") + "_unenhanced_tags")
  }
}
