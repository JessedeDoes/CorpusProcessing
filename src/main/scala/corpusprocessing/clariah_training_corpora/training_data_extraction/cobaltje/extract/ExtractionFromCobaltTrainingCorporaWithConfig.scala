package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje.extract

import corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje.Rename.renaming
import corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje.{LancelotExportSettings, LancelotSettings}
import corpusprocessing.clariah_training_corpora.training_data_extraction.{TrainingDataInfo, TrainingDataInfos}

import java.io.{File, PrintWriter}

// dit moet anders - alleen de partities uit de oude json lezen, maar gewoon de datapaden uit de cobalt export settings nemen
object ExtractionFromCobaltTrainingCorporaWithConfig {

  val jsonLocation = "data/cobaltExtraction/cobaltSets.met14.oldNames.json" //  "/mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2024_2/training-data-2/cobaltSets.json"
  val newJsonLocation = "data/cobaltExtraction/cobaltSets.newDocumentNames.noLegacyNames.json" // "data/cobaltExtraction/cobaltSets.newDocumentNames.json"

  val info = TrainingDataInfos.readFromFile(newJsonLocation)

  def extract(cobaltExportSettings: LancelotExportSettings, enhanceTags: Boolean = false,
              extractToX: String = info.extractedDataDir.replaceAll("/$", "") + "_enhanced_tags"): Unit = {
    //print(renaming)


    //new File(extractTo).mkdir()

    val extractedSets = new File(cobaltExportSettings.directoryWithCobaltExports).listFiles()
      .filter(_.getName.endsWith(".zip"))
      //.filter(_.getName.contains("evaluation_set"))
      //.filter(_.getName.contains("gtbcit_mnw_15"))
      .map(f => {
        val datasetNameOrg = f.getName().replaceAll("^cobalt_export_", "").replaceAll(".zip", "")
        val datasetName = renaming.getOrElse(datasetNameOrg, datasetNameOrg)
        val datasetConfig: Option[TrainingDataInfo] = info.trainingDataInfos.get(datasetNameOrg)
        //val dirAsDir = new File(extractTo + "/" + datasetName)
        val isQuotationCorpus = (datasetName.contains("cit") || datasetName.contains("quotation"))
        // dirAsDir.delete()
        // dirAsDir.mkdir()
        // val outputPrefix = extractTo + "/" + datasetName + "/" + datasetName
        if (datasetName == "dictionary-quotations-15") {

        }
        val outputPrefix = cobaltExportSettings.createdTrainingDataDirectory + "/" + datasetName + "/" + datasetName
        new File(cobaltExportSettings.createdTrainingDataDirectory + "/" + datasetName).mkdir()
        val e = ExtractionFromCobaltExport(f.getCanonicalPath,
          outputPrefix,
          sentenceElement = datasetConfig.map(_.sentenceElement).getOrElse(if (isQuotationCorpus) "q" else "s"),
          cleanBrackets = isQuotationCorpus,
          enhanceTags = enhanceTags, // dan wordt dus alles wel anders.......
          info = datasetConfig
        )
        val newConfig: TrainingDataInfo = e.extract()
        val useOld: Boolean = if (Some(newConfig) != datasetConfig) {
          Console.err.println(s"Hm, partitionering is niet hetzelfde voor $datasetName, ${datasetConfig.nonEmpty}")
          if (datasetConfig.nonEmpty) {
            TrainingDataInfo.compareConfigs(datasetConfig.get,newConfig)
          } else false
        } else false
        datasetName -> (if (useOld && datasetConfig.nonEmpty) datasetConfig.get else newConfig)
      }).toMap

    val infos = TrainingDataInfos(cobaltExportSettings.directoryWithCobaltExports,
      cobaltExportSettings.createdTrainingDataDirectory, extractedSets)
    val w = new PrintWriter(cobaltExportSettings.createdTrainingDataDirectory + "/" + "cobaltSets.x.json")
    w.println(TrainingDataInfos.write(infos))
    w.close()
  }

  def main(args: Array[String]) = {
    //extract(enhanceTags = true)
    extract(LancelotSettings, false, null) // info.extractedDataDir.replaceAll("/$", "") + "_unenhanced_tags")
  }
}

/*
Hm, partitionering is niet hetzelfde voor dictionary-quotations-15, true
Boom for dev!: 11, old:282, new:271:  Set(DocumentInfo(docpid_822.xml,None),
 */