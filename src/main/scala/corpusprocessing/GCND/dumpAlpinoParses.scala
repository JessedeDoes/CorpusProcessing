package corpusprocessing.GCND

object dumpAlpinoParses {
  def main(args: Array[String]) = {

    val alpinoDumpDir = new java.io.File("/home/jesse/Downloads/AlpinoDumps/")
    alpinoDumpDir.mkdir()
    GCNDDatabase.transcriptions.foreach(t => GCNDDatabase.saveAlpinoParses(t, alpinoDumpDir))
    // saveAlpinoParses(transcriptie_id = 1, alpinoDumpDir)

  }
}
