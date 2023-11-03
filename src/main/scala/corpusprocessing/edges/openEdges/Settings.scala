package corpusprocessing.edges.openEdges

object Settings {
   val baseDirAtWork = "/mnt/Projecten/Corpora/Historische_Corpora/EDGeS_historical_bible_corpus/OpenEDGeS/OpenEDGeS/"
   val baseDirAtHome = "/media/jesse/Data/Corpora/Edges/OpenEDGeS"
   val baseDir = baseDirAtHome
   val nl_nl = baseDir + "Alignments/nl-nl/"
   val en_nl = baseDir + "Alignments/en-nl/"
   val staten_canisius = nl_nl + "nl_1637_Staten-nl_1939_Canisius.tsv"
   val staten_darby = en_nl + "en_1890_Darby-nl_1637_Staten.tsv"
   val darby_canisius = en_nl + "en_1890_Darby-nl_1939_Canisius.tsv"
}
