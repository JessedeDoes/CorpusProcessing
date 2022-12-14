package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy

case class Sentence(id: String,
                    tokens: List[String],
                    tags: List[String],
                    lemmata: List[String],
                    xml_ids: List[String]  = List(),
                    relevances: List[String]  = List(),
                    hilex_pos : List[String]  = List(),
                    file: String = "unknown" // pas op andere dingen hebben dit niet
                   )