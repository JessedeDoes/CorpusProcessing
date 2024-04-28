package corpusprocessing.clariah_training_corpora.training_data_extraction

import java.io.File;
trait nederval extends extract_training_data_trait {
  val name = "nederval"
  override val sentence_element="s"
  val base_folder = "/mnt/Projecten/Corpora/TrainingDataForTools/NederlabSetjes/"
  val output_base = "/mnt/Projecten/Corpora/TrainingDataForTools/NederlabSetjes/Splits/"
  override lazy val output_folder = { val z = output_base + name; new File(z).mkdir(); z }

  override lazy val default_input_folder = base_folder + "/"  + name + "/CobaltServeExport/"
  override lazy val output_prefix = name
}


object clariah_15 extends  nederval {
  override val name = "evaluation_set_15"
}


object clariah_16 extends  nederval {
  override val name = "evaluation_set_16"

}

object clariah_17 extends  nederval {
  override val name = "evaluation_set_17"

}
object clariah_18 extends  nederval {
  override val name = "evaluation_set_18"

}

object clariah_19 extends  nederval {
  override val name = "evaluation_set_19"
}

object clariah_several {
  def main(args: Array[String])  = {
    List(
      clariah_15

      //clariah_16, clariah_17, clariah_18, clariah_19
       )
      .foreach(_.main(Array()))
  }
}