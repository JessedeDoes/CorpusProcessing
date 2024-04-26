package corpusprocessing.clariah_training_corpora.training_data_extraction

import utils.PostProcessXML

import java.io.File
import scala.xml.{Elem, Null, UnprefixedAttribute}


object gtbcit_to_huggingface extends extract_training_data_trait {
  val gtbCit = "/mnt/Projecten/Corpora/Historische_Corpora/Wolkencorpus/GTB/CitatenTDN2/Refurbished/"

  override  def always_sampled(s1: Sentence) = {
    val s = s1.asInstanceOf[PimpedSentence]
    s.hilex_pos.indices.exists(i => s.relevances(i) == "yes" && s.hilex_pos(i).matches(".*(PD|CON|ADP|NUM|INT)"))
  }

  def setPos(w: Elem, p:String) = w.copy(attributes =  w.attributes.append(new UnprefixedAttribute("hilex-pos", p, Null)))

  override def decentSentence(s: Sentence, b: Partition)  = s.asInstanceOf[PimpedSentence].hilex_pos.exists(x => x != "unk")

  def propagateHilexPos(d: Elem): Elem = {
    PostProcessXML.updateElement(d,_.label=="cit", cit =>  {
      val pos = (cit \ "@pos").text
      // System.err.println(pos)
      PostProcessXML.updateElement(cit,_.label=="w", w => setPos(w,pos))
    })
  }

  override def preprocess(e: Elem): Elem = propagateHilexPos(e)
}

object ofr_to_huggingface extends extract_training_data_trait {
  override val pos_attribute = "@type"
  override lazy val default_folder = "/mnt/Projecten/Corpora/Historische_Corpora/OudFries/RitaVdPoel/corpusfiles/"
  override val split_test_train_on_document_level = true
  override lazy val output_prefix = "ofr"
  override def decentSentence(s: Sentence, b: Partition)  =  {
    val tags =  s.asInstanceOf[BasicSentence].tags
    tags.count(_.nonEmpty) > 0.7 * tags.size
  }
}
//
object onw_to_huggingface extends extract_training_data_trait {
  override val pos_attribute = "@pos"
  override   lazy val default_folder = "/mnt/Projecten/Corpora/Historische_Corpora/ONW/ONW-januari-2022/"
  override val split_test_train_on_document_level = false
  override lazy val output_prefix = "onw"
  override def decentSentence(s: Sentence, b: Partition)  =  {
    val tags =  s.asInstanceOf[BasicSentence].tags
    tags.count(t => t.nonEmpty && !t.contains("RES")) > 0.6 * tags.size
  }
}
object bab_to_huggingface extends extract_training_data_trait {
  override val split_test_train_on_document_level: Boolean = true
  override lazy val output_prefix: String = "bab"
  override val max_files: Int = Integer.MAX_VALUE
  override val training_subsets: Int = 10
  override lazy val output_folder = "/mnt/Projecten/Corpora/TrainingDataForTools/BaB/All/"  + "test_train" + (if (training_subsets > 1) "/partitioned/" else "")
  new File(output_folder).mkdir()
  override lazy val default_folder = "/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuit/2.8TDN/"
}
object crm_to_huggingface extends extract_training_data_trait {
  override val split_test_train_on_document_level: Boolean = true
  override lazy val output_prefix: String = "CRM"
  override val max_files: Int = Integer.MAX_VALUE // 500
  override val training_subsets: Int = 10
  override lazy val output_folder: String = "/mnt/Projecten/Corpora/TrainingDataForTools/CRM/All/" + "/" + "test_train" + (if (training_subsets > 1) "/partitioned/" else "")
  //override val output_folder: String = "/mnt/Projecten/Corpora/TrainingDataForTools/CRM/All/"
  override lazy val default_folder = "/mnt/Projecten/Corpora/Historische_Corpora/CRM/TEI-tagmapped/"
}
