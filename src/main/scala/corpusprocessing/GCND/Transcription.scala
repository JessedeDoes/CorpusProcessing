package corpusprocessing.GCND
import scala.xml._
import database.DatabaseUtilities.{AlmostQuery, Select, doeHet}
import database._
import org.json4s._
import org.json4s.jackson.Serialization._

import scala.xml.PrettyPrinter
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.{AlpinoSentence, AlpinoToken}

import java.io.PrintWriter
import scala.xml.dtd.DocType


case class Transcription(transcriptie_id: Int) {
  lazy val elanQ = Select(r => ElanAnnotation(
    r.getInt("elan_annotatie_id"),
    r.getInt("transcriptie_id"),
    r.getString("annotatie_code"),
    r.getInt("opname__persoon_id"),
    r.getString("tekst_lv"),
    r.getString("tekst_zv"),
    r.getInt("starttijd"),
    r.getInt("eindtijd"),
    this
  ), "elan_annotatie")

  def alpinosForTranscriptionId(id: Int) = {
    val alpinoQ: Select[AlpinoAnnotation] = Select(
      r => AlpinoAnnotation(
        r.getInt("alpino_annotatie_id"),
        r.getInt("transcriptie_id"),
        r.getString("annotatie_code"),
        r.getInt("opname__persoon_id"),
        r.getString("tekst_lv"),
        r.getString("tekst_zv"),
        r.getString("alpino_xml"),
        r.getString("tokens"),
        r.getInt("starttijd"),
        r.getInt("eindtijd"), this), "alpino_annotatie where transcriptie_id=" + id)
    GCNDDatabase.db.slurp(alpinoQ).sortBy(x => x.sortKey)
  }

  lazy val alpinoAnnotations = alpinosForTranscriptionId(transcriptie_id)

  lazy val elanAnnotations = {
    val q = elanQ.copy(from = s"elan_annotatie where transcriptie_id=$transcriptie_id")
    GCNDDatabase.db.slurp(q).sortBy(x => x.starttijd + x.eindtijd)
  }

  def getPseudoFoLiAForElanAnnotations() =
    <FoLiA xml:id={"gcnd.transcriptie." + transcriptie_id} version="2.5.1" xmlns:folia="http://ilk.uvt.nl/folia" xmlns="http://ilk.uvt.nl/folia">
      <metadata type="internal" xmlns="http://ilk.uvt.nl/folia">
        <annotations>
          <pos-annotation set="hdl:1839/00-SCHM-0000-0000-000B-9"/>
          <lemma-annotation set="hdl:1839/00-SCHM-0000-0000-000E-3"/>
          <syntax-annotation set="lassy.syntax.annotation"/>
          <syntax-annotation set="ud.syntax.annotation"/>
          <division-annotation set="gcnd_div_classes"/>
          <timesegment-annotation set="cgn"/>
          <text-annotation set="https://raw.githubusercontent.com/proycon/folia/master/setdefinitions/text.foliaset.ttl"/>
          <token-annotation/>
          <sentence-annotation set="gcnd.sentence"/>
          <dependency-annotation set="gcnd.dependency"/>
        </annotations>
        <foreign-data>
          {Metadata.getMetadata(transcriptie_id)}
        </foreign-data>
      </metadata>{elanAnnotations.sortBy(_.starttijd).map(x => x.pseudoFolia())}
    </FoLiA>
}
