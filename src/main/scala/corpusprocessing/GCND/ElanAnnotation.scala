package corpusprocessing.GCND

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.AlpinoToken
import utils.Tokenizer

import scala.xml._

object ElanStats {
  var nopes = 0
  var nopesX = 0
  var alpinos = 0
  var alignments = 0
  var nulls = 0
}
case class ElanAnnotation(elan_annotatie_id: Int,
                          transcriptie_id: Int,
                          annotatie_code: String,
                          opname_persoon_id: Int,
                          tekst_lv: String,
                          tekst_zv: String,
                          starttijd: Int,
                          eindtijd: Int,
                          transcription: Transcription
                         )
{
  type token = Tokenizer.Token

  lazy val overLappingAlpinoAnnotations: Seq[AlpinoAnnotation] = transcription.alpinoAnnotations.filter(e => // pas op ook op id filteren!
    e.starttijd >= starttijd & e.starttijd < eindtijd || e.eindtijd > starttijd & e.eindtijd <= eindtijd
  )

  lazy val allAlpinoTokens: Seq[(GCNDDatabase.Token, AlpinoToken)] = overLappingAlpinoAnnotations.flatMap(a => a.zipped.getOrElse(Seq()))

  def pseudoFolia()  = {
     Console.err.println(s"###################### Generating xml for elan annotations transcriptie=$transcriptie_id, elan=$elan_annotatie_id, overlapping: ${overLappingAlpinoAnnotations.size}, alpinos voor transcriptie: ${transcription.alpinoAnnotations.size}")
    val (useAlpino, useAlignment, enrichedContent, message): (Boolean, Boolean, NodeSeq, String) =
      if (tekst_zv != null && tekst_lv != null)
      {
        val (useAlpino, elanAlignedTokens, message) = HeavyLightAlignment(this).align()
        Console.err.println(s"useAlpino=$useAlpino")
        if (useAlpino) {
          ElanStats.alpinos = ElanStats.alpinos + 1
          (true,false, overLappingAlpinoAnnotations.flatMap(a => a.Folia.pseudoFolia(includeAlpinoParse = true) \\ "s"), message)
        } else
        {
          // Console.err.println(elanAlignedTokens)
          if (elanAlignedTokens.nonEmpty)
          {
            ElanStats.alignments = ElanStats.alignments + 1
            val weetjes = elanAlignedTokens.map({case (tl, tz) => <w><t class="lightDutchification">{tl.toString}</t><t class="heavyDutchification">{tz.toString}</t></w>})
            (false, true, <div class="noSyntacticAnnotation">{weetjes}</div>, message)
          }
          else  {

            (false, false, Seq(), message) }
        }
      } else {
        ElanStats.nopes = ElanStats.nopes + 1
        ElanStats.nulls = ElanStats.nulls+1
        (false,false,Seq(), "Null text field")
      };

    if (!useAlpino) {
      // Console.err.println(enrichedContent)
    }
    lazy val speech_id = s"speech.elan.$elan_annotatie_id"
    lazy val alpinoStukje = overLappingAlpinoAnnotations.map(a => {

      <div class="alpinoAnnotation" begintime={Stuff.formatTime(a.starttijd)} endtime={Stuff.formatTime(a.eindtijd)}>
        {if (a.text_lv.trim.nonEmpty) <t class="alpinoLightDutchification">{a.text_lv}</t>}
        <t class="alpinoHeavyDutchification">{a.text_zv}</t>
      </div>
    })

    <speech tag={functie} speaker={naam} xml:id={speech_id}  begintime={Stuff.formatTime(starttijd)} endtime={Stuff.formatTime(eindtijd)}>
      {if (tekst_lv != null && tekst_lv.trim.nonEmpty) <t class="lightDutchification">{tekst_lv}</t>}
      {if (tekst_zv != null && tekst_zv.trim.nonEmpty) <t class="heavyDutchification">{tekst_zv}</t>}
      {Comment(s"speaker role:$functie, n_alpino_annotations: " +  overLappingAlpinoAnnotations.size.toString + s"; Use alpino: $useAlpino, Use alignment: $useAlignment\n$message")}
      {enrichedContent}
      <foreign-data>{Metadata.getMetadataForElanAnnotation(elan_annotatie_id)}</foreign-data>
    </speech>
  }

  lazy val meta = {Metadata.getMetadataForElanAnnotation(elan_annotatie_id)}
  lazy val functie = (meta \\ "functie").text
  lazy val naam = (meta \\ "naam").text
  lazy val nAlpinos = overLappingAlpinoAnnotations.size
}

