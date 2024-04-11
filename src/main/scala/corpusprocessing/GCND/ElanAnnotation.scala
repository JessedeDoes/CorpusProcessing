package corpusprocessing.GCND


import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.AlpinoToken
import org.json4s.{DefaultFormats, Formats}
import org.json4s.jackson.Serialization.read
import utils.Tokenizer

import scala.util.{Failure, Success}
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
                          tokens: String,
                          tagged_tokens: String,
                          transcription: Transcription,
                          allowablePairs:Set[(Int,Int)]  = Set()
                         )
{
  type token = Tokenizer.Token

  lazy val overLappingAlpinoAnnotations: Seq[AlpinoAnnotation] = transcription.alpinoAnnotations.filter(a =>
    a.starttijd >= starttijd & a.starttijd < eindtijd || a.eindtijd > starttijd & a.eindtijd <= eindtijd
  )

  lazy val alpinoAnnotations = transcription.getAlpinos(elan_annotatie_id)

  implicit lazy val serializationFormats: Formats = DefaultFormats

  lazy val taggedTokensFromDatabase: List[GCNDToken] = scala.util.Try(read[Array[GCNDToken]](tagged_tokens).toList) match {
    case Success(x) => x
    case Failure(e) => {
      System.err.println(s"!!!Bad tagged tokens for elan $elan_annotatie_id: $tagged_tokens")
      List[GCNDToken]()
    }
  }

  lazy val alignedTokensFromDatabase: List[GCNDToken] = if (taggedTokensFromDatabase.nonEmpty) taggedTokensFromDatabase else scala.util.Try(read[Array[GCNDToken]](tokens).toList) match {
    case Success(x) => x
    case Failure(e) => {
      System.err.println(s"!!!Bad tokens for elan: $tokens")
      List[GCNDToken]()
    }
  }

  lazy val allAlpinoTokens: Seq[(GCNDToken, AlpinoToken)] = alpinoAnnotations.flatMap(a => a.gcndTokensZippedWithAlpinoTokens)

  lazy val (useAlpino, useAlignment, enrichedContent, message): (Boolean, Boolean, NodeSeq, String) = {
    if (tekst_zv != null && tekst_lv != null) {
      val (useAlpino, elanAlignedTokensRaw, message) = HeavyLightAlignment(this).alignHeavyLight()
      val elanAlignedTokens0: Seq[GCNDToken] = if (alignedTokensFromDatabase.nonEmpty)  alignedTokensFromDatabase else elanAlignedTokensRaw.map({case (x,y) => GCNDToken(x.toString, y.toString)})
      val elanAlignedTokens: Seq[GCNDToken] = elanAlignedTokens0.indices.map(i => {
        val space_after = i < elanAlignedTokens0.size-1 && !elanAlignedTokens0(i+1).joined
        val id = s"w.$elan_annotatie_id.unannotated.$i"
        elanAlignedTokens0(i).copy(space_after=space_after, id=id)
      })
      // Console.err.println(s"useAlpino=$useAlpino")
      if (useAlpino) {
        ElanStats.alpinos = ElanStats.alpinos + 1
        (true, false, alpinoAnnotations.flatMap(a => a.Folia.pseudoFolia(this, includeAlpinoParse = true) \\ "s"), message)
      } else {
        // Console.err.println(elanAlignedTokens)
        if (elanAlignedTokens.nonEmpty) {
          ElanStats.alignments = ElanStats.alignments + 1
          /*
            This element (w) carries an extra and optional space attribute with value yes (default), or no,
            indicating whether a space follows between this token and the next one. This attribute is used to reconstruct the untokenised text.
           */
          val wordElements = elanAlignedTokens.map({ case t => t.asFoLiA()})
          (false, true, <utt class="noSyntacticAnnotation" xml:id={s"utterance.$elan_annotatie_id"}>
            {wordElements}
          </utt>, message)
        }
        else {

          (false, false, Seq(), message)
        }
      }
    } else {
      ElanStats.nopes = ElanStats.nopes + 1
      ElanStats.nulls = ElanStats.nulls + 1
      (false, false, Seq(), "Null text field")
    }
  };

  lazy val pseudoFolia:Elem  = {
     //Console.err.println(s"###################### Generating xml for elan annotations transcriptie=$transcriptie_id, elan=$elan_annotatie_id, overlapping: ${alpinoAnnotations.size}, alpinos voor transcriptie: ${transcription.alpinoAnnotations.size}")

    if (!useAlpino) {
      // Console.err.println(enrichedContent)
    }
    lazy val speech_id = s"speech.elan.$elan_annotatie_id"
    /*
    lazy val alpinoStukje = alpinoAnnotations.map(a => {

      <div class="alpinoAnnotation" begintime={Stuff.formatTime(a.starttijd)} endtime={Stuff.formatTime(a.eindtijd)}>
        {if (a.text_lv.trim.nonEmpty) <t class="alpinoLightNormalization">{a.text_lv}</t>}
        <t class="alpinoHeavyNormalization">{a.text_zv}</t>
      </div>
    })
    */
    <speech tag={functie} speaker={alias} xml:id={speech_id}  begintime={Stuff.formatTime(starttijd)} endtime={Stuff.formatTime(eindtijd)}>
      {if (tekst_lv != null && tekst_lv.trim.nonEmpty) <t class="lightNormalization">{tekst_lv}</t>}
      {if (tekst_zv != null && tekst_zv.trim.nonEmpty) <t class="heavyNormalization">{tekst_zv}</t>}
      {enrichedContent}
      <foreign-data>{Metadata.getMetadataForElanAnnotation(elan_annotatie_id)}</foreign-data>
    </speech>
  }

  lazy val meta: Elem = {Metadata.getMetadataForElanAnnotation(elan_annotatie_id)}
  lazy val functie = (meta \\ "functie").text
  lazy val naam = (meta \\ "naam").text
  lazy val alias = (meta \\ "alias").text
  lazy val nAlpinos = alpinoAnnotations.size

  lazy val about = Map(
    "overlappingAlpinos"  -> nAlpinos,
    "useAlpino" -> useAlpino,
    "useAlignment" -> useAlignment
  )
}

