package corpusprocessing.GCND

import posmapping.CGNTagset

import scala.xml.NodeSeq

object GCNDToken {
  val cliticMarker = "↪"
  val nbsp = ' '
  val emptyToken = nbsp // was _
}

import GCNDToken._
case class GCNDToken(text_zv: String, text_lv: String, joined: Boolean = false, space_after: Boolean = true, id:String="", pos: String="", lemma:String="") {



  lazy val text_lv_nonempty  = if (text_lv.isEmpty) emptyToken else text_lv
  lazy val text_zv_nonempty  = if (text_zv.isEmpty) emptyToken else text_zv
  lazy val space_after_yes_no = if (space_after) "yes" else "no"

  def cliticallyAware(): GCNDToken = if (text_lv.startsWith(cliticMarker)) this.copy(joined=true, text_lv=text_lv.replaceAll(cliticMarker,"")) else this
  def asFoLiA(): NodeSeq = (if (joined) <w space="no"><t class="cliticMarker">{cliticMarker}</t></w> else Seq()) ++ Seq(mainWord)

  lazy val mainWord =
   {
    lazy val hasLemma = lemma.nonEmpty && !text_zv.matches("\\p{P}+") && !pos.matches("LET.*")
    if (pos.nonEmpty) {
      val posTag = CGNTagset.fromString(pos)

      <w xml:id={id} space={space_after_yes_no}>
        <t class="lightNormalization">{text_lv_nonempty}</t>
        <t class="heavyNormalization">{text_zv_nonempty}</t>
        <pos class={pos} head={posTag.pos}>
          {posTag.features.filter(_.name != "UNK").map(f => {
            <feat class={f.value} subset={f.name}/>
        })}
        </pos>
        { if (hasLemma)  <lemma class={lemma}/> }
      </w>
    } else {
      <w xml:id={id} space={space_after_yes_no}>
        <t class="lightNormalization">{text_lv_nonempty}</t>
        <t class="heavyNormalization">{text_zv_nonempty}</t>
      </w>
    }
  }
}
