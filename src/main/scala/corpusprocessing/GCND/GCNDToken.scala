package corpusprocessing.GCND

import posmapping.CGNTagset

case class GCNDToken(text_zv: String, text_lv: String, joined: Boolean = false, space_after: Boolean = true, id:String="", pos: String="", lemma:String="") {
  def asFoLiA() = {

    lazy val hasLemma = lemma.nonEmpty && !text_zv.matches("\\p{P}+") && !pos.matches("LET.*")
    if (pos.nonEmpty) {
      val posTag = CGNTagset.fromString(pos)

      <w xml:id={id} space={space_after.toString}>
        <t class="lightNormalization">{text_lv}</t>
        <t class="heavyNormalization">{text_zv}</t>
        <pos class={pos} head={posTag.pos}>
          {posTag.features.filter(_.name != "UNK").map(f => {
            <feat class={f.value} subset={f.name}/>
        })}
        </pos>
        { if (hasLemma)  <lemma class={lemma}/> }
      </w>
    } else {
      <w xml:id={id} space={space_after.toString}>
        <t class="lightNormalization">{text_lv}</t>
        <t class="heavyNormalization">{text_zv}</t>
      </w>
    }
  }
}
