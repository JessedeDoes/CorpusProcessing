package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc

import scala.xml._

case class HitRelation(h: RelHit, r: Node)  {
  val typ = (r \ "type").text
  val sourceStart = (r \ "sourceStart").text.toInt
  val sourceEnd = (r \ "sourceEnd").text.toInt
  val targetStart = (r \ "targetStart").text.toInt
  val targetEnd = (r \ "targetEnd").text.toInt

  val sourcePosition = (sourceStart - h.start + h.nLeft, sourceEnd  - h.start + h.nLeft)
  val targetPosition = (targetStart - h.start + h.nLeft, targetEnd  - h.start + h.nLeft)

  val source = h.words.slice(sourcePosition._1, sourcePosition._2).mkString(" ")
  val target = h.words.slice(targetPosition._1, targetPosition._2).mkString(" ")

  override def toString = s"$source --$typ--> $target"
}

case class RelHit(n: Node) {
  val start = (n \ "start").text.toInt
  lazy val relations = (n \\ "relation").map(HitRelation(this, _))
  val nLeft = (n \ "left" \ "w").size
  val nMatch = (n \ "match" \ "w").size
  val nRight = (n \ "right" \ "w").size
  val words = (n \\ "w").toSeq.map(_.text)
}

/*
<hit>
<docPid>0</docPid>
<start>103</start>
<end>104</end>
<relations>
<relation>
<type>dep::nsubj</type>
<sourceStart>103</sourceStart>
<sourceEnd>104</sourceEnd>
<targetStart>100</targetStart>
<targetEnd>101</targetEnd>
</relation>
<relation>
<type>dep::obj</type>
<sourceStart>103</sourceStart>
<sourceEnd>104</sourceEnd>
<targetStart>102</targetStart>
<targetEnd>103</targetEnd>
</relation>
</relations>
<left>
<w head="11" deprel="amod" other="_" xpos="adj|prenom|basis|met-e|stan" pos="adj" wordnum="10" lemma="vermaard" deps="11:amod" punct=" " feats="degree=pos">vermaarde</w>
<w head="1" deprel="appos" other="_" xpos="n|soort|ev|basis|zijd|stan" pos="noun" wordnum="11" lemma="instelling" deps="1:appos|15:nsubj" punct=" " feats="number=sing">instelling</w>
<w head="15" deprel="nsubj" other="_" xpos="vnw|betr|pron|stan|vol|persoon|getal" pos="pron" wordnum="12" lemma="die" deps="11:ref" punct=" " feats="prontype=rel">die</w>
<w head="14" deprel="amod" other="_" xpos="adj|prenom|basis|met-e|stan" pos="adj" wordnum="13" lemma="oud" deps="14:amod" punct=" " feats="degree=pos">oude</w>
<w head="15" deprel="obj" other="_" xpos="n|soort|mv|basis" pos="noun" wordnum="14" lemma="wand_tapijt" deps="15:obj" punct=" " feats="number=plur">wandtapijten</w>
</left>
<match>
<w head="11" deprel="acl:relcl" other="spaceafter=no" xpos="ww|pv|tgw|met-t" pos="verb" wordnum="15" lemma="restaureren" deps="11:acl:relcl" punct=" " feats="verbform=fin">restaureert</w>
</match>
<right>
<w head="1" deprel="punct" other="_" xpos="let" pos="punct" wordnum="16" lemma="." deps="1:punct" punct=" " feats="_">.</w>
<w head="0" deprel="root" other="spaceafter=no" xpos="n|soort|ev|basis|zijd|stan" pos="noun" wordnum="1" lemma="web_site" deps="0:root" punct=" " feats="number=sing">Website</w>
<w head="3" deprel="punct" other="_" xpos="let" pos="punct" wordnum="2" lemma=":" deps="3:punct" punct=" " feats="_">:</w>
<w head="1" deprel="parataxis" other="_" xpos="spec|symb" pos="sym" wordnum="3" lemma="http://www.dewit.be" deps="1:parataxis" punct=" " feats="_">http://www.dewit.be</w>
<w head="3" deprel="nmod" other="_" xpos="spec|deeleigen" pos="propn" wordnum="1" lemma="Stedelijk" deps="3:nmod" punct=" " feats="_">Stedelijk</w>
</right>
</hit>
 */