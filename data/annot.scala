import scala.xml._

case class LemPos(lemma: String, pos: String) { override def toString = s"$lemma:$pos" } 
case class TokenRef(tokenId: String, position: Int) {
  override def toString = s"$tokenId/$position"
}

implicit  def tokref(id: String): TokenRef = TokenRef(id,0)

case class Annotation(members: List[LemPos], tokenRefs: Set[TokenRef]) { 

override def toString = s"${members.map(_.toString).mkString(";")} -> {${tokenRefs.mkString(";")}}"

def toNaf:NodeSeq  = members.toSeq.map(m =>
  <term lemma={m.lemma} pos={m.pos}>
     <span>
        {
          tokenRefs.map(tr => <target id={tr.tokenId}/>)
        }
     </span>
  </term>)
} 

def simpleAnnotation(lemma: String, pos: String, tokenRef: TokenRef):Annotation = 
  Annotation(List(LemPos(lemma,pos)), Set(tokenRef))

def multiToken(lemma: String, pos: String, tokenRefs: Set[TokenRef]): Annotation = 
  Annotation(List(LemPos(lemma,pos)),tokenRefs)

case class Token(id: String, content: String) {}

val s1 = List(Token("t00", "zagik"))
val s2 = List(Token("t10", "bel"), Token("t11","hem"), Token("t12","op"))
val s3 = List(Token("t20", "bellem"), Token("t21","eens"), Token("t22","op"))

val a1_0 = Annotation(List(LemPos("zien", "ww"), LemPos("ik" , "vnw")), Set("t00"))
val a1_1 = List(simpleAnnotation("zien","ww","t00"), simpleAnnotation("ik","vnw","t00"))

val a3_flat = List(
  multiToken("opbellen", "ww", Set(TokenRef("t20",0), "t22")),
  simpleAnnotation("hij", "vnw", TokenRef("t20",1)),
  simpleAnnotation("eens", "bw", "t21")
)

val a3_xml:NodeSeq = a3_flat.toSeq.flatMap(_.toNaf)

println(s"""
s1 = $s1
a1_0 = $a1_0
a1_1 = $a1_1

s3 = $s3
Annotatie: $a3_flat
XML: 
  ${a3_xml}
