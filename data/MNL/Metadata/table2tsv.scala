import scala.xml._

/*
 *
 * NOU-C(number,case,gender,inflection)	case & gender alleen bij number=sg
NOU-P(number,case,gender,inflection)	case & gender alleen bij number=sg
AA(degree,position,case,inflection)
VRB(finiteness,tense,mood,NA,inflection)	tense,mood en NA alleen bij finiteness=fin
NUM(type,position,representation,case,inflection)	case alleen bij position=prenom
PD(type,subtype,position,case,person,number,gender,NA,GA,inflection)	number komt alleen bij bez.vnw. samen met NA en GA voor
ADP(type)
CONJ(type)
ADV(type,subtype)	Subtype is toegevoegd in U519a en U537a
INT()
RES(type)
*/

object table2tsv {


def main(args: Array[String]) = {
  val doc = XML.load(args(0))
(doc \\ "tr").foreach(r => 
{
  val cells = (r \\ "td").map(_.text.trim.replaceAll("\\s+"," "))
  println(cells.mkString("\t"))
})
}
}
