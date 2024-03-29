package corpusprocessing.delpher

import scorpusprocessing.delpher.KBKwic
import utils.LexiconService
import corpusprocessing.delpher._

import scala.xml._

trait TextQuery
{
	def toQueryString():String = this match {
			case SingleTerm(s) => s
			case And(t1,t2) => "(" + t1.toQueryString + "+AND+" + t2.toQueryString + ")"
			case Or(t1,t2) => "(" + t1.toQueryString + "+OR+" + t2.toQueryString + ")"
			case Disjunction(l @ _*) => "(" + l.map(_.toQueryString).mkString("+OR+")  + ")"
      case Conjunction(l @ _*) => "(" + l.map(_.toQueryString).mkString("+AND+")  + ")"
			case ListDisjunction(l) => "(" + l.map(_.toQueryString).mkString("+OR+")  + ")"
			case ListConjunction(l) => "(" + l.map(_.toQueryString).mkString("+AND+")  + ")"
			case Phrase(l @ _*) => "%22" + l.map(_.toQueryString).mkString("+")  + "%22"
			}

  def toXML(): NodeSeq = this match  {
    case SingleTerm(s) => <t>{s}</t>
    case And(t1,t2) => <and>{List(t1,t2).map(t => t.toXML())}</and>
    case Or(t1,t2) => <or>{List(t1,t2).map(t => t.toXML())++ Seq(Text("\n"))}</or>
    case Disjunction(l @ _*) => <or>{l.map(t => t.toXML)}++ Seq(Text("\n"))</or>
    case Conjunction(l @ _*) => <and>{l.map(t => t.toXML)}</and>
    case ListDisjunction(l) => <or>{l.map(t => t.toXML ++ Seq(Text("\n")))}</or>
    case ListConjunction(l) => <and>{l.map(t => t.toXML)}</and>
    case Phrase(l @ _*) => <phrase>{l.map(t => t.toXML)}</phrase>
  }

  def minimumPhraseLength() : Int = this match  {
    case SingleTerm(s) => 1
    case And(t1,t2) => Math.min(t1.minimumPhraseLength(), t2.minimumPhraseLength())
    case Or(t1,t2) =>  Math.min(t1.minimumPhraseLength(), t2.minimumPhraseLength())
    case Disjunction(l @ _*) => l.map(t => t.minimumPhraseLength()).min
    case Conjunction(l @ _*) => l.map(t => t.minimumPhraseLength()).min
    case ListDisjunction(l) => l.map(t => t.minimumPhraseLength()).min
    case ListConjunction(l) => l.map(t => t.minimumPhraseLength()).min
    case Phrase(l @ _*) => l.map(t => t.minimumPhraseLength()).sum
  }

  def phrasesIn() : List[Phrase] = this match {
    case SingleTerm(s) => List(Phrase(this.asInstanceOf[SingleTerm]))
    case Phrase(l @ _*) => List(Phrase(l : _*))
    case Or(t1,t2) => t1.phrasesIn() ++ t2.phrasesIn()
    case And(t1,t2) => t1.phrasesIn() ++ t2.phrasesIn()
    case ListDisjunction(l) => l.flatMap(_.phrasesIn())
    case ListConjunction(l) => l.flatMap(_.phrasesIn())
    case Disjunction(l) => List(l).flatMap(_.phrasesIn())
    case Conjunction(l) => List(l).flatMap(_.phrasesIn())
  }
}

case class SingleTerm(term:String) extends TextQuery
case class And(t1:TextQuery, t2:TextQuery) extends TextQuery
case class Or(t1:TextQuery, t2:TextQuery) extends TextQuery

case class Phrase(l: SingleTerm*) extends TextQuery {
  def matches(s : Seq[String]) : Boolean =  {
    val terms = l.map( { case SingleTerm(t) => t } )
    //Console.err.println(s"$terms ?=? $s")
    terms.size == s.size &&
      terms.zip(s).forall({case (p,x) => x.toLowerCase.contains(p.toLowerCase)})
  }
}

case class Disjunction(l: TextQuery*) extends TextQuery
case class Conjunction(l: TextQuery*) extends TextQuery
case class ListDisjunction(l: List[TextQuery]) extends TextQuery
case class ListConjunction(l: List[TextQuery]) extends TextQuery

// http://sk.taalbanknederlands.inl.nl/LexiconService/lexicon/get_wordforms?database=lexicon_service_db&lemma=bunzing

trait ContentQueryT
{
	def startDate:String
	val endDate:String
	val textQuery:TextQuery
	def toParameterValue():String ="date+within+%22" + startDate + "+" + endDate + "%22+AND+" + textQuery.toQueryString
}

trait SRUQueryT
{
	def server:String 
	def operation:String
	def collection:String 
	def startRecord:Int 
	def maximumRecords:Int
	def query:ContentQuery
  def recordSchema:String = "ddd"
	def mkURL(): String =
	server + "&operation=" + operation +
    "&recordSchema=" + recordSchema +
    "&x-collection=" + collection +
    "&startRecord=" + startRecord +
    "&maximumRecords=" + maximumRecords +
    "&query=" + query.toParameterValue()
}

case class ContentQuery(startDate:String, 
    endDate:String, textQuery:TextQuery) extends ContentQueryT
    
case class SRUQuery(server:String, 
    operation:String, collection:String, 
    startRecord:Int, maximumRecords:Int, query:ContentQuery) extends SRUQueryT

object SRU
{
   implicit def wrapTextQuery(t:TextQuery):SRUQuery = 
          SRUQuery(QueryKB.defaultServer, "searchRetrieve", 
             QueryKB.defaultCollection, 0, QueryKB.maxDocuments, 
             ContentQuery(QueryKB.defaultStartDate, QueryKB.defaultEndDate, t))
             
  def singleWordQuery(term:String):SRUQuery = wrapTextQuery(SingleTerm(term))
  implicit def StringToTerm(s:String):SingleTerm = SingleTerm(s)
  implicit def StringToQuery(s:String):SRUQuery = singleWordQuery(s)
  
  def termsIn(t: TextQuery):Set[String] = 
  {
     t match 
    {
      case SingleTerm(term0) => {
        val term = term0.replaceAll(""""""","") ; if (false && (term.contains("|") || term.contains(" ") || term.contains("+"))) term.split("[| +]").toSet
        else  Set(term)
      }
      case And(t1,t2)  => termsIn(t1) ++ termsIn(t2) 
      case Or(t1,t2) => termsIn(t1) ++ termsIn(t2)
      case Disjunction(l @ _*) => l.flatMap(termsIn).toSet
      case Conjunction(l @ _*) => l.flatMap(termsIn).toSet
      case ListDisjunction(li) => li.flatMap(termsIn).toSet
      case ListConjunction(li) => li.flatMap(termsIn).toSet
      case Phrase(l @ _*) => l.flatMap(termsIn).toSet
    }
  }

  def makePhrase(l: Seq[String]) = if (l.size ==1 ) SingleTerm(l.head) else Phrase(l.map(w => SingleTerm(w)) : _*)

  def termsIn(t: Seq[TextQuery]):Set[String] = t.flatMap(termsIn).toSet

   def toString(s: SRUQuery) = termsIn(s.query.textQuery).mkString("_")

   def cartesianProduct[A](l: List[List[A]]):List[List[A]] =
   {
      l match
      {
         case head :: tail if tail.size > 0 => head.flatMap(x =>  cartesianProduct(tail).map(y => x :: y))
         case head :: tail if tail.size == 0 => head.map ( a => List(a) )
         case _ => List.empty
      }
   }
   
  def expandQuery(f: String => List[String])(t: TextQuery):TextQuery = 
  {
	  val expand:TextQuery=>TextQuery = expandQuery(f)
    t match 
    {
      case SingleTerm(term) =>
        val l = f(term)
        val l1 = if (l.contains(term.toLowerCase)) l else term.toLowerCase :: l
        ListDisjunction(l1.map( x => SingleTerm(x)))
      case And(t1,t2) => And(expand(t1),expand(t2))
      case Or(t1,t2) => Or(expand(t1),expand(t2))
      case Disjunction(l @ _*) => Disjunction(l.map(expand):_*)
      case Conjunction(l @ _*) => Conjunction(l.map(expand):_*)
      case ListDisjunction(li) => ListDisjunction(li.map(expand))
      case ListConjunction(li) => ListConjunction(li.map(expand)) 
      case Phrase(l @ _*) => // pfft. dit moet toch simpeler kunnen? In ieder geval zou je beter de expansies filteren op voorkomen; of de lijst in queries splitsen
          val x = cartesianProduct(l.toList.map( { case SingleTerm(t) => f(t) }))
          val y = x.map(s =>  { val l = s.map(x => SingleTerm(x)); Phrase(l:_*) } )   
          ListDisjunction(y) 
    }
  }
  def fromXML(e: Elem): TextQuery =  {
    val childElems = e.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem]).toList
    e.label match {
    case "t" => SingleTerm(e.text)
    case "and" => ListConjunction(childElems.map(fromXML))
    case "or" => ListDisjunction(childElems.map(fromXML))
    case "phrase" => Phrase(childElems.map(x => fromXML(x).asInstanceOf[SingleTerm]) : _*)
  } }

}

object testSRU
{
   import SRU._
   val expand: String => List[String] = s => LexiconService.getWordforms(s)
   val expandRestricted: String => List[String]  = x => LexiconService.getWordforms(x).filter(QueryKB.getNumberOfResults(_) > 0)

   def main(args:Array[String]) =
   {
      val t0 = Phrase("goed", "gedachte")
      val t00 = Phrase("ernstig", "probleem")
      
      val t1 = expandQuery(expandRestricted)(t00)
      println(t1)
      println(termsIn(t1))
      println(t1.toQueryString())
      println(QueryKB.getNumberOfResults(t1))
      KBKwic.kwicResultsPar(t1)
   }
}
    
