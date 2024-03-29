package utils

import java.net.URLEncoder

import corpusprocessing.delpher.serpens.BeestenZoeker

import scala.util.{Failure, Success, Try}
import scala.xml._

case class LexiconService(serviceURL: String, database: String)
{
  def mapPoS(s: String) =
  {
    if (database == "molex" && s == "NOU") "NOU-C" else
    if (database == "molex" && s == "ADJ") "AA" else s
  }

  def getWordforms(lemma: String, pos: Option[String] = None):List[String] =
  {
    val posPart = pos.map(mapPoS).map(x => s"&pos=$x").getOrElse("")
    val encoded = URLEncoder.encode(lemma.toLowerCase())
    val request = s"$serviceURL/get_wordforms?database=$database&lemma=" + encoded + posPart

    val result = Try((XML.load(request)
      \\ "found_wordforms").toList.map(_.text.toLowerCase).filter(_.matches("^(\\p{L})*$")).filter(!BeestenZoeker.onBeest(_))) match {
      case Success(value) => value
      case Failure(e) => {e.printStackTrace(); List()}
    }
    Console.err.println("LS:" + request + " ==> " + result)
    result
  }

  def getLemmata(wordform: String):List[(String,String)] =
  {
    val encoded = URLEncoder.encode(wordform.toLowerCase())
    (XML.load(s"$serviceURL/get_lemma?database=$database&wordform=" + encoded)
      \\ "found_lemmata").toList.map(e => ((e \ "lemma").text, (e \ "pos").text))
  }
}

object defaults
{
  val serviceURL = "http://sk.taalbanknederlands.inl.nl/LexiconService/lexicon"
  val database = "lexicon_service_db"
}

object LexiconService extends LexiconService(defaults.serviceURL, defaults.database)
{
   def main(args: Array[String]) : Unit =
   {
     val searchTerm = args(0)
     getWordforms(searchTerm).foreach(println)
     getLemmata(searchTerm).foreach(println)
   }
}

object MolexService extends LexiconService(defaults.serviceURL, "molex")
{
  def main(args: Array[String]) : Unit =
  {
    getWordforms("klagen").foreach(println)
    getLemmata("kipt").foreach(println)
  }
}

object LexicaService 
{
  def main(args: Array[String]) : Unit =
  {
    val searchTerm = args(0)
    (LexiconService.getWordforms(searchTerm) ++ MolexService.getWordforms(searchTerm)).foreach(println)
    (LexiconService.getLemmata(searchTerm) ++ MolexService.getLemmata(searchTerm)).foreach(println)
  }
}

object HilexMolexService {
  def getWordforms(lemma: String, pos: Option[String] = None):List[String] =
  {
    (MolexService.getWordforms(lemma, pos)++ LexiconService.getWordforms(lemma,pos)).toSet.toList
  }

  def getLemmata(wordform: String):List[(String,String)] =
  {
    (MolexService.getLemmata(wordform)++ LexiconService.getLemmata(wordform)).toSet.toList
  }
}
