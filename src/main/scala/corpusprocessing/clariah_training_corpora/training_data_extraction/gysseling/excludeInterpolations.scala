package corpusprocessing.clariah_training_corpora.training_data_extraction.gysseling

import corpusprocessing.clariah_training_corpora.training_data_extraction.extract_training_data_trait
import posmapping.TagsetDiachroonNederlands
import utils.PostProcessXML

import scala.xml._
import utils.PostProcessXML._

import scala.collection.immutable
object excludeInterpolations {
  lazy val docje = XML.load("/mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/TeIndexeren/2020_07_31/3000.tei.xml")


  def exclude(e: Elem)  = Exclusion(e)()

  def main(args: Array[String]) = {
    // println(String.format("%05d", 100.asInstanceOf[Object]))

    //XML.save("/tmp/docje.xml", d1)
    val d2 = Exclusion(docje)()
    XML.save("/tmp/docje.xml", d2)
  }
}

case class Exclusion(docje: Elem)
{
  lazy val d1 = numberNodes(docje)._1.asInstanceOf[Elem]
  lazy val idIndex = (d1.descendant.filter(_.isInstanceOf[Elem])).map(e => e.attributes.filter(_.key == "id").map(_.value.text).mkString("") -> e).toMap.filter(_._1.nonEmpty)
  lazy val everything = d1.descendant.toArray
  lazy val tokenElements = everything.filter(x => Set("pc","w").contains(x.label))
  lazy val positionIndex = everything.zipWithIndex.map{case (t, i) => (t \ "@position").text -> i}.toMap
  def f(n: Int): String  = String.format("%08d", n.asInstanceOf[Object])
  def numberNodes(n: Node, k: Int=0): (Node,Int)  = {
    val (newChildren,kk) = n.child.foldLeft[(Seq[Node], Int)](Seq[Node]()->(k+1))({case ((seq, n),node) =>
       val (n1,k1) =  numberNodes(node,n)
       (seq :+ n1) -> k1
    })
    val n1 = n match {
      case e: Elem => e.copy(child=newChildren, attributes = e.attributes.append(new UnprefixedAttribute("position", f(k), Null)))
      case _ => n
    }
    n1 -> (kk)
  }

  case class Span(bibl: Node, from: String, to: String) {
    lazy val properties = (bibl \\ "interpGrp").map(g => {
      val name = (g \ "@type").text
      val value = (g \ "interp").text
      name -> value
    }).filter(_._2.nonEmpty)

    lazy val dateringen = properties.filter(_._1.contains("itnessYearLevel0"))
    lazy val hasDate = dateringen.nonEmpty
    lazy val startstone = idIndex(from)
    lazy val endstone = idIndex(to)

    lazy val later = dateringen.exists(_._2 > "1300")
    override def toString = s"$from-$to: $startPosition-$endPosition, $dateringen:  $wordsIn "

    lazy val startPosition = (startstone \ "@position").text
    lazy val endPosition = (endstone \ "@position").text

    lazy val wordsElementsIn = {
      lazy val s = positionIndex(startPosition)
      lazy val e = positionIndex(endPosition)
      val z = everything.slice(s,e+1)
      z.lastOption.foreach(x => {
        Console.err.println( s"Exclusion range: $s:$startPosition:${z.head \ "@position"}  --- $e:$endPosition:${x \ "@position"}")
      })
      z.filter(x => Set("w", "pc").contains(x.label)).toSet
    }

    lazy val wordsIn = wordsElementsIn.map(x => (x \ "seg").text).mkString(" ")
  }

  def isSupplied(w: Elem)  = {
    (w \ "seg").text.trim == (w \ "seg" \ "supplied").text.trim && ((w \ "@type").text == "999")
  }
  def apply(): Elem = {

    Console.err.println(s"Start exclusions.... ${docje.descendant.size}=?${everything.size}, Tokens: ${tokenElements.size}")
    val e1 = d1
    val biblz = (e1 \ "teiHeader" \\ "bibl").filter(b => (b \\ "span").nonEmpty)

    val spans = biblz.flatMap(b => {
      (b \\ "span").map(s => Span(b, (s \ "@from").text.replaceAll("#",""), (s \ "@to").text.replaceAll("#","")) )
    }).toSet

    lazy val allSpans = (e1 \ "teiHeader" \\ "span")
    val laterSpans =  spans.filter(_.hasDate).filter(_.later)
    Console.err.println(s"Spans: span elements: ${allSpans.size}, spans:${spans.size}, date after 1300: ${laterSpans.size}")

    val forbiddenPositions: Set[String] = laterSpans.flatMap(_.wordsElementsIn).map(x => (x \ "@position").text)
    Console.err.println(s"Excluded tokens: ${forbiddenPositions.size}")
    val r= PostProcessXML.updateElement(d1, x => Set("pc", "w").contains(x.label), w => {
      if (forbiddenPositions.contains((w \ "@position").text) || isSupplied(w)) w.copy(label = "ex_" + w.label) else w
    })
    Console.err.println("....End exclusions")
    r
  }

}

object gysseling_to_huggingface extends extract_training_data_trait {
  override val split_test_train_on_document_level: Boolean = true
  override lazy val output_prefix: String = "gys"
  override val max_files: Int = Integer.MAX_VALUE // 500
  override lazy val output_folder: String = "/mnt/Projecten/Corpora/TrainingDataForTools/Gysseling/All/"
  override lazy val default_folder = "/mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/TeIndexeren/2020_07_31/"
}

object gysseling_to_huggingface_core extends extract_training_data_trait {
  override def tagMapping(s: String) = TagsetDiachroonNederlands.mapMultipleTagToCore(s).replaceAll("position=prenom.postnom.pred","position=uncl")
  override val split_test_train_on_document_level: Boolean = true
  override lazy val output_prefix: String = "gys"
  override val max_files: Int = Integer.MAX_VALUE // 500
  override lazy val output_folder: String = "/mnt/Projecten/Corpora/TrainingDataForTools/Gysseling/Core/"
  override lazy val default_folder = "/mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/TeIndexeren/2020_07_31/"
}
object gysseling_to_huggingface_filtered extends extract_training_data_trait {

  override val training_subsets: Int = 1

  import corpusprocessing.clariah_training_corpora.training_data_extraction.Partition
  override def preprocess(x: Elem) = excludeInterpolations.exclude(x)

  val unfiltered = "/mnt/Projecten/Corpora/TrainingDataForTools/Gysseling/All/"

  lazy val partitionsFromLists = new java.io.File(unfiltered).listFiles(_.getName.endsWith("filenames")).flatMap(f => {
    val files = io.Source.fromFile(f).getLines.toSet
    val partition = f.getName.replaceAll(".*(test|train|dev).*","$1")
    files.map(f => f -> Partition(partition, -1))
  }).toMap
  override def pickPartition(f: Option[String]): Partition =
    if (f.isEmpty || !partitionsFromLists.contains(f.get)) super.pickPartition(f) else {
      val z = partitionsFromLists(f.get)
      Console.err.println(s"##### ${f.get}: ${z.prefix} #####")
      z
    }
  override val split_test_train_on_document_level: Boolean = true
  override lazy val output_prefix: String = "gys"
  override val max_files: Int = Integer.MAX_VALUE // 500
  override lazy val output_folder: String = "/mnt/Projecten/Corpora/TrainingDataForTools/Gysseling/AllFiltered/"
  override lazy val default_folder = "/mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/TeIndexeren/2020_07_31/"
}