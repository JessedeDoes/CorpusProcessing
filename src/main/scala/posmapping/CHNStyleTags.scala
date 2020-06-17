package posmapping
import java.io.{File, PrintWriter}

import posmapping.distinctTagsFromONW.dir
import propositional.{And, Literal, Proposition}

import scala.util.matching.Regex
import scala.xml._

case class CHNStyleTag(tag: String, tagset: TagSet = null) extends Tag(tag,tagset)
{
  val Tag = new Regex("^([A-Z]+-?[A-Z]*)\\((.*?)\\)")


  val patchedTag = if (!tag.endsWith(")")) s"$tag()" else tag


  val tagToParse = if (Tag.findFirstIn(patchedTag).isEmpty) "RES(type=other)" else patchedTag.replaceAll(",other", ",type=other")

  if (Tag.findFirstIn(tagToParse).isEmpty)
  {
    Console.err.println(s"PAS OP Mislukte tag: $tagToParse")
  }

  val Tag(pos,feats) = tagToParse

  val featureValues:Array[String] = feats.split("\\s*,\\s*").filter(f => !(f.trim == ""))

  val features:List[Feature] = featureValues.map(parseFeature).toList ++ List(Feature("pos", pos))

  import scala.util.{Try, Success, Failure}

  def parseFeature(f: String) = {
    val x = f.split("=")
    Try(Feature(x(0),x(1))) match
      {
      case Success(f) if f.value == "auxiliary" => f.copy(value="aux/cop") // HM.....
      case Success(f) if f.value == "indefinite" => f.copy(value="indef") // HM.....
      case Success(f) => f
      case _ => Feature("kwip", f)
    }
  }

  def hasUnforcedFeatures() = {
    val alternatives = features.filter(_.name != "pos").map(f => {
      val f0 = features.filter(_ != f)
      val t0 = this.copy(tag = this.toString(f0))
      f.name -> t0
    }).filter(t => tagset.tagSatisfiesImplications(t._2)._1)
    alternatives
  }

  def removeUnforcedFeatures(): CHNStyleTag = {
    val fS = hasUnforcedFeatures().map(_._1)
    val newFeatures = features.filter(f => !fS.contains(f.name))
    this.copy(tag =  this.toString(newFeatures))
  }

  def normalizeFeatureValue(n: String, v: String): String= {
    if (tagset == null) v else {
      val p = tagset.partitions.getOrElse(n,List())
      val values = v.split("\\|")
      val lijstje = p.zipWithIndex.toMap
      val additions = values.filter(f => !lijstje.contains(v)).zipWithIndex.map({ case (x, i) => (x, lijstje.size + i) }).toMap

      def combi = lijstje ++ additions
      //println(n + " " + combi)
      Try {values.sortBy(x => combi(x)).mkString("|") } match {
        case Success(z) => z
        case _ => v
      }
    }
  }

  lazy val features_sorted = if (tagset == null) features else {
    val pos = features.find(_.name == "pos").map(_.value).getOrElse("UNK")
    val lijstje: Map[String, Int] = tagset.pos2partitions.getOrElse(pos, List()).zipWithIndex.toMap
    val additions = features.filter(f => !lijstje.contains(f.name)).zipWithIndex.map({case (x,i) => (x.name,lijstje.size + i)}).toMap
    def combi = lijstje ++ additions
    //println(combi)
    features.sortBy(x => combi(x.name))
  }

  override def toString: String = s"$pos(${features_sorted.filter(_.name != "pos").map(f => s"${f.name}=${normalizeFeatureValue(f.name,f.value)}").mkString(",")})"
  def toString(f: List[Feature]): String = s"$pos(${f.filter(_.name != "pos").map(f => s"${f.name}=${normalizeFeatureValue(f.name,f.value)}").mkString(",")})"
  def proposition:Proposition = And(features.map({case Feature(n,v) => Literal(s"${tagset.prefix}:${if (n != "pos") "feat." else ""}$n=$v") } ) :_*)
}

object CHNStyleTags {

  def tagsetFromSetOfStrings(prefix: String, s: Set[String]) =
  {
    val tags = s.map(s => CHNStyleTags.parseTag(s))
    TagSet.tagsetFromSetOfTags(prefix,tags)
  }

  def parseTag(s: String): Tag = new CHNStyleTag(s)

  object parser extends TagParser {
    def parseTag(ts: TagSet, s: String)  = CHNStyleTags.parseTag(s)
  }

  val gysTagset = TagSet.fromXML("data/CRM/gys.tagset.xml")

  //val onwTagset = TagSet.fromXML("data/ONW/onw.tagset.xml").copy(parser=CHNStyleTags.parser)
}

object distinctTagsFromGysseling
{
  def main(args: Array[String]): Unit = {
    val dir = args(0)

    val attribute = "pos"
    tagsetFromCorpusFiles(dir, attribute)
  }


  def tagsetFromCorpusFiles(dirName: String, attribute: String,
                            separator: String = "[+|]", prefix: String = "/tmp/") =
    TagsetDiachroonNederlands.tagsetFromCorpusFiles(dirName, attribute,separator, prefix)
}

object distinctTagsFromCRM {
  val dir = corpusprocessing.CRM.Settings.dir + "PostProcessedMetadata"

  def main(args: Array[String]): Unit = {
    distinctTagsFromGysseling.tagsetFromCorpusFiles(dir, "pos", "[+]")
  }
}


object distinctTagsFromONW {
  val dir = "/home/jesse/workspace/data-historische-corpora/ONW/ONW-processed-metadata"

  def main(args: Array[String]): Unit = {
    distinctTagsFromGysseling.tagsetFromCorpusFiles(dir, "msd", "[+]")
  }
}

object distinctTagsFromBaB
{
  val dir = brievenalsbuit.Settings.output.getCanonicalPath

  def main(args: Array[String]): Unit = {
    distinctTagsFromGysseling.tagsetFromCorpusFiles(dir, "pos")
  }
}

object distinctTagsFromMolex
{
  val file = "data/Molex/fakeDocument.xml"

  def main(args: Array[String]): Unit = {
    distinctTagsFromGysseling.tagsetFromCorpusFiles(file, "pos")
  }
}


object compareGysselingToMolex
{

  def pretty(tagset: TagSet, pw: PrintWriter = new PrintWriter(System.out)): Unit = {
    //val xmlWriter = new PrintWriter(System.out)
    pw.println(TagSet.pretty.format(tagset.toXML()))
    pw.close()
  }

  val molexLemmaTagset = TagSet.fromXML("data/Molex/lemma_tagset.xml")
  val molexCompleteTagset = TagSet.fromXML("data/Molex/combined_tagset.xml")
  val gysTagset = TagSet.fromXML("data/CG/tagset_met_valuerestricties.xml")
  val molexCompleteTagsetPlus = molexCompleteTagset.copy(descriptions = molexLemmaTagset.descriptions)

  def main(args: Array[String]): Unit = {
    pretty(molexCompleteTagsetPlus)
    pretty(molexCompleteTagsetPlus, new PrintWriter("/tmp/tagset_plus.xml"))
    //println(gysTagset.descriptions)
  }
}

object addDescriptionsToCorpusBasedGysseling
{
  def main(args: Array[String]): Unit = {
    val corpusBased = TagSet.fromXML("data/CG/tagset_from_corpus.xml")

    val molexTagset = TagSet.fromXML("data/Molex/combined_tagset_desc.xml")
    val gysselSet = TagSet.fromXML("data/CG/tagset_met_valuerestricties.xml")

    //println(molexTagset.displayNames) // jakkes die hebben we niet ....

    val corpusBasedWithDesc = corpusBased.copy(descriptions = molexTagset.descriptions,
      displayNames = TagSet.mergeDescriptions(molexTagset.displayNames, molexTagset.descriptions))

    val nogWat = corpusBasedWithDesc.copy(displayNames = TagSet.mergeDescriptions(molexTagset.displayNames, gysselSet.displayNames))
    //val molexWithDesc = molexTagset.copy(displayNames = TagSet.mergeDescriptions(gysselSet.displayNames, molexTagset.descriptions))

    compareGysselingToMolex.pretty(nogWat, new PrintWriter("/tmp/gys_corpus_tagset_desc.xml"))

    val jsonWriter = new PrintWriter("/tmp/gys_corpus_tagset_desc.json")
    jsonWriter.println(nogWat.asJSON)
    jsonWriter.close()

    val blfWriter = new PrintWriter("/tmp/gys_corpus_tagset_desc.bl.yaml")
    blfWriter.println(nogWat.forBlacklab)
    blfWriter.close()
    //compareGysselingToMolex.pretty(molexWithDesc, new PrintWriter("/tmp/molex_tagset_displayNames.xml"))
  }
}