package posmapping

import java.io

import org.json4s
import propositional.Proposition

import scala.xml._
import org.json4s.JsonDSL._
import org.json4s.Xml.{toJson, toXml}
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization.writePretty
import posmapping.CHNStyleTags.parser
import sparql2xquery.ValueRestrictionSet

import scala.collection.immutable
//import scala.Predef.Map


case class Feature(name: String, value: String)
{
  override def toString: String = s"$name=$value"
}

abstract class Tag(tag: String, tagset: TagSet)
{
  val pos:String
  val features:List[Feature]
  def parseFeature(f:String):Feature
  def proposition:Proposition
}

trait TagParser
{
  def parseTag(ts: TagSet, t: String): Tag
}

object  defaultTagParser extends TagParser
{
  override def parseTag(ts: TagSet, t: String): Tag = new CGNStyleTag(t, ts)
}


object MolexTagSet extends TagSet("molex")
{
  override def fromProposition(p:Proposition, posIsSpecial: Boolean = false):Tag = UDTagSet.fromProposition(p)
  override def fromString(t: String): Tag = new UDStyleTag(t, this)
}

case class TagDescription(posDescriptions: scala.collection.immutable.Map[String, String] = Map.empty,
                          featureDescriptions: scala.collection.immutable.Map[String, String] = Map.empty,
                          valueDescriptions: scala.collection.immutable.Map[(String, String), String] = Map.empty )
{
  def posDescription(p: String) = posDescriptions.getOrElse(p, p)
  def featureDescription(p: String) = featureDescriptions.getOrElse(p, p)
  def valueDescription(f: String, v: String) = valueDescriptions.getOrElse((f,v), v)
  def getPosDescription(p: String) = posDescriptions.getOrElse(p, p)
  def getFeatureDescription(p: String) = featureDescriptions.getOrElse(p, p)
  def getValueDescription(f: String, v: String) = valueDescriptions.getOrElse((f,v), v)
}

object TagSet
{
  val pretty = new scala.xml.PrettyPrinter(300, 4)

  def fromXML(d: Elem):TagSet =
  {
    val prefix = (d \ "prefix").text

    val posTags = (d \\ "pos").map(_.text.trim).toSet.toList

    val partitions: Map[String, List[String]] = (d \\ "partitions" \ "feature").map(f => (f \ "name").text -> (f \\ "value").map(_.text).toList ).toMap

    val pos2partitions: Map[String, List[String]] = (d \\ "constraints" \ "constraint").map(f => (f \ "pos").text -> (f \\ "feature").map(_.text).toList ).toMap

    val valueRestrictions: List[ValueRestriction] = (d \\ "partitions" \ "feature").flatMap(
      f => {
        val name = (f \ "name").text
        val valpos = (f \\ "featureValue").flatMap(fv => (fv \\ "pos").map(p => ValueRestriction(p.text, name,   List((fv \ "value").text)) ))
        //Console.err.println(s"valpos: $valpos")
        val regrouped = valpos.groupBy(v => (v.pos, v.featureName)).map({case ((p,n),l) => ValueRestriction(p,n, l.flatMap(x => x.values).toList)})
        regrouped
      }
    ).toList

    // Console.err.println(s"Value restrictions = $valueRestrictions")

    val posDescriptions: Map[String, String] = (d \\ "mainPoS" \ "pos").map(p => p.text -> (p \ "@desc").text).toMap
    val featureDescriptions: Map[String, String] = (d \\ "partitions" \ "feature").map(f => (f \ "name").text -> (f \ "@desc").text).toMap

    val valueDescriptions: Map[(String, String), String] = (d \\ "partitions" \ "feature").flatMap(f => {
      val name = (f \ "name").text
      val valuedescs = (f \\ "featureValue").map(fv =>  ( (name, (fv \ "value").text), (fv \ "@desc").text))
      valuedescs
    }).toMap

    val posDisplayNames: Map[String, String] = (d \\ "mainPoS" \ "pos").map(p => p.text -> (p \ "@displayName").text).toMap
    val featureDisplayNames: Map[String, String] = (d \\ "partitions" \ "feature").map(f => (f \ "name").text -> (f \ "@displayName").text).toMap

    val valueDisplayNames: Map[(String, String), String] = (d \\ "partitions" \ "feature").flatMap(f => {
      val name = (f \ "name").text
      val valuedescs = (f \\ "featureValue").map(fv =>  ( (name, (fv \ "value").text), (fv \ "@displayName").text))
      valuedescs
    }).toMap

    val descriptionsAndDisplayNames = TagDescription(posDisplayNames, featureDisplayNames, valueDisplayNames)

    val implications = Implication.fromXML(d)

    // if (dp.posDescription("PD").nonEmpty) println(dp)

    TagSet(prefix, posTags, partitions, pos2partitions, defaultTagParser, List(), valueRestrictions,
      TagDescription(posDescriptions, featureDescriptions, valueDescriptions), descriptionsAndDisplayNames, implications=implications
      ) // tag parser should also be specified in XML
  }

  def fromXML(f: String):TagSet = fromXML(XML.load(f))

  def fromJSON(json: String) = {
    val xml = <tagset>{toXml(parse(json))}</tagset>
    println(pretty.format(xml))
  }

  def tagsetFromSetOfTags(prefix: String, tags: Set[Tag]) = {
    val posTags = tags.map(_.pos).toList
    val partitions: Map[String,List[String]] = tags.flatMap(_.features).groupBy(_.name).mapValues(x => x.map(f => f.value).toList)
    val pos2partitions: Map[String, List[String]] = tags.groupBy(_.pos).mapValues(l => l.flatMap(p => p.features.map(f => f.name))).mapValues(s => s.toList)
    val valueRestrictions:List[ValueRestriction] =
      tags.groupBy(_.pos).flatMap(
        { case (p,thisPos) => {
          val features = thisPos.flatMap(_.features.toSet.toList)
          features.groupBy(_.name).mapValues(f => f.map(_.value)).map({case (fn,l) => ValueRestriction(p, fn, l.toList)})
        } }
      ).toList

    TagSet(prefix, posTags, partitions, pos2partitions, parser, List(), valueRestrictions, listOfTags = tags)
  }

  def compare(t1: TagSet, t2: TagSet): Unit =
  {
    val pos_not_in_t1 = t2.posTags.diff(t1.posTags)

    val featureNames_t1 = t1.partitions.keySet
    val featureNames_not_in_t1 = t2.partitions.keySet.diff(t1.partitions.keySet)
    val featureValues_t1 = t1.partitions.toList.flatMap{x => x._2.map(y => (x._1, y))}
    val featureValues_t2 = t2.partitions.toList.flatMap{x => x._2.map(y => (x._1, y))}
    val featureValues_t1_split = t1.partitions.toList.flatMap{ x => x._2.flatMap(y => y.split("\\|").map(y0 => (x._1, y0)))}.toSet
    val featureValues_not_in_t1 = featureValues_t2.diff(featureValues_t1)
      .filter{case (n,v) => v.split("\\|").exists(v0 => !featureValues_t1_split.contains((n,v0)))}
      .filter(x => featureNames_t1.contains(x._1))

    Console.err.println(
      s"""|Pos: $pos_not_in_t1
         |Feature names: $featureNames_not_in_t1
         |Feature values: $featureValues_not_in_t1
         |""".stripMargin)

    t2.listOfTags.toList.sortBy(_.toString).foreach(t => {
      val infringed = t2.implications.filter(i => !i(t))
      if (infringed.nonEmpty) {
          val fixed = infringed.foldLeft(t)({case (t2,r) => if (r.fix.isEmpty) t2 else (r.fix.get(t2))})
          // Console.err.println(s"$t does not satisfy $infringed")
        }
    })
  }

  def simplify(v: String):String  = v // v.replaceAll(" .*", "")
  def simplifyMap(m: Map[String,String]): Map[String, String] = m.filter(_._2.nonEmpty).map{case (k,v) => (k, simplify(v))}
  def simplifyMap2(m: Map[(String,String),String]): Map[(String,String), String] = m.filter(_._2.nonEmpty).map{case (k,v) => (k, simplify(v))}

  // waarden van a blijven indien beide gedefinieerd
  def mergeDescriptions(a: TagDescription, b: TagDescription) = {
    TagDescription(simplifyMap(b.posDescriptions ++ a.posDescriptions),
      simplifyMap(b.featureDescriptions ++ a.featureDescriptions),
      simplifyMap2(b.valueDescriptions ++ a.valueDescriptions))
  }

  def main(args: Array[String]): Unit = {
    val test = "data/CRM/tagset.json"
    fromJSON(scala.io.Source.fromFile(test).getLines().mkString("\n"))
  }
}

case class PartitionCondition(pos: String, featureName: String, conditions: List[String])
case class ValueRestriction(pos: String, featureName: String, values: List[String])

case class TagSet(prefix: String,
                  val posTags: List[String] = List.empty,
                  partitions:Map[String,List[String]] = Map.empty,
                  pos2partitions: Map[String,List[String]] = Map.empty,
                  parser: TagParser=defaultTagParser,
                  partitionConditions: List[PartitionCondition] = List.empty,
                  valueRestrictions: List[ValueRestriction] = List.empty,
                  descriptions: TagDescription = TagDescription(),
                  displayNames: TagDescription = TagDescription(),
                  listOfTags: Set[Tag] = Set.empty,
                  implications: Seq[Implication] = Seq.empty, featureOrder: Option[Seq[String]] = None)
{

  def toXML(server: String="http://svotmc10.ivdnt.loc/", corpus: String="gysseling_nt") =
    <tagset>
      <prefix>{prefix}</prefix>
      <corpus>{corpus}</corpus>
      <server>{server}</server>
      <mainPoS>
        {posTags.sorted.map(p => <pos displayName={displayNames.posDescription(p)} desc={descriptions.posDescription(p)}>{p}</pos>)}
      </mainPoS>
      <partitions>
        {partitions.toList.sortBy(_._1).map( { case (f, vs) => <feature displayName={displayNames.featureDescription(f)} desc={descriptions.featureDescription(f)}><name>{f}</name><values>{vs.map(v =>
        <featureValue displayName={val v1 = if (f != "pos") Some(Text(displayNames.valueDescription(f,v))) else None; v1}
                      desc={val v1 = if (f != "pos") Some(Text(descriptions.valueDescription(f,v))) else None; v1}><value>{v}</value><posForValue>{this.posForFeature(f,v).sorted.map(p => <pos>{p}</pos>)}</posForValue></featureValue>)}</values></feature>} )}
      </partitions>
      <constraints>
        {pos2partitions.toList.sortBy(_._1).map( { case (p, fs) => <constraint><pos>{p}</pos><features>{fs.map(f => <feature>{f}</feature>)}</features></constraint>} )}
      </constraints>
      <implications>
        {this.implications.map{i => <declaration corpus={i.corpus.toString}>{i.description}</declaration>}}
      </implications>
      <tags>
        {listOfTags.toList.sortBy(_.toString).map(t => <tag>{t}</tag>)}
      </tags>
    </tagset>

  def elem(label: String, child: Seq[Node]) = Elem(null, label, Null, scala.xml.TopScope, child: _*)

  def blacklabName(n: String) = n.replaceAll("[-.]", "_")

  def getDescription(p : String)  = displayNames.posDescription(p)
  def getDisplayName(p: String) = displayNames.posDescription(p)
  def getDisplayName(f: String, v: String)  = displayNames.valueDescription(f,v)

  def asJSON =
  {

    import org.json4s.Extraction

    def encodeJson(src: Any): JValue = {
      import org.json4s.{ Extraction, NoTypeHints }
      import org.json4s.JsonDSL.WithDouble._
      import org.json4s.jackson.Serialization
      implicit val formats = Serialization.formats(NoTypeHints)
      Extraction.decompose(src)
    }

    def pos2J(p: String) =
      p -> Map(
        "value" -> p,
        "displayName" -> getDisplayName(p),
        "subAnnotationIds" -> pos2partitions(p).filter(_ != "pos").map(p => prefix + "_" + blacklabName(p)))


    val values: (String, List[(String, Map[String, io.Serializable])]) = "values" -> posTags.sorted.map(pos2J) // how to get this sorted ....

    //println(values)
    val json = encodeJson(values)
    pretty(render(json))

    val subannotations = partitions.filter(_._1 != "pos").map({case (f, vals) => s"${prefix}_$f" -> Map("id" -> s"${prefix}_$f",
      "values" -> vals.toList.sorted.map(v => Map("value" -> v, "displayName" -> getDisplayName(f,v), "pos" -> this.posForFeature(f,v)))).toMap
    })

    val JSON =
      Map("values" -> posTags.toList.sorted.map(pos2J), //.toList.sortBy(_._1).toMap, // hier stond nog een toMap bij??
       "subAnnotations" -> subannotations)

    val jobject: JValue = encodeJson(JSON)
    val val1 = (jobject \ "values").asInstanceOf[JArray].arr.map{case JObject(l) => l.head}
    // println(pretty(render(val1)))
    val jobject1 = JObject("values" -> val1, "subAnnotations" -> jobject \ "subAnnotations")
    pretty(render(jobject1))
    //pretty(render(toJson(xml)))
  }

  def asPlainText =
    s"""
       |[features]
       |${partitions.map( {case (f,vs) => s"$f=${vs.mkString(", ")}" } ).mkString("\n")}
       |
       |[constraints]
       |${pos2partitions.map( {case (p,fs) => s"$p=${fs.mkString(", ")}" } ).mkString("\n")}
     """.stripMargin

  def forBlacklab = partitions.keySet.map( p =>
    s"""
       |      - name: ${blacklabName(p)}
       |        displayName: $p
       |        uiType: select
       |        multipleValues: true
     """.stripMargin).mkString("\n")

  def inSubsets(f:String):List[String] = partitions.filter({ case (s, v) => v.contains(f) }).toList.map(_._1)

  def fromPropositionCGN(p:Proposition, posIsSpecial: Boolean = false):Tag =
  {
    val vars = p.varsIn

    val pos = vars.find(v => v.startsWith(s"${this.prefix}:pos=")).getOrElse("UNK").replaceAll(".*=", "")
    val features = vars.filter(f => !f.startsWith(s"${this.prefix}:pos=")).toList
      .sortBy(f => {
        val n = f.replaceAll(s"^$prefix:feat.|=.*","")
        //Console.err.println(v)
        val l = pos2partitions(pos)
        val i = l.zipWithIndex.find({case (s,i) => s == n}).map({case (s,i) => i})
        i.getOrElse(0)
      })
      .map(v => v.replaceAll(s"^.*=", "")).mkString(",")

    var x = new CGNStyleTag(s"$pos($features)", this)
    //Console.err.println(s"Proposition to tag: $p->$x")
    x
  }

  def fromProposition(p:Proposition, posIsSpecial: Boolean = false):Tag = fromPropositionCGN(p,posIsSpecial)


  def featuresFromProposition(p:Proposition): Set[Feature] =
  {
    val vars = p.varsIn
    val features = vars.map(v => v.replaceAll(s"^${this.prefix}:", "")).map(x => {val r = x.split("="); Feature(r(0), r(1))})
    features
  }

  def fromString(t: String):Tag = parser.parseTag(this,t)

  def asTEIFeatureStructure(s: String):Elem = asTEIFeatureStructure(fromString(s))

  def asTEIFeatureStructure(t:Tag):Elem = <fs type={prefix}>
    {t.features.map(f => <f name={f.name}><symbol value={f.value}/></f>)}
  </fs>

  def consistentPartitions(pos: String, n: String, f: List[String]) = true



  def posForFeature(n: String, v: String) =
    {
      val r= valueRestrictions.filter(r => r.featureName == n && r.values.contains(v)).map(_.pos)
      // Console.err.println(s"Pos for $n $v: $r")
      r
    }

  def fixTag(t: Tag) = {
    val infringed = implications.filter(i => !i(t))
    if (infringed.nonEmpty) {
      val fixed = infringed.foldLeft(t)({case (t2,r) => if (r.fix.isEmpty) t2 else (r.fix.get(t2))})
      // Console.err.println(s"$t does not satisfy $infringed")
      fixed
    } else t
  }

  def tagSatisfiesImplications(t: Tag): (Boolean, Seq[Implication]) = {
    val infringed = implications.filter(i => !i(t))
    (infringed.isEmpty, infringed)
  }


  def isValid(t: Tag)  = {
    this.posTags.contains(t.pos) && t.features.forall(f => {
      val b = this.posForFeature(f.name,f.value).contains(t.pos) || f.value.split("\\||/").forall(x => this.posForFeature(f.name,x).contains(t.pos))
      if (!b)
        Console.err.println(s"$f not ok for ${t.pos}")
      b
    })
  }
}
