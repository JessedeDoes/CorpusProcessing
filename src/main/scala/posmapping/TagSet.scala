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
import scala.util.{Success, Try}
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

    val partitions: Map[String, List[String]] = (d \\ "partitions" \ "feature").map(f => (f \ "name").text -> (f \\ "featureValue" \\ "value").map(_.text).toList ).toMap

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

  def tagsetFromSetOfTags(prefix: String, tags: Set[Tag], addParts: Boolean=true) = {
    val posTags = tags.map(_.pos).toList

    val partitions: Map[String,List[String]] = tags.flatMap(_.features).groupBy(_.name).mapValues(x => x.map(f => f.value).toList)

    val partitions_enhanced = if (addParts) partitions.mapValues(l => l.toSet ++ l.flatMap(_.split("\\|")).toSet) // voor values van de vorma|b|c enz, add a b c
      .mapValues(_.toList) else partitions

    val pos2partitions: Map[String, List[String]] = tags.groupBy(_.pos).mapValues(l => l.flatMap(p => p.features.map(f => f.name))).mapValues(s => s.toList)

    val valueRestrictions:List[ValueRestriction] =
      tags.groupBy(_.pos).flatMap(
        { case (p,tags_with_pos) => {
          val features = tags_with_pos.flatMap(_.features.toSet.toList)
          features.groupBy(_.name).mapValues(f => f.map(_.value)).map({case (fn,l) => ValueRestriction(p, fn, l.toList)})
        } }
      ).toList

    TagSet(prefix, posTags, partitions_enhanced, pos2partitions, parser, List(), valueRestrictions, listOfTags = tags)
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

  def sortFeatures(t: TagSet, tRef: TagSet): TagSet = {

    def sortPartition(pos: String, features: List[String]) = {
      val lijstje: Map[String, Int] = tRef.pos2partitions.getOrElse(pos, List()).zipWithIndex.toMap

      // dit sorteert gecombineerde waarden nog niet goed.....

      val additions = features.filter(f => !lijstje.contains(f)).zipWithIndex.map({ case (x, i) => (x, lijstje.size + i) }).toMap

      val combi = lijstje ++ additions

      (pos, features.sortBy(x => combi(x)))
    }

    /*
       Doe het zo dat
       - we eerst de simpele waarden sorteren
       - dan alles lexicografisch sorteren
    */


    def sortFeatureValuesBase(n: String, v: List[String]) = {
      val lijstje: Map[String, Int] = tRef.partitions.getOrElse(n, List()).zipWithIndex.toMap

      val addition = v.filter(f => !lijstje.contains(f)).zipWithIndex.map({ case (x, i) => (x, lijstje.size + i) }).toMap

      val combi = lijstje ++ addition

      v.sortBy(x => combi(x)).map(tRef.normalizeFeatureValue(n, _))
    }


    def zcompare(a: List[Int], b: List[Int]): Int = {
      if (a.isEmpty && b.isEmpty) 0
      else if (a.isEmpty) -1
      else if (b.isEmpty) 1 else if (a(0) == b(0)) zcompare(a.tail, b.tail) else if (a(0) > b(0)) 1 else -1
    }


    def sortFeatureValues(n: String, v0: List[String]): List[String] =
    {
      val v = v0.map(v =>  tRef.normalizeFeatureValue(n,v))
      val base = sortFeatureValuesBase(n, v.flatMap(_.split("\\|"))).zipWithIndex.toMap

      def orderList(v: String)  = v.split("\\|").toList.map(base)

      val z = v.sortWith((a,b) => {
        val l1 = orderList(a)
        val l2 = orderList(b)
        zcompare(l1,l2) < 0
      })
      //println(z)
      z
    }

    def sortValueRestriction(vr: ValueRestriction) = {
      val z = vr.copy(values = vr.values.map(tRef.normalizeFeatureValue(vr.featureName,_)))
      //println(s"With sorted features: $z")
      z
    }

    val sortedVR = t.valueRestrictions.map(sortValueRestriction)
    //println("--> " + sortedVR.filter(_.pos=="AA"))
    val z = t.copy(
      partitions = t.partitions.map({case (n,v) => (n,sortFeatureValues(n,v))}),
      pos2partitions = t.pos2partitions.map({case (p,f)=> sortPartition(p,f)}),
      valueRestrictions = sortedVR)
   // println("--> " + z.valueRestrictions.filter(_.pos=="AA"))
    z
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

  def elem(label: String, child: Seq[Node]) = Elem(null, label, Null, scala.xml.TopScope, false, child: _*)

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
      "values" -> vals.map(v => Map("value" -> v, "displayName" -> getDisplayName(f,v), "pos" -> this.posForFeature(f,v)))).toMap
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

  def forBlacklabCHNStyle = partitions.keySet.map( p =>
    s"""
       |      - name: pos_$p
       |        displayName: $p
       |        valuePath: "@pos"
       |        uiType: select
       |        multipleValues: true
       |        allowDuplicateValues : false
       |        process:
       |        - action: parsePos
       |          field: $p
       |        - action: split
       |          separator: "\\\\|"
       |          keep: both
       |""".stripMargin).mkString("\n")

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
    val infringed = implications.filter(i => !i(t)) // aha ik zie de fout al, je moet over ALLE implicaties loopen....
    if (infringed.nonEmpty) {
      val fixed = implications.foldLeft(t)({case (t2,r) => if (r(t2)) t2 else if (r.fix.isEmpty) t2 else (r.fix.get(t2))})
      // Console.err.println(s"$t does not satisfy $infringed")

      fixed
    } else t
  }

  def tagSatisfiesImplications(t: Tag, corpus_name: Option[String] = None): (Boolean, Seq[Implication]) = {
    val relevant = implications.filter(i => corpus_name.isEmpty || i.corpus.nonEmpty && i.corpus.get.contains(corpus_name.get))
    //println(relevant)
    val infringed = relevant.filter(i => !i(t))
    //println(s"$t $infringed")
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

  def fixAllTags(addParts: Boolean = true) = {
    val fixedTags = this.listOfTags.map(fixTag)
    val newValueRestrictions : List[ValueRestriction] = // aha die moet je dan weer sorteren
      fixedTags.groupBy(_.pos).flatMap(
        { case (p,tags_with_pos) => {
          val features = tags_with_pos.flatMap(_.features.toSet.toList)
          features.groupBy(_.name).mapValues(f => f.map(_.value)).map({case (fn,l) => ValueRestriction(p, fn, l.toList)})
        } }
      ).toList
    val fs = fixedTags.toSet

    //probleem: featureValues moeten ook worden toegevoegd zo nodig....
    val newPartitions: Map[String,List[String]] = fixedTags.flatMap(_.features).groupBy(_.name).mapValues(x => x.map(f => f.value).toList)

    val partitions_enhanced = if (addParts) newPartitions.mapValues(l => l.toSet ++ l.flatMap(_.split("\\|")).toSet) // voor values van de vorma|b|c enz, add a b c
      .mapValues(_.toList) else newPartitions
    //val newPartitions = this.partitions.map({case (n,v) => })
    //println(newValueRestrictions)

    this.copy(valueRestrictions=newValueRestrictions, partitions = partitions_enhanced, listOfTags=fs)
  }

  def featureOccurs(n: String, v: String): Boolean = {
    this.listOfTags.isEmpty || this.listOfTags.exists(t => {
      val z = t.features.exists(x => x.name == n && x.value == v)
      if (z) {
        Console.err.println(s"Yep: $n $v $t")
      }
      z })
  }

  def removeNonExistentValues()  = {
    if (this.listOfTags.isEmpty) this else {
      val newPartitions = this.partitions.map({case (n,l) => (n,l.filter(featureOccurs(n,_)))})
      this.copy(partitions = newPartitions)
    }
  }

  def normalizeFeatureValue(n: String, v: String): String = {
    val tagset = this

      val featureValues = tagset.partitions.getOrElse(n,List())

      val valueParts = v.split("\\|")
      val canonical = featureValues.find(_.split("\\|").toSet == valueParts.toSet)

      canonical.getOrElse({
        val lijstje = featureValues.zipWithIndex.toMap
        val additions = valueParts.filter(f => !lijstje.exists(_._1 == f)).zipWithIndex.map({ case (x, i) => (x, lijstje.size + i) }).toMap
        def combi = lijstje ++ additions
        // if (additions.nonEmpty) Console.err.println(n + ": " + lijstje + " --> " + additions)
        Try {
          valueParts.sortBy(x => combi(x)).mkString("|")
        } match {
          case Success(z) => z
          case _ => Console.err.println(s"Failure sorting $valueParts"); v
        }
      })
    }

  def adHocje(t: CHNStyleTag) = {

    val c0 = if (Set("art", "oth").contains(t.featureValue("subtype"))) Set("indef", "d-p", "dem").contains(t.featureValue("type")) else true
    val c1 = Set("prenom", "postnom", "free","").contains(t.featureValue("position"))
    val c2  = Set("","uncl").contains(t.featureValue("tense")) || t.featureValue("finiteness") == "fin"

    val c3 = if (t.pos != "PD") true else {
      val position = t.featureValue("position")

      if (t.featureValue("subtype") == "art") position == "prenom" else {
        if (Set("pers", "refl", "recip", "recip|refl", "rel").contains(t.featureValue("type"))) position=="free"
        else true
      }
    }
    c0 && c1 && c2 && c3
  }

  def generateTags(corpusName: String) = {
    val taglist = this.posTags.sorted.flatMap(p => {
      val features = pos2partitions.getOrElse(p, List()).filter(_ != "WFje")
      if (features.isEmpty)
        List(p) else {
        //println(features)
        val dimensions: Seq[List[String]] = features.map((fn: String) => {
          val values = this.partitions(fn)

          //println(s"$fn: $values")
          values.filter(v => this.posForFeature(fn, v).contains(p) || this.posForFeature(fn, v).isEmpty).map(v => s"$fn=$v")
        })
        //println(s"D: $dimensions")
        val initial: Seq[String] = dimensions(0)
        val d = dimensions.drop(1).take(5).filter(_.nonEmpty).foldLeft(initial)({ case (b, l) => b.flatMap(x => l.map(y => s"$x,$y")) })
        d.map(x => s"$p($x)")
      }
    })

    taglist.sorted
      .map(x => CHNStyleTag(x,this))
      .filter(x => x.pos != "ADJ" && x.pos != "COLL")
      .filter(x => this.tagSatisfiesImplications(x, Some("core"))._1)
      .map(t => t.removeUnforcedFeatures(Some("core"))).flatMap(t => List(t, t.removeFeature("WF"))).map(_.toString).toSet.toList.sorted.foreach(println)
  }
}
