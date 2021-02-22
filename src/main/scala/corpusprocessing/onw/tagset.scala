package corpusprocessing.onw
import java.io.PrintWriter

import posmapping.CHNStyleTag

import scala.xml._
import utils.PostProcessXML._

object tagset {

  implicit def bla(m: Map[String,String]) : (Map[String,String], Map[String,String]) = (m, Map())


  import tagConversionRules.Rule
  import posmapping.Tag

  case class ONWCorpusTag(pos: String, flexie: String, features: Map[String,String], pos_org:String="", flexie_org:String="", tdnTag: Option[Tag] = None)
  {
    def getPoS = features.getOrElse("pos", "RES")

    def replaceFeatureName(pos: String, name: String, newName: String) = this.copy(features = this.features.map{
      case (n,v) if (this.getPoS == pos && n == name) => (newName, v) ; case (n,v) => (n,v) })

    def replaceFeatureName(pos: String, condName: String, condValue: String, name: String, newName: String) =
      if (features.getOrElse(condName, "___") == condValue)
        this.copy(features = this.features.map {
          case (n, v) if (this.getPoS == pos && n == name) => (newName, v);
          case (n, v) => (n, v)
        }) else this

    def removeFeature(pos: String, name: String) : ONWCorpusTag = this.copy(features = this.features.filter{
      case (n,v) if (this.getPoS == pos && n == name) => false ; case (n,v) => true })

    def removeFeature(name: String) : ONWCorpusTag = this.copy(features = this.features.filter{
      case (n,v) if (n == name) => false ; case (n,v) => true })

    def replaceFeatureValue(name: String, value: String, newValue: String) = this.copy(features = this.features.map{case (n,v) if (n,v) == (name, value) => (n,newValue) ; case (n,v) => (n,v) })

    def replaceFeatureValue(condName: String, condValue: String, name: String, value: String, newValue: String) = {
      if (features.getOrElse(condName, "___") == condValue)
       this.copy(features = this.features.map{case (n,v) if (n,v) == (name, value) => (n,newValue) ; case (n,v) => (n,v) })
      else this
    }

    def replacePoS(value: String, newValue: String) = {
      val t0 = if (value == pos) this.copy(pos = newValue) else this
      t0.replaceFeatureValue("pos", value, newValue)
    }

    def addFiniteness: ONWCorpusTag = {
      if (this.getPoS== "VRB" && (features.contains("tense") || features.contains("person")) && !(features.contains("finiteness")))
        this.copy(features = this.features ++ Map("finiteness" -> "fin")) else this
    }

    def addDegree: ONWCorpusTag = {
      if (this.getPoS == "AA" && !this.features.contains("degree"))
        this.copy(features = this.features ++ Map("degree" -> "pos")) else this
    }

    val old = false;
    def ifOld(f: ONWCorpusTag => ONWCorpusTag): ONWCorpusTag => ONWCorpusTag = t => if (old) f(t) else t

    def someTagPostProcessing(): ONWCorpusTag = {
      List[ONWCorpusTag => ONWCorpusTag](
        t => t.addFiniteness,
        // transcat

        t => t.replaceFeatureValue("position", "nom",  "pos", "ADJ", "NOU-C"),

        t => t.replaceFeatureValue("degree","sup", "pos", "ADV", "AA"),
        t => t.replaceFeatureValue("degree","comp", "pos", "ADV", "AA"),
        t => t.replaceFeatureValue("finiteness","ger", "pos", "VRB", "NOU-C"),

        t => t.replacePoS("ADJ", "AA"),
        t => t.addDegree,

        t => t.replaceFeatureValue("number", "sg|pl", "uncl"),
        t => t.replaceFeatureValue("number", "pl|sg", "uncl"),
        t => t.replaceFeatureValue("position", "nom", "free"),
        t => t.replaceFeatureValue("position", "pred|prenom", "pred"),
        t => t.replaceFeatureValue("position", "postnom|prenom", "postnom"),

        // transcats
        t => t.replaceFeatureValue("xtype", "pron",  "pos", "AA", "PD"),
        t => t.replaceFeatureValue("xtype", "pron",  "pos", "NOU-C", "PD"),

        //t => t.replaceFeatureName("VRB", "number", "NA"),
        //t => t.replaceFeatureName("VRB", "person", "PA"),
        t => t.replaceFeatureName("VRB", "val", "valency"),
        t => t.replaceFeatureName("VRB", "class", "verbclass"),
        t => t.replaceFeatureName("ADP", "gov", "government"),

        ifOld (t => t.replaceFeatureName("ADJ", "number", "NA")),
        ifOld(t => t.replaceFeatureName("AA", "number", "NA")),
        ifOld(t => t.replaceFeatureName("ADJ", "gender", "GA")),
        ifOld(t => t.replaceFeatureName("AA", "gender", "GA")),

/*
        (t => t.replaceFeatureName("PD", "number", "NA")),
        t => t.replaceFeatureName("PD", "gender", "GA"),
        t => t.replaceFeatureName("PD", "person", "PA"),

        t => t.replaceFeatureName("NUM", "number", "NA"),
        t => t.replaceFeatureName("NUM", "gender", "GA"),
        t => t.replaceFeatureName("NUM", "person", "PA"),
*/

        t => t.replaceFeatureName("PD", "type", "pers", "number", "LN"),
        t => t.replaceFeatureName("PD", "type", "refl", "number", "LN"),
        t => t.replaceFeatureName("PD", "type", "recip", "number", "LN"),
        t => t.replaceFeatureName("PD", "type", "pers", "gender", "LG"),
        t => t.replaceFeatureName("PD", "type", "refl", "gender", "LG"),
        t => t.replaceFeatureValue("pos","PD", "position", "pred", "free"),
        t => t.replaceFeatureName("PD","PA", "LP"),

        t => t.replaceFeatureName("PD","person", "LP"),


        t => t.replaceFeatureName("VRB", "declination", "conjugation"),
        t => t.removeFeature(pos="NOU-C", "position"),

        t => t.removeFeature(pos="NOU-C", "declination"),
        t => t.removeFeature(pos="AA", "declination"),
        t => t.removeFeature(pos="PD", "declination"),
        t => t.removeFeature(pos="NUM", "declination"),

        t => t.removeFeature(pos="ADV", "position"),
        t => t.removeFeature("xtype"),
        t => t.removeFeature(pos="VRB", "gov")

      ).foldLeft(this){case (t1, f) => f(t1)}
    }

    def toStringLong =
      features.getOrElse("pos", "RES") + "(" + features.toList.sortBy(_._1).filter(_._1 != "pos").map({case (n,v) => n + "=" + v}).mkString(",") + ")" +
      "\t" + pos_org + "\t" + flexie_org

    override def toString:String = {
      if (tdnTag.isDefined) tdnTag.get.toString else
      features.getOrElse("pos", "RES") + "(" + features.toList.sortBy(_._1).filter(_._1 != "pos")
      .map({case (n,v) => n + "=" + v}).mkString(",") + ")" }

    def toStringWithOrigin() = this.toString + " <- " + this.pos_org + " "  + this.flexie_org

    // bij ADJ wvorm => adjtype
    // bnwtype => adjtype
    // bij N wvorm weg
    // bij N positie weg, maar wel even checken hoe hij in het corpus komt
    // ntype=pron bekijken. Moeten dit geen VNW worden??

    val cleanWvorm: ONWCorpusTag => ONWCorpusTag = t => if (!t.features.contains("wvorm")) t else t.features.get("pos") match
      {
        case Some("N") => t.copy(features = t.features - "wvorm")
        case Some("ADJ") => t.copy(features = (t.features - "wvorm") + ("adjtype" -> t.features("wvorm")))
        case _ => t
      }

    def cleanPositie: ONWCorpusTag => ONWCorpusTag = t => t.features.get("pos") match {
      case Some("N") => t.copy(features = t.features - "positie")
      case _ => t
    }

    val NotInAdj = Set("finiteness", "tense", "inflection")

    val cleanNominalizedVerbForms: ONWCorpusTag => ONWCorpusTag = t =>  {
      val p  =  t.features.get("pos")
      if (p == Some("ADJ") || p == Some("NOU-C")) {
        t.copy(features = t.features.filter({ case (k,v) => !NotInAdj.contains(k)}))
      } else t;
    }

    def setPoS(p: String)  = this.copy(
      features = this.features.filterKeys(_ != "pos") ++ Map("pos" -> p)
    )

    val cleanGerunds: ONWCorpusTag => ONWCorpusTag = t => {
      val p  =  t.features.get("pos")

      if (p == Some("VRB") && t.features.get("finiteness").map(_ == "gerund")==Some(true) )
        {
          if (t.features.get("case").map(_ == "gen") == Some(true))
            t.setPoS("NOU-C")
          else t
        } else t
    }

    val addPV: ONWCorpusTag => ONWCorpusTag = t => t.features.get("pos") match {
      case Some("WW") => {
        if (!t.features.contains("wvorm") && Set("tijd", "modus", "persoon").exists(t.features.contains(_)))
        t.copy(features = t.features + ("wvorm" -> "pv"))
        else t
      }
      case _ => t
    }

    def renameKey(m: Map[String,String], n1: String, n2:String) =
      {
        if (!m.contains(n1)) m else {
          val v = m(n1); m - n1 + (n2 ->  v)
        }
      }

    def renameExtraFeatures: ONWCorpusTag => ONWCorpusTag = t =>
      {
         val transformations = Set("flexietype", "flexieklasse", "transitiviteit", "metnaamval").map(s =>
           {
             val f: Map[String,String]  =>  Map[String,String] = (m => renameKey(m, s, "x-" + s))
             f
           }
        )
        val newFeatures = transformations.foldLeft(t.features)( (m,f) => f(m))
        t.copy(features = newFeatures)
      }

    def clean = {
      //Console.err.println("cleaning  .. "  + this)
      //System.exit(1)
       List(cleanWvorm, cleanPositie, addPV, renameExtraFeatures, cleanNominalizedVerbForms).foldLeft(this)((t,f) => f(t))
    }
  }

  def flexieKlasse(t: String)  = Rule("\\(" + t + "\\)", Map("flexieklasse" -> t)
    -> Map("class" -> {
    if (t.startsWith("3")) t else
    t.replaceAll("[ab]","")
  }) ,false)

  val flexieKlasseRegels =
    List("1", "1a", "1b", "1c", "2", "2a", "2b", "3", "3a", "3b", "4", "5", "5b", "6", "6b", "7").map(flexieKlasse)


  val stukkiesXML =  List("1", "1a", "1b", "1c", "2", "2a", "2b", "3", "3a", "3b", "4", "5", "5b", "6", "6b", "7").map(x =>
    <featureValue displayName={x} desc={x}>
      <value>{x}</value>
      <posForValue>
        <pos>VRB</pos>
      </posForValue>
    </featureValue>)



  lazy val rules = tagConversionRules.rules ++ flexieKlasseRegels

  def applyRules(t: ONWCorpusTag) = rules.foldLeft(t)((t, r) => r.apply(t))

  val d = Settings.inputDir

  val files = (new java.io.File(d)).listFiles.toStream

  val allTokens = files.flatMap(f =>
  {
    val doc = XML.loadFile(f)
    (doc \\ "w").toStream
  })

  val allTags:List[(ONWCorpusTag,Int)] = allTokens.groupBy(
    w => {
      val pos = (w \ "@pos").text

      val flexief = (w \\ "f").filter(x => (x \ "@name").text == "flexie")
      // Console.err.println(flexief)
      val flexie = flexief.text
      applyRules(ONWCorpusTag(pos, flexie, Map.empty, pos, flexie)).clean.someTagPostProcessing
    }
  ).mapValues(l => l.size).toList.sortBy(_._1.toString)

  val tagDump = new PrintWriter("/tmp/allTags.onw.txt")
  allTags.map(_._1.toString).toSet.toList.sorted
    .foreach(t => tagDump.println(t))
  tagDump.close()

  def orgjes(t: ONWCorpusTag):(String,String) = (t.pos_org, t.flexie_org)

  val tagMap:Map[(String,String), ONWCorpusTag] = allTags.map(_._1).toSet.groupBy(orgjes).mapValues(l => l.head)

  val distinctTags = tagMap.values.toSet

  val danges = tagMap.map{case ((p,f),t) => s"$p;$f" -> t.toString} // nee ik moet naar een ONW stijl tag, bah.

  lazy val tagMapTDN: Map[String, CHNStyleTag] = posmapping.TagsetDiachroonNederlands.tagsetFromSetOfTags("/tmp/", "ONW", danges)._1

  lazy val tagMapEnhanced = tagMap.map({case ((p,f),t) =>
    val tdnTag = tagMapTDN(s"$p;$f")
    val features = tdnTag.features.map(f => f.name -> f.value).toMap
    (p,f) -> t.copy(features = features, tdnTag = Some(tdnTag))})

  val tagMapDump = new PrintWriter("/tmp/allTags.onwmap.txt")
  tagMapEnhanced.toList.sortBy(_._1.toString)
    .foreach({case ((org,f), t)  => tagMapDump.println(s"$org\t$f\t$t")})
  tagMapDump.close()

  def mapTags(w: Elem):Elem =
  {
    val pos = (w \ "@pos").text
    val newChild = w.child.map(x =>

    if (x.label == "fs")
    {
      val flexie =  (w \\ "f").filter(x => (x \ "@name").text == "flexie").text
      val t = tagMapEnhanced(pos,flexie)
      val extraFjes = t.features.map({case (k,v) => <f name={"pos." + k}>{v}</f>})
      x.asInstanceOf[Elem].copy(child = x.child ++ extraFjes)
    } else x)

    val msd = (w \ "fs").map(x => {
      val flexie =  (w \\ "f").filter(x => (x \ "@name").text == "flexie").text
      val t = tagMapEnhanced(pos,flexie)
      t.toString
    }).mkString("+")

    def msd2fs(fs: Elem) = {
      val flexie =  (w \\ "f").filter(x => (x \ "@name").text == "flexie").text
      val t = tagMapEnhanced(pos,flexie)
      val msd = t.toString
      fs.copy(child = ({fs.child} ++ Seq(<f name="msd">{msd}</f>)))
    }
    val msdA = new UnprefixedAttribute("msd", msd, Null)
    updateElement(w.asInstanceOf[Elem].copy(child=newChild, attributes = w.attributes.append(msdA)) , _.label == "fs", msd2fs)
  }

  def dumpTags = {
    val f = new java.io.FileWriter("/tmp/tags.txt")
    allTags.foreach(x => f.write(x._1.toStringWithOrigin + "\t" + x._2 + "\n"))
    f.close
  }

  def tagSet = {
    val possen = distinctTags.map(_.features.getOrElse("pos", "RES"))
    val featureNames = distinctTags.flatMap(_.features.keySet)

    println("[features]")
    featureNames.toList.sortBy(identity).foreach(
      f => {
        val values = distinctTags.filter(_.features.contains(f)).flatMap(t => Set(t.features(f))).toList.sorted
        println(s"$f=${values.mkString(",")}")
      }
    )
    println("\n[constraints]")
    possen.toList.sortBy(identity).foreach(p => {
      val fz = distinctTags.filter(t => t.features.contains("pos") && t.features("pos") == p).flatMap(_.features.keySet).filter(_ != "pos").toList.sorted
      println(s"$p=${fz.mkString(",")}")
    })
  }

  def main(args: Array[String]): Unit = {
      dumpTags
      tagSet
  }
}
