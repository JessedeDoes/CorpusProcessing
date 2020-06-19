package corpusprocessing.onw

import corpusprocessing.onw.tagset.{ONWCorpusTag}

object tagConversionRules {

  case class Rule(pattern: String, featuresx: (Map[String, String], Map[String,String]), isFlexie: Boolean, extraCondition: Option[String] = None, sillyAction: Option[Rule=>Unit] = None)
  {
    def log() = if (sillyAction.nonEmpty) {
      println(s"Applying $this")
      sillyAction.get(this);
    }

    lazy val features = if (Settings.useCHNStyle) featuresx._2 else featuresx._1

    def mergeMaps(base: Map[String,String], add: Map[String,String]) =
    {
      def addVal(base: Map[String,String], nv: (String, String) ) = {
        val (n,v) = nv
        val v0 = if (base.contains(n))
          base(n).split("\\|").toSet
        else
          Set()
        base - n + (n -> (v0 ++ v.split("\\|").toSet).toList.sortBy(identity).mkString("|"))
      }
      add.toList.foldLeft(base)(addVal)
    }

    def apply(t: ONWCorpusTag):ONWCorpusTag =
    {
      if (isFlexie)
      {
        if (t.flexie.matches(".*" + pattern + ".*") && (extraCondition.isEmpty || t.pos.matches(".*" + extraCondition.get + ".*")))
        {
          log()
          ONWCorpusTag(t.pos, t.flexie.replaceAll(pattern, ""), mergeMaps(t.features,features), t.pos_org, t.flexie_org)
        } else
          t
      } else
      {
        if (t.pos.matches(".*" + pattern + ".*") && (extraCondition.isEmpty || t.flexie.matches(".*" + extraCondition.get + ".*")))
        {
          log()
          ONWCorpusTag(t.pos.replaceAll(pattern, ""), t.flexie, mergeMaps(t.features,features), t.pos_org, t.flexie_org)
        } else t
      }
    }
  }

  val rules = List(
    // !! instr.datief
    // telw. (zelfst.);nom.sg.mann. (zwak)

    Rule("telw.*zelfst", Map("pos" -> "TW", "numtype" -> "numtype") -> Map("pos" -> "NOU-C"), false),

    Rule("znw.*waternaam", Map("pos" -> "N", "ntype" -> "eigen") -> Map("pos" -> "NOU-P", "gender" -> "f", "xtype" -> "location"), false),
    Rule("met *vooropgeplaatste *gen", Map("pos" -> "NOU-C") -> Map("pos" -> "NOU-C"), true),
    Rule("met *vooropgeplaatste *gen", Map("pos" -> "NOU-C") -> Map("pos" -> "NOU-C"), false),
    Rule("part.perf", Map("pos" -> "ADJ", "adjtype" -> "vd") ->
      { val z = Map("pos" -> "ADJ", "xtype" -> "past_part"); println(z); z }, true, Some("bnw"), Some(t => { })),

    Rule("vz\\.", Map("pos" -> "VZ") -> Map("pos" -> "ADP", "type" -> "uncl"), false),
    Rule("vz$", Map("pos" -> "VZ") -> Map("pos" -> "ADP", "type" -> "uncl"), false),

    Rule("\\(\\+ acc\\.\\)", Map("metnaamval" -> "acc") -> Map("gov" -> "acc") , false),
    Rule("\\(\\+ dat\\.\\)", Map("metnaamval" -> "dat") ->  Map("gov" -> "dat"), false),
    Rule("\\( dat\\.\\)", Map("metnaamval" -> "dat") ->  Map("gov" -> "dat"), false),
    Rule("\\(\\+ dat\\./acc\\.\\)", Map("metnaamval" -> "dat|acc") ->  Map("gov" -> "dat|acc"), false),
    Rule("\\(\\+ gen\\.?.?\\)", Map("metnaamval" -> "gen") ->  Map("gov" -> "gen"), false), // bij werkwoorden weghalen
    //(+ acc.)

    Rule("nevensch.vw\\.", Map("pos" -> "VG", "conjtype" -> "neven") -> Map("pos" -> "CONJ", "type" -> "coor"), false),
    Rule("ondersch.vw\\.", Map("pos" -> "VG", "conjtype" -> "onder") -> Map("pos" -> "CONJ", "type" -> "sub"), false),
    Rule("verg.vw\\.", Map("pos" -> "VG", "conjtype" -> "verg") -> Map("pos" -> "CONJ", "type" -> "comp") , false),

    Rule("vw\\.", Map("pos" -> "VG") -> Map("pos" -> "CONJ"), false),

    Rule("bnw\\..*pron\\.", Map("pos" -> "ADJ", "adjtype" -> "pron") -> Map("pos" -> "PD", "type" -> "indef"), false),

    // bnw. (part.perf)



    Rule("bnw. *\\(telw.\\)", Map("pos" -> "ADJ", "adjtype" -> "num") -> Map("pos" -> "ADJ") , false),
    Rule("bnw. *\\(part.perf.?\\)", Map("pos" -> "ADJ", "adjtype" -> "vd") ->
      Map("pos" -> "ADJ", "xtype" -> "past_part") , false),


    Rule("bnw. *part.perf\\.?", Map("pos" -> "ADJ", "adjtype" -> "vd") ->
      Map("pos" -> "ADJ", "xtype" -> "past_part") , false),

    Rule("bnw. *\\(part.pres.?\\)", Map("pos" -> "ADJ", "adjtype" -> "vd") ->
      Map("pos" -> "ADJ", "xtype" -> "part_pres") , false),

    Rule("part.pres.", Map("pos" -> "ADJ", "adjtype" -> "vd") ->
      Map("xtype" -> "part_pres") , true, Some("bnw")),

    Rule("part.pres.", Map("pos" -> "ADJ", "adjtype" -> "vd") ->
      Map("finiteness" -> "prespart") , true),



 // alleen bij adjectief, anders finiteness ....

    Rule("bnw. *\\(teg.deelw.\\)", Map("pos" -> "ADJ", "adjtype" -> "od") ->
      Map("pos" -> "ADJ", "xtype" -> "pres_part"), false),

    Rule("\\(zelfst\\.\\)", Map("positie" -> "nom") -> Map("position" -> "nom"), false),
    Rule("\\(zelfst\\.\\)", Map("positie" -> "nom") -> Map("position" -> "nom"), true),

    Rule("\\(zelfst\\)", Map("positie" -> "nom") -> Map("position" -> "nom"), false),
    Rule("\\(zelfst\\)", Map("positie" -> "nom") -> Map("position" -> "nom"), true),

    Rule("\\(bijv\\.\\)", Map("positie" -> "attributief") -> Map("position" -> "prenom"), false),
    Rule("\\(bijv\\.\\)", Map("positie" -> "attributief") -> Map("position" -> "prenom"), true),

    Rule("\\(bijv\\.\\.\\)", Map("positie" -> "attributief") -> Map("position" -> "prenom"), false),
    Rule("\\(bijv\\.\\.\\)", Map("positie" -> "attributief") -> Map("position" -> "prenom"), true),

    Rule("postpos\\.", Map("positie" -> "postnom") -> Map("position" -> "postnom"), true),
    Rule("pred\\.", Map("positie" -> "pred|vrij") -> Map("position" -> "pred"), true),
    Rule("pred\\.", Map("positie" -> "pred|vrij") -> Map("position" -> "pred"), false),

    Rule("pred", Map("positie" -> "pred|vrij") -> Map("position" -> "pred"), true),
    Rule("pred", Map("positie" -> "pred|vrij") -> Map("position" -> "pred"), false),

    Rule("bnw\\.?", Map("pos"-> "ADJ") -> Map("pos"-> "ADJ"), false),
    Rule("adj\\.", Map("pos"-> "ADJ") -> Map("pos"-> "ADJ"), false),

    Rule("superl\\.", Map("graad"-> "sup") -> Map("degree"-> "sup") , false),
    Rule("sup\\.", Map("graad"-> "sup") -> Map("degree"-> "sup") , false),
    Rule("superlatief", Map("graad"-> "sup") -> Map("degree"-> "sup") , false),
    Rule("comp\\.", Map("graad"-> "comp") -> Map("degree"-> "comp"), false),

    Rule("ww\\.", Map("pos"-> "WW") -> Map("pos"-> "VRB"), false),

    Rule("\\(?hulp\\)?", Map("wwtype" ->  "hulp") -> Map("type" ->  "aux"), false),
    Rule("\\(?koppel\\)?", Map("wwtype" ->  "koppel") -> Map("type" ->  "cop"), false),

    Rule("part.pres.*gerund", Map("wvorm" ->  "od|gerund") -> Map("finiteness" ->  "prespart|ger"), true),

    Rule("part\\.perf\\.?", Map("wvorm" ->  "vd") -> Map("finiteness" ->  "pastpart"), false),
    Rule("part\\.pres\\.", Map("wvorm" ->  "od") -> Map("finiteness" ->  "prespart"), false),
    Rule("part\\.perf\\.?", Map("wvorm" ->  "vd") -> Map("finiteness" ->  "pastpart"), true),
    Rule("part\\.pres\\.", Map("wvorm" ->  "od") -> Map("finiteness" ->  "prespart"), true), // sie stinchende mit then bezzesten saluon ? (lw)


    Rule("intr\\.", Map("transitiviteit" ->  "intr") -> Map("val" ->  "intr"), false),
    Rule("trans\\.", Map("transitiviteit" ->  "trans") -> Map("val" ->  "trans"), false),
    Rule("refl\\.", Map("transitiviteit" ->  "refl") -> Map("val" ->  "refl"), false),
    Rule("onpers\\.", Map("transitiviteit" ->  "onpers") -> Map("val" ->  "impers"), false),


    //inf./gerund. part.pres. / gerund.
    Rule("te_gerund\\.?", Map("wvorm" ->  "gerund") -> Map("finiteness" ->  "inf"), true),
    Rule("inf_gerund\\.?", Map("wvorm" ->  "gerund") -> Map("finiteness" ->  "inf"), true),


    Rule("inf./gerund\\.?", Map("wvorm" ->  "inf|gerund") -> Map("finiteness" ->  "inf|ger"), false),
    Rule("inf\\.", Map("wvorm" ->  "inf") -> Map("finiteness" ->  "inf"), false),
    Rule("gerund.*gen.sg", Map("wvorm" ->  "gerund") -> Map("finiteness" ->  "ger", "case" -> "gen", "gender" -> "n"), true),
    Rule("gerund\\.?", Map("wvorm" ->  "gerund") -> Map("finiteness" ->  "ger", "gender" -> "n"), true),

    Rule("inf./gerund\\.?", Map("wvorm" ->  "inf|gerund") -> Map("finiteness" ->  "inf"), true),
    Rule("inf\\.", Map("wvorm" ->  "inf") -> Map("finiteness" ->  "inf"), true),



    Rule("onbep\\.lidw\\.", Map("pos" -> "LID", "lwtype" -> "onbep") ->
      Map("pos" -> "PD", "type" -> "indef", "subtype" -> "art"), false),

    Rule("lidw.*onbep", Map("pos" -> "LID", "lwtype" -> "onbep") ->
      Map("pos" -> "PD", "type" -> "indef", "subtype" -> "art"), false),

    Rule("bep\\.lidw\\.", Map("pos" -> "LID", "lwtype" -> "bep") -> Map("pos" -> "PD",
      "type" -> "dem", "subtype" -> "art"), false),

    Rule("lidw\\.", Map("pos" -> "LID") -> Map("pos" -> "PD",  "subtype" -> "art"), false), // !probleem


    Rule("znw\\..*pron\\.", Map("pos" -> "N", "ntype" -> "pron") -> Map("pos" -> "PD", "type" -> "indef", "xtype" -> "znwpron"), false),
    Rule("znw\\.", Map("pos" -> "N", "ntype" -> "soort") -> Map("pos" -> "NOU-C"), false),

    Rule("persoonsnaam", Map("pos" -> "N", "ntype" -> "eigen") -> Map("pos" -> "NOU-P", "xtype" -> "person"), false),

    Rule("toponiem", Map("pos" -> "N", "ntype" -> "eigen") -> Map("pos" -> "NOU-P", "xtype" -> "location"), false),

    Rule("bw.rel.partikel", Map("pos" -> "BW", "bwtype" -> "relatiefpartikel") -> Map("pos" -> "ADV",
      "type" -> "reg", "subtype" -> "rel"), false), // !probleem, wat doen we met rel partikel (komt alleen ee paar keer in lw voor, lijkt gewoon

    Rule("rel.partikel", Map("pos" -> "BW", "bwtype" -> "relatiefpartikel") -> Map("pos" -> "ADV",
      "type" -> "reg", "subtype" -> "rel"), false),

    Rule("aanw.vnw.bw\\.", Map("pos" -> "BW", "pdtype" -> "adv-pron", "bwtype" -> "aanw") ->
      Map("pos" -> "ADV", "type" -> "pron", "subtype" -> "dem"), false),
    Rule("betr.vnw.bw\\.", Map("pos" -> "BW", "pdtype" -> "adv-pron", "bwtype" -> "betr")
      -> Map("pos" -> "ADV", "type" -> "pron", "subtype" -> "rel"), false),
    Rule("betr.vnw. bw\\.", Map("pos" -> "BW", "pdtype" -> "adv-pron", "bwtype" -> "betr")
      -> Map("pos" -> "ADV", "type" -> "pron", "subtype" -> "rel"), false),
    Rule("vrag.vnw.bw\\.", Map("pos" -> "BW", "pdtype" -> "adv-pron", "bwtype" -> "vrag")
      -> Map("pos" -> "ADV", "type" -> "pron", "subtype" -> "inter"), false),

    Rule("vnw.bw\\.", Map("pos" -> "BW", "pdtype" -> "adv-pron") ->
      Map("pos" -> "ADV", "type" -> "pron"), false),

    Rule("aanw.bw\\.", Map("pos" -> "BW", "bwtype" -> "aanw") -> Map("pos" -> "ADV",  "type" -> "reg", "subtype" -> "dem"), false),
    Rule("betr.bw\\.", Map("pos" -> "BW", "bwtype" -> "betr") -> Map("pos" -> "ADV", "type" -> "reg", "subtype" -> "rel"), false),
    Rule("ontk.bw\\.", Map("pos" -> "BW", "bwtype" -> "neg") -> Map("pos" -> "ADV", "type" -> "reg", "subtype" -> "neg"), false),
    Rule("vrag.bw.onbep\\.", Map("pos" -> "BW", "bwtype" -> "vrag|onbep") -> Map("pos" -> "ADV", "type" -> "reg",  "subtype"
      -> "inter|indef"), false), //!probleem, is dit echt zo?
    Rule("vrag.bw\\.", Map("pos" -> "BW", "bwtype" -> "vrag") -> Map("pos" -> "ADV", "type" -> "reg", "subtype" -> "inter"), false),
    Rule("aanw.bijw\\.", Map("pos" -> "BW", "bwtype" -> "aanw") -> Map("pos" -> "ADV", "type" -> "reg", "subtype" -> "dem"), false),
    Rule("ontk.bijw\\.", Map("pos" -> "BW", "bwtype" -> "neg") -> Map("pos" -> "ADV", "type" -> "reg", "subtype" -> "neg"), false),


    Rule("bijw\\.", Map("pos" -> "BW") -> Map("pos" -> "ADV"), false),
    Rule("bw\\.", Map("pos" -> "BW") -> Map("pos" -> "ADV"), false),

    Rule("aanw\\.vnw\\.", Map("pos" ->  "VNW", "vwtype" -> "aanw") -> Map("pos" ->  "PD", "type" -> "dem"), false),
    Rule("bez\\.vnw\\.", Map("pos" ->  "VNW", "vwtype" -> "bez") -> Map("pos" ->  "PD", "type" -> "poss"), false),
    Rule("betr\\.vnw\\.", Map("pos" ->  "VNW", "vwtype" -> "betr") -> Map("pos" ->  "PD", "type" -> "rel"), false),
    Rule("vrag\\.vnw\\.", Map("pos" ->  "VNW", "vwtype" -> "vrag") -> Map("pos" ->  "PD", "type" -> "inter"), false),
    Rule("wdk\\.vnw\\.", Map("pos" ->  "VNW", "vwtype" -> "refl") -> Map("pos" ->  "PD", "type" -> "refl"), false),
    Rule("wkg\\.vnw\\.", Map("pos" ->  "VNW", "vwtype" -> "recip") -> Map("pos" ->  "PD", "type" -> "recip"), false),
    Rule("onbep\\.vnw\\.", Map("pos" ->  "VNW", "vwtype" -> "onbep") -> Map("pos" ->  "PD", "type" -> "indef"), false),
    Rule("pers\\.vnw\\.", Map("pos" ->  "VNW", "vwtype" -> "pers") -> Map("pos" ->  "PD", "type" -> "pers"), false),
    Rule("pers\\.", Map("pos" ->  "VNW", "vwtype" -> "pers") -> Map("pos" ->  "PD", "type" -> "pers"), false),

    Rule("\\(bijv\\.\\)", Map("positie" -> "prenom") -> Map("position" -> "prenom"), false), // !! probleem kan dit niet postnom zijn?
    /*
        (Tag(aanw.,,Map(pos -> BW),aanw.bw.,),36)
          (Tag(betr.,,Map(pos -> BW),betr.bw.,),27)
          (Tag(bnw,Lat. context,Map(),bnw,Lat. context),1)
          (Tag(mv.,Lat. context,Map(pos -> N, ntype -> soort),znw.mv.,Lat. context),2)
          (Tag(nevensch.vw.,,Map(),nevensch.vw.,),1048)
          (Tag(nevensch.vw.,Lat. context,Map(),nevensch.vw.,Lat. context),3)
          (Tag(ondersch.vw.,,Map(),ondersch.vw.,),426)
          (Tag(ontk.,,Map(pos -> BW),ontk.bw.,),223)
          (Tag(pers..  ,,Map(getal -> ev, persoon -> 3, naamval -> gen),pers.. 3e sg.,gen.),1)
      (Tag(persoonsnaam,,Map(naamval -> dat),persoonsnaam,dat.),2)
      (Tag(rel.partikel,-.,Map(pos -> BW),bw.rel.partikel,-.),1)
      (Tag(rel.partikel,,Map(pos -> BW),bw.rel.partikel,),3)
      (Tag(telw.,Lat. context,Map(),telw.,Lat. context),1)
    */

    Rule("tussenw\\.", Map("pos" -> "TSW") -> Map("pos" -> "INT"), false),



    Rule("vnw\\.", Map("pos" -> "VNW") -> Map("pos" -> "PD"), false),




    Rule("onbep.hoofdtelw\\.", Map("pos" -> "TW", "numtype" -> "onbep|hoofd")
      -> Map("pos" -> "PD", "type" -> "indef"), false), // ?
    Rule("bep.hoofdtelw\\.", Map("pos" -> "TW", "numtype" -> "bep|hoofd")
      -> Map("pos" -> "NUM", "type" -> "card"), false),
    Rule("onbep.rangtelw\\.", Map("pos" -> "TW", "numtype" -> "onbep|rang") -> Map("pos" -> "PD",
      "type" -> "indef"), false), // ?
    Rule("bep.rangtelw\\.", Map("pos" -> "TW", "numtype" -> "bep|rang") -> Map("pos" -> "NUM", "type" -> "ord"), false),

    Rule("hoofdtelw\\.", Map("pos" -> "TW", "numtype" -> "hoofd") -> Map("pos" -> "NUM", "type" -> "card"), false),
    Rule("rangtelw\\.", Map("pos" -> "TW", "numtype" -> "rang") -> Map("pos" -> "NUM", "type" -> "ord"), false),
    Rule("onbep.telw\\.", Map("pos" -> "TW", "numtype" -> "onbep") -> Map("pos" -> "PD", "type" -> "indef"), false),
    Rule("telw\\.", Map("pos" -> "TW") -> Map("pos" -> "NUM"), false),

    Rule("st\\.", Map("flexietype" ->  "sterk") -> Map("declination" ->  "strong"), false),
    Rule("zw\\.", Map("flexietype" ->  "zwak") -> Map("declination" ->  "weak"), false),
    Rule("zw,", Map("flexietype" ->  "zwak") -> Map("declination" ->  "weak"), false),
    Rule("zwak", Map("flexietype" ->  "zwak") -> Map("declination" ->  "weak"), false),
    Rule("zwak", Map("flexietype" ->  "zwak") -> Map("declination" ->  "weak"), true),
    Rule("sterk", Map("flexietype" ->  "sterk") -> Map("declination" ->  "strong"), false),
    Rule("sterk", Map("flexietype" ->  "sterk") -> Map("declination" ->  "strong"), true),



    Rule("redup[a-z]*\\.", Map("flexietype" ->  "sterk|redup") -> Map("declination" ->  "strong"), false),

    Rule("\\(sterk\\)", Map("flexietype" ->  "sterk") -> Map("declination" ->  "strong"), false),
    Rule("\\(zwak\\)", Map("flexietype" ->  "zwak") -> Map("declination" ->  "weak"), false),

    Rule("\\(?onr\\.\\)?", Map("flexietype" ->  "onregelmatig") -> Map("declination" ->  "irreg"), false),
    Rule("\\(?pret.-?pres\\.\\)?", Map("flexietype" ->  "pretpres") -> Map("declination" ->  "pretpres"), false),


    Rule("sg./pl\\.", Map("getal" -> "ev|mv") -> Map("number" -> "sg|pl"), true),
    Rule("sg.pl\\.", Map("getal" -> "ev|mv") -> Map("number" -> "sg|pl"), true),
    Rule("pl./sg\\.", Map("getal" -> "mv|ev") -> Map("number" -> "pl|sg"), true),
    Rule("pl.sg\\.", Map("getal" -> "mv|ev") -> Map("number" -> "pl|sg"), true),




    Rule("acc.pl./gen.pl.", Map("getal" -> "mv", "naamval" -> "acc|gen") ->
      Map("number" -> "pl", "case" -> "acc|gen"), true),
    Rule("sg\\.", Map("getal" -> "ev") -> Map("number" -> "sg"), true),
    Rule("ev\\.", Map("getal" -> "ev") -> Map("number" -> "sg"), true),
    Rule("pl\\.", Map("getal" -> "mv") -> Map("number" -> "pl"), true),

    Rule("sg\\.", Map("getal" -> "ev") -> Map("number" -> "sg"), false),
    Rule("pl\\.", Map("getal" -> "mv") -> Map("number" -> "pl"), false),
    Rule("plur.", Map("getal" -> "mv") -> Map("number" -> "pl"), false),
    Rule("plur", Map("getal" -> "mv") -> Map("number" -> "pl"), false),
    Rule("mv\\.", Map("getal" -> "mv") -> Map("number" -> "pl"), false),

    Rule("1e/3e", Map("persoon" -> "1|3") -> Map("person" -> "1|3"), true),
    Rule("1e", Map("persoon" -> "1") -> Map("person" -> "1"), true),
    Rule("2e", Map("persoon" -> "2") -> Map("person" -> "2"), true),
    Rule("3e", Map("persoon" -> "3") -> Map("person" -> "3"), true),

    Rule("1e", Map("persoon" -> "1") -> Map("person" -> "1"), false),
    Rule("2e", Map("persoon" -> "2") -> Map("person" -> "2"), false),
    Rule("3e", Map("persoon" -> "3") -> Map("person" -> "3"), false),


    Rule("dat.plur.onz", Map("genus" ->  "fem") -> Map("gender" ->  "n", "case" -> "dat", "number" -> "pl"), true),

    //nom./acc
    Rule("nom./acc\\.", Map("naamval" -> "nom|acc") -> Map("case" -> "nom|acc"), true),
    Rule("nom./gen\\.", Map("naamval" -> "nom|gen") -> Map("case" -> "nom|gen"), true),
    Rule("gen./dat\\.", Map("naamval" -> "gen|dat") -> Map("case" -> "gen|dat"), true),
    Rule("dat./acc\\.", Map("naamval" -> "dat|acc") -> Map("case" -> "dat|acc"), true),

    Rule("datief", Map("naamval" -> "dat") -> Map("case" -> "dat"), true),
    Rule("dat\\.", Map("naamval" -> "dat") -> Map("case" -> "dat"), true),
    Rule("acc\\.", Map("naamval" -> "acc") -> Map("case" -> "acc"), true),
    Rule("adverbiale genitief", Map("naamval" -> "gen") -> Map("case" -> "gen", "position" -> "free"), true),
    Rule("gen\\.", Map("naamval" -> "gen") -> Map("case" -> "gen"), true),
    Rule("nom\\.", Map("naamval" -> "nomin") -> Map("case" -> "nom"), true),
    Rule("voc\\.", Map("naamval" -> "voc") -> Map("case" -> "voc"), true),



    Rule("mann./vr\\.", Map("genus" ->  "masc|fem") -> Map("gender" ->  "m|f"), true),
    Rule("mann./onz\\.", Map("genus" ->  "masc|onz") -> Map("gender" ->  "m|n"), true),
    Rule("vr./onz\\.", Map("genus" ->  "fem|onz") -> Map("gender" ->  "f|n"), true),

    Rule("vr\\.", Map("genus" ->  "fem") -> Map("gender" ->  "f"), true),
    Rule("onz\\.", Map("genus" ->  "onz") -> Map("gender" ->  "n"), true),
    Rule("mann\\.", Map("genus" ->  "masc") -> Map("gender" ->  "m"), true),
    Rule("mann", Map("genus" ->  "masc") -> Map("gender" ->  "m"), true),

    Rule("m./v\\.", Map("genus" ->  "masc|fem") -> Map("gender" ->  "m|f"), false),
    Rule("o./v\\.", Map("genus" ->  "onz|fem") -> Map("gender" ->  "n|f"), false),
    Rule("m./v\\.", Map("genus" ->  "masc|fem") -> Map("gender" ->  "m|f"), true),
    Rule("o./v\\.", Map("genus" ->  "onz|fem") -> Map("gender" ->  "n|f"), true),



    Rule("m\\. en v\\.", Map("genus" ->  "masc|fem") -> Map("gender" ->  "m|f"), false),
    Rule("m.[/,]v\\.", Map("genus" ->  "masc|fem") -> Map("gender" ->  "m|f"), true),
    Rule("m.[/,]v\\.", Map("genus" ->  "masc|fem") -> Map("gender" ->  "m|f"), false),

    Rule("v.[/,]o\\.", Map("genus" ->  "fem|onz") -> Map("gender" ->  "f|n"), true),
    Rule("v.[/,]o\\.", Map("genus" ->  "fem|onz") -> Map("gender" ->  "f|n"), false),

    Rule("v./o\\.", Map("genus" ->  "fem|onz") -> Map("gender" ->  "f|n"), false),
    Rule("m./o\\.", Map("genus" ->  "masc|onz") -> Map("gender" ->  "m|n"), false),


    Rule("m./o\\.", Map("genus" ->  "masc|onz") -> Map("gender" ->  "m|n"), true),

    Rule("^v ?\\.", Map("genus" ->  "fem") -> Map("gender" ->  "f"), false),
    Rule("^v\\.", Map("genus" ->  "fem") -> Map("gender" ->  "f"), true),
    Rule("^o\\.", Map("genus" ->  "onz") -> Map("gender" ->  "n"), false),
    Rule("^m\\.", Map("genus" ->  "masc") -> Map("gender" ->  "m"), false),


    Rule("^v\\.", Map("genus" ->  "fem") -> Map("gender" ->  "f"), true),
    Rule("^o\\.", Map("genus" ->  "onz") -> Map("gender" ->  "n"), true),
    Rule("^m\\.", Map("genus" ->  "masc") -> Map("gender" ->  "m"), true),

    Rule("pret./pres.", Map("tijd" -> "verl|tgw") -> Map("tense" -> "past|pres"), true),
    Rule("pres\\.", Map("tijd" -> "tgw") -> Map("tense" -> "pres"), true),
    Rule("pres$", Map("tijd" -> "tgw") -> Map("tense" -> "pres"), true),
    Rule("pret\\.?", Map("tijd" -> "verl") -> Map("tense" -> "past"), true),


    Rule("ind./conj\\.", Map("modus" -> "ind|conj") -> Map("mood" -> "ind|conj"), true),
    Rule("conj\\.", Map("modus" -> "conj") -> Map("mood" -> "conj"), true),
    Rule("ind\\.", Map("modus" -> "ind") -> Map("mood" -> "ind"), true),
    Rule("imp\\.", Map("modus" -> "imp") -> Map("mood" -> "imp"), true),

  )
}
