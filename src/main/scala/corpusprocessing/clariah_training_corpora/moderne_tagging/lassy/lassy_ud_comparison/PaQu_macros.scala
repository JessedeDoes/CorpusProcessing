// macro's for PaQu
// mostly by Gertjan van Noord
// comments welcome at g.j.m.van.noord@rug.nl
//
// any macro definition that is a conjunction or disjunction
// should be placed in () brackets in order to avoid counter-intuitive
// results
//
// any macro definition that starts with //node should not
// have outer () brackets
package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy_ud_comparison
import scala.util.matching.Regex._

object PaQu_macros  {

val macros = Map(

"PQ_b" ->  """ number(@begin) """,
"PQ_e" ->  """ number(@end) """,
"PQ_i" ->  """ number(@index) """,

"PQ_headrel" ->  """ @rel=("hd","cmp","mwp","crd","rhd","whd","nucl","dp") """,

"PQ_begin_of_head" ->  """ node[%PQ_headrel%]/%PQ_b% """,
"PQ_end_of_head"   -> """ node[%PQ_headrel%]/%PQ_e% """,

"PQ_begin_of_hd"   -> """ node[@rel="hd"]/%PQ_b% """,
"PQ_end_of_hd"     -> """ node[@rel="hd"]/%PQ_e% """,

"PQ_precedes_head_of_smain" ->  """
        (  ancestor::node[@cat="smain"]/node[@rel="hd"]/%PQ_b% > %PQ_begin_of_head%
           or
           (  ancestor::node[@cat="smain"]/node[@rel="hd"]/%PQ_b% > %PQ_b%
              and
              not(node[%PQ_headrel%])
           )
        ) """,

"PQ_precedes_head_of_whq" ->  """
        (  ancestor-or-self::node[@rel="whd"]/parent::node[@cat="whq"]/node[@rel="body" and @cat="sv1"]/node[@rel="hd"]/%PQ_b% > %PQ_begin_of_head%
           or
           (  ancestor-or-self::node[@rel="whd"]/parent::node[@cat="whq"]/node[@rel="body" and @cat="sv1"]/node[@rel="hd"]/%PQ_b% > %PQ_b%
              and
              not(node[%PQ_headrel%])
           )
        ) """,

"PQ_vorfeld" ->  """
        (
            (  /alpino_ds[@version = ("1.16", "1.17")] and @is_vorfeld )
               or
               ( /alpino_ds[not(@version = ("1.16", "1.17"))]
               and
               (
                   ( (  %PQ_precedes_head_of_smain%
                        and
                        not(parent::node[%PQ_precedes_head_of_smain%])
                     )
                     or
                     (  %PQ_precedes_head_of_whq%
                        and
                        not(parent::node[%PQ_precedes_head_of_whq%])
                     )
                   )
                   and
                   (@cat or @pt)
               )
            )
        ) """,

// This also finds vorfeld on an empty index node.
// This is very slow, so you might want to use the regular PQ_vorfeld, and
// do a search with expanded index nodes instead.
"PQ_vorfeld_slow" ->  """
        (  %PQ_vorfeld%
           or
           //node[%PQ_vorfeld%]/%PQ_i% = %PQ_i%
        ) """,

"PQ_single_name" ->  """
        ( @ntype = 'eigen'
          or
          @postag='SPEC(deeleigen)'
        ) """,

"PQ_multi_name" ->  """
        ( @cat='mwu'
          and
          node[@rel='mwp'
               and
               %PQ_single_name%
          ]
        ) """,

"PQ_name" ->  """
        ( %PQ_single_name%
          or
          %PQ_multi_name%
        ) """,

"PQ_name_phrase" ->  """
        ( %PQ_name%
          or
          node[@rel="hd"
               and
               %PQ_name%
          ]
        ) """,

"PQ_vp" ->  """ @cat=("inf","ti","ssub","oti","ppart") """,

"PQ_s" ->  """
        ( %PQ_vp%
          or
          @cat=("smain","sv1")
        ) """,

"PQ_follows_head_of_vp" ->  """
        ( not(@cat=("inf","ppart") or @rel=("hd","svp"))
          and
          ( ancestor::node[%PQ_vp%]/node[@rel="hd"]/%PQ_b% < %PQ_begin_of_head%
            or
            ancestor::node[%PQ_vp%]/node[@rel="hd"]/%PQ_b% < %PQ_b% and not(node[%PQ_headrel%])
          )
        ) """,

"PQ_dep_node_in_verbcluster" ->  """
        ( @rel="vc"
          and
          @cat=("ti","inf","ppart")
          and
          node/%PQ_b% < ../node[@rel="hd"
                                and
                                @pt="ww"
                                and
                                not(../@cat=("smain","sv1"))
          ]/%PQ_b%
        ) """,

"PQ_dep_verb_in_verbcluster" ->  """
        ( ( @rel="hd"
            and
            @pt="ww"
            and parent::node[%PQ_dep_node_in_verbcluster%]
          )
          or
          ( @rel="cmp"
            and
            @lemma="te"
            and
            parent::node[%PQ_dep_node_in_verbcluster%]
          )
          or
          ( @rel="hd"
            and
            @pt="ww"
            and parent::node[parent::node[%PQ_dep_node_in_verbcluster%]]
          )
        ) """,

"PQ_head_verb_in_verbcluster" ->  """
        ( not(../@cat=("smain","sv1"))
          and
          @rel="hd"
          and
          ../node[%PQ_dep_node_in_verbcluster%]
        ) """,

"PQ_verb_in_verbcluster" ->  """
        //node[%PQ_dep_verb_in_verbcluster%
               or
               %PQ_head_verb_in_verbcluster%
        ] """,

// omdat ik jou gezien heb
//
// this query is slightly too permissive in that it will
// find topicalized participles as well
// (cases such as "Gestart zal worden met een introductie")
"PQ_groen" ->  """
        //node[@rel="hd"
               and
               @wvorm="vd"
               and
               %PQ_b% < parent::node[@rel="vc"]/../node[@rel="hd"]/%PQ_b%
               and
               not(../../@cat=("smain","sv1"))
        ] """,

// verwacht werd dat hij komt
"PQ_topicalized_participle" ->  """
        //node[@wvorm="vd"
               and
               @rel="hd"
               and
               ../@rel="vc"
               and
               %PQ_b% = ancestor::node[@cat="smain"]/%PQ_b%
        ] """,

// omdat ik jou heb gezien
"PQ_rood" ->  """
        //node[@rel="hd"
               and
               @wvorm="vd"
               and
               %PQ_b% > ../../node[@rel="hd" and @pt="ww"]/%PQ_b%
               and
               not(../../@cat=("smain","sv1"))
        ] """,

// this query depends on co-indexing and therefore
// does not work for the CGN treebank
//
// "dat hij hen hun eigen graf zag graven"
"PQ_cross_serial_verbcluster" ->  """
        //node[%PQ_dep_node_in_verbcluster%
               and
               @cat="inf"
               and
               ../node[@rel="obj1"]/%PQ_i% = node[@rel="su"]/%PQ_i%
        ] """,

// only personal passives
// this query depends on co-indexing and therefore
// does not work for the CGN treebank
"PQ_passive" ->  """
        ( @rel="vc"
          and
          ( ( @cat="ppart"
              and
              node[@rel="obj1"]/%PQ_i% = ../../node[@rel="su"]/%PQ_i%
            )
            or
            ( @cat="ti"
              and
              node/node[@rel="obj1"]/%PQ_i% = ../../node[@rel="su"]/%PQ_i%
            )
          )
        ) """,

// only impersonal passives
// "in Rotterdam wordt gefeest"
//
// does not work for the CGN treebank
"PQ_impersonal_passive" ->  """
        (  (  %PQ_impersonal_passive_man%
              or
              %PQ_impersonal_passive_aut%
           )
           and
           parent::node[not(node[@rel="su"])
                        and
                        ( @cat=("smain","ssub")
                          or
                          ( @cat="sv1"
                            and
                            not(%PQ_imperatieven%)
                          )
                        )
           ]
        ) """,

"PQ_impersonal_passive_aut" ->  """
        ( @rel="vc"
          and
          ../node[@rel="hd"
                  and
                  @sc=("passive","te_passive")
          ]
        ) """,

"PQ_impersonal_passive_man" ->  """
        ( @rel="vc"
          and
          ../node[@rel="hd"
                  and
                  @pt="ww"
          ]
          and
          ( ( @cat="ppart"
              and
              not(node[@rel=("obj1","su","vc","predc")])
            )
            or
            ( @cat="ti"
              and
              not(node[@rel="body"]/node[@rel=("obj1","su","vc","predc")])
            )
          )
        ) """,

"PQ_laag_ev" ->  """
        ( ../@cat="np"
          and
          @rel="hd"
          and
          @genus
          and
          string(@genus)
          =
          ../node[@rel="mod"
                  and
                  @cat="pp"]/node[@rel="obj1"
                                  and
                                  @cat="np"
                                  and
                                  node[@rel="mod"
                                       and
                                       @cat="rel"
                                  ]
          ]/node[@rel="hd"
                 and
                 @genus]/string(@genus)
        ) """,

"PQ_laag_mv" ->  """
        ( ../@cat="np"
          and
          @rel="hd"
          and
          @getal="mv"
          and
          ../node[@rel="mod"
                  and
                  @cat="pp"]/node[@rel="obj1"
                                  and
                                  @cat="np"
                                  and
                                  node[@rel="mod"
                                       and
                                       @cat="rel"
                                  ]
          ]/node[@rel="hd"
                 and
                 @getal="mv"
          ]
        ) """,

// relatieve bijzin die laag is aangehecht (dus er is een hogere
// noun waar de bijzin eventueel ook bij had kunnen worden aangehect)
"PQ_laag" ->  """ ( %PQ_laag_ev% or %PQ_laag_mv% ) """,

"PQ_hoog_ev" ->  """
        ( ../@cat="np"
          and
          @rel="hd"
          and
          @genus and
          string(@genus)
          =
          ../node[@cat="pp"]/node[@cat="np"]/node[@rel="hd"
                                                  and
                                                  @genus]/string(@genus)
          and
          ../node[@rel="mod"
                  and
                  @cat="rel"
          ]
        ) """,

"PQ_hoog_mv" ->  """
        ( ../@cat="np"
          and
          @rel="hd"
          and
          @getal="mv"
          and
          ../node[@cat="pp"]/node[@cat="np"]/node[@rel="hd"
                                                  and
                                                  @getal="mv"]
          and
          ../node[@rel="mod"
                  and
                  @cat="rel"
          ]
        ) """,

// relatieve bijzin die hoog is aangehecht, maar waar er dus ook een
// kandidaat is voor een eventuele lagere aanhechting
"PQ_hoog" ->  """ ( %PQ_hoog_ev% or %PQ_hoog_mv% ) """,

"PQ_np_zonder_lidwoord" ->  """
        ( ( @cat="np"
            and
            node[@rel="hd"
                 and
                 @pt="n"
                 and
                 @ntype="soort"
            ]
            and
            not(node[@rel="det"])
          )
          or
          ( @pt="n"
            and
            @ntype="soort"
            and
            not(@rel=("hd","mwp"))
          )
        ) """,

"PQ_onbepaald_lidwoord" ->  """ @lemma=("een","geen","veel","sommig","zo'n","enkel") """,

"PQ_np_onbepaald_lidwoord" ->  """
        ( @cat="np"
          and
          node[@rel="det"
               and
               %PQ_onbepaald_lidwoord%]
        ) """,

"PQ_onbepaalde_np" ->  """
        ( %PQ_np_zonder_lidwoord%
          or
          %PQ_np_onbepaald_lidwoord%
        ) """,

"PQ_bepaalde_np" ->  """
        ( @cat="np"
          and
          node[@rel="hd"
               and
               @ntype="soort"
          ]
          and
          node[@rel="det"
               and
               @lemma
               and
               not(%PQ_onbepaald_lidwoord%)
          ]
        ) """,

"PQ_mwdet_np" ->  """
        ( @cat="np"
          and
          node[@rel="hd"
               and
               @ntype="soort"
          ]
          and
          node[@rel="det"
               and
               @cat
          ]
        ) """,

"PQ_eigen" ->  """
        ( @cat="mwu"
          or
          ( @pt="n"
            and
            @ntype="eigen"
          )
          or
          @pt="spec"
        ) """,

"PQ_eigen_p" ->  """
        ( node[@rel="hd"
               and
               %PQ_eigen%
          ]
          or
          %PQ_eigen%
        ) """,

"PQ_pron" ->  """ @pt="vnw" """,

"PQ_pron_p" ->  """
        ( node[@rel="hd"
               and
               %PQ_pron%
          ]
          or
          %PQ_pron%
        ) """,

"PQ_adj" ->  """ @pt="adj" """,

"PQ_adj_p" ->  """
        ( node[@rel="hd"
               and
               %PQ_adj%
          ]
          or
          %PQ_adj%
        ) """,

"PQ_ww" ->  """ @pt="ww" """,

"PQ_ww_p" ->  """
        ( node[@rel="hd"
               and
               %PQ_ww%
          ]
          or
          %PQ_ww%
        ) """,

"PQ_tw" ->  """ @pt="tw" """,

"PQ_tw_p" ->  """
        ( node[@rel="hd"
               and
               %PQ_tw%
          ]
          or
          %PQ_tw%
        ) """,

"PQ_negatielemma" ->  """ @lemma=("niet","geen","nooit","nergens","niemand") """,

"PQ_s_met_negatie" ->  """
        ( %PQ_s%
          and
          ( node[%PQ_negatielemma%]
            or
            node/node[%PQ_negatielemma%]
          )
        ) """,

"PQ_pp_keten" ->  """
        ( @cat="pp"
          and
          count(.//node[@cat="pp"])>2
        ) """,

"PQ_max_pp_keten" ->  """
        //node[%PQ_pp_keten%
               and
               not(ancestor::node[%PQ_pp_keten%])
        ] """,

"PQ_su_op_zijn_lexical" ->  """
        ( @rel="su"
          and
          @word
          and
          ../node[@rel="hd"
                  and
                  @lemma="zijn"
          ]
          and
          ../node[@rel="predc"
                  and
                  @lemma="op"
          ]
        ) """,

"PQ_su_op_zijn_non_lex" ->  """
        ( @rel="hd"
          and
          @word
          and
          ../@rel="su"
          and
          ../../node[@rel="hd"
                     and
                     @lemma="zijn"
          ]
          and
          ../../node[@rel="predc"
                     and
                     @lemma="op"
          ]
        ) """,

"PQ_su_op_zijn_nloc_lex" -> """
        ( @word
          and
          %PQ_i% = //node[@rel="su"
                          and
                          ../node[@rel="hd"
                                  and
                                  @lemma="zijn"
                          ]
                          and
                          ../node[@rel="predc"
                                  and
                                  @lemma="op"
                          ]
          ]/%PQ_i%
        ) """,

"PQ_su_op_zijn_nloc_nlex" -> """
        ( @rel="hd"
          and
          @word
          and
          ../%PQ_i% = //node[@rel="su"
                             and
                             ../node[@rel="hd"
                                     and
                                     @lemma="zijn"
                             ]
                             and
                             ../node[@rel="predc"
                                     and
                                     @lemma="op"
                             ]
          ]/%PQ_i%
        ) """,

"PQ_su_op_zijn" ->  """
        ( %PQ_su_op_zijn_lexical%
          or
          %PQ_su_op_zijn_non_lex%
          or
          %PQ_su_op_zijn_nloc_lex%
          or
          %PQ_su_op_zijn_nloc_nlex%
        ) """,

"PQ_vv_bigram" ->  """
        //node[@pt="ww"
               and
               %PQ_e%=//node[@pt="ww"]/%PQ_b%
        ] """,

"PQ_vv_trigram_ssub" ->  """
        //node[@cat="ssub"
               and
               ( some $x in .//node[@pt="ww"],
                     $y in .//node[@pt="ww"],
                     $z in .//node[@pt="ww"]

                 satisfies
                 (  $x/%PQ_e% = $y/%PQ_b%
                    and
                    $y/%PQ_e% = $z/%PQ_b%
                 )
               )
        ] """,

"PQ_vv_fourgram_ssub" ->  """
        //node[@cat="ssub"
               and
               ( some $x in .//node[@pt="ww"],
                     $y in .//node[@pt="ww"],
                     $z in .//node[@pt="ww"],
                     $w in .//node[@pt="ww"]

                 satisfies

                 (  $x/%PQ_e% = $y/%PQ_b%
                    and
                    $y/%PQ_e% = $z/%PQ_b%
                    and
                    $z/%PQ_e% = $w/%PQ_b%
                 )
               )
        ] """,

"PQ_lcatnp" ->  """
        ( @lcat="np"
          and
          not(@rel=("hd","mwp"))
        ) """,

"PQ_barenp" ->  """
        ( @pt="n"
          and not(@rel="hd")
        ) """,

"PQ_pronnp" ->  """
        ( @pt="vnw"
          and
          @pdtype="pron"
          and
          not(@rel="hd")
        ) """,

"PQ_mwunp" ->  """
        ( @cat="mwu"
          and
          not(@rel="hd")
          and
          @rel=("su","obj1","obj2","app")
        ) """,

"PQ_basenp" ->  """
        ( @cat="np"
          or
          %PQ_lcatnp%
          or
          %PQ_barenp%
          or
          %PQ_pronnp%
          or
          %PQ_mwunp%
        ) """,

"PQ_np" ->  """
        (
            (
                /alpino_ds[@version=("1.16","1.17")]
                and
                @is_np
            )
            or
            (
                /alpino_ds[not(@version=("1.16","1.17"))]
                and
                (
                    %PQ_basenp%
                    or
                    ( @cat="conj" and node[%PQ_basenp%] )
                )
            )
        ) """,

"PQ_npnode" -> """ //node[%PQ_np%] """,

"PQ_finiete_zin" ->  """ @cat=("smain","sv1","ssub") """,

"PQ_finiete_inbedding0" ->  """
        ( %PQ_finiete_zin%
          and
          not(.//node[ %PQ_finiete_zin%])
          and
          not(ancestor::node[%PQ_finiete_zin%])
        ) """,

"PQ_finiete_inbedding1" ->  """
        ( %PQ_finiete_zin%
          and .//node[%PQ_finiete_zin%]
        ) """,

"PQ_finiete_inbedding2" ->  """
        ( %PQ_finiete_zin%
          and
          .//node[%PQ_finiete_inbedding1%]
        ) """,

"PQ_finiete_inbedding3" ->  """
        ( %PQ_finiete_zin%
          and
          .//node[%PQ_finiete_inbedding2%]
        ) """,

"PQ_finiete_inbedding4" ->  """
        ( %PQ_finiete_zin%
          and
          .//node[%PQ_finiete_inbedding3%]
        ) """,

"PQ_finiete_inbedding5" ->  """
        ( %PQ_finiete_zin%
          and
          .//node[%PQ_finiete_inbedding4%]
        ) """,

"PQ_finiete_inbedding6" ->  """
        ( %PQ_finiete_zin%
          and
          .//node[%PQ_finiete_inbedding5%]
        ) """,

"PQ_finiete_inbedding7" ->  """
        ( %PQ_finiete_zin%
          and
          .//node[%PQ_finiete_inbedding6%]
        ) """,

"PQ_finiete_inbedding8" ->  """
        ( %PQ_finiete_zin%
          and
          .//node[%PQ_finiete_inbedding7%]
        ) """,

"PQ_janee_vragen" ->  """
        //node[@cat='sv1'
               and
               not(@rel='body')
               and
               not(node[@rel="hd"
                        and
                        @stype
                        and
                        not(@stype='ynquestion')
                   ]
               )
               and
               number(@end) < //node[@word='?']/number(@end)
        ] """,

"PQ_imperatieven" ->  """
        ( @cat='sv1'
          and
          not(node[@rel='su'])
          and
          not(node[@rel='hd'
                   and
                   @stype
                   and
                   not( @stype="imparative" )
              ]
          )
          and
          not(node[@rel='hd'
                   and
                   ( @tense='past'
                     or
                     @pvagr=('mv','met-t')
                     or
                     @pvtijd='verl'
                     or
                     @lemma=('zijn','kunnen','willen','moeten','mogen','zullen','denken','geloven','vinden','hebben')
                   )
              ]
          )
        ) """,

"PQ_ssub" ->  """
        ( @cat="ssub"
          or
          ( @cat="conj"
            and
            node[@rel="cnj"
                 and
                 @cat="ssub"
            ]
          )
          or
          ( @cat="du"
            and
            node[@cat="ssub"]
          )
        ) """,

"PQ_relatieve_bijzinnen" ->  """
        //node[@cat='rel'
               and
               node[@rel='body'
                    and
                    %PQ_ssub%
               ]
        ] """,

"PQ_free_relatives" ->  """
        //node[@cat='whrel'
               and
               not(@rel="mod")
               and
               node[@rel='body'
                    and
                    %PQ_ssub%
               ]
        ] """,

"PQ_ingebedde_vraagzinnen" ->  """
        //node[@cat='whsub'
               and
               node[@rel='body'
                    and
                    %PQ_ssub%
               ]
        ] """,

"PQ_vorfeld_np_subject" ->  """
        //node[ %PQ_vorfeld%
                and
                @rel=("su","sup")
                and
                %PQ_np%
        ] """,

"PQ_vorfeld_np_no_subject" ->  """
        //node[ %PQ_vorfeld%
                and
                not(@rel=("su","sup"))
                and
                %PQ_np%
        ] """,

"PQ_non_local_coindex" ->  """
        ( %PQ_i% = ..//node[@cat="ssub"]//node[@cat="ssub"
                                               and
                                               not(../@rel="obcomp")]//node/%PQ_i%
        ) """,

"PQ_non_local_extraction" ->  """
        //node[@rel=("whd","rhd")
               and
               %PQ_non_local_coindex%
        ] """,

"PQ_local_extraction" ->  """
        //node[@rel=("whd","rhd")
               and
               not(%PQ_non_local_coindex%)
        ] """,

"PQ_local_extraction" ->  """
        //node[@rel=("whd","rhd")
               and
               not(%PQ_non_local_coindex%)
        ] """,

"PQ_s_dominating_vorfeld" ->  """
        ( @cat=("ssub","smain")
          and
          .//node[%PQ_vorfeld%]
        ) """,

"PQ_minimal_s_dominating_vorfeld" ->  """
        ( %PQ_s_dominating_vorfeld%
          and
          not(.//node[%PQ_s_dominating_vorfeld%])
        ) """,

"PQ_vorfeld_non_local" ->  """
        //node[ @cat="ssub"
                and
                %PQ_minimal_s_dominating_vorfeld%
        ] """,

"PQ_hoe_langer" ->  """
        ( node[ @lemma=("hoe","deste")
                or
                ( node[@lemma="des"]
                  and
                  node[@lemma="te"]
                )
          ] and
          node[ @graad="comp"]
        ) """,

"PQ_corr_comp" ->  """
        ( @cat="du"
          and count(.//node[ %PQ_hoe_langer% ])>1
        ) """,

"PQ_minimal_corr_comp" ->  """
        //node[%PQ_corr_comp%
               and
               not(.//node[%PQ_corr_comp%])
        ] """,

"PQ_ott" ->  """ ( @pvtijd="tgw" and not(%PQ_voltooid%) ) """,

"PQ_ovt" ->  """ ( @pvtijd="verl" and not(%PQ_voltooid%) ) """,

"PQ_vtt" ->  """ ( @pvtijd="tgw" and %PQ_voltooid% ) """,

"PQ_vvt" ->  """ ( @pvtijd="verl" and %PQ_voltooid% ) """,

"PQ_voltooid" ->  """
        (
            @rel="hd"
            and
            (  @lemma="hebben" or @lemma="zijn" )
            and
            ../node[@rel="vc" and @cat="ppart"]
        ) """,

"PQ_nachfeld_t" ->  """
        (
            (@cat or @pt)
            and
            not(@cat=("inf","ppart") or @rel=("hd","svp"))
            and
            (
                (
                    ancestor::node[%PQ_vp%]
                    except
                    ancestor::node[%PQ_vp%]/ancestor::node[%PQ_vp%]
                )/node[@rel="hd"]/%PQ_b% < %PQ_begin_of_head%
                or
                (
                    ancestor::node[%PQ_vp%]
                    except
                    ancestor::node[%PQ_vp%]/ancestor::node[%PQ_vp%]
                )/node[@rel="hd"]/%PQ_b% < %PQ_b% and not(node[%PQ_headrel%])
            )
        ) """,

"PQ_nachfeld" ->  """
        (
            (  /alpino_ds[@version = ("1.16", "1.17")] and @is_nachfeld )
               or
               ( /alpino_ds[not(@version = ("1.16", "1.17"))]
               and
               (
                   %PQ_nachfeld_t%
                   and
                   not(
                          ancestor::node[%PQ_nachfeld_t%]
                          except
                          ancestor::node[%PQ_vp%]/ancestor-or-self::node[%PQ_nachfeld_t%]
                   )
               )
            )
        ) """
   )

  def expand(s: String):String  = if (!s.contains("%PQ")) s else {
    val s1 = "%(PQ[^%]*)%".r.replaceAllIn(s, m => macros(m.group(1)))
    expand(s1)
  }

  val expandedMacros = macros.mapValues(expand)

   def main(args: Array[String]) : Unit = {
     macros.values.foreach(s => {
       val e = expand(s)
       if (e != s)
       println(
         s"""######
            |$s
            |--->
            |$e""".stripMargin)
     })
   }
}
