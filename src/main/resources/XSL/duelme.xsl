<xsl:stylesheet version="2.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fun="http://some.functions.nl"
                xmlns="http://www.w3.org/1999/xhtml">
  <xsl:output method="text"/>
  <xsl:strip-space elements="*" />

<xsl:function name="fun:triple"><xsl:param name="subject"/><xsl:param name="predicate"/><xsl:param name="object"/>
<xsl:value-of select="$subject"/><xsl:text> </xsl:text> <xsl:value-of select="$predicate"/> <xsl:text> </xsl:text>  <xsl:value-of select="$object"/> <xsl:text> .
</xsl:text>
</xsl:function>
  
  <xsl:template match="Lexicon">
<xsl:text>@prefix rdf: &lt;http://www.w3.org/1999/02/22-rdf-syntax-ns#&gt; .
@prefix rdfs: &lt;http://www.w3.org/2000/01/rdf-schema#&gt; .
@prefix synsem: &lt;http://www.w3.org/ns/lemon/synsem#&gt; .
@prefix lexinfo: &lt;http://www.lexinfo.net/ontology/2.0/lexinfo&gt; .
@prefix dcterms: &lt;http://purl.org/dc/terms/&gt; .
@prefix fabio: &lt;http://purl.org/spar/fabio/&gt; .
@prefix : &lt;http://rdf.ivdnt.org/lexica/duelme#&gt; .
@prefix wordnet: &lt;http://wordnet-rdf.princeton.edu/wn31/&gt; .
@prefix lmf: &lt;http://www.lexinfo.net/lmf&gt; .
@prefix intskos: &lt;http://ivdnt.org/schema/lexica#&gt; .
@prefix celex: &lt;http://rdf.ivdnt.org/lexica/celex#&gt; .
@prefix xs: &lt;http://www.w3.org/2001/XMLSchema#&gt; .
@prefix prov: &lt;http://www.w3.org/ns/prov#&gt; .
@prefix ontolex: &lt;http://www.w3.org/ns/lemon/ontolex#&gt; . &#xa;
@prefix olia: &lt;http://purl.org/olia/olia.owl#&gt; . &#xa;
</xsl:text>
    <xsl:apply-templates />
  </xsl:template>

  <xsl:template match="LexicalEntry">
    <xsl:text>:duelme_</xsl:text><xsl:value-of select="replace(./@expression, '[ /'']','_')"/><xsl:text> a ontolex:LexicalEntry;&#xa;</xsl:text>
    <xsl:text>    lmf:hasMWEPattern :mwepattern_</xsl:text><xsl:value-of select="replace(./@patternID, ' ', '_')"/><xsl:text>;&#xa;</xsl:text>
    <xsl:if test="./@type='collocation'">
      <xsl:text>    a intskos:collocation;&#xa;</xsl:text>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="./@conjugation='H'">
	<xsl:text>    intskos:conjugation celex:celex_lemma_38935;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@conjugation='Z'">
	<xsl:text>    intskos:conjugation celex:celex_lemma_122511;&#xa;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>    intskos:conjugation celex:celex_lemma_38935;&#xa;</xsl:text>
	<xsl:text>    intskos:conjugation celex:celex_lemma_122511;&#xa;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="./@comment">
      <xsl:text>    rdfs:comment "</xsl:text><xsl:value-of select="./@comment"/><xsl:text>";&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@polarity='NPI'">
      <xsl:text>    rdfs:label "Negative Polarity Item";&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@polarity='PPI'">
      <xsl:text>    rdfs:label "Positive Polarity Item";&#xa;</xsl:text>
    </xsl:if>
    <xsl:apply-templates />
    <xsl:text>.&#xa;&#xa;</xsl:text>
  </xsl:template>

  <xsl:template match="MweNode">
    <xsl:variable name="me">http://rdf.ivdnt.org/duelme/node/<xsl:value-of select="generate-id(.)"/></xsl:variable>
    <xsl:value-of select="fun:triple($me,'a','node')"/>
    <xsl:variable name="type">
    <xsl:if test="./@syntacticCategory='A'">
      <xsl:text>intskos:NotModifiableAdjective;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='A1'">
      <xsl:text>intskos:ModifiableAdjective;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='Adv'">
      <xsl:text>olia:Adverb;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='AdvP'">
      <xsl:text>olia:Adverbial;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='AP'">
      <xsl:text>olia:AdjectivePhrase;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='C'">
      <xsl:text>intskos:Complementizer;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='CP'">
      <xsl:text>intskos:ComplementizerPhrase;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='D'">
      <xsl:text>olia:Determiner;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='INF'">
      <xsl:text>olia:Infinitive;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='N'">
      <xsl:text>intskos:NotFreelyModifiableNoun;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='N1'">
      <xsl:text>intskos:ModifiableNoun;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='NP'">
      <xsl:text>olia:NounPhrase;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='OTI'">
      <xsl:text>intskos:OmTeInfinitivePhrase;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='P'">
      <xsl:text>olia:Preposition;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='PP'">
      <xsl:text>olia:PrepositionalPhrase;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='PRON'">
      <xsl:text>olia:Pronoun;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='REFLV'">
      <xsl:text>olia:Verb; olia:hasVoice olia:Reflexive;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='SSUB'">
      <xsl:text>olia:SubordicateClause;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='TI'">
      <xsl:text>intskos:TeInfinitePhrase;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='V'">
      <xsl:text>olia:Verb;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticCategory='VP'">
      <xsl:text>olia:VerbPhrase;&#xa;</xsl:text>
    </xsl:if>
    </xsl:variable>
    <xsl:value-of select="fun:triple($me,'a', $type)"/>
    <xsl:for-each select="./MweEdge"><xsl:call-template name="MweEdge"><xsl:with-param name="node"><xsl:value-of select="$me"/></xsl:with-param></xsl:call-template></xsl:for-each>
  </xsl:template>

  <xsl:template name="MweEdge">
    <xsl:param name="node"/>
    <xsl:variable name="me">http://rdf.ivdnt.org/duelme/egde/<xsl:value-of select="generate-id(.)"/></xsl:variable>
    <xsl:value-of select="fun:triple($me,'a','edge')"/>
    <xsl:variable name="type">
    <xsl:if test="./@syntacticFunction='body'">
      <xsl:text> intskos:BodyOfComplementizerPhrase;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticFunction='cmp'">
      <xsl:text>intskos:HeadOfComplementizerPhrase;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticFunction='det'">
      <xsl:text>olia:Determiner;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticFunction='hd'">
      <xsl:text>olia:Head;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticFunction='ld'">
      <xsl:text>intskos:LocativeDirectionalComplement;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticFunction='mod'">
      <xsl:text>olia:Modifier;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticFunction='obcomp'">
      <xsl:text>intskos:comparisonComplement;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticFunction='obj1'">
      <xsl:text>olia:DirectObject;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticFunction='obj2'">
      <xsl:text>olia:IndirectObject;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticFunction='pc'">
      <xsl:text>intskos:PPArgument;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticFunction='ppcomp'">
      <xsl:text>intskos:ComplementOfAPreposition;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticFunction='predc'">
      <xsl:text>intskos:PredicativeComplement;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticFunction='rpron'">
      <xsl:text>intskos:PredicativeAdjunct;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticFunction='se'">
      <xsl:text>intskos:ObligatoryReflexive;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticFunction='su'">
      <xsl:text>olia:SyntacticSubject;&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@syntacticFunction='vc'">
      <xsl:text>intskos:VerbalComplement;&#xa;</xsl:text>
    </xsl:if>
    </xsl:variable>
    <xsl:value-of select="./
  </xsl:template>

  <xsl:template match="DataRecords/DataRecord/CorpusExamples/CorpusExample">
    <xsl:text>intskos:hasAttestation [&#xa;</xsl:text>
    <xsl:text>  a prov:Entity;&#xa;</xsl:text>
    <xsl:text>  prov:value "</xsl:text><xsl:value-of select="replace(./@example,'&quot;','')" /><xsl:text>"@nl;&#xa;</xsl:text>
    <xsl:text>  prov:wasQuotedFrom [&#xa;</xsl:text>
    <xsl:text>    dcterms:title "</xsl:text><xsl:value-of select="./@pub" /><xsl:text>";&#xa;</xsl:text>
    <xsl:text>    xs:date "</xsl:text><xsl:value-of select="./@date" /><xsl:text>"];&#xa;</xsl:text>    
    <xsl:text>];&#xa;</xsl:text>
  </xsl:template>

  <xsl:template match="ExampleSentence">
    <xsl:text>    intskos:ExampleSentence "</xsl:text><xsl:value-of select="./@sentence"/><xsl:text>";&#xa;</xsl:text>
  </xsl:template>

  <xsl:template match="ListOfComponents">
    <xsl:text>    lmf:ListOfComponents [&#xa;</xsl:text>
    <xsl:apply-templates />
    <xsl:text>    ];&#xa;</xsl:text>
  </xsl:template>

  <xsl:template match="Component">
    <xsl:text>        lmf:Component [&#xa;</xsl:text>
    <xsl:text>            intskos:componentRank "</xsl:text><xsl:value-of select="count(preceding-sibling::Component) + 1"/><xsl:text>"^^xs:integer ;&#xa;</xsl:text>
    <xsl:if test="./@form">
      <xsl:text>            lmf:writtenForm "</xsl:text><xsl:value-of select="./@form" /><xsl:text>"@nl ;&#xa;</xsl:text>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="./@dBin='dob'">
	<xsl:text>            intskos:bindingType intskos:directObjectBound;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@dBin='iob'">
	<xsl:text>            intskos:bindingType intskos:indirectObjectBound;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@dBin='sb'">
	<xsl:text>            intskos:bindingType intskos:subjectBound;&#xa;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="./@nGen='het'">
	<xsl:text>            lexinfo:gender lexinfo:neuter;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@nGen='de'">
	<xsl:text>            lexinfo:gender lexinfo:commonGender;&#xa;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="./@nCount='mass'">
	<xsl:text>            lexinfo:number lexinfo:massNoun;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@nCount='count'">
	<xsl:text>            lexinfo:number intskos:countable;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@nCount='countmass'">
	<xsl:text>            lexinfo:number lexinfo:massNoun;&#xa;</xsl:text>
	<xsl:text>            lexinfo:number intskos:countable;&#xa;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="./@nNum='sg'">
	<xsl:text>            lexinfo:number lexinfo:singular;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@nNum='pl'">
	<xsl:text>            lexinfo:number lexinfo:plural;&#xa;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:if test="./@nFrm='dim'">
      <xsl:text>            lexinfo:PartOfSpeech lexinfo:diminutiveNoun;&#xa;</xsl:text>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="./@aFrm='noe'">
	<xsl:text>            lexinfo:gender intskos:noe;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@aFrm='optepl'">
	<xsl:text>            lexinfo:gender intskos:optepl;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@aFrm='noesg'">
	<xsl:text>            lexinfo:gender intskos:noesg;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@aFrm='opte'">
	<xsl:text>            lexinfo:gender intskos:opte;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@aFrm='comp'">
	<xsl:text>            lexinfo:degree lexinfo:comparative;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@aFrm='sup'">
	<xsl:text>            lexinfo:degree lexinfo:superlative;&#xa;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="./@vFrm='inf'">
	<xsl:text>            lexinfo:verbFormMood lexinfo:infinitive;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@vFrm='part'">
	<xsl:text>            lexinfo:verbFormMood lexinfo:participle;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@vFrm='presp'">
	<xsl:text>            lexinfo:Tense lexinfo:present;</xsl:text>
	<xsl:text>            lexinfo:verbFormMood lexinfo:participle;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@vFrm='passp'">
	<xsl:text>            lexinfo:PartOfSpeech lexinfo:pastParticipleAdjective;&#xa;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="./@pPos='post'">
	<xsl:text>            lexinfo:PartOfSpeech lexinfo:postposition;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@pPos='prep'">
	<xsl:text>            lexinfo:PartOfSpeech lexinfo:preposition;&#xa;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:apply-templates />
    <xsl:text>        ];&#xa;</xsl:text>
  </xsl:template>

  <xsl:template match="Lemma">
    <xsl:text>            lmf:hasLemma "</xsl:text><xsl:value-of select="./@writtenForm" /><xsl:text>"@nl;&#xa;</xsl:text>
    <xsl:if test="./@particle">
      <xsl:text>            lexinfo:proposition "</xsl:text><xsl:value-of select="./@particle" /><xsl:text>"@nl;&#xa;</xsl:text>
    </xsl:if>
  </xsl:template>
      
  <xsl:template match="AlternativeDeterminer">
    <xsl:text>            intskos:AlternativeDeterminer [&#xa;</xsl:text>
    <xsl:choose>
      <xsl:when test="./@form='POSS'">
	<xsl:text>                lexinfo:PartOfSpeech lexinfo:possessivePronoun;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@form='REFL'">
	<xsl:text>                lexinfo:PartOfSpeech lexinfo:reflexivePersonalPronoun;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@form='PNP'">
	<xsl:text>                lexinfo:Phrase [lexinfo:NounPhrase lexinfo:possessive];&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@form='INDEF'">
	<xsl:text>                lexinfo:PartOfSpeech lexinfo:indefiniteDeterminer;&#xa;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="./@dBin='dob'">
	<xsl:text>                intskos:bindingType intskos:directObjectBound;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@dBin='iob'">
	<xsl:text>                intskos:bindingType intskos:indirectObjectBound;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@dBin='sb'">
	<xsl:text>                intskos:bindingType intskos:subjectBound;&#xa;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:apply-templates />
    <xsl:text>            ];&#xa;</xsl:text>
  </xsl:template>

  <xsl:template match="SemanticVariables">
    <xsl:apply-templates />
  </xsl:template>

  <xsl:template match="SemanticVariable">
    <xsl:text>intskos:semanticVariable [&#xa;</xsl:text>
    <xsl:text>rdfs:label "</xsl:text><xsl:value-of select="./@label" /><xsl:text>" ;&#xa;</xsl:text>
    <xsl:apply-templates />
    <xsl:text>] ;&#xa;</xsl:text>
  </xsl:template>

  <xsl:template match="OR">
    <xsl:apply-templates />
  </xsl:template>

  <xsl:template match="Restriction">
    <xsl:choose>
      <xsl:when test="./@restriction='FEM'">
	<xsl:text>intskos:restriction "female subject"@en ;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@restriction='MALE'">
	<xsl:text>intskos:restriction "male subject"@en ;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@restriction='SG'">
	<xsl:text>intskos:restriction "singular subject"@en ;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@restriction='PL'">
	<xsl:text>intskos:restriction "plural (or mass) subject"@en ;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@restriction='PLSGMET'">
	<xsl:text>intskos:restriction "the subject should be either plural (or mass), or else combined with a comitative-met clause"@en ;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@restriction='PLSGALS'">
	<xsl:text>intskos:restriction "the subject should be either plural (or mass),or else combined with a comparative-als clause"@en ;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@restriction='ANIM'">
	<xsl:text>intskos:restriction "animate subject"@en ;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@restriction='NONANIM'">
	<xsl:text>intskos:restriction "non-animate subject"@en ;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@restriction='UNSPECIFIED'">
	<xsl:text>intskos:restriction "unspecified"@en ;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@restriction='HET'">
	<xsl:text>intskos:restriction "the subject must be the expletive het"@en ;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@restriction='HETSSUB'">
	<xsl:text>intskos:restriction "besides a normal realization of the subject, the subject can also be a clause starting with a complementizer and either occupying the first position of the sentence or occupying any other position combined with the anticipatory subject het in the subject position"@en ;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@restriction='NOHETSSUB'">
	<xsl:text>intskos:restriction "besides a normal realization of the subject, the subject can also be a clause starting with a complementizer occupying any position of the sentence other than the first, without the anticipatory subject het in the subject position"@en ;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@restriction='HETVP'">
	<xsl:text>intskos:restriction "besides a normal realization of the subject, the subject can also be an infinitive clause occupying any position of the sentence other than the first combined with the anticipatory subject het in the subject position"@en ;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@restriction='NOHETVP'">
	<xsl:text>intskos:restriction "besides a normal realization of the object, the object can also be an infinitive clause without the anticipatory object het in the object position"@en ;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@restriction='SSUB'">
	<xsl:text>intskos:restriction "the complement of the pronominalized PP must be realized as a clause starting with a complementizer"@en ;&#xa;</xsl:text>
      </xsl:when>
      <xsl:when test="./@restriction='VP'">
	<xsl:text>intskos:restriction "the complement of the pronominalized PP must be realized as an infinitive clause"@en ;&#xa;</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="SyntacticVariables">
   <xsl:apply-templates />
  </xsl:template>

  <xsl:template match="SyntacticVariable">
    <xsl:text>intskos:SyntacticVariable [&#xa;</xsl:text>
    <xsl:text>intskos:syntacticFunction "</xsl:text><xsl:value-of select="./@syntacticFunction" /><xsl:text>"@en ;&#xa;</xsl:text>
    <xsl:apply-templates />
    <xsl:text>] ;&#xa;</xsl:text>
  </xsl:template>

  <xsl:template match="MwePattern">
    <xsl:text>:mwepattern_</xsl:text><xsl:value-of select="replace(./@ID, ' ','_')" /><xsl:text>&#xa;</xsl:text>
    <xsl:text>    a lmf:MwePattern;&#xa;</xsl:text>
    <xsl:if test="./@description">
      <xsl:text>    rdf:Description "</xsl:text><xsl:value-of select="replace(./@description,'&quot;', '''')" /><xsl:text>";&#xa;</xsl:text>
    </xsl:if>
    <xsl:if test="./@comments">
      <xsl:text>    rdfs:comment "</xsl:text><xsl:value-of select="./@comments" /><xsl:text>";</xsl:text>
    </xsl:if>
    <xsl:apply-templates />
    <xsl:text>    .&#xa;&#xa;</xsl:text>
  </xsl:template>

  <xsl:template match="MappingList">
    <xsl:text>    intskos:mappingList [&#xa;</xsl:text>
    <xsl:text>        rdf:label "</xsl:text><xsl:value-of select="./@mapping" /><xsl:text>";&#xa;</xsl:text>
    <xsl:apply-templates />
    <xsl:text>        ];&#xa;</xsl:text>
  </xsl:template>
  
  <xsl:template match="MappingNumber">
    <xsl:text>        intskos:mappingNumber "</xsl:text><xsl:value-of select="./@value" /><xsl:text>"^^xs:integer;&#xa;</xsl:text>
  </xsl:template>

  <!--xsl:template match="MweNode">
    <xsl:text>lmf:MWENode [&#xa;</xsl:text>
    <xsl:if test="./@syntacticCategory">
      <xsl:text>intskos:syntacticCategory "</xsl:text><xsl:value-of select="./@syntacticCategory" /><xsl:text>";&#xa;</xsl:text>
    </xsl:if>
    <xsl:apply-templates />
    <xsl:text>];&#xa;</xsl:text>
  </xsl:template -->


  <xsl:template match="MweLex">
    <xsl:text>lmf:MWELex [&#xa;</xsl:text>
    <xsl:text>intskos:componentRank "</xsl:text><xsl:value-of select="./@componentRank" /><xsl:text>"^^xs:integer;&#xa;</xsl:text>
    <xsl:text>intskos:graphicalSeparator "</xsl:text><xsl:value-of select="./@graphicalSeparator" /><xsl:text>";&#xa;</xsl:text>
    <xsl:apply-templates />
    <xsl:text>];&#xa;</xsl:text>
  </xsl:template>
    

</xsl:stylesheet>
