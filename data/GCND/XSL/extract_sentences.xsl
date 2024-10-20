<xsl:stylesheet
  version="3.0"
  xmlns:folia="http://ilk.uvt.nl/folia"
  xpath-default-namespace="http://ilk.uvt.nl/folia"
  xmlns:meta="http://gcnd.ivdnt.org/metadata"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- stylesheet om de alpino-zinnetjes met metadata uit de GCND folia bestanden te halen -->

<xsl:variable name="pid"><xsl:value-of select="//meta:gcnd_transcriptie_metadata/@xml:id"/></xsl:variable>
<xsl:variable name="docpid"><xsl:value-of select="/*/@xml:id"/></xsl:variable>
<xsl:variable name="opname_code"><xsl:value-of select="//metadata//meta:opname_code"/></xsl:variable>
<xsl:variable name="Code"><xsl:value-of select="//metadata//meta:opname_code"/></xsl:variable>
<xsl:variable name="status"><xsl:value-of select='//metadata//meta:transcriptie_status/meta:label'/></xsl:variable>
<xsl:variable name="docurl">http://svotmc10.ivdnt.loc/blacklab-server/GCND_mei/docs/<xsl:value-of select="$docpid"/>?outputformat=xml</xsl:variable>


<xsl:variable name="Provincie"><xsl:value-of select='//metadata//meta:plaats[@rel="opname⟶plaats"]//meta:provincie/meta:label'/></xsl:variable>
<xsl:variable name="Plaats_opname"><xsl:value-of select='//metadata//meta:plaats[@rel="opname⟶plaats"]//meta:naam'/></xsl:variable>

<xsl:variable name="doctitle"><xsl:value-of select="$Code"/>: <xsl:value-of select="$Provincie"/>, <xsl:value-of select="$Plaats_opname"/></xsl:variable>
<xsl:template match="/">
<document xml:id="{$pid}">
  <xsl:apply-templates select=".//speech"/>
</document>
</xsl:template>

<!--
  - name: Title
    value: ""
    process: 
    - action: append
      field: Code
    - action: append
      value: ": "
    - action: append
      field: Provincie
    - action: append
      value: ", "
    - action: append
      field: Plaats_opname
    - action: replace
      find: " :"
      replace: ":"
    - action: replace
      find: " ,"
      replace: ","
-->

<xsl:template match="@*"><xsl:attribute name="{name(.)}"><xsl:value-of select="."/></xsl:attribute></xsl:template>

<xsl:template match="*"><xsl:element name="{name()}"><xsl:apply-templates select="@*"/><xsl:apply-templates select="node()"/></xsl:element></xsl:template>

<xsl:template match="@originalOffsets"/>

<xsl:template match="speech">

 <xsl:message><xsl:value-of select="$docurl"/>: <xsl:value-of select="$doctitle"/></xsl:message>

  <xsl:if test="contains(//metadata//meta:transcriptie_status/meta:label,'aalkundige verrijking geco')">
  <xsl:variable name="metadata" as="node()*">

	  <meta type="text" group="Identificatie" name="docpid" value="{$opname_code}"/>
		<meta type="text" group="Identificatie" name="doctitle" value="{$doctitle}"/>

		<meta type="text" group="Lokalisering" name="Plaats van opname"><xsl:attribute name="value"><xsl:value-of select='//metadata//meta:plaats[@rel="opname⟶plaats"]//meta:naam'/></xsl:attribute></meta>
		<meta type="text" group="Lokalisering" name="Kloekecode"><xsl:attribute name="value"><xsl:value-of select='//metadata//meta:plaats[@rel="opname⟶plaats"]//meta:kloeke_code'/></xsl:attribute></meta>
		<meta type="text" group="Lokalisering" name="Provincie"><xsl:attribute name="value"><xsl:value-of select='//metadata//meta:plaats[@rel="opname⟶plaats"]//meta:provincie/meta:label'/></xsl:attribute></meta>
		<meta type="text" group="Lokalisering" name="Land"><xsl:attribute name="value"><xsl:value-of select='//metadata//meta:plaats[@rel="opname⟶plaats"]//meta:land/meta:label'/></xsl:attribute></meta>
		<meta type="text" group="Lokalisering" name="Dialectindeling (ruw)"><xsl:attribute name="value"><xsl:value-of select='//metadata//meta:plaats[@rel="opname⟶plaats"]//meta:dialectgebied_ruw/meta:label'/></xsl:attribute></meta>
		<meta type="text" group="Lokalisering" name="Dialectindeling (fijn)"><xsl:attribute name="value"><xsl:value-of select='//metadata//meta:plaats[@rel="opname⟶plaats"]//meta:dialectgebied/meta:label'/></xsl:attribute></meta>
		<meta type="text" group="Lokalisering" name="SAND-plaats"><xsl:attribute name="value"><xsl:value-of select='//metadata//meta:opname//meta:sand_plaats'/></xsl:attribute></meta>
		<meta type="text" group="Lokalisering" name="RND-plaats"><xsl:attribute name="value"><xsl:value-of select='//metadata//meta:opname//meta:rnd_plaats'/></xsl:attribute></meta>

		<meta type="text" group="Opname" name="Bron"><xsl:attribute name="value"><xsl:value-of select='//metadata//meta:opname/meta:bron'/></xsl:attribute></meta>
    <meta type="text" group="Opname" name="Jaar van opname"><xsl:attribute name="value"><xsl:value-of select='//metadata//meta:opname/meta:jaar'/></xsl:attribute></meta>
		<meta type="text" group="Opname" name="Verificatiestatus"><xsl:attribute name="value"><xsl:choose>
      <xsl:when test="$status='transcriptie gecorrigeerd, geen taalkundige verrijking'">Transcriptie gecontroleerd, geen taalkundige verrijking</xsl:when>
      <xsl:when test="$status='transcriptie niet gecorrigeerd, taalkundige verrijking niet gecontroleerd'">Transcriptie niet gecontroleerd, taalkundige verrijking niet gecontroleerd</xsl:when>
      <xsl:when test="$status='transcriptie gecontroleerd, taalkundige verrijking niet gecontroleerd'">Transcriptie gecontroleerd, taalkundige verrijking niet gecontroleerd</xsl:when>
      <xsl:when test="$status='transcriptie niet gecorrigeerd, geen taalkundige verrijking'">Transcriptie niet gecontroleerd, geen taalkundige verrijking</xsl:when>
      <xsl:when test="$status='transcriptie gecontroleerd, taalkundige verrijking gecontroleerd'">Transcriptie gecontroleerd, taalkundige verrijking gecontroleerd</xsl:when></xsl:choose></xsl:attribute></meta> <!-- values mappen-->
    <meta group="speech" type="text" name="Spreker"><xsl:attribute name="value"><xsl:value-of select='@speaker'/></xsl:attribute></meta>
		<meta group="speech" type="text" name="Geslacht"><xsl:attribute name="value"><xsl:value-of select='.//meta:opname__persoon[.//meta:label="spreker"]//meta:gender//meta:label'/></xsl:attribute></meta>
		<meta group="speech" type="text" name="Beroep"><xsl:attribute name="value"><xsl:value-of select='let $b := .//meta:opname__persoon[.//meta:label="spreker"]//meta:beroep//meta:label/text() return $b[1]'/></xsl:attribute></meta>
		<meta group="speech" type="text" name="Geboorteplaats"><xsl:attribute name="value"><xsl:value-of select='.//meta:opname__persoon[.//meta:label="spreker"]//meta:plaats[contains(@rel,"boor")]//meta:naam'/></xsl:attribute></meta>
		<meta group="speech" type="text" name="Mobiliteit woonplaats"><xsl:attribute name="value"><xsl:value-of select='.//meta:opname__persoon[.//meta:label="spreker"]//meta:mobiliteit[contains(@rel,"woon")]//meta:label'/></xsl:attribute></meta>
		<meta group="speech" type="text" name="Mobiliteit werkplaats"><xsl:attribute name="value"><xsl:value-of select='.//meta:opname__persoon[.//meta:label="spreker"]//meta:mobiliteit[contains(@rel,"beroep")]//meta:label'/></xsl:attribute></meta>
		<meta group="speech" type="text" name="Herkomst partner"><xsl:attribute name="value"><xsl:value-of select='.//meta:opname__persoon[.//meta:label="spreker"]//meta:persoon_partnerplaats//meta:match_level'/></xsl:attribute></meta>
		<meta group="speech" type="text" name="Herkomst moeder"><xsl:attribute name="value"><xsl:value-of select='.//meta:opname__persoon[.//meta:label="spreker"]//meta:persoon_moederplaats//meta:match_level'/></xsl:attribute></meta>
		<meta group="speech" type="text" name="Herkomst vader"><xsl:attribute name="value"><xsl:value-of select='./meta:opname__persoon[.//meta:label="spreker"]//meta:persoon_vaderplaats//meta:match_level'/></xsl:attribute></meta></xsl:variable>
    
  <xsl:for-each select=".//*[local-name()='alpino_ds']">
   <alpino_ds>
     <xsl:for-each select="@*|node()[not (name()='alud')]"><xsl:apply-templates select="."/></xsl:for-each>
     <metadata>
         <meta type="text" name="speech_id"><xsl:attribute name="value"><xsl:value-of select="ancestor::speech/@xml:id"/></xsl:attribute></meta>
		     <meta type="text" name="sentence_id"><xsl:attribute name="value"><xsl:value-of select="ancestor::s/@xml:id"/></xsl:attribute></meta>
         <xsl:copy-of select="$metadata"/>
     </metadata>
   </alpino_ds>
  </xsl:for-each>
  </xsl:if>
</xsl:template>
</xsl:stylesheet>

