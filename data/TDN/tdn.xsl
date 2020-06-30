<xsl:stylesheet version="2.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fun="http://some.functions.nl"
                xmlns="http://www.w3.org/1999/xhtml">
  <xsl:output method="xhtml"/>
  <xsl:strip-space elements="*" />

<xsl:function name="fun:triple"><xsl:param name="subject"/><xsl:param name="predicate"/><xsl:param name="object"/>
<xsl:value-of select="$subject"/><xsl:text> </xsl:text> <xsl:value-of select="$predicate"/> <xsl:text> </xsl:text>  <xsl:value-of select="$object"/> <xsl:text> .
</xsl:text>
</xsl:function>

  
<xsl:param name="server">svotmc10.ivdnt.loc</xsl:param>
<xsl:variable name="corpus"><xsl-value-of select="//corpus"/></xsl:variable> 
<xsl:param name="corpora">
  <xsl:sequence><corpora>
    <corpus id="ONW" name="ONW"><xsl:sequence select="document('Corpora/ONW/tagset_desc_temp.xml')"></xsl:sequence></corpus>
    <corpus id="gysseling_nt" name="Gysseling">Gysseling<xsl:sequence select="document('Corpora/Gysseling/tagset_desc_temp.xml')"></xsl:sequence></corpus>
    <corpus id="CGN_TDN" name="CGN">CGN<xsl:sequence select="document('Corpora/CGN/tagset_desc_temp.xml')"></xsl:sequence></corpus>
  </corpora></xsl:sequence>
</xsl:param>
  
<xsl:template match="/">
	<html>
		<head>
  <style type="text/css">
    body { font-family: Calibri, Arial, Helvetica, Sans }
    h1 { font-size: 16pt; background-color: lightgrey }
    h2 { font-size: 14pt }
    ul { list-style: none; margin-left: 1em; padding-left: 0;} 
    li::before { content: "— "; margin-left: -1em;  }
    .featureValue { background-color: lightgrey}
    .class { color: black }
  </style>
  </head>
  <body>
  <xsl:for-each select=".//mainPoS/pos">
    <xsl:variable name="pos"><xsl:value-of select="./text()"/></xsl:variable>
    <h1><xsl:value-of select="./@desc"/> (<xsl:value-of select="./text()"/>)</h1>
    <xsl:for-each select="//constraint[./pos=$pos]">
      <xsl:for-each select=".//feature[./text() != 'pos']">
        <xsl:variable name="feature"><xsl:value-of select="./text()"/></xsl:variable>
        <h2 style="margin-bottom:0.2em; font-size:11pt"><xsl:value-of select="$feature"/><xsl:text> </xsl:text><span style="font-weight:normal; font-style:italic">(<xsl:value-of select="//partitions/feature[./name=$feature]/@desc"/>)</span></h2>
        <div style="font-style: italic"></div>
	<ul style="column-count: 2;  font-size: 9pt"><xsl:for-each select="//partitions/feature[./name=$feature]//featureValue[(.//pos=$pos or not (.//pos)) and not(contains(./value/text(),'|'))]">
          <li>
            <xsl:variable name="value"><xsl:value-of select="encode-for-uri(replace(./value, '\|', '\\|'))"/></xsl:variable>
            <xsl:variable name="value_unencoded"><xsl:value-of select="./value"></xsl:value-of></xsl:variable>
            <xsl:variable name="displayName"><xsl:choose>
              <xsl:when test="./@displayName">, '<xsl:value-of select="./@displayName"/>'</xsl:when>
              <xsl:when test="not(contains(./@desc, ''))"></xsl:when>
              <xsl:otherwise></xsl:otherwise>
            </xsl:choose></xsl:variable>
            <a target="_blank" style="text-decoration: none; color: black">
              <xsl:attribute name="xhref">http://<xsl:value-of select="$server"/>/corpus-frontend/<xsl:value-of select="//corpus"/>/search/hits?first=0&amp;number=20&amp;patt=%5Bpos%3D%22<xsl:value-of select="$pos"/>%22%26pos_<xsl:value-of select="$feature"/>%3D%22<xsl:value-of select="$value"/>%22%5D&amp;interface=%7B"form"%3A"search"%2C"patternMode"%3A"advanced"%7D</xsl:attribute>
              <span class="featureValue"><xsl:value-of select="./value"/><xsl:value-of select="$displayName"/></span><xsl:text>: </xsl:text> <span style="font-style: italic"><xsl:value-of select="./@desc"/></span> 
            </a>
            <xsl:for-each select="$corpora/*/*">
              <xsl:if test=".//partitions/feature[./name=$feature]//featureValue[.//value=$value_unencoded and .//pos=$pos]">
              <xsl:variable name="curcorp_id"><xsl:value-of select="./@id"/></xsl:variable>
              <xsl:variable name="curcorp_name"><xsl:value-of select="./@name"/></xsl:variable>
              <xsl:text> </xsl:text>
              <a target="_blank" style="text-decoration: none; color: black; border-style:outset">
                <xsl:attribute name="href">http://<xsl:value-of select="$server"/>/corpus-frontend/<xsl:value-of select="$curcorp_id"/>/search/hits?first=0&amp;number=20&amp;patt=%5Bpos%3D%22<xsl:value-of select="$pos"/>%22%26pos_<xsl:value-of select="$feature"/>%3D%22<xsl:value-of select="$value"/>%22%5D&amp;interface=%7B"form"%3A"search"%2C"patternMode"%3A"advanced"%7D</xsl:attribute>
                <span class="featureValue"><xsl:value-of select="$curcorp_name"/></span><xsl:text> </xsl:text> <span style="font-style: italic"><xsl:value-of select="./@desc"/></span> 
              </a>
              </xsl:if>
            </xsl:for-each>
          </li></xsl:for-each></ul>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:for-each>
  <xsl:if test="//declaration">
    <h1>Declarations</h1>
    <div style="column-count: 2">
    <xsl:variable name="root"><xsl:sequence select="/"></xsl:sequence></xsl:variable>
    <xsl:message><xsl:value-of select="$corpora//*/name()"/></xsl:message>
    <xsl:for-each select="$corpora/*/*">
      
      <xsl:variable name="curcorp"><xsl:value-of select="./@name"/></xsl:variable>
      
      <h3><xsl:value-of select="$curcorp"/></h3>
      <div style="font-size:9pt">
	  <xsl:apply-templates select="$root//declaration[@corpus=$curcorp]"/>
      </div>
    </xsl:for-each>
    </div>
  </xsl:if>
  <xsl:if test=".//tags">
  <h1>Occurring tags</h1>
  <div style="column-count: 3; font-size: 9pt">
	  <xsl:for-each select=".//tag">
		  <div class='tag'>
		  <xsl:variable name="tag"><xsl:value-of select="encode-for-uri(replace(replace(replace(./text(), '\|', '\\|'),'\)','\\)'),'\(','\\('))"/></xsl:variable>
		  <a target="_blank" style="text-decoration: none; color: black">
              <xsl:attribute name="href">http://<xsl:value-of select="$server"/>/corpus-frontend/<xsl:value-of select="//corpus"/>/search/hits?first=0&amp;number=20&amp;patt=%5Bgrouping_pos_full%3D%22<xsl:value-of select="$tag"/>%22%5D&amp;interface=%7B"form"%3A"search"%2C"patternMode"%3A"advanced"%7D</xsl:attribute>
	      <xsl:value-of select="./text()"/></a></div>
	  </xsl:for-each>
  </div>
  </xsl:if>
  </body>
  </html>
</xsl:template>

<xsl:template match="declaration">
	<div><xsl:value-of select="replace(.,'\|\|.*','')"/></div>
</xsl:template>

<xsl:template match="name">
  <tr><td><xsl:value-of select="./text()"/></td><td><xsl:value-of select="./@desc"/></td></tr>
</xsl:template>

</xsl:stylesheet>
