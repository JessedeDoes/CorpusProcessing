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
  
  
<xsl:template match="/">
	<html>
		<head>
  <style type="text/css">
    body { font-family: Calibri, Arial, Helvetica, Sans }
    h1 { font-size: 16pt; background-color: lightblue }
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
        <h2><xsl:value-of select="$feature"/></h2>
        <div style="font-style: italic"><xsl:value-of select="//partitions/feature[./name=$feature]/@desc"/></div>
        <ul style="column-count: 5"><xsl:for-each select="//partitions/feature[./name=$feature]//featureValue[.//pos=$pos]">
          <li>
            <xsl:variable name="value"><xsl:value-of select="encode-for-uri(replace(./value, '\|', '\\|'))"/></xsl:variable>
            <xsl:variable name="displayName"><xsl:choose>
              <xsl:when test="./@displayName">, '<xsl:value-of select="./@displayName"/>'</xsl:when>
              <xsl:when test="not(contains(./@desc, ''))"></xsl:when>
              <xsl:otherwise></xsl:otherwise>
            </xsl:choose></xsl:variable>
            <a target="_blank" style="text-decoration: none; color: black">
              <xsl:attribute name="href">http://<xsl:value-of select="$server"/>/corpus-frontend/<xsl:value-of select="//corpus"/>/search/hits?first=0&amp;number=20&amp;patt=%5Bpos%3D%22<xsl:value-of select="$pos"/>%22%26pos_<xsl:value-of select="$feature"/>%3D%22<xsl:value-of select="$value"/>%22%5D&amp;interface=%7B"form"%3A"search"%2C"patternMode"%3A"advanced"%7D</xsl:attribute>
              <span class="featureValue"><xsl:value-of select="./value"/><xsl:value-of select="$displayName"/></span><xsl:text>: </xsl:text> <span style="font-style: italic"><xsl:value-of select="./@desc"/></span> 
            </a>
          </li></xsl:for-each></ul>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:for-each>
  <xsl:if test=".//tags">
  <h1>Occurring tags</h1>
  <div style="column-count: 3">
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
 
<xsl:template match="name">
  <tr><td><xsl:value-of select="./text()"/></td><td><xsl:value-of select="./@desc"/></td></tr>
</xsl:template>

</xsl:stylesheet>
