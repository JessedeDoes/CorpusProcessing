<xsl:stylesheet version="2.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fun="http://some.functions.nl"
                xmlns="http://www.w3.org/1999/xhtml">
  <xsl:output method="html"/>
  <xsl:strip-space elements="*" />

<xsl:function name="fun:triple"><xsl:param name="subject"/><xsl:param name="predicate"/><xsl:param name="object"/>
<xsl:value-of select="$subject"/><xsl:text> </xsl:text> <xsl:value-of select="$predicate"/> <xsl:text> </xsl:text>  <xsl:value-of select="$object"/> <xsl:text> .
</xsl:text>
</xsl:function>
  
<xsl:template match="/">
  <xsl:for-each select=".//mainPoS/pos">
    <xsl:variable name="pos"><xsl:value-of select="./text()"/></xsl:variable>
    <h1><xsl:value-of select="./text()"/> (<xsl:value-of select="./@desc"/>)</h1>
    <xsl:for-each select="//constraint[./pos=$pos]">
      <xsl:for-each select=".//feature">
        <xsl:variable name="feature"><xsl:value-of select="./text()"/></xsl:variable>
        <h3><xsl:value-of select="$feature"/></h3>
        <ul><xsl:for-each select="//partitions/feature[./name=$feature]//featureValue[.//pos=$pos]"><li><xsl:value-of select="./value"/></li></xsl:for-each></ul>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:for-each>
</xsl:template>
 
<xsl:template match="name">
  <tr><td><xsl:value-of select="./text()"/></td><td><xsl:value-of select="./@desc"/></td></tr>
</xsl:template>

</xsl:stylesheet>
