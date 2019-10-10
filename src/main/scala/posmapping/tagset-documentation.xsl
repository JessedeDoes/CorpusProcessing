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
  
<xsl:template match="/">
  <xsl:for-each select=".//mainPoS/pos">
    
  </xsl:for-each>
</xsl:template>
 
<xsl:template match="name">
  <tr><td><xsl:value-of select="./text()"/></td><td><xsl:value-of select="./@desc"/></td></tr>
</xsl:template>

</xsl:stylesheet>
