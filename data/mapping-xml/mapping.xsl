<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    exclude-result-prefixes="xs"
    version="2.0">
    
  <xsl:template match="and">
      <xsl:for-each select="*">
          <xsl:if test="position() > 1">
              &amp;
          </xsl:if>
          <xsl:apply-templates></xsl:apply-templates>
      </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>