<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
        xmlns:edate="http://exslt.org/dates-and-times"
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:tei="http://www.tei-c.org/ns/1.0"
        xmlns:xlink="https://www.w3.org/1999/xlink"
        exclude-result-prefixes="tei edate xlink"
        version="2.0"
        xpath-default-namespace="http://www.tei-c.org/ns/1.0">

    <xsl:output method="xml" indent="yes"/>
    <!--
      <xsl:strip-space elements="*"/>
    -->

    <xsl:template match="/">

        <table>
            <xsl:apply-templates select=".//div1"/>
        </table>
    </xsl:template>

    <xsl:template match="head">
        <h3><xsl:apply-templates/></h3>
    </xsl:template>

    <xsl:template match="div1">

        <tr><td colspan="2"><xsl:apply-templates select="head"/></td></tr>

        <tr style="vertical-align: top">
            <td>
                <xsl:apply-templates select="./cit[@type='context']">
                </xsl:apply-templates>
            </td>
            <td>
                <xsl:apply-templates select="./cit[@type='translation']">

                </xsl:apply-templates>
            </td>
            <td>
                <xsl:for-each select="./p/w|./l/w"><xsl:call-template name="unlinked"/></xsl:for-each>
            </td>
        </tr>

    </xsl:template>

    <xsl:template match="cit[@type='context']//w">
        <xsl:choose>
            <xsl:when test="./@sameAs">
                <xsl:variable name="ref"><xsl:value-of select="substring-after(./@sameAs,'#')"/></xsl:variable>
                <xsl:variable name="w1"><xsl:value-of select="ancestor::div1//w[@xml:id=$ref]"/></xsl:variable>
                <xsl:message><xsl:value-of select="$w1"/></xsl:message>
                <xsl:variable name="aid"><xsl:value-of select="$w1/fs/f[@name='article']//text()"/></xsl:variable>
		<xsl:variable name="link">http://gtb.ivdnt.org/iWDB/search?actie=article&amp;wdb=ONW&amp;id=ID<xsl:value-of select="$aid"/></xsl:variable>
                <xsl:variable name="title">
                    <xsl:apply-templates select="ancestor::div1//w[@xml:id=$ref]"/>
                </xsl:variable>
                <span href="{$link}" title="{$title}"><xsl:apply-templates/></span>
            </xsl:when>
            <xsl:otherwise><i><xsl:apply-templates/></i></xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="w[not (ancestor::cit)]">
        <xsl:value-of select="concat(@lemma,': ', @pos, ' ', string-join(.//f[@name='flexie'], ','))"/>
    </xsl:template>

    <xsl:template name="unlinked">
        <xsl:variable name="ref">#<xsl:value-of select="./@xml:id"/></xsl:variable>
	<xsl:if test="not(ancestor::div1/cit//w[@sameAs=$ref])"><u><xsl:apply-templates select="./seg"/> (<xsl:value-of select="$ref"/>)</u> </xsl:if>
    </xsl:template>
</xsl:stylesheet>
