import yaml
import re
import sys
'''
Dit scriptje haalt de xpath van velden uit de blacklab config, om de velden ook als metadata in de alpinozinnetjes te kunnen toevoegen

-<metadata>

<meta value="" name="group" type="text"/>

<meta value="nld" name="language" type="text"/>

<meta value="119" name="months" type="int"/>

<meta value="Child" name="role" type="text"/>

<meta value="male" name="sex" type="text"/>

<meta value="21" name="uttno" type="int"/>

</metadata>

</alpino_ds>
'''

sentence_metas = [ 
  '<meta type="text" name="speech_id"><xsl:attribute name="value"><xsl:value-of select="ancestor::speech/@xml:id"/></xsl:attribute></meta>',
  '<meta type="text" name="sentence_id"><xsl:attribute name="value"><xsl:value-of select="ancestor::s/@xml:id"/></xsl:attribute></meta>'
]

metas = [

]

with open(sys.argv[1]) as f:
   dict = yaml.load(f, Loader=yaml.FullLoader)

   metaFields = dict['metadata'][0]['fields']
   config = dict['corpusConfig']
   groupOf = {}

   for g in config['metadataFieldGroups']:
     groupName = g['name']
     for name in g['fields']:
       groupOf[name] = groupName

   for f in metaFields:
     name = f['name']
     if name in groupOf:
       group = groupOf[name]
       if 'valuePath' in f and group != 'Spreker':
         path = re.sub("'", '"',  f['valuePath'])
         metas.append(f'''<meta type="text" group="{group}" name="{name}"><xsl:attribute name="value"><xsl:value-of select='{path}'/></xsl:attribute></meta>''')  


   inlineTags = dict['annotatedFields']['contents']['inlineTags']

   speech = list(filter(lambda x: x['displayAs'] == 'Speech', inlineTags))[0]

   for a in speech['extraAttributes']:
     name = a['name']
     print(name, file=sys.stderr)
     path = re.sub("'", '"',  a['valuePath'])
     path = re.sub("^[.]", "ancestor::speech", path)
     metas.append(f'''<meta group="speech" type="text" name="{name}"><xsl:attribute name="value"><xsl:value-of select='{path}'/></xsl:attribute></meta>''')


newline='\n\t\t'
allmeta = newline.join(metas)
sentmeta = newline.join(sentence_metas)

template = f'''<xsl:stylesheet
  version="3.0"
  xmlns:folia="http://ilk.uvt.nl/folia"
  xpath-default-namespace="http://ilk.uvt.nl/folia"
  xmlns:meta="http://gcnd.ivdnt.org/metadata"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- stylesheet om de alpino-zinnetjes met metadata uit de GCND folia bestanden te halen -->

<xsl:variable name="pid"><xsl:value-of select="//meta:gcnd_transcriptie_metadata/@xml:id"/></xsl:variable>
<xsl:template match="/">
<document xml:id="{{$pid}}">
  <xsl:apply-templates select=".//speech"/>
</document>
</xsl:template>

<xsl:template match="speech">

  <xsl:variable name="metadata" as="node()*">{newline}{allmeta}</xsl:variable>
    
  <xsl:for-each select=".//*[local-name()='alpino_ds']">
   <alpino_ds>
     <xsl:for-each select="@*|node()[not (name()='alud')]"><xsl:copy-of select="."/></xsl:for-each>
     <metadata>
        {sentmeta}
        <xsl:copy-of select="$metadata"/>
     </metadata>
   </alpino_ds>
  </xsl:for-each>
</xsl:template>

</xsl:stylesheet>
'''

print(template)
