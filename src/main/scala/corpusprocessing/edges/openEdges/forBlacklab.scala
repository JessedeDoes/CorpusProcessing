package corpusprocessing.edges.openEdges

object forBlacklab {
  def stukje(version: String) = {
    """
# Version $version
  # --------------------------------
  contents__$version:
    containerPath: TEI/text[matches(@xml:id, '^nl_1477_Delftse')]
    wordPath: .//w
    tokenIdPath: "@xml:id" # remember id so we can refer to it in standoff annotations
    punctPath: .//text()[not(ancestor::w)]   # = "all text nodes (under containerPath) not inside a <w/> element"

    annotations:
    - name: word
      displayName: Word
      valuePath: .
      sensitivity: sensitive_insensitive

    standoffAnnotations:
    - path: /teiCorpus//link[matches(@target, '^#$version')]
      type: relation
      relationClass: al
      targetVersionPath: "replace(./@target, '^.+ #(\\w+_\\d+).+$', '$1')"
      valuePath: "@type"   # relation type
      sourcePath: "replace(./@target, '^#(.+) .+$', '$1')"
      targetPath: "replace(./@target, '^.+ #(.+)$', '$1')"


    - path: /teiCorpus//link[matches(@target, '.* #$version.*')]
      type: relation
      relationClass: al
      targetVersionPath: "replace(./@target, '^#(\\w+_\\d+).+$', '$1')"
      valuePath: "@type"   # relation type
      targetPath: "replace(./@target, '^#(.+) .+$', '$1')"
      sourcePath: "replace(./@target, '^.+ #(.+)$', '$1')"

    inlineTags:
    - path: .//div # book/chapter
    - path: .//ab  # verse
      tokenIdPath: "@xml:id" # remember id so we can refer to it in standoff annotations

      # index some attributes by XPath
      #includeAttributes: []  # don't index the tag's attributes
      extraAttributes:
      - name: chapter
        valuePath: "replace(./@n, '^\\w+\\.(\\d+)\\.(\\d+)$', '$1')"
      - name: verse
        valuePath: "replace(./@n, '^\\w+\\.(\\d+)\\.(\\d+)$', '$2')"
      - name: chapter-verse
        valuePath: "replace(./@n, '^\\w+\\.(\\d+)\\.(\\d+)$', '$1:$2')"

    - path: .//s   # sentence

    """.replaceAll("\\$version", version)
  }
    def main(args: Array[String]) = Metadata.metadataForPid.keySet.foreach(k => {
      println(stukje(k))
    })

}
