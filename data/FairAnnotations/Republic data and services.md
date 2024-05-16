In the Republic project (Resolutions of the Dutch States General from the 17th and 18th century) we process texts using the following workflow:

- Retrieve text **line-by-line** from the PageXML format that is the output of our Handwritten Text Retrieval software and store this per Volume (roughly 'yearbook') as a **list of strings**
- Represent all physical structure from the PageXML as web annotations referring to the lines by using list indexes of the line strings as coordinates, optionally in combination with character offsets within these line strings. Next to these text targets Republic web annotations also have standard IIIF image targets.
- Analyse the text automatically and add derived structure and entities as *semantic* web annotations, referring to the same line based coordinates. Again, these semantic web annotations also contain image targets. Example: https://annorepo.republic-caf.diginfra.org/w3c/republic-2024.01.19/3bf40e38-b8eb-40fa-9d59-5c4b58aa8063. 
- Concatenate text lines and 'fix' hyphenated words, thereby altering the text. The result is a list of paragraph strings.
- We hereby introduced an alternative way to refer to text: refer to paragraphs using a paragraph list index plus optional character offsets.
- For entity recognition (and other NLP) we need a tokenised version of the (paragraph) text. This introduces yet another way to refer to the text: token index (plus local character offset).
- We explicitly keep track of mappings between these three alternative coordinate systems for (variations of) some text.
#### Text reference examples
URL for the text target of a semantic web annotation, returning the text of a specific Republic resolution. This specific resolution is retrieved from text on two consecutive scans.

- [https://textrepo.republic-caf.diginfra.org/api/view/versions/51d8c3b2-100f-4bbe-8c43-9b0fc2ef83d3/segments/index/45880/45907](https://textrepo.republic-caf.diginfra.org/api/view/versions/51d8c3b2-100f-4bbe-8c43-9b0fc2ef83d3/segments/index/45880/45907) (line based coordinates)

Text of the same resolution, after concatenation of text lines, based on paragraph counts plus character offsets. In this case a list of just one paragraph.

- [https://textrepo.republic-caf.diginfra.org/api/view/versions/008c350c-cb53-4d89-9fc9-6f00a131643a/segments/index/2041/0/2041/1103](https://textrepo.republic-caf.diginfra.org/api/view/versions/008c350c-cb53-4d89-9fc9-6f00a131643a/segments/index/2041/0/2041/1103) (paragraph based)

URL of the text of a 'named entity annotation', in paragraph text.
- [https://textrepo.republic-caf.diginfra.org/api/view/versions/008c350c-cb53-4d89-9fc9-6f00a131643a/segments/index/2041/111/2041/120](https://textrepo.republic-caf.diginfra.org/api/view/versions/008c350c-cb53-4d89-9fc9-6f00a131643a/segments/index/2041/111/2041/120)

The same named entity text, but now in the original line based text plus coordinates.
- [https://textrepo.republic-caf.diginfra.org/api/view/versions/51d8c3b2-100f-4bbe-8c43-9b0fc2ef83d3/segments/index/45882/36/45883/7](https://textrepo.republic-caf.diginfra.org/api/view/versions/51d8c3b2-100f-4bbe-8c43-9b0fc2ef83d3/segments/index/45882/36/45883/7)



