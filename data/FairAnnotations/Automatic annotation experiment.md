
1. Select a subset of Republic resolutions, in the form of a set of web annotations.
	1. For example, a range of years, or the result of a search operation.
	2. Question: export of search results as web annotations needed? Useful?
2. For these resolutions, retrieve the UTF-8 text using the current implementation of the Republic text referencing API (see: [[Republic data and services]]). Keep track of the *coordinates* that are used for retrieval.
	1. alternative 1: retrieve paragraphs as a list of UTF-8 strings. Tokenising is considered part of the automatic linguistic enrichment with INT historic language tools. Challenge is then to transfer these tokenisation-based coordinates back to the collection provider, as part of the enrichment data.
	2. alternative 2: retrieve tokens from HuC services. Tokenising is then considered the responsibility of the collection provider (HuC).
3. Apply NLP tools, for example, add lemmas and part-of-speech tagging.
4. Convert the NLP annotations to valid web annotations, using the original HuC *text coordinates*, or token-based coordinates with mapping to the original HuC coordinates.
	1. Remark: scale may be an issue for large numbers of web annotations
5. Deliver these annotations as a separate Web Annotation container, preferably directly using the batch upload API of the Republic AnnoRepo service instance.
6. Visualise new annotations in the Republic version of HuCs generic TextAnnoViz frontend.