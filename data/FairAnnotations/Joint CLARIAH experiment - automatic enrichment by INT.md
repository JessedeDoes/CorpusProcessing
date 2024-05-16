CLARIAH FAIR Annotations promotes the concept of **Shared Annotation Spaces**. The idea is that (text) collection publishers expose the text coordinates that they use to publish their own text annotations to the outside world. These same coordinates can be shared by everybody to add their own sets of annotations. If desired, these sets effectively can be considered a uniform part of the enriched text collection.

CLARIAH FAIR Annotations mainly is concerned with manual annotation scenarios, but we see no fundamental reason why automated scenarios should not be supported as well. A typical way to automatically enrich text collections is by applying NLP tooling.

We propose to experiment with a more or less realistic automatic annotation scenario, with the following objectives:

- test our assumption that automatic scenarios are indeed realistic and within scaling limits
- test if we can really 'share an annotation space' with an external partner organisation.
- find out what is additionally required for already existing automatic annotation client software. Does it need adaptations?
- experiment with different *text referencing (coordinate) granularities*. In case of NLP tools we specifically want to test *token* granularity.
- Add a potentially valuable enrichment to an existing text collection (secondary objective, nice to have).
- Evaluate in how far this scenario is a model for new online ways to collaborate between partner institutions or research groups.
### Context - the Republic project
Specifically, we want to test a scenario in the following context: [[Republic data and services]]
### The experiment
For steps of our experiment, see [[Automatic annotation experiment]]
### Results and deliverables
We could have the following results:

- Annotation container with web annotations, generated with INT tooling (and possibly HuC postprocessing)
	- delivered as jsonld data set or directly uploaded in a AnnoRepo instance at HuC
	- owned by/created by INT
	- recognisable as such (provenance, branding, intellectual ownership)
- Additional coordinates at token level
	- One way to represent this token information is as a set of web annotations on the paragraph text, one for each token. But we may come up with better and more compact representations.
- Visualisation of INT annotations in Republic TextAnnoViz context
	- HuC additionally indexes information from this annotation container next to the already existing Republic annotations, and provides configuration info for its front-end.
- Sample queries including INT annotations?
	- Both via GUI and API
- Short report with *lessons learned*.

