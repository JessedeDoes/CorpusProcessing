{
  "@graph": [
    {
      "@id": "http://rdf.ivdnt.org/lexica/diamant/lemma/mnw/10041",
      "": ":NOUN",
      "canonicalForm": "http://rdf.ivdnt.org/lexica/diamant/lemma/mnw/10041f",
      "lexicalForm": [
        "http://rdf.ivdnt.org/lexica/diamant/wordform/hilex/810930",
        "http://rdf.ivdnt.org/lexica/diamant/wordform/hilex/795129",
        "http://rdf.ivdnt.org/lexica/diamant/wordform/hilex/747123",
        "http://rdf.ivdnt.org/lexica/diamant/wordform/hilex/711530",
        "http://rdf.ivdnt.org/lexica/diamant/wordform/hilex/683310"
      ],
      "sense": [
        "http://rdf.ivdnt.org/lexica/diamant/sense/10041.sense.1",
        "http://rdf.ivdnt.org/lexica/diamant/sense/10041.sense.2"
      ]
    },
    {
      "@id": "http://rdf.ivdnt.org/lexica/diamant/lemma/mnw/10041f",
      "@type": "http://www.w3.org/ns/lemon/ontolex#Form",
      "writtenRep": "gaardenaar"
    },
    {
      "@id": "http://rdf.ivdnt.org/lexica/diamant/sense/10041.sense.1",
      "@type": "http://www.w3.org/ns/lemon/ontolex#LexicalSense",
      "definition": [
        "http://rdf.ivdnt.org/lexica/diamant/synonymdefinition//22700",
        "http://rdf.ivdnt.org/lexica/diamant/synonymdefinition//22699",
        "http://rdf.ivdnt.org/lexica/diamant/definition/10041.sense.1"
      ],
      "subsense": "http://rdf.ivdnt.org/lexica/diamant/sense/10041.sense.2",
      "attestation": [
        "http://rdf.ivdnt.org/lexica/diamant/attestation/1689386",
        "http://rdf.ivdnt.org/lexica/diamant/attestation/2004518",
        "http://rdf.ivdnt.org/lexica/diamant/attestation/1785484",
        "http://rdf.ivdnt.org/lexica/diamant/attestation/1872287",
        "http://rdf.ivdnt.org/lexica/diamant/attestation/1851657",
        "http://rdf.ivdnt.org/lexica/diamant/attestation/1812103"
      ],
      "id": "10041.sense.1",
      "synonymDefinition": [
        "tuinman",
        "Hovenier"
      ],
      "label": "Hovenier, tuinman; als familienaam is nog heden Gerdenier bekend."
    },
    {
      "@id": "http://rdf.ivdnt.org/lexica/diamant/sense/10041.sense.2",
      "@type": "http://www.w3.org/ns/lemon/ontolex#LexicalSense",
      "definition": "http://rdf.ivdnt.org/lexica/diamant/definition/10041.sense.2",
      "id": "10041.sense.2",
      "label": "Vgl. gaerdemeester, bogaertman, en zie nog een voorbeeld op hofman."
    },
    {
      "@id": "http://rdf.ivdnt.org/lexica/diamant/wordform/hilex/683310",
      "@type": "http://www.w3.org/ns/lemon/ontolex#Form",
      "attestation": "http://rdf.ivdnt.org/lexica/diamant/attestation/2004518",
      "id": "hilex/683310",
      "part_of_speech": "NOU",
      "writtenRep": "gardeniers"
    },
    {
      "@id": "http://rdf.ivdnt.org/lexica/diamant/wordform/hilex/711530",
      "@type": "http://www.w3.org/ns/lemon/ontolex#Form",
      "attestation": "http://rdf.ivdnt.org/lexica/diamant/attestation/1851657",
      "id": "hilex/711530",
      "part_of_speech": "NOU",
      "writtenRep": "gairdenairs"
    },
    {
      "@id": "http://rdf.ivdnt.org/lexica/diamant/wordform/hilex/747123",
      "@type": "http://www.w3.org/ns/lemon/ontolex#Form",
      "attestation": "http://rdf.ivdnt.org/lexica/diamant/attestation/1812103",
      "id": "hilex/747123",
      "part_of_speech": "NOU",
      "writtenRep": "gardenaer"
    },
    {
      "@id": "http://rdf.ivdnt.org/lexica/diamant/wordform/hilex/795129",
      "@type": "http://www.w3.org/ns/lemon/ontolex#Form",
      "attestation": [
        "http://rdf.ivdnt.org/lexica/diamant/attestation/1785484",
        "http://rdf.ivdnt.org/lexica/diamant/attestation/1872287"
      ],
      "id": "hilex/795129",
      "part_of_speech": "NOU",
      "writtenRep": "gaerdenaer"
    },
    {
      "@id": "http://rdf.ivdnt.org/lexica/diamant/wordform/hilex/810930",
      "@type": "http://www.w3.org/ns/lemon/ontolex#Form",
      "attestation": "http://rdf.ivdnt.org/lexica/diamant/attestation/1689386",
      "id": "hilex/810930",
      "part_of_speech": "NOU",
      "writtenRep": "gaerdenere"
    },
    {
      "@id": "http://server/unset-base/aap",
      "noot": "http://server/unset-base/mies"
    },
    {
      "@id": ":NOUN",
      "@type": "",
      "label": [
        "NOUN",
        "pos=NOUN"
      ]
    }
  ],
  "@context": {
    "definition": {
      "@id": "http://lemon-model.net/lemon#definition",
      "@type": "@id"
    },
    "id": {
      "@id": "http://rdf.ivdnt.org/schema/diamant#id"
    },
    "label": {
      "@id": "http://www.w3.org/2000/01/rdf-schema#label"
    },
    "attestation": {
      "@id": "http://rdf.ivdnt.org/schema/diamant#attestation",
      "@type": "@id"
    },
    "part_of_speech": {
      "@id": "http://wordnet-rdf.princeton.edu/ontology#part_of_speech"
    },
    "writtenRep": {
      "@id": "http://www.w3.org/ns/lemon/ontolex#writtenRep"
    },
    "synonymDefinition": {
      "@id": "http://rdf.ivdnt.org/schema/diamant#synonymDefinition"
    },
    "subsense": {
      "@id": "http://lemon-model.net/lemon#subsense",
      "@type": "@id"
    },
    "sense": {
      "@id": "http://www.w3.org/ns/lemon/ontolex#sense",
      "@type": "@id"
    },
    "lexicalForm": {
      "@id": "http://www.w3.org/ns/lemon/ontolex#lexicalForm",
      "@type": "@id"
    },
    "": {
      "@id": "http://universaldependencies.org/u/pos/",
      "@type": "@id"
    },
    "canonicalForm": {
      "@id": "http://www.w3.org/ns/lemon/ontolex#canonicalForm",
      "@type": "@id"
    },
    "noot": {
      "@id": "http://server/unset-base/noot",
      "@type": "@id"
    }
  }
}
