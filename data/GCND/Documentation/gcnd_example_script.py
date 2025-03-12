#!/usr/bin/env python3
"""
Example script demonstrating how to parse a GCND FoLiA file using lxml,
focusing on:
1) Retrieving recording-level metadata (audio filename, etc.)
2) Iterating over <speech> elements to get speaker info, timestamps
3) Extracting tokens' light/heavy transcriptions, lemma, PoS

Usage:
    python parse_gcnd.py path/to/gcnd_xxx.folia.xml
"""

import sys
from lxml import etree

# Namespaces used in the GCND FoLiA files:
NS = {
    'folia': 'http://ilk.uvt.nl/folia',        # FoLiA default namespace
    'gcnd': 'http://gcnd.ivdnt.org/metadata',  # GCND metadata namespace
    'meta': 'http://gcnd.ivdnt.org/metadata',  # GCND metadata namespace
    'xml': 'http://www.w3.org/XML/1998/namespace'
}

def txt(x):
  if x:
     return x[0]
  else: 
    return 'None'

def main(xml_file):
    # Parse the XML with lxml
    doc = etree.parse(xml_file)

    # 1) EXTRACT RECORDING-LEVEL METADATA
    # Example: get the main audio file name from the <opname> element
    # (within <gcnd_transcriptie_metadata> block)
    opname_name = doc.xpath('//gcnd:opname/gcnd:bestand/gcnd:naam/text()', namespaces=NS)
    if opname_name:
        print(f"\nAudio file name: {opname_name[0]}")
    else:
        print("\nAudio file name not found in metadata")

    kloeke_code = doc.xpath('//gcnd:plaats[@rel="opname⟶plaats"]//gcnd:kloeke_code/text()',  namespaces=NS)
    verification_status = doc.xpath('//gcnd:transcriptie_status/gcnd:label/text()',  namespaces=NS)

    print(f"\nKloekecode: {txt(kloeke_code)}")
    print(f"\nVerification status: {txt(verification_status)}")
    # 2) FIND ALL <speech> ELEMENTS
    speeches = doc.xpath('//folia:speech', namespaces=NS)
    print(f"\nFound {len(speeches)} <speech> segments.\n")

    # Iterate over each <speech> to extract relevant info
    for i, sp in enumerate(speeches, start=1):
        # The 'speaker' attribute identifies who is speaking
        speaker_id = sp.get('speaker')
        role = sp.get('tag')  # e.g. "spreker" or "interviewer"
        begintime = sp.get('begintime')
        endtime = sp.get('endtime')

        # 2A) GET THE HEAVILY NORMALIZED TEXT FOR THE ENTIRE UTTERANCE
        heavy_t_elem = sp.find('folia:t[@class="heavyNormalization"]', namespaces=NS)
        heavy_utt_text = heavy_t_elem.text if heavy_t_elem is not None else "(no heavy utterance text)"

        print(f"--- Speech #{i} ---")
        print(f"  Speaker ID: {speaker_id} (role: {role})")
        print(f"  Time Span:  {begintime} - {endtime}")
        print(f"  Full normalized utterance: {heavy_utt_text}")

        # 2B) EXTRACT TOKENS (<w> ELEMENTS) AND THEIR ANNOTATIONS
        w_tokens = sp.xpath('.//folia:w', namespaces=NS)
        if not w_tokens:
            print("  (No tokens found inside this speech segment)")
            continue

        for w in w_tokens:
            # (a) Dialect (light) and Heavy forms
            light_t = w.find('folia:t[@class="lightNormalization"]', namespaces=NS)
            heavy_t = w.find('folia:t[@class="heavyNormalization"]', namespaces=NS)
            dialect_form = light_t.text if light_t is not None else "(no light form)"
            heavy_form = heavy_t.text if heavy_t is not None else "(no heavy form)"

            # (b) Lemma
            lemma_elem = w.find('folia:lemma', namespaces=NS)
            lemma = lemma_elem.get('class') if lemma_elem is not None else "(no lemma)"

            # (c) PoS (coarse & full tag)
            pos_elem = w.find('folia:pos', namespaces=NS)
            if pos_elem is not None:
                pos_coarse = pos_elem.get('head') or "(no coarse POS)"
                pos_full = pos_elem.get('class') or "(no full POS)"
            else:
                pos_coarse = "(no pos)"
                pos_full = "(no pos)"

            print(f"     Token ID: {w.get('{http://www.w3.org/XML/1998/namespace}id')}")
            print(f"       Dialect (light): '{dialect_form}'")
            print(f"       Normalized (heavy): '{heavy_form}'")
            print(f"       Lemma: {lemma}")
            print(f"       PoS: {pos_coarse} [{pos_full}]\n")

    print("\nDone.\n")


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: python parse_gcnd.py path/to/gcnd_xxx.folia.xml")
        sys.exit(1)

    xml_filepath = sys.argv[1]
    main(xml_filepath)

