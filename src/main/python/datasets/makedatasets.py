import json
import os
import sys
import re

t = open("template.json", "r")
template=json.load(t)
template['tagset'] = 'TDN-Core'
template["version"] = '0.9'
template["columns"] = ["token", "pos", "lemma", "group_id"]

f = open('lijstje.tsv','r')
scalamapfile = open('rename.scala','w')
scalamap = 'val rename: Map[String,String] =('

lines = f.readlines()
datasets = []

def clone(x):
  return json.loads(json.dumps(x))

for l in lines:
  cols = re.split('\t', l.strip())
  #print(cols)
  [name, desc, code, f, t, url] = cols
  instance = clone(template)
  instance['trainingPath'] = f"training-data/{name}"
  instance['eraFrom'] = f
  instance['eraTo'] = t
  instance['name'] = name
  instance['sourceName'] = name
  instance['sourceURL'] = url # "http://github.com/INL/galahad-corpus-data"
  instance['sourcePath'] = f'source-data/{name}'
  instance['description'] = desc
  scalamap = scalamap + f'"{code}" -> "{instance["name"]}",\n'
  datasets.append(instance)

  fout=open(f"sets/{name}.json", "w")
  json.dump(instance,fout,indent=4)
  fout.close()

print(json.dumps(datasets,indent=4))
scalamap = scalamap + ")"
print(scalamap,file=scalamapfile)
scalamapfile.close()
