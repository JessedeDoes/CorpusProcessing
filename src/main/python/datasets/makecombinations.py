import json
import os
import sys
import re

t = open("template.json", "r")
template={'name': None, 'datasets': []}
template['tagset'] = 'TDN-Core'
template["version"] = '1.0'

f = open('combinaties.tsv','r')

lines = f.readlines()
datasets = []

def clone(x):
  return json.loads(json.dumps(x))

for l in lines:
  cols = re.split('\t', l.strip())
  combination = list(re.split("\s*,\s*", cols[1]))
  name = cols[0]
  frm = cols[2]
  to = cols[3]
  #print(cols)
  instance = clone(template)

  instance['name'] = name
  instance['datasets'] = combination
  instance['eraFrom'] = frm
  instance['eraTo'] = to
  datasets.append(instance)

  fout=open(f"combinations/{name}.combination.json", "w")
  json.dump(instance,fout,indent=4)
  print('',file=fout)
  fout.close()

print(json.dumps(datasets,indent=4))
