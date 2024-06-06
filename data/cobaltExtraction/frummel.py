import json

old=json.load(open("cobaltSets.json","r"))
nw= json.load(open("cobaltSets.met14.json","r"))
old["trainingDataInfos"]["gtbcit_14_fromscratch"] = nw["trainingDataInfos"]["dictionary-quotations-14"]
out=open("mix.json","w")
json.dump(old,out,indent=4)

