import sys
import re
import urllib.parse

f=open(sys.argv[1])

lines = f.readlines()
txt = ""
for l in lines:
  if 'Elliptische constructies, onderbroken zinnen' in l:
     break
  txt = txt + l

link="https://gretel5.ato.ivdnt.org/xpath-search?currentStep=2&xpath=_XPATH_&selectedTreebanks=%7B%22gretel%22:%7B%22gcnd_24-09-2024%22:%5B%22main%22%5D%7D%7D&retrieveContext=0"

def f(match):
    matched_text = match.group(0)
    # Process the matched string (e.g., convert to uppercase)
    query = re.sub("```(xpath)?", "",  matched_text)
    encoded = urllib.parse.quote(query)
    gretellink = re.sub("_XPATH_", encoded, link) 
    return matched_text + "\n" + f"[<img src='img.png' width='20pt'/>]({gretellink})"

result = re.sub("```xpath.*?```", f, txt, flags=re.DOTALL) 
result = re.sub(r"!\[img_([0-9]+).png\]\(img_[0-9]+.png\)", r"<img src='img_\1.png' width='40%'/>\n", result, flags=re.DOTALL)
print(result)
#print(result)
