package folia

object entityReplacement
{
val entityMap = Map(
  "amp" -> "&",
"aacute" -> "\u00E1",
"Aacute" -> "\u00C1",
"acirc" -> "\u00E2",
"Acirc" -> "\u00C2",
"agrave" -> "\u00E0",
"Agrave" -> "\u00C0",
"aring" -> "\u00E5",
"Aring" -> "\u00C5",
"atilde" -> "\u00E3",
"Atilde" -> "\u00C3",
"auml" -> "\u00E4",
"Auml" -> "\u00C4",
"aelig" -> "\u00E6",
"AElig" -> "\u00C6",
"ccedil" -> "\u00E7",
"Ccedil" -> "\u00C7",
"eth" -> "\u00F0",
"ETH" -> "\u00D0",
"eacute" -> "\u00E9",
"Eacute" -> "\u00C9",
"ecirc" -> "\u00EA",
"Ecirc" -> "\u00CA",
"egrave" -> "\u00E8",
"Egrave" -> "\u00C8",
"euml" -> "\u00EB",
"Euml" -> "\u00CB",
"iacute" -> "\u00ED",
"Iacute" -> "\u00CD",
"icirc" -> "\u00EE",
"Icirc" -> "\u00CE",
"igrave" -> "\u00EC",
"Igrave" -> "\u00CC",
"iuml" -> "\u00EF",
"Iuml" -> "\u00CF",
"ntilde" -> "\u00F1",
"Ntilde" -> "\u00D1",
"oacute" -> "\u00F3",
"Oacute" -> "\u00D3",
"ocirc" -> "\u00F4",
"Ocirc" -> "\u00D4",
"ograve" -> "\u00F2",
"Ograve" -> "\u00D2",
"oslash" -> "\u00F8",
"Oslash" -> "\u00D8",
"otilde" -> "\u00F5",
"Otilde" -> "\u00D5",
"ouml" -> "\u00F6",
"Ouml" -> "\u00D6",
"szlig" -> "\u00DF",
"thorn" -> "\u00FE",
"THORN" -> "\u00DE",
"uacute" -> "\u00FA",
"Uacute" -> "\u00DA",
"ucirc" -> "\u00FB",
"Ucirc" -> "\u00DB",
"ugrave" -> "\u00F9",
"Ugrave" -> "\u00D9",
"uuml" -> "\u00FC",
"Uuml" -> "\u00DC",
"yacute" -> "\u00FD",
"Yacute" -> "\u00DD",
"yuml" -> "\u00FF",
);


import scala.util.matching.Regex

val x = "&([A-za-z]+);".r

def replace(s: String):String = x.replaceAllIn(s, m =>  entityMap.getOrElse(m.group(1),m.group(0)))
}


