$txt="";
$toc = "";
while(<>) 
{
  $txt .= $_;
  while (/<h([2-9]) id="(.*?)">(.*?)<\/h[2-9]>/g)
  {
    $level = $1;
    $id = $2;
    $kopje = $3;
    warn("$level $id $kopje\n");
    $before = "&nbsp;&nbsp;" x ($level - 2);
    $toc .= "<div style='margin-top:0pt; margin-bottom:0pt'>$before<a href='#$id'>$kopje</a></div>\n";
  }
}

print <<END;
<div>
<h1>Zoeken naar dialectconstructies in het GCND-corpus met behulp van XPath</h1>
<h2>Inhoudsopgave</h2>
$toc
</div>
$txt
END
