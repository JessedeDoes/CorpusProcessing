undef $/;

my $inHeader = 1;
binmode(stdin,":encoding(utf8)");
binmode(stdout,":encoding(utf8)");

while(<>)
{
  my %meta;
  my %delenda;
  my $content = $_;

  $content =~ s/<interpGrp.*?<\/interpGrp>//gs;

  s/.*<\/teiHeader>//s;

  my $txt = $_;

  $txt =~ s/<.*?>//gs;
  my $wc = 0;
  while ($txt =~ /\S+/g)
  {
    $wc++;
  }

  $meta{'wc'} = $wc;

  my @possibleDates;

  while (/1[5-7][0-9]{2}/g)
  {
    push(@possibleDates,$&);
  }

  $meta{'possibleDates'} = join(",",@possibleDates);

  my $lastP;
  my $lastX;

  while (/<p.*?>(.*?)<\/p>/gs)
  {
    my $x = $&;

    my $p = $1;
    my $pOrg = $p;

    $p =~ s/\s+/ /g;
    $p =~ s/<lb\/>/#lb#/g;

    $p =~ s/<[^<>]*?>//gs;

    if ($p =~ /(.*?)\s*:\s*(.*)/s)
    {
      if ($inHeader)
      {
        my $key = $1;
        my $value = $2; 
        if (length($key) > 20)
        {
          warn "Too long in $key -> $value";
          $inHeader = 0;
        } else
        {
          $delenda{$x} = $key;
          warn "delendum: $x";
          foreach my $line (split(/#lb#/,$p))
          {
            if ($line =~ /(.*?)\s*:\s*(.*)/s)
            {
              my $key = $1;
              my $value = $2;  
              $key = trim($key);
              $value = trim($value);
              warn "$key --> $value\n";
              $meta{$key} = $value;
            }
          }
        }
      }
    } else
    {
      $inHeader = 0;
    }
    $lastP = $p;
    $lastX = $x;
  }

  warn "last P: $lastP";

  if ($lastP =~ /^\s*bron\s*:/is)
  {
    $meta{'bron'} = $lastX;
    $delenda{$lastX} = 'bron' ;
  }

  $content =~ s/<p( .*?)?>(.*?)<\/p>/deleteMeta(\%delenda,$&)/ges;

  my $xml = makeFields(\%meta);
  $content =~ s/<bibl>/<bibl>\n$xml\n/;
  print $content;
}

sub makeFields
{
  my $m = shift;
  my $xml="";
  my $N=0;
  foreach my $k (sort keys %$m)
  {
    my $v = $$m{$k};
    $xml .= "<interpGrp type='$k'><interp>$v</interp></interpGrp>\n";
    $N++;
  }
  if ($N < 4)
  {
    warn "only $N fields in $xml";
  }
  return $xml;
}

sub trim
{
  my $x = shift;
  $x =~ s/\s+/ /g;
  $x =~ s/^\s+//;
  $x =~ s/\s+$//;
# $x =~ s/[^a-z0-9() ]//g;
  return lc $x;
}

sub deleteMeta
{
  my ($h,$p) = @_;
  if ($p =~ />p\s*[0-9]+</)
  {
    my $pplain = $p;
    $pplain =~ s/<.*?>//g;
    $pplain = trim($pplain);
    if ($pplain =~ /^p\s*([0-9]+)\s*$/)
    {
      return "<pb n=\"$1\"/>";
    } 
  }
  if ($$h{$p}) 
  {
    return "<!-- deleted field $$h{$p}, full: $p -->"  ;
  } else
  {
    if ($p =~ /referentie/)
    {
#      warn "Why not: $p??";
    }
    return $p;
  }
}
