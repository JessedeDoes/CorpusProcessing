#my $INPUT_DIRECTORY = "Export30082013";
#$INPUT_DIRECTORY = "Export16_06_2014";
#my $OUTPUT_DIRECTORY="BlacklabInput16_06_2014";

my ($INPUT_DIRECTORY,$OUTPUT_DIRECTORY) = @ARGV;

my @relevantFields = split(/\s+/,"pid
			datum_jaar
                        type_brief
                        autograaf
                        signatuur
                        adr_naam adr_naam_norm
                        adr_loc_plaats_norm
                        adr_loc_land_norm
                        adr_loc_regio_norm
                        adr_loc_schip_norm
                        afz_naam afz_naam_norm
                        afz_geslacht
                        afz_klasse
                        afz_geb_lftcat
                        regiocode
                        datum_maand datum_dag
                        afz_rel_tot_adr
                        afz_loc_plaats_norm
                        afz_loc_land_norm
                        afz_loc_regio_norm
                        afz_loc_schip_norm");
my %isRelevant;
foreach my $f (@relevantFields) { $isRelevant{$f}++ };

my @maanden = qw /unknown januari februari maart april mei juni juli augustus september oktober november december/; 
my $text2image = "Metadata/text_image.txt";
$text2image = "Metadata/text_image_met_knipsels.manual.txt";

my %patch;
open(t2i,$text2image) || die $text2image;
my $txt;
while(<t2i>)
{
  s/\s*#.*//;
  s/^\s*//;
  s/\s*$//;
  chomp();
  next if (/^\s*$/);
  if (/xml/)
  {
    $txt = $_;
    $txt =~ s/.*\///;
  } else
  {
    my $prev = $text2img{$txt};
    if ($prev)
    {
      $text2img{$txt} .= ",";
    }
    $text2img{$txt} .= $_;
  }
}

open(PATCH, "Metadata/patch.txt");
while(<PATCH>)
{
  chomp();
  if (/xml/)
  {
    $txt = $_;
    $txt =~ s/.*\///;
    $txt =~ s/.exported.xml$//;

  } else
  {
     s/^\s+//;
     s/\s+$//;
     my ($property,$value) = split(/\t/,$_);
     my $key = stripKey($txt);
     warn "$txt: $property->$value";

     $patch{$txt}{$property} = $value;
     $patch{$key}{$property} = $value;
  }
}
#while(<t2i>)
#{
#  chomp();
#  my ($text,$images) =  split(/\t/,$_);
#  $text =~ s/.*\///;
#  $text2img{$text} = $images;
#}

sub readTabSeparated
{
  my ($r,$file) = @_;
  open(f,$file) || die $file;
  my $k=0;
  my @fieldNames;
  while(<f>)
  {
    chomp();
    my @fields = split(/\t/,$_);
    if (!$k)
    {
      for (my $i=0; $i < @fields; $i++) { $fields[$i] =~ s/[^a-zA-Z0-9]+/_/g; };
      @fieldNames = @fields;
    }  else
    {
      my $h = ();
      for (my $i=0; $i < @fields; $i++) 
      { 
         $fields[$i] =~ s/& /&amp; /g;
         $fields[$i] =~ s/"/&quot;/g;
         $fields[$i] =~ s/\r/\n/g;
         if ($fields[$i] eq "Curacao") { $fields[$i] ="Curaçao"; };
         $$h{$fieldNames[$i]} = replaceValue($fieldNames[$i], $fields[$i]);
      }
      my $key = $$h{'trans_bestand'};
      # warn "ID: $key";
      $$r{$key} = $h;
      $$r{stripKey($key)} = $h;
      $$h{'pid'} = sprintf("bab%04d", $$h{'code'});
      $$h{'afz_rel_tot_adr'} = $$h{'relatie_a'};

#      if ($$h{'relatie_b'} ne "NULL") { $$h{'afz_rel_tot_adr'} .= "; " . $$h{'relatie_b'} };

      if (!$$h{'afz_naam'}) { $$h{'afz_naam'} = "onbekend" };
      if (!$$h{'adr_naam'}) { $$h{'adr_naam'} = "onbekend" };
      # keep only important fields; 
      undef $$h{'adr_bijzonderheden'};
      undef $$h{'afz_bijzonderheden'};
      undef $$h{'bijzonderheden'};
  
      my $p = $patch{$key};
      if (!$p)
      {
        $p = $patch{stripKey($key)};
        warn "YEP YEP YEP!" if $p;
      }
      foreach my $x (sort keys %$h)
      {
        # if (!$isRelevant{$x})  { delete $$h{$x}; } ;
      }
      foreach my $x (sort keys %$p) ## apply patch
      {
        warn "PATCHING field $x to $$p{$x} in file $$h{'code'} = $$h{'trans_bestand'}\n";
         $$h{$x} = $$p{$x};
      }
    }
    $k++; 
  }
#  die join("\n", sort keys %$r);
}

my %h = ();

#readTabSeparated(\%h,"Metadata/17.tab");
#readTabSeparated(\%h,"Metadata/18.tab");
readTabSeparated(\%h,"Metadata/brief.tab");
## warn join("\n", sort keys %h);

opendir(D,$INPUT_DIRECTORY);

while ($x = readdir(D))
{
  next if !(-f "$INPUT_DIRECTORY/$x");
  my $images = $text2img{$x};
  my $z ="";
  foreach my $i (split(/\s*,\s*/,$images))
  {
    $i =~ s/.*\/INL-/INL-/;
    $z .= "<interp value=\"$i\"/>";
  }
#  warn "IMAGES for $x: $z";
  $z = "<interpGrp type=\"images\">$z</interpGrp>";
  my $bestand = $x; 


  $bestand =~ s/.exported.xml$//;
  my $key = stripKey($bestand);
  my $m = $h{$bestand};
  
  if (!$m)
  {
    $m = $h{$key};
  }
  if (!$m)
  {
    warn "METADATA NOT FOUND: $bestand ($key)";
  } else
  {
#    warn "$bestand ($key) --> $$m{datering} ($$m{afz_naam} $$m{afz_rel_tot_geadr_}) aan $$m{adr_naam}\n";
    undef $/;
    open(f,"$INPUT_DIRECTORY/$x");
    open(OUT,">$OUTPUT_DIRECTORY/$x"); ## encoding????
    while(<f>)
    {
      s/(<w[^<>]* )type=/$1xtype=/g;
      s|lemma="([^\s>]*)"|my $x = $1; $x =~ s/"/&quot;/g; warn $x if ($x =~ /quot/) ; "lemma=\"$x\""|eg;
      s/ctag=/type=/g;
      s/(<w[^<>]*type="[^<>"]*MOD[^<>]*>)([^<>]*)<\/w>/<seg type="editorial">$2<\/seg>/gs;
      s/type="(.*?)"/"type=" . fixPoS($1)/egs;
      s/lemma="(.*?)"/"lemma=" . fixLemma($1)/egs;

      my $bibl = makeBibl($m);
      $bibl =~ s/<\/bibl>/$z\n<\/bibl>/;
      s/<\/teiHeader>/<sourceDesc>$bibl<\/sourceDesc><\/teiHeader>/;  
      s/<teiHeader[^<>]*\/>/<teiHeader><sourceDesc>$bibl<\/sourceDesc><\/teiHeader>/;
      s/<!DOCTYPE.*?>//;
      print OUT $_;
    }
    close(OUT);
    close(f);
  }
}

sub stripKey
{
  my $key = shift;
  my $keyOrg = $key;
  $key =~ s/, /,/;
  $key =~ s/078-81-/078-081/;
  $key =~ s/110-11([^1]|$)/110-111$1/;
  $key =~ s/-30-32/-030-032/;
  $key =~ s/-2-028-029/-028-029/; 
  $key =~ s/\(def[0-9]\)//g;
  $key =~ s/-c-/-/;
  $key =~ s/[- ]?TR-?def.*//i;
  $key =~ s/-tr.*//i;
#  $key =~ s/\s+//g; # neen niet goed
  $key =~ s/-def$//i;
#  $key =~ s/vliet-/vliet/i;
  $key =~ s/\s+/-/g;
  if ($key =~ /226.*110/)
  {
    warn "LET OP $keyOrg --> $key";
  }
  return lc $key; 
}

sub makeBibl
{
  my $hash = shift;
  my %h = %$hash;
  my $year =   $h{'datum_jaar'};
  $year =~ s/.*([0-9]{4}).*/$1/;
  my $maand = $maanden[$h{'datum_maand'}];
##  warn "DATERING (MAAND): $maand = $h{'datum_maand'}";
  if (!$maand || !($h{'datum_maand'}) || ($h{'datum_maand'} eq "unknown"))
  {
    warn "GEEN MAAND VOOR $h{'trans_bestand'} / $h{'datum_dag'}  $year ($h{'datum_maand'})";
    $maand = "[ ]";
  }
  my $datering = $h{'datum_dag'} . " " .  $maand . " " . $h{'datum_jaar'};
  $datering =~ s/\s*unknown\s*//g;
  $datering =~ s/\s*NULL\s*//g;

  my $allFields;
  foreach my $k (sort keys %h)
  {
    my $v = $h{$k};
    $v =~ s/\\'/&apos;/g;
    $v =~ s/'/&apos;/g;
    $allFields .= "<interpGrp type='$k'><interp value='$v'/></interpGrp>\n";
  }
return<<END;
<listBibl id='inlMetadata'>
<bibl>
<interpGrp type='title'><interp value="To $h{adr_naam_norm}, $datering"/></interpGrp>
<interpGrp type='author'><interp value="$h{afz_naam_norm}"/></interpGrp>
<interpGrp type='witnessYear_from'><interp value="$year"/></interpGrp>
<interpGrp type='witnessYear_to'><interp value="$year"/></interpGrp>
$allFields
</bibl>
</listBibl>
END
}

sub replaceValue
{
  my ($name,$value) = @_;
  my %ageMap = ("0-12"=>"&lt;30", "12-20"=>"&lt;30","20-30"=>"&lt;30", 
                  "30-40"=>"30-50","40-50"=>"30-50",
                     "50-60"=>"&gt;50", "60+"=>"&gt;50");
  my %genderMap = ("man"=>"male","vrouw"=>"female","onbekend"=>"unknown");
  my %typeMap = ("prive"=>"private", zakelijk=>"business");
  my %autoMap = ("autograaf",=>"autograph", "niet-autograaf"=>"non-autograph", "onduidelijk"=>"uncertain", "NULL"=>"uncertain");
  
  my %classMap = ( "laag"=>"low", "hoog"=>"high", "midden-hoog"=>"middle-high", "midden-laag"=>"middle-low", "onbekend"=>"unknown", "NULL"=>"unknown");

  my %relMap =
  (
    "(ex)echtgenoot" => "(ex-)husband",
    "(ex)echtgenote" => "(ex-)wife",
    "anders" => "other",
    "broer / zwager" => "brother / brother in law",
    "dochter" => "daughter",
    "geliefde (m) / minnaar" => "beloved (m)",
    "geliefde (v) / minnares" => "beloved (f)",
    "kennis" => "acquaintance",
    "kleinzoon" => "grandson",
    "meerdere familieleden" => "family members",
    "moeder" => "mother",
    "neef" => "nephew/cousin",
    "nicht" => "niece/cousin",
    "oom" => "uncle",
    "tante" => "aunt",
    "vader" => "father",
    "vriend" => "friend (m)",
    "vriendin" => "friend (f)",
    "werkgever" => "employer",
    "werknemer" => "employee",
    "zakenrelatie" => "business relation",
    "zoon" => "son",
    "zus / zuster / schoonzus" => "sister / sister in law",
    "NULL"=>""
  );
  my $mapRef;
  if ($name eq "afz_geb_lftcat")
  {
    $mapRef = \%ageMap;
  }
  if ($name eq "type_brief")
  {
    $mapRef = \%typeMap;
  }
  if ($name eq "autograaf")
  {
    $mapRef = \%autoMap;
  }
  if ($name eq "afz_geslacht")
  {
    $mapRef = \%genderMap;
  }
  if ($name eq "afz_klasse")
  {
    $mapRef = \%classMap;
  }
  if ($name eq "relatie_a")
  {
    $mapRef = \%relMap;
  } 
  if ($value && $mapRef)
  {
    if (defined($$mapRef{$value}))
    {
      $value = $$mapRef{$value};
    } else
    {
      die "NOT MAPPED: $value for $name";
    }
  }
  if ($name eq "regiocode")
  {
    if ($value eq "Noord-Holland")
    {
      $value = "Noord-Holland (excluding Amsterdam)";
    }
  }
  if ($value =~ /^onbekend$/i || $value =~ /null/i || (($name eq "adr_naam_norm" || $name eq "afz_naam_norm") && $value =~ /^\s*$/))
  {
    $value="unknown";
  }
  return $value;
}

sub fixLemma
{
  my $lemma=shift;
  $lemma =~ s/(XXX|YYY)/UNRESOLVED/gi;
  return "\"$lemma\"";
}

sub fixPoS
{
  my $pos=shift;
  $pos =~ s/ARB|VRB/VRB/;
  if ($pos eq "LOC") { $pos = "NELOC"; } ;
  $pos =~ s/ONOPL/UNRESOLVED/gi;
  $pos =~ s/(XXX|YYY|FFF)/UNRESOLVED/gi;
  $pos =~ s/\s+//g;
  $pos =~ s/.EDELHEID//;
  $pos =~ s/VNW/PRN/g;
  $pos =~ s/PART/VRB/g;
  $pos =~ s/NE_PR/NE_PER/g;
  $pos =~ s/NE_PERS/NE_PER/g;
  $pos =~ s/NE_LIC/NE_LOC/g;
  $pos =~ s/NE_PERN/NE_PER/g;
  $pos =~ s/NE_(PER|LOC|ORG|OTHER|FOREIGN)/NE$1/g;
  $pos =~ s/NE_PE/NEPER/g;
  $pos =~ s/NEFOREIGN/FOREIGN/;
  return "\"$pos\"";
}
