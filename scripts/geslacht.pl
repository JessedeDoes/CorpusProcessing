my $sql = "copy (select lemma_id,modern_lemma,lemma_gigpos from data.lemmata where lemma_gigpos ~ 'NOU-C' and is_parent=false) to stdout";


$ENV{"PGPASSWORD"} = "inl";

open(x, "psql -hsvowdb02 -Upostgres gig_pro -c \"$sql\"|") || die;

while(<x>)
{
  chomp();
  my ($id, $lemma, $pos) = split("\t",$_);
  if ($pos =~ /gender=(.*?)[,)]/)
  {
    my $g = $1;
    my @genders;
    if ($g =~ /n/) { push(@genders,"onz"); } ;
    if ($g =~ /[mf]/) { push(@genders,"zijd"); } ;
    foreach my $ge (@genders)
    {
      $gender{$lemma}{$ge}++;
    }
  }
}

foreach my $x (sort keys %gender)
{
  my $h = $gender{$x};
  my $ge = join(",", sort keys %$h);
  print "$x\t$ge\n";
}


