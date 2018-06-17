while(<>)
{
  chomp();
  my ($pre,$post) = split(/<b>/,$_);
  $pre =~ s/<(\/?)span.*?>//g;
  $post =~ s/<span.*?<\/span>/<b>$&<\/b>/;
  print "$pre<b>$post\n";
}
