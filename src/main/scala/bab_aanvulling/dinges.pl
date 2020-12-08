while(<>)
{
  my $line = $_;
  s/\r//;
  my $id = $line;
  $id =~ s/\t.*//s;
  print("    // $line");
  print("    \"bab$id\" => List( ),\n");
}
