#!/usr/bin/env perl

use Data::Dumper ;

{
  my $overs = $ARGV[0] ;
  chomp $overs ;
  my @l = split(/\./, $overs) ;
  print (sprintf "%d%02d", $l[0], $l[1]) ;
}
