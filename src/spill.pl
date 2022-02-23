#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

my $text = do { local $/ = undef; my $fh = 'FileHandle'->new ("<make.eo"); <$fh> };

my %F902text = ($text =~ m/(\w+\.F90)(.*?)(?=nvfortran)/goms);

for my $F90 (sort keys (%F902text))
  {
    my $text = $F902text{$F90};
    my @spill = ($text =~ m/(\d+) bytes spill stores, (\d+) bytes spill loads/goms);
    my ($stores, $loads) = (0, 0);

    while (my ($x, $y) = splice (@spill, 0, 2))
      {
        $stores += $x;
        $loads  += $y;
      }

    printf ("%-40s : %8d %8d\n", $F90, $stores, $loads) if ($stores || $loads);
  }
