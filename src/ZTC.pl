#!/usr/bin/perl -w

use strict;
use FileHandle;
use Data::Dumper;

my $f = shift;

my $text = do { my $fh = 'FileHandle'->new ("<$f"); local $/ = undef; <$fh> };


my @bZTC = ($text =~ m/cpu_(s|d).*?--nproma\s+(\d+)\s+--ngpblk\s+(\d+).*?ZTC =\s+(\S+)\s+(\S+)/goms);

my %fh;

while (my ($p, $n, $b, $ZTC1, $ZTC2) = splice (@bZTC, 0, 5))
  {
    my $fh = $fh{"$p.$n"};
    unless ($fh)
      {
        $fh{"$p.$n"} = $fh = 'FileHandle'->new (">ZTC.AMD.$p.$n.txt");
      }
    $fh->printf ("%s %10d %10d %10d %12.4e %12.4e\n", $p, $n * $b, $n, $b, $ZTC1, $ZTC2);
  }

for (values (%fh))
  {
    $_->close ();
  }

__END__
+ ../src/compile.gpu/main.x --nproma 32 --ngpblk 1284 --ncount 10 --save --check
 Running acraneb2 wrapper with
   nproma =            32
   nlev   =            87
   ngpblk =          1284
   ncount =            10
   lcheck =   T   
   lsave  =   T   
elapsed time :     7.82 s
          i.e.   0.0190 ms/gp
  ZTD =     7.815284490585327        1.9020844262522700E-005
  ZTC =     6.341063976287842        1.5432885456308028E-005

