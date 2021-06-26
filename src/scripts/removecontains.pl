#!/home/ms/fr/sor/install/perl-5.32.1/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

my ($f1) = @ARGV;

my $d1 = &Fxtran::fxtran (location => $f1);

for (&f ('.//f:program-unit//f:program-unit', $d1))
  {
    $_->unbindNode ();
  }

for (&f ('.//f:program-unit//f:contains-stmt', $d1))
  {
    $_->unbindNode ();
  }

'FileHandle'->new (">$f1.new")->print ($d1->textContent);


