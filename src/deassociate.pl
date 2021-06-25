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

my @assoc = &f ('.//f:associate-construct', $d1);

for my $assoc (@assoc)
  {
    my @as = &f ('./f:associate-stmt/f:associate-LT/f:associate', $assoc);

    # Apply association rules

    for my $as (@as)
      {
        my ($n) = &f ('./f:associate-N/f:n/text ()', $as, 1);
        my ($e) = &f ('./f:selector/f:named-E', $as);

        my @expr = &f ('.//f:named-E[./f:N/f:n/text ()="?"]', $n, $assoc);

        for my $expr (@expr)
          {
            my @r = &f ('./f:R-LT/node ()', $expr);
            my $E = $e->cloneNode (1);
            if (@r)
              {
                my ($rlt) = &f ('./f:R-LT', $E);
                unless ($rlt)
                  {
                    $rlt = &n ('<R-LT/>');
                    $E->appendChild ($rlt);
                  }
                for (@r)
                  {
                    $rlt->appendChild ($_);
                  }
              }
            $expr->replaceNode ($E);
          }

      }
 
    # Remove ASSOCIATE block & statements

    $assoc->firstChild->unbindNode ();
    $assoc->lastChild->unbindNode ();

    for (&f ('./node ()', $assoc))
      {
        $assoc->parentNode->insertBefore ($_, $assoc);
      }

    $assoc->unbindNode ();
  }


'FileHandle'->new (">$f1.new")->print ($d1->textContent);


