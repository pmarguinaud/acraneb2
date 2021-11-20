#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

my $f = shift;

my $d = &Fxtran::fxtran (location => $f);


my ($noexec) = do  
{
  my ($exec) = grep { &Fxtran::stmt_is_executable ($_) } &F ('.//ANY-stmt', $d);
  my @prev = &F ('preceding::*', $exec);

  my $prev;
  for my $p (reverse (@prev))
    {   
      next if ($p->nodeName eq '#text');
      next if ($p->nodeName eq 'C');
      $prev = $p; 
      last;
    }   

  my @anc = &F ('ancestor::*', $prev);

  for my $anc (reverse (@anc))
    {   
      if (($anc->nodeName =~ m/-(?:construct|stmt)$/o) || ($anc->nodeName eq 'include'))
        {
          $prev = $anc;
        }
    }   

  $prev
};  





my @en_decl = &F ('.//EN-decl[./array-spec/shape-spec-LT[count(shape-spec)=1][string(shape-spec)="?"]]', 'KLON', $d);

for my $en_decl (@en_decl)
  {
    my ($N) = &F ('./EN-N', $en_decl, 1);

    my ($as) = &F ('./array-spec', $en_decl);
    $as->unbindNode ();

    my @rlt = &F ('.//named-E[string(N)="?"]/R-LT', $N, $d);

    for my $rlt (@rlt)
      {
        $rlt->unbindNode ();
      }

  }
    

'FileHandle'->new (">$f.new")->print ($d->textContent);
