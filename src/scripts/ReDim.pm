package ReDim;
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

sub reDim
{
  my $d = shift;
  
  my @en_decl = &F ('.//EN-decl[./array-spec/shape-spec-LT[count(shape-spec)=1][string(shape-spec)="?"]]', 'KLON', $d);
  
  for my $en_decl (@en_decl)
    {
      my ($N) = &F ('./EN-N', $en_decl, 1);
      my ($stmt) = &Fxtran::stmt ($en_decl);

      next if (&F ('.//attribute-N[string(.)="INTENT"]', $stmt));
      next if (&F ('.//call-stmt[.//named-E[string(N)="?"]', $N, $d));

  
      my ($as) = &F ('./array-spec', $en_decl);
      $as->unbindNode ();
  
      my @rlt = &F ('.//named-E[string(N)="?"]/R-LT', $N, $d);
  
      for my $rlt (@rlt)
        {
          $rlt->unbindNode ();
        }
  
    }
}

1;
