#!/home/ms/fr/sor/install/perl-5.32.1/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

sub replaceArg
{
  my ($e, $a) = @_;

  die $a->toString if ($a->nodeName ne 'named-E');

  my ($ne) = &f ('./f:N/f:n/text ()', $e);
  my ($na) = &f ('./f:N/f:n/text ()', $a);

  my @re = &f ('./f:R-LT/' . &Fxtran::xpath_by_type ('R'), $e);
  my @ra = &f ('./f:R-LT/' . &Fxtran::xpath_by_type ('R'), $a);

  my $se = $e->toString;
  my $sa = $a->toString;

  $ne->replaceNode (&t ($na->textContent));

  if (scalar (@ra) == 0)
    {
      return;
    }

  if ((scalar (@ra) == 0) && (scalar (@re) == 1))
    {
      return;
    }
  
  if ((scalar (@ra) == 1) && (scalar (@re) == 1))
    {
      my ($re, $ra) = (@re, @ra);
  
      $ra = $ra->cloneNode (1);

      die unless ($re->nodeName eq 'parens-R');
      die unless ($ra->nodeName eq 'parens-R');

      my @el = &f ('./f:element-LT/f:element/' . &Fxtran::xpath_by_type ('E'), $re);
      my @ss = &f ('./f:array-R/f:section-subscript-LT/f:section-subscript/node ()', $ra);

      for (my $i = 0; $i < @el; $i++)
        {
          if ($ss[$i]->textContent eq ':')
            {
              $ss[$i]->replaceNode ($el[$i]->cloneNode (1));
            }
          else
            {
              die $ss[$i]->textContent;
            }
        }
        
      $re->replaceNode ($ra);
      return;
    }


  print &Dumper ([map { $_->toString } @re]);
  print &Dumper ([map { $_->toString } @ra]);

  die "$se\n$sa\n";

}

my ($f1, $f2, $suf) = @ARGV;

$suf ||= '';

my $d1 = &Fxtran::fxtran (location => $f1);
my $d2 = &Fxtran::fxtran (location => $f2);

my ($s1) = &f ('.//f:subroutine-stmt', $d1);
my ($s2) = &f ('.//f:subroutine-stmt', $d2);

my ($n2) = &f ('./f:subroutine-N/f:N/f:n/text ()', $s2, 1);

my @da = &f ('./f:dummy-arg-LT/f:arg-N/f:N/f:n/text ()', $s2, 1);
my %da2aa;

my @call = &f ('.//f:call-stmt[./f:procedure-designator/f:named-E/f:N/f:n/text ()="?"]', $n2, $d1);

for my $call (@call)
  {
    my @aa = &f ('.//f:arg-spec/f:arg/*', $call);
    die $call->toString unless (@aa == @da);
    for my $aa (@aa)
      {
        # check we have a simple named expression without any reference 
        if (($aa->nodeName ne 'named-E') && (&f ('.//f:R-LT//parens-R', $aa)))
          {
            die $aa->toString;
          }
      }
    for my $i (0 .. $#aa)
      {
        $da2aa{$da[$i]} = $aa[$i];
      }
    
    last;
  }


# Remove dummy arguments declaration

for my $da (@da)
  {
    my @en_decl = &f ('.//f:EN-decl/f:EN-N/f:N/f:n[text ()="?"]', $da, $d2);
    for my $en_decl (@en_decl)
      {
        my ($stmt) = &Fxtran::stmt ($en_decl);
        my ($cr) = &f ('following::text ()[contains (., "' . "\n" . '")]', $stmt);
        $stmt->unbindNode ();
        $cr->unbindNode ();
      }
  }

# Move use statements 

my @use1 = &f ('.//f:use-stmt', $d1);

my $stmt = $use1[-1];

$stmt ||= $s1;

my @use2 = &f ('.//f:use-stmt', $d2);

for my $use2 (reverse (@use2))
  {
    my ($cr) = &f ('following::text ()[contains (., "' . "\n" . '")]', $use2);
    $s1->parentNode ()->insertAfter ($use2, $stmt);
    $s1->parentNode ()->insertAfter (&t ("\n"), $stmt);
    $cr->unbindNode ();
  }

my @en_decl1 = &f ('.//f:EN-decl', $d1);
my @en_decl2 = &f ('.//f:EN-decl', $d2);

# Remove comments before declarations

if (@en_decl2)
  {
    my @C = &f ('preceding::f:C', $en_decl2[0]);
    for my $C (@C)
      {
        my ($cr) = &f ('following::text ()[contains (., "' . "\n" . '")]', $C);
        $C->unbindNode ();
        $cr->unbindNode ();
      }
  }

# Remove first and last statements

$s2->parentNode ()->lastChild ()->unbindNode ();
$s2->unbindNode ();

# Move declarations

for my $en_decl2 (@en_decl2)
  {

    my ($v2) = &f ('./f:EN-N/f:N/f:n/text ()', $en_decl2);

    my ($v1) = &f ('.//f:EN-decl[./f:EN-N/f:N/f:n[text ()="?"]]', $v2->textContent, $d1);

    # This variable exists in first subroutine
    if (($v1 || $suf) && ($v2 !~ m/^(?:JLON|JLEV|JROF)$/o))
      {
        my $s = $suf || "_$n2";
        $v2->replaceNode (&t ("$v2$suf"));
        my @e2 = &f ('.//f:named-E/f:N/f:n[text ()="?"]/text ()', $v2, $d2);
        for my $e2 (@e2) 
          {
            $e2->replaceNode (&t ("$v2$suf"));
          }
      }
  }

# Replace dummy arguments by actual arguments
for my $da (@da)
  {
    my @e = &f ('.//f:named-E[./f:N/f:n/text ()="?"]', $da, $d2);

    for my $e (@e)
      {
        &replaceArg ($e, $da2aa{$da});
      }
  }


my @decl_stmt1 = &f ('.//f:' . &Fxtran::xpath_by_type ('stmt') . '[.//f:EN-decl]', $d1);

my $decl_stmt1 = $decl_stmt1[-1];

for my $decl_stmt2 (&f ('.//f:' . &Fxtran::xpath_by_type ('stmt') . '[.//f:EN-decl]', $d2))
  {
    $decl_stmt1->parentNode->insertAfter ($decl_stmt2, $decl_stmt1);
    $decl_stmt1->parentNode->insertAfter (&t ("\n"), $decl_stmt1);
  }

for (&f ('.//f:include', $d2))
  {
    $_->unbindNode (); # Should move includes to d1
  }
for (&f ('.//f:implicit-none-stmt', $d2))
  {
    $_->unbindNode (); # Should move includes to d1
  }
for (&f ('.//f:broken-stmt[./text ()="init_stack ()"]', $d2))
  {
    $_->unbindNode (); 
  }


for my $call (@call)
  {
    my @stmt = &f ('.//f:program-unit/node ()', $d2);
    for my $stmt (@stmt)
      {
        $call->parentNode->insertBefore ($stmt, $call);
      }
    my $c = $call->textContent ();
    $c =~ s/\n/\n! /goms;
    $c = "! $c";
    $call->parentNode->insertBefore (&t ($c), $call);
    $call->unbindNode ();
    last;
  }





'FileHandle'->new (">$f1.new")->print ($d1->textContent ());
'FileHandle'->new (">$f2.new")->print ($d2->textContent ());


