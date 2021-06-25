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

  if ($a->nodeName eq 'named-E')
    {
      return &replaceArgNamedE ($e, $a);
    }
  elsif ($a->nodeName eq 'op-E')
    {
      return &replaceArgOpE ($e, $a);
    }
  else
    {
      die $a->toString;
    }

}

sub replaceArgOpE
{
  my ($e, $a) = @_;

  $a = &n ('<parens-E>(' . $a->textContent . ')</parens-E>');

  my @re = &f ('./f:R-LT/' . &Fxtran::xpath_by_type ('R'), $e);

  die $a->toString if (@re);

  $e->replaceNode ($a);

}

sub replaceArgNamedE
{
  my ($e, $a) = @_;

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

      die unless ($re->nodeName =~ m/(?:parens|array)-R$/o);
      die unless ($ra->nodeName =~ m/(?:parens|array)-R$/o);

      my @el = &f ('./f:element-LT/f:element/' . &Fxtran::xpath_by_type ('E'), $re);
      my @ss = &f ('./f:section-subscript-LT/f:section-subscript/node ()', $ra);

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

my ($f1, $suf) = @ARGV;

$suf ||= '';

my $d1 = &Fxtran::fxtran (location => $f1);

my ($d2) = &f ('.//f:program-unit[./f:subroutine-stmt[./f:subroutine-N/f:N/f:n/text ()="DELTA_C"]]', $d1);

$d2 = $d2->cloneNode (1);

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

        my @n;
        for (my $n = $stmt; $n; $n = $n->nextSibling)
          {
            push @n, $n;
            if ($n->nodeName eq '#text')
              {
                last if ($n->data =~ m/\n/o);
              }
          }

        for (my $n = $stmt->previousSibling; $n; $n = $n->previousSibling)
          {
            if ($n->nodeName eq '#text')
              {
                if ($n->data =~ m/\n/o)
                  {
                    my $t = $n->data;
                    $t =~ s/\n\s*$/\n/o;
                    $n->setData ($t);
                    last;
                  }
              }
            push @n, $n;
          }


        for (@n)
          {
            $_->unbindNode ();
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


for (&f ('.//f:implicit-none-stmt', $d2))
  {
    $_->unbindNode (); # Should move includes to d1
  }

for my $call (@call)
  {
    my @stmt = &f ('descendant-or-self::f:program-unit/node ()', $d2);

    shift (@stmt);
    pop (@stmt);

    for my $stmt (reverse @stmt)
      {
        $call->parentNode->insertAfter ($stmt, $call);
      }

    my @c = split (m/\n/o, $call->textContent ());
    for my $c (reverse @c)
      {
        $c = "! $c";
        $c = &t ($c);
        $c = $c->toString ();
        $call->parentNode->insertAfter (&t ("\n"), $call);
        $call->parentNode->insertAfter (&n ("<C>" . $c . "</C>"), $call);
      }
    $call->unbindNode ();

    last;
  }





'FileHandle'->new (">$f1.new")->print ($d1->textContent ());
'FileHandle'->new (">delta_c.F90.new")->print ($d2->textContent ());


