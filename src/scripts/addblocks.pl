#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

my $F90 = shift;

my $doc = &Fxtran::fxtran (location => $F90);

my @pu = &f ('./f:object/f:file/f:program-unit', $doc);

for my $pu (@pu)
  {

    # Add KGPBLKS argument
    {
      my ($KLON) = &f ('.//f:arg-N[./f:N/f:n/text ()="KLON"]', $pu);
      $KLON->parentNode->insertBefore (&n ('<arg-N><N><n>KGPBLKS</n></N></arg-N>'), $KLON);
      $KLON->parentNode->insertBefore (&t (','), $KLON);
    }

    # Declare KGPBLKS argument
    {
      my ($KLON) = &f ('.//f:T-decl-stmt//f:EN-decl[./f:EN-N/f:N/f:n/text ()="KLON"]', $pu);
      $KLON->parentNode->insertBefore (&n ('<EN-decl><EN-N><N><n>KGPBLKS</n></N></EN-N></EN-decl>'), $KLON);
      $KLON->parentNode->insertBefore (&t (','), $KLON);
    }

    # Insert KGPBLKS argument in CALL statements
    {
      my @KLON = &f ('.//f:arg[./f:named-E/f:N/f:n/text ()="KLON"]', $pu);
      for my $KLON (@KLON)
        {
          $KLON->parentNode->insertBefore (&n ('<arg><named-E><N><n>KGPBLKS</n></N></named-E></arg>'), $KLON);
          $KLON->parentNode->insertBefore (&t (','), $KLON);
        }
    }

    # Add JBLK loop variable
    {
      my ($JLON) = &f ('.//f:T-decl-stmt//f:EN-decl[./f:EN-N/f:N/f:n/text ()="JLON"]', $pu);
      $JLON->parentNode->insertBefore (&n ('<EN-decl><EN-N><N><n>JBLK</n></N></EN-N></EN-decl>'), $JLON);
      $JLON->parentNode->insertBefore (&t (','), $JLON);
    }


    # Add JBLK dimension to arrays whose first dimension is KLON
    my @sslt = &f ('.//f:EN-decl/f:array-spec/f:shape-spec-LT[./f:shape-spec/f:upper-bound/f:named-E/f:N/f:n/text ()="KLON"]', $pu);

    for my $sslt (@sslt)
      {
        $sslt->appendChild (&t (','));
        $sslt->appendChild (&n ('<shape-spec><upper-bound><named-E><N><n>KGPBLKS</n></N></named-E></upper-bound></shape-spec>'));
      }
   

    # Find DO loops with JLON variable

    my @do = &f ('.//f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text ()="JLON"]', $pu);

    my %lh;


    # For each JLON DO loop, find the outermost loop enclosing this JLON DO loop

    for my $do (@do)
      {
        my ($doo) = &f ('ancestor-or-self::f:do-construct', $do);
        $lh{$doo} = $doo;
      }

    my %seen;
    my @lh = grep { ! ($seen{$_}++) } values (%lh);

    # For each top level loop, enclose it in JBLK and JLON loops

    for my $lh (@lh)
      {
        # Find current level of indentation

        my $sp = $lh->previousSibling;
        $sp = $sp->textContent;
        $sp =~ s/^\s*\n//o;

        # Create a JBLK/JLON loop nest

        my @dob = &n (<< "EOF");
<do-construct><do-stmt>DO <do-V><named-E><N><n>JBLK</n></N></named-E></do-V> = <lower-bound><literal-E><l>1</l></literal-E></lower-bound>, <upper-bound><named-E><N><n>KGPBLKS</n></N></named-E></upper-bound></do-stmt>
$sp<do-construct><do-stmt>DO <do-V><named-E><N><n>JLON</n></N></named-E></do-V> = <lower-bound><named-E><N><n>KIDIA</n></N></named-E></lower-bound>, <upper-bound><named-E><N><n>KFDIA</n></N></named-E></upper-bound></do-stmt>
$sp<C/>
$sp<end-do-stmt>ENDDO</end-do-stmt></do-construct>
$sp<end-do-stmt>ENDDO</end-do-stmt></do-construct>
EOF

        # Inject the nest before the outermost loop

        for my $dob (@dob)
          {
            $lh->parentNode->insertBefore ($dob, $lh);
          }

        # Re-nest outermost loop
        my ($dob)= &f ('.//f:C', $dob[0]);

        $dob->replaceNode ($lh);

        # Add JBLK index to variables

        my @elt = &f ('.//f:element-LT[./f:element/f:named-E/f:N/f:n/text ()="JLON"]', $lh);
        for my $elt (@elt)
          {
            $elt->appendChild (&t (','));
            $elt->appendChild (&n ('<element><named-E><N><n>JBLK</n></N></named-E></element>'));
          }

        # Remove innermost JLON DO loops

        my @do = &f ('descendant-or-self::f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text ()="JLON"]', $lh);

        for my $do  (@do)
          {
            $do->firstChild->unbindNode ();
            $do->lastChild->unbindNode ();
            # Use a pseudo target to remove the loop construct
            my $C = &n ('<C/>');
            $do->replaceNode ($C);
            for my $c ($do->childNodes ())
              {
                $C->parentNode->insertBefore ($c, $C);
              }
            $C->unbindNode ();
          } 
      }


  }

'FileHandle'->new (">$F90.new")->print ($doc->textContent);
