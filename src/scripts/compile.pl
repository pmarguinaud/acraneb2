#!/usr/bin/perl -w

use strict;
use FindBin qw ($Bin);
use lib $Bin;
use FileHandle;
use File::Copy;
use File::Basename;
use File::stat;
use File::Path;

sub newer
{
  my ($f1, $f2)  = @_;
  die unless (-f $f1);
  return 1 unless (-f $f2);
  return stat ($f1)->mtime > stat ($f2)->mtime;
}

sub copyIfNewer
{
  my ($f1, $f2) = @_;

  if (&newer ($f1, $f2))
    {
      print "Copy $f1 to $f2\n"; 
      &copy ($f1, $f2); 
    }
}

sub saveToFile
{
  my ($x, $f) = @_;

  unless (-d (my $d = &dirname ($f)))
    {
      &mkpath ($d);
    }

  'FileHandle'->new (">$f")->print ($x->textContent ());
  'FileHandle'->new (">$f.xml")->print ($x->toString ());
}

sub preProcessIfNewer
{
  use Inline;
  use Associate;
  use Fxtran;
  use Blocks;

  my ($f1, $f2) = @_;

  if (&newer ($f1, $f2))
    {
      print "Preprocess $f1\n";

      my $d = &Fxtran::fxtran (location => $f1);
      &saveToFile ($d, "tmp/$f2");

      &Inline::inlineContainedSubroutines ($d);
      &saveToFile ($d, "tmp/inlineContainedSubroutines/$f2");

      &Associate::resolveAssociates ($d);
      &saveToFile ($d, "tmp/resolveAssociates/$f2");

      &Blocks::addBlocks ($d);
      &saveToFile ($d, "tmp/addBlocks/$f2");

      &Blocks::addDirectives ($d);
      &saveToFile ($d, "tmp/addDirectives/$f2");

      'FileHandle'->new (">$f2")->print ($d->textContent ());

      &Fxtran::intfb ($f2);
    }
}

my $arch = shift;

my @compute = map { &basename ($_) } <compute/*.F90>;
my @support = map { &basename ($_) } <support/*.F90>;

&mkpath ("compile.$arch");

chdir ("compile.$arch");

for my $f (@support)
  {
    &copyIfNewer ("../support/$f", $f);
  }

for my $f (@compute)
  {
    &preProcessIfNewer ("../compute/$f", $f);
  }

&copy ("../Makefile.$arch", "Makefile.inc");
system ("$Bin/Makefile.PL") and die;
system ('make -j4 main.x') and die;





