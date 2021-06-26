#!/usr/bin/perl -w

use strict;
use FindBin qw ($Bin);
use lib $Bin;
use FileHandle;
use File::Copy;
use File::Basename;
use File::stat;

sub slurp
{
  my $f = shift;
  my $fh = 'FileHandle'->new ("<$f");
  $fh or return '';
  local $/ = undef;
  return <$fh>;
}

sub cmp
{
  my ($f1, $f2)  = @_;
  return &slurp ($f1) ne &slurp ($f2);
}

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

sub preProcessIfNewer
{
  use Inline;
  use Fxtran;

  my ($f1, $f2) = @_;

  if (&newer ($f1, $f2))
    {
      print "Preprocess $f1\n";
      my $d = &Fxtran::fxtran (location => $f1);
      &Inline::inlineContainedSubroutines ($d);
      'FileHandle'->new (">$f2")->print ($d->textContent ());
      &Fxtran::intfb ($f2);
    }
}

my @compute = map { &basename ($_) } <compute/*.F90>;
my @support = map { &basename ($_) } <support/*.F90>;

chdir ('compile');

for my $f (@support)
  {
    &copyIfNewer ("../support/$f", $f);
  }

for my $f (@compute)
  {
    &preProcessIfNewer ("../compute/$f", $f);
  }


__END__

cd compile
../../util/finddeps.sh > Makefile_deps.mk

for f in "${compute[@]}"
do

done




