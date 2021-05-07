# Acraneb2 wrapper

## Background

acraneb2 is an atmospheric radiation parameterization scheme

A scientific description can be found in Geleyn et al., 2017 (doi:10.1002/qj.3006)
and Masek et al., 2015 (doi: 10.1002/qj.2653).

Data for this wrapper are taken from a 1.3km resolution run with 87 vertical levels
with a timestep of 60s, on August 1 2016. Forecast lead time is 12h.

Intermittency is switched off.

## Usage

Compilation is done with make; a Makefile for gfortran is provided.

Running is done by executing the `main` program. Command line options:  
  `--help           `   show this help  
  `--nproma {nproma}`   blocking size  
  `--nlev   {nlev}  `   number of vertical levels  
  `--ngpblk {ngpblk}`   number of blocks  
  `--ncount {ncount}`   number of repetitions  
  `--check          `   check results  
 
Data are provided in the files `acraneb2.in` and `acraneb2.out` in the data directory.
These are non-standard data files (undocumented, little-endian stream from Fortran).
They can only be read with the `load_acraneb2` subroutine

Although the test data are limited in size (256 columns), the wrapper replicates these data as needed to fill the user-defined `nproma`x`ngpblk` columns.
