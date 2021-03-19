# M_ncurses.f90 and associated files

![example](docs/images/example.gif)

## NAME

### M_ncurses - Fortran interface for the C Ncurses library

## DESCRIPTION

   M_ncurses(3f) is a Fortran module that allows use of the ncurses(3c)
   library for controlling and formatting terminal displays.

## DOWNLOAD
   ```bash
       git clone https://github.com/urbanjost/M_ncurses.git
       cd M_ncurses/src
       # change Makefile if not using one of the listed compilers
     
       # for gfortran
       make clean
       make F90=gfortran gfortran
     
       # for ifort
       make clean
       make F90=ifort ifort

       # for nvfortran
       make clean
       make F90=nvfortran nvfortran
   ```
   This will compile the M_ncurses module and build all the example programs.

## DOCUMENTATION

- [M_ncurses](https://urbanjost.github.io/M_ncurses/ncurses_from_Fortran.html)  -- An overview of the M_ncurses module
