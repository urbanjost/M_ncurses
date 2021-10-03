# M_ncurses.f90 and associated files

![example](docs/images/example.gif)

## NAME

### M_ncurses - Fortran interface for the C Ncurses library

## DESCRIPTION

   M_ncurses(3f) is a Fortran module that allows use of the ncurses(3c)
   library for controlling and formatting terminal displays.

## DOWNLOAD AND BUILD WITH MAKE
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

## DOWNLOAD AND BUILD WITH FPM ![fpm](docs/images/fpm_logo.gif)
<!--
#### (registered at the [fpm(1) registry](https://github.com/fortran-lang/fpm-registry) )
-->

To download the github repository and build it with 
fpm ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )

```bash
     git clone https://github.com/urbanjost/M_strings.git
     cd M_strings
     fpm test  # run unit tests
```

or just list it as a dependency in your fpm.toml project file.

```toml
     [dependencies]
     M_strings        = { git = "https://github.com/urbanjost/M_strings.git" ,tag="v1.0.1"}
```


## DOCUMENTATION

- [M_ncurses](https://urbanjost.github.io/M_ncurses/index.html)  -- An overview of the M_ncurses module
