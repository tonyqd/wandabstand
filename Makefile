# location for the CGNS includes and required libraries
CGNSdir     = /global/CGNS
HDF5dir     = /global/HDF5
CGNSinclude = $(CGNSdir)/include
HDF5include = $(HDF5dir)/include
CGNSlibs    = $(CGNSdir)/lib/libcgns.a


# if you are using CGNS version 3.1 and have built as 64-bit,
# then you wiil need to set this option to whatever your Fortran compiler
# uses to automatically promote integer values to integer*8
I8OPT = -fdefault-integer-8 -fdefault-real-8

# Fortran compiler and options
F90   = mpif90 -f90=ifort
FFLAGS = -O3 
FOPTS = -I$(CGNSinclude) -I$(HDF5include)
LIBS  = $(CGNS_LIB) -lm

# how to name output executable and extension
FEOUT = -o

all: SmallestWallDistance

SmallestWallDistance: 
	$(F90) $(FOPTS) -c global.F90
	$(F90) $(FOPTS) -c SmallestWallDistance.F90
	$(F90) $(FEOUT) WallDistance global.o SmallestWallDistance.o $(LIBS)

clean:
	rm -rf *.o *.mod WallDistance 







