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
FFLAGS = -c -O3 -fpp 
FOPTS = -I$(CGNSinclude) -I$(HDF5include)
LIBS  = $(CGNS_LIB) -lm

# how to name output executable and extension
FEOUT = -o

all: SmallestWallDistance

SmallestWallDistance: 
	$(F90) $(FOPTS) $(FFLAGS) global.F90 
	$(F90) $(FOPTS) $(FFLAGS) parallel.F90
	$(F90) $(FOPTS) $(FFLAGS) helpers.F90
	$(F90) $(FOPTS) $(FFLAGS) LogModule.F90
	$(F90) $(FOPTS) $(FFLAGS) ConfigurationModule.F90 
	$(F90) $(FOPTS) $(FFLAGS) ReadCGNSModule.F90
	$(F90) $(FOPTS) $(FFLAGS) SmallestWallDistance.F90
	$(F90) $(FEOUT) WallDistance global.o parallel.o helpers.o LogModule.o ConfigurationModule.o ReadCGNSModule.o SmallestWallDistance.o $(LIBS)

clean:
	rm -rf  *.o *.mod WallDistance 







