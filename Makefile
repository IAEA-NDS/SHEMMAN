
# set default fortran compiler:

FC   =  gfortran
#FC  =  ifort



# various flags are set based on compiler FC:
# FFLAGS are the normal complier options for production code
# DFLAGS are the options used when debugging (except for ECIS)
# EFLAGS are the options used for ECIS (no debug allowed)
# OFLAGS are the options used for OPTMAN

LIBS =
DFLAGS = 
FFLAGS = 
EFLAGS = 
OFLAGS = 

ifeq ($(FC),gfortran)

  #---------------------------------
  #----GNU gfortran FORTRAN compiler
  #---------------------------------
  #----flags for production compilation with gfortran
  FFLAGS = -O3 -std=legacy -ftree-vectorize -ffast-math -cpp
  ifeq ($(PARALLEL),OPENMP) 
    FFLAGS += -fopenmp
  endif
  #FFLAGS = -O3 -pg -std=legacy
  #----flags for debuging
  DFLAGS =  -O0 -g --bounds-check -std=legacy -ftrapv 
  # -pg shoudl be added for profiling
  #----flags for OPTMAN
  OFLAGS = -O2 -std=legacy -ffast-math

else ifeq ($(FC),ifort)
 
  #---------------------------
  #----INTEL f95 compiler
  #---------------------------
  #----flags for production compilation using ifort
  # ***  Please note that ifort v11.1 does not work properly with -ipo !!!!!  
  #----flags for debuging
  DFLAGS = -O0 -g -debug all -check all -warn unused -fp-stack-check -ftrapuv -trace -logo
  #------------------------------------------------------------------------------------------------
  # FFLAGS =  -O3 -x=host -logo -parallel -openmp-report1 -par-threshold2 -openmp -vec-report1
  # flags for automatic & openMP directives
  
  FFLAGS = -O3 -fpp 
  # Flags for OPTMAN
  OFLAGS = -O2 


endif


# make sure MAKE knows f90 extension
%.o : src/%.f
	$(FC) $(FFLAGS) -c $<

OBJF = SHEMMAN.o SHEMSOFD.o

all: 
	$(MAKE) shemman

shemman: $(OBJF) 
	$(FC) $(FFLAGS) -o shemman $(OBJF) $(LIBS)

clean:
	rm -f *.o *.mod

cleanall:
	rm -f *.o *.mod shemman *.optrpt
