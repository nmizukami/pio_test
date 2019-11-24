#========================================================================
# Makefile to compile the routing model
#========================================================================
#
# Need to edit FC, FC_EXE, EXE, F_MASTER, NCDF_PATH or PNCDF_PATH (if used)
# Feel free to modify compiler flag (FLAGS)
# Do not leave space after your edits
#
#========================================================================
# User configure part
#========================================================================
# Define fortran compiler - gnu, intel or pgi
FC  =

# Define the compiler exe
# For MPI use, mpifort, mpif77, mpif90, mpif08
FC_EXE =

# Define the executable
EXE = pio_test.exe

# define if should use external librarys (yes if used)
# external librarys are: mpi-serial, Parallel-I/O, and GPTL timing library
# define path to PNETCDF if you want to use it
isMPISERIAL =
isPIO = yes
isGPTL =

# If isPIO==yes then you can give hints to PIO about the local system
# gpfs is the specific filesystem used on cheyenne
# (can be gpfs or lustre) Only use it if you know for sure about your filesystem.
# If not make this setting blank
PIO_FILESYSTEM_HINTS = gpfs

# Define core directory below which everything resides
# parent directory of the 'build' directory
# do not put space at the end of path
F_MASTER =

# Define the NetCDF libraries and path to include files

# pnetcdf location on cheyenne
#  gnu   mpt:      /glade/u/apps/ch/opt/pnetcdf/1.11.1/mpt/2.19/gnu/8.3.0
#  gnu   openmpi:  /glade/u/apps/ch/opt/pnetcdf/1.11.1/openmpi/3.1.4/gnu/8.3.0
#  intel mpt:      /glade/u/apps/ch/opt/pnetcdf/1.11.1/mpt/2.19/intel/18.0.5
#  intel opnempi:  /glade/u/apps/ch/opt/pnetcdf/1.11.1/openmpi/3.1.4/intel/18.0.5
#  pgi   mpt:      /glade/u/apps/ch/opt/pnetcdf/1.11.1/mpt/2.19/pgi/19.3
#  pgi   opnempi:  /glade/u/apps/ch/opt/pnetcdf/1.11.1/openmpi/3.1.4/pgi/19.3

# netcdf location on cheyenne
#  gnu:   /glade/u/apps/ch/opt/netcdf/4.6.3/gnu/8.3.0
#  intel: /glade/u/apps/ch/opt/netcdf/4.6.3/intel/18.0.5
#  pgi:   /glade/u/apps/ch/opt/netcdf/4.6.3/pgi/19.3

# netcdf location on casper
# gnu:   /glade/u/apps/ch/opt/netcdf/4.6.1/gnu/7.3.0

ifeq "$(FC)" "gnu"
 NCDF_PATH = /glade/u/apps/ch/opt/netcdf/4.6.3/gnu/8.3.0
 PNETCDF_PATH = /glade/u/apps/ch/opt/pnetcdf/1.11.1/mpt/2.19/gnu/8.3.0
endif
ifeq "$(FC)" "intel"
 NCDF_PATH = /glade/u/apps/ch/opt/netcdf/4.6.3/intel/18.0.5
 PNETCDF_PATH = /glade/u/apps/ch/opt/pnetcdf/1.11.1/mpt/2.19/intel/18.0.5
endif
ifeq "$(FC)" "pgi"
 NCDF_PATH = /glade/u/apps/ch/opt/netcdf/4.6.3/pgi/19.3
 PNETCDF_PATH = /glade/u/apps/ch/opt/pnetcdf/1.11.1/mpt/2.19/pgi/19.3
endif

LIBNETCDF = -Wl,-rpath,$(NCDF_PATH)/lib \
            -L$(NCDF_PATH)/lib -lnetcdff -lnetcdf
INCNETCDF = -I$(NCDF_PATH)/include


#========================================================================
# Define flags
#========================================================================
ifeq "$(FC)" "gnu"
    FLAGS = -g -Wall -fmax-errors=0 -fbacktrace -fcheck=all -ffpe-trap=zero -ffree-line-length-none $(FLAGS_OMP)
endif

ifeq "$(FC)" "intel"
    FLAGS = -g -debug all -warn all -check all -FR -O0 -auto -WB -fpe0 -traceback -fltconsistency $(FLAGS_OMP)
endif

ifeq "$(FC)" "pgi"
    FLAGS = -g -traceback -Mbounds -Mlist -Minfo -Mdclchk $(FLAGS_OMP)
endif

#========================================================================
# Define directory paths
#========================================================================
# Core directory that contains source code
F_KORE_DIR = $(F_MASTER)build/src/

# Location of the compiled modules
MOD_PATH = $(F_MASTER)build/

# Define the directory for the executables
EXE_PATH = $(F_MASTER)bin

# External libraries (if used)
EXTLIBS =
EXTINCLUDES =

#========================================================================
# Assemble all of the sub-routines
#========================================================================
# Define subroutines
SUB = \
    nrtype.f90 \
    globalData.f90 \
    nr_utility.f90 \
    mpi_utils.f90 \
    pio_utils.f90

# concatanate model subroutines
TEMP_MODSUB = $(SUB)

# insert appropriate directory name
MODSUB = $(patsubst %, $(F_KORE_DIR)%, $(TEMP_MODSUB))

#========================================================================
# Define driver programs
#========================================================================
# Define the driver routine
DRIVER = $(F_KORE_DIR)main.f90

# tasks
all: compile install clean

#========================================================================
# External libaries that might need to be built
#========================================================================
LIBDIR       = $(F_MASTER)build/lib
PIOLIBDIR    = $(LIBDIR)/piolib/lib
PIOINCDIR    = $(LIBDIR)/piolib/include
PIOLIB       = $(PIOLIBDIR)/libpiof.a $(PIOLIBDIR)/libpioc.a
MPISERLIBDIR = $(LIBDIR)/mpi-seriallib
MPISERLIB    = $(MPISERLIBDIR)/libmpi-serial.a

ifeq "$(isMPISERIAL)" "yes"
  EXTINCLUDES += -I$(MPISERLIBDIR)
  EXTLIBS += $(MPISERLIB)
  LDFLAGS += -L$(MPISERLIBDIR) -lmpi-serial
  ifdef PNETCDF_PATH
    undefine PNETCDF_PATH
  endif
  MPISERIAL = $(MPISERLIB)
else
  MPISERIAL =
endif

ifeq "$(isPIO)" "yes"
  EXTINCLUDES += -I$(PIOINCDIR)
  EXTLIBS += $(PIOLIB)
  LDFLAGS += -L$(PIOLIBDIR) -lpiof -lpioc

  ifeq "$(isGPTL)" "yes"
     LDFLAGS += -lgptl
  endif
endif

ifdef PNETCDF_PATH
        LDFLAGS    += -L$(PNETCDF_PATH)/lib -lpnetcdf
endif

$(PIOLIB): $(MPISERIAL)
	cd $(LIBDIR); \
	$(MAKE) $(MFLAGS) F_MASTER=$(F_MASTER) FC=$(FC) FC_EXE=$(FC_EXE) FLAGS="$(FLAGS)" \
	PIO_FILESYSTEM_HINTS=$(PIO_FILESYSTEM_HINTS) PNETCDF_PATH=$(PNETCDF_PATH) isPIO=yes isGPTL=$(isGPTL) \
	MPIINC_PATHLIST=$(CPATH) NCDF_PATH=$(NCDF_PATH) $(PIOLIB)

$(MPISERLIB):
	cd $(LIBDIR); \
	$(MAKE) $(MFLAGS) F_MASTER=$(F_MASTER) FC=$(FC) FC_EXE=$(FC_EXE) FLAGS="$(FLAGS)" isMPISERIAL=yes $(MPISERLIB)

# Clean external libs
cleanlibs: cleanpiolib cleanmpiseriallib
	@echo "Clean the pio and mpi-serial external libraries"
.PHONY : cleanlibs

cleanpiolib:
	cd $(LIBDIR); \
	$(MAKE) $(MFLAGS) F_MASTER=$(F_MASTER) cleanpiolib
.PHONY : cleanpiolib

cleanmpiseriallib:
	cd $(LIBDIR); \
	$(MAKE) $(MFLAGS) F_MASTER=$(F_MASTER) cleanmpiseriallib
.PHONY : cleanmpiseriallib

#========================================================================
# Compile the puppy
#========================================================================
# compile
compile: $(EXTLIBS)
	$(FC_EXE) $(FLAGS) $(EXTINCLUDES) $(MODSUB) $(DRIVER) \
	$(LIBNETCDF) $(INCNETCDF) $(LDFLAGS) -o $(EXE)

# Remove object files
clean:
						rm -f *.o
						rm -f *.lst
						rm -f *.mod
						rm -f *__genmod.f90
#
# Copy the executable to the bin directory
install:
	@mkdir -p $(EXE_PATH)
	@mv $(EXE) $(EXE_PATH)
