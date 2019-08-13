module pio_module

  USE mpi
  USE nrtype
  USE pio,   ONLY : iosystem_desc_t, file_desc_t, io_desc_t,var_desc_t
  USE pio,   ONLY : PIO_init, PIO_rearr_subset
  USE pio,   ONLY : PIO_finalize, PIO_noerr, PIO_iotype_netcdf
  USE pio,   ONLY : PIO_int, PIO_double, PIO_redef, PIO_enddef
  USE pio,   ONLY : PIO_def_dim                                 !
  USE pio,   ONLY : PIO_def_var                                 ! Overload function to Define a netcdf variable
  USE pio,   ONLY : PIO_closefile, PIO_initdecomp
  USE pio,   ONLY : PIO_createfile, PIO_clobber, PIO_noclobber  ! Create a NetCDF and creation mode
  USE pio,   ONLY : PIO_write_darray                            ! Overloaded function to write a distributed array to disk
  USE pio,   ONLY : PIO_read_darray                             ! Overloaded function to read a distributed array from disk
  USE pio,   ONLY : PIO_freedecomp, PIO_syncfile, PIO_OFFSET_KIND
  USE pio,   ONLY : PIO_nowrite, PIO_openfile

  implicit none

  private

  !> @brief Error code if anything goes wrong.
  integer(i4b)     :: ERR_CODE

  !> @brief A class to hold data and procedures.
  type, public :: pioClass

    !> @brief Rank of processor running the code.
    integer(i4b) :: myRank

    !> @brief Number of processors participating in MPI communicator.
    integer(i4b) :: ntasks

    !> @brief Number of processors performing I/O.
    integer(i4b) :: niotasks

    !> @brief Stride in the mpi rank between io tasks.
    integer(i4b) :: stride

    !> @brief Number of aggregator.
    integer(i4b) :: numAggregator

    !> @brief Start index of I/O processors.
    integer(i4b) :: optBase

    !> @brief The ParallelIO system set up by @ref PIO_init.
    type(iosystem_desc_t) :: pioIoSystem

    !> @brief Contains data identifying the file.
    type(file_desc_t)     :: pioFileDesc

    !> @brief The netCDF variable ID.
    type(var_desc_t)      :: pioVar

    !> @brief An io descriptor handle that is generated in @ref PIO_initdecomp.
    type(io_desc_t)       :: iodescNCells

    !> @brief Specifies the flavor of netCDF output.
    integer(i4b)               :: iotype

    !> @brief The netCDF dimension ID.
    integer(i4b)               :: pioDimId

    !> @brief 1-based index of start of this processors data in full data array.
    integer(i4b)               :: ista

    !> @brief Size of data array for this processor.
    integer(i4b)               :: isto

    !> @brief Number of elements handled by each processor.
    integer(i4b)               :: arrIdxPerPe

    !> @brief The length of the dimension of the netCDF variable.
    integer(i4b), dimension(1) :: dimLen

    !> @brief Buffer to hold sample data that is written to netCDF file.
    real(dp),     allocatable  :: dataBuffer(:)

    !> @brief Buffer to read data into.
    integer(i4b), allocatable  :: readBuffer(:)

    !> @brief Array describing the decomposition of the data.
    integer(i4b), allocatable  :: compdof(:)

    !> @brief Name of the sample netCDF file written by this example.
    character(strLen)  :: fileName

    contains

      !> @brief Initialize MPI, ParallelIO, and example data.
      !! Initialize the MPI and ParallelIO libraries. Also allocate
      !! memory to write and read the sample data to the netCDF file.
      procedure,  public  :: init

      !> @brief Create the decomposition for the example.
      !! This subroutine creates the decomposition for the example.
      procedure,  public  :: createDecomp

      !> @brief Create netCDF output file.
      !! This subroutine creates the netCDF output file for the example.
      procedure,  public  :: createFile

      !> @brief Define the netCDF metadata.
      !! This subroutine defines the netCDF dimension and variable used
      !! in the output file.
      procedure,  public  :: defineVar

      !> @brief Write the sample data to the output file.
      !! This subroutine writes the sample data array to the netCDF
      !! output file.
      procedure,  public  :: writeVar

      !> @brief Read the sample data from the output file.
      !! This subroutine reads the sample data array from the netCDF
      !! output file.
      procedure,  public  :: readVar

      !> @brief Close the netCDF output file.
      !! This subroutine closes the output file used by this example.
      procedure,  public  :: closeFile

      !> @brief Clean up resources.
      !! This subroutine cleans up resources used in the example. The
      !! ParallelIO and MPI libraries are finalized, and memory
      !! allocated in this example program is freed.
      procedure,  public  :: cleanUp

      !> @brief Handle errors.
      !! This subroutine is called if there is an error.
      procedure,  private :: errorHandle

    end type pioClass

contains

    !> @brief Initialize MPI, ParallelIO, and example data.
    !! Initialize the MPI and ParallelIO libraries. Also allocate
    !! memory to write and read the sample data to the netCDF file.
    subroutine init(this, rank, ntasks, start_idx, end_idx, array, dlen, fileName)

        implicit none

        class(pioClass),    intent(inout) :: this
        integer(i4b),       intent(in)    :: rank
        integer(i4b),       intent(in)    :: ntasks
        integer(i4b),       intent(in)    :: start_idx(:)
        integer(i4b),       intent(in)    :: end_idx(:)
        real(dp),           intent(in)    :: array(:)
        integer(i4b),       intent(in)    :: dlen      ! length of the array. this is then divided among MPI processes
        character(strLen),  intent(in)    :: fileName
        integer(i4b)                      :: i

        ! get MPI rank and tasks
        this%myRank = rank
        this%ntasks = ntasks

        ! set up PIO for rest of example
        this%niotasks      = this%ntasks ! keep things simple - 1 iotask per MPI process
        this%numAggregator = 0
        this%stride        = 1
        this%optBase       = 1

        !write(*,*) 'this%niotasks ',this%niotasks

        call PIO_init(this%myRank,            & ! input: MPI rank
                      MPI_COMM_WORLD,         & ! input: MPI communicator
                      this%niotasks,          & ! input: Number of iotasks (ntasks/stride)
                      this%numAggregator,     & ! input: number of aggregators to use
                      this%stride,            & ! input: stride
                      PIO_rearr_subset,       & ! input: do not use any form of rearrangement
                      this%pioIoSystem,       & ! output: iosystem
                      base=this%optBase)        ! input: offset the first io task - default base is task 1 (optional argument)

        !
        ! set up some data that we will write to a netcdf file

        this%iotype        = PIO_iotype_netcdf
        this%fileName      = fileName
        this%dimLen(1)     = dlen

        this%arrIdxPerPe = size(array)  !  number of elements per task

        if (this%arrIdxPerPe < 1) then
            call this%errorHandle("Not enough work to distribute among pes", ERR_CODE)
        endif

        this%ista = start_idx(rank+1)     ! start index
        this%isto = end_idx(rank+1)       ! end index

        allocate(this%compdof(this%ista:this%isto))
        allocate(this%dataBuffer(this%ista:this%isto))
        allocate(this%readBuffer(this%ista:this%isto))

        this%compdof(this%ista:this%isto) = (/(i, i=this%ista,this%isto, 1)/)     ! indice in terms of global arrays
        this%dataBuffer(this%ista:this%isto) = array
        this%readBuffer(this%ista:this%isto) = 0                                  ! array to be read in

    end subroutine init

    subroutine createDecomp(this)

      implicit none

      class(pioClass), intent(inout) :: this
      integer(PIO_OFFSET_KIND)       :: start(1)
      integer(PIO_OFFSET_KIND)       :: count(1)

      start(1) = this%ista
      count(1) = this%arrIdxPerPe

      call PIO_initdecomp(this%pioIoSystem, PIO_double, this%dimLen, this%compdof(this%ista:this%isto), &
          this%iodescNCells)

    end subroutine createDecomp

    subroutine createFile(this)

      implicit none

      class(pioClass), intent(inout) :: this
      integer(i4b)                   :: retVal

      retVal = PIO_createfile(this%pioIoSystem, this%pioFileDesc, this%iotype, trim(this%fileName), PIO_clobber)
      call this%errorHandle("Could not create "//trim(this%fileName), retVal)

    end subroutine createFile

    subroutine defineVar(this)

      implicit none

      class(pioClass), intent(inout) :: this
      integer(i4b)                   :: retVal

      retVal = PIO_def_dim(this%pioFileDesc, 'x', this%dimLen(1) , this%pioDimId)
      call this%errorHandle("Could not define dimension x", retVal)

      retVal = PIO_def_var(this%pioFileDesc, 'foo', PIO_double, (/this%pioDimId/), this%pioVar)
      call this%errorHandle("Could not define variable foo", retVal)

      retVal = PIO_enddef(this%pioFileDesc)
      call this%errorHandle("Could not end define mode", retVal)

    end subroutine defineVar

    subroutine writeVar(this)

      implicit none

      class(pioClass), intent(inout) :: this
      integer(i4b)                   :: retVal

      call PIO_write_darray(this%pioFileDesc, this%pioVar, this%iodescNCells, this%dataBuffer(this%ista:this%isto), retVal)
      call this%errorHandle("Could not write foo", retVal)
      call PIO_syncfile(this%pioFileDesc)

    end subroutine writeVar

    subroutine readVar(this)

      implicit none

      class(pioClass), intent(inout) :: this
      integer(i4b)                   :: retVal

      call PIO_read_darray(this%pioFileDesc, this%pioVar, this%iodescNCells,  this%readBuffer, retVal)
      call this%errorHandle("Could not read foo", retVal)

    end subroutine readVar

    subroutine closeFile(this)

      implicit none

      class(pioClass), intent(inout) :: this

      call PIO_closefile(this%pioFileDesc)

    end subroutine closeFile

    subroutine cleanUp(this)

      implicit none

      class(pioClass), intent(inout) :: this
      integer(i4b)                   :: ierr

      deallocate(this%compdof)
      deallocate(this%dataBuffer)
      deallocate(this%readBuffer)

      call PIO_freedecomp(this%pioIoSystem, this%iodescNCells)
      call PIO_finalize(this%pioIoSystem, ierr)
      call MPI_Finalize(ierr)

    end subroutine cleanUp

    subroutine errorHandle(this, errMsg, retVal)

      implicit none

      class(pioClass),  intent(inout) :: this
      character(len=*), intent(in)    :: errMsg
      integer(i4b),     intent(inout) :: retVal

      if (retVal .ne. PIO_NOERR) then
          write(*,*) retVal,errMsg
          call PIO_closefile(this%pioFileDesc)
          call mpi_abort(MPI_COMM_WORLD,0,retVal)
      end if

    end subroutine errorHandle

end module pio_module
