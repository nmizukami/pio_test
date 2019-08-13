program main

USE mpi
USE nrtype
USE globalData,        ONLY: root, pid, nNodes
USE nr_utility_module, ONLY: arth
USE mpi_mod,           ONLY: shr_mpi_scatterV
USE pio_utils
use pio, only : pio_set_log_level

implicit none

! parameters
integer(i4b),       parameter :: ndim1=4000000
integer(i4b),       parameter :: ndim2=20
integer(i4b),       parameter :: iter=20
character(strLen),  parameter :: fileName='examplePio_f90.nc'

type(file_desc_t)             :: pioFileDesc       ! contains data identifying the file.
type(iosystem_desc_t)         :: pioSystem
type(io_desc_t)               :: iodesc1           ! io descriptor handle that is generated in PIO_initdecomp
!type(io_desc_t)               :: iodesc2           ! io descriptor handle that is generated in PIO_initdecomp

integer(i4b),allocatable      :: compdof1(:)
!integer(i4b),allocatable      :: compdof2(:)       ! Mapping of the storage order for the computational decomposition
integer(i4b)                  :: myid
integer(i4b)                  :: elem_per_proc
integer(i4b),allocatable      :: start_idx(:), end_idx(:)
integer(i4b),allocatable      :: num_per_task(:)
real(dp)                      :: array(ndim1)
real(dp),allocatable          :: array_local(:)
real(dp),allocatable          :: array_out_local(:)
real(dp),allocatable          :: array2d_local(:,:)
! real(dp),allocatable          :: array2d_global(:,:)
real(dp),allocatable          :: array2d_out_local(:,:)
integer(i4b)                  :: ix
!integer(i4b)                  :: jx,kx
!integer(i4b)                  :: ndim1_in
integer(i4b)                  :: ierr
character(len=strLen)         :: cmessage      ! error message
! timing
integer*8                     :: cr, startTime, endTime
real(dp)                      :: elapsedTime

!  Initialize MPI
call MPI_INIT(ierr)
!  Get the number of processes
call MPI_COMM_SIZE(MPI_COMM_WORLD, nNodes, ierr)
!  Get the individual process ID
call MPI_COMM_RANK(MPI_COMM_WORLD, pid, ierr)

call system_clock(count_rate=cr)

if (pid == 0) then
  ! program header
  write(*, '(a)' ) ''
  write(*, '(a,i2,2x,a)' )    'Proc:', pid, '  An MPI test program.'
  write(*, '(a,i2,2x,a,i8)' ) 'Proc:', pid, '  The number of MPI processes is ', nNodes
endif

! elements for each process
elem_per_proc = ndim1/nNodes

! starting and ending indices for local array per node and number of elements per node
allocate(start_idx(0:nNodes-1), end_idx(0:nNodes-1), num_per_task(0:nNodes-1))
do myid = 0, nNodes-1
  start_idx(myid) = myid*elem_per_proc + 1
  end_idx(myid)   = start_idx(myid) + elem_per_proc - 1
  if (myid == nNodes-1) end_idx(myid) = ndim1             ! adjust last index for last chunk
  num_per_task(myid) = end_idx(myid)-start_idx(myid)+1     ! actual size of array to be sent
enddo

! create mapping file between local to global
! case 1
! compdof1 = [start_idx(0),  ..., end_idx(0)]
!          = [start_idx(1),  ..., end_idx(1)]
!          = [start_idx(2),  ..., end_idx(2)]
!          .....
!          = [start_idx(m),  ..., end_idx(m)]
allocate(compdof1(ndim1))
compdof1 = arth(1, 1, ndim1)     ! indice in terms of global arrays

! case 2
! compdof2 = [0, 0, ..., 0, 10, 0, ..., 0]
!          = [0]
!   if (pid/=0) then
!    ndim1_in = 1
!   else
!    ndim1_in = ndim1
!   endif

!   allocate(compdof2(ndim1_in))

!   if (pid==0) then
!    do ix = 1, ndim1_in
!      compdof2(ix) = 0
!    end do
!    compdof2(10) = 10
!   else
!    compdof2 = 0
!   endif
!
!   if (pid==0) then
!    print*, (compdof2(jx),jx=1,ndim1)
!   endif

! Initialize netCDF with PIO
call init_nc()

! global array
! case 1
! create sequential array and scatter it to each node
array = arth(1.0_dp, 1.0_dp, ndim1)
call shr_mpi_scatterV(array, num_per_task, array_local, ierr, cmessage)

! spread 1D local array to 2D array in local
allocate(array2d_local(num_per_task(pid), ndim2))
array2d_local = spread(array_local,dim=2,ncopies=ndim2)


! case 2
!  ! global array that exist only on root and want to output a part of global array on root
!  if (pid==0) then
!    allocate(array2d_global(ndim1, ndim2))
!  else
!    allocate(array2d_global(1,ndim2))
!  endif

! iterate iter time and update array2d_local and array2d_global and output using pio
do ix = 1,iter

  ! 1D array
  ! compute something in parallel
  if (allocated(array_out_local)) deallocate(array_out_local)
  allocate(array_out_local(num_per_task(pid)))
  call power(array_local, num_per_task(pid), ix, array_out_local)

  ! 2D array
  ! compute something in parallel
  if (allocated(array2d_out_local)) deallocate(array2d_out_local)
  allocate(array2d_out_local(num_per_task(pid),ndim2))
  call power2d(array2d_local, num_per_task(pid), ndim2, ix, array2d_out_local)

  ! 2D array
  ! compute something in parallel
!   if (pid==0) then
!     array2d_global(10,:) = ix
!   endif

!  if (pid==0) then
!   print*, 'ix= ',ix
!   do kx = 1,ndim1_in
!     print*, (array2d_global(kx,jx),jx=1,ndim2)
!   enddo
!  endif

!  if (pid==1) then
!   print*, 'ix= ',ix
!   do kx = 1,num_per_task(pid)
!     print*, (array2d_out_local(kx,jx),jx=1,ndim2)
!   enddo
!  endif

  ! write variables
  call write_netcdf(pioSystem,        &
                    fileName,         & ! input:
                    'voo',            &  ! input: variable name
                    array,            &  ! input: variable data
                    [1,ix],           &  ! input: start index
                    [ndim1,1],        &  ! input: length of vector
                    ierr, cmessage)      ! output: error control


  ierr = pio_set_log_level(3)
!  call write_pnetcdf_recdim(pioSystem,          & ! input:
!                            fileName,           & ! input:
!                            'boo',              & ! input:
!                            array2d_global,     & ! input:
!                            iodesc2,            & ! input: ??? it is from initdecomp routine
!                            ix,                 & ! input: record dimension index
!                            ierr, cmessage)

  call write_pnetcdf_recdim(pioSystem,          & ! input:
                            fileName,           & ! input:
                            'foo',              & ! input:
                            array2d_out_local,  & ! input:
                            iodesc1,            & ! input: ??? it is from initdecomp routine
                            ix,                 & ! input: record dimension index
                            ierr, cmessage)

 call MPI_BARRIER(MPI_COMM_WORLD, ierr)

end do

!  Shut down MPI
call MPI_FINALIZE(ierr)

stop

contains

 subroutine init_nc()
   implicit none
   integer(i4b),       parameter :: recordDim=-999
   integer(i4b)                  :: dimId(3)

   ! Write array in each task with PIO
   call pio_sys_init(pid, nNodes,  & ! input
                     pioSystem)      ! output

   ! Initialize decomposition descriptor
call system_clock(startTime)
   call pio_decomp(pioSystem,                            & ! input: pio system descriptor
                   ncd_float,                            & ! input: data type (ncd_int, ncd_float, ncd_double, ncd_char)
                   [ndim1, ndim2],                       & ! input: dimension length
                   compdof1(start_idx(pid):end_idx(pid)),& ! input:
                   iodesc1)                                ! output:
call system_clock(endTime)
elapsedTime = real(endTime-startTime, kind(dp))/real(cr)
write(*,"(A,1PG15.7,A)") '   elapsed-time [pio_decomp] = ', elapsedTime, ' s'

!   call pio_decomp(pioSystem,                            & ! input: pio system descriptor
!                   ncd_float,                            & ! input: data type (ncd_int, ncd_float, ncd_double, ncd_char)
!                   [ndim1, ndim2],                       & ! input: dimension length
!                   compdof2,                             & ! input:
!                   iodesc2)                                ! output:

   ! Create netCDF
   call createFile(pioSystem,      &  ! input:
                   fileName,       &  ! input:
                   pioFileDesc,    &  ! output:
                   ierr, cmessage)

   ! Define dimensions
   call defdim(pioFileDesc, & ! input: file descriptor
               'x',         & ! input: dim name
               ndim1,       & ! input: dimsize
               dimId(1))

   call defdim(pioFileDesc, & ! input: file descriptor
               'y',         & ! input: dim name
               ndim2,       & ! input: dimsize
               dimId(2))

   call defdim(pioFileDesc, & ! input: file descriptor
               'time',      & ! input: dim name
               recordDim,   & ! input: dimsize
               dimId(3))

   ! Define variables
   call defVar(pioFileDesc,         & ! input: file descriptor
               'voo',               & ! input: variable name
               [dimId(1),dimId(3)], & ! input: dimension id(s)
               ncd_float,           & ! input: variable type.
               ierr, cmessage)        ! output: error code and message

!   call defVar(pioFileDesc,   & ! input: file descriptor
!               'boo',         & ! input: variable name
!               dimId,         & ! input: dimension id(s)
!               ncd_float,     & ! input: variable type.
!               ierr, cmessage)  ! output: error code and message

   call defVar(pioFileDesc,   & ! input: file descriptor
               'foo',         & ! input: variable name
               dimId,         & ! input: dimension id(s)
               ncd_float,     & ! input: variable type.
               ierr, cmessage)  ! output: error code and message

   call endDef(pioFileDesc, ierr, cmessage)

  end subroutine init_nc

end program main



subroutine power(array_in, ndim1, m, array_out)
  USE nrtype
  implicit none
  real(dp),     intent(in)  :: array_in(ndim1)
  integer(i4b), intent(in)  :: ndim1
  integer(i4b), intent(in)  :: m
  real(dp)                  :: array_out(ndim1)

  array_out = array_in*m

end subroutine


subroutine power2d(array_in, ndim1, ndim2, m, array_out)
  USE nrtype
  implicit none
  real(dp),     intent(in)  :: array_in(ndim1,ndim2)
  integer(i4b), intent(in)  :: ndim1
  integer(i4b), intent(in)  :: ndim2
  integer(i4b), intent(in)  :: m
  real(dp)                  :: array_out(ndim1,ndim2)

  array_out = array_in*m

end subroutine
