module globalData
  ! This module includes global data structures

  ! data types
  use nrtype

  implicit none

  save

  ! ---------- MPI/OMP variables -------------------------------------------------------------------

  integer(i4b),parameter        :: root=0              ! root node id
  integer(i4b)                  :: pid                 ! process id
  integer(i4b)                  :: nNodes              ! number of nodes

end module globalData
