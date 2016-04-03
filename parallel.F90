module parallel

  use global

  implicit none

  include 'mpif.h'

  public :: MPI_RANK, MPI_SIZE, MPI_MASTER, MPI_IERR

  public :: InitializeParallelModule, DestroyParallelModule

  integer               , parameter :: MPI_MASTER = 0

  integer                           :: MPI_RANK, MPI_SIZE                             , & ! ID of current process and no. of processes in MPI env
                                       MPI_IERR                                           ! error code for calls to MPI infrastructure
  integer                           :: errorcode
contains

  subroutine InitializeParallelModule()
  !
  ! initialize parallel environment
  !
  ! properly set up MPI environment and determine
  ! needs to be called prior to everything else!
  !
    implicit none

    !---- initialize MPI environment

    call MPI_INIT( MPI_IERR )
    if( MPI_IERR /= MPI_SUCCESS ) then
      write(*,*) 'ERROR! MPI_INIT failed!'
      call MPI_ABORT(MPI_COMM_WORLD, errorcode, MPI_IERR)
    end if

    !---- determine MPI_RANK and MPI_SIZE

    call MPI_COMM_RANK( MPI_COMM_WORLD, MPI_RANK, MPI_IERR )
    if( MPI_IERR /= MPI_SUCCESS ) then
      write(*,*) 'ERROR! MPI_COMM_RANK failed!'
      call MPI_ABORT(MPI_COMM_WORLD, errorcode, MPI_IERR)
    end if

    call MPI_COMM_SIZE( MPI_COMM_WORLD, MPI_SIZE, MPI_IERR )
    if( MPI_IERR /= MPI_SUCCESS ) then
      write(*,*) 'ERROR! MPI_COMM_SIZE failed!'
      call MPI_ABORT(MPI_COMM_WORLD, errorcode, MPI_IERR)
    end if

  end subroutine InitializeParallelModule

  subroutine DestroyParallelModule()
  !
  ! shutdown parallel environment
  !
  ! properly shut down MPI environment (and possibly deallocate ressources)
  ! needs to be called at the very end !
  !
    implicit none

    call MPI_FINALIZE( MPI_IERR )
    if( MPI_IERR /= MPI_SUCCESS ) then
      write(*,*) 'ERROR! MPI_FINALIZE failed!'
      call MPI_ABORT(MPI_COMM_WORLD, errorcode, MPI_IERR)
    end if

  end subroutine DestroyParallelModule

end module parallel
