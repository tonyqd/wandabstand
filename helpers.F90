module os
  use parallel

  implicit none

contains

  subroutine create_folder ( folder )
  !
  ! let one process within a given communicator create a folder,
  ! specified by its absolute path or relative to the current working directory
  !
  ! note: this is a collective operation within the specified communicator!
  ! create_folder NEEDS to be called by EVERY process within the specified communicator,
  ! however only the MASTER process will actually perform the work.
  !
  ! an MPI_BARRIER ensures that the folder is present before the simulation continues
  !
    implicit none

    character(len=*)   , intent(in) :: folder
    integer                         :: master, communicator

    ! determine the master process (default: MPI_MASTER)
    master = MPI_MASTER

    ! determine the communicator (default: MPI_COMM_WORLD)
    communicator = MPI_COMM_WORLD

    if( MPI_RANK .eq. master )then
      call create_folder_bare( folder )
    end if

    ! MPI BARRIER such that folder is created before continuation
    call MPI_BARRIER(communicator, MPI_IERR)

  end subroutine create_folder

  subroutine create_folder_bare ( folder )
  !
  ! without any safeguarding, create a folder, specified by its absolute path
  ! or relative to the current working directory
  !
  ! use this function ONLY if you can assure that in a parallel environment,
  ! other process don't rely on this folder before the calling process actually created it
  ! (i.e. utilize an MPI_BARRIER outside of this function)
  !
  ! (a safer version is create_folder)
  !
    implicit none

    character(len=*)   , intent(in) :: folder

    logical                         :: io_found

    inquire( file=trim(folder)//"/.", exist=io_found )
    if( .not. io_found )then
      write( io_log, fmt_std ) 'SYS', "creating folder "//trim(folder)

      call system('mkdir -p '//trim(folder) )
    end if

  end subroutine create_folder_bare

end module os
