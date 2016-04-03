module ConfigurationModule

  use global

  implicit none

  public

  include 'mpif.h'

contains

  subroutine ReadConfigurationFile()

  ! initialize configuration
  !
  ! Read all the input from parameters.txt

    implicit none

    logical                         :: file_found
    integer                         :: ierr, errorcode


    !---- open global configuration file

    inquire( file=file_config, exist=file_found )
    if( file_found )then
      open( newunit=io_config, file=file_config, status='old', iostat=ierr )
    end if

    if( (.not. file_found) .or. (ierr /=0) )then
      close( io_config )
      print *,"ERROR ! global control file "//trim(file_config)//" not found or error while opening!"
      call MPI_ABORT(MPI_COMM_WORLD, errorcode, ierr)
    end if

    !---- read global configuration namelist

    rewind( io_config )
    read( io_config, nml=MAIN, iostat=ierr )
    if( ierr /= 0 )then
      close( io_config )
      print *, "ERROR ! global control file "//trim(file_config)//" failed to read for namelist main!"
      call MPI_ABORT(MPI_COMM_WORLD, errorcode, ierr)
    end if

    !---- if multiple processors involved, read metis information
    if ( ParallelMode .eq. .true. ) then
      rewind ( io_config )
      read ( io_config, nml=parallel, iostat=ierr )
      if ( ierr /= 0 )then
        close( io_config )
        print *, "ERROR ! global control file "//trim(file_config)//" failed to read for namelist parallel!"
        call MPI_ABORT(MPI_COMM_WORLD, errorcode, ierr)
      end if
    end if
    !---- if multiple processors involved, read metis information
    if ( SearchMethod .eq. 2) then
      rewind( io_config )
      read( io_config, nml=quasitree, iostat=ierr )
      if( ierr /= 0 )then
        close( io_config )
        print *, "ERROR ! global control file "//trim(file_config)//" failed to read for namelist quasitree!"
        call MPI_ABORT(MPI_COMM_WORLD, errorcode, ierr)
      end if
    end if

  end subroutine ReadConfigurationFile


end module ConfigurationModule
