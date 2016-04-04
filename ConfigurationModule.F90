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
    integer                         :: temp_filenum, temp_length
    integer                         :: i


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

    !---- check the number of cgns input files
    ! be careful, if the input cgns file name length should not be 256 characters. But this is normally also not the case.
    temp_filenum = 0
    do i = 1, size(CGNSFiles)
      if(len(trim(CGNSFiles(i))) .ne. 256)  temp_filenum = temp_filenum+1
    end do

    n_cgnsfiles = temp_filenum

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

    !---- if quasi-tree search method is chosen, the blocklength in quasi-tree search must be given
    if ( SearchMethod .eq. 2) then
      rewind( io_config )
      read( io_config, nml=quasitree, iostat=ierr )
      if( ierr /= 0 )then
        close( io_config )
        print *, "ERROR ! global control file "//trim(file_config)//" failed to read for namelist quasitree!"
        call MPI_ABORT(MPI_COMM_WORLD, errorcode, ierr)
      end if
    end if

    !---- if input is given, format must be chosen
    if ( InletInput .eq. .true.) then
      rewind( io_config )
      read( io_config, nml=inlet, iostat=ierr )
      if( ierr /= 0 )then
        close( io_config )
        print *, "ERROR ! global control file "//trim(file_config)//" failed to read for namelist inlet!"
        call MPI_ABORT(MPI_COMM_WORLD, errorcode, ierr)
      end if
    end if

    !---- initialize formattings

    write( fmt_proc,  '("I",I1,".",I1)') n_proc_digits,  n_proc_digits                    ! gives I4.4 (for 4 digits proc IDs)
    write( fmt_block, '("I",I1,".",I1)') n_block_digits, n_block_digits                   ! gives I6.6 (for 6 digits block IDs)

  end subroutine ReadConfigurationFile


end module ConfigurationModule
