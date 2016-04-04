module LogModule

  use global
  use parallel

  implicit none

  public

contains

  subroutine InitializeLogModule()

  !
  ! initialize the log files
  !
  ! create new log files and greet the user
  !
    use os

    implicit none

    character(len=36)               ::  filename

    integer, dimension(8)           :: date
    integer                         :: ierr, ier, errorcode


    !---- start parallel log output

    call date_and_time(VALUES=date)
    call create_folder( folder_log )

    !---- open log output file

    ! each process writes a separate log file to folder_log
    filename = ParallelFilename(trim(folder_log)//'/'//trim(file_par_log))
    open( newunit=io_log, file=filename, iostat=ierr, status="replace", action="write" )
    if ( ier /=0 )then
      write(*,*) 'ERROR! can not initialize log file!!'
      call MPI_ABORT(MPI_COMM_WORLD, errorcode, ier)
    end if

    write( io_log, fmt_std ) 'INFO', fmt_bar
    write( io_log, fmt_raw//"A,"//fmt_date_and_time//")" ) &
        '', 'INFO', 'starting new smallest wall distance calculation session on ', &
                     date(3), date(2), date(1), &
                     date(5), date(6)
    write( io_log, fmt_raw//"'proc ',"//fmt_proc//",' of ',"//fmt_proc//")" ) &
        '', 'INFO', MPI_RANK+1, MPI_SIZE

  end subroutine InitializeLogModule

  subroutine DestroyLogModule()
  !
  ! close log files
  !
  ! print farewell message and then close the log file for writing.
  !
    implicit none

    integer, dimension(8)           :: date

    call date_and_time(VALUES=date)

    write( io_log, fmt_std ) 'INFO', fmt_bar
    write( io_log, fmt_raw//"A,"//fmt_date_and_time//")" ) &
        '', 'INFO', 'shutting down smallest wall distance calculation session on ', &
                     date(3), date(2), date(1), &
                     date(5), date(6)

    close( io_log )

  end subroutine DestroyLogModule

end module LogModule
