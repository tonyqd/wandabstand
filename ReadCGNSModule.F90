module ReadCGNSModule

  use global
  use parallel

  implicit none

  private

  public ReadGridMetaData

  character(len=32)                 :: ZoneName
  integer,dimension(3,3)            :: ISize
  integer                           :: CgnsZone

contains

  subroutine ReadGridMetaData()

  ! read all the necessary grid information from cgns file

    implicit none
    include 'cgnslib_f.h'

    integer                         :: ier
    integer                         :: i,j,k
    integer                         :: errorcode


    call cg_open_f(CGNSFile, MODE_MODIFY, CgnsIndex, ier)
    if ( ier /=0 )then
      write(*,*) 'ERROR! can not open CGNS file!!'
      call MPI_ABORT(MPI_COMM_WORLD, errorcode, ier)
    end if

    !check the CGNS file information
    call cg_nbases_f(CgnsIndex, nBases, ier)
    if ( nBases == 1) then
        CgnsBase = 1
    else
        write(*,*) 'There are more than one Base, be careful only the first Base is proceeded!'
        CgnsBase = 1
    end if

    call cg_nzones_f(CgnsIndex, CgnsBase, nZones, ier)
    if (nZones <= 0) then
        write(*,*) 'the number of zones must be greater or equal than 1 '
    end if

    !Read the size information for all zones
    allocate(SizeInformation(nZones,3,3))
    do CgnsZone = 1, nZones
        call cg_zone_read_f(CgnsIndex, CgnsBase, CgnsZone, ZoneName, ISize, ier)
        do i = 1, 3
            do j = 1,3
                SizeInformation(CgnsZone,i,j) = ISize(i,j)
            end do
        end do

    end do

    !If inlet/outlet information are specified from Input.txt, then read them from Input.txt
    If(InletInput == .true.) then
        call GetInletSize(12, InletFile, InletSize, InletFormat)
        allocate(InletBlockNum(InletSize))
        allocate(InletRange(InletSize,6))
        if(InletFormat == 1) then
            !call GetInletInformation1(13, InletFile, InletSize, InletBlockNum, InletRange)
        else if(InletFormat == 2) then
            !call GetInletInformation2(13, InletFile, InletSize, InletBlockNum, InletRange,nZones,SizeInformation)
        end if
    end if

  end subroutine ReadGridMetaData


  Subroutine GetInletSize(ioo, InletFile, InletSize, InletFormat)

  !The first line of the Inlet.txt should be an explanation and shouldn be counted in!

    implicit none

    integer, intent(in)             :: ioo
    integer, intent(out)            :: InletSize ! The number of Inlet-Outlet Blocks
    integer                         :: ierr, n_lines
    integer, intent(out)            :: InletFormat
    character(len=*), intent(in)  :: InletFile
    character(len=256)              :: dummyvariable, line

    ! open input file for reading
    open(unit=ioo, file=InletFile, iostat=ierr, FORM='FORMATTED')
    if( ierr /= 0 )then
      write(*,*) 'ERROR ! could not open InletInformation file! '
    end if

    ! determine number of lines in input file
    ! by reading line-by-line and waiting for error to occur when EOF is reached
    n_lines = 2
    ierr = 0

    read(ioo,*)
    read(ioo,*) InletFormat
    do while( ierr == 0 )
      read( ioo, *, iostat=ierr ) dummyvariable
      if( ierr == 0 ) n_lines = n_lines + 1
    end do

    if (InletFormat == 1) then
        InletSize = (n_lines-2)/3
    else if (InletFormat == 2) then
        InletSize = (n_lines-2)
    else
        write(*,*) 'ERROR ! No Valid Option for Inlet/Outlet Information Format!!!'
        stop
    end if
    close(ioo)

  end subroutine GetInletSize

end module ReadCGNSModule
