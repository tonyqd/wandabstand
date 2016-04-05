module ReadCGNSModule

  use global
  use parallel

  implicit none

  private

  public ReadGridMetaData

  !public cg_int

  character(len=32)                 :: ZoneName
  integer,dimension(3,3)            :: ISize
  integer                           :: CgnsZone

contains

  subroutine ReadGridMetaData()

  ! read all the necessary grid information from cgns file

    implicit none
    include 'cgnslib_f.h'

    integer             , parameter :: cg_int = kind(CG_null)
    integer                         :: ier
    integer                         :: i,j,k,l,m,n
    integer                         :: errorcode
    integer                         :: Fileindex                                , &
                                       temp_n_bocos

    character(len=32)               :: BocoFamily
    integer                         :: BocoType, BocoRange(3,2), BocoNormal(3)
    character(len=32)               :: BocoName
    integer                         :: ptset_type, NormalDataType, ndataset
    integer(cg_int)                 :: npnts, NormalListSize
    integer(cg_int), dimension(3,2) :: pnts, NormalList(1)

    logical                         :: found
    integer                         :: temp_counter
    integer                         :: boundarynum1, boundarynum2


    !---- allocate memory for nzones and nBases
    allocate(nBases(n_cgnsfiles))
    allocate(nZones(n_cgnsfiles))
    allocate(CgnsIndex(n_cgnsfiles))

    nZones_total = 0

    do Fileindex = 1, n_cgnsfiles
      call cg_open_f(CGNSFiles(Fileindex), MODE_MODIFY, CgnsIndex(Fileindex), ier)
      if ( ier /=0 )then
        write(*,*) 'ERROR! can not open CGNS file!!'
        call MPI_ABORT(MPI_COMM_WORLD, errorcode, ier)
      end if

      !check the CGNS file information
      call cg_nbases_f(CgnsIndex(Fileindex), nBases(Fileindex), ier)
      if ( nBases(Fileindex) == 1) then
          CgnsBase = 1
      else
          write(*,*) 'There are more than one Base, be careful only the first Base is proceeded!'
          CgnsBase = 1
      end if

      call cg_nzones_f(CgnsIndex(Fileindex), CgnsBase, nZones(Fileindex), ier)
      if (nZones(Fileindex) <= 0) then
          write(*,*) 'the number of zones must be greater or equal than 1 '
      end if

      nZones_total = nZones_total + nZones(Fileindex)
    end do

      !Read the size information for all zones in all cgns files
      allocate(n_bocos(n_cgnsfiles,nZones_total))
      allocate(SizeInformation(n_cgnsfiles,nZones_total,3,3))
      do i = 1, n_cgnsfiles
        do CgnsZone = 1, nZones(i)
          call cg_zone_read_f(CgnsIndex(i), CgnsBase, CgnsZone, ZoneName, ISize, ier)
          SizeInformation(i,CgnsZone,:,:) = ISize(:,:)
        end do
      end do

    ! Read the Inlet/Outlet boundary conditions directly from cgns file
    ! get the number of boundary conditions in each zone
    do i = 1, n_cgnsfiles
      do j = 1, nZones(i)
        call cg_nbocos_f( CgnsIndex(i), CgnsBase, j, temp_n_bocos, ier)
        if ( ier /=0 )then
          write(*,*) 'ERROR! can not read number of boundary conditions!!'
          call MPI_ABORT(MPI_COMM_WORLD, errorcode, ier)
        end if
        n_bocos(i,j) = temp_n_bocos
      end do
    end do

    !check the boundary type for each boundary
    allocate(InletBlockIndex1(nZones_total*6,2))
    allocate(InletRange1(nZones_total*6,6))
    temp_counter = 0
    do i = 1, n_cgnsfiles
      do j = 1, nZones(i)
        do k = 1, n_bocos(i,j)
          found = .false.
          call cg_boco_info_f(CgnsIndex(i), CgnsBase, j, k, BocoName, BocoType, &
               ptset_type, npnts, BocoNormal, NormalListSize, NormalDataType, ndataset, ier)

          if ( ier /=0 )then
            write(*,*) 'ERROR! can not read boundary condition information!!'
            call MPI_ABORT(MPI_COMM_WORLD, errorcode, ier)
          end if

          select case ( BocoType )
            case ( BCInflow, BCInflowSubsonic, BCInflowSupersonic )
              write( io_log, fmt_raw//'"recognized inlet boundary on file ",I0, " zone ", I0, " surface ", I0)') &
                    '','', i, j, k
              found = .true.
            case( BCOutflow, BCOutflowSubsonic, BCOutflowSupersonic )
              write( io_log, fmt_raw//'"recognized outlet boundary on file ",I0, " zone ", I0, " surface ", I0)') &
                    '','', i, j, k
              found = .true.
            case( BCGeneral )
              write( io_log, fmt_raw//'"recognized block interface on file ",I0, " zone ", I0, " surface ", I0)') &
                    '','', i, j, k
              found = .true.
          end select

          if ( found .eq. .true. ) then
            call cg_boco_read_f(CgnsIndex(i), CgnsBase, j, k, pnts, NormalList, ier)
            temp_counter = temp_counter +1
            InletBlockIndex1(temp_counter,1) = i
            InletBlockIndex1(temp_counter,2) = j
            InletRange1(temp_counter,1) = pnts(1,1)
            InletRange1(temp_counter,2) = pnts(2,1)
            InletRange1(temp_counter,3) = pnts(3,1)
            InletRange1(temp_counter,4) = pnts(1,2)
            InletRange1(temp_counter,5) = pnts(2,2)
            InletRange1(temp_counter,6) = pnts(3,2)
          end if
        end do
      end do
    end do

    boundarynum1 = temp_counter

    !If inlet/outlet information are specified from Input.txt, then read them from Input.txt
    If(InletInput == .true.) then
        call GetInletSize(12, InletFile, boundarynum2, InletFormat)
        allocate(InletBlockIndex2(boundarynum2,2))
        allocate(InletRange2(boundarynum2,6))
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
