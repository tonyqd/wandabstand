module global

  implicit none

  public

  include 'cgnslib_f.h'

  ! --- global parameters

  ! --- parameters which are read from file parameters.txt

  integer                           :: io_config

  character(len=32)                 :: CGNSFile

  logical                           :: ParallelMode
  character(len=64)                 :: MetisFileName

  integer                           :: SearchMethod
  integer                           :: WallCalculationPoint
  integer                           :: CellCalculationPoint
  integer                           :: DistanceCalcMode
  integer                           :: DistanceVectorMode
  logical                           :: InletInput
  integer                           :: InletFormat

  real(8)                           :: BlockLength

  character(len=*) , parameter      :: file_config = 'parameters.txt'
  character(len=*) , parameter      :: InletFile = 'Inlet.txt'

  namelist /MAIN/                      CGNSFile                                       ,&
                                       ParallelMode                                   ,&
                                       SearchMethod                                   ,&
                                       WallCalculationPoint                           ,&
                                       CellCalculationPoint                           ,&
                                       DistanceCalcMode                               ,&
                                       DistanceVectorMode                             ,&
                                       InletInput

  namelist /parallel/                  MetisFileName
  namelist /quasitree/                 BlockLength
  namelist /inlet/                     InletFormat


  ! --- parameters related to the cgns file
  integer                           :: CgnsIndex                                      ,&
                                       CgnsBase                                       ,&
                                       CgnsCoord                                      ,&
                                       CgnsConnectivity

  integer                           :: nBases                                         ,& ! total number of bases in the cgns file
                                       nZones                                            ! total number of zones in BASE 1

  ! --- Inlet Information
  integer                           :: InletSize


  ! --- Variables for storing size information for all zones
  integer,allocatable, &
                   dimension(:,:,:) :: sizeInformation
  integer,allocatable, &
                       dimension(:) :: InletBlockNum
  integer,allocatable, &
                     dimension(:,:) :: InletRange                                         ! InletRange(:,6) 1-3 Pointrange Begin, 4-6 Pointrange End, for vertex number information!!!




end module global
