module global

  implicit none

  public

  include 'cgnslib_f.h'

  ! --- global parameters

  ! --- parameters which are read from file parameters.txt

  integer               , parameter :: n_proc_digits  = 4                             , & ! number of digits for processes
                                       n_block_digits = 6                             , & ! number of digits for blocks
                                       n_conn_digits  = 8                             , & ! number of digits for connections
                                       n_iter_digits  = 10                            , & ! number of digits for iterations
                                       n_file_max     = 10                                ! maximum allowed input cgns files

  integer                           :: io_config

  character(len=32)                 :: folder_log        = './logs'                    ! folder for concurrent log files, this is a namelist variable


  character(len=32)                 :: CGNSFileType
  character(len=256)                :: CGNSFiles(n_file_max)
  integer                           :: n_cgnsfiles

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

  character(len=*) , parameter      :: file_config = 'parameters.txt'                 , &
                                       InletFile = 'Inlet.txt'                        , &
                                       file_par_log      = 'WallDistance.out'              ! parallel log file

  namelist /MAIN/                      CGNSFileType                                   , &
                                       CGNSFiles                                      , &
                                       ParallelMode                                   , &
                                       SearchMethod                                   , &
                                       WallCalculationPoint                           , &
                                       CellCalculationPoint                           , &
                                       DistanceCalcMode                               , &
                                       DistanceVectorMode                             , &
                                       InletInput

  namelist /parallel/                  MetisFileName
  namelist /quasitree/                 BlockLength
  namelist /inlet/                     InletFormat

  !---- file specifiers

  integer                           :: io_log                                             ! file ID for parallel log output


  !---- formating codes
                                       ! formatting codes for parallel log file
  character(len=*) , parameter      :: fmt_raw   =     "(A7,'  ',A10,' | ',"          , & ! raw code: use for custom messages containing complex info
                                       fmt_err   = "('  ERROR  ',A10,' | ',A)"        , & ! tag message as error
                                       fmt_warn  = "('WARNING  ',A10,' | ',A)"        , & ! tag message as warning
                                       fmt_debug = "('  DEBUG  ',A10,' | ',A)"        , & ! tag message as debug info
                                       fmt_std   = "('         ',A10,' | ',A)"        , & ! normal message
                                       ! formatting codes for date & time
                                       fmt_date  = "I2.2,'.',I2.2,'.',I4.4"           , & ! DD.MM.YYYY
                                       fmt_time  = "I2.2,':',I2.2"                    , & ! HH:MM
                                       fmt_date_and_time = fmt_date//"' '"//fmt_time  , & ! DD.MM.YYYY HH:MM
                                       fmt_bar  = "==========================================================================="

                                       ! dynamic formatting codes (see ConfigurationModule for assignment)
  character(len=4)                  :: fmt_proc, fmt_block, fmt_conn, fmt_base            ! process, block, connection & cgns bases

  ! --- parameters related to the cgns file
  integer                           :: CgnsBase                                       , &
                                       CgnsCoord                                      , &
                                       CgnsConnectivity

  integer, allocatable, dimension(:):: nBases                                         , & ! total number of bases in each cgns file!
                                       nZones                                         , & ! total number of zones in each cgns file!
                                       CgnsIndex
  integer                           :: nZones_total                                       ! Total number of zones across all the cgns files


  ! --- Variables for storing size information for all zones
  integer,allocatable, &
                 dimension(:,:,:,:) :: SizeInformation
  integer,allocatable, &
                     dimension(:,:) :: n_bocos                                            ! Store the number of boundary conditions for every file and every zone
  integer,allocatable, &
                     dimension(:,:) :: InletBlockIndex1, InletBlockIndex2                 ! InletBlockIndex(:,2) 1: File index  2: Zone index
  integer,allocatable, &
                     dimension(:,:) :: InletRange1, InletRange2                           ! InletRange(:,6) 1-3 Pointrange Begin, 4-6 Pointrange End, for vertex number information!!!



end module global
