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

  ! --- parameters related to the cgns file


end module global
