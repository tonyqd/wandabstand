module global

  implicit none

  include 'cgnslib_f.h'

  public

  ! --- global parameters

  ! --- parameters which are read from file parameters.txt

  character(len=64)                 :: CGNSFile

  logical                           :: ParallelMode
  character(len=64)                 :: MetisFileName

  integer                           :: SearchMethod
  integer                           :: WallCalculationPoint
  integer                           :: CellCalculationPoint
  integer                           :: DistanceCalcMode
  integer                           :: DistanceVectorMode
  logical                           :: InletOption

  real(8)                           :: BlockLength


  character(len=*) , parameter      :: InputFile = 'parameters.txt'
  character(len=*) , parameter      :: InletFile = 'Inlet.txt'
  namelist /main/                   CGNSFile

end module global
