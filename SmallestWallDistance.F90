program SmallestWallDistance

  ! use modules
  use global
  use parallel
  use ConfigurationModule
  use ReadCGNSModule

  implicit none

  call InitializeParallelModule()

  call ReadConfigurationFile()

  call ReadGridMetaData()


write(*,*)                             CGNSFile                                       ,&
                                       ParallelMode                                   ,&
                                       SearchMethod                                   ,&
                                       WallCalculationPoint                           ,&
                                       CellCalculationPoint                           ,&
                                       DistanceCalcMode                               ,&
                                       DistanceVectorMode                             ,&
                                       InletInput





  call DestroyParallelModule()


end program SmallestWallDistance
