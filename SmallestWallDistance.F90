program SmallestWallDistance

  ! use modules
  use global
  use parallel
  use ConfigurationModule
  use ReadCGNSModule
  use LogModule

  implicit none

  call InitializeParallelModule()

  call ReadConfigurationFile()

  call InitializeLogModule()

  call ReadGridMetaData()


write(*,*)                             CGNSFileType                                       ,&
                                       ParallelMode                                   ,&
                                       SearchMethod                                   ,&
                                       WallCalculationPoint                           ,&
                                       CellCalculationPoint                           ,&
                                       DistanceCalcMode                               ,&
                                       DistanceVectorMode                             ,&
                                       InletInput



  call DestroyLogModule()

  call DestroyParallelModule()


end program SmallestWallDistance
