!
! File is Kernel.f90
!
! This   controls all calls to the FORTRAN model
! -----------------------------------------------------------------------------------
!
!      WaterSimDCDC Regional Water Demand and Supply Model Version 5.0

!       This is the Fortran code for the WaterSim_DCDC FORTRAN dll.

!       Copyright (C) 2014 , The Arizona Board of Regents
!              on behalf of Arizona State University

!       All rights reserved.

!       Developed by the Decision Center for a Desert City
!       Lead Model Development - David A. Sampson <david.a.sampson@asu.edu>

!       This program is free software: you can redistribute it and/or modify
!       it under the terms of the GNU General Public License version 3 as published by
!       the Free Software Foundation.

!       This program is distributed in the hope that it will be useful,
!       but WITHOUT ANY WARRANTY; without even the implied warranty of
!       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!       GNU General Public License for more details.

!       You should have received a copy of the GNU General Public License
!       along with this program.  If not, please see <http:!www.gnu.org/licenses/>.
!
!====================================================================================
!

! Module:       Module lm_Initialize
! Subroutines:  subroutine InitK()
!               calls:
!               call global()
!               call initializeModel()
!               call initializeMeteo()
!               call initializeSaltTontoVerde()
!               call initializeVerde()
!               call initializeSaltTonto()
!               call initializeColorado()
!               call initializeGWaterData()
!               call initializeAgriculture()
!               call initializeSTVdesignations()
!               call initializeDailyDesig()
!               call initializeColoradoDesig()
!               call  initializePopAndDemand()
!

! Module:       Module lm_Kernel
! Subroutines:  subroutine RunOneYearKernel()
!                 calls (also present):
!                   call upStreamK(T,year)
!                   call sKernalCode(T,acft,gvi,lvl_flag,lvl_Interrupt)
!                   call sKernalCode_2(T,acft,gvi,lvl_flag,lvl_Interrupt)
!                    both call
!                       call pGlobalDemand(T)                                
!                       call pCityParameterControl_a(T)
!                       call sFlowsSaltTontoVerde(T)       
!                       call pFlowsReservoirsColorado(T,acft,Bout)               
!                       call pInitGroundwater(T)       
!                       call pDemand(T,gvi)      
!                       call pRainWaterHarvest(T,gvi,lvl_flag)               
!                       call pWaterForVadose_s(T,gvi,lvl_flag)        
!                       call pWaterForDirectInject_s(T,gvi,lvl_flag) 
!                       call pWaterReclaimed_s(T,gvi)              
!                       call pROWaterReclaimed_s(T,gvi)           
!                       call pProviderGroundwater(T,gvi,vPV)                       
!                       call pWaterBanking_k(T,gvi,lvl_Interrupt)           
!                       call pUnmetDemand(T,gvi,lvl_flag,lvl_Interrupt)
!                       call pCitiModel(T,vPV)
!                       call pSRPnewRelease(T,Aout,vPV)                         
!                   call downStreamK(T,year)

!               subroutine daysintheyearK(year,T)
!               subroutine dateTime()
!               subroutine startupK()
!               subroutine teardownK()
!
! Module: lm_Start
!  subroutine:  OpenFilesK()
! Module: lm_Terminate
!  subroutine:  CloseFilesK()

! created on 08.12.10
!
! david arthur sampson

! last write was: 01.24.14,06.09.14,07.20.14,07.30.14
! ---------------------------------------------------
! 

! =================================================================================================
!
Module lm_Initialize
 use gm_ModelControl
    !
  use gm_GlobalData
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
 contains
    !
    ! ----------------
    subroutine InitK()
        !
        gvl_errorFlag=.true.
        !
        !if(gvl_readStatus)then
          call global()
           call initializeModel()
            call initializeMeteo()
             call initializeLULC()
              call initializeSaltTontoVerde()
               call initializeVerde()
                call initializeSaltTonto()
                 call initializeColorado()
                  call initializeGWaterData()
                   call initializeAgriculture()
                  call initializeSTVdesignations()
                call initializeDailyDesig()
              call initializeColoradoDesig()
            call initializePopAndDemand()
       ! endif
      return
    end subroutine InitK        
    ! ------------------

    ! -----------------
    subroutine global()
        !    
        if(gpl_release)then

        else
          gvl_writeLog=.true.
        endif
        !
        call openRelease()
        !
      return
    end subroutine global
    ! -------------------
    subroutine openRelease()
        ! --- Types -------------------------------
        character(len=16) :: lvc_outputPath='WaterSim_Output/'
        ! =========================================
        !
        if(gpl_writeLog)then

                if(gpl_release)then
                else
                 gvl_writeLog=.true.
                endif
                !
                 open(7,FILE=(lvc_outputPath//'Log.txt'), status='replace')
                 write(7,*)"Initialize (startupK - in Kernel.f90):"
                !
                if(gpl_writeProviderDotTxt)then
                  open(4,FILE=(trim(lvc_outputPath)//'Provider.txt'), status='replace')
                endif   
                ! ==============================================================================
                if(gpl_1)then
                 open(1,FILE=(trim(lvc_outputPath)//'One.txt'), status='replace')        
                endif
                if(gpl_103)then
                  open(103,FILE=(trim(lvc_outputPath)//'OneHundredThree.txt'), status='replace')
                endif
                if(gpl_104)then
                 open(104,FILE=(trim(lvc_outputPath)//'OneHundredFour.txt'), status='replace')        
                endif
                if(gpl_105)then
                 open(105,FILE=(trim(lvc_outputPath)//'OneHundredFive.txt'), status='replace')                        
                endif
                if(gpl_106)then
                 open(106,FILE=(trim(lvc_outputPath)//'OneHundredSix.txt'), status='replace')                        
                endif
                if(gpl_107)then
                 open(107,FILE=(trim(lvc_outputPath)//'OneHundredSeven.txt'), status='replace')                        
                endif
                if(gpl_110)then
                 open(110,FILE=(trim(lvc_outputPath)//'OneHundredTen.txt'), status='replace')                        
                endif

        endif
     return
    end subroutine openRelease
       !
    subroutine CloseK()
     integer :: k
        if(gpl_writeLog)then
          close(7)
          do k = 100,110,1
            close(k)
          end do
        endif
     return
    end subroutine CloseK
End Module lm_Initialize
!
Module lm_Kernel
  use gm_GlobalData
   use lm_BuildNotes
    use gm_ModelControl
     use gm_TypeControl
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
 contains
    ! ---------------------------
    subroutine RunOneYearKernel()
        !
        ! --------- Types ------------------------
        integer :: acft=0,year=2000
        integer(1) :: gvi=1

        logical :: lvl_flag(gvi_maxProV)=.false.
        logical :: lvl_Interrupt=.false.
        logical :: yes
        ! =======================================-

        !--- TYPE constructs ---
        type(runTime)T
        ! ======================
        ! new code testing
        !T%Policy%harvesting=2017
            !
            if(.not. gvl_errorFlag)then
              gvl_writeError=.true.
              goto 1000                            ! Error in finding files or reading files: abort
            endif
            !
            call upStreamK(T,year)                                      ! Set start,end, and run variables
             call sKernalCode(T,acft,gvi,lvl_flag,lvl_Interrupt)
            call downStreamK(T,year)                                    ! Local- close files, tidy up
            gvi=1
            !
          return
1000    continue
            ! 03.03.15
            if(gvl_writeError)then
              if(gvl_writeLog)then
                string=1
                LU=0
                INQUIRE(unit=7,OPENED=Yes)
                 if(.NOT. yes)open(7,FILE="ErrorLog.txt", status='unknown')
                 ! Both calls are In "Strings.f90"
                 call sStrings(string,errorString)
                 call eWrite(errorString,LU)
                close(7)
                !
              endif
            endif
            !
    end subroutine RunOneYearKernel
    ! -----------------------------



    ! ---------------------------
    subroutine RunManyYearKernel()
     return
    end subroutine RunManyYearKernel
    ! ------------------------------

    ! -------------------------------------------------------------
    subroutine sKernalCode(T,acft,gvi,lvl_flag,lvl_Interrupt)

         ! --------- Types ----------------------------
         integer :: acft
         integer(1) :: gvi
         logical :: lvl_flag(gvi_maxProV),lvl_Interrupt
         ! ============================================

         !------ TYPE constructs -----
         type(runTime)T
         type(RiverA)Aout
         type(RiverB)Bout
         type(Provider)vPV
         type(Surfacewater)Sout
         ! ===========================
             !
             call pStartUp_k(T)
              call pCityParameterControl_a(T)                       ! Initialize all parameters/variables
               call pGlobalDemand(T)                                ! calls calcDemand(T)
                call pSaltTontoVerde(T)                             ! WaterShed_STV.f90
                  call pFlowsReservoirsColorado(T,acft,Bout)        ! WaterShed_CO.f90
                   call pInitGroundwater(T)                         ! WaterShed.f90    
                    call pDemand(T,gvi)                             ! ProviderPopulationandDemand.f90
                     call pWaterForVadose_s(T,gvi,lvl_flag)        ! Demand.f90
                      call pWaterForDirectInject_s(T,gvi,lvl_flag) ! Demand.f90
                        call pRainWaterHarvest(T,gvi,lvl_flag)               ! Water To 
                         call pStormWaterHarvest(T,gvi,lvl_flag)
                          call pGrayWater(T,gvi,lvl_flag)
                           call pWaterReclaimed_s(T,gvi)               ! Demand.f90
                            call pROWaterReclaimed_s(T,gvi)            ! Demand.f90
                           call pProviderDefaultGroundwater(T,gvi)
                          call pSRP_newClassA(T,gvi,Aout)           ! SRPwaterOperations.f90 line 334
                         call pSRP_newClassBC(T,gvi,Aout,vPV)     ! SRPwaterOperations.f90 line 582
                        call pCAP_water(T,gvi,Bout%bCAP_maf,vPV)  ! Designations_CO.f90 line 441
                       call pSRP_newNCS (T,gvi,Aout)               ! SRPnewRelease.f90 line 390
                      call pNewWaterSupplies_k(T,gvi)             ! Demand.f90
                     call pWaterBanking_k(T,gvi,lvl_Interrupt)    ! Demand.f90
                    call pProviderGroundwater(T,gvi,vPV)          ! Groundwater.f90 line 413

                   call pUnmetDemand(T,gvi,lvl_flag,lvl_Interrupt) ! ProviderPopulationandDemand.f90
                  call pUnusedNonPotable(T,gvi)
                 call pDesignate(T,Aout,Sout)                      ! WaterShed.f90

                call pWaterProviders(T,Sout,vPV)                  ! Provider.f90

               call pCitiModel(T,vPV)                             ! Water_CityModel.f90
              call pSRPnewRelease(T,Aout)                          ! SRPnewRelease.f90
             call pCleanUp_k(T)
             !
        !
      return
    end subroutine sKernalCode
    ! ---------------------------
    
     ! ---------------------------
     subroutine upStreamK(T,year)
      use gm_ModelControl
        !
        ! ---------- Types --------
        integer :: year,gv_endyear
        integer :: run
        real :: totalYears
        real :: lvf_diffYears
        real,parameter :: lpf_finite=0.01
        ! =========================
        !

        ! -- Type Constructs --
        type(runTime)T
        ! =====================
            !
            gv_startyear=gpi_start    ! default
            ! Override my default start year from the interface
           if(0 < gvi_startSimulationYear)then
             gv_startyear=gvi_startSimulationYear            
           else
            gvi_startSimulationYear=gv_startyear
           endif
           !
            gv_endyear=gpi_end
           if(0 < gvi_endSimulationYear)then
            gv_endyear=gvi_endSimulationYear
           else
            gvi_endSimulationYear=gv_endyear
           endif
           gv_simduration=(gv_endyear-gv_startyear)
           !
           T%startyear=gv_startyear
           T%endyear= gv_endyear
           !
           T%simyear=gv_simyear
           T%jumpYear=gv_simyear + 1
           !
          if(gv_simduration == 0)gv_simduration=30
           if(year ==0 .or. year == gv_startyear .or. year > gv_startyear+(gv_simduration-1) .or. T%simyear==0)then
           if(gv_startyear ==0)then
              year=gpi_start
              gv_startyear=year
              T%simyear=0
            else
              year=gv_startyear
            endif
            if(year == gv_startyear)then
             T%startyear=gv_startyear
             T%year=T%startyear
             T%month=1
             T%simyear=0
             !T%policyYear=0
             gv_simyear=0
             gvl_start=.true.
             T%atStartOfSimulation=.true.
             !
             call ClearInclude(T)
             !
              call dateTime()
             !
            endif
          else
          endif  
          !
           run=gv_simduration
           T%simulations=run
          if(run == 0)T%simulations=40
          T%simulations=T%endyear-T%startyear

          T%year=year
          call daysintheyearK(year,T)
          !
            !lvf_simYear=T%simyear
            totalYears=T%simulations
           ! lvf_diffYears=T%endyear-gvi_baseYear
          lvf_diffYears = max(0,(year - gvi_baseYear))
            T%propYears_2040=0
            T%propYears=0
           T%propYears_2040= 1-(lvf_diffYears/(2040-gvi_baseYear))
           T%propYears= 1-(lvf_diffYears/(totalYears-(gvi_baseYear- T%startyear )))
           !
           if(T%propYears_2040 < lpf_finite)T%propYears_2040=0
           if(T%propYears < lpf_finite)T%propYears=0
          !
        !
           T%policyYear=0
          T%policyYear=max(0,T%year-gvi_baseYear+1)
          T%lcluYear=max(0,(T%year-gpi_lclu)+1)
        !
     return
    end subroutine upstreamK
    !-----------------------

    ! --------------------
    subroutine dateTime()
        !
        if(gvl_writeLog)then
         LU=7
          ! In GlobalSubroutines.f90
          call sDateTimeBuild(LU)
          write(7,*)"Simulation start: upstream in kernel"
          write(7,*)" Once Through Process:"
        endif
        !
      return
    end subroutine dateTime
    ! --------------------------
    
    ! ----------------------------
    subroutine downStreamK(T,year)
     use gm_ModelControl
     !
        ! --- Types ---
        integer :: year
        ! =============
        !

        ! - Type Construct -
        type(runTime)T
        ! ==================
        !    
            year=year+1
            T%year=year
            gv_simyear=gv_simyear+1
            T%simyear=gv_simyear
            
            gvl_start=.false.
            T%atStartOfSimulation=.false.
            !
            if(year == gv_startyear+1)then 
              if(gvl_writeLog)then
                write(7,*)"Simulation end: downstream"
                write(7,*)"----------------------------------------"
                write(7,*)"Run-time Errors (if present) as follows:"
                write(7,*) " "
              endif
              !
              if(.not. gpl_release)gvl_WriteDisk=.true.
              !
            else if(year == T%endyear)then 
              year=gv_startyear 
              if(0 < gvi_startSimulationYear)year=gvi_startSimulationYear
            else
            endif
            !
       !
     return
    end subroutine downstreamK
    !-------------------------
   
    ! ----------------------------
    function int2char(i) result(c)
      integer, intent(in) :: i
      character(len=10) :: c
      write(c,'(i0)')i
    end function int2char
    ! -------------------

    ! -------------------
    subroutine startupK()
      use gm_GlobalData
        !
        ! --- Types -------------------------------
        character(len=15) :: lvc_outputPath='WaterSim_Output/'
        ! =========================================
        !
            if(gpl_writeLog)then
              gvl_writeLog=.true.
                if(gpl_release)then
                else
                 gvl_writeLog=.true.
                endif
                !

                 open(7,FILE=(lvc_outputPath//'Log.txt'), status='replace')
                 write(7,*)"Initialize (startupK - in Kernel.f90):"
                !
                if(gpl_writeProviderDotTxt)then
                  open(4,FILE=(trim(lvc_outputPath)//'Provider.txt'), status='replace')
                endif   
                ! ==============================================================================
                if(gpl_1)then
                 open(1,FILE=(lvc_outputPath//'One.txt'), status='replace')        
                endif
                if(gpl_103)then
                  open(103,FILE=(trim(lvc_outputPath)//'OneHundredThree.txt'), status='replace')
                endif
                if(gpl_104)then
                 open(104,FILE=(trim(lvc_outputPath)//'OneHundredFour.txt'), status='replace')        
                endif
                if(gpl_105)then
                 open(105,FILE=(trim(lvc_outputPath)//'OneHundredFive.txt'), status='replace')                        
                endif
                if(gpl_106)then
                 open(106,FILE=(trim(lvc_outputPath)//'OneHundredSix.txt'), status='replace')                        
                endif
                if(gpl_107)then
                 open(107,FILE=(trim(lvc_outputPath)//'OneHundredSeven.txt'), status='replace')                        
                endif
                if(gpl_108)then
                 open(108,FILE=(trim(lvc_outputPath)//'OneHundredEight.txt'), status='replace')                        
                endif
                if(gpl_109)then
                 open(109,FILE=(trim(lvc_outputPath)//'OneHundredNine.txt'), status='replace')                        
                endif
                if(gpl_110)then
                 open(110,FILE=(trim(lvc_outputPath)//'OneHundredTen.txt'), status='replace')                        
                endif
                ! ===============================================================================
                if(gpl_comparisons)then
                 open(100,FILE=(trim(lvc_outputPath)//'Colorado.txt'), status='replace')        
                 open(101,FILE=(trim(lvc_outputPath)//'Verde.txt'), status='replace')        
                 open(102,FILE=(trim(lvc_outputPath)//'SaltTonto.txt'), status='replace')        
                 !
                endif
             !
                if(gpl_testing2016)then
                  open(103,FILE=(trim(lvc_outputPath)//'OneHundredThree.txt'), status='replace')
                  open(104,FILE=(trim(lvc_outputPath)//'OneHundredFour.txt'), status='replace')   
                  open(105,FILE=(trim(lvc_outputPath)//'OneHundredFive.txt'), status='replace')           
                  open(106,FILE=(trim(lvc_outputPath)//'OneHundredSix.txt'), status='replace')           
                  open(107,FILE=(trim(lvc_outputPath)//'OneHundredSeven.txt'), status='replace')           
                  open(108,FILE=(trim(lvc_outputPath)//'OneHundredEight.txt'), status='replace')           
                  open(109,FILE=(trim(lvc_outputPath)//'OneHundredNine.txt'), status='replace')           
                  open(110,FILE=(trim(lvc_outputPath)//'OneHundredTen.txt'), status='replace')           
                endif
             !
            endif
         !
      return
    end subroutine startupK
    ! ---------------------

    ! ---------------------
    subroutine readFilesCheckK
     use gm_GlobalData
        !

        ! ------------------ Types -------------------------------
        integer :: length_path,length_fid

        character(len=50) :: file_
        character(len=200) :: lvc_dataPath='App_Data\WaterSim_6_0\*'
        logical :: checkFID
        ! =========================================================
        !
            gvl_readStatus=.true.
            checkFID=.false.
            !
            file_='App_Data\Data\PathCheck.txt'
            !
            if(gpl_release)then
             lvc_dataPath=trim(gvc_DPath)
            endif
            !
            length_path=index(lvc_dataPath,'*') -1
            length_fid=index(file_,' ') -1
            !
            inquire(File=trim(lvc_dataPath(1:length_path)//file_(1:length_fid)), exist=checkFID)
            if(checkFID)then
            else
               if(gpl_release)then
                 gvl_readStatus=.false.
               else
               endif
              ! return error code to the interface
            endif
            !
      return
    end subroutine readFilesCheckK
    ! ---------------------

    ! --------------------
    subroutine teardownK()
        !
        !--- Types --
        integer :: k
        ! ===========
        !
          !
          if(gpl_writeLog)then
            if(gvl_writeLog)then
               close(7)
              if(gpl_writeProviderDotTxt)then
               close(4)
              endif
              !
              if(gpl_comparisons)then
                do k = 100,102,1
                 close(k)
                end do
              endif
              !
              if(gpl_testing2016)then
                do k = 103,110,1
                 close(k)
                end do
              endif
              !
              if(gpl_1)then
                close(1)
              endif
              if(gpl_103)then
               close(103)
              endif
              if(gpl_104)then
               close(104)
              endif
              if(gpl_105)then
               close(105)
              endif
              if(gpl_106)then
               close(106)
              endif
              if(gpl_107)then
               close(107)
              endif
              if(gpl_108)then
               close(108)
              endif
              if(gpl_109)then
               close(109)
              endif
              if(gpl_110)then
               close(110)
              endif
              !
            endif
            !
          endif
        !
     return
    end subroutine teardownK
    ! ----------------------
!
    ! -----------------------
    subroutine testing(T)
        !
        ! --- Types ----
        
        ! ==============

        !--- TYPE constructs ---
        type(runTime)T
        ! ======================
        !
      
        !
     return 
    end subroutine testing
    ! --------------------
!   
End module lm_Kernel

  ! -------------------------------
    subroutine daysintheyearK(year,T)
     use gm_ModelControl
 
        implicit none
       ! -----------------

        ! --- Types ----
        integer :: year
        integer LPyear
        ! ==============

        !--- TYPE constructs ---
        type(runTime)T
        ! ======================
        !
            !
            T%days=365
            LPyear=(int((year+1)/4)*4-1)+1
            if(year == LPyear)T%days=366
            !
        !
       return
    end subroutine daysintheyearK
    ! ---------------------------
Module lm_Start
 use lm_Kernel
 contains
  subroutine OpenFilesK()
    call startupK()
   return
  end subroutine OpenFilesK
  subroutine ErrorCheckReadK()
    call readFilesCheckK()
   return
  end subroutine ErrorCheckReadK
End Module lm_Start
!
Module lm_Terminate
 use lm_Kernel
 contains
  subroutine CloseFilesK()
    call teardownK()
   return
  end subroutine CloseFilesK
End Module lm_Terminate
!
! ======================================================================================================
! E.O.F Kernel.f90

   subroutine test()
!     character(len=200) :: lvc_outputPath=' '
!      if(gpl_release)lvc_outputPath=gvc_outputPath
      !
!      if(.not. gv_start)open(102,FILE=trim(trim(lvc_outputPath)//'App_Data\Outputs\onehundredtwo.out'),status="replace")
!      gv_start=.true.
    end subroutine test
   subroutine outPutPath()
      use gm_GlobalData
        use lm_Kernel
        character(len=200) :: lvc_outputPath=' '
         lvc_outputPath=trim(gvc_OPath)
        close(103)
        open(103,FILE=trim(trim(lvc_outputPath)//'OneHundredThree.txt'), status='replace')        

     return
    end subroutine outPutPath

! ----------------------------------------------------------------------------------------
! E.O.F. really