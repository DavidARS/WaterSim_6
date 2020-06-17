!
! File is ProviderPopulationandDemand.f90
!
! This file contains the provider level estimates of water demand and population
! ---------------------------------------------------------------------------------------
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
! Module:       Module lm_ProviderPopulation
! Subroutines:  subroutine initPop()
!                 calls (also present):
!                   call openFiles_pop()
!                   call readFiles_pop()

! No Module:    subroutine initializePopAndDemand()
!
! Module:       Module lms_ProviderPopandDemand
! Subroutines:  subroutine calcDemand(T)
!               subroutine readNewGPCD(i,lvf_GPCDavg)
!
! No Module:    subroutine readGPCDglobal(i,lvf_GPCDavg)
!               subroutine readProviderGPCD(i,lvf_GPCDavg)
!               function fOutDoorEstimate(pop,demand,days,bwProp)
!               subroutine pUnmetDemand(T,gvi_order,lvl_flag,lvl_Interrupt)
!               subroutine pDemand(T,gvi_order)
!               subroutine pGlobalDemand(T)
!               subroutine sClearDemand(T)
!               subroutine ProviderArea2012(vArea2012)
!               subroutine ProviderArea2012i(i,Relative)
!               subroutine ProviderAreaAg_2012(i,vArea2012)
!               subroutine ProviderAreaAg_2012r(i,Relative)
!
! Created on 06.09.10 
!
! Global OUTPUTS: 
!
! Local OUTPUTS:   No local outputs
!
! Local INPUTS:    Inputs are outputs
!
!
! Last write was: 06.09.14,07.18.14
! ---------------------------------
!

! ======================================================================================================
!
Module lm_ProviderPopulation
 use gm_ModelControl
  use gm_GlobalData
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
  contains
    !
    ! ---------------------
     subroutine initPop()
       call openFiles_pop()
       call readFiles_pop()
       call initialize()
      return
     end subroutine initPop
    ! ---------------------

      ! ------------------------
      subroutine openFiles_pop()
          !
          ! -------------- Types -------------
          character(len=200) :: lvc_DPath=' '
          ! ==================================
          !
                !
                if(gpl_release)then
                    lvc_DPath=trim(gvc_DPath)
                else
                    lvc_DPath=gvc_Path
                endif
              !
              module="lm_ProviderPopulation"
              !
               Infile='App_Data\Data\providerDemand_2085.txt'; LU=51
               call openFiles(module,lvc_DPath,Infile,LU)
              !
              ! Infile='App_Data\Data\gpcd_2010.txt'; LU=53
              Infile='App_Data\Data\gpcd_2013.txt'; LU=53
               call openFiles(module,lvc_DPath,Infile,LU)
              !
              ! Infile='App_Data\Data\OnProjectPopulation_2085.txt'; LU=54
               Infile='App_Data\Data\OnProjectApril_2085.txt'; LU=54

               call openFiles(module,lvc_DPath,Infile,LU)
              !
              ! Infile='App_Data\Data\OtherPopulation_2085.txt'; LU=55
               Infile='App_Data\Data\OtherProjectApril_2085.txt'; LU=55

               call openFiles(module,lvc_DPath,Infile,LU)
          !
        return
1000    continue
        !
         gvo_errorCode=1
           if(gvl_writeLog)then
                string=61
                LU=0
                call sStrings(string,errorString)
                call eWrite(errorString,LU)
             endif
            gvl_errorFlag=.false.
        !
      end subroutine openFiles_pop 
     ! ---------------------------

        ! ------------------------
        subroutine readFiles_pop()
            !
            ! -------- Types -------
!            integer :: LU
            integer :: i,j,ios
            integer :: lvi_countSRP
            ! ======================
            ios=0
            !----------------
            ! Read from Disk
            ! Provider-level data; populations and water demand, and acres
            !--------------------------------------------------------------------------------------
            ! acft a-1
            ! 
            read(51,*,err=42,iostat=ios)((lid_providerDemand(i,j),j=1,gvi_maxProV),i=gpi_lBY,gpi_uBY)
            read(53,*,err=43,iostat=ios)((gvi_gpcd_2010(i,j),j=1,gvi_maxProV),i=2000,2013)
            !
            ! Change the input format to allow year to stay in the first column
            ! -------------------------------------------------------------------
            read(54,*,err=44,iostat=ios)((gvf_Pop_OnProject(i,j),j=1,gvi_maxProV),i=gpi_lBY,gpi_uBY)
            read(55,*,err=45,iostat=ios)((gvf_Pop_Other(i,j),j=1,gvi_maxProV),i=gpi_lBY,gpi_uBY)
            ! -------------------------------------------------------------------------------------
                !
                ! -------------------------------------------------
                ! write to the common block
                ! -----------------------             
                do i = gpi_lBY,gpi_uBY,1
                  do j = 1,gvi_maxProV,1
                    gvf_population(i,j,1)=0.
                    gvf_population(i,j,1)=0.
                      gvf_population(i,j,1)=gvf_Pop_OnProject(i,j)
                      gvf_population(i,j,2)=gvf_Pop_Other(i,j)
                  end do
                end do
                !
                do i = gpi_lBY,gpi_uBY,1
                  do j = 1,gvi_maxProV,1
                      gvf_population(i,j,3)=gvf_population(i,j,1)+gvf_population(i,j,2)
                  end do
                end do
                ! ------------------------
 
            ! 
42          continue
            close(51)
            if(ios >0)then
             LU=51
             gvo_errorCode=51
             goto 1000
            endif
43          continue
            close(53)
            if(ios >0)then
             LU=53           
             gvo_errorCode=53
             goto 1000
            endif
44          continue
            close(54)
            if(ios >0)then
             LU=54
             gvo_errorCode=54
             goto 1000
            endif
45          continue
            close(55)
            if(ios >0)then
             LU=55
             gvo_errorCode=55
             goto 1000
            endif
            ! ----------------------
            ! Write to common block
            ! ----------------------
            do i = 1,10,1
              gvd_acreageSVT(i,1)=0
              gvd_acreageSVT(i,2)=0
              gvd_acreageSVT(i,3)=0
            end do
            call sMaskSRP(gvl_maskSRP)
            lvi_countSRP=1
            do i = 1,gvi_Providers,1
              if(gvl_maskSRP(i))then     
                 gvd_acreageSVT(lvi_countSRP,1)=lid_acreageSVT(lvi_countSRP) ! Class A from Designations_SVT.f90 (lm_Designations_A)
                 gvd_acreageSVT(lvi_countSRP,2)=lid_providerAcres(i,1) ! on project 
                 gvd_acreageSVT(lvi_countSRP,3)=lid_providerAcres(i,2) ! off project
                lvi_countSRP=lvi_countSRP+1
              endif
                !
                gvf_acerageTotal(i)=lid_providerAcres(i,1) + lid_providerAcres(i,2) 
                !
            end do 

            ! ---------------------
 140        format(10(F7.5,1x))
 141        format(F5.3)
 142        format(F6.0)
 143        format(F6.0)
            !
         return
1000     continue
           if(gvl_writeLog)then
              string=2
                call sStrings(string,errorString)
                call eWrite(errorString,LU)
            endif
          gvl_errorFlag=.false.
          !
        end subroutine readFiles_pop
        ! --------------------------

        ! 04.15.15
        ! ---------------------
        subroutine initialize()
            ! ---- Types ----
            integer:: i
            real :: pop,gpcd
            real:: fDemandFromPopGPCD
            ! =============== 
                !
                do i = 1,gvi_Providers,1
                   pop=0
                  pop= gvf_population(2000,i,3)
                   gpcd=0
                  gpcd=gvi_gpcd_2010(2000,i)
                go_ProviderDemand(i)=fDemandFromPopGPCD(gpcd,pop)
                end do
                !
          return
        end subroutine initialize
        ! -----------------------
!
End Module lm_ProviderPopulation
!
    ! ---------------------------------
    subroutine initializePopAndDemand()
        use lm_ProviderPopulation
        !
            call initPop()
        !
      return
    end subroutine initializePopAndDemand
    ! -----------------------------------
!


! ======================================================================================================
!
Module lms_ProviderPopandDemand
 use gm_ModelControl
  use gm_GlobalData
   use gm_TypeControl
    use gm_Euler
     use gm_DataAndSensitivity
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
        character(20),parameter :: Model='ProviderPopandDemand'
    !
 contains
    !
       ! ----------------------
       subroutine calcDemand(T)
          use gm_Exception
            !
            ! Requires: gvf_popgrowthfactor - from interface
            !           lid_providerpop(T%year,i): population by provider - from include file
            !           lvd_providerDemandRandM - from include file
            !           gv_EmpiricalGPCD - from include file
            ! ----------------------------------------------------------
            !

            ! -------------------------- Types -----------------------------------------------------------------------------
            integer :: i,j,k
            integer(4) :: gv_EmpiricalGPCD(35)

           ! real :: fRound
            real :: lvf_differencefluxpop_on(gvi_maxProV),lvf_differencefluxpop_other(gvi_maxProV)
            real :: lvf_differencepop_on(gpi_lBY:gpi_uBY,gvi_maxProV),lvf_differencepop_other(gpi_lBY:gpi_uBY,gvi_maxProV)
            !
            ! demand based on a simple exponential smoothing of GPCD (over time)
            real :: lvf_GPCDses
            real :: lvf_GPCD_total(gvi_maxProV)
                ! http://www.azwaterinstitute.org/media/EWSR/200909/200909_Hartmann.pdf
    !           real, parameter :: lvf_CF_Hartmann=-3.7 ! NOAA CLIMAS 200909_Hartmann.pdf
            real :: lvf_providerArea2012(gvi_maxProV) 
            real :: lvf_providerPopulations(gpi_minPVY:gpi_maxPVY,gvi_maxProV,2)         
            real(8) :: popFromInterfaceOn(gpi_lBY:gpi_uBY,gvi_maxProV),popFromInterfaceOther(gpi_lBY:gpi_uBY,gvi_maxProV)
            real(8) :: differencefluxpop(gvi_maxProV),differencedemand(gvi_maxProV)
            real(8) :: lv_addedDemand_acft(gvi_maxProV)
            real :: lvf_defaultIndoorDemand,lvf_days
            real :: lvf_temp,lvf_temp_2,lvf_temp_3
            !
            logical :: lvl_demandOption(4),lvl_raw
            ! ===================================================================================================================
            !

            ! -- Type constructs --
            type(runTime)T
           ! ======================
            !
!            E%MySub='calcDemand'
            !
            lvf_defaultIndoorDemand=0
            lvf_temp=0
            lvf_temp_2=0
            lvf_temp_3=0
            ! Conditional defaults - in Interface
            !
             IF(gvl_errorFlag)THEN
                ! 
                ! IF 0, then use original GPCD data (to replicate past)
                ! IF 1, then new GPCD with Brown's equal-weight SES function
                ! IF 2 then new GPCD with Brown's discount
                ! IF 3,then new GPCD with Brown's SES function increase
                ! default is 1 for gvi_GPCDmethod (lvi_functionGPCD)
                ! ----------------
                !
                if(gvl_start)then
    !               lvf_providerPopulations=gvf_population
                    ! Should move this to read input and put in global memory
                     gvf_providerAreaTotal_m2=0
                    call ProviderArea2012(lvf_providerArea2012)
                    gvf_providerAreaTotal_m2=sum(lvf_providerArea2012)
                endif
                !
                ! Should move this to read input and put in global memory
                call ProviderArea2012(lvf_providerArea2012)
                !
                gvf_providerAreaTotal_m2=sum(lvf_providerArea2012)
                !
                ! Calculate the on and off project lands
                ! 02.06.12 DAS- data from Ray
                ! ------------------------------------------
                ! From above
                ! --------------------------
                  do i = 1,gvi_Providers,1
                    !
                    if(gpl_runSensitivity)then
                        ! Sensitivity Analyses calls
                        ! 07.11.16 added here
                        ! ===========================
                        call LoopStartEnd(T,i)
!                         call embeddedCall(T)
                        ! ===========================
                        !
                    endif
                    gvd_classABCacreRatio(i)=0
                    ! 12.30.11 DAS
                    ! On (provider,1) and off (provider,2)
                    ! 04.09.12 DAS
                    ! 05.25.12
                    ! Stop exception division b zero
                    if(0 < gvf_population(T%year,i,3))then
                      gvd_classABCacreRatio(i)=gvf_population(T%year,i,1)/gvf_population(T%year,i,3)
                    else
                        if(gvl_WriteLog)then
                            string=6
                            LU=0
                            call sStrings(string,errorString)
                            call eWrite(errorString,LU)
                        endif
                      gvl_errorFlag=.false.
                      !
                    endif
                  end do
                ! ---------------------------------------------------------------------------------------
                !           Population - set and adjust
                ! --------------------------------------
                !

                !   Interface sending in populations growth rate adjustments for Providers
                ! Modify base populations and then add to get total population
                ! 05.25.12 DAS
                ! -----------------------------------------------------------------------
                do i = 1,gvi_Providers,1
                  !
                  lvf_providerPopulations(T%year,i,1)=anint(gvf_population(T%year,i,1))
                  !
                  lvf_providerPopulations(T%year,i,2)=anint(gvf_population(T%year,i,2))
                  !
                  ! Total population- on=project and other
                  lid_providerpop(T%year,i)= lvf_providerPopulations(T%year,i,1)+ lvf_providerPopulations(T%year,i,2)
                  !
                 end do

               do i = 1,gvi_Providers,1
                differencefluxpop(i)=0.
                lvf_differencefluxpop_on(i)=0.
                lvf_differencefluxpop_other(i)=0.
                lvf_differencepop_on(T%year,i)=0.
                lvf_differencepop_other(T%year,i)=0.
                !
                if(0 < gi_ProviderPopulationOn(i))gvl_popFromInterfaceOn(i)=.true.
                if(0 < gi_ProviderPopulationOther(i))gvl_popFromInterfaceOther(i)=.true.
               end do
                !
                do i = 1,gvi_Providers,1
                  ! 12.17.11 DAS
                     popFromInterfaceOn(T%year,i)=0
                     popFromInterfaceOther(T%year,i)=0
                    if(T%year == T%startyear)then
                      lvf_differencepop_on(T%year,i)=0
                      lvf_differencepop_other(T%year,i)=0
                    else
                      lvf_differencepop_on(T%year,i)=gvf_population(T%year,i,1)-gvf_population(T%year-1,i,1)
                      lvf_differencepop_other(T%year,i)=gvf_population(T%year,i,2)-gvf_population(T%year-1,i,2)
                    endif
                       ! 08.06.12
                        if(gvl_popFromInterfaceOn(i))then
                            popFromInterfaceOn(T%year,i)=gi_ProviderPopulationOn(i)
                        endif
                        if(gvl_popFromInterfaceOther(i))then
                            popFromInterfaceOther(T%year,i)=gi_ProviderPopulationOther(i)
                        endif
                     !
                        if(0 < gv_PopGrowthRateAdjPct(i))then
                          if(gvi_baseYear <= T%year)then
                            gvd_PopGrowthRateOn(i)=gv_PopGrowthRateAdjPct(i)
                            gvd_PopGrowthRateOther(i)=gv_PopGrowthRateAdjPct(i)
                          endif
                        endif
                    !

                     lvf_differencefluxpop_on(i)=lvf_differencepop_on(T%year,i) 
                    if(0 < gvd_PopGrowthRateOn(i))then
                     lvf_differencefluxpop_on(i)=lvf_differencepop_on(T%year,i) * gvd_PopGrowthRateOn(i)
                    endif
                    !
                     lvf_differencefluxpop_other(i)=lvf_differencepop_other(T%year,i) 
                    if(0 < gvd_PopGrowthRateOther(i))then
                     lvf_differencefluxpop_other(i)=lvf_differencepop_other(T%year,i) * gvd_PopGrowthRateOther(i)
                    endif
                    !
                     go_PopGrowthRate(i)=0
                    if(T%year == T%startyear)then
                    else
                      ! 05.30.12 DAS
                     go_PopGrowthRate(i)=nint( lvf_differencefluxpop_on(i) +  lvf_differencefluxpop_other(i))
                        ! Do something with this.
                      if(gvl_popFromInterfaceOn(i)) go_PopGrowthRate(i)=0
                    endif
                    ! ------------------------------------------------------------------------------------------
                    ! Growth Rate Adjusted Populations (GRAP)
                    if(T%year == T%startyear)then
                       !
                        gvf_GRAPopulationOnProject(T%year,i)=0.
                       gvf_GRAPopulationOnProject(T%year,i)=gvf_population(T%year,i,1)
                        gvf_GRAPopulationOther(T%year,i)=0.
                       gvf_GRAPopulationOther(T%year,i)= gvf_population(T%year,i,2)
                        lvd_GrowthRateAdjPop(T%year,i)=0.
                       lvd_GrowthRateAdjPop(T%year,i)=gvf_population(T%year,i,1)+gvf_population(T%year,i,2)
                       !
                    else
                        if(gvl_popFromInterfaceOn(i))then   
                            if(gvl_popFromInterfaceOther(i))then   
                              lvd_GrowthRateAdjPop(T%year,i)= popFromInterfaceOn(T%year,i) + popFromInterfaceOther(T%year,i)
                              gvf_GRAPopulationOnProject(T%year,i)=popFromInterfaceOn(T%year,i)
                              gvf_GRAPopulationOther(T%year,i)=popFromInterfaceOther(T%year,i)
                            else
                              lvd_GrowthRateAdjPop(T%year,i)= (gvf_GRAPopulationOther(T%year-1,i) + lvf_differencefluxpop_other(i)) +  popFromInterfaceOn(T%year,i)
                              gvf_GRAPopulationOnProject(T%year,i)= popFromInterfaceOn(T%year,i)
                              gvf_GRAPopulationOther(T%year,i)=gvf_GRAPopulationOther(T%year-1,i) + lvf_differencefluxpop_other(i)
                            endif
                        else
                            ! 10.06.15 das (zero growth event)
                            ! gvi_baseYear now in Parameter_control.f90 hardcoded - could be modifiable
                            ! by including into the interface

                            if(0 < gv_PopGrowthRateAdjPct(i) .and. gv_PopGrowthRateAdjPct(i) <= 0.01)then
                              if(gvi_baseYear <= T%year)then
                               lvf_differencefluxpop_on(i)=0
                               lvf_differencefluxpop_other(i)=0
                              endif
                            endif
                            !
                            if(gvl_popFromInterfaceOther(i))then
                              lvd_GrowthRateAdjPop(T%year,i)=popFromInterfaceOther(T%year,i) + (gvf_GRAPopulationOnProject(T%year-1,i)+lvf_differencefluxpop_on(i))
                              gvf_GRAPopulationOnProject(T%year,i)=gvf_GRAPopulationOnProject(T%year-1,i)+lvf_differencefluxpop_on(i)
                              gvf_GRAPopulationOther(T%year,i)= popFromInterfaceOther(T%year,i)
                            else
                              lvd_GrowthRateAdjPop(T%year,i)=lvd_GrowthRateAdjPop(T%year-1,i)+ lvf_differencefluxpop_on(i) + lvf_differencefluxpop_other(i)
                              gvf_GRAPopulationOnProject(T%year,i)=gvf_GRAPopulationOnProject(T%year-1,i)+lvf_differencefluxpop_on(i)
                              gvf_GRAPopulationOther(T%year,i)=gvf_GRAPopulationOther(T%year-1,i) + lvf_differencefluxpop_other(i)
                            endif
                        endif
                        !
                    endif
                 !
                 lid_providerpop(T%year,i)= lvd_GrowthRateAdjPop(T%year,i)
                 !
                end do
                !
                ! ----------------------------------------------------------------------------------------
                !
                lvl_demandOption=.false.
                !
                ! --------------------------
                ! Be careful of this DEFAULT SETTING DAS
                ! This option is passed in from the interface
                if(0 == gvi_ProviderDemandOption)gvi_ProviderDemandOption=3
                 if(gvi_ProviderDemandOption==1)lvl_demandOption(1)=.true.
                  if(gvi_ProviderDemandOption==2)lvl_demandOption(2)=.true.
                 if(gvi_ProviderDemandOption==3)lvl_demandOption(3)=.true.
                if(gvi_ProviderDemandOption==4)lvl_demandOption(4)=.true.
                !
                if(lvl_demandOption(1))then
                ! Change from integer input from the interface to float
                if(0 == gvi_ReductionDem_pctBaseline)gvi_ReductionDem_pctBaseline = 1.

                 ! Read Demand from files
                  do i = gpi_minPVY,gpi_maxPVY-1,1
                    do j = 1,gvi_Providers,1
                      ! 06.2.11 das
                      lvd_providerDemand(i,j)= lid_providerDemand(i,j)
                      lvd_providerDemandRandM(i,j)= lvd_providerDemand(i,j)*gvi_ReductionDem_pctBaseline
                      !
                    end do
                  end do
                else if (lvl_demandOption(2))then
                else if (lvl_demandOption(3))then
                else if (lvl_demandOption(4))then
                else
                   if(gvl_WriteLog)then
                        string=5
                        LU=0
                        call sStrings(string,errorString)
                        call eWrite(errorString,LU)
                    endif
                  gvl_errorFlag=.false.
                  !
                endif
                !
                gvf_popgrowthfactor=1
               ! =========================================================
                do j = 1,gvi_Providers,1
                   !  write to the inputs file (2000 only)
                    if(T%year <= 2000)then
                     go_providerpop(j)=lid_providerpop(2000,j)  
                     go_ProviderDemand(j)=lid_providerDemand(2000,j)
                    endif
                   !
                   if(0 < go_PopGrowthRate(j))gvf_popgrowthfactor=go_PopGrowthRate(j)
                   !
                   !--------------------------------------------------   
                        !  Populate "raw" gpcd in the event that the
                        ! user wishes to alter GPCD, then we have
                        ! the baseline to return to (adjust from)
                        ! ---------------------------------------
                        lvl_raw=.true.
                        call primeGPCD(T,j,k,lvl_raw,lvf_GPCDses)
                        !
                        go_rawGPCD(j)=nint(lvf_GPCDses)
                        gvf_GPCDraw(j,k)=lvf_GPCDses
                        !
                   !--------------------------------------------------   
                end do
                ! =========================================================
                ! --------------------------------------------------------------------------
                !           Demand - Adjusted
                ! --------------------------------
               ! Option 1, read demand directly from the file: modify for population growth changes ONLY
               !
              select case(gvi_ProviderDemandOption)
              case (1)
                do i = 1,gvi_Providers,1
                  lv_addedDemand_acft(i)=0.
                    if(2000 < T%year)then
                     differencedemand(i)=lvd_providerDemandRandM(T%year,i)-lvd_providerDemandRandM(T%year-1,i)
                    else
                     differencedemand(i)=lvd_providerDemandRandM(T%year,i)-lvd_providerDemandRandM(T%year,i)
                    endif
                   !
                   lv_addedDemand_acft(i)= differencedemand(i)*gvf_popgrowthfactor
                   !
                   if(T%year == T%startyear)then
                    if(2000 < T%year)then
                     lvd_GrowthRateAdjDemand(T%year,i)=lvd_providerDemandRandM(T%year-1,i)+lv_addedDemand_acft(i)
                    else
                     lvd_GrowthRateAdjDemand(T%year,i)=lvd_providerDemandRandM(T%year,i)+lv_addedDemand_acft(i)
                    endif
                   else
                    lvd_GrowthRateAdjDemand(T%year,i)=lvd_GrowthRateAdjDemand(T%year-1,i)+lv_addedDemand_acft(i)
                   endif
                 end do
                !
              case(2)
              !       Six year averages of GPCD = gv_EmpiricalGPCD
                !
                do i = 1,gvi_Providers,1

                  lv_addedDemand_acft(i)=0.
                    if(2000 < T%year)then
                      differencedemand(i)=max(0,lvd_providerDemandRandM(T%year,i)-lvd_providerDemandRandM(T%year-1,i))
                    else
                      differencedemand(i)=max(0,lvd_providerDemandRandM(T%year,i)-lvd_providerDemandRandM(T%year,i))
                    endif
                    !
                    if(gvf_popgrowthfactor <= 0.999)then
                      if(0 < gvf_popgrowthfactor)then
                        lv_addedDemand_acft(i)= -(differencefluxpop(i)* gv_EmpiricalGPCD(i)*T%days* (1./gpd_galperacft))
                      else
                        lv_addedDemand_acft(i)= (differencefluxpop(i)* gv_EmpiricalGPCD(i)*T%days* (1./gpd_galperacft))
                      endif
                    else
                     lv_addedDemand_acft(i)= (differencefluxpop(i)* gv_EmpiricalGPCD(i)*T%days* (1./gpd_galperacft))
                    endif
                    !
                     if(T%year == T%startyear)then
                        if(2000 < T%year)then
                          lvd_GrowthRateAdjDemand(T%year,i)=lvd_providerDemandRandM(T%year-1,i)+lv_addedDemand_acft(i)
                        else
                          lvd_GrowthRateAdjDemand(T%year,i)=lvd_providerDemandRandM(T%year,i)+lv_addedDemand_acft(i)
                        endif
                     else
                        lvd_GrowthRateAdjDemand(T%year,i)=max(0,lvd_GrowthRateAdjDemand(T%year-1,i)+lv_addedDemand_acft(i))
                     endif
                    !
                  call GPCD(gv_EmpiricalGPCD)
                  go_SES_GPCD(i)= (gv_EmpiricalGPCD(i))
                    !
                end do
                !
              case(3)
                    ! Simple exponential smoothing of ADWR gpcd data extended for the simulation; 
                    !
                   do j = 1,gvi_Providers,1
                    ! 
                    !  Whether increasing, decreasing, or constant - lvi_functionGPCD
                    ! fSlope gives the pct change in GPCD with time by 2085
                    ! 11.05.13 DAS
                    ! ===============================================================
                        !
                         lvl_raw=.false.
                        call primeGPCD(T,j,k,lvl_raw,lvf_GPCDses)
                        !
                    !
                    gvf_GPCDout(j,k)=lvf_GPCDses
                    !
                    !call GPCDfromMetrics(scale,j,calcGPCD)
                    !
                    lvd_GrowthRateAdjDemand(T%year,j) = &
                     ((lvf_GPCDses*365)*(1./gpd_galperacft))*lvd_GrowthRateAdjPop(T%year,j)
                    !
                    ! Pass to the interface
                    !go_SES_GPCD(j)=fRound(lvf_GPCDses)
                    go_SES_GPCD(j)=nint(lvf_GPCDses)
                    !
                  end do
                !
              case(4)
                    !  Simple exponential smoothing of ADWR gpcd data extended for the simulation; gpcd modified
                    ! from the C# interface to alter the estimates at any point
                    !
                   do j = 1,gvi_Providers,1
                    !
                    if(T%year < 2014)then
                        lvl_raw=.false.
                      call primeGPCD(T,j,k,lvl_raw,lvf_GPCDses)
                        !lvf_GPCDses=gv_AdjustedGPCD(j)
                        gvf_GPCDout(j,k)=lvf_GPCDses
                    else
                        lvf_GPCD_total(j)=gv_AdjustedGPCD(j)
                        !
                        if(0 < lvf_GPCD_total(j))then
                          lvf_GPCDses= lvf_GPCD_total(j)
                        endif   
                    endif
                    ! 
                     gvf_GPCDout(j,k)=lvf_GPCDses
                    !
                    lvd_GrowthRateAdjDemand(T%year,j) = &
                     ((lvf_GPCDses*365)*(1./gpd_galperacft))*lvd_GrowthRateAdjPop(T%year,j)
                    !
                    ! Pass to the interface
                    go_SES_GPCD(j)=nint(lvf_GPCDses)
                    !
                   end do
               !
              case default
                !
                if(gvl_writeLog)then
                    LU=0
                    string=4
                    call sStrings(string,errorString)
                    call eWrite(errorString,LU)
                endif
                !
              end select
              !
              ! Adjust LCLU demand data to population/GPCD data for Scenarios Project
              ! 05.18.16
              !
                  call daysintheyearK(T%year,T)
                   lvf_days=T%days
              !
              do j = 1,gvi_Providers,1
                !
                if(T%year <=T%startyear)then
                    lvd_providerDemandRandM(T%year,j)=lvd_GrowthRateAdjDemand(T%year,j)
                endif
                    !
                    ! This gets sent to the interface in Provider.f90-subroutine sProvider(T,vGP,vLP)
                     gvf_populations(T%year,j)=0.
                    gvf_populations(T%year,j)=lvd_GrowthRateAdjPop(T%year,j) 
                    !
                    go_PopulationOnProject(j)=nint(gvf_GRAPopulationOnProject(T%year,j))
                    go_PopulationOther(j)    =nint(gvf_GRAPopulationOther(T%year,j))
                    !
                    gvf_demandNoAg(j)=0.
                   gvf_demandNoAg(j)=lvd_GrowthRateAdjDemand(T%year,j) 

                  ! 2 March 2011,07.19.11 das
                    gvf_TotalDemand_acft(j)=0.
                   gvf_TotalDemand_acft(j)=gvf_demandNoAg(j)
!                    ! 11.15.16 DAS
!                    ! NEW CODE to reduce OutDoor Water demand and, thus, total demand
!                    ! Need to also now reduce GPCD to align with the new demand estimates
!                    ! for each water provider
!                    ! ------------------------------------------------------------------------------------
!                     lvf_defaultIndoorDemand=0
!                     lvf_temp_2=0
!                     lvf_temp_3=0
!                    if(gvl_defaultRun(7,j))then
!                      gvf_TotalDemand_acft(j)=gvf_demandNoAg(j)
!                    else
!                      lvf_defaultIndoorDemand=gvf_demandNoAg(j)*(1-gdf_parm_OutDoorResPCT(j)*0.01)
!                      if(0 < gdf_parm_OutDoorResPCT(j))then
!                        lvf_temp=gvf_parm_OutDoorResProp(j)/(gdf_parm_OutDoorResPCT(j)*0.01)
!                        gvf_TotalDemand_acft(j)=gvf_demandNoAg(j) * lvf_temp
!                        !
!                         lvf_temp_2= lvf_defaultIndoorDemand/gvf_TotalDemand_acft(j)                       
!                        gvf_parm_OutDoorResProp(j)= 1-lvf_temp_2
!                        !
!                        !lvf_temp_3=go_SES_GPCD(j)
!                        !go_SES_GPCD(j) = nint(lvf_temp_3 * lvf_temp)
!                        if( 0 < lvf_days)then
!                         lvf_temp_3= (gvf_TotalDemand_acft(j)*gpd_galperacft)* (1/gvf_populations(T%year,j))* (1/lvf_days)
!                         go_SES_GPCD(j)=nint(lvf_temp_3)
!                        endif
!                      endif
!                    endif
!                    ! ====================================================================================
                    !
                    ! DAS 05.12.16
                ! ---------------------------------
                ! Set, and then estimate, water demand based on lanc cover/land use
                ! New to WaterSim 6
                ! 05.17.16
                if(gvl_IncludeMeteorology)then
                    !
                  if(gvl_utilizeLCLU)then
                    !
                    call sRunLCLUarea(T,j)
                    ! 2010
                    if(gpi_lclu <= T%year)then
                      !
                      ! set waterUseByLCLU
                      call LULCdemand(T,j)
                      ! 2016
                      if(gvi_baseYear <= T%year)then
                      ! utilize waterUseByLCLU
                         gvf_TotalDemand_acft(j)=0
                        do k = 1,13,1
                          if(0 < gvf_waterUseRatioLCLU(k,j))then
                            if(1 < k)then
                              gvf_TotalDemand_acft(j)=  gvf_TotalDemand_acft(j) + gvf_waterUseLCLU(k,j)
                            else
!                              gvf_agDemand_acft(j)=gvf_waterUseLCLU(k,j)
!                              gvf_LCLUdemand(k,j)=gvf_agDemand_acft(j)
                            endif
                          endif
                        end do
                        !
                      else



                      endif
                    endif
                  else
                    ! Use GPCD and population for water demand NOT LCLU
                  endif
                endif
                !
!                    ! 11.15.16 DAS
                    ! NEW CODE to reduce OutDoor Water demand and, thus, total demand
                    ! Need to also now reduce GPCD to align with the new demand estimates
                    ! for each water provider
                    ! ------------------------------------------------------------------------------------
                     lvf_defaultIndoorDemand=0
                     lvf_temp_2=0
                     lvf_temp_3=0
                  if(gvl_IwaniecYN)then
 
                      lvf_temp_3=go_SES_GPCD(j)
!                        if(2014 <= T%year)then
                        if(2010 <= T%year)then
                          lvf_temp_2=gvi_lcluGPCD(j)
                        else
                          lvf_temp_2=lvf_temp_3
                        endif
                        !
                          go_SES_GPCD(j)=lvf_temp_2
                        !
1000 format(I4,1x,2(F7.2,2x))
                  else
                     lvf_temp_2=0
                     lvf_temp_3=0
                    !
                    if(gvl_defaultRun(7,j))then
                      gvf_TotalDemand_acft(j)=gvf_demandNoAg(j)
                    else
                      lvf_defaultIndoorDemand=gvf_demandNoAg(j)*(1-gdf_parm_OutDoorResPCT(j)*0.01)
                      if(0 < gdf_parm_OutDoorResPCT(j))then
                        lvf_temp=gvf_parm_OutDoorResProp(j)/(gdf_parm_OutDoorResPCT(j)*0.01)
                        gvf_TotalDemand_acft(j)=gvf_demandNoAg(j) * lvf_temp
                        !
                         lvf_temp_2= lvf_defaultIndoorDemand/gvf_TotalDemand_acft(j)                       
                        gvf_parm_OutDoorResProp(j)= 1-lvf_temp_2
                        !
                        !lvf_temp_3=go_SES_GPCD(j)
                        !go_SES_GPCD(j) = nint(lvf_temp_3 * lvf_temp)
                        if( 0 < lvf_days)then
                         lvf_temp_3= (gvf_TotalDemand_acft(j)*gpd_galperacft)* (1/gvf_populations(T%year,j))* (1/lvf_days)
                         go_SES_GPCD(j)=nint(lvf_temp_3)
                        endif
                      endif
                    endif
                  endif
                    ! ====================================================================================


7   format(I4,1x,I2,1x,2(F9.1,1x))
130 format(I4,1x,I2,1x,I2,1x,I2,1x,F6.3)
200 format(I4,1x,I2,1x,I2,1x,F9.1,1x,I4)
                  !  05.25.12
                   gvf_WaterDemand_acft(j,1,1)=anint(gvf_TotalDemand_acft(j))                           ! Total demand
                   gvf_WaterDemand_acft(j,1,3)=anint(gvf_TotalDemand_acft(j)*gvd_classABCacreRatio(j))   ! On project
                   gvf_WaterDemand_acft(j,1,2)=anint(gvf_TotalDemand_acft(j)-gvf_WaterDemand_acft(j,1,3))  ! Off Project
                  !  04.11.16 
                   gvf_WaterDemand_acft(j,1,7)=gvf_WaterDemand_acft(j,1,2)* (1 - fOutdoorUse(j))
                   gvf_WaterDemand_acft(j,1,8)=gvf_WaterDemand_acft(j,1,2)-gvf_WaterDemand_acft(j,1,7)
                   gvf_WaterDemand_acft(j,1,9)= gvf_WaterDemand_acft(j,1,3)* (1 - fOutdoorUse(j))
                   gvf_WaterDemand_acft(j,1,10)=gvf_WaterDemand_acft(j,1,3)-gvf_WaterDemand_acft(j,1,9)
                  !
                   gvf_reclaimedInputMax(T%year,j)=0.
                  gvf_reclaimedInputMax(T%year,j)=gvf_WaterDemand_acft(j,1,1)*gvf_parm_RtoInputMaxPct(j)
                  !
                    go_OnProjectDemand(j)=0
                  go_OnProjectDemand(j)= nint(gvf_WaterDemand_acft(j,1,3))
                  !
                    go_OffProjectDemand(j)=0
                  go_OffProjectDemand(j)= nint(gvf_WaterDemand_acft(j,1,2))
                    ! 01.21.15
                   go_residentialGPCD(j)=0
                  go_residentialGPCD(j)= go_SES_GPCD(j) * gvf_parm_WStoRes_prop(j)
                !     
                   go_industrialGPCD(j)=0
                  go_industrialGPCD(j)=go_SES_GPCD(j) * gvf_parm_WStoInd_prop(j)
                !
            end do
               ! ------------------------------------------------------------------
               !
250         format(i4,2x,i2,2x,4(i4,2x),2(F6.3))
                ! 
                if(T%year .EQ. T%startyear)then
                    if(gvl_writeLog)then
                        string=2
                        LU=0
                        call sOnceThrough(string,streamString)
                        call sWrite(streamString,LU)
                    endif
                endif
                !
               ! ------------------------------------------------------------------
            !
            ELSE
                !
               if(gvl_writeLog)then
                    string=60
                    LU=0
                    call sStrings(string,errorString)
                    call eWrite(errorString,LU)
                endif
              gvl_errorFlag=.false.
              !
            END IF
             !
            return
           end subroutine calcDemand
           !---------------------------

            ! ------------------------------
            subroutine LULCdemand(T,i)
                ! ------ Types -------
                integer :: i
                ! ====================

                ! -- Type Construct ---
                type(runTime)T
                ! ======================
                    !
                    call  sRunLCLU(T,i)
                    !
                !
              return
            end subroutine LULCdemand

            ! ------------------------------

            ! ----------------------------------------------
            function fOutdoorUse(provider)
            !
                ! ------- types -------
                integer :: provider
                real :: fOutdoorUse
                real :: Res,Com,Ind,temp
                ! =====================
                    !
                    Res = gvf_parm_WStoRes_prop(provider)*gvf_parm_OutDoorResProp(provider)
                    Com = gvf_parm_WStoCom_prop(provider)*gvf_parm_OutDoorComProp(provider)
                    Ind = gvf_parm_WStoInd_prop(provider)*gvf_parm_OutDoorIndProp(provider)
                    !
                    temp = Res+Com+Ind
                    fOutdoorUse=temp
            !
            end function fOutdoorUse
            ! ----------------------------------------------

            ! ----------------------------------------------
            subroutine primeGPCD(T,j,k,lvl_raw,lvf_GPCDses)
                !
                ! -------------------- Types -----------------------------
                integer :: i,j,k
                integer :: lvi_functionGPCD,fGPCDmethodValues,lvi_N
                integer :: lvi_AlterGPCD

                real :: lvf_passGPCD(7)
                real :: lvf_gpcdRunning(14)
                real :: lvf_GPCDses,fSlope
                logical :: lvl_raw
                ! ========================================================
                !

                ! - Type constructs -
                type(runTime)T
                ! ===================
                !`
                    lvi_N=7 
                    !
                      !   lvi_AlterGPCD is the default values that come
                      ! from the API-interface (as a get accessor, i.e.,
                      ! getProviderAlterGPCDPct(count, values))
                      ! lvi_AlterGPCD is scaled from 
                      ! -------------------------------------------------
                      ! Provider parameter
                      lvi_AlterGPCD=0
                      if(gpl_release)then
                        lvi_AlterGPCD=gvi_ProviderAlterGPCD_pct(j)
                      else
                        lvi_AlterGPCD=-4
                        gvl_APIstatus=.true.
                      endif
                     !
                      ! 01.09.15 - if the web UI is accessing the code,
                      ! separate rules. Use the inputs directly
                      !
                      ! NOTE: the model must run at least one year in the default mode
                      ! in order to set the default settings......
                      ! 06.05.15 das
                      !gpl_finalOutputs
                     if(gvl_APIstatus)then
                        fSlope=lvi_AlterGPCD
                        lvi_functionGPCD=fGPCDmethodValues(lvi_AlterGPCD)
                        gvf_gpcdSlopeDefault(1,j)=fSlope
                        gvf_gpcdSlopeDefault(2,j)=lvi_functionGPCD
                      else
                        !
                        fslope =  gvf_gpcdSlopeDefault(1,j)
                        lvi_functionGPCD = gvf_gpcdSlopeDefault(2,j)
                        lvi_functionGPCD= nint(gvf_gpcdSlopeDefault(2,j))
                        !
                        select case(lvi_functionGPCD)
                          case(1) ! Equaly Weight
                            fSlope= gvf_gpcdSlopeDefault(1,j)
                            fSlope=gvi_ProviderAlterGPCD_pct(j)
                            lvi_functionGPCD=2

                          case(2) ! Discount
                            fSlope=gvi_ProviderAlterGPCD_pct(j)
                          case(3) ! Increasing
                            fSlope= gvf_gpcdSlopeDefault(1,j)      
                           if(gvi_ProviderAlterGPCD_pct(j) < fSlope)then
                               fSlope=gvi_ProviderAlterGPCD_pct(j)
                               if(fSlope < 0)lvi_functionGPCD=2
                           endif
                          case default
                            fSlope=gvi_ProviderAlterGPCD_pct(j)
                        end select
                      endif
                     !  

                     ! gvi_AlterGPCD_pct comes from the API-interface as 
                     ! a set accesor (i.e.,setAlterGPCDPct(value))
                     ! Thus,gvi_AlterGPCD_pct is scaled from -95 to 95 
                     ! --------------------------------------------------
                     !      Base parameter
                     if(0 < gvi_AlterGPCD_pct)then
                      fSlope=gvi_AlterGPCD_pct
                      lvi_functionGPCD=fGPCDmethodValues(gvi_AlterGPCD_pct)
                     else if(gvi_AlterGPCD_pct < 0)then
                      fSlope=gvi_AlterGPCD_pct
                      lvi_functionGPCD=fGPCDmethodValues(gvi_AlterGPCD_pct)
                     else
                      ! cannot use zero for the base parameter.
                     endif
                     !     
                    k=5
                    if(T%startyear == 2000)then
                      !
                      do i = 1,7,1
                        lvf_passGPCD(i)=0
                      end do
                      !
                      do i = 1,10,1
                        lvf_gpcdRunning(i)=0
                      end do          
                        !
                        k=T%year-1995
                        !
                    endif
                        !
!                      lvf_passGPCD1=0;lvf_passGPCD2=0 ;lvf_passGPCD3=0;lvf_passGPCD4=0
                      if(T%year <= 2013)then
                         if(2006 <= T%year)then
                          do i = 1,7,1
                           lvf_gpcdRunning(i)=gvf_GPCDout(j,k-i)
                            !
                            lvf_passGPCD(i)=gvf_GPCDout(j,k-i)
                            !
                          end do
                        endif
                        !
                      else
                        if(lvl_raw)then
                            do i = 1,7,1
                             lvf_gpcdRunning(i)=gvf_GPCDraw(j,k-i)
                             lvf_passGPCD(i)=gvf_GPCDraw(j,k-i)
                            end do
                            !
                        else
                           do i = 1,7,1
                             lvf_gpcdRunning(i)=gvf_GPCDout(j,k-i)
                             lvf_passGPCD(i)=gvf_GPCDout(j,k-i)
                            end do
                            !
                        endif
                      endif
                     !
                     call gpcdSES(T,j,lvl_raw,lvf_passGPCD,lvi_functionGPCD,fSlope, &
                        lvi_N,lvf_gpcdRunning,lvf_GPCDses) 
                !
             return
            end subroutine primeGPCD
           ! ------------------------

           ! -----------------------------------------
            subroutine readNewGPCD(provider,lvf_gpcd)
                !
                ! -------------- Types ---------------
                integer :: i,provider
                real :: lvf_gpcd(2000:2013,gvi_maxProV)
                ! ====================================
                !
                 do i = 2000,2013,1
                    lvf_gpcd(i,provider)=gvi_gpcd_2010(i,provider)
                 end do
                !
             return
            end subroutine readNewGPCD
            ! --------------------------

            ! ------------------------------------------------------------------------------
             subroutine gpcdSES(T,provider,lvl_raw,lvf_passGPCD,lvi_functionGPCD,gvf_AlterGPCD_pct, &
                    lvi_N,gpcd,SESgpcdRunning)

                use gm_ModelControl
                    !
                    ! ------------------ Types and parameters ---------------
                    integer :: i,provider,lvi_functionGPCD,N,lvi_N
                    integer :: lpi_refYear,lpi_runYear,lvi_case,lvi_caseNewGPCD

                    real :: alpha_ses
                    real :: gvf_AlterGPCD_pct 
                    real :: gpcd(14)
                    real :: lvf_GPCDraw(2000:2013,gvi_maxProV)
                    real :: fSESgpcd_discount,fSESgpcd_equalWeight
                    real :: SESgpcdRunning 
                    real :: lvf_passGPCD(7),lvf_tempGPCD(7)
                    real :: lvf_base,lvf_baseYear
                    
                    real:: lvf_beta,lvf_gamma,lvf_newGPCD(3)

                    logical:: lvl_raw
                    ! ======================================================

                    ! --- Type Constructs ---
                    type(runTime)T
                    ! =======================
                        !
                        alpha_ses=0.9
                        lpi_refYear=2014
                        lpi_runYear=2014

                        SESgpcdRunning=0
                        !lvf_modAlpha=0
                        lvf_baseYear=0
                        !
                        do i = 1,3,1
                          lvf_newGPCD(i)=0
                        end do
                        !
                        if(T%year < 2001)then
                          if(provider < 2)then
                            if(.not. gpl_release)then
                              do i = 1,gvi_maxProV,1
                                !gvi_ProviderMinGPCD(i)= 70
                              end do
                            endif
                          endif
                        endif
                        !
                        ! http://www.duke.edu/~rnau/411avg.htm
                        ! Brown's simple Exponential Smoothing (exponentially weighted moving average)
                        ! alpha determines relative weight among observations

                        ! lvf_projReductionGPCD= projected reduction in GPCD by 2085 (over 2006 estimates) - tempe used as a base case
                        !
                        ! Modified type IV exponential [exponent is < 0] and power functions [type IV is the scaling parameter];
                        ! See functions.f90
                        ! -------------------------------------------------------------------------------------
                        ! 08.19.13, 04.18.14 re-formulated again
                        ! baseline is now 2010 (and/or, user defined)
                        ! -------------------------------------

                    ! - No provider and Other provider -----
                    !   crashes model if not set
                    if(provider == 22)then
                        lvi_functionGPCD=2; gvf_AlterGPCD_pct=-3
                    endif
                    if(provider == 23)then
                        lvi_functionGPCD=2 ; gvf_AlterGPCD_pct=-3
                    endif
                     !
                     lvi_case=1
                    lvi_caseNewGPCD=lvi_functionGPCD

                    !   This is now a place holder for a policy that could be 
                    ! implemented as "start a reduction in GPCD at x year"
                    !
                    ! For now, set as 0
                     gvf_parm_gpcdBaseYear(provider)=0
                    lvf_baseYear=gvf_parm_gpcdBaseYear(provider)
                    !
                       if(2012 < T%year)then
                            ! Change the base year from the Interface. That is, if you need to
                            ! alter the trajectory of GPCD at some point during the simulation run stream
                            ! then invoke gvf_parm_gpcdBaseYear(provider)
                            ! 04.18.14
                            if(0 < lvf_baseYear)then
                               lvf_base =lvf_passGPCD(1)
                                 !
                                 lvf_tempGPCD(1)=lvf_base; 
                                 lvf_tempGPCD(2)=lvf_passGPCD(1); lvf_tempGPCD(3)=lvf_passGPCD(2);lvf_tempGPCD(4)=lvf_passGPCD(3); lvf_tempGPCD(5)=lvf_passGPCD(4); lvf_tempGPCD(6)=lvf_passGPCD(5);  lvf_tempGPCD(7)=lvf_passGPCD(6);
                                 !
                               select case(lvi_caseNewGPCD)
                                case(2)
                                  call calculateAlpha(lvf_baseYear,provider,gvf_AlterGPCD_pct,lvf_base,lvf_tempGPCD) 
                                case(3)
                                !
                                end select
                            endif

                        endif
                        !   Standard, reduction or increase in GPCD uses 2010 as the base year. That is, a 30% decrease in GPCD
                        ! by 2085 uses 2010 as the year to compare the decrease to. So, a 30% decrease over the 2010 estimate.
                        ! 04.18.14 DAS
                        ! 06.17.15 das
                        if(T%year < lpi_refYear)then

                             call readProviderGPCD(provider,lvf_GPCDraw)
                              lvf_base =lvf_GPCDraw(T%year,provider)
                                !
                                lvf_tempGPCD(1)=lvf_base; 
                                lvf_tempGPCD(2)=lvf_passGPCD(1); lvf_tempGPCD(3)=lvf_passGPCD(2);lvf_tempGPCD(4)=lvf_passGPCD(3); lvf_tempGPCD(5)=lvf_passGPCD(4); lvf_tempGPCD(6)=lvf_passGPCD(5);  lvf_tempGPCD(7)=lvf_passGPCD(6);
                                !
                            ! 
                           select case(lvi_caseNewGPCD)

                            case(1)
                            case(2)
                                ! want 2010
                                if(lpi_refYear-2 < T%year)then

                                   lvf_baseYear=2013
                                  call calculateAlpha(lvf_baseYear,provider,gvf_AlterGPCD_pct,lvf_base,lvf_tempGPCD) 
                                endif
                            case(3)

                                ! want 2010
                                ! Find the parameter estimates for the increasing GPCD function
                                ! Enter 
                               if(lpi_refYear-2 < T%year)then
                                 !
                                 call sFindBetaGamma(lvl_raw,provider,gvf_AlterGPCD_pct,lvf_newGPCD,lvf_tempGPCD)
                                 !
                               endif
                           end select
                      
                        endif
                    !
                    lvf_baseYear=0
                    !
                    select case(lvi_case)
                      case(0)
                      case(1)
                            !
                             N=lvi_N
                            call  sSetGPCD(provider,lvl_raw,lvf_newGPCD)
                            !
                            ! Use empirical values for 2000 through 2013
                            if(T%year < lpi_runYear)then
                              call readProviderGPCD(provider,lvf_GPCDraw)
                                SESgpcdRunning =lvf_GPCDraw(T%year,provider)
                            else
                                !
                                select case(lvi_caseNewGPCD)
                                  case(1)
                                  case(2)
                                    alpha_ses = gvf_parm_alpha(provider)
                                  case(3)
                                  case default
                                end select
                                !
                                select case(lvi_caseNewGPCD)
                                  case(1)
                                    SESgpcdRunning = fSESgpcd_equalWeight(N,gpcd)   

                                  case(2)
                                    SESgpcdRunning = fSESgpcd_discount(alpha_ses,lvf_passGPCD)
                                    if(nint(SESgpcdRunning) <= gvi_ProviderMinGPCD(provider))then
                                      SESgpcdRunning = gvi_ProviderMinGPCD(provider)-1
                                    endif

                                 case(3)
                                    if(0 < lvd_LinearRegression(provider,1))then
                                     SESgpcdRunning =  lvd_LinearRegression(provider,2) * T%year &
                                         + lvd_LinearRegression(provider,3)
                                    else
                                        lvf_beta= gvf_betaHoltGPCD(provider)
                                        lvf_gamma= gvf_gammaHoltGPCD(provider)
                                        !
                                        call  sSetGPCD(provider,lvl_raw,lvf_newGPCD)
                                        SESgpcdRunning =  fHolt(lvf_newGPCD,lvf_beta,lvf_gamma) 
                                        ! 
                                        gvf_dynamicGPCD(3,provider)=lvf_newGPCD(3);gvf_dynamicGPCD(2,provider)=lvf_newGPCD(2)
                                        gvf_dynamicGPCD(1,provider)=lvf_newGPCD(1)
                                    endif
                                   case default
                                    SESgpcdRunning = fSESgpcd_equalWeight(N,gpcd)
        
                                end select
                           endif
                    case default
                    end select  
                    !
                 return
              end subroutine
!             ! -------------

!            ! ------------------------------------------------------------------------
             subroutine  calculateAlpha(fyear,provider,lvf_alterGPCD,lvf_base,lvf_gpcd)
                !
                ! ----------------  Types ----------------------
                integer :: provider,LU

                real :: fSESgpcd_alpha,fyear
                real :: lvf_alterGPCD,lvf_target,c_alpha,lvf_base
                real :: lvf_gpcd(7)
                real :: lvf_alterGPCDdown

                logical :: lvl_error
                ! ==============================================
                    !
                    lvl_error=.false.
                    lvf_alterGPCDdown=abs(lvf_alterGPCD) 
                    !     
                    if(0 < fyear)then
                      call gpcd_target(lvf_base,lvf_alterGPCD,lvf_target)                     
                    else
                      call gpcd_target(lvf_base,lvf_alterGPCD,lvf_target)
                    endif
                    !
                    !  
                    ! 01.07.15
                        if(lvf_alterGPCDdown < 1)then
                          c_alpha=0.90
                        else
                          c_alpha=fSESgpcd_alpha(fyear,lvf_alterGPCD,lvf_target,lvf_gpcd,lvl_error)
                        endif
                    !
                    fyear=0
                    gvf_parm_alpha(provider)=c_alpha
                    !
                if(lvl_error)then
                     if(gvl_writeLog)then
                        LU=provider
                        string=54
                        call sStrings(string,errorString)
                        call eWrite(errorString,LU)
                      else
                      endif   
                     gvl_errorFlag=.false.
                endif
              return
             end subroutine calculateAlpha
            ! ----------------------------

            ! ------------------------------------------------------
            subroutine gpcd_target(lvf_base,lvf_GPCD_pct,lvf_target)
                !
                ! ----------- Types ------------
                real:: lvf_target
                real(4) :: lvf_base,lvf_GPCD_pct
                ! ==============================
                    !
                    lvf_target = (lvf_base + lvf_base*(lvf_GPCD_pct)*1/100)
                    !
              return
            end subroutine gpcd_target
            ! --------------------------
         
            ! ====================================================================================
            ! Start Holt-Sampson functions and subroutines
            ! 05.15.2015
            !
            ! New code to estimate/simulate increasing GPCD

            ! ------------------------------------------------------
            subroutine sHoltTarget(gpcd,target,alpha,beta,success,j)
                !
                ! ------------ Types ---------
                integer:: i,j,k
                integer:: n

                real:: target
                real:: INgpcd(3),gpcd(3),test
                real:: alpha,beta
                real:: incAlpha,incBeta
                real:: initBeta
                real,parameter:: margin=2

                logical:: success
                ! ============================
                !
                success=.false.
                initBeta=beta
                !
                n=1000
                incAlpha=0.01
                incBeta=0.01
                do k = 1,3,1
                 INgpcd(k)=gpcd(k)
                end do
                !
                do j = 1,n,1
                  !
                    test=0
                  do i = 1,72,1
                    test=fHolt(gpcd,alpha,beta)
                  end do
                  !
                  if((target-margin < test) .and. (test < target+margin))then
                        success=.true.
                        goto 10
                   else
            5       continue
                     do k = 1,3,1
                      gpcd(k)=INgpcd(k)
                     end do
                    !
                    beta=fMinMaxBeta(beta+incBeta)
                    !
                    if(0.99 < beta)then
                     alpha=fMinMaxAlpha(alpha-incAlpha)
                     beta=initBeta
                    endif
                    !
                    success=.false.
                    !
                   endif
                !
                end do
            10  continue
             return
            end subroutine sHoltTarget
            ! -------------------------

                ! University of Baltimore
                ! 05.11.2015
                ! Holt's Linear Exponential Smoothing Technique:
                !http://home.ubalt.edu/ntsbarsh/business-stat/stat-data/forecast.htm#rHoltLinear

                function fHolt(gpcd,alpha,beta) result(F4)
                    ! ----------- Types -----------
                    real:: gpcd(3)
                    real:: L2,L3,T2,T3,F3,F4
                    real:: alpha,beta
                    real,parameter:: gamma=1.0
                    ! =============================
                        !
                        L2=gpcd(2); T2=gpcd(2)-gpcd(1) ; F3=L2+T2
                        L3=alpha*gpcd(3) + (1-alpha)*F3
                        T3=(beta*(L3-L2) + (1.-beta)*T2)**gamma
                        F4 = L3+ beta*T3
                        !
                        gpcd(1)=gpcd(2)
                        gpcd(2)=gpcd(3)
                        gpcd(3)=F4
                      !
                 return
                end function fHolt
                ! ----------------

                ! -------------------------------------------------------
                function fMinMaxAlpha(alpha_a) result(alpha_b)
                        real:: alpha_a,alpha_b
                        real,parameter:: lpf_minAlpha=0.51
                        real,parameter:: lpf_maxAlpha=0.95
                        !
                        alpha_b=max(lpf_minAlpha,min(alpha_a,lpf_maxAlpha))
                        !
                    return
                  end function fMinMaxAlpha
                 function fMinMaxBeta(beta_a) result(beta_b)
                        real:: beta_a,beta_b
                        real,parameter:: lpf_minBeta=0.1
                        real,parameter:: lpf_maxBeta=1
                        !
                        beta_b=max(lpf_minBeta,min(beta_a,lpf_maxBeta))
                        !
                    return
                  end function fMinMaxBeta
                ! ------------------------------------------------------

                ! -------------------------
                subroutine sFindAlphaBetaStart(target,gpcd,alpha,beta,success)
                    implicit none
                    ! ------ Types -----
                    integer:: i,j,k,n

                    real:: gpcd(3),INgpcd(3)
                    real:: target,test
                    real:: incA,incB
                    real:: alpha,beta,initBeta
                    
                    logical:: success
                    ! ==================
                        !
                        do k = 1,3,1
                         INgpcd(k)=gpcd(k)
                        end do

                        n=300
                        success=.false.
                        incA=0.05
                        incB=0.01
                        initBeta=beta
                        do j = 1,n,1
                            test=0
                          do i = 1,72,1
                            test=fHolt(gpcd,alpha,beta)
                          end do
                          !
                          if(target < test)then

                            beta=fMinMaxBeta(beta-incB)

                            if(beta <= 0.1)then
                             alpha=fMinMaxAlpha(alpha+incA)
                             beta=initBeta
                            endif
                          else
                           success=.true.
                          endif
                          !
                            do k = 1,3,1
                              gpcd(k)=INgpcd(k)
                            end do
                          !
                         if(success)exit
                        end do
                        !
                 return
                end subroutine sFindAlphaBetaStart                 
                ! --------------------------------

                ! --------------------------------
                subroutine sLinearRegression(base,target,m,b)
                  ! no default data types
                   ! -------------- Types ------------------
                   integer, parameter  :: dbl = kind (0.0d0)                                        ! define kind for double precision
                   integer:: i

                   real,parameter:: Rstart=2014
                   real,parameter:: Rend=2085
                   real:: base, target
                   real(dbl)           ::  b                                                        ! y-intercept of least-squares best fit line
                   real(dbl)           ::  m                                                        ! slope of least-squares best fit line
                   real(dbl)           ::  n = 0.0d0                                                ! number of data points
                   !real(dbl)           ::  r                                                        ! squared correlation coefficient
                   real(dbl)           ::  sumx  = 0.0d0                                            ! sum of x
                   real(dbl)           ::  sumx2 = 0.0d0                                            ! sum of x**2
                   real(dbl)           ::  sumxy = 0.0d0                                            ! sum of x * y
                   real(dbl)           ::  sumy  = 0.0d0                                            ! sum of y
                   real(dbl)           ::  sumy2 = 0.0d0                                            ! sum of y**2
                   real(dbl)           ::  x                                                        ! input x data
                   real(dbl)           ::  y                                                        ! input y data
                   ! =====================================
                    do i = 1,2,1
                       if(i == 1)then
                        x=Rstart
                        y=base
                       else
                        x=Rend
                        y=target
                       endif
                      !
                      n = n + 1.0d0                                                                 ! increment number of data points by 1
                      sumx  = sumx + x                                                              ! compute sum of x
                      sumx2 = sumx2 + x * x                                                         ! compute sum of x**2
                      sumxy = sumxy + x * y                                                         ! compute sum of x * y
                      sumy  = sumy + y                                                              ! compute sum of y
                      sumy2 = sumy2 + y * y                                                         ! compute sum of y**2
                   end do

                   m = (n * sumxy  -  sumx * sumy) / (n * sumx2 - sumx**2)                          ! compute slope
                   b = (sumy * sumx2  -  sumx * sumxy) / (n * sumx2  -  sumx**2)                    ! compute y-intercept
!                   r = (sumxy - sumx * sumy / n) /      &          
!                        sqrt((sumx2 - sumx**2/n) * (sumy2 - sumy**2/n))

                  return
                end subroutine sLinearRegression
                ! --------------------------------

                ! ---------------------------------------------------------------------------
                subroutine sFindBetaGamma(lvl_raw,provider,gvf_AlterGPCD_pct,lvf_newGPCD,lvf_tempGPCD)
                    !
                    ! -------------------- Types ---------------------
                    integer:: cycles,provider,i

                    real :: lvf_tempGPCD(7),gvf_AlterGPCD_pct
                    real :: lvf_base,lvf_gpcdTarget,lvf_beta,lvf_gamma
                    real :: lvf_newGPCD(3),fCreateTarget
                    real,parameter:: lpf_maxAlterGPCD=85

                    real(8):: m,b
               
                    logical:: success,FindAlphaBeta,lvl_raw
                    ! ================================================
                        !
                        lvf_newGPCD(1)=lvf_tempGPCD(3)
                        lvf_newGPCD(2)= max(lvf_newGPCD(1)+0.01*lvf_newGPCD(1),lvf_tempGPCD(2))
                        lvf_newGPCD(3)= max(lvf_newGPCD(2)+0.01*lvf_newGPCD(2),lvf_tempGPCD(1))
                         !
                        gvf_dynamicGPCD(3,provider)=lvf_newGPCD(3);  gvf_dynamicGPCD(2,provider)=lvf_newGPCD(2);gvf_dynamicGPCD(1,provider)=lvf_newGPCD(1)
                        ! here, beta is alpha from Holt, and gamma is beta (because of the other alpha in this subroutine!!!)
                           lvf_base=lvf_newGPCD(1)
                            gvf_AlterGPCD_pct=(min(lpf_maxAlterGPCD,gvf_AlterGPCD_pct))
                            !
                              lvf_beta=0.6
                              lvf_gamma=0.7
                              success=.true.
                            !
                            lvf_gpcdTarget=fCreateTarget(lvf_base,gvf_AlterGPCD_pct)
                            !
                            call sFindAlphaBetaStart(lvf_gpcdTarget,lvf_newGPCD,lvf_beta,lvf_gamma,FindAlphaBeta)
                            !
                            if(FindAlphaBeta)then
                              call sHoltTarget(lvf_newGPCD,lvf_gpcdTarget,lvf_beta,lvf_gamma,success,cycles)
                               do i = 1,3,1
                                lvd_LinearRegression(provider,i)=0
                               end do
                                !
                                gvf_betaHoltGPCD(provider)=lvf_beta
                                gvf_gammaHoltGPCD(provider)=lvf_gamma
                            else
                              !     
                              if(.not. lvl_raw)then
                                call sLinearRegression(lvf_base,lvf_gpcdTarget,m,b)
                                !
                                lvd_LinearRegression(provider,1)=1
                                lvd_LinearRegression(provider,2)=m
                                lvd_LinearRegression(provider,3)=b
                             !
                              endif
                             gvf_betaHoltGPCD(provider)=0
                             gvf_gammaHoltGPCD(provider)=0
                            endif
                            !
                         !
                        if(.not. success)goto 20
                    return
20              continue
                  if(gvl_writeLog)then
                    gvl_errorFlag=.false.
                        LU=0
                        string=55
                        call sStrings(string,errorString)
                        call eWrite(errorString,LU)
                  endif

                end subroutine sFindBetaGamma
                ! ----------------------------

                ! ----------------------------
                subroutine sSetGPCD(provider,lvl_raw,lvf_newGPCD)
                    ! ------------- Types ------------
                    integer:: provider

                    real:: lvf_newGPCD(3)

                    logical:: lvl_raw
                    ! ================================
                        !
                        if(lvl_raw)then   
                          lvf_newGPCD(3)= gvf_dynamicGPCD(3,provider)
                          lvf_newGPCD(2)= gvf_dynamicGPCD(2,provider)
                          lvf_newGPCD(1)= gvf_dynamicGPCD(1,provider)
                          gvf_oneGPCD(provider)=lvf_newGPCD(1)
                        else
                          lvf_newGPCD(3)= gvf_dynamicGPCD(2,provider)
                          lvf_newGPCD(2)= gvf_dynamicGPCD(1,provider)
                          lvf_newGPCD(1)= gvf_oneGPCD(provider)
                        endif
                        !
                  return
                end subroutine sSetGPCD
                ! ----------------------------

            ! End of Holt
            ! ========================================================================================
!
End Module lms_ProviderPopandDemand
!
    ! --------------------------------------
    subroutine readGPCDglobal(i,lvf_GPCDavg)
        use lms_ProviderPopandDemand
        !
        ! ------------- Types ------------
        integer :: i

        real :: lvf_GPCDavg(5,gvi_maxProV)   
        ! ================================
            !
            call readNewGPCD(i,lvf_GPCDavg)
            !
        !
     return
    end subroutine readGPCDglobal
    ! ----------------------------

    ! ----------------------------------------
    subroutine readProviderGPCD(i,lvf_GPCDavg)
        use lms_ProviderPopandDemand
        !
        ! ----------------- Types ----------------
        integer :: i

        real :: lvf_GPCDavg(2000:2013,gvi_maxProV)   
        ! ========================================
            !
            call readNewGPCD(i,lvf_GPCDavg)
            !
        !
     return
    end subroutine readProviderGPCD
    ! -----------------------------

    ! Not currently used; as of 03.25.14
    ! -----------------------------------------------
    function fOutDoorEstimate(pop,demand,lvf_days,bwProp)
        use gm_GlobalData
        !
        ! ---------------- Types -------------------
!        integer :: days

        real,parameter :: lpf_gallonsPerFlush=3.63 
        real,parameter :: lpf_flushesPerDay=5.05
        real,parameter :: lpf_blackWaterProp=0.27

        real :: fOutDoorEstimate
        real :: lvf_BWtypical_acft
        real :: pop,demand,bwProp
        real :: lvf_days
        ! ==========================================
            !
            lvf_BWtypical_acft= pop*(lpf_gallonsPerFlush*lpf_flushesPerDay*lvf_days)*1/gpd_galperacft
            ! 
            if(0. < demand)then
              if(0 < bwProp)then
                fOutDoorEstimate = (1-((1./demand) * (lvf_BWtypical_acft/bwProp)) )
              else
                fOutDoorEstimate = 1-((1./demand) * (lvf_BWtypical_acft/lpf_blackWaterProp))
              endif
            endif
            !
        !
    end function fOutDoorEstimate
    ! ----------------------------

    ! ----------------------------------------------------------
    function fGPCDmethodValues(provider) result(lvi_functionGPCD)
        use gm_GlobalData
        !
        ! -------------- Types ----------------
        integer :: provider,gvi_GPCDmethod
        integer :: alterGPCD,lvi_functionGPCD
        ! =====================================
        !
         alterGPCD=0
        alterGPCD=provider
        ! 10.17.13
        if(0 < alterGPCD)then
          gvi_GPCDmethod=3
        else if (alterGPCD < 0)then
          gvi_GPCDmethod=2  
        else
          gvi_GPCDmethod=1
        endif
        !
         lvi_functionGPCD=gvi_GPCDmethod
        !
    end function
    ! --------------------------

    ! ---------------------------------------
    ! --------------------------

    ! ---------------------------------------------------------
    subroutine pUnmetDemand(T,gvi_order,lvl_flag,lvl_Interrupt)
     use gm_ModelControl
      use gm_TypeControl
       use lms_ProviderPopandDemand
        !
        ! ---------- Types --------------
        integer :: i
        integer(1) :: gvi_order

        real :: code=gpi_unmet

        logical :: lvl_Interrupt
        logical :: lvl_flag(gvi_maxProV)
        ! ===============================
        !

        ! -- Type Constructs --
        type(runTime)T
        ! =====================
          !
          ! =======================================================
          ! Unmark these if I once again use feedback loops
!          if(lvl_Interrupt)goto 1
           lvl_Interrupt=.false.
          do i = 1,gvi_Providers,1
            gvf_WaterDemand_acft(i,gvi_order,6)=code
          end do
          !
          do i = 1,gvi_Providers,1
            lvl_flag(i)=.false.
            gvl_parm_shortage(i)=.false.
              !
              ! Deficit year or situation.
              if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
              else
                lvl_flag(i)=.false.
                gvl_parm_shortage(i)=.false.
              endif
              !
              call sParameterControl_tstep(T,i)
              !
            end do
            !
            if(lvl_Interrupt)then
            endif  
            goto 10
1          continue    
            lvl_Interrupt=.false.
10         continue
            !
            gvi_order=gvi_order+1
            ! =======================================================
            !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=19
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
        !
      return
    end subroutine pUnmetDemand
    ! --------------------------

    ! -----------------------------
    subroutine pDemand(T,gvi_order)
     use gm_ModelControl
      use lms_ProviderPopandDemand
        !
        ! -------- Types ---------
        integer :: i
        integer(1) :: gvi_order

        real :: code=1
        ! ========================
        !

        ! - Type Constructs -
        type(runTime)T
        ! ===================
        !
            ! =======================================================
            do i = 1,gvi_Providers,1
               !
                 gvf_WaterDemand_acft(i,gvi_order,1)=  gvf_WaterDemand_acft(i,1,1) 
                 gvf_WaterDemand_acft(i,gvi_order,2)=  gvf_WaterDemand_acft(i,1,2)
                 gvf_WaterDemand_acft(i,gvi_order,3)=  gvf_WaterDemand_acft(i,1,3) 
               !
                 gvf_WaterDemand_acft(i,gvi_order,7)=  gvf_WaterDemand_acft(i,1,7) 
                 gvf_WaterDemand_acft(i,gvi_order,8)=  gvf_WaterDemand_acft(i,1,8)
                 gvf_WaterDemand_acft(i,gvi_order,9)=  gvf_WaterDemand_acft(i,1,9) 
                 gvf_WaterDemand_acft(i,gvi_order,10)=  gvf_WaterDemand_acft(i,1,10) 

                gvf_WaterDemand_acft(i,gvi_order+1,1)=  gvf_WaterDemand_acft(i,1,1) 
                gvf_WaterDemand_acft(i,gvi_order+1,2)=  gvf_WaterDemand_acft(i,1,2)
                gvf_WaterDemand_acft(i,gvi_order+1,3)=   gvf_WaterDemand_acft(i,1,3)
               !
               gvf_WaterDemand_acft(i,gvi_order,6)=code
               !
                gvf_WaterDemand_acft(i,gvi_order+1,7)=   gvf_WaterDemand_acft(i,1,7)
                gvf_WaterDemand_acft(i,gvi_order+1,8)=   gvf_WaterDemand_acft(i,1,8)
                gvf_WaterDemand_acft(i,gvi_order+1,9)=   gvf_WaterDemand_acft(i,1,9)
                gvf_WaterDemand_acft(i,gvi_order+1,10)=   gvf_WaterDemand_acft(i,1,10)

            end do
            !
            gvi_order=gvi_order+1
            !
                !
                if(T%year .EQ. T%startyear)then
                    if(gvl_writeLog)then
                        string=6
                        LU=0
                        call sOnceThrough(string,streamString)
                        call sWrite(streamString,LU)
                    endif
                endif
                !
           ! =======================================================
      !
    return
  end subroutine pDemand
  ! ------------------------

 
  ! -------------------------
  subroutine pGlobalDemand(T)
    use gm_ModelControl
     use gm_TypeControl
      use lms_ProviderPopandDemand
        !
        ! ---- Types ------
        integer :: i,j,k
        ! =================
        !

        !-- TYPE constructs --
        type(runTime)T
        ! ====================
          ! 
          ! 15 Defined but only 12 used at the moment
            do i = 1,gvi_Providers,1
              do j = 1,18,1
                do k = 1,14,1
                  gvf_WaterDemand_acft(i,j,k)=0
                end do
              end do
            end do
          !
          call calcDemand(T)
          !
    return
  end subroutine pGlobalDemand
  ! --------------------------


  ! ----------------------------------------------------------------------------
  !
  ! Utilities 
  ! ------------------------
  subroutine sClearDemand(T)
    use gm_ModelControl
     use gm_TypeControl
      use lms_ProviderPopandDemand
        !
        !----- Types -----
        integer :: i,j,k
        ! ================
        !

        !-- TYPE constructs --
        type(runTime)T
        ! ====================
          !
          ! Zero out the array each year 
          if(gpl_DemandDriven)then
            do i = 1,gvi_Providers,1
                do j = 1,18,1
                 if(1 < j)then
                   do k = 1,14,1
                    if(3 < k)then
                     gvf_WaterDemand_acft(i,j,k)=0
                    endif
                   end do
                 endif
                end do
            end do
          endif
          !
    return
  end subroutine sClearDemand
  ! -------------------------

        ! ------------------------------------
        subroutine ProviderArea2012(vArea2012)
            use lms_ProviderPopandDemand
            !
            ! ------------------ Types ------------------------
            integer :: i

            real,parameter :: lvc_AcresToSqMeters=4046.8564224
            real :: vArea2012(gvi_maxProV) ! m2
            ! =================================================
            !
                !
                do i = 1,gvi_Providers,1
                  vArea2012(i)=0
                 vArea2012(i)=lvc_AcresToSqMeters* (lid_providerAcres(i,1) + lid_providerAcres(i,2)) 
                end do  
                !
            !
         return
        end subroutine ProviderArea2012
        ! -----------------------------

       ! ---------------------------------------
        subroutine ProviderArea2012i(i,Relative)
            use lms_ProviderPopandDemand
            !
            ! -------------------- Types ---------------------
            integer :: i

            real,parameter :: lvc_AcresToSqMeters=4046.8564224
            real :: vArea2012(gvi_maxProV) ! m2
            real :: Relative(gvi_maxProV)
            ! ================================================
            !
                !
                  vArea2012(i)=0
                 vArea2012(i)=lvc_AcresToSqMeters* (lid_providerAcres(i,1) + lid_providerAcres(i,2)) 
                !
                 Relative(i)=0
                if(0 < gvf_providerAreaTotal_m2)Relative(i)=vArea2012(i)/gvf_providerAreaTotal_m2
                !
            !
          return
        end subroutine ProviderArea2012i
        ! ------------------------------

       ! -----------------------------------------
       subroutine ProviderAreaAg_2012(i,vArea2012)
            use lms_ProviderPopandDemand
            !
            ! ------------------ Types ------------------------
            integer :: i,j,k

           ! real,parameter :: lvc_AcresToSqMeters=4046.8564224
            real :: vArea2012(gvi_maxProV) ! m2
            real :: temp(gvi_maxProV)
            real :: vArea(gvi_maxProV)
            ! =================================================
            !
                temp=0
                !
                call ProviderArea2012(vArea)

                  vArea2012(i)=0
                ! 05.24.16
                ! vArea2012(i)=lvc_AcresToSqMeters* (lid_providerAcres(i,1) + lid_providerAcres(i,2)) 
                !
                ! 05.24.16
                do j = 1,gpi_LULC
                  do k = 1,gvi_maxProV
                    if(j == 1)then
                     temp(k) = gvf_landCover_2010(j,k)
                    endif
                  end do
                end do
                !
                vArea2012(i)=(temp(i)* 0.01)*vArea(i)
                !
            !
           return
        end subroutine ProviderAreaAg_2012
       ! ------------------------------------

       ! ------------------------------------
       subroutine ProviderAreaAg_2012r(i,Relative)
            use lms_ProviderPopandDemand
            !
            ! -------------------- Types ---------------------
            integer :: i

            real,parameter :: lvc_AcresToSqMeters=4046.8564224
            real :: vArea2012(gvi_maxProV) ! m2
            real :: Relative(gvi_maxProV)
            ! ================================================
            !
                !
                 call sMask(gvl_mask)
                 Relative(22)=0
                 Relative(23)=0
                 if(gvl_mask(i))then
                    vArea2012(i)=0
                    vArea2012(i)=lvc_AcresToSqMeters* (lid_providerAcres(i,1) + lid_providerAcres(i,2)) 
                    !
                    Relative(i)=0
                    if(0 < gvf_providerAreaTotal_m2)Relative(i)=vArea2012(i)/gvf_providerAreaTotal_m2
                endif
                !
            !
           return
        end subroutine ProviderAreaAg_2012r
       ! ----------------------------------
!
! ======================================================================================================
! E.O.F. ProviderPopulationandDemand.f90