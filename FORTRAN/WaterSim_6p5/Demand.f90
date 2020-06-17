!
! File is Demand.f90
!
! This file controls added demand for the FORTRAN model
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
! No Modules:   subroutine pRainWaterHarvest(T,gvi_order,lvl_flag)  
!               subroutine pWaterBanking_k(T,gvi_order,interrupt)
!               subroutine pWaterForDirectInject_s(T,gvi_order,lvl_flag)
!               subroutine pWaterReclaimed_s(T,gvi_order)
!               subroutine pROWaterReclaimed_s(T,gvi_order)
!               subroutine pWaterForVadose_s(T,gvi_order,lvl_flag)
!               subroutine sCWaccounting(gvi_order,lvf_in_1,lvf_in_2,code)
!

! created on 01.16.13
!
! david arthur sampson

! last write was: 04.01.13,07.18.14,04.18.19
! ------------------------------------------
!

! ===========================================================================================================

    ! --------------------------
    subroutine pStartUp_k(T)
      use lms_ParameterControl
       use gm_ModelControl
        !  
        ! - Type Constructs -
        type(runTime)T
        ! ===================
        !
        if(gvl_IwaniecYN)then
          gvl_LULCstatus=.true.
          gvl_waterSim5YN=.false.
        endif
        !
      return
    end subroutine pStartUp_k
    ! -------------------------
    ! =============================================================================================
    ! WaterSim 6 Additions
    ! -------------------------------
    subroutine sNonPotableAvail(T,ratio,lvf_rainHarvesting,lvf_stormWater,lvf_grayWater)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        ! 
        ! ---------------- Types --------------
        integer :: i
        real :: lvf_rainWaterHarvest_mm
        real :: lvf_rainHarvesting(gvi_maxProV)
        real :: rain,ratio(gvi_maxProV)
        real :: lvf_grayWater(gvi_maxProV)
        real :: lvf_stormWater(gvi_maxProV)
        ! =====================================
        ! - Type Constructs -
        type(runTime)T
        ! ===================

            !
            do i = 1,gvi_Providers,1
              ratio(i)=0
              lvf_stormWater(i)=0
              lvf_grayWater(i)=0
            end do
            !
            if(gvl_IncludeMeteorology)then
                do i = 1,gvi_Providers,1
                   rain = lvf_MetDataProbability(T%year,i,3) ! 
                  call sRainfallHarvesting(T,i,rain,lvf_rainWaterHarvest_mm,lvf_rainHarvesting)
                   if(0 < gvf_WaterDemand_acft(i,1,1))ratio(i) =gvf_WaterDemand_acft(i,1,3)/gvf_WaterDemand_acft(i,1,1)
                    !
                    lvf_stormWater(i)=0
                    !
                end do
            endif
            if(gvl_parm_grayWater)then
                do i = 1,gvi_Providers,1
                   lvf_grayWater(i)=go_GrayWaterReclaimed_acft(i,1)+go_GrayWaterReclaimed_acft(i,2)+go_GrayWaterReclaimed_acft(i,3)               
                end do
            endif
            !
      return
    end subroutine sNonPotableAvail
    ! -------------------------------
    ! -------------------------------------------
    subroutine pGrayWater(T,gvi_order,lvl_flag)
     use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! -------------------- Types ---------------------------------
         integer :: i,j
         integer(1) :: gvi_order

         real :: lvf_grayWater(gvi_maxProV)
         real :: lvf_usedGrayWater(gvi_maxProV)
         real :: lvf_totalOutdoorDemand
         real :: code=gpi_grayWater

         logical :: lvl_flag(gvi_maxProV)
        ! ============================================================

        ! - Type Constructs -
        type(runTime)T
        ! ===================
          !
          ! ===========================
          do i = 1,gvi_Providers,1
             lvf_grayWater(i)=0
             lvf_usedGrayWater(i)=0
             !
           if(lvl_flag(i))then
           else
             if(gvl_parm_grayWater)then
                !
                if(gvi_baseYear <= T%year)then
                  !
                  ! From the previous time-step
                  ! 04.18.16
                  ! --------------------------------------------------------------------
                  do j = 1,3,1
                   lvf_grayWater(i)= lvf_grayWater(i) + go_GrayWaterReclaimed_acft(i,j)
                  end do
                else
                endif
                !
                if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
                  ! Check to see if there is outdoor demand for use
                  ! of harvested water. If no demand, goes to unused pool
                  ! 04.18.16
                  ! -----------------------------------------------------           
                    lvf_usedGrayWater(i)=0
                    lvf_totalOutdoorDemand=gvf_WaterDemand_acft(i,gvi_order,8)+gvf_WaterDemand_acft(i,gvi_order,10)
                  !
                  if( 0 < lvf_totalOutdoorDemand)then
                    if( lvf_grayWater(i) <= lvf_totalOutdoorDemand)then
                      lvf_usedGrayWater(i)=lvf_grayWater(i)
                    else
                      lvf_usedGrayWater(i)=lvf_totalOutdoorDemand
                        !
                        gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1) &
                        + (lvf_grayWater(i) - lvf_totalOutdoorDemand)
                    !
                      call sOutdoorAccounting(i,gvi_order,(lvf_grayWater(i) - lvf_totalOutdoorDemand))
                    !
                    endif

                  else
                    gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1) &
                        + lvf_grayWater(i)
                    call sOutdoorAccounting(i,gvi_order,lvf_grayWater(i))

                  endif             

                else
                     
                endif
               !
             else
                !
                 call sDemandWrap(i,gvi_order)
                !

             endif
            !
           endif
            gvf_WaterDemand_acft(i,gvi_order,6)=code
            !
          end do
            !

             gvl_InOutDemandArray=.true. 
            call sCWaccountingIndoorOut(gvi_order,code,lvf_usedGrayWater,gvl_InOutDemandArray)
          !

          ! ===================================================================================
            !
            gvi_order=gvi_order+1
            !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=17
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
          !
      return
    end subroutine pGrayWater
    ! -------------------------------------------

    ! ---------------------------------------------------
     subroutine pStormWaterHarvest(T,gvi_order,lvl_flag)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! -------------------- Types ---------------------------------
         integer :: i
         integer(1) :: gvi_order

         real :: lvf_WaterHarvesting(gvi_maxProV)
         real :: lvf_WaterHarvestStormWater(gvi_maxProV)
         real :: lvf_harvesting(gvi_maxProV)
         real :: lvf_totalOutdoorDemand
         real :: code=gpi_stormWaterCapture

         logical :: lvl_flag(gvi_maxProV)
        ! ============================================================

        ! - Type Constructs -
        type(runTime)T
        ! ===================
          !
          ! ================================
           do i = 1,gvi_Providers,1
             lvf_WaterHarvestStormWater(i)=0
             lvf_harvesting(i)=0
             lvf_WaterHarvesting(i)=0
             lvf_totalOutdoorDemand=0
             !
            if(lvl_flag(i))then

            else
                !
                if(gvl_IncludeMeteorology)then
                    if(gvl_stormWaterHarvesting)then
                    ! Add to the demand stream so that it can be accounted for
                    ! ---------------------------------------------------------------------
                        ! Bring in from Citi_model, Time T-1
                        ! 11.30.15 DAS
                        !
                        if(gvi_baseYear <= T%year)then
                            lvf_WaterHarvesting(i)=0
                            !
                            if(gvl_stormWaterHarvesting)then
                            lvf_WaterHarvestStormWater(i)=go_HarvestStormWater_AF(i)
                            lvf_WaterHarvesting(i)= max(0,lvf_WaterHarvesting(i)+lvf_WaterHarvestStormWater(i))
                            endif    
                         else
                        endif
                        !
                      if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
                          ! Check to see if there is outdoor demand for use
                          ! of harvested water. If no demand, goes to unused pool
                          ! 04.18.16
                          ! -----------------------------------------------------           
                            lvf_harvesting(i)=0
                            lvf_totalOutdoorDemand=gvf_WaterDemand_acft(i,gvi_order,8)+gvf_WaterDemand_acft(i,gvi_order,10)
                          !
                          if( 0 < lvf_totalOutdoorDemand)then
                            if(lvf_WaterHarvesting(i) <= lvf_totalOutdoorDemand)then
                              lvf_harvesting(i)=lvf_WaterHarvesting(i)
                            else
                              lvf_harvesting(i)=lvf_totalOutdoorDemand
                                gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1) &
                                + (lvf_WaterHarvesting(i) - lvf_totalOutdoorDemand)
                              !
                              call sOutdoorAccounting(i,gvi_order,(lvf_WaterHarvesting(i) - lvf_totalOutdoorDemand))
                              !
                            endif
                          else
                            gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1) &
                                + lvf_WaterHarvesting(i)
                              !
                              call sOutdoorAccounting(i,gvi_order,lvf_WaterHarvesting(i))
                              !
                          endif
                      endif
                     !
                     gvf_WaterDemand_acft(i,gvi_order,6)=code
                     !
                    else
                       !
                        call sDemandWrap(i,gvi_order)
                       !
                    endif
                else
                    !
                     call sDemandWrap(i,gvi_order)
                    !
                endif
            endif
            !
            gvf_WaterDemand_acft(i,gvi_order,6)=code
            !
           end do
           !         
           if(gvl_stormWaterHarvesting)then
            !
                gvl_InOutDemandArray=.true.
              call sCWaccountingIndoorOut(gvi_order,code,lvf_harvesting,gvl_InOutDemandArray)
            !
           endif
          !
          ! =================================================================================

            gvi_order=gvi_order+1
          !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=7
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
        !
      return
    end subroutine pStormWaterHarvest
    ! --------------------------------

    ! --------------------------------------------
     subroutine pRainWaterHarvest(T,gvi_order,lvl_flag)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! -------------------- Types ---------------------------------
         integer :: i
         integer(1) :: gvi_order

         real :: lvf_WaterHarvesting(gvi_maxProV)
         real :: lvf_rainHarvesting(gvi_maxProV)
         real :: lvf_harvesting(gvi_maxProV),lvf_rainWaterHarvest_mm
         real :: lvf_totalOutdoorDemand
         real :: rain,lvf_reduceRain
         real :: code=gpi_rainWaterHarvest

         logical :: lvl_flag(gvi_maxProV)
        ! ============================================================

        ! - Type Constructs -
        type(runTime)T
        ! ===================
          !
          ! ================================
           do i = 1,gvi_Providers,1
             lvf_harvesting(i)=0
             lvf_WaterHarvesting(i)=0
             lvf_totalOutdoorDemand=0
             lvf_reduceRain=0
             !
            if(lvl_flag(i))then

            else
                !
                 gvf_rainFall(i)=0
                if(gvl_IncludeMeteorology)then
                 call RainfallFactor(T%year,lvf_reduceRain)
                 gvf_rainFall(i)=lvf_MetDataProbability(T%year,i,3)*lvf_reduceRain
                endif
                !
                if(gvl_IncludeMeteorology)then
                      !
                      go_rainWaterHarvested_SF_AF(i)=0
                      go_rainWaterHarvested_MF_AF(i)=0
                      go_rainWaterHarvested_PU_AF(i)=0
                      go_rainWaterHarvested_COM_AF(i)=0
                      !
                        ! 01.24.2019 - commented this out
                        !
                        !
                    if(gvl_rainWaterHarvesting)then
                    ! Add to the demand stream so that it can be accounted for
                    ! ---------------------------------------------------------------------
                        ! Bring in from Citi_model, Time T-1
                        ! 11.30.15 DAS
                        rain = lvf_MetDataProbability(T%year,i,3)*lvf_reduceRain ! 
                        !
                        ! Provider area (m2)
                        !
                        if(gvi_baseYear <= T%year)then
                              lvf_WaterHarvesting(i)=0
                            if(gvl_rainWaterHarvesting)then
                              call sRainfallHarvesting(T,i,rain,lvf_rainWaterHarvest_mm,lvf_rainHarvesting)
                            endif
                            ! Add in Storm water captured
                            lvf_WaterHarvesting(i)=lvf_rainHarvesting(i) 
                        else
                         lvf_rainWaterHarvest_mm=0
                        endif
                        !
                        ! Pass to CityModel.f90
                        ! To be used for runoff, percolation, and storm water capture (and evapotranspiration)
                        ! 04.01.16 das
                        gvf_rainFall(i) = max(0,lvf_MetDataProbability(T%year,i,3)*lvf_reduceRain - lvf_rainWaterHarvest_mm)
                        !
!write(104,34)T%year,i,gvf_rainFall(i)
34 format(I4,2x,I2,3x,F8.2)

                        !
                      if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
                          ! Check to see if there is outdoor demand for use
                          ! of harvested water. If no demand, goes to unused pool
                          ! 04.18.16
                          ! -----------------------------------------------------           
                            lvf_harvesting(i)=0
                            lvf_totalOutdoorDemand=gvf_WaterDemand_acft(i,gvi_order,8)+gvf_WaterDemand_acft(i,gvi_order,10)
                          !
                          if( 0 < lvf_totalOutdoorDemand)then
                            if(lvf_WaterHarvesting(i) <= lvf_totalOutdoorDemand)then
                              lvf_harvesting(i)=lvf_WaterHarvesting(i)
                            else
                              lvf_harvesting(i)=lvf_totalOutdoorDemand
                                gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1) &
                                + (lvf_WaterHarvesting(i) - lvf_totalOutdoorDemand)
                              !
                              call sOutdoorAccounting(i,gvi_order,(lvf_WaterHarvesting(i) - lvf_totalOutdoorDemand))
                              !
                            endif
                          else
                            gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1) &
                                + lvf_WaterHarvesting(i)
                              !
                              call sOutdoorAccounting(i,gvi_order,lvf_WaterHarvesting(i))
                              !
                          endif
                      endif
                     !
                     gvf_WaterDemand_acft(i,gvi_order,6)=code
                     !
                    else
                       !
                        call sDemandWrap(i,gvi_order)
                       !
                    endif
                else
                    !
                     call sDemandWrap(i,gvi_order)
                    !
                endif
            endif
            !
            gvf_WaterDemand_acft(i,gvi_order,6)=code
            !
           end do
           !         
           if(gvl_rainWaterHarvesting)then
            !
                gvl_InOutDemandArray=.true.
              call sCWaccountingIndoorOut(gvi_order,code,lvf_harvesting,gvl_InOutDemandArray)
            !
           endif
          !
          ! =================================================================================

            gvi_order=gvi_order+1
          !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=7
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
        !
      return
    end subroutine pRainWaterHarvest
    ! ------------------------
    
    ! ---------------------------------
    subroutine sRainfallHarvesting(T,i,rain,lvf_rainWaterHarvest_mm,lvf_rainHarvesting)
      use lms_CitiWaterBudgets
       use gm_ModelControl

        ! -------------- types --------------------
        integer :: i
        integer :: model
        real :: lvf_rainPotential_AF,rain
        real :: lvf_rainPotMulti_Fam_AF
        real :: lvf_rainPotentialPeriUrban_AF
        real :: lvf_lowDensityHousing,lvf_medDensityHousing,lvf_highDensityHousing
        real :: lvf_Area(gvi_maxProV) ! m2
        real :: lvf_rainHarvesting_res(gvi_maxProV)
        real :: lvf_rainHarvesting_resMF(gvi_maxProV)
        real :: lvf_rainHarvesting_resPI(gvi_maxProV)
        real :: lvf_rainHarvesting_totRes(gvi_maxProV)
        real :: lvf_rainHarvesting_com(gvi_maxProV)
        real :: lvf_rainHarvesting(gvi_maxProV)
        !
        real :: lvf_rainWaterHarvest_mm     
        real :: lvf_businesses,lvf_ratio,lpf_roofAreaBusiness
        real :: lvf_gallonsAvailable
        !
        real,parameter :: lpf_runoffCoefficient=0.90 ! http://ag.arizona.edu/pubs/water/az1052/harvest.html
        real,parameter :: lpf_runoffCoeffMultiFamily=0.90
        real, parameter :: lpf_evaporationCoeff=1.0
        !
        real,parameter :: lpf_roofAreaHomes=232 ! m2 or 2500 ft2 (two pitch roof [plus garage] 60*25 feet [conservative]) 1400 ft2 house
        real,parameter :: lpf_storageCapacity=1000 ! 1000 gallon storage tank $ 521.00 from plastic-mart
        real :: lvf_storage !,tempRain
        !integer :: code !,tempCode
              ! Only 50% of multi-family buildings adopt this policy (above and beyond the general adoption curve)
        ! i.e., 25 % overall (50% of 50%)
        !
        ! I am using a time-derived estimate of implementation of the code
        ! for rainwater harvesting (i.e., not all installations could happen in ONE YEAR)
        real :: lvf_periUrban,lvf_singleFamily,lvf_multiFamily 
        real :: lvf_actualHouses,lvf_actualBusinesses
        real :: lvf_compliance
        ! Milti-family based on the Hanover Building in downtown Tempe 
        ! Building has a pool in the middle - reduce roof area by 1000 m2
        ! Building is 90x92 + 12*42 = 8784 m2
        !  reduce 1000 m2 for pool area = 7784
        !  reduce for conservative estimate, by 12% = 6850 m2
        ! NEW Analysis
        real,parameter :: lpf_roofAreaMFamily=6850 ! active roof area
        real,parameter :: lpf_roofAreaPeriUrbanHomes=464 ! 2. x single family roof area
        !real, parameter:: lpf_numberOfStories=5 !using five story walkup
        real, parameter :: lpf_familysPerBuilding=60 ! number of units on one level - # of stories already reduced in LCLU.f90
        !
        REAL :: fLogistic
        !
        logical :: SustainableScenarios=.false.
        ! ============================================
            !
        ! - Type Constructs -
        type(runTime)T
        ! ===================
            ! Not used when LCLU=0
            model=0
            !
            go_harvestRainWater_AF(i)=0
                !
                  go_rainWaterHarvested_SF_AF(i)=0
                  go_rainWaterHarvested_MF_AF(i)=0
                  go_rainWaterHarvested_PU_AF(i)=0
                  go_rainWaterHarvested_COM_AF(i)=0
            !
            lvf_storage=0 !; code=0
            !
            if( T%year < gvi_baseYear)then
            else

              call ProviderArea2012(lvf_Area)
                !
                  ! AF year-1
                  ! Residential Houses
                  ! 
                   lvf_rainPotential_AF=0
                   lvf_rainPotMulti_Fam_AF=0
                   lvf_rainPotentialPeriUrban_AF=0
                   lvf_ratio=0
                   lvf_rainWaterHarvest_mm=0
                   lpf_roofAreaBusiness=0
                   !
                  lvf_rainHarvesting(i)=0
                  lvf_rainHarvesting_resMF(i)=0
                  lvf_rainHarvesting_resPI(i)=0
                  lvf_rainHarvesting_res(i)=0
                  lvf_rainHarvesting_totRes(i)=0
                  lvf_rainHarvesting_com(i)=0
                  !
                  lvf_periUrban=0
                  lvf_singleFamily=0
                  lvf_multiFamily=0
                  !
                  ! -------------------------------------------------------------------------------------
                  !
                  lvf_rainPotential_AF=lpf_roofAreaHomes * (rain * gpf_mmTometers) * gpf_cubicMetersToAF
                  lvf_rainPotMulti_Fam_AF=lpf_roofAreaMFamily * (rain * gpf_mmTometers) * gpf_cubicMetersToAF
                  lvf_rainPotentialPeriUrban_AF=lpf_roofAreaPeriUrbanHomes * (rain * gpf_mmTometers) * gpf_cubicMetersToAF
                  !
                  lvf_periUrban=gvf_houseHolds(1,i)
                  lvf_singleFamily=gvf_houseHolds(2,i)
                  lvf_multiFamily=gvf_houseHolds(3,i)
                  ! Notice. 06.05.18; Four story walkups... so, this must be accounted for see line 798 in LandCoverLandUse.f90
                  ! Do not need to duplicate the four stories, however
                  lvf_multiFamily=gvf_houseHolds(3,i)/lpf_familysPerBuilding
                  !

                   ! write(107,750)T%year,i,lvf_periUrban,lvf_singleFamily,lvf_multiFamily,gvf_houseHolds(3,i),gvf_lcluPopulations(3,i),gvf_lcluPopulations(2,i),gvf_lcluPopulations(1,i)

750 format(I4,1x,I2,1x,7(F10.2,1x))
                  !
                  ! 11.01.16
                  ! 12.10.16.. these labels match the terminology used in the Futures Scenario's Project. i.e., low density
                  ! is periUrban, medium density is actually single family homes, and high density is multi-family or urban cores
                  ! ===================================
                  !lvf_houses=lvf_singleFamily 
                  lvf_lowDensityHousing=lvf_periUrban
                  lvf_medDensityHousing=lvf_singleFamily 
                  lvf_highDensityHousing=lvf_multiFamily
                  !
                  ! residential land use land cover proportion = gvf_waterUseRatioLCLU(8,i) * area m2
                  !
                  policy=1; LCLU=0; span=10
                  !
                      lvf_actualHouses =0
                      lvf_compliance=0
                      gvf_YearsToInflection=0
                    !
                    if(gvf_RainGrayCompliance < 1)then
                        if(gvf_yearsToAdoptRainWater < 1)SustainableScenarios=.true.
                    endif
                    if(gvl_IwaniecYN)then
                        !
                        if(SustainableScenarios)then
                          call sSpanAndInflection(policy,gvi_IwaniecScenario,LCLU,span,lvf_compliance,gvf_YearsToInflection,model)
                        else
                          lvf_compliance=gvf_RainGrayCompliance*0.01
                          gvf_YearsToInflection=gvf_yearsToAdoptRainWater
                        endif
                        !
                    else
                      gvf_YearsToInflection=gvf_yearsToAdoptRainWater
                      lvf_compliance=1.0
                    endif
                    ! Provides an annual estimate of the increase in houses outfitted, and the compliance rate
                    !    
                     lvf_actualHouses = fLogistic(T%policyYear,lvf_compliance,span,gvf_YearsToInflection)
                    !
301 format(I4,3x,I2,3x,2(F10.5,5x)) 
                    !         
                  !                   
                  ! 11.01.16,12.12.16,12.13.16
                  lvf_rainHarvesting_res(i)= lpf_runoffCoefficient *  lvf_actualHouses  & 
                  * lvf_medDensityHousing * lvf_rainPotential_AF * lpf_evaporationCoeff
                  !
                  !lvf_rainHarvesting_resMF(i)= lpf_runoffCoeffMultiFamily *  lvf_actualHouses * lpf_MFamilyAdoption  & 
                  lvf_rainHarvesting_resMF(i)= lpf_runoffCoeffMultiFamily *  lvf_actualHouses  & 
                  * lvf_highDensityHousing * lvf_rainPotMulti_Fam_AF * lpf_evaporationCoeff
                  !
                  lvf_rainHarvesting_resPI(i)= lpf_runoffCoefficient *  lvf_actualHouses  & 
                  * lvf_lowDensityHousing * lvf_rainPotentialPeriUrban_AF  * lpf_evaporationCoeff

                    ! Assume a seven day rain - need a distribution of the volume (durnation * intensity) over the year
                    ! Somewhat conservative
                ! ------------------------------------------------------------------------------------------------------
                !
                if(0 < lvf_singleFamily)then
                    lvf_gallonsAvailable= 0
                   lvf_gallonsAvailable= ((lvf_rainHarvesting_res(i)* gpd_galperacft)/lvf_singleFamily)/12
                    lvf_storage=lpf_storageCapacity
                  !  code=2
                  if(lvf_storage < lvf_gallonsAvailable)then
                   lvf_rainHarvesting_res(i)=(lvf_storage/ gpd_galperacft) * lvf_singleFamily *12
                  endif
                endif

                ! Peri-Urban - 2000 gallon tanks total
                if(0 < lvf_periUrban)then
                    lvf_gallonsAvailable=0
                   lvf_gallonsAvailable= ((lvf_rainHarvesting_resPI(i)* gpd_galperacft)/lvf_periUrban)/12
                    lvf_storage=lpf_storageCapacity*2.0
                    !code=1
                  if(lvf_storage < lvf_gallonsAvailable)then
                   lvf_rainHarvesting_resPI(i)=(lvf_storage/ gpd_galperacft) * lvf_periUrban *12
                  endif
                endif
                !
               ! lvf_multiFamily - 5000 gallon tanks total
                if(0 < lvf_multiFamily)then
                    lvf_gallonsAvailable=0
                   lvf_gallonsAvailable= ((lvf_rainHarvesting_resMF(i)* gpd_galperacft)/lvf_multiFamily)/12
                     lvf_storage=lpf_storageCapacity*5
                    !  code=3
                  if(lvf_storage < lvf_gallonsAvailable)then
                   lvf_rainHarvesting_resMF(i)=(lvf_storage/ gpd_galperacft) * lvf_multiFamily *12
                  endif
                endif
                !
!                write(109,920)T%year,i,code,lvf_gallonsAvailable,lvf_storage,lvf_rainHarvesting_resMF(i)
920             format(I4,2x,I2,2x,I2,2x,3(F12.3,1x))
                !
                lvf_rainHarvesting_totRes(i)=lvf_rainHarvesting_res(i)+lvf_rainHarvesting_resMF(i)+lvf_rainHarvesting_resPI(i)
                !
                go_rainWaterHarvested_SF_AF(i)=nint(lvf_rainHarvesting_res(i))
                go_rainWaterHarvested_MF_AF(i)=nint(lvf_rainHarvesting_resMF(i))
                go_rainWaterHarvested_PU_AF(i)=nint(lvf_rainHarvesting_resPI(i))
                go_rainWaterHarvested_COM_AF(i)=0
                !
               ! 
                write(109,610)T%year,i,lvf_rainHarvesting_resPI(i),lvf_periUrban,go_OutDemand_lclu_AF(1,i), &
                    gvf_rainfallfactor,gv_PopGrowthRateAdjPct(i),gvi_efficiencyLCLUres,lvf_compliance,gvf_YearsToInflection
                write(110,710)T%year,i,lvf_rainHarvesting_res(i),lvf_singleFamily,go_OutDemand_lclu_AF(2,i), &
                    gvf_rainfallfactor,gv_PopGrowthRateAdjPct(i),gvi_efficiencyLCLUres,lvf_compliance,gvf_YearsToInflection
                write(111,810)T%year,i,lvf_rainHarvesting_resMF(i),lvf_multiFamily,go_OutDemand_lclu_AF(3,i), &
                    gvf_rainfallfactor,gv_PopGrowthRateAdjPct(i),gvi_efficiencyLCLUres,lvf_compliance,gvf_YearsToInflection
610             format(I4,1x,I2,1x,3(F13.2,1x),2(F8.2,2x),5x,I4,1x,2(F6.2,3x))
710             format(I4,1x,I2,1x,3(F13.2,1x),2(F8.2,2x),5x,I4,1x,2(F6.2,3x))
810             format(I4,1x,I2,1x,3(F13.2,1x),2(F8.2,2x),5x,I4,1x,2(F6.2,3x))
                !
              if(gvl_rainWaterHarvestResOnly)then
              else
                  ! AF year-1
                  ! Business (commercial and Industrial)- based on LandUseLandCover data
                  ! ------------------------
                   lvf_actualBusinesses=0
                   lvf_rainPotential_AF=0
                  ! -------------------------------------------------------------------------------------
                if(gvl_rainWaterHarvestComOnly)then
                    lvf_rainHarvesting_res(i)=0
                    policy=2
                    LCLU=0
                    span=10
                    !
                    if(gvl_IwaniecYN)then
                      call sSpanAndInflection(policy,gvi_IwaniecScenario,LCLU,span,lvf_compliance,gvf_YearsToInflection,model)
                    else
                     lvf_compliance=1.0
                    endif
                    !
                    if(0 < gvf_RainGrayCompliance)then
                      lvf_compliance=gvf_RainGrayCompliance*0.01
                    endif
                  !

                    lvf_actualBusinesses=fLogistic(T%policyYear,lvf_compliance,span,gvf_YearsToInflection)
                    !
                else
                     policy=2
                     LCLU=0
                     span=10
                    if(gvl_IwaniecYN)then
                      call sSpanAndInflection(policy,gvi_IwaniecScenario,LCLU,span,lvf_compliance,gvf_YearsToInflection,model)
                    else
                     lvf_compliance=1.0
                    endif
                    !
                    lvf_actualBusinesses=fLogistic(T%policyYear,lvf_compliance,span,gvf_YearsToInflection)
                endif
                    !
                  lpf_roofAreaBusiness=lvf_Area(i)*gvf_waterUseRatioLCLU(2,i)* lvf_actualBusinesses *gvf_parm_WStoCom_prop(i) &
                    + lvf_Area(i)*gvf_waterUseRatioLCLU(8,i)* lvf_actualBusinesses *gvf_parm_WStoCom_prop(i) &
                    + lvf_Area(i)*gvf_waterUseRatioLCLU(13,i)* lvf_actualBusinesses *gvf_parm_WStoCom_prop(i)
                    !
                  lvf_rainPotential_AF=lpf_roofAreaBusiness * (rain * gpf_mmTometers) * gpf_cubicMetersToAF
                  lvf_businesses=0.07 ! 15% of the business property is roof - Needs checking
                  ! 
!                    lvf_totalRain=lpf_roofAreaBusiness * (rain * gpf_mmTometers)  * lpf_runoffCoefficient * lvf_businesses

                  lvf_rainHarvesting_com(i)= (lpf_runoffCoefficient & 
                  * lvf_businesses * lvf_rainPotential_AF) 
                  !
!                write(108,910)T%year,i,lvf_rainHarvesting_com(i),lpf_roofAreaBusiness, lvf_rainPotential_AF, &
!                    gvf_rainfallfactor,gv_PopGrowthRateAdjPct(i),gvi_efficiencyLCLUres,gv_indexyearSVT,gv_indexyearCO
!910             format(I4,1x,I2,1x,3(F13.2,1x),2(F8.2,2x),5x,3(I4,3x))

              endif
            endif
                  !
                   go_rainWaterHarvested_COM_AF(i)=lvf_rainHarvesting_com(i)
                  !
                    lvf_ratio=0
                  if(0 < (rain * gpf_mmTometers*lvf_Area(i)*gpf_cubicMetersToAF ))then
                    lvf_ratio=(lvf_rainHarvesting_totRes(i)+ lvf_rainHarvesting_com(i)) /(rain * gpf_mmTometers*lvf_Area(i)*gpf_cubicMetersToAF )
                  endif
                    ! mm year-1
                    lvf_rainWaterHarvest_mm= rain *lvf_ratio

                    lvf_rainHarvesting(i)=lvf_rainHarvesting_totRes(i) + lvf_rainHarvesting_com(i)
                    ! ---------------

                    go_harvestRainWater_AF(i)= nint(lvf_rainHarvesting_totRes(i) + lvf_rainHarvesting_com(i))
                    ! ====================================================================================
                    !
10 format(I4,1x,I2,1x,3(F10.1,1x),1x,F7.3,1x,3(F8.1,1x))
30 format(I4,1x,I2,1x,2(F9.1,1x),F7.1)      
40 format(I4,3x,I2,3x,F12.1)            !
            !
      return
    end subroutine sRainfallHarvesting
    ! ---------------------------------
    ! =============================================================================================

    ! -------------------------------------------
    subroutine sDemandWrap(i,gvi_order)
     use lms_CitiWaterBudgets
       use gm_ModelControl

        ! -------- types --------
        integer :: i
        integer(1) :: gvi_order
        integer :: code=16
        ! =======================
            ! 
            ! What is this?
          !
          gvf_WaterDemand_acft(i,gvi_order,1)=  gvf_WaterDemand_acft(i,gvi_order,1) 
          gvf_WaterDemand_acft(i,gvi_order,2)=  gvf_WaterDemand_acft(i,gvi_order,2)
          gvf_WaterDemand_acft(i,gvi_order,3)=  gvf_WaterDemand_acft(i,gvi_order,3)
          !
          gvf_WaterDemand_acft(i,gvi_order,7)=  gvf_WaterDemand_acft(i,gvi_order,7) 
          gvf_WaterDemand_acft(i,gvi_order,8)=  gvf_WaterDemand_acft(i,gvi_order,8)
          gvf_WaterDemand_acft(i,gvi_order,9)=  gvf_WaterDemand_acft(i,gvi_order,9)
          gvf_WaterDemand_acft(i,gvi_order,10)=  gvf_WaterDemand_acft(i,gvi_order,10)
          ! -------------------------------------------------------------------------
          ! Move the demand to the next level- i.e. none added or removed on this step
          gvf_WaterDemand_acft(i,gvi_order+1,1)=  gvf_WaterDemand_acft(i,gvi_order,1) 
          gvf_WaterDemand_acft(i,gvi_order+1,2)=  gvf_WaterDemand_acft(i,gvi_order,2)
          gvf_WaterDemand_acft(i,gvi_order+1,3)=  gvf_WaterDemand_acft(i,gvi_order,3) 
          !
          gvf_WaterDemand_acft(i,gvi_order,6)=code
          !
          gvf_WaterDemand_acft(i,gvi_order+1,7)=  gvf_WaterDemand_acft(i,gvi_order,7) 
          gvf_WaterDemand_acft(i,gvi_order+1,8)=  gvf_WaterDemand_acft(i,gvi_order,8)
          gvf_WaterDemand_acft(i,gvi_order+1,9)=  gvf_WaterDemand_acft(i,gvi_order,9) 
          gvf_WaterDemand_acft(i,gvi_order+1,10)= gvf_WaterDemand_acft(i,gvi_order,10) 
        !
      return
    end subroutine sDemandWrap
    ! -------------------------

    ! ------------------------------------------------
    subroutine sOutdoorAccounting(i,gvi_order,lvf_add)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        ! -------- Types --------
        integer :: i
        integer(1) :: gvi_order
        real :: lvf_propDemand
        real :: lvf_add
        real :: lvf_outdoorDemand
        ! =======================
            !
             lvf_propDemand=0
             lvf_outdoorDemand=0
            lvf_outdoorDemand=gvf_WaterDemand_acft(i,gvi_order,10)+gvf_WaterDemand_acft(i,gvi_order,8)
            if(0 < lvf_outdoorDemand)then
               lvf_propDemand=gvf_WaterDemand_acft(i,gvi_order,10)/ &
                 (gvf_WaterDemand_acft(i,gvi_order,10)+gvf_WaterDemand_acft(i,gvi_order,8))
                !
                 gvf_WaterDemand_acft(i,gpi_unusedNonPotable,10)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,10) &
                    + lvf_propDemand*lvf_add
                 gvf_WaterDemand_acft(i,gpi_unusedNonPotable,8)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,8) &
                    + ((1-lvf_propDemand)*lvf_add)
                !
            else
              if(0 < gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1))then
               lvf_propDemand=gvf_WaterDemand_acft(i,gvi_order,3)/ gvf_WaterDemand_acft(i,gvi_order,1)
                !
                 gvf_WaterDemand_acft(i,gpi_unusedNonPotable,10)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,10) &
                    + lvf_propDemand*lvf_add
                 gvf_WaterDemand_acft(i,gpi_unusedNonPotable,8)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,8) &
                    + ((1-lvf_propDemand)*lvf_add)
                !
              endif
            endif
            !
      return
    end subroutine sOutdoorAccounting
    ! -------------------------------------------

    ! -----------------------------------------------
    subroutine pWaterBanking_k(T,gvi_order,interrupt)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! --------------------------------- Types --------------------------------
         integer :: i
         integer(1) :: gvi_order

         real :: lvf_Banking(gvi_maxProV)
         real :: code=gpi_banking

         logical :: interrupt
        ! ========================================================================
        !
    
        ! - Type Constructs -
        type(runTime)T
        ! ===================
          !
          ! =======================================================
          ! Note: only annual CityModel time step covered here.  Will need to change        
            do i = 1,gvi_Providers,1
              lvf_Banking(i)=0
              gvf_GW_Banking(T%year,i,1)=0
             !
              if(interrupt)then
                gvf_WBankingBalance(T%year,i,1)= gvf_GW_Banking_Hold(i)
              endif
            ! 10.14.13
            ! 
             if(0 < gvf_WBankingBalance(T%year,i,1))then
                if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
                    if(gvf_WaterDemand_acft(i,gvi_order,1) < gvf_WBankingBalance(T%year,i,1))then
                      lvf_Banking(i)=gvf_WaterDemand_acft(i,gvi_order,1)
                      gvf_WBankingBalance(T%year,i,1)= max(0,gvf_WBankingBalance(T%year,i,1)-lvf_Banking(i))
                    else
                      lvf_Banking(i)=max(0,gvf_WBankingBalance(T%year,i,1))
                      gvf_WBankingBalance(T%year,i,1)=0
                    endif     
                    ! Used in subroutine sSurfaceWater
                    gvf_GW_Banking(T%year,i,1)=lvf_Banking(i)                             
                endif
              !
              else
                lvf_Banking(i)=0
              endif
                !
                ! 10.16.14 DAS added to account for banked water in CAP used (if SRP is added, must adjust)
                ! NOTE: multi-year banked (possible), so could exceed the annual CAP designation.....!!!!
                 gvf_usedBankedWater_CAP(i)=0
                gvf_usedBankedWater_CAP(i)=lvf_Banking(i)
                !
            end do
!            !
               gvl_InOutDemandArray=.false.
              call sCWaccountingIndoorOut(gvi_order,code,lvf_Banking,gvl_InOutDemandArray)
          !
          gvi_order=gvi_order+1
          !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=17
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
          !
        return
    end subroutine pWaterBanking_k
    ! ----------------------------

    ! ------------------------------------------------------
    subroutine pWaterForDirectInject_s(T,gvi_order,lvl_flag)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! ------------------- Types ---------------------
         integer :: i
         integer(1) :: gvi_order

         real :: lvf_DirectInject,lvf_add,lvf_proportion
         real :: lvf_addIN_on,lvf_addIN_off
         real :: lvf_indoor_on,lvf_indoor_off

         real :: code=gpi_directInject

         logical :: lvl_flag(gvi_maxProV)
        ! ===============================================

        ! - TYpe Construct -
        type(runTime)T
        ! ==================
          !          
          ! ===========================
          !  This subroutine MUST be called first, as coded- gvf_GW_Banking(T%year,i,vTstep)
          ! -----------------------
          call sMaskCAP(gvl_maskCAP)
          !              
          do i = 1,gvi_Providers,1
            lvf_DirectInject=0
            lvf_proportion=0
            !
            if(gvl_maskCAP(i))then
              if(lvl_flag(i))then

              else
                  if(gvi_baseYear <= T%year)then
                   lvf_DirectInject=gvf_parm_WStoDIamount(i)
                  endif
                if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
                  lvf_proportion= gvf_WaterDemand_acft(i,gvi_order,2) * (1./gvf_WaterDemand_acft(i,gvi_order,1))
                endif
                  lvf_add=0
                  lvf_add= lvf_DirectInject * lvf_proportion
                  !
                    lvf_indoor_on=0
                    lvf_indoor_off=0
                    lvf_addIN_on=0
                    lvf_addIN_off=0
                   lvf_addIN_off=gvf_WaterDemand_acft(i,gvi_order,7)+ gvf_WaterDemand_acft(i,gvi_order,8)
                   lvf_addIN_on=gvf_WaterDemand_acft(i,gvi_order,9)+ gvf_WaterDemand_acft(i,gvi_order,10)
                   if(0 < lvf_addIN_off)lvf_indoor_off= gvf_WaterDemand_acft(i,gvi_order,7)/lvf_addIN_off
                   if(0 < lvf_addIN_on)lvf_indoor_on= gvf_WaterDemand_acft(i,gvi_order,9)/lvf_addIN_on
                   !
                gvf_WaterDemand_acft(i,gvi_order,1)=  gvf_WaterDemand_acft(i,gvi_order,1) + lvf_DirectInject            
                if(0 <  gvf_WaterDemand_acft(i,1,3))then
                  gvf_WaterDemand_acft(i,gvi_order,2)= anint(gvf_WaterDemand_acft(i,gvi_order,2)+ lvf_add)
                  gvf_WaterDemand_acft(i,gvi_order,3)= gvf_WaterDemand_acft(i,gvi_order,3)+ anint(lvf_DirectInject -lvf_add)
                    !
                    gvf_WaterDemand_acft(i,gvi_order,9)=  gvf_WaterDemand_acft(i,gvi_order,9) + lvf_indoor_on     * (lvf_DirectInject* (1-lvf_proportion ))
                    gvf_WaterDemand_acft(i,gvi_order,10)= gvf_WaterDemand_acft(i,gvi_order,10) + (1-lvf_indoor_on)* (lvf_DirectInject* (1-lvf_proportion ))
                    gvf_WaterDemand_acft(i,gvi_order,7)= gvf_WaterDemand_acft(i,gvi_order,7)+  lvf_indoor_off     * (lvf_DirectInject* lvf_proportion )
                    gvf_WaterDemand_acft(i,gvi_order,8)= gvf_WaterDemand_acft(i,gvi_order,8)+  (1-lvf_indoor_off) * (lvf_DirectInject* lvf_proportion )
                    !
                else
                  gvf_WaterDemand_acft(i,gvi_order,2)= gvf_WaterDemand_acft(i,gvi_order,2)+ lvf_DirectInject
                  gvf_WaterDemand_acft(i,gvi_order,3)=  gvf_WaterDemand_acft(i,gvi_order,3)+ 0
                    !
                    gvf_WaterDemand_acft(i,gvi_order,7)=  gvf_WaterDemand_acft(i,gvi_order,7) +  lvf_indoor_off * lvf_DirectInject
                    gvf_WaterDemand_acft(i,gvi_order,8)=   gvf_WaterDemand_acft(i,gvi_order,8) + lvf_DirectInject - (lvf_indoor_off * lvf_DirectInject)
                    !
                endif
              endif
            endif
              !         
                  !
                    call sDemandWrap(i,gvi_order)
                  !
            gvf_WaterDemand_acft(i,gvi_order,6)=code
            !
          end do
          !
         gvi_order=gvi_order+1
         ! 
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=9
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
         !     
      return
    end subroutine pWaterForDirectInject_s
    ! -------------------------------------

    ! ---------------------------------------
    subroutine pWaterReclaimed_s(T,gvi_order)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! ------------------- Types -----------------------------
        integer :: i
        integer(1) :: gvi_order

        real :: lvf_reclaimed(gvi_maxProV)
        real :: lvf_totalRecDemand
        real :: code=gpi_reclaimed
        real :: lvf_demand
        ! =======================================================
        !
        ! - Type Constructs -
        type(runTime)T
        ! ===================
          !
        do i = 1,gvi_Providers,1      
           !
           lvf_reclaimed(i)=0
           lvf_demand=0
            lvf_demand=gvf_WaterDemand_acft(i,gvi_order,1)
            lvf_totalRecDemand=0
            gvf_reclaimedUseOutdoors(i)=0
           !
          if(0 < lvf_demand)then
            if(gpi_timeStepWB /= 1)then
            else
              !
                  ! We have default settings for reclaimed water - No baseYear determination of use
                  ! From the previous time-step
                  ! 04.18.16
                  ! -----------------------------------------------------------------------------------------
                if(2000 < T%year)then
                  lvf_totalRecDemand=gvf_WaterDemand_acft(i,gvi_order,8)+gvf_WaterDemand_acft(i,gvi_order,10)

                    if(0 < gvf_reclaimedOutput(T%year-1,i,1))then
                        if(gvf_reclaimedInputMax(T%year,i) < gvf_reclaimedOutput(T%year-1,i,1))then
                         gvf_reclaimedInput(T%year,i,1)=gvf_reclaimedInputMax(T%year,i)
                        else
                         gvf_reclaimedInput(T%year,i,1)= gvf_reclaimedOutput(T%year-1,i,1)
                        endif  
                      !
                      ! Check to see if there is outdoor demand for use (das jan)
                      ! of reclaimed water. If no demand, goes to unused pool
                      ! 04.18.16
                      ! -----------------------------------------------------           
                      !
                        gvf_reclaimedUseOutdoors(i)=0
                      if(0 < lvf_totalRecDemand)then
                        !
                        lvf_reclaimed(i)= gvf_reclaimedInput(T%year,i,1)*gvf_parm_ReclaimedOutdoor(i)
                        !
                        if(lvf_reclaimed(i) <= lvf_totalRecDemand)then
                         ! 
                        else
                            gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1) &
                            + (lvf_reclaimed(i) - lvf_totalRecDemand)
                          !
                        endif
                      else
                        lvf_reclaimed(i)=0
                        gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1) &
                            + gvf_reclaimedInput(T%year,i,1)
                      endif
                      !
                    else
                    endif

                  !-------------------------------------------------------------------
                else
                    ! Initialize
                   if(0 < gvf_reclaimedOutput(T%year,i,1))then
                        if(gvf_reclaimedInputMax(T%year,i) < gvf_reclaimedOutput(T%year,i,1))then
                         gvf_reclaimedInput(T%year,i,1)=gvf_reclaimedInputMax(T%year,i)
                        else
                         gvf_reclaimedInput(T%year,i,1)= gvf_reclaimedOutput(T%year,i,1)
                        endif                
                     lvf_reclaimed(i)=gvf_reclaimedInput(T%year,i,1)
                    endif
                    ! 04.16.19 added the following four lines of code
                    gvf_reclaimedInput(T%year,i,1) = &
                          (gvf_WaterDemand_acft(i,1,1)*(1-gvf_parm_OutDoorResProp(i)) &
                          * gvf_parm_RtoOutput(i)*(1-gvf_parm_RWWtoRO(i))*gvf_parm_WWtoRWWTP(i) &
                          * gvf_parm_RtoInputMaxPct(i) ) * 1.6 ! fudge to create starting value
                    !
                    lvf_reclaimed(i)=gvf_reclaimedInput(T%year,i,1)
                    !
                endif
              !
            endif
           !
          else
          endif
        end do
        !
!               call sCWaccounting(gvi_order,lvf_off,lvf_on,code)
                gvl_InOutDemandArray=.true.
              call sCWaccountingIndoorOut(gvi_order,code,lvf_reclaimed,gvl_InOutDemandArray)
 
        !
        gvi_order=gvi_order+1
        !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=10
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
        !
      return
    end subroutine pWaterReclaimed_s
    ! -------------------------------

    ! -----------------------------------------
    subroutine pROWaterReclaimed_s(T,gvi_order)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! ------------------------- Types -------------------------
        integer :: i,j
        integer(1) :: gvi_order
        real,parameter :: lpf_fudge=0.85
        real :: lvf_ROreclaimed(gvi_maxProV)

        real :: code=gpi_roReclaimed
        real :: lvf_demand
        ! =========================================================
        !

        ! - Type Constructs -
        type(runTime)T
        ! ===================
          !
          ! =======================================================
          do i = 1,gvi_Providers,1
           lvf_ROreclaimed(i)=0
           lvf_demand=0
           lvf_demand=gvf_WaterDemand_acft(i,gvi_order,1)
           !
           if(0 < lvf_demand)then
            if(gpi_timeStepWB /= 1)then
              do j = 1,12,1
!                lvf_ROreclaimed(i)=lvf_ROreclaimed(i)+gvf_ROreclaimedInput(T%year-1,i,j)
              end do
            else
                if(2000 < T%year)then
                    if(0 < gvf_ROreclaimedOutput(T%year-1,i,1))then
                        lvf_ROreclaimed(i)=gvf_ROreclaimedInput(T%year,i,1)
                    endif
                else
                    ! 04.16.19 das estimate
                         gvf_ROreclaimedInput(T%year,i,1)= &
                           gvf_WaterDemand_acft(i,1,1)* (1-gvf_parm_OutDoorResProp(i)) &
                            *  gvf_parm_RWWtoRO(i)* gvf_parm_ROtoOutput(i)*gvf_parm_WWtoRWWTP(i) * lpf_fudge
                    !
                    if(0 <  gvf_ROreclaimedInput(T%year,i,1))then
                        lvf_ROreclaimed(i)=gvf_ROreclaimedInput(T%year,i,1)
                    endif
                    !
                endif          
            endif
            !
           else
           endif
          end do
          !
                gvl_InOutDemandArray=.false.
              call sCWaccountingIndoorOut(gvi_order,code,lvf_ROreclaimed,gvl_InOutDemandArray)
          !
          gvi_order=gvi_order+1
          !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=11
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
          !
     return
    end subroutine pROWaterReclaimed_s
    ! --------------------------------

    ! --------------------------------------------------
     subroutine pWaterForVadose_s(T,gvi_order,lvl_flag)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! ------------------------- Types ----------------------
         integer :: i
         integer(1) :: gvi_order

         real :: lvf_Vadose(gvi_maxProV),lvf_add,lvf_proportion
         real :: lvf_addIN_on,lvf_addIN_off
         real :: lvf_indoor_on,lvf_indoor_off
         real :: code=gpi_vadose

         logical :: lvl_flag(gvi_maxProV)
        ! ======================================================

        ! - Type COnstructs -
        type(runTime)T
        ! ===================
          !
          call sMaskCAP(gvl_maskCAP)
          !
          ! ===========================
          if(gvl_start)then
            do i = 1,gvi_Providers,1
              lvl_flag(i)=.false.
              gvl_parm_shortage(i)=.false.
            end do
          endif
          do i = 1,gvi_Providers,1
            !
            lvf_proportion=0
            !
            if(gvl_maskCAP(i))then
              if(lvl_flag(i))then
                lvf_Vadose(i)=0
                !
              else
                !
                lvf_Vadose(i)=0
                
                ! Add to the demand so that I can remove later
                ! ---------------------------------
                  if(gvi_baseYear <= T%year)then
                    lvf_Vadose(i)= gvf_parm_SWtoVadoseAmt(i) 
                  endif
                  !
                  if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
                    lvf_proportion= gvf_WaterDemand_acft(i,gvi_order,2)/gvf_WaterDemand_acft(i,gvi_order,1) 
                  endif

                  lvf_add=0
                  lvf_add=lvf_Vadose(i)*lvf_proportion
                  !
                    lvf_indoor_on=0
                    lvf_indoor_off=0
                    lvf_addIN_on=0
                    lvf_addIN_off=0
                   lvf_addIN_off=gvf_WaterDemand_acft(i,gvi_order,7)+ gvf_WaterDemand_acft(i,gvi_order,8)
                   lvf_addIN_on=gvf_WaterDemand_acft(i,gvi_order,9)+ gvf_WaterDemand_acft(i,gvi_order,10)
                   if(0 < lvf_addIN_off)lvf_indoor_off= gvf_WaterDemand_acft(i,gvi_order,7)/lvf_addIN_off
                   if(0 < lvf_addIN_on)lvf_indoor_on= gvf_WaterDemand_acft(i,gvi_order,9)/lvf_addIN_on


                  if(0 <  gvf_WaterDemand_acft(i,1,3))then
                    gvf_WaterDemand_acft(i,gvi_order,1)=  gvf_WaterDemand_acft(i,gvi_order,1) + lvf_Vadose(i)
                    gvf_WaterDemand_acft(i,gvi_order,2)=  anint(gvf_WaterDemand_acft(i,gvi_order,2) + lvf_add)
                    gvf_WaterDemand_acft(i,gvi_order,3)=  gvf_WaterDemand_acft(i,gvi_order,3) + anint(lvf_Vadose(i)-lvf_add)
                    !
                    gvf_WaterDemand_acft(i,gvi_order,9)=  gvf_WaterDemand_acft(i,gvi_order,9) + lvf_indoor_on     * (lvf_Vadose(i)* (1-lvf_proportion ))
                    gvf_WaterDemand_acft(i,gvi_order,10)= gvf_WaterDemand_acft(i,gvi_order,10) + (1-lvf_indoor_on)* (lvf_Vadose(i)* (1-lvf_proportion ))
                    gvf_WaterDemand_acft(i,gvi_order,7)= gvf_WaterDemand_acft(i,gvi_order,7)+  lvf_indoor_off     * (lvf_Vadose(i)* lvf_proportion )
                    gvf_WaterDemand_acft(i,gvi_order,8)= gvf_WaterDemand_acft(i,gvi_order,8)+  (1-lvf_indoor_off) * (lvf_Vadose(i)* lvf_proportion )
                    !
                  else
                    gvf_WaterDemand_acft(i,gvi_order,1)=  gvf_WaterDemand_acft(i,gvi_order,1) + lvf_Vadose(i)
                    gvf_WaterDemand_acft(i,gvi_order,2)=  gvf_WaterDemand_acft(i,gvi_order,2) + lvf_Vadose(i) 
                    gvf_WaterDemand_acft(i,gvi_order,3)=  gvf_WaterDemand_acft(i,gvi_order,3)
                    !
                    gvf_WaterDemand_acft(i,gvi_order,7)=  gvf_WaterDemand_acft(i,gvi_order,7) +  lvf_indoor_off * lvf_Vadose(i)
                    gvf_WaterDemand_acft(i,gvi_order,8)=   gvf_WaterDemand_acft(i,gvi_order,8) + lvf_Vadose(i) - (lvf_indoor_off * lvf_Vadose(i))
                    !
                  endif

                endif
                !
              endif
                  !
                    call sDemandWrap(i,gvi_order)
                  !
               !
               gvf_WaterDemand_acft(i,gvi_order,6)=code
               !
            end do
            !
            gvi_order=gvi_order+1
            !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=8
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
          !
      return
    end subroutine pWaterForVadose_s
    ! ------------------------------

   ! ------------------------------------------
    subroutine pNewWaterSupplies_k(T,gvi_order)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! --------------------------- Types -------------------------
         integer :: i
         integer(1) :: gvi_order

         real :: lvf_NewSupplies(gvi_maxProV)
         real :: lvf_AugmentedInput(gvi_maxProV)
         real :: code=gpi_newSupplies
        ! ===========================================================
        !
    
        ! - TYpe Constructs -
        type(runTime)T
        ! ===================
          !
          ! =======================================================
          !    Note: only annual CityModel time step covered here.  Will need to change        
            ! This output was added on 03.02.14 as water augmentation- I choose to use the
            ! output variable so that I do not need to create a new internal variable. The
            ! variable is called "go_WaterFromAgSurface_acft_a(i)"
            !
            ! 10.11.15. das- I removed Ag surface water from this routine and put it with
            ! added credits to the groundwater credit bucket in Water_CityModel.f90
            ! --------------------------------
            do i = 1,gvi_Providers,1
                if(T%atStartOfSimulation)then
                   gvf_newSuppliesBalance_acft(T%year,i)=0
                   lvf_NewSupplies(i)=0
                   lvf_AugmentedInput(i)=0
                else
                  if(gvi_baseYear <= T%year)then
!                     gvf_newSuppliesBalance_acft(T%year,i)= &
!                    gvf_newSuppliesBalance_acft(T%year,i)+gvi_newWaterSupplies_acft_a(i)!+go_WaterFromAgSurface_acft_a(i)
                    ! 01.10.2016 DAS
                    if( 0 < gvi_newWaterSupplies_acft_a(i))then
!                     gvf_newSuppliesBalance_acft(T%year,i)= gvi_newWaterSupplies_acft_a(i)
                    ! 03.04.19 ... not sure what the fuck I was doing before... now, its as a percentage of demand (as originally formulated)
                        lvf_AugmentedInput(i)=gvi_newWaterSupplies_acft_a(i)
                     gvf_newSuppliesBalance_acft(T%year,i)= (lvf_AugmentedInput(i)/100.0) * gvf_WaterDemand_acft(i,gvi_order,1)
                    endif

                  else
                    gvf_newSuppliesBalance_acft(T%year,i)=0
                  endif
                endif
                !
                lvf_NewSupplies(i)=0
                gvf_newSuppliesUsed_acft_a(i)=0
                !
                if(0 < gvf_newSuppliesBalance_acft(T%year,i))then
                    if(gvf_newSuppliesBalance_acft(T%year,i) < gvf_WaterDemand_acft(i,gvi_order,1))then
                        !
                        lvf_NewSupplies(i)=gvf_newSuppliesBalance_acft(T%year,i)
                        !
                    else
                        !
                        lvf_NewSupplies(i)=gvf_WaterDemand_acft(i,gvi_order,1)
                        !
                    endif
                      !
                else
                    !
                    lvf_NewSupplies(i)=0
                    !
                endif
                !
                ! Used
                ! 
                 gvf_newSuppliesUsed_acft_a(i)=lvf_NewSupplies(i)
                gvf_newSuppliesBalance_acft(T%year+1,i)=gvf_newSuppliesBalance_acft(T%year,i)-lvf_NewSupplies(i)
                !
            end do
            !
               gvl_InOutDemandArray=.false.
             call sCWaccountingIndoorOut(gvi_order,code,lvf_NewSupplies,gvl_InOutDemandArray)

          !
          gvi_order=gvi_order+1
          !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=16
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
         !
        return
    end subroutine pNewWaterSupplies_k
    ! ----------------------------------

   ! --------------------------
    subroutine pCleanUp_k(T)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !  
        ! - TYpe Constructs -
        type(runTime)T
        ! ===================
          !

          !
      return
    end subroutine pCleanUp_k
    ! ----------------------------------

   ! --------------------------
    subroutine pUnusedNonPotable(T,gvi_order)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !  
        !
        ! --------------------------- Types -------------------------
         integer :: i
         integer(1) :: gvi_order
         real :: code=gpi_unusedNonPotable
        ! ===========================================================

        ! - TYpe Constructs -
        type(runTime)T
        ! ===================
          !
            do i = 1,gvi_Providers,1
               gvf_WaterDemand_acft(i,gvi_order,6)=code
            end do
          !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=25
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
         !
      return
    end subroutine pUnusedNonPotable
    ! ----------------------------------


    ! --------------------------------------------------------
    subroutine sCWaccounting(gvi_order,lvf_in_1,lvf_in_2,code)
     use lms_CitiWaterBudgets
        !
        ! ------------------------ Types ---------------------
        integer :: i
        integer(1) :: gvi_order

        real :: lvf_in_1(gvi_maxProV),lvf_in_2(gvi_maxProV)
        real :: code
        ! ====================================================     
          !  
          do i = 1,gvi_Providers,1
           ! in_1 is "off-project", in_2 is "on-project"
           gvf_WaterDemand_acft(i,gvi_order,4)= anint(max(0,lvf_in_1(i)))
           gvf_WaterDemand_acft(i,gvi_order,5)= anint(max(0,lvf_in_2(i)))
           gvf_WaterDemand_acft(i,gvi_order,6)=code
           !  
              ! -------------------------------------------
               gvf_WaterDemand_acft(i,gvi_order+1,2)=0
              if(0 < gvf_WaterDemand_acft(i,gvi_order,2))then
                   gvf_WaterDemand_acft(i,gvi_order+1,2)= &
                    anint(max(0,(gvf_WaterDemand_acft(i,gvi_order,2)- gvf_WaterDemand_acft(i,gvi_order,4))))
              endif
              !
               gvf_WaterDemand_acft(i,gvi_order+1,3)=0
              if(0 < gvf_WaterDemand_acft(i,gvi_order,3))then
                 gvf_WaterDemand_acft(i,gvi_order+1,3)= &
                  anint(max(0,(gvf_WaterDemand_acft(i,gvi_order,3)- gvf_WaterDemand_acft(i,gvi_order,5))))
              endif
            !
               gvf_WaterDemand_acft(i,gvi_order+1,1)=0
            if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
             gvf_WaterDemand_acft(i,gvi_order+1,1)= anint( &
                max(0,gvf_WaterDemand_acft(i,gvi_order,1) &
                - (gvf_WaterDemand_acft(i,gvi_order,4) + gvf_WaterDemand_acft(i,gvi_order,5)  ) ))
            endif
            !      
          end do
          !
        return
    end subroutine sCWaccounting
    ! --------------------------

    ! ---------------------------------------
    subroutine sOptimize(i,gvi_order,lvf_inputs,lvf_in_1,lvf_out_1,lvf_in_2,lvf_out_2)
      use lms_CitiWaterBudgets
        !
        ! ------------------- Types ------------------------
        integer :: i
        integer(1) :: gvi_order
        !real :: lvf_propDemand,lvf_propDemandOutdoor
        real :: lvf_inputs(gvi_maxProV)
        real :: lvf_in_1(gvi_maxProV),lvf_out_1(gvi_maxProV)
        real :: lvf_in_2(gvi_maxProV),lvf_out_2(gvi_maxProV)
        real :: on,off,off_out,on_out,off_in,on_in,total
        ! ==================================================
        !
        off=0
        on=0
        off_out=0
        on_out=0
        off = gvf_WaterDemand_acft(i,gvi_order,7) + gvf_WaterDemand_acft(i,gvi_order,8)
        on = gvf_WaterDemand_acft(i,gvi_order,9) + gvf_WaterDemand_acft(i,gvi_order,10)
        total=off+on
        !
        if(0 < total)off_out= gvf_WaterDemand_acft(i,gvi_order,8)/total
        if(0 < total)on_out = gvf_WaterDemand_acft(i,gvi_order,10)/total
        if(0 < total)on_in = gvf_WaterDemand_acft(i,gvi_order,9)/total
        if(0 < total)off_in = gvf_WaterDemand_acft(i,gvi_order,7)/total

        !
         if(0 < lvf_inputs(i))then
              if(0 < on)then
                if(0 < gvf_WaterDemand_acft(i,gvi_order,9))then
                 lvf_in_2(i)=lvf_inputs(i) * on_in !  Indoor water use On
!                  lvf_in_2(i)=min(gvf_WaterDemand_acft(i,gvi_order,9), lvf_inputs(i) * on_in) !  Indoor water use On

                endif
                if(0 < gvf_WaterDemand_acft(i,gvi_order,10))then
                 lvf_out_2(i)= lvf_inputs(i) * on_out
                 !lvf_out_2(i)=min(gvf_WaterDemand_acft(i,gvi_order,10), lvf_inputs(i) * on_out)   ! Outdoor water use On
                endif
              endif
              !
              if(0 < off)then
                if(0 < gvf_WaterDemand_acft(i,gvi_order,7))then
                 lvf_in_1(i)=  lvf_inputs(i) * off_in
                 !lvf_in_1(i)=  min(gvf_WaterDemand_acft(i,gvi_order,7),(lvf_inputs(i)* off_in)) !* (1-lvf_dynamic)   !  Indoor Off
                endif
                if(0 < gvf_WaterDemand_acft(i,gvi_order,8))then
                 lvf_out_1(i) = lvf_inputs(i) * off_out
                 !lvf_out_1(i)=min(gvf_WaterDemand_acft(i,gvi_order,8),lvf_inputs(i) * off_out ) ! Outdoor water use Off    
                endif
              endif
         endif

        ! ------
     return
    end subroutine sOptimize
    ! ---------------------------------------

  ! ---------------------------------------
    subroutine sOptimize_OFF(i,gvi_order,lvf_inputs,lvf_in_1,lvf_out_1)
      use lms_CitiWaterBudgets
        !
        ! ------------------- Types ------------------------
        integer :: i
        integer(1) :: gvi_order
        real :: lvf_inputs(gvi_maxProV)
        real :: lvf_in_1(gvi_maxProV),lvf_out_1(gvi_maxProV)
        real :: off,off_in
        ! ==================================================
        !
        off_in=0
        off = gvf_WaterDemand_acft(i,gvi_order,7) + gvf_WaterDemand_acft(i,gvi_order,8)
        !
        if(0 < off)off_in = gvf_WaterDemand_acft(i,gvi_order,7)/off
        !
         if(0 < lvf_inputs(i))then
              if(0 < off)then
                if(0 < gvf_WaterDemand_acft(i,gvi_order,7))then
                 lvf_in_1(i)= min( gvf_WaterDemand_acft(i,gvi_order,7),  lvf_inputs(i) * off_in)
                endif
                if(0 < gvf_WaterDemand_acft(i,gvi_order,8))then
                 lvf_out_1(i)= min(gvf_WaterDemand_acft(i,gvi_order,8), lvf_inputs(i)  - lvf_in_1(i))
                endif
              endif
         endif

        ! ------
     return
    end subroutine sOptimize_OFF
    ! ---------------------------------------

    ! -----------------------------------------------------
    subroutine sOn_inRounding(i,gvi_order,lvf_in_2)
      use lms_CitiWaterBudgets

        ! ------- Types ----------
        integer :: i
        integer(1) :: gvi_order
        real :: lvf_in_2(gvi_maxProV)
        real :: lvf_difference
        ! ========================
        lvf_difference=0
       ! Catch rounding errors
        if(lvf_in_2(i) < gvf_WaterDemand_acft(i,gvi_order,9))then
            lvf_difference=gvf_WaterDemand_acft(i,gvi_order,9)-lvf_in_2(i)
            if(lvf_difference <= 1)then
              lvf_in_2(i)=gvf_WaterDemand_acft(i,gvi_order,9)
            endif
        endif
        !
      return
    end subroutine sOn_inRounding
    ! ---------------------------------------
  ! -----------------------------------------------------
    subroutine sOn_outRounding(i,gvi_order,lvf_out_2)
      use lms_CitiWaterBudgets

        ! ------- Types ----------
        integer :: i
        integer(1) :: gvi_order
        real :: lvf_out_2(gvi_maxProV)
        real :: lvf_difference
        ! ========================
        !
        lvf_difference=0
        if(lvf_out_2(i) < gvf_WaterDemand_acft(i,gvi_order,10))then
            lvf_difference=gvf_WaterDemand_acft(i,gvi_order,10)-lvf_out_2(i)
            if(lvf_difference <= 1)then
              lvf_out_2(i)=gvf_WaterDemand_acft(i,gvi_order,10)
            endif
        endif
      return
    end subroutine sOn_outRounding
    ! ---------------------------------------
  ! -----------------------------------------------------
    subroutine sOn_Rounding(i,gvi_order,lvf_in_2,lvf_out_2)
      use lms_CitiWaterBudgets

        ! ------- Types ----------
        integer :: i
        integer(1) :: gvi_order
        real :: lvf_in_2(gvi_maxProV)  
        real :: lvf_out_2(gvi_maxProV)
        real :: lvf_difference
        ! ========================
        !
        lvf_difference=0
        ! Catch rounding errors
        if(lvf_in_2(i) < gvf_WaterDemand_acft(i,gvi_order,9))then
            lvf_difference=gvf_WaterDemand_acft(i,gvi_order,9)-lvf_in_2(i)
            if(lvf_difference <= 1)then
              lvf_in_2(i)=gvf_WaterDemand_acft(i,gvi_order,9)
            endif
        endif
        !
        lvf_difference=0
        if(lvf_out_2(i) < gvf_WaterDemand_acft(i,gvi_order,10))then
            lvf_difference=gvf_WaterDemand_acft(i,gvi_order,10)-lvf_out_2(i)
            if(lvf_difference <= 1)then
              lvf_out_2(i)=gvf_WaterDemand_acft(i,gvi_order,10)
            endif
        endif
      return
    end subroutine sOn_Rounding
    ! ---------------------------------------

  ! -----------------------------------------------------
    subroutine sOff_Rounding(i,gvi_order,lvf_in_1,lvf_out_1)
      use lms_CitiWaterBudgets

        ! ------- Types ----------
        integer :: i
        integer(1) :: gvi_order
        real :: lvf_in_1(gvi_maxProV)  
        real :: lvf_out_1(gvi_maxProV)
        real :: lvf_difference
        ! ========================
        !
        lvf_difference=0
        ! Catch rounding errors
        if(lvf_in_1(i) < gvf_WaterDemand_acft(i,gvi_order,7))then
            lvf_difference=gvf_WaterDemand_acft(i,gvi_order,7)-lvf_in_1(i)
            if(lvf_difference <= 1.5)then
              lvf_in_1(i)=gvf_WaterDemand_acft(i,gvi_order,7)
            endif
        endif
        !
        lvf_difference=0
        if(lvf_out_1(i) < gvf_WaterDemand_acft(i,gvi_order,8))then
            lvf_difference=gvf_WaterDemand_acft(i,gvi_order,8)-lvf_out_1(i)
            if(lvf_difference <= 1.5)then
              lvf_out_1(i)=gvf_WaterDemand_acft(i,gvi_order,8)
            endif
        endif
      return
    end subroutine sOff_Rounding
    ! ---------------------------------------

    ! ------------------------------------------------------------------
    subroutine sCWaccountingIndoorOut(gvi_order,code,lvf_inputs,lvl_InOut)
     use lms_CitiWaterBudgets
        !
        ! ------------------------ Types ---------------------
        integer :: i,j,case_code
        integer(1) :: gvi_order
        real :: lvf_propDemandOutdoor
        real :: lvf_propDemand
        real :: lvf_inputs(gvi_maxProV)
        real :: lvf_on(gvi_maxProV), lvf_off(gvi_maxProV)
        real :: lvf_temp_IN,lvf_temp_OUT

        real :: lvf_in_1(gvi_maxProV),lvf_out_1(gvi_maxProV)
        real :: lvf_in_2(gvi_maxProV),lvf_out_2(gvi_maxProV)
        real :: lvf_demand

        real :: code

        logical :: lvl_InOut
        ! ====================================================     
        !  
        do i = 1,gvi_Providers,1
            lvf_temp_IN=0
            lvf_temp_OUT=0
            !
            lvf_on(i)=0
            lvf_off(i)=0
            lvf_propDemandOutdoor=0
            lvf_propDemand=0
            !         
            lvf_in_1(i)=0
            lvf_in_2(i)=0
            lvf_out_1(i)=0
            lvf_out_2(i)=0
            !
            ! --------------------------------
            !
            if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
             lvf_propDemand=gvf_WaterDemand_acft(i,gvi_order,3)/gvf_WaterDemand_acft(i,gvi_order,1)
              if(lvl_InOut)then
                ! On project, but outdoor demand driven
                if(0 <(gvf_WaterDemand_acft(i,gvi_order,10)+gvf_WaterDemand_acft(i,gvi_order,8)))&
                 lvf_propDemand=gvf_WaterDemand_acft(i,gvi_order,10)/ &
                  (gvf_WaterDemand_acft(i,gvi_order,10)+gvf_WaterDemand_acft(i,gvi_order,8))


              endif
            endif
            !
               lvf_temp_IN= gvf_WaterDemand_acft(i,gvi_order,7)+ gvf_WaterDemand_acft(i,gvi_order,9)
               lvf_temp_OUT=gvf_WaterDemand_acft(i,gvi_order,8)+ gvf_WaterDemand_acft(i,gvi_order,10)
              if(0 < (lvf_temp_IN+lvf_temp_OUT))lvf_propDemandOutdoor= lvf_temp_OUT/(lvf_temp_IN+lvf_temp_OUT)
            !
             case_code= nint(code)
            select case(case_code)
                !if(code == gpi_classA)then
                case (gpi_classA)
                  !
                  lvf_on(i) =lvf_inputs(i)
                  !
                  ! ---------------------------
                  lvf_in_2(i)=  lvf_on(i)  * (1-lvf_propDemandOutdoor)! lvf_propDemandIndoor)  ! Indoor water use On
                    !
                  lvf_out_2(i)= lvf_on(i)  - lvf_in_2(i) !* lvf_propDemandOutdoor  ! Outdoor water use On
                    call sOn_Rounding(i,gvi_order,lvf_in_2,lvf_out_2)
                    !
                  !
                !else if( code == gpi_ClassBC)then
                case (gpi_ClassBC)
                  lvf_on(i) =lvf_inputs(i)
                  !
                  ! ---------------------------
                  lvf_in_2(i)=  lvf_on(i)  * (1-lvf_propDemandOutdoor)! lvf_propDemandIndoor)  ! Indoor water use On
                    call sOn_inRounding(i,gvi_order,lvf_in_2)
                    !
                  lvf_out_2(i)= lvf_on(i) - lvf_in_2(i) ! * lvf_propDemandOutdoor  ! Outdoor water use On
                    call sOn_Rounding(i,gvi_order,lvf_in_2,lvf_out_2)

                  !
                !else if(code == gpi_cap)then
                case (gpi_cap)
                  !
                  if(0 < gvf_WaterDemand_acft(i,gvi_order,2))then
                    if(lvf_inputs(i) <= gvf_WaterDemand_acft(i,gvi_order,2))then
                      lvf_off(i)=lvf_inputs(i)
                    else
                      lvf_off(i)= gvf_WaterDemand_acft(i,gvi_order,2)
                      lvf_on(i) = lvf_inputs(i)- lvf_off(i)
                    endif
                      !
                        if(lvf_inputs(i) <= gvf_WaterDemand_acft(i,gvi_order,7)+ gvf_WaterDemand_acft(i,gvi_order,8))then
                        !
                         call sOptimize_OFF(i,gvi_order,lvf_inputs,lvf_in_1,lvf_out_1)
                          call sOff_Rounding(i,gvi_order,lvf_in_1,lvf_out_1)
                        !
                        else
                        !
                         call sOptimize(i,gvi_order,lvf_inputs,lvf_in_1,lvf_out_1,lvf_in_2,lvf_out_2)
                        !
                        endif
                        !
                      !
                  else
                    lvf_on(i)=lvf_inputs(i)
                    !
                    lvf_in_2(i)=lvf_inputs(i)  * (1-lvf_propDemandOutdoor)! lvf_propDemandIndoor)  ! Indoor water use On
                    lvf_out_2(i)= lvf_inputs(i) -  lvf_in_2(i) !* lvf_propDemandOutdoor  ! Outdoor water use On
                      call sOn_Rounding(i,gvi_order,lvf_in_2,lvf_out_2)

                    !
                  endif
                  !
                  ! -------------------
                  !
                !else if(code == gpi_groundWater)then
                case(gpi_groundWater)
                    !
                    lvf_demand=gvf_WaterDemand_acft(i,gvi_order,1)
                    ! Total demand > 0
                    if(0 < lvf_demand)then
                        ! Check on-project first
                       if(0 < gvf_WaterDemand_acft(i,gvi_order,3))then
                         !
                         if( 1 <= gvf_WaterDemand_acft(i,gvi_order,3))then
                          lvf_propDemand=gvf_WaterDemand_acft(i,gvi_order,3) * (1./lvf_demand)
                          ! 
                          lvf_on(i)=lvf_propDemand* lvf_inputs(i)
                          lvf_off(i)=lvf_inputs(i)-lvf_on(i)
                          !
                            !
                            call sOptimize(i,gvi_order,lvf_inputs,lvf_in_1,lvf_out_1,lvf_in_2,lvf_out_2)
                            !
                         else
                            ! give the 1 AF demand to off-project
                            lvf_off(i)=lvf_inputs(i)
                            !
                            lvf_in_1(i)=min(gvf_WaterDemand_acft(i,gvi_order,7),lvf_inputs(i)  * (1-lvf_propDemandOutdoor))! lvf_propDemandIndoor)  ! Indoor water use Off
                            lvf_out_1(i)=max(gvf_WaterDemand_acft(i,gvi_order,8),lvf_inputs(i) - lvf_in_1(i))  ! Outdoor water use Off
                            !
                         endif
                       else
                        lvf_off(i)=lvf_inputs(i)
                        !
                        lvf_in_1(i)= min(gvf_WaterDemand_acft(i,gvi_order,7),lvf_inputs(i)  * (1-lvf_propDemandOutdoor))! lvf_propDemandIndoor)  ! Indoor water use On
                        lvf_out_1(i)=max(gvf_WaterDemand_acft(i,gvi_order,8), lvf_inputs(i)  - lvf_in_1(i)) ! Outdoor water use On
                        !
                       endif
                    else
                    endif           
                !else
                case(gpi_newSupplies)

                   lvf_demand=gvf_WaterDemand_acft(i,gvi_order,1)
                    ! Total demand > 0
                   if(0 < lvf_demand)then
                     if(0 < lvf_inputs(i))then
                        ! Check on-project first
                       if(0 < gvf_WaterDemand_acft(i,gvi_order,3))then
                         !
                         if( 1 <= gvf_WaterDemand_acft(i,gvi_order,3))then
                          lvf_propDemand=gvf_WaterDemand_acft(i,gvi_order,3) * (1./lvf_demand)
                          ! 
                          lvf_on(i)=lvf_propDemand* lvf_inputs(i)
                          lvf_off(i)=lvf_inputs(i)-lvf_on(i)
                          !
                            !
                            call sOptimize(i,gvi_order,lvf_inputs,lvf_in_1,lvf_out_1,lvf_in_2,lvf_out_2)
                            !
                         else
                            ! give the 1 AF demand to off-project
                            lvf_off(i)=lvf_inputs(i)
                            !
                            lvf_in_1(i)=min(gvf_WaterDemand_acft(i,gvi_order,7),lvf_inputs(i)  * (1-lvf_propDemandOutdoor))! lvf_propDemandIndoor)  ! Indoor water use Off
                            lvf_out_1(i)=max(gvf_WaterDemand_acft(i,gvi_order,8),lvf_inputs(i) - lvf_in_1(i))  ! Outdoor water use Off
                            !
                         endif
                       else
                        lvf_off(i)=lvf_inputs(i)
                        !
                        lvf_in_1(i)= min(gvf_WaterDemand_acft(i,gvi_order,7),lvf_inputs(i)  * (1-lvf_propDemandOutdoor))! lvf_propDemandIndoor)  ! Indoor water use On
                        lvf_out_1(i)=max(gvf_WaterDemand_acft(i,gvi_order,8), lvf_inputs(i)  - lvf_in_1(i)) ! Outdoor water use On
                        !
                       endif
                     endif
                   else
                   endif           
                case default
                !
                  if(0 < lvf_inputs(i))then
                      lvf_on(i) =lvf_inputs(i)*lvf_propDemand
                      lvf_off(i)=lvf_inputs(i)-lvf_on(i)
                      !

                      ! For reclaimed water and rainwater and storm water
                      if(lvl_InOut)then
                       lvf_out_1(i)= lvf_off(i)  ! Outdoor water use Off
                       lvf_out_2(i)= lvf_on(i)   ! Outdoor water use On

                       lvf_in_1(i)=  0  ! Indoor water use Off Project
                       lvf_in_2(i)=  0  ! Indoor water use On
                        !
                      else
                        !
                        call sOptimize(i,gvi_order,lvf_inputs,lvf_in_1,lvf_out_1,lvf_in_2,lvf_out_2)
                         if(0 < lvf_propDemand)then
                          call sOn_Rounding(i,gvi_order,lvf_in_2,lvf_out_2)
                         else
                          call sOff_Rounding(i,gvi_order,lvf_in_1,lvf_out_1)
                         endif
                        !
                      endif   
                     !
                  else
                  endif
                                            
            !endif
            end select
            !
         end do
        !
        ! -------------------------------------------
          do i = 1,gvi_Providers,1
           ! in_1 is "off-project", in_2 is "on-project"
           gvf_WaterDemand_acft(i,gvi_order,4)= anint(max(0,lvf_off(i)))
           gvf_WaterDemand_acft(i,gvi_order,5)= anint(max(0,lvf_on(i)))
           gvf_WaterDemand_acft(i,gvi_order,6)=code
           !  
           gvf_WaterDemand_acft(i,gvi_order,11)= anint(max(0,lvf_in_1(i)))
           gvf_WaterDemand_acft(i,gvi_order,12)= anint(max(0,lvf_out_1(i)))
           gvf_WaterDemand_acft(i,gvi_order,13)= anint(max(0,lvf_in_2(i)))
           gvf_WaterDemand_acft(i,gvi_order,14)= anint(max(0,lvf_out_2(i)))
           !
           ! -------------------------------------------
            do j = 2,3,1
               gvf_WaterDemand_acft(i,gvi_order+1,j)=0
              if(0 < gvf_WaterDemand_acft(i,gvi_order,j))then
                   gvf_WaterDemand_acft(i,gvi_order+1,j)= &
                    anint(max(0,(gvf_WaterDemand_acft(i,gvi_order,j)- gvf_WaterDemand_acft(i,gvi_order,j+2))))
              endif
            end do
              !
               gvf_WaterDemand_acft(i,gvi_order+1,1)=0
            if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
             gvf_WaterDemand_acft(i,gvi_order+1,1)= anint( &
                max(0,gvf_WaterDemand_acft(i,gvi_order,1) &
                - (gvf_WaterDemand_acft(i,gvi_order,4) + gvf_WaterDemand_acft(i,gvi_order,5)  ) ))
            endif
            !    
            ! -------------------------------------------
            do j = 7,10,1
               gvf_WaterDemand_acft(i,gvi_order+1,j)=0
              if(0 < gvf_WaterDemand_acft(i,gvi_order,j))then
                   gvf_WaterDemand_acft(i,gvi_order+1,j)= &
                    anint(max(0,(gvf_WaterDemand_acft(i,gvi_order,j)- gvf_WaterDemand_acft(i,gvi_order,j+4))))
              endif
            end do
                ! 
          end do
          !

        return
    end subroutine sCWaccountingIndoorOut
    ! ------------------------------------

!
! =======================================================================================================
! E.O.F. Demand.f90