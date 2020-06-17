!
!      WaterSimDCDC Regional Water Demand and Supply Model Version 6.0

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
!



! ===================================================================================
Module lm_LULC
 use gm_ModelControl
  use gm_GlobalData
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
  contains
    !
    ! ---------------------
     subroutine initLULC()
       call openFiles_cm()
       call readFiles_cm()
      return
     end subroutine initLULC
    ! ---------------------

      ! ------------------------
      subroutine openFiles_cm()
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
              module="lm_LULC"
              !
               Infile='App_Data\Data\landUseLandCover_2010.txt'; LU=81
               call openFiles(module,lvc_DPath,Infile,LU)
              !
              Infile='App_Data\Data\lCLU_Scenario.txt'; LU=82
               call openFiles(module,lvc_DPath,Infile,LU)
              !
              Infile='App_Data\Data\lCLU_MedianAndPPH.txt'; LU=83
               call openFiles(module,lvc_DPath,Infile,LU)
              !

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
      end subroutine openFiles_cm
     ! ---------------------------

        ! ------------------------
        subroutine readFiles_cm()
            !
            ! -------- Types -------
            integer :: i,j,k,ios
!            integer :: k,l
            ! ======================
            ios=0
            !----------------
            ! Read from Disk
            ! Provider-level data;
            !--------------------------------------------------------------------------------------
            ! acft a-1
            ! 
            ! ---------------------
            read(81,*,err=810,iostat=ios)((gvf_landCover_2010(i,j),j=1,gvi_maxProV),i=1,gpi_LULC)      
            ! Missing "Other Provider"
            !
810          continue
            close(81)
            if(ios >0)then
             LU=81
             gvo_errorCode=80
             goto 1000
            endif
            ! ---------------------
            read(82,*,err=820,iostat=ios)((gvf_landCover_2060(i,j),j=1,gvi_maxProV),i=1,gpi_LULC)        
            ! Missing "Other Provider"
            !
820          continue
            close(82)
            if(ios >0)then
             LU=82
             gvo_errorCode=80
             goto 1000
            endif
            ! ----------------------
            !
            do j = 1,35,1
              do k = 1,3,1
               gvf_medianAndPPH(j,k)=0
              end do
            end do
            ! i=1- pplHH, i=2 - median income, i=3 - blank (at present)
            read(83,*,err=830,iostat=ios)((gvf_medianAndPPH(j,k),k=1,2),j=1,gvi_maxProV)      
            !
830          continue
            close(83)
            if(ios >0)then
             LU=83
             gvo_errorCode=80
             goto 1000
            endif
            ! ----------------------


         return
1000     continue
           if(gvl_writeLog)then
              string=62
                call sStrings(string,errorString)
                call eWrite(errorString,LU)
            endif
          gvl_errorFlag=.false.
          !
        end subroutine readFiles_cm
        ! --------------------------
!
End Module lm_LULC

! ---------------------------------
    subroutine initializeLULC ()
        use lm_LULC
        !
            call initLULC()
        !
      return
    end subroutine initializeLULC
    ! -----------------------------------

Module lms_LULC
 use gm_ModelControl
  use gm_GlobalData
    use gm_TypeControl
        !
        ! -----
        include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
        !
        real,parameter :: mpf_timeDifference=50
        real,parameter :: mpf_x1=2010
        real,parameter :: mpf_x2=2009
        !
  contains

        ! -----------------------------------------------------------------------------------------

        ! Surface water subroutines from rainfall
        ! ------------------------------------------ 
        subroutine ProviderRCN(T,Provider,lvf_xbarRCN)
            !
            ! ---------- Types -------------
            integer :: Provider,i

            real :: lvf_RCN(13) ! m2
            real ::lvf_PropLandCover(13)
            real :: lvf_add,lvf_totalRCN
            real :: lvf_xbarRCN(gvi_maxProV) ! individual provider-specific possible, though not yet used
            ! -------------------------------
            !
!            type (waterbalance)lvWB
            type(runTime)T
            ! ==============================================
                !
!                ! ------------------------------------
                 ! New RCN numbers for the Scenario Project
                 ! 03.06.16 DAS
                 lvf_RCN(1)=74 ! Ag
                 lvf_RCN(2)=89 ! building - use commercial business, soil group A
                 lvf_RCN(3)=10 ! canal - assuming some runoff
                 lvf_RCN(4)=61 ! Turf
                 lvf_RCN(5)=70 ! Greenway : Turf plus water plus impervious
                 lvf_RCN(6)=98 ! Impervious
                 lvf_RCN(7)=96 ! Mtn Vegetation
                 lvf_RCN(8)=75 ! residential - using 1/4 acre B soil group
                 lvf_RCN(9)=82 ! soil/desert - using "dirt Right-of-way" B hydrologic soil group
                 lvf_RCN(10)=68 ! tree - poor condition grass cover
                 lvf_RCN(11)=50 ! unclassified
                 lvf_RCN(12)=76 ! wash - using the "A hydrologic soil group for gravel
                 lvf_RCN(13)=0 ! water
                !http://onlinemanuals.txdot.gov/txdotmanuals/hyd/nrcs_runoff_curve_number_methods.htm
!                lvf_RCN(1)=61 ! Grass 39,61,74,80  correspond to A,B,C,D soil groups (see pub)
!                lvf_RCN(2)=98 ! Paved all (A,B,C,D) =98
!                lvf_RCN(3)=85 ! Gravel 76,85,89,91
!                lvf_RCN(4)=82 ! "Dirt" 72,82,87,89 
!                lvf_RCN(5)=77 ! Desert Landscaping 63,77,85,88
!                lvf_RCN(6)=96 ! Artificial Desert 96
!                lvf_RCN(7)=92 ! Commercial 89,92,94,95
!                lvf_RCN(8)=88 ! Industrial 81,88,91,93
!                lvf_RCN(9)=70 ! Residential -mesic-(using 1/2 acre lot) 54,70,80,85
!                lvf_RCN(10)=85 ! Residedntial -xerix- (using 1/8 acre lot size) 77,85,90,92
!                lvf_RCN(11)=83 ! "cultivated" 74,83,88,90
                !
!                if( T%year < gpi_lclu)then
!                  call sDefaultRCN(T,i,lvf_xbarRCN,lvf_PropLandCover)
!                else
!                  call sProportionalLandCover(T%year,Provider,T%lcluYear,lvf_PropLandCover)
!                  call sLandUseLandCoverArea(T%lcluYear,Provider,lvf_PropLandCover)
!                endif
                !               
                lvf_totalRCN=0.
                lvf_add=0.
                !
                do i = 1,gpi_LULC,1
                !
                lvf_PropLandCover(i)=gvf_waterUseRatioLCLU(i,Provider)
                 !
                 gvf_LULCProp(i,provider)=lvf_PropLandCover(i)
                 gvf_propLandCover(i,provider)=lvf_PropLandCover(i)
                 !
                 lvf_add=lvf_RCN(i)*lvf_PropLandCover(i)
                 lvf_totalRCN=lvf_totalRCN+lvf_add
                   lvf_xbarRCN(Provider)=lvf_totalRCN
                end do
                !
            !
         return
        end subroutine ProviderRCN
        ! ------------------------
 
        ! -----------------------------
        subroutine sLCLUArea(T,provider)
            ! ---------- Types -------------
            integer :: Provider
            real ::lvf_PropLandCover(13)
            ! -------------------------------
            !
            type(runTime)T
            ! ==============================================
                !
                call sProportionalLandCover(T%year,Provider,T%lcluYear,lvf_PropLandCover)
                call sLandUseLandCoverArea(T%lcluYear,Provider,lvf_PropLandCover)
            !
          return
        end subroutine sLCLUArea
        ! -----------------------------

        ! ------------------------------
        subroutine sProportionalLandCover(year,provider,policyYear, lvf_PropLandCover)
            ! ----------------------- types ------------------
            integer :: i,k,provider,year,policyYear
            real :: lvf_PctLandCover(13),lvf_PropLandCover(13)
            real :: lpf_minProp=0.001
            ! ================================================
                !
                if(gvl_LULCstatus)then
                  !
                  call sMultiplyMatricies(year,provider,policyYear,lvf_PctLandCover)
                  !
                else
                ! New values for 2015 - Default when data not available
                ! ----------------------------
                ! Set the 2010 estimate, the 2011 and beyond will be calculated by the change
                ! in Agricultural acreas as determined by the UI using the API field "WEBAGTR1"
                ! 09.04.16 das
                if(year < 2011)then
                   call sMultiplyMatricies(year,provider,policyYear,lvf_PctLandCover)          
                    do k = 1,13,1
                     gvf_LULCProp(k,provider)=max(lpf_minProp,lvf_PctLandCover(k)* 0.01)
                    end do
                else
                    do k = 1,13,1
                     lvf_PctLandCover(k)= gvf_LULCProp(k,provider)
                    end do
                endif
                ! =======================================
                endif
                ! New classifications from Elizabeth - March, 2016
                ! Scenarios
                !1-AG
                !2-building
                !3-canal
                !4-cultivated grass
                !5-greenway
                !6-impevious
                !7-mountain vege
                !8-residential
                !9-soil
                !10-tree
                !11-unclassified
                !12-wash
                !13-water
                !
                ! New classifications from Elizabeth - August, 2016
                ! Scenarios
                !1-AG
                !2-building (high density)
                !3-canal
                !4-cultivated grass
                !5-greenway
                !6-impevious
                !7-mountain vege
                !8-residential (medium density)
                !9-soil
                !10-tree
                !11-wash
                !12-water
                !13-residential (low density)

                if(gvl_LULCstatus)then
                  do i = 1,13,1
                   lvf_PropLandCover(i)= lvf_PctLandCover(i) * 0.01
                  end do
                else
                  do i = 1,13,1
                   lvf_PropLandCover(i)= gvf_LULCProp(i,provider)
                  end do        
                endif
                !
          return
        end subroutine sProportionalLandCover
        ! ------------------------------------

        ! ------------------------------------
        subroutine sLandUseLandCoverArea(PolicyYear,provider,lvf_PropLandCover)
            ! ------------- Types -------------
            integer :: i,PolicyYear,provider
            real :: lvf_PropLandCover(13)
            real :: vArea(gvi_maxProV)
            ! =================================
                !
                ! Provider service area (m2)
                call ProviderArea2012(vArea)
                !
                PolicyYear=max(1,PolicyYear)
                    do i = 1,gpi_LULC,1
                     if(PolicyYear <=55)then
                       gvf_areaLCLU(i,provider,PolicyYear)=0
                      gvf_areaLCLU(i,provider,PolicyYear)=lvf_PropLandCover(i)*vArea(provider)
                        !
                        gvf_propLandCover(i,provider)=lvf_PropLandCover(i)
                        !
                     endif
                      gvf_waterUseRatioLCLU(i,provider)=0
                    end do
                !
                gvf_waterUseRatioLCLU(1,provider)= lvf_PropLandCover(1)
                gvf_waterUseRatioLCLU(2,provider)= lvf_PropLandCover(2)
                gvf_waterUseRatioLCLU(4,provider)= lvf_PropLandCover(4)
                gvf_waterUseRatioLCLU(5,provider)= lvf_PropLandCover(5)
                gvf_waterUseRatioLCLU(8,provider)= lvf_PropLandCover(8)
                gvf_waterUseRatioLCLU(9,provider)= lvf_PropLandCover(9)
                gvf_waterUseRatioLCLU(10,provider)=lvf_PropLandCover(10)
                gvf_waterUseRatioLCLU(13,provider)=lvf_PropLandCover(13)
            !
            return
        end subroutine sLandUseLandCoverArea
        ! ------------------------------------    
 
        ! --------------------------------------------------
        subroutine sSetWaterUseByLCLU(T,provider)
            ! ------------ types ------------
            integer :: i,j,k,provider,PolicyYear
            real :: Ag 
            real :: lvf_days
            logical :: lvl_useGPCD=.true.

            ! ===============================

            type(runTime)T
            ! ===============================
                !
            !    temp=0
                PolicyYear=0
                PolicyYear=T%lcluYear ! max(1,PolicyYear)
                !
                call daysintheyearK(T%year,T)
                 lvf_days=T%days
                ! 2010
                if(mpf_x1 <= T%year)then
                    !
                    !       temp=gvf_TotalDemand_acft(provider) ! population and GPCD                
                         Ag=go_AgToLCLUdemand(provider) ! ?

                    ! Ag Only
                       if(PolicyYear <=55)then
                         if(0 < gvf_areaLCLU(1,provider,PolicyYear))then
                           gvf_waterUseLCLUsquareMeter(1,provider)= Ag * (1/ gvf_areaLCLU(1,provider,PolicyYear))
                         endif
                       endif
                        ! 

                        ! ========================================================
                        ! All other Municipal
                        ! Utilize poopulation, gpcd, and the new demand algorithms to estimate the LCLU demand
                        ! each year
                        do j = 1,3,1
                          do k = 1,13,1
                            gvf_gpcdLCLU(k,j,provider)=0
                          end do
                        end do
                        ! Calculate added outdoor water demand ahead of time
                        ! Of course, this is specific to the LCLU types in this study
                        ! -----------------------------------------------------------
                        do i = 2,13,1
                          !
                            if(lvl_useGPCD)then
                              gvf_waterUseLCLUsquareMeter(i,provider)=0
                              call sGetWeighting(T%year,lvf_days,i,provider)
                            endif
                               !
                               if(PolicyYear <=55)then
                                 if(0 < gvf_areaLCLU(i,provider,PolicyYear))then
                                     gvf_waterUseLCLUsquareMeter(i,provider)= gvf_LCLUdemand(i,provider)/gvf_areaLCLU(i,provider,PolicyYear)
                                 endif
                               ! 
                               endif
                        end do
                        !
                        ! Call the LCLU routines to estimate GPCD
                        if(lvl_useGPCD)then
                            gvi_lcluGPCD(provider)=0
                            do i = 2,13,1
                              do j = 1,3,1
                                gvi_lcluGPCD(provider)=nint(gvi_lcluGPCD(provider)+gvf_gpcdLCLU(i,j,provider))
                              end do
                            end do
                        endif
                !
                endif
100 format(I4,1x,I2,1x,2(F8.3,1x))
200 format(I4,1x,I2,1x,F10.1)
          return
        end subroutine sSetWaterUseByLCLU
        ! ------------------------------------

        ! ------------------------------------
        subroutine sGetWaterUseByLCLU(T,provider)
            ! ------------ types ------------
            integer :: i,provider,amount
            real :: vArea(gvi_maxProV)
            real :: temp
            real :: startLULC=2016
            real :: fLCLUefficiencies
            logical :: lvl_useGPCD=.false.
            ! ===============================

            type(runTime)T
            ! ==============================================
                !
                do i = 1,13,1
                  gvf_waterUseLCLU(i,provider)=0
                end do
                !
                if(startLULC <= T%year)then
                    !
                    call ProviderArea2012(vArea)
                    !
                    do i = 1,13,1
                      temp=0
                     temp= gvf_propLandCover(i,provider)*((gvf_waterUseLCLUsquareMeter(i,provider)*vArea(provider)))
                     !
                      call sGetRawEfficiencies(i,amount)
                        if(lvl_useGPCD)then
                            gvf_waterUseLCLU(i,provider) = temp
                        else
                          if(0 < temp)then
                            gvf_waterUseLCLU(i,provider) = temp * fLCLUefficiencies(T%lcluYear,amount)
                          endif
                        endif
                      !
                    end do
                    !
                else

                endif
          return
        end subroutine sGetWaterUseByLCLU
        ! -------------------------------

        ! ==================================
        subroutine sGetRawEfficiencies(lvi_lclu,lvf_rawEfficiency)
            ! --------- types ----------
            integer :: lvi_lclu
            integer :: lvf_rawEfficiency
            ! ==========================
                !
                select case(lvi_lclu)
                    case(1)
                      lvf_rawEfficiency=90
                    case(2)
                      lvf_rawEfficiency=90
                    case(8)
                      lvf_rawEfficiency=80
                     if(0 < gvi_efficiencyLCLUres)lvf_rawEfficiency=gvi_efficiencyLCLUres
                    case(13)
                      lvf_rawEfficiency=80
                     if(0 < gvi_efficiencyLCLUres)lvf_rawEfficiency=gvi_efficiencyLCLUres
                    case default
                     lvf_rawEfficiency=90
                end select
                !
            !
          return
        end subroutine sGetRawEfficiencies
        ! =================================

        ! =================================
        subroutine sGetWeighting(year,lvf_days,lvi_lclu,provider)
            ! --------- types ----------
            integer :: lvi_lclu,year
            integer:: provider
            real :: lvf_days
            ! ==========================
                !
                ! ---------------------------------------------------------------------
                ! Default = NO utility water used for the LCLU
                gvf_LCLUdemand(lvi_lclu,provider)=0
                !
                select case(lvi_lclu)
                    case(1)
                     ! Agriculture-
                    case(2)
                      call sPartitionPopulation(year,lvf_days,lvi_lclu,provider)
                    case(4)
                      call sAddedOutDoorWater(year,lvf_days,lvi_lclu,provider)
                    case(5)
                      call sAddedOutDoorWater(year,lvf_days,lvi_lclu,provider)
                    case(8)
                      call sPartitionPopulation(year,lvf_days,lvi_lclu,provider)
                    case(10)
                      call sAddedOutDoorWater(year,lvf_days,lvi_lclu,provider)
                    case(13)
                      call sPartitionPopulation(year,lvf_days,lvi_lclu,provider)
                    case default                   
                end select
                !
            !
          return
        end subroutine sGetWeighting
        ! =================================
        
        ! =================================
        subroutine sAddedOutDoorWater(year,lvf_days,lclu,provider)
            ! ----------------- Types ----------
            integer :: year
            integer :: provider,lclu
            real :: lvf_days
            ! ==================================
                !
                select case(lclu)
                    case(1)
                     ! High Density Residential, Commercial, and Industrial
                    case(4)
                      call sTurfWaterUse(year,lvf_days,lclu,provider)
                    case(5)
                      call sTurfWaterUse(year,lvf_days,lclu,provider)
                    case(10)
                      call sTreeWaterUse(year,lvf_days,lclu,provider)                   
                    case default
                endselect
                !
          return
        end subroutine sAddedOutDoorWater
        ! =================================

        ! =================================
        subroutine sPartitionPopulation(year,lvf_days,lclu,provider)
            ! ---------------------- Types -------------------
            integer :: year,amount
            integer :: provider,lclu,code
            real :: lcluProportion,lvf_householdProp
            real :: lvf_days
            real :: lvf_popT,lvf_pop,lvf_gpcd
            real :: lvf_com,lvf_ind
            real :: lvf_pplPerHH
            real :: lvf_IndoorGPCD,lvf_OutdoorGPCD
            real :: demand
            real :: residential,commercial,industrial
            real :: resGPCD,comGPCD,indGPCD
            real :: resGPCDtoUI
            real :: lvf_outDoorRatio,addOutDoor(3)
            real :: lvf_upDatedOutDoorGPCD
            real :: population,temp
            real :: fLCLUefficiencies
            ! ================================================
                !
                lcluProportion=0; lvf_householdProp=0; population=0
                residential=0; commercial=0; industrial=0; temp=0
                resGPCD=0; comGPCD=0; indGPCD=0; resGPCDtoUI=0; code=1
                lvf_upDatedOutDoorGPCD=0;lvf_outDoorRatio=0
                !
                !lvf_res=gvf_parm_WStoRes_prop(provider)
                lvf_com=gvf_parm_WStoCom_prop(provider)
                lvf_ind=gvf_parm_WStoInd_prop(provider)
                !
                lvf_popT=gvf_populations(year,provider)
                population=lvf_popT
                !
                lvf_gpcd=go_SES_GPCD(provider)
                !
                select case(lclu)
                    case(1)
                     ! High Density Residential, Commercial, and Industrial
                    ! re-set to zero each provider and year (to loop over densities)
                    case(2)
                      gvf_OutdoorResProp(1)=0;gvf_OutdoorResProp(2)=0; gvf_OutdoorResProp(3)=0
                      gvf_OutdoorResProp(4)=0
                      addOutDoor(1)=0;addOutDoor(2)=0;addOutDoor(3)=0
                      call sHouseHolds(provider,lvf_popT)               
                    case default
                endselect
                !
                 lcluProportion=0
                lcluProportion=fRatioProportion(lclu,provider)
                !
                call sPPlHousehold(lclu,code,lvf_pplPerHH,provider)
                !
                if(0 < gvf_houseHolds(4,provider))lvf_householdProp=gvf_houseHolds(code,provider)/gvf_houseHolds(4,provider)
                !
                 lvf_pop=0
                lvf_pop=lvf_popT*lvf_householdProp
                !
                call sGetRawEfficiencies(lclu,amount)
                temp=fLCLUefficiencies(max(0,(year-gpi_lclu)+1),amount)
                ! ==========================================================
                 !
                 if(0 < lvf_householdProp)then
                   call sIndoorGPCDperHHRes(lvf_pplPerHH,lvf_IndoorGPCD)
                   call sTotalGPCDperHHRes(provider,lvf_days,lvf_pplPerHH,lvf_pop,lvf_IndoorGPCD,lvf_OutdoorGPCD,lvf_outDoorRatio,demand)
                 endif
                 ! 
                 addOutDoor(1)=gvf_gpcdLCLU(4,1,provider)+gvf_gpcdLCLU(5,1,provider)+gvf_gpcdLCLU(10,1,provider)
                 addOutDoor(2)=gvf_gpcdLCLU(4,2,provider)+gvf_gpcdLCLU(5,2,provider)+gvf_gpcdLCLU(10,2,provider)
                 addOutDoor(3)=gvf_gpcdLCLU(4,3,provider)+gvf_gpcdLCLU(5,3,provider)+gvf_gpcdLCLU(10,3,provider)
                !
                 addOutDoor(1)=lvf_householdProp*addOutDoor(1)
                 addOutDoor(2)=lcluProportion*addOutDoor(2)
                 addOutDoor(3)=lcluProportion*addOutDoor(3)
                !
                 lvf_upDatedOutDoorGPCD=0
                 resGPCD=0
                if(0 < lvf_householdProp)then
                 lvf_upDatedOutDoorGPCD=max(0,lvf_OutdoorGPCD-addOutDoor(1))
                 resGPCD=lcluProportion*(lvf_IndoorGPCD+lvf_upDatedOutDoorGPCD)*temp 
                 resGPCDtoUI=lcluProportion*(lvf_IndoorGPCD+lvf_OutdoorGPCD)*temp 
                else
                 lvf_IndoorGPCD=0
                 lvf_OutdoorGPCD=0
                endif
                 comGPCD=max(0,((lcluProportion*lvf_com*lvf_gpcd)-addOutDoor(2)))* temp
                 indGPCD=max(0,((lcluProportion*lvf_ind*lvf_gpcd)-addOutDoor(3)))* temp
                 !
                 ! These are partial's in GPCD because we have to account for turf and tree GPCD
                 ! later (above)
                 ! ------------------------------------
                 gvf_gpcdLCLU(lclu,1,provider)=resGPCD
                 gvf_gpcdLCLU(lclu,2,provider)=comGPCD
                 gvf_gpcdLCLU(lclu,3,provider)=indGPCD
                 ! ====================================

                 ! lclu; 1=res,2-com,3-ind;1=indoor gpcd, 2=outdoor gpcd; provider
                 !  i.e., gvf_gpcdLCLUinOut(lclu,res,indoor,provider), etc.
                 ! Only doing residential, and only for the three density classes, for now..
                 ! 01.14.17 das
                 ! ===============
                  gvf_gpcdLCLUinOut(lclu,1,1,provider)=0
                 gvf_gpcdLCLUinOut(lclu,1,1,provider)=resGPCDtoUI*(1-lvf_outDoorRatio)
                 gvf_gpcdLCLUinOut(lclu,1,2,provider)=resGPCDtoUI*lvf_outDoorRatio
                 ! ----------------------------------------------------------------

                 ! Water Demand (AF a-1)
                 residential=resGPCD*lvf_days*(1./gpd_galperacft)*population
                 commercial=comGPCD*lvf_days*(1./gpd_galperacft)*population
                 industrial=indGPCD*lvf_days*(1./gpd_galperacft)*population
                 !
                 gvf_LCLUdemand(lclu,provider)=residential+commercial+industrial
                 !
                 gvf_OutdoorResProp(code)=lvf_householdProp*lvf_outDoorRatio
                 gvf_OutdoorResProp(4)=gvf_OutdoorResProp(4)+lvf_householdProp*lvf_outDoorRatio
                 !
                  gvf_parm_OutDoorResProp(provider)=0
                 gvf_parm_OutDoorResProp(provider)=gvf_OutdoorResProp(4)
                 !
300 format(I4,1x,I2,1x,F6.3,1x,I2,1x,4(F6.1,1x),1(F6.3,1x),2(F9.1,1x))
310 format(I4,1x,I2,1x,I2,1x,4(F6.1,1x))
          return
        end subroutine sPartitionPopulation
        ! =================================

                ! Scenarios
                !1-AG
                !2-building (high density)
                !3-canal
                !4-cultivated grass
                !5-greenway
                !6-impevious
                !7-mountain vege
                !8-residential (medium density)
                !9-soil
                !10-tree
                !11-wash
                !12-water
                !13-residential (low density)

            ! =========================================
            subroutine sHouseHolds(provider,lvf_PopTotal)
                ! --------------- types --------------
                integer :: i,provider,code,lclu
                real :: lvf_pplPerHH,lcluProportion
                real :: lvf_PopTotal
                ! ====================================
                    !
                    do i = 1,4,1
                     gvf_houseHolds(i,provider)=0
                    end do
                    !
                    do lclu = 1,13,1
                        !
                        call sPPlHousehold(lclu,code,lvf_pplPerHH,provider)
                        !
                         lcluProportion=0
                        !
                        select case(lclu)
                            case(2)
                               gvf_houseHolds(4,provider)=0
                              lcluProportion=fRatioProportion(lclu,provider)
                              gvf_houseHolds(code,provider)=(1/lvf_pplPerHH)*lvf_PopTotal*lcluProportion
                              gvf_houseHolds(4,provider)=gvf_houseHolds(code,provider)
                             case(8)
                              lcluProportion=fRatioProportion(lclu,provider)
                              gvf_houseHolds(code,provider)=(1/lvf_pplPerHH)*lvf_PopTotal*lcluProportion
                              gvf_houseHolds(4,provider)=gvf_houseHolds(4,provider)+gvf_houseHolds(code,provider)
                             case(13)
                              lcluProportion=fRatioProportion(lclu,provider)
                              gvf_houseHolds(code,provider)=(1/lvf_pplPerHH)*lvf_PopTotal*lcluProportion
                              gvf_houseHolds(4,provider)=gvf_houseHolds(4,provider)+gvf_houseHolds(code,provider)
                            !
                            ! Four will be total households
                            !
                        case default
                        end select
                        !
                    end do  
                    !
                    ! ==========================================================
                    !
              return
            end subroutine sHouseHolds
            ! =========================================

            ! ==========================================
            subroutine sPPlHousehold(lclu,code,lvf_pplPerHH,provider)
                ! ------------- types --------------
                integer :: lclu,code,provider
                real :: lvf_pplPerHH
                real, parameter :: lowDensityPPH=2.0
                real, parameter :: medDensityPPH=2.85
                real, parameter :: highDensityPPH=12
                ! ==================================
                    !
                        select case(lclu)
                            ! High Density Residential, Commercial, and Industrial
                            case(2)
                              code=3
                              lvf_pplPerHH=highDensityPPH
                              call sPPH(lclu,provider,lvf_pplPerHH)
                            case(8)
                              code=2
                              lvf_pplPerHH=medDensityPPH
                              call sPPH(lclu,provider,lvf_pplPerHH)
                            case(13)
                              code=1
                              lvf_pplPerHH=lowDensityPPH
                              call sPPH(lclu,provider,lvf_pplPerHH)
                            case default
                              lvf_pplPerHH=1
                        end select
                    !
              return
            end subroutine sPPlHousehold
            ! ==========================================
            ! ==========================================
            subroutine sPPH(lclu,provider,ppH)
                ! ------------- types --------------
                integer :: provider,lclu
                real :: ppH
                ! ==================================
                    ! www.census.gov/quickfacts/table/city
                    !
                    select case(lclu)
                         case(8)
                            select case(provider)
                              case(1)
                                ppH=2.30 ! Adaman Mutual - no data (DONE)
                              case(2)
                                ppH=2.67 ! WHite Tanks - no  data (DONE)
                              case(3)
                                ppH=1.77 ! pv (new model) (DONE)
                              case(4)
                                ppH=2.07 ! sc (census was 1.67 ) (DONE)
                              case(5)
                                ppH=2.53 ! sw (census was 1.65) (DONE)
                              case(6)
                                ppH=2.61 ! av (census was 3.35) (DONE
                              case(7)    
                                ppH=2.0   ! Berneil (no data)(Not gonna be well modeled)(DONE as done can be)
                              case(8)    
                                ppH=4.00  ! Buckeye (census was 3.28) (DONE)
                              case(11)
                                ppH= 2.37  ! 2.64 is census data-Chandler (done)
                              case(12)
                                ppH=2.27 ! Chaparral City - no data (done)
                              case(13)
                                ppH=3.0 ! Surprise (2.74 is census data) (done)
                              case(16)
                                ppH=2.80  ! El Mirage (3.38 is census) (done)
                              case(17)
                                ppH=2.44 ! Gilbert Census is 3.13 - (done)
                              case(18)
                                ppH=2.70  ! Glendale (census 2.92) (done)
                              case(19)
                                ppH=2.89  ! Goodyear (census is 2.89) (done)
                              case(20)
                                ppH=2.77  ! Litchfield Park (census is 2.52) (done)
                              case(21)
                                ppH=2.45   ! Mesa (census -s 2.7) (done)
                              case(24)
                                ppH=2.47   ! Peoria (census 2.77 ) (done)
                              case(25)
                                ppH=2.55   ! Phoenix (census 2.85) (done)
                              case(26)
                                ppH=2.25   ! Queen Creek (census is 3.44) (done)
                              case(27)
                                ppH=2.99    ! Rigby - no data (done)
                              case(29)
                                ppH=2.32    ! Rose Valley - no data (done)
                              case(30)
                                ppH=2.2    ! Scottsdate (census is 2.20)  (done)
                              case(31)
                                ppH=1.98    ! Sunrise - no data (?? done)
                              case(32)
                                ppH=2.28    ! Tempe (census is 2.46) (done)
                              case(33)
                                ppH=2.03    ! Tolleson (census is 3.14)(done)
                              case(34)
                                ppH=2.42     ! Valley Utilities (done)
                              case default
                                ppH=2.6

                            end select

                       case(13)
                            ! Peri-Urban (lowest density)
                            select case(provider)
                              case(8)
                                ppH=3.60 ! Buckeye
                              case(9)
                                ppH=2.21 ! Care Free (DONE)
                              case(10)  
                                ppH=2.12 ! Cave Creek (done)
                              case(13)
                                ppH=2.75 ! Surprise (done)
                              case(14)
                                ppH=2.55  ! clearwater utilities (done)
                              case(15)
                                ppH=2.29 ! Desert Hills (done)
                              case(19)
                                ppH=2.17 ! Goodyear (done)
                              case(25)
                                ppH=2.4 ! Phoenix  (done)
                              case(28)
                                ppH=2.26 ! Rio Verde (done)
                              case(30)
                                ppH=2.07 ! Scottsdale (done)
                              case(35)
                                ppH=2.88  ! West End (done)
                              case default
                                ppH=2.5
                            end select

                    end select
                    !
!                    select case(lclu)
!                        case(2)
!                        case(8)
!                          if(0 < gvf_ppH_M)ppH=gvf_ppH_M/100
!
!                        case(13)
!                        if(0 < gvf_ppH_L)ppH=gvf_ppH_L/100
!
!                        case default
!                    end select
              return
            end subroutine sPPH
            ! ==========================================

            ! ===================================================
            function fRatioProportion(lclu,provider) result(temp)
                ! --------------- types --------------
                integer :: lclu,provider
                real :: lvf_prop,lvf_propP
                real :: temp
                ! ===================================
                    ! 
                    temp = 0
                    lvf_prop=gvf_propLandCover(lclu,provider)
                    lvf_propP=gvf_propLandCover(2,provider)+gvf_propLandCover(8,provider)+gvf_propLandCover(13,provider)
                    if(0 < lvf_propP)temp = lvf_prop/lvf_propP
                    !
              return
            end function fRatioProportion
            ! ===========================

            ! ========================================================
             subroutine sIndoorGPCDperHHRes(pplHH,lvf_indoorGPCD)
                ! --------------------- types -----------------
                real :: pplHH,temp
                real :: lvf_indoorGPCD
                real,parameter :: lpf_slopeSFindoor=-21.9
                real, parameter :: lpf_interceptSFindoor=76.416
                ! Figure 6.10, Water Research Foundation, Residential
                ! End Use of Water, Version 2, DeOreo et al. (2016)
                ! And see table 8.11
                ! ==============================================
                    ! 
                    ! To 
                    ! 10.27.16 das
                    ! ------------
                      temp = 1.0
                     lvf_indoorGPCD=0
                    if(0 < pplHH .and. pplHH < 10)then 
                      temp=lpf_slopeSFindoor*log(pplHH)+lpf_interceptSFindoor
                    else
                      temp=max(0,lpf_slopeSFindoor*log(pplHH)+lpf_interceptSFindoor)
                    endif
                    ! 
                lvf_indoorGPCD=temp
                !
              return
            end subroutine sIndoorGPCDperHHRes
            ! =================================

           ! =================================================================================
             subroutine sTotalGPCDperHHRes(provider,days,ppH,pop,lvf_indoorGPCD,lvf_outdoorGPCD,lvf_outDoorRatio,temp)
                
                ! --------------------- types -----------------
                integer :: provider
                real :: pop,temp,days
                real :: ppH
                real :: lvf_indoorGPCD,lvf_outdoorGPCD
                real :: lvf_totalGPCD
                real :: lvf_outDoorRatio
                real :: lvf_estOutdoorGPCD
                real :: pplHH
                real :: lvf_income
!                real, parameter :: ALPHAppH=-0.2006
!                real, parameter :: BETAppH=0.1002
!                real, parameter :: lpf_minppH=2.1
!                real, parameter :: lpf_maxppH=14
                !
               ! real, parameter :: a_ppH=2.1613
!                real, parameter :: b_ppH=6.7766
!                real, parameter :: c_ppH=0.9761
!                real, parameter :: b_ppH=11.3355
!                real, parameter :: c_ppH=0.9852

                real, parameter :: aet=15
                real, parameter :: lpf_minppH=1.75
!                real, parameter :: lpf_maxppH=12
                !               
                real, parameter :: a=-4.6050
                real, parameter :: b=4.3092
                real, parameter :: c=1.9237
                real, parameter :: d=0.9684   
                ! ==============================================
                    ! 
!                    Sanford, Ward D., and David L Selnick. 2013. Estimation of evapotranspiration across 
!                    the conterminous United States using a regression with climate and land-cover data. Journal
!                    of the American Water Resource Association, 49: 217-230.
                    pplHH=0
                    lvf_income=0

                    ! To 
                    ! 11.28.16 das
                    ! Table 6.2,  Water Research Foundation, Residential
                    ! End Use of Water, Version 2, DeOreo et al. (2016)
                    ! ------------
                    !
                    ! -------------------------------------------
                    ! 12.02.16 analyses - First Model
                    ! First degree inverse polynomial: hyperbola
                    ! model Y = x / (a + b*x)
                    !  -0.4381 < a < -0.1183 : alpha = -0.2782
                    ! 0.0637 < b < 0.2101 : beta = 0.1369 ; approximate r2=0.75; p < 0.001; F=30.17; 22 degrees of Freedom
                    ! ===========================================
                    ! -------------------------------------------
                    ! 12.13.16 analyses - Second Model
                    ! First degree inverse polynomial: hyperbola
                    ! model Y = x / (a + b*x)
                    !  -0.2805 < a < -0.1207 : alpha = -0.2006
                    !   0.1002 < b < 0.1367 : beta=0.1002 ;  approximate r2=0.88; p < 0.001; F=57.98; 21 obs, 19 degrees of Freedom
                    ! ===========================================
                    ! -------------------------------------------
                    ! 12.14.16 analyses - Third Model
                    ! First degree inverse polynomial: hyperbola with power function
                    ! model Y = x / (a + b*x) * gamma**AET
                    !  -0.00985 < a < -0.100 : alpha = -0.0549
                    !   0.00994 < b < 0.0521 : beta=0.0310 
                    !   0.9746 < c < 0.9943  : gamma=0.9844;  approximate r2=0.92; p < 0.001; F=61.66; 21 obs, 18 degrees of Freedom
                    ! ===========================================
                    ! -------------------------------------------
                    ! This model gave much better estimates of outdoor water use (ratios with indoor)
                    ! Even though the r2 is lower
                    !
                    ! 12.15.16 analyses - Fourth Model
                    ! Schumacher's equation with a modified (reversed) Power function
                    ! model Y = exp(a + b/x) * c**AET
                    !   0.3214 < a < 4.0011  : alpha = 2.1613
                    !   2.6987 < b < 10.8546 : beta=6.7766 
                    !   0.9655 < c < 0.9868  : gamma=0.9761;  approximate r2=0.86; p < 0.001; F=49.83; 28 obs, 25 degrees of Freedom
                    ! ===========================================
                    ! 12.23.16 analyses - Fifth Model
                    ! Schumacher's equation with a modified (reversed) Power function (added PV estimates - ppH lower than last attempt)
                    ! model Y = exp(b/x) * c**AET (alpha was NOT significantly different from zero, so removed)
                    !   
                    !   11.0741< b < 11.5969 : beta=11.3355 
                    !   0.9781 < c < 0.9923  : gamma=0.9852;  approximate r2=0.96; p < 0.001; F=355.06; 28 obs, 25 degrees of Freedom
                    ! ===========================================

                    ! ==================================================
                    ! Final analyses - 01.01.17, January 2017
                    ! Schumacher's equation with a modified power function for median income and aet
	                ! Model Outdoor =  exp(a+ b/pph) * k**c * d**aet;
                    !   -7.6083 < a < -1.6016 : alpha =-4.6050
                    !   0.0722  < b < 8.5462  : beta  =4.3092 
                    !   1.5958  < c < 2.2516  : gamma =1.9237;  approximate r2=0.96; p < 0.001; F=141.74; 27 obs, 23 degrees of Freedom
                    !   0.9552  < d < 0.9815  : gamma =0.9684;
                    ! Where:
                    ! pph -s people per household
                    ! k = median income [in thousands)(i.e., x 1000)
                    ! aet = actual evapotranspiration (cm)
                    !
                    ! gvf_medianAndPPH(i,j) where i=1 eq pplHH, i=2 eq median income (USD), i=3 eq blank (not used)

                    ! ==================================================
                    !
                     lvf_estOutdoorGPCD=0
                    if(lpf_minppH <= ppH)then
!                     lvf_estOutdoorGPCD=    exp(b_ppH/(min(lpf_maxppH,max(lpf_minppH,ppH)))) *  c_ppH**aet
!                     ((gvf_medianAndPPH(j,k),k=1,2),j=1,gvi_maxProV)  
                        pplHH=gvf_medianAndPPH(provider,1)
                        lvf_income=gvf_medianAndPPH(provider,2)* 1/1e3
                      lvf_estOutdoorGPCD=    exp(a+b/pplHH) * lvf_income**c * d**aet
                    endif
                    !
                     lvf_totalGPCD=0
                    lvf_totalGPCD=lvf_estOutdoorGPCD+lvf_indoorGPCD
                    !
                    lvf_outdoorGPCD=lvf_estOutdoorGPCD
                     lvf_outDoorRatio=0
                    if(0 < lvf_totalGPCD)lvf_outDoorRatio=lvf_estOutdoorGPCD/lvf_totalGPCD
                    !
                     temp=0
                    temp = (lvf_totalGPCD*days)*(1./gpd_galperacft)*pop 
                    !
130  format(5(F9.2,1x))
                    !
              return
            end subroutine sTotalGPCDperHHRes
            ! =================================

            ! =================================
            subroutine sTurfWaterUse(year,days,lclu,provider)
                ! --------------- types --------------
                integer :: year,lclu,provider
                real :: m2,ft2
                real :: waterUse
                real :: pavementMod,days
                real :: lvf_popT,lvf_com,lvf_ind
                real,parameter :: lpf_gallonsPerFt2=6.5
                real,parameter :: lpf_m2ToFt2=10.763910417
                real,parameter :: lpf_adjustYears=2009
                ! ===================================
                    ! 
                    lvf_popT=0; lvf_com=0; lvf_ind=0
                    !
                    if(lclu == 4)then
                      ! No sidewalks or concrete
                      pavementMod=1
                    else if(lclu == 5)then
                      ! Concrete associated with the greenway
                      pavementMod=0.93
                    endif
                    !
                    lvf_popT=gvf_populations(year,provider)
                    lvf_com=gvf_parm_WStoCom_prop(provider)
                    lvf_ind=gvf_parm_WStoInd_prop(provider)
                    !
                      m2=gvf_areaLCLU(lclu,provider,year-lpf_adjustYears)
                      ft2=m2*lpf_m2ToFt2
                      ! GPCD * days
                      ! AF year-1
                       waterUse= (((lpf_gallonsPerFt2*ft2))*(1./gpd_galperacft))        
                      gvf_LCLUdemand(lclu,provider)=waterUse *pavementMod
                      gvf_gpcdLCLU(lclu,1,provider)=(1.-(lvf_com+lvf_ind))*gpd_galperacft*gvf_LCLUdemand(lclu,provider)/(days*lvf_popT)
                      gvf_gpcdLCLU(lclu,2,provider)=lvf_com*gpd_galperacft*gvf_LCLUdemand(lclu,provider)/(days*lvf_popT)
                      gvf_gpcdLCLU(lclu,3,provider)=lvf_ind*gpd_galperacft*gvf_LCLUdemand(lclu,provider)/(days*lvf_popT)
                    !
30 format(I4,1x,I2,1x,I2,1x,F7.2)
              return
            end subroutine sTurfWaterUse
            ! ===========================

            ! =================================
            subroutine sTreeWaterUse(year,days,lclu,provider)
                ! --------------- types --------------
                integer :: year,lclu,provider
                real :: lvf_popT,lvf_com,lvf_ind,days
                real :: temp,out
                ! ===================================
                    ! 
                    temp = 0
                    out= (gvf_parm_OutDoorResProp(provider)+gvf_parm_OutDoorComProp(provider)+gvf_parm_OutDoorIndProp(provider))/3
                    !
                    lvf_popT=gvf_populations(year,provider)
                    lvf_com=gvf_parm_WStoCom_prop(provider)
                    lvf_ind=gvf_parm_WStoInd_prop(provider)

                    temp = fHyperbolaTree(out*100) 
                    gvf_LCLUdemand(lclu,provider)= gvf_LCLUdemand(4,provider)*temp

                    gvf_gpcdLCLU(lclu,1,provider)=(1.-(lvf_com+lvf_ind))*gpd_galperacft*gvf_LCLUdemand(lclu,provider)/(days*lvf_popT)
                    gvf_gpcdLCLU(lclu,2,provider)=lvf_com*gpd_galperacft*gvf_LCLUdemand(lclu,provider)/(days*lvf_popT)
                    gvf_gpcdLCLU(lclu,3,provider)=lvf_ind*gpd_galperacft*gvf_LCLUdemand(lclu,provider)/(days*lvf_popT)
                    !
              return
            end subroutine sTreeWaterUse
            ! ========================

                function fHyperbolaTree(OutDoorWatUse) result(temp)
                    ! --------- types ----------
                    real :: OutDoorWatUse
                    real :: alpha,beta  ! horizontal asymptote
                    real :: temp
                    ! ========================================
                    ! ------------
                    temp =1.0
                    beta = 2
                    beta = 2.5
                    alpha = 15
                     temp = max(0,(OutDoorWatUse / (alpha + beta *OutDoorWatUse)))
                   !
              return
            end function fHyperbolaTree
            ! -------------------------------------------------------

            ! ===============================================================================================================================

            ! ===========================================================
            subroutine sMultiplyMatricies(year,provider,policyYear,lvf_PctLandCover)
                ! ---------------- types -------------------
                integer :: i,provider,year,policyYear
                real :: lvf_PctLandCover(gpi_LULC)
                real :: lvf_Y1
                logical :: lvl_flag
                ! ==========================================
                    !

                    lvl_flag=.false.
                    if(year <= mpf_x1)then
                       do i = 1,gpi_LULC,1
                        lvf_PctLandCover(i)=gvf_landCover_2010(i,provider)
                       enddo                     
                      call setLinearLCLU(provider)
                    endif
                     !
                    call setLinearLCLU(provider)
                    call getLCLUpct(year,policyYear,provider,lvf_PctLandCover)
                    !
                      lvf_Y1=gvf_landCover_2010(1,provider)
                     if(99.9 < lvf_Y1)then
                       lvl_flag=.true.
                     endif
                     if(2060 < year)lvl_flag=.true.
                    !
                     if(lvl_flag)then
                       call sDefaultLandCover(lvf_PctLandCover)
                     endif
                !
!130 format(I4,1x,I2,1x,I2,1x,I2,1x,F6.3)
              return
            end subroutine sMultiplyMatricies
            ! =====================================

            ! =====================================
            subroutine getLCLUpct(year,policyYear,provider,lvf_PctLandCover)
                ! ------------------ types -----------------
                integer :: lvi_lclu,provider,year,policyYear,model
                integer :: policy
                real :: lvf_compliance
                real :: lvf_PctLandCover(gpi_LULC)
                logical :: lvl_static
                ! ==========================================
                    !
                    ! ! Scenarios: 1-AG,2-building,3-canal,4-cultivated grass,5-greenway,6-impevious
                    ! 7-mountain vege,8-residential-medium density,9-soil,10-tree,11-wash,12-water,13-res-low density
                    !
                    ! Trajectories in the change in LCLU over time (1010-2060) for the 13 cover types
                    ! 05.19.16
                    ! =============================================================================
                      lvl_static=.false.
                      policy=0
                    do lvi_lclu=1,13,1
                        !
                        call sSpanAndInflection(policy,gvi_IwaniecScenario,lvi_lclu,span,lvf_compliance,gvf_YearsToInflection,model)
                        !
                        select case(lvi_lclu)
                            case(1) ! Agriculture
                                if(1 < gvi_IwaniecScenario)then
                                  call getLinearLCLU(year,lvi_lclu,provider,lvl_static,lvf_PctLandCover)
                                else
                                  call logisticLCLU(policyYear,lvi_lclu,provider,lvl_static,lvf_compliance,span,gvf_YearsToInflection,lvf_PctLandCover)
                                endif
                            case(2)
                                if(model < 2)then
                                 !use linear
                                  call getLinearLCLU(year,lvi_lclu,provider,lvl_static,lvf_PctLandCover)
                                else    
                                 ! use logistic
                                  call logisticLCLU(policyYear,lvi_lclu,provider,lvl_static,lvf_compliance,span,gvf_YearsToInflection,lvf_PctLandCover)
                                endif
                            case(5)
                                if(model < 2)then
                                 !use linear
                                  call getLinearLCLU(year,lvi_lclu,provider,lvl_static,lvf_PctLandCover)
                                else    
                                 ! use logistic
                                  call logisticLCLU(policyYear,lvi_lclu,provider,lvl_static,lvf_compliance,span,gvf_YearsToInflection,lvf_PctLandCover)
                                endif
                            case(6)
                                if(model < 2)then
                                 !use linear
                                  call getLinearLCLU(year,lvi_lclu,provider,lvl_static,lvf_PctLandCover)
                                else    
                                 ! use logistic
                                  call logisticLCLU(policyYear,lvi_lclu,provider,lvl_static,lvf_compliance,span,gvf_YearsToInflection,lvf_PctLandCover)
                                endif
                            case(8)
                                if(model < 2)then
                                 !use linear
                                  call getLinearLCLU(year,lvi_lclu,provider,lvl_static,lvf_PctLandCover)
                                else    
                                 ! use logistic
                                  call logisticLCLU(policyYear,lvi_lclu,provider,lvl_static,lvf_compliance,span,gvf_YearsToInflection,lvf_PctLandCover)
                                endif
                            case(10)
                                if(model < 2)then
                                 !use linear
                                  call getLinearLCLU(year,lvi_lclu,provider,lvl_static,lvf_PctLandCover)
                                else    
                                 ! use logistic
                                  call logisticLCLU(policyYear,lvi_lclu,provider,lvl_static,lvf_compliance,span,gvf_YearsToInflection,lvf_PctLandCover)
                                endif
                            case(13)
                              call logisticLCLU(policyYear,lvi_lclu,provider,lvl_static,lvf_compliance,span,gvf_YearsToInflection,lvf_PctLandCover)
                            case default
                             call getLinearLCLU(year,lvi_lclu,provider,lvl_static,lvf_PctLandCover)
                        end select
                        !       
                    end do
                !
              return
            end subroutine getLCLUpct
            ! =====================================

            ! =====================================
            subroutine setLinearLCLU(provider)
                ! --------- Types ---------
                integer :: i,provider
                real :: lvf_Y1,lvf_y2,lvf_m, lvf_b 
                ! =========================
                    !
                     lvf_y2=0
                     lvf_Y1=0
                     lvf_m=0
                     lvf_b=0
                     ! lvf_est=0
                    !
                     do i = 1,gpi_LULC,1
                       if(gvf_landCover_2060(i,provider) < 99.999)then
                        lvf_y2=gvf_landCover_2060(i,provider)
                        lvf_Y1=gvf_landCover_2010(i,provider)
                       endif

                        lvf_m=(lvf_y2-lvf_y1)/mpf_timeDifference
                        !
                        lvf_b=lvf_Y1-(lvf_m*mpf_x1)
                        !
                        gvf_slope(i,provider)=lvf_m
                        gvf_intercept(i,provider)=lvf_b

                       ! lvf_est = max(0, lvf_m * year + lvf_b)
                      end do
                    !
              return
            end subroutine setLinearLCLU
            ! =====================================

            ! ===================================
            subroutine getLinearLCLU(year,lvi_lclu,provider,lvl_static,lvf_PctLandCover)
                ! -------- Types -------
                integer :: i,lvi_lclu,provider,year
                real :: lvf_PctLandCover(gpi_LULC)
                logical :: lvl_static
                ! ======================
                    !
                    if(lvl_static)then
                     do i = 1,13,1
                       lvf_PctLandCover(i)=gvf_slope(i,provider)* year + gvf_intercept(i,provider)   
                     enddo
                    else
                     lvf_PctLandCover(lvi_lclu)=0
                       lvf_PctLandCover(lvi_lclu)=min(100,max(0,gvf_slope(lvi_lclu,provider)* year + gvf_intercept(lvi_lclu,provider)))
                    endif
                    !
                !
              return
            end subroutine getLinearLCLU
            ! ===================================

            ! =====================================
            subroutine logisticLCLU(Time,j,provider,lvl_static,asympote,span,years,lvf_PctLandCover)
                ! ------------- Types ---------------
                integer :: i,j,provider,Time
                real :: lvf_y2,lvf_Y1,lvf_proportion
                real :: lvf_PctLandCover(gpi_LULC)
                real :: asympote,span,years
                real :: fLogisticLCLUstatic
                real :: fLogisticLCLUspan
                logical :: lvl_static
                ! ===================================
                    !  Time = T%policyYear (zero to x)
                    !
                      lvf_y2=0
                      lvf_Y1=0
                    if(lvl_static)then
                        lvf_proportion =0
                       lvf_proportion =  fLogisticLCLUstatic(Time)
                      do i = 1,gpi_LULC,1
                        if(gvf_landCover_2060(i,provider) < 99.999)then
                         lvf_y2=gvf_landCover_2060(i,provider)
                         lvf_Y1=gvf_landCover_2010(i,provider)
                        endif
                        if(lvf_y1 < lvf_y2)then
                         lvf_PctLandCover(i)= lvf_Y1 + lvf_proportion* (lvf_y2-lvf_Y1)
                        else
                         lvf_PctLandCover(i)= lvf_Y1 - lvf_proportion* (lvf_Y1-lvf_y2)
                        endif
                        ! FIX THIS WHEN WE CAN!
                        ! 05.17.16 das
                        if(provider == 23)then
                         lvf_PctLandCover(i)=0
                        endif
                      end do
                    else
                          lvf_proportion =0
                         lvf_proportion =  fLogisticLCLUspan(Time,asympote,span,years)
                        if(gvf_landCover_2060(j,provider) < 99.999)then
                         lvf_y2=gvf_landCover_2060(j,provider)
                         lvf_Y1=gvf_landCover_2010(j,provider)
                        endif
                        if(lvf_y1 < lvf_y2)then
                         lvf_PctLandCover(j)= lvf_Y1 + lvf_proportion* (lvf_y2-lvf_Y1)
                        else
                         lvf_PctLandCover(j)= lvf_Y1 - lvf_proportion* (lvf_Y1-lvf_y2)
                        endif
                        ! FIX THIS WHEN WE CAN!
                        ! 05.17.16 das
                        if(provider == 23)then
                         lvf_PctLandCover(j)=0
                        endif

                    endif
                !
100 format(I4,1x,7(F8.3,1x))
              return
            end subroutine logisticLCLU
            ! =====================================

            ! =====================================
            subroutine hyperbolLCLU(Time,j,provider,lvl_static,lvf_PctLandCover)
                ! ------------- Types ---------------
                integer :: i,j,provider,Time
                real :: lvf_proportion
                real :: lvf_Y1,lvf_y2
                real :: lvf_PctLandCover(gpi_LULC)

             !   real,parameter :: lpf_alpha=5
              !  real,parameter :: lpf_beta=0.9
                real :: fHyperbolLCLUstatic
                logical :: lvl_static
                ! ===================================
                    !  Time = T%policyYear (zero to x)
                    !
                      lvf_y2=0
                      lvf_y1=0
                    if(lvl_static)then
                        lvf_proportion =0
                       lvf_proportion =  fHyperbolLCLUstatic(Time)
                      do i = 1,gpi_LULC,1
                       if(gvf_landCover_2060(i,provider) < 99.999)then
                        lvf_y2=gvf_landCover_2060(i,provider)
                        lvf_Y1=gvf_landCover_2010(i,provider)
                       endif
                        if(lvf_y1 < lvf_y2)then
                         lvf_PctLandCover(i)= lvf_Y1 + lvf_proportion* (lvf_y2-lvf_Y1)
                        else
                         lvf_PctLandCover(i)= lvf_Y1 - lvf_proportion* (lvf_Y1-lvf_y2)
                        endif
                        ! FIX THIS WHEN WE CAN!
                        ! 05.17.16 das
                        if(provider == 23)then
                         lvf_PctLandCover(i)=0
                        endif
                      end do
                    else
                          lvf_proportion =0
                         lvf_proportion =  fHyperbolLCLUstatic(Time)
                        if(gvf_landCover_2060(j,provider) < 99.999)then
                         lvf_y2=gvf_landCover_2060(j,provider)
                         lvf_Y1=gvf_landCover_2010(j,provider)
                        endif
                        if(lvf_y1 < lvf_y2)then
                         lvf_PctLandCover(j)= lvf_Y1 + lvf_proportion* (lvf_y2-lvf_Y1)
                        else
                         lvf_PctLandCover(j)= lvf_Y1 - lvf_proportion* (lvf_Y1-lvf_y2)
                        endif
                        ! FIX THIS WHEN WE CAN!
                        ! 05.17.16 das
                        if(provider == 23)then
                         lvf_PctLandCover(j)=0
                        endif

                    endif
                !
              return
            end subroutine hyperbolLCLU
            ! =====================================

            ! ===================================
            subroutine sDefaultLandCover(lvf_PctLandCover)
                real :: lvf_PctLandCover(13)
                !
                lvf_PctLandCover(1)=1  ! 
                lvf_PctLandCover(2)=14 ! 
                lvf_PctLandCover(3)=1  ! 
                lvf_PctLandCover(4)=1  ! 
                lvf_PctLandCover(5)=7  ! 
                lvf_PctLandCover(6)=12 ! 
                lvf_PctLandCover(7)=6  ! 
                lvf_PctLandCover(8)=6  ! 
                lvf_PctLandCover(9)=45 ! 
                lvf_PctLandCover(10)=4 !
                lvf_PctLandCover(11)=4 !
                lvf_PctLandCover(12)=4 !
                lvf_PctLandCover(13)=0 !
                !
              return
            end subroutine sDefaultLandCover
            ! ===================================

End Module lms_LULC

       subroutine sDefaultRCN(T,i,lvf_xbarRCN,lvf_PropLandCover)
         use lms_LULC
            integer :: i,j
            real :: lvf_totalRCN,lvf_add,lvf_RCN(13)
            real :: lvf_xbarRCN(gvi_maxProV)
            real :: lvf_PropLandCover(gvi_maxProV)
            type(runTime)T
            ! ==============================================

                 lvf_RCN(1)=74 ! Ag
                 lvf_RCN(2)=89 ! building - use commercial business, soil group A
                 lvf_RCN(3)=10 ! canal - assuming some runoff
                 lvf_RCN(4)=61 ! Turf
                 lvf_RCN(5)=70 ! Greenway : Turf plus water plus impervious
                 lvf_RCN(6)=98 ! Impervious
                 lvf_RCN(7)=96 ! Mtn Vegetation
                 lvf_RCN(8)=75 ! residential - using 1/4 acre B soil group
                 lvf_RCN(9)=82 ! soil/desert - using "dirt Right-of-way" B hydrologic soil group
                 lvf_RCN(10)=68 ! tree - poor condition grass cover
                 lvf_RCN(11)=50 ! unclassified
                 lvf_RCN(12)=76 ! wash - using the "A hydrologic soil group for gravel
                 lvf_RCN(13)=0 ! water

                lvf_totalRCN=0.
                lvf_add=0.
                !
                do j = 1,gpi_LULC,1
                  lvf_PropLandCover(j)=gvf_landCover_2010(j,i)*0.01
                  lvf_add=lvf_RCN(j)*lvf_PropLandCover(j)
                  lvf_totalRCN=lvf_totalRCN+lvf_add
                  lvf_xbarRCN(i)=lvf_totalRCN
                enddo      
                !
          return
        end subroutine sDefaultRCN

    Subroutine sProviderRCN(T,i,lvf_xbarRCN)
     use lms_LULC
        ! ------------ types ------------
        integer :: i
        real :: lvf_xbarRCN(gvi_maxProV)
        ! ===============================
        ! -- Type Construct ---
        type(runTime)T
        ! ======================
            !
            call ProviderRCN(T,i,lvf_xbarRCN)
            !
        ! 
      return
    end subroutine sProviderRCN
    ! ---------------------------------------

    ! ---------------------------------------
    Subroutine sSetLCLUdemand(T,i)
     use lms_LULC
        ! ------------ types ------------
        integer :: i
        ! ===============================
        ! -- Type Construct ---
        type(runTime)T
        ! ======================
            !
            call sSetWaterUseByLCLU(T,i)
            !
        ! 
      return
    end subroutine sSetLCLUdemand
    ! ---------------------------------------
    ! ---------------------------------------
    Subroutine sGetLCLUdemand(T,i)
     use lms_LULC
        ! ------------ types ------------
        integer :: i
        ! ===============================

        ! -- Type Construct ---
        type(runTime)T
        ! ======================
            !
            call sGetWaterUseByLCLU(T,i)
            !
        ! 
      return
    end subroutine sGetLCLUdemand
    ! ---------------------------------------

    ! ---------------------------------------
    subroutine sRunLCLUarea(T,i)
     use lms_LULC
        ! ------ Types -------
        integer :: i
        ! ====================

        ! -- Type Construct ---
        type(runTime)T
        ! ======================
            !
            call sLCLUArea(T,i)
            !
      return
     end subroutine sRunLCLUarea
    ! ---------------------------------------

    ! ---------------------------------------
    subroutine sRunLCLU(T,i)
     use lms_LULC
        ! ------ Types -------
        integer :: i
        ! ====================

        ! -- Type Construct ---
        type(runTime)T
        ! ======================
            !
            call sLCLUArea(T,i)
            call  sSetLCLUdemand(T,i)
            call  sGetLCLUdemand(T,i)
            !
      return
    end subroutine sRunLCLU
    ! ---------------------------------------
! E.O.F.