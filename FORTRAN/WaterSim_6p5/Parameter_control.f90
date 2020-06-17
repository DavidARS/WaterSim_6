!
! File is Parameter_control.f90
!
!   This file accepts inputs from the KernelInterface to alter parameters.  It also
! initializes some parameters.
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
!
! Module:       Module lm_ParameterControl
! Subroutines:  subroutine initParameters()
!                 calls (also present):
!                   call sMaskSRP(gvl_maskSRP) (not present)
!                   call initGlobal()
!                   call CO()
!                   call SVT(i,lvi_countSRP)
!                   call initPopAndDemand(i)
!                   call City(i)
!                   call General(i)

! Module:       Module lms_ParameterControl
! Subroutines:  subroutine modifyDesignations()
!               
! No Modules:   subroutine pCityParameterControl_a(T)
!               subroutine sParameterControl_tstep(T,i)
!               subroutine ClearInclude(T)
!               subroutine sInputsCity(T)
!
!
! created on 04.25.11
!
! david arthur sampson

! last write was: 10.17.13,07.18.14,07.21.14,07.30.14
! ---------------------------------------------------

! ----------------------------------------------------------------------------------------
!
!       UNIT inputs
!

!       Files that the model writes to: Unit 1 and 4 not used; only special case use
!      open(1,FILE=trim(trim(lvc_outputPath)//'Consistency.txt'), status='replace')
!      open(4,FILE=trim(trim(lvc_outputPath)//'Provider.txt'), status='replace')
!      open(7,FILE=trim(trim(lvc_outputPath)//'Log.txt'), status='replace')

!       WaterShed_CO.f90
!      open(10,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\CORiver_BofRbase.txt'),status='old')
!      open(11,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\CORiver_BofR.txt'),status='unknown')
!      open(12,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\CORiver_paleo.txt'),status='unknown')
!      open(13,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\CORiver_scenario.txt'),status='unknown')
!      open(14,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\CoInFlowPowellToMead.txt'),status='unknown')
!      open(15,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\UpperBasin.txt'),status='unknown')
!      open(16,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\powellequalizations.txt'),status='unknown')
!      open(17,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\Initial_storage.txt'),status='unknown')
!
!       Found Groundwater.f90- CO water used
!      open(18,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\cagrd.txt'),status='unknown')

!       WaterShed_SVT.f90
!      open(20,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\SaltTonto_doy.txt'),status="old")
!      open(21,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\Verde_doy.txt'),status="old")

!       WaterShed_CO.f90
!      open(30,FILE=trim(trim(lvc_dataPath)//'App_Data\Parameters\parm_CO.dat'),status='unknown')

!       WaterShed_SVT.f90
!      open(31,FILE=trim(trim(lvc_dataPath)//'App_Data\Parameters\parm_SVT.dat'),status='old')

!       Groundwater.f90
!      open(33,FILE=trim(trim(lvc_dataPath)//'App_Data\Parameters\GWdesignations.txt'),status='unknown')
!      open(34,FILE=trim(trim(lvc_dataPath)//'App_Data\Parameters\parm_GW.dat'),status='unknown')

!       Designations_CO.f90
!      open(35,FILE=trim(trim(lvc_dataPath)//"App_Data\Parameters\parm_PV.dat"))
!      open(36,FILE=trim(trim(lvc_dataPath)//'App_Data\Parameters\SurfaceWaterDesignations.txt'))

!       Designations_SVT.f90
!      open(40,FILE=trim(trim(lvc_dataPath)//'App_Data\Parameters\SVT_Designations_2012.txt'),status='unknown')
!      open(41,FILE=trim(trim(lvc_dataPath)//'App_Data\Parameters\SVT_Threshold_2012.txt'),status='unknown')
!      open(42,FILE=trim(trim(lvc_dataPath)//'App_Data\Parameters\SVTacreageClassA.txt'),status='unknown')

!       Designations_CO.f90
!      open(43,FILE=trim(trim(lvc_dataPath)//'App_Data\Parameters\CAPdesignations.txt'),status='unknown')

!       Designations_SVT.f90
!      open(44,FILE=trim(trim(lvc_dataPath)//'App_Data\Parameters\SVT_Relative.txt'),status='unknown')
!      open(45,FILE=trim(trim(lvc_dataPath)//'App_Data\Parameters\SVT_Designations_Daily.txt'),status='unknown')
!      open(46,FILE=trim(trim(lvc_dataPath)//'App_Data\Parameters\SVT_Threshold_Daily.txt'),status='unknown')
!
!       ProviderPopulationandDemand.f90
!      open(51,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\providerDemand_2085.txt'),status="unknown")
!      open(52,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\providerAcres_2012.txt'),status="unknown")
!
!      open(54,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\Population_On_2085.txt'),status="unknown")
!      open(55,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\Population_Other_2085.txt'),status="unknown")
!
!                                  THESE (following two) are NOT in the directory at present
!                               ----------------------------------------------------------------
!       Meteorology.f90
!      open(70,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\MetDataBaseline.txt'),status='old') ! 12.04.15; David Iwaniec generated rainfall and temperature data
!      open(71,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\MetDataProjected.txt'),status='old')
!      open(72,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\rainFall_60.txt'),status='old') ! 12.09.15; Giuseppe generated rainfall data for each provider

!
!       Water_CityModel.f90 
!      open(80,FILE=trim(trim(lvc_dataPath)//'App_Data\Parameters\parm_default.txt'),status='old')
!      open(81,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\LandCover_2010.txt'),status='old')
!      open(82,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\LandCover_2060.txt'),status='old')
!
!       Kernel.f90
!      open(100,FILE=trim(trim(lvc_outputPath)//'CO_state.out'),status="replace")
!      open(101,FILE=trim(trim(lvc_outputPath)//'Verde_state.txt'),status="replace")
!      open(102,FILE=trim(trim(lvc_outputPath)//'Salt-Tonto_state.txt'),status="replace")
!      open(103,FILE=trim(trim(lvc_outputPath)//'onehundredthree.out'),status="replace")


! Current data files (as of 01.16.15)
! --------------------------------------
! LU=80; agPumpingSurface_2085.txt
! LU=10; CORiver_BofRbase.txt
! LU=18; cagrd.txt
! LU=14; COinFlowPowellToMead_year.txt
! LU=11; CORiver_BofR.txt
! LU=12; CORiver_paleo.txt
! LU=13; CORiver_scenario.txt
! LU=53; gpcd_2010
! LU=17; Initial_storage.txt
! LU=54; OnProjectPopulation_2085
! LU=55; OtherPopulation.txt
! LU=16; powellequalizations.txt
! LU=52; providerAcres_2012.txt
! LU=51; providerDemand_2085.txt
! LU=20; SaltTonto_doy.txt
! LU=15; UpperBasin.txt
! LU=21; VerdeTango_doy.txt
! (17 files)

! Currrent parameter files (as of 10.17.13)
! -----------------------------------------
! LU=43; CAPdesignations.txt
! LU=33; GWdesignations.txt
! LU=30; parm_CO.dat
! LU=34; parm_GW.dat
! LU=35; parm_PV.txt
! LU=31; parm_SVT.dat
! LU=36; SurfaceWaterDesignations.txt
! LU=40; SVT_Designations_2012.txt
! LU=45; SVT_Designations_Daily.txt
! LU=44; SVT_Relative.txt
! LU=41; SVT_Threshold_2012.txt
! LU=46; SVT_Threshold_Daily.txt
! LU=42; SVTacreageClassA_2012.txt
! (13 files)
!
! Modified on 10.17.13,07.20.14
! -------------------------------
!

! ======================================================================================================
!
Module lm_ParameterControl
  use gm_ModelControl
   use gm_GlobalData
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
 contains
    !
    ! ---------------------
     subroutine initParmControl()
       call openFiles_pc()
       call readFiles_pc()
      return
     end subroutine initParmControl
    ! ---------------------
  ! ------------------------
      subroutine openFiles_pc()
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
              module="lm_ParameterControl"
              !
              Infile='App_Data\Parameters\parm_defaultNew.txt'; LU=80
              call openFiles(module,lvc_DPath,Infile,LU)
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
      end subroutine openFiles_pc
     ! ---------------------------
      ! ------------------------
        subroutine readFiles_pc()
            !
            ! -------- Types -------
            integer :: i,j,ios
            ! ======================
            ios=0
            !----------------
            ! Read from Disk
            ! Provider-level data;
            !--------------------------------------------------------------------------------------
            ! acft a-1
            ! 
            do i = 1,gvi_Providers,1
             gdf_parm_WWtoRWWTP(i)=0
             gdf_parm_RWWtoRO(i)=0
             gdf_parm_WWtoEFF(i)=0
             gdf_parm_EFFtoVadose(i)=0
             gdf_parm_EFFtoPP(i)=0
             gdf_parm_SWtoWBamt(i)=0
             gdf_parm_OutDoorResPCT(i)=0
            end do
            !
            read(80,*,err=800,iostat=ios)((gdf_parm_WWtoRWWTP(j),gdf_parm_RWWtoRO(j),gdf_parm_WWtoEFF(j), &
                                           gdf_parm_EFFtoVadose(j),gdf_parm_EFFtoPP(j),gdf_parm_SWtoWBamt(j), &
                                           gdf_parm_OutDoorResPCT(j)),j=1,gvi_Providers)
            !
            close(80)
800         continue
            if(ios >0)then
             LU=80
             gvo_errorCode=80
             goto 1000
            endif
            ! ---------------------
           return
1000     continue
           if(gvl_writeLog)then
              string=27
                call sStrings(string,errorString)
                call eWrite(errorString,LU)
            endif
          gvl_errorFlag=.false.
          !
        end subroutine readFiles_pc
        ! --------------------------

    ! ------------------------
    subroutine initArrays()
        !
        ! ------ Types ------
        integer :: i,j,k,l,m
        ! ===================
        !
        ! 10.10.2013
        do i = 1,gpi_dailySVTrec,1
            do j = 1,3,1
               gvf_flowSVT(i,j)=0.
            end do
        end do
        !
        do i = 1,10,1
          lvf_modifyNormalFlow(i)=0.0
        end do
        !
        do i = 1,3,1
          gvf_LinearClimateFactorSVT(i)=0.
          gvf_LinearClimateFactorCO(i)=0.
            !
            do j = 1,gpi_dailySVTrec,1
             gvf_flowSaltTonto(j,i)=0.
             gvf_flowVerde(j,i)=0.
            end do
        end do
        ! Again, initialized in ProviderPopulationandDemand.f90
        do k = 1,gvi_maxProV,1
         gvf_TotalDemand_acft(k)=0.
         !
          do l = 1,gpi_unusedNonPotable,1
            do m = 1,14,1
             gvf_WaterDemand_acft(k,l,m)=0.
            end do
          end do
        end do
        !
        do i = 2000,2085,1
          do j = 1,12,1
            do k = 1,3,1
              lvf_MetData(i,j,k)=0
              lvf_MetDataProjected(i,j,k)=0
            end do
          end do
        end do
        lvf_Beta=0
      return
    end subroutine initArrays
    ! --------------------------

    ! 
    !  First subroutine called after: 1)connection to the model (i.e., build), 
    ! and 2) open files
    ! ---------------------------
    subroutine initParameters()
        !
        ! -------- Types -----------
        integer :: i,lvi_countSRP
        ! ==========================
            !
            ! --- First Connects ---
            call initBaseParms()
            !
             call sMaskSRP(gvl_maskSRP)
             lvi_countSRP=1
            !
            ! default settings for the model
            ! --------------------------------
            !
             call initGlobal()
             call initCO()
             call initGWater()
             call initGeneric()
             call initNew()
             call initWaterSim_6()
            !
            do i = 1,gvi_Providers,1
             call initSVT(i,lvi_countSRP)
             call initPopAndDemand(i)
             call initCity(i)
             call initGeneral(i)
            end do
            !
            gvf_increment=1
            !
            ! --------------------------------
            !
        !
    return
  end subroutine initParameters
  ! ---------------------------

    ! ------------------------
    subroutine initBaseParms()
        integer :: i,j
        !
            gvo_errorCode=0
            gv_startyear=2000
            gv_simduration=30
            gvl_start=.true.
            gvi_baseYear=2016
            gvl_IncludeMeteorology=.true.
            do i = 1,10,1
              do j = 1,35,1
                gvl_defaultRun(i,j)=.true.
              end do
            end do
        !
      return
    end subroutine initBaseParms
    ! --------------------------

    ! ---------------------
    subroutine initGlobal()

        ! ------ Types ------
        integer :: j
        integer,parameter :: lvi_End=85
        ! ===================
        !
        gvc_OPath='WaterSim_Output'
        !
        gvi_startSimulationYear=2000
        gvi_endSimulationYear=2085      
!        gvi_kernel_case=1
        gv_simyear=0
       ! 0.75- Used to calculate January water demand use of Class A, and BC water
        gvf_janUsePropOfTotal=0.70
        gvf_janUsePropOfTotal=0.75
        !
        do j = 1,12,1
          gvf_UsedDemand_A_acft(j)=0.
          gvf_Used_NCS_acft(j)=0.
        end do
        !
        gvl_modelVinzeJohnston=.false.
        ! Start in 2016 - was 12
        ! 01.27.17 das
        gvi_StartFactors=gvi_baseYear-2000
        !gvi_StartFactors=12
        gi_ClimateAdjustmentYearEnd=lvi_End-gvi_StartFactors
        !
        gvi_AgEndYear=85
        gvf_AgRetirementPct=0.70
        !
        gvi_EndAgRetire=gvi_AgEndYear-gvi_StartFactors        
        !
        gii_regDemand=0;gii_regVadose=0;gii_regDirectInject=0;gii_regReclaimed=0
        gii_regRO=0;gii_regSRPClassA=0;gii_regSRPClassBC=0; gii_regSRPNCS=0
        gii_regCAP=0;gii_regUnmet=0;gii_reg_2=0;gii_reg_3=0
        gii_regNewWater=0;gii_regBankedWater=0;gii_regDefaultPump=0;gii_regMuniPump=0
        !
        gvf_RainGrayCompliance=0
        !
      return
    end subroutine initGlobal
    ! -----------------------

    ! -----------------
    subroutine initCO()
        !
        gv_dataCO=1
        gvi_COtrace=30
        gv_indexyearCO=1939
        gv_droughtyearCO=2001
        gv_droughtfactorCO=1.
        gv_droughtyearendCO=2026
        !
        gv_climatefactorCO=1
        go_deliveriesCO=0
        gvf_upperBasinEstimate=2
        !
        li_numdroughtyearsCO=0.
        !
        go_mandiCAP=0.
      return
    end subroutine initCO
    ! -------------------

    ! --------------------------------
    subroutine initSVT(i,lvi_countSRP)
        !
        ! ----------- Types ----------
        integer :: i,lvi_countSRP,j,k
        ! ============================
        !
        vArecAv=1
        !
        gv_dataSVT=1
        gvi_SVTtrace=30
        gv_indexyearSVT=1978
        gv_droughtyearSVT=2013
        gv_droughtfactorSVT=1.
        gv_droughtyearendSVT=2026
        !
        gv_climatefactorSVT=1.
        gvi_countYearsSVT=0.
        !
        gvd_acftperAcreSVT=3.0
        gvf_modifyNormalFlow=0.95
        lid_dailyDesignationsSVT=0
        !
        if(gvl_maskSRP(i))then
          go_classBCdesignations(lvi_countSRP)=0
          gvf_availableSRP_acft(lvi_countSRP)=0
          go_classAdesignations(lvi_countSRP)=0
!          go_NCS_acft(lvi_countSRP)=0
!          go_NCSmax_acft(lvi_countSRP)=0
          go_classBCmax(lvi_countSRP)=0
         go_classBCstorage_acft(lvi_countSRP)=0
         go_classBCstorageMax_acft(lvi_countSRP)=0
         !
         ! note- 90 is 0.9 as used in the model proper
         gvi_NormalFlowEmpMax_AFac(lvi_countSRP)=90
           !
          lvi_countSRP=lvi_countSRP+1
        endif
        ! 10.14.13
        if(i < 2)then
            do j = 1,12,1
             gvf_verdeMonthly_acft(j)=0
             gvf_saltTontoMonthly_acft(j)=0.
                do k = gpi_lBY,gpi_uBY,1
                  gvf_flowVerde_Monthly_maf(k,j)=0.
                  gvf_flowSalt_Monthly_maf(k,j)=0.
                end do
             end do
        endif
        ! 10.14.13
        if( i < 2)then
          do k = gpi_lBY,gpi_uBY,1
            gvd_saltTonto_maf_yr(k)=0.
            gvd_verde_maf_yr(k)=0.
            lv_State_A_maf(k)=0.
            gvd_SVT_flow_maf(k)=0.
            lvd_State_Verde_acft(k)=0.
            lvd_State_Salt_acft(k)=0.
            lvd_State_Roosevelt_acft(k)=0.
            lvd_State_Others_acft(k)=0.
          end do
        endif
        ! no output to interface as of 10.14.13
        gvf_Release=0
        !
        go_saltTontoRiverFlow_acft=0.
        go_verdeRiverFlow_acft=0.
        !
       return
    end subroutine initSVT
    ! --------------------

    ! ----------------------------
    subroutine initPopAndDemand(i)
        !
        ! ---- Types ------
        integer :: i,j,k,l
        ! =================
        !
        go_CAPunusedPriority4(i)=0
        go_CAPunusedPriority5(i)=0

             gv_PopGrowthRateAdjPct(i)=0
             gvd_PopGrowthRateOn(i)=1
             gvd_PopGrowthRateOther(i)=1
             !
             go_OnProjectDemand(i)=0
             go_OffProjectDemand(i)=0
             !
             go_ProviderDemand(i)=0.
             go_PopulationOnProject(i)=0.
             go_PopulationOther(i)=0.
             !
             gvi_AlterGPCD_pct=0
             gvi_GPCDmethod=1
             go_SES_GPCD(i)=0
             gvl_popFromInterfaceOn(i)=.false.
             gvl_popFromInterfaceOther(i)=.false.
             gvi_eightyPercentRule(i)=0
             gvf_population=0
             gvi_ReductionDem_pctBaseline=1
             gvi_ProviderDemandOption=3
        !
            !gvf_newSuppliesBalance_acft(2000,i)=0
            gvf_newSuppliesUsed_acft_a(i)=0
            ! 10.11.13
            gvl_parm_shortage(i)=.false.
            !
            gi_ProviderPopulationOn(i)=0
            gi_ProviderPopulationOther(i)=0

            do k = 1,100,1
              gvf_GPCDout(i,k)=0.
            end do
                 gv_EmpiricalGPCD(i)=0
                ! GPCD from the interface
                 gv_AdjustedGPCD(i)=0
                 go_GWPumpageSRP_acft(i)=0
         !
        do l =gpi_lBY,gpi_uBY,1
         lid_providerpop(l,i)=0
         lvd_GrowthRateAdjDemand(l,i)=0.
         lvd_GrowthRateAdjPop(l,i)=0.
         gvf_Pop_OnProject(l,i)=0.
         gvf_Pop_Other(l,i)=0.
        end do
        !
        do k = 1,gpi_LULC,1
         gvf_LULCProp(k,i)=0
        end do
        ! NOT in the interface- this is actual GPCD from water use- 10.15.13
        go_adjustedGPCD(i)=0
        ! 10.15.13
        gvf_providerAreaTotal_m2=0      
        !
        do j = 1,3,1
            gvf_dynamicGPCD(j,i)=0
            lvd_LinearRegression(i,j)=0
        end do
        !
        gvf_betaHoltGPCD(i)=0
        gvf_gammaHoltGPCD(i)=0
        gvf_oneGPCD(i)=0
        !
        gvi_ProviderMinGPCD(i)=70
        return
    end subroutine initPopAndDemand
    ! -----------------------------

    ! --------------------
    subroutine initCity(i)
        !
        ! ------ Types -------
        integer :: h,i,j,k
        ! ====================
        !
            ! 02.21.13
            gvi_newWaterSupplies_acft_a(i)=0

            ! 05.17.12 DAS
            gvf_reclaimedOutput(2000,i,1)=0
            gvf_reclaimedInput(2000,i,1)=0
            gvf_ROreclaimedOutput(2000,i,1)=0
            gvf_ROreclaimedInput(2000,i,1)=0
            !
            ! No provider (22) and other provider (23)
            !   
            gvf_ROreclaimedOutput(2000,22,1)=0
            gvf_ROreclaimedOutput(2000,23,1)=0
            !
            go_ProviderGroundWater_a(i)=0
            !
            gvf_reclaimedBalance(2000,i,1)=0
            gvf_ROreclaimedBalance(2000,i,1)=0
            !
            gvf_AG_Banking(2000)=0
            gvf_parm_OutDoorResProp(i)=0.55
            gvf_parm_OutDoorComProp(i)=0.5
            gvf_parm_OutDoorIndProp(i)=0.5
            !
            gvf_parm_WStoRes_prop(i)=0.6
            gvf_parm_WStoInd_prop(i)=0.1
            gvf_parm_WStoCom_prop(i)=1-( gvf_parm_WStoRes_prop(i)+ gvf_parm_WStoInd_prop(i))
            !
            !
            !
            ! Lag of fluxes to Vadose in City Model: for variable list see documents or
            ! subroutine  sLagVadose(T,...)
             ! This constant could cause problems.. (i.e., eight)
            do j = gpi_lBY,gpi_uBY,1
             gvf_AG_Banking(j)=0
             do k = 1,8,1
              gvf_lagToVadose(j,i,k)=0
             end do
              gvf_reclaimedInputMax(j,i)=0
            end do
            !
            ! =========================================================
            ! Provider:usage:location
            ! One=res, Two=commercial, three=industrial
            ! ---------------------------------
            ! Indoor residential
            gvf_gpcdIndoor_behavior(i,1,1)=25.3
            gvf_gpcdIndoor_behavior(i,2,1)=21.8
            gvf_gpcdIndoor_behavior(i,3,1)=16.1
            gvf_gpcdIndoor_behavior(i,4,1)=12.4
            gvf_gpcdIndoor_behavior(i,5,1)=19.1
            gvf_gpcdIndoor_behavior(i,6,1)=2.8
            gvf_gpcdIndoor_behavior(i,7,1)=1.5
            gvf_gpcdIndoor_behavior(i,8,1)=1
            gvf_gpcdIndoor_behavior(i,9,1)=0
            gvf_gpcdIndoor_behavior(i,10,1)=0
            !
           ! Indoor commercial 
            gvf_gpcdIndoor_behavior(i,1,2)=0  ! shower
            gvf_gpcdIndoor_behavior(i,2,2)=0  ! bath
            gvf_gpcdIndoor_behavior(i,3,2)=7  ! other
            gvf_gpcdIndoor_behavior(i,4,2)=40 ! place holder
            gvf_gpcdIndoor_behavior(i,5,2)=20 ! place holder
            gvf_gpcdIndoor_behavior(i,6,2)=3  ! place holder
            gvf_gpcdIndoor_behavior(i,7,2)=0  ! faucet
            gvf_gpcdIndoor_behavior(i,8,2)=0  ! toilet
            gvf_gpcdIndoor_behavior(i,9,2)=0  ! dishwasher
            gvf_gpcdIndoor_behavior(i,10,2)=0 ! clothes washer
            !
          ! Indoor industrial
            gvf_gpcdIndoor_behavior(i,1,3)=30 !
            gvf_gpcdIndoor_behavior(i,2,3)=0  ! 
            gvf_gpcdIndoor_behavior(i,3,3)=7  ! 
            gvf_gpcdIndoor_behavior(i,4,3)=40 ! 
            gvf_gpcdIndoor_behavior(i,5,3)=20 ! 
            gvf_gpcdIndoor_behavior(i,6,3)=3  ! 
            gvf_gpcdIndoor_behavior(i,7,3)=0  ! 
            gvf_gpcdIndoor_behavior(i,8,3)=0  ! 
            gvf_gpcdIndoor_behavior(i,9,3)=0  ! place holder
            gvf_gpcdIndoor_behavior(i,10,3)=0 ! place holder
            !
            gvf_gpcdOutdoor_behavior(i,1,1)=0  ! pool
            gvf_gpcdOutdoor_behavior(i,2,1)=0  ! Turf
            gvf_gpcdOutdoor_behavior(i,3,1)=0  ! Landscaping (not turf)
            gvf_gpcdOutdoor_behavior(i,4,1)=0  ! Other
            gvf_gpcdOutdoor_behavior(i,5,1)=0  ! not specified
            !
            gvf_gpcdOutdoor_behavior(i,1,2)=0  ! same as above
            gvf_gpcdOutdoor_behavior(i,2,2)=0
            gvf_gpcdOutdoor_behavior(i,3,2)=0
            gvf_gpcdOutdoor_behavior(i,4,2)=0
            gvf_gpcdOutdoor_behavior(i,5,2)=0
            !
            gvf_gpcdOutdoor_behavior(i,1,3)=0  ! same as above
            gvf_gpcdOutdoor_behavior(i,2,3)=0
            gvf_gpcdOutdoor_behavior(i,3,3)=0
            gvf_gpcdOutdoor_behavior(i,4,3)=0
            gvf_gpcdOutdoor_behavior(i,5,3)=0
            ! ======================================================
            !
            gvf_parm_RtoDInjection(i)=0
            gvf_parm_RtoOutput(i)=0
            gvf_parm_RtoVadose(i)=0
            gvf_parm_RtoInputMaxPct(i)=0.1
            gvf_parm_ReclaimedOutdoor(i)=0
            gvf_parm_RWWtoRO(i)=0
            !
            gvf_parm_ROtoOutput(i)=0

            gvf_parm_GWtoGWTP(i)=0
            !
            gvf_parm_WWtoRWWTP(i)=0
            ! 02.21.14 DAS I set this to zero
            gvf_parm_WWtoEffluent(i)=0 
            gvf_parm_WWtoIndReuse(i)=0.0
            !
            gvf_parm_SWtoWB(i)=0.0
            !
            gvf_parm_EffluentToPP(i)=0
            gvf_parm_EffluentToVadose(i)=0

            gvi_timeLagVadose(i)=12

            gvi_WBankingOption(i)=2
            gvf_parm_WStoDI(i)=0.0

            ! Johnson and Vince will use this parameter
            gvf_parm_BWResProp(i)=0.271
            gvf_parm_BWCioProp(i)=0.25 
            !

            gvf_RateResLeak(i)=0.0533
            gvf_RateComLeak(i)=0.0533
            gvf_RateIndLeak(i)=0.0533

            gvf_incidentalCredit(2000,i)=0
            gvf_annualGWStored_acft(i)=0
            !
            ! 10.11.13
            gvf_parm_LitersPFres(i)=0
            gvf_parm_FlushPDres(i)=0
            gvf_hold_RtoVadose(i)=0
            gvf_hold_ROtoOutput(i)=0
            gvf_hold_RtoDInjection(i)=0
            gvf_hold_WStoDI(i)=0
            gvf_hold_RtoOutput(i)=0
            gvf_parm_WBtoSWamount(i)=0
            go_incidentalCredit_acft(i)=0
            go_totalCredits_acft(i)=0
            go_annualCredits_acft(i)=0
            gvf_RateGreyWaterRES(i)=0
            gvf_RateGreyWaterCIO(i)=0
!            go_ProviderPostReclaimed(i)=0
            gvf_parm_SWtoWBamount(i)=0
            gvf_parm_WStoDIamount(i)=0
            gvf_parm_SWtoVadoseAmt(i)=0
            !
            gvi_retainVadoseFlux(i)=0
            ! --------------------------------------
            ! Year,provider,time-step
            do h = gpi_lBY,gpi_uBY,1
             do j = 1,gpi_timeStepWB,1
               lv_fluxReclaimedResGreywater(h,i,j)=0
               lv_fluxReclaimedIndGreywater(h,i,j)=0
               lv_fluxReclaimedComGreywater(h,i,j)=0
               !
               gvf_WBankingBalance(h,i,j)=0.
               gvf_GW_Banking(h,i,j)=0.
             end do
            end do
            !
            gvf_GW_Banking_Hold(i)=0.
            !
            ! Only need to initialize once throught the provider array
            ! --------------------------------------------------------
            if(i < 2)then
             !
             gvl_parm_grayWater=.false.

             ! Black water- calculate toilet water use from gallons per flush etc.
             gvl_parm_toiletPct=.false.
             !
            go_CAGRD_acft=0

             gof_regionalGW=0
             gvf_AddAgVadose=0
             gif_showersBathPCT =0.24
             gii_CAGRD_acft=0

             gvf_AgNet=0
             gvf_AgIn=0
             !
             gof_naturalRecharge=0
             gof_AgAndOtherPumped=0
             gof_Outflow=0
             gvf_regionalGWrecharged=0
             gof_naturalRecharge=0
             gof_AgAndOtherPumped=0
             gof_nonPotable_useOutdoor=0
             !gif_showersBathPCT=0
            go_EffluentAg_acft=0
             !
            endif
            !
            gof_flushesPCD(i)=0
            gof_showersBathPCD(i)=0
            go_GWBanked_acft(i)=0
            go_GWBankUsed_acft(i)=0
            go_ReclaimedWoutput_acft(i)=0
            go_ReclaimedWused_acft(i)=0
            go_ReclaimedWtotal_acft(i)=0
            go_ReclaimedWrecharged_acft(i)=0
            go_ReclaimedWdischarged_acft(i)=0
            go_ROReclaimedWoutput_acft(i)=0
            go_ROReclaimedWused_acft(i)=0
            go_ROReclaimedWDI_acft(i)=0
            go_ReclaimedWDI_acft(i)=0
            go_EffluentPPlant_acft(i)=0
            go_EffluentVadose_acft(i)=0
            go_DemandDeficit_acft(i)=0
            go_EffluentToDischarge_acft(i)=0
            go_Effluent_acft(i)=0
!            go_ReclaimedDifference_acft(i)=0
            go_fluxVadoseToAquifer(i)=0
            go_GrayWaterReclaimed_acft(i,1)=0
            go_GrayWaterReclaimed_acft(i,2)=0
            go_GrayWaterReclaimed_acft(i,3)=0
            !
            ! New City Model parameters- from new Web Interface variables
            ! 12.31.13 
            !
            do j = gpi_laggedStartYr,gpi_uBY,1
             gvf_lagVadoseToAquifer(j,i)=0
            end do
            !
            gvf_rainFall(i)=0
            gvl_rainWaterHarvesting=.false.
            gvl_stormWaterHarvesting=.false.
            gvl_rainWaterHarvestResOnly=.false.
            gvl_IwaniecScenarios_PPtoAg=.false.
            gvl_IwaniecScenarios_PPtoCities=.false.
            gvl_APIcleared=.false.
            !
            do j = 1,gpi_LULC,1
             gvf_landCover_2010(j,i)=0
             gvf_landCover_2060(j,i)=0
             gvf_slope(j,i)=0
             gvf_intercept(j,i)=0
             gvf_propLandCover(j,i)=0
            end do
            ! 06.21.16
            do j = 1,10,1
             gdf_parms_newValueRetain(i,j)=0
            end do
            !
        return
    end subroutine initCity
    ! ---------------------

    ! -----------------------
    subroutine initGeneral(i)
        !
        ! ------ Types ----
        integer :: i,j,k
        ! =================
            !
            ! Vinze-Johnston project
            ! -----------------------------
            gvi_WaterToAgriculture_acft_a(i)=0
!            gvi_WaterToEnvironSalt_acft_a = 0
!            gvi_WaterToEnvironVerde_acft_a = 0
!            gvi_WaterToEnvironCO_acft_a = 0
            ! 01.20.15 DAS new "environmental water" using CAP water 
            gvi_WaterToCOdelta_acft_a=0
            !
            gvf_WaterToCommercialTurf_Prop = 0
            !
            gvi_WaterFromAgSurface_acft_a(i)=0
            go_addedCreditsAgToMuni_acft(i)=0
            go_totalAddedCredits_acft(i)=0
!            gvf_cumulativeCredit(i)=0
            !
            gvi_ProviderAgCreditCurveIndex(i)=9
            ! ---------------------------------
            gvi_ProviderGroundWater_a(i)=0
            !
            ! 07.30.15
            gvi_defaultMandIPumping_Pct(i)=1
            gvf_minimumPumpingMandI(i)=0
            go_CAPunusedPriority4(i)=0
            go_CAPunusedPriority5(i)=0
            go_lossPotentialP4(i)=0
            go_lossPotentialP5(i)=0
            go_waterBankCAP4(i)=0
             !
            do j = 1,3
             gvf_designationsCAPandSRP(i,j)=0.
            end do  
            !
            go_GWPumpageMuni_acft(i)=0
            go_GWRecharge_acft(i)=0
            go_ProviderWaterUsed_acft(i)=0
            !
           ! The following were set starting on 10.14.13
            gvf_setAnnualCredits_acft(i)=0
            do j = gpi_lBY,gpi_uBY,1
             gvf_totalAnnualCredits_acft(j,i)=0.
             gvf_AgCreditWeight(j,i)=0.
            end do
           !
           gvf_ResponseToTemperature=0.
           !
           go_SWdesignations_acft(i)=0
           go_newSupplies_acft_a(i)=0
            !
           gvi_WaterFromAgPumping_acft_a(i)=0
            ! New on 01.09.14
           gvi_defaultAgPumping_acft_a(i)=0
           gvi_DefaultAgSurface_acft_a(i)=0
           go_MaxAgSurface(i)=0
           go_MaxAgPumping(i)=0
           gvf_defaultAgPumping(i)=0
            !
           gvl_removeGWRestrictions(i)=.false.
           gvf_usedGWater_acft(i)=0
           !
           go_ratioCOdeltaBurden=0
           !
           gvf_addedAnnualGWcredits(i)=0
            !
            ! 09.21.15
            do k = gpi_lBY,gpi_uBY,1
             gvf_otherAnnualGWcredits_acft(k,i)=0
             gvf_annualCredits_acft(k,i)=0
             gvd_CreditModel_acft(k,i)=0
            end do
            !
        !
      return
    end subroutine initGeneral
    ! ------------------------

    ! ---------------------
    subroutine initGWater()
        !
        ! ------ Types ----
        integer :: i,j,k
        ! =================
            !
            gvl_parmAllStrawsSucking = .true.
            !
            do j = 1,gvi_Providers,1
!             gv_providerGWFromInterface(j)=0. ! will be MODFLOW GW brought into the model
              !
              do i = gpi_lBY,gpi_uBY,1
                vState_WB_Aquifer(i,j)=0.
                gv_AgWaterPumpingSRV(i,1)=0.
                gv_AgWaterPumpingSRV(i,2)=0.
                gv_AgWaterPumpingSRV(i,3)=0.
                gv_AgWaterPumpingSRV(i,4)=0.
                gv_AgWaterPumpingSRV(i,5)=0.
                gv_AgWaterPumpingSRV(i,6)=0.
                !
                do k = 1,gpi_timeStepWB,1
                  vState_WB_Vadose(i,j,k)=0.
                end do
              end do
             end do
            !
        !
      return 
    end subroutine initGWater
    ! -----------------------

    ! -------------------------
    subroutine initGeneric()
        ! --- Types ---
        integer :: i,j,k
        ! =============            
            !
            gvl_AZdeltaBurden=.false.
            gvi_AgPumpCurveIndex = 9
            gvi_AgCreditTransferPCT=80
            !
            gvf_AgBenchMarkOther=0
            gvf_AgBenchMarkCAP=0
            gvf_AgBenchMarkGWcredits=0
            !
            do i = 1,20,1
             gvf_Ag_alphaSurfOther(i)=0
            end do
            !
              go_AgWaterUsedByMuni_PCT_a=0
            !
            do j = 1,gvi_Providers,1
              go_WaterFromAgPumping_acft_a(j)=0
              go_WaterFromAgSurface_acft_a(j)=0
              !
               go_AgWaterCAP_AF(j)=0
               go_AgWaterCAP_PCT(j)=0
               go_AgWaterSRP_AF(j)=0
               go_AgWaterSRP_PCT(j)=0
               go_AgWaterGW_AF(j)=0
            end do
            !
            go_AgProductionPCT=0
            !
            do i = 1,100,1
              do j = 1,2,1
                gvf_AgSurfOtherEst_acft_a(j,i)=0
                gvf_agSurfOtherEDefault_AF_a(j,i)=0
                gvf_AgSurfCAPEstDefault_acft_a(j,i)=0
                do k = 1,gvi_Providers,1
                 gvf_AgPumpingEst_acft_a(j,k,i)=0
                 gvf_AgSurfCAPEst_acft_a(j,k,i)=0
                enddo
               end do
            end do
        !
            do i = gpi_lBY,gpi_uBY,1
              go_pumpingAgTotal_AF(i)=0
              go_surfaceAgTotal_AF(i)=0
               do k = 1,gvi_Providers,1
                 gvf_newSuppliesBalance_acft(i,k)=0
               end do
            end do
        !
      return
    end subroutine initGeneric
    ! -------------------------

    ! ------------------------
    subroutine initNew()
        ! --- Types ------
        integer :: i,j,k,l
        ! ================            
            !
             gvf_AgGroundWater_2015=0
             gvf_AgSurfaceWater_2015=0
             gvf_AgEffluent_2015=0
            !
            do i = 1,gvi_maxProV,1
             gii_harvestRainWater_AF(i)=0
             gii_StormWaterCapacity_m3(i)=100
             go_harvestRainWater_AF(i)=0
             go_HarvestStormWater_AF(i)=0
             go_stormWaterUsedUsed_AF(i)=0
             go_resGPCDgrayWater(i)=0
             go_comGPCDgrayWater(i)=0
             go_indGPCDgrayWater(i)=0
             go_GrossStormWater_AF(i)=0
             go_NetAgWater_acft=0
             go_GrossAgWater_acft(i)=0
             go_AgToLCLUdemand(i)=0
             !
             ! 06.20.16
             gvf_acerageTotal(i)=0
             gvf_relativeAgArea(i)=0
             do j = gpi_lBY,gpi_uBY
              gvf_acerageAg(j,i)=0
              gvf_AgAFGWTotal(j,i)=0
                ! 03.21.17
              go_pumpAgTotalEfficiency_AF(j)=0

             end do
             !
             gvf_AgEfficiency(i)=1
             do j = 1,3,1
              gvf_AgWaterTotals(j)=0
             end do
            end do        
            !
            gvl_NoleaksYN=.false.
            gvf_grayWaterCompliance=0.5
            gvf_YearsToInflection=12
            !
            if(gvl_start)then
             do i = 1,13,1
              do j = 1,gvi_maxProV,1
                 do k = 1,85,1
                  gvf_areaLCLU(i,j,k)=0
                 end do
                !
                gvf_waterUseRatioLCLU(i,j)=0
                gvf_waterUseLCLUsquareMeter(i,j)=0
              end do
             end do
            endif
            !
            gvf_agEvapotrans=0
            gvi_AgCreditTransferPCT=100
            gvf_ratioMaricopaAg=0
            !
            do k = gpi_lBY,gpi_uBY,1
             gvf_EffluentToAg_diff(k)=0
            end do
            ! 11.16.15
            go_AgEfficiency=0
            !
            do l = 1,3,1
              do k = 1,gvi_maxProV,1
               gvf_AgWaterTotalsP(l,k)  =0  
              end do
            end do
        !
        gvf_rainfallfactor=1.0
        !
     return
    end subroutine initNew
    ! --------------------

    ! -------------------------
    subroutine initWaterSim_6()
        ! ------ Types ---------
        integer :: i,j,k
        ! ======================
        !
            go_indIndoorUse=0
            go_indOutdoorUse=0
            go_resIndoorUse=0
            go_resOutdoorUse=0
            go_comIndoorUse=0
            go_comOutdoorUse=0
            gvl_utilizeLCLU=.true.
            gvi_efficiencyLCLUres=70
            gvf_yearsToAdoptRainWater=10
            gvf_yearsToAdoptGrayWater=10

            gvl_IwaniecYN =.false.
            gvl_LULCstatus=.false.
            !
            gpl_runSensitivity=.false.
            gvi_IwaniecScenario=7
            !
            do i = 1,gvi_maxProV,1
                go_nonpotableUsed(i)=0
                go_demand_MD_AF(i)=0
                go_demand_HD_AF(i)=0
                go_demand_LD_AF(i)=0
                go_demand_TurfTree_AF(1,i)=0  !4-cultivated grass
                go_demand_TurfTree_AF(2,i)=0  !5-greenway
                go_demand_TurfTree_AF(3,i)=0  !10-tree
              do j = 1,gpi_LULC,1
                gvf_LCLUdemand(j,i)=0
                !
                do k = 1,3,1
                  gvf_gpcdLCLUinOut(j,k,1,i)=0
                  gvf_gpcdLCLUinOut(j,k,2,i)=0
                end do
              end do
              !
              gvf_nonPotableTotalUsed(i)=0
              !
              go_RainHarvestToTotalOutdoor(i)=0
              go_GrayWaterToTotalOutdoor(i)=0
              go_ReclaimedToTotalOutdoor(i)=0
            end do
            !
            gvl_waterSim5YN=.false.
        !
      return
    end subroutine initWaterSim_6
    ! ---------------------------
!
End module lm_ParameterControl
!
    ! --------------------------
    subroutine initializeModel()
      use lm_ParameterControl
        !
        call initParmControl()
        call initParameters()
        call initArrays()
        !
       return
     end subroutine initializeModel
    ! -----------------------------
! =================================================================================================
!
Module lms_ParameterControl
  use gm_ModelControl
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !

        character(10),parameter :: Model='ParameterControl'
    !
 contains
    !
    ! -----------------------------
    subroutine modifyDesignations(T)

        ! -------------- Types ---------------
        integer :: i,j
        integer :: lvi_max(10)

        real :: lvf_normalFlow(10),lvf_out(10)
        real :: fModifyNormalFlow
        ! ====================================
        !

        ! -- Type Construct ---
        type(runTime)T
        ! ---------------------
            ! 12.09.13 das
            ! maximum class A that a provider can utilize in acre-ft per acre
            lvi_max=gvi_NormalFlowEmpMax_AFac
            !
            do j = 1,10,1
               lvf_out(j)=0.9 
            end do
            call sModifyNormalFlow(T,lvi_max,gvf_modifyNormalFlow,lvf_out)      
            !
            !lvf_normalFlow=gvf_modifyNormalFlow
            lvf_normalFlow=lvf_out
            !
            if(gvl_start)then
               do i = 1,42,1
                  do j = 1,10,1
                    ! alter the acre-feet per acre designated [comes in from the interface]
                    ! see notes (BuildNotes) 01.18.12 DAS
                     lid_dailyDesignationsSVT(i,j)=fModifyNormalFlow(lvf_normalFlow(j))* lid_dailyDesignationsSVT(i,j)
                  end do
                end do
            endif
      return
    end subroutine modifyDesignations
    ! -------------------------------
 !
End Module lms_ParameterControl
!
! -----------------------------------
 subroutine pCityParameterControl_a(T)
   use lms_ParameterControl
        !
        ! ----- Types ----
        integer :: i,j,k
        real :: lvf_modParams(10)
        real :: lvf_compliance
        real :: response,lvf_years
        real :: lvf_YearsToInflection
        real :: fLogistic
        real :: fInverseLogistic
        ! ================
        !

        ! -- Type Construct --
        type(runTime)T
        ! ====================
        !
        T%atStartOfProviderLoop=.true.
        do k = 1,10,1
         lvf_modParams(k)=0
        end do
        Do i = 1,gvi_maxProV,1
            !
            !if(T%year <=T%startyear )then     
            if(T%year .EQ. T%startyear)then
             !
             ! 06.20.16
             ! SET default values here for the City_Model   
             gvl_catchAPI=.true.
             ! ===============================================
             !
               ! Start Year sets internal first year values and defaults
               call sInputsCity(T,i)
               ! i < 2 used to avoid FORTRAN warning 
               !if(i < 2)then
               if(T%atStartOfProviderLoop)then
                call modifyDesignations(T)          
               endif
                    !
               ! NEW CODE 06.21.16
               ! =====================================================
               ! set New values to be used when gvi_baseYear <= T%year
               ! 
               if(gvl_catchAPI)then
                 if(gpl_release)then
                  gdf_parms_newValueRetain(i,1)=gvf_parm_WWtoRWWTP(i)
                  gdf_parms_newValueRetain(i,2)=gvf_parm_RWWtoRO(i)
                  gdf_parms_newValueRetain(i,3)=gvf_parm_WWtoEffluent(i)
                  gdf_parms_newValueRetain(i,4)=gvf_parm_EffluentToVadose(i)
                  gdf_parms_newValueRetain(i,5)=gvf_parm_EffluentToPP(i)
                  gdf_parms_newValueRetain(i,6)=gvf_parm_SWtoWBamount(i) 
                  gdf_parms_newValueRetain(i,7)=gvf_parm_OutDoorResProp(i)
                    do j = 1,10,1
                     if(gdf_parms_newValueRetain(i,j) < 0.0095)gdf_parms_newValueRetain(i,j)=0
                    end do
                    call invokeDefault(i)
                 else
                  !
                  gdf_parms_newValueRetain(i,1)=gvf_parm_WWtoRWWTP(i)
                  gdf_parms_newValueRetain(i,2)=gvf_parm_RWWtoRO(i)
                  gdf_parms_newValueRetain(i,3)=gvf_parm_WWtoEffluent(i)
                  gdf_parms_newValueRetain(i,4)=gvf_parm_EffluentToVadose(i)
                  gdf_parms_newValueRetain(i,5)=gvf_parm_EffluentToPP(i)
                  gdf_parms_newValueRetain(i,6)=gvf_parm_SWtoWBamount(i) 
                  gdf_parms_newValueRetain(i,7)=gvf_parm_OutDoorResProp(i)
                    !
                    call invokeDefault(i)
                    !
                 endif
               endif
                !
            else
              !
              if(gvi_baseYear <= T%year)then
                if(gpl_release)then
                   ! if(T%year <=gvi_baseYear)then
                    if(T%year <=gvi_baseYear+1)then
                     call checkDefault(i,T)
                    endif
                      !
                       lvf_compliance=1
                       span=10
                       lvf_YearsToInflection=10
                      ! use interface values
                      ! --------------------------------------------------------------------------------------------------------
                      !
                    if(gvl_waterSim5YN)then
                        !
                        gvf_parm_WWtoRWWTP(i) =gdf_parm_WWtoRWWTP(i)
                        gvf_parm_RWWtoRO(i)=gdf_parm_RWWtoRO(i)
                        gvf_parm_WWtoEffluent(i) =gdf_parm_WWtoEFF(i)
                        gvf_parm_EffluentToVadose(i)=gdf_parm_EFFtoVadose(i)
                        gvf_parm_EffluentToPP(i)=gdf_parm_EFFtoPP(i)
                        gvf_parm_SWtoWBamount(i)=gdf_parm_SWtoWBamt(i)
                        gvf_parm_OutDoorResProp(i)  =gdf_parm_OutDoorResPCT(i)*0.01
                        !
                    else
                      !
                      if(gvl_defaultRun(1,i))then
                        gvf_parm_WWtoRWWTP(i) =gdf_parm_WWtoRWWTP(i)
                        lvf_modParams(1) =1.0
                      else
                       lvf_modParams(1)=fLogistic(T%policyYear,lvf_compliance,span,lvf_YearsToInflection)
                        gvf_parm_WWtoRWWTP(i)        =gdf_parms_newValueRetain(i,1)*lvf_modParams(1)   
                      endif
                      !
                      if(gvl_defaultRun(2,i))then
                        gvf_parm_RWWtoRO(i)=gdf_parm_RWWtoRO(i)
                        lvf_modParams(2)=1
                      else
                        lvf_modParams(2)=fLogistic(T%policyYear,lvf_compliance,span,lvf_YearsToInflection)
                        gvf_parm_RWWtoRO(i)          =gdf_parms_newValueRetain(i,2)*lvf_modParams(2)
                      endif
                      !
                      if(gvl_defaultRun(3,i))then
                        gvf_parm_WWtoEffluent(i) =gdf_parm_WWtoEFF(i)
                        lvf_modParams(3)=1
                      else
                        lvf_years=10
                        !lvf_modParams(3)=fLogistic(T%policyYear,lvf_compliance,span,lvf_YearsToInflection)
!                        gvf_parm_WWtoEffluent(i)    =gdf_parms_newValueRetain(i,3) !*lvf_modParams(3)
                        call sRampParameters(T%policyYear,lvf_years,gdf_parm_WWtoEFF(i),gdf_parms_newValueRetain(i,3),response)
                        gvf_parm_WWtoEffluent(i)=response
                      endif
                      !
                      if(gvl_defaultRun(4,i))then
                        gvf_parm_EffluentToVadose(i)=gdf_parm_EFFtoVadose(i)
                        lvf_modParams(4)=1
                      else
                        !lvf_modParams(4)=fLogistic(T%policyYear,lvf_compliance,span,lvf_YearsToInflection)
                        !gvf_parm_EffluentToVadose(i)=gdf_parms_newValueRetain(i,4)!*lvf_modParams(4)
                        call sRampParameters(T%policyYear,lvf_years,gdf_parm_EFFtoVadose(i),gdf_parms_newValueRetain(i,4),response)
                        gvf_parm_EffluentToVadose(i)=response
                      endif
                      !
                      if(gvl_defaultRun(5,i))then
                        gvf_parm_EffluentToPP(i)=gdf_parm_EFFtoPP(i)
                        lvf_modParams(5)=1
                      else
                        ! 
                        ! Taken care of in Effluent subroutine, City Model
                        !lvf_modParams(5)=fLogistic(T%policyYear,lvf_compliance,span,lvf_YearsToInflection)
                        gvf_parm_EffluentToPP(i)    =gdf_parms_newValueRetain(i,5) !*lvf_modParams(5)
                      endif
                      ! 
                      !
                      if(gvl_defaultRun(6,i))then
                        gvf_parm_SWtoWBamount(i)=gdf_parm_SWtoWBamt(i)
                        lvf_modParams(6)=1
                      else
                        ! Happens immediately (no delay in the time lag)
                        lvf_modParams(6)=fLogistic(T%policyYear,lvf_compliance,span,lvf_YearsToInflection)
                        gvf_parm_SWtoWBamount(i)    =gdf_parms_newValueRetain(i,6) !*lvf_modParams(6)
                      endif
                      !
                      !
                      if(gvl_defaultRun(7,i))then
                        gvf_parm_OutDoorResProp(i)  =gdf_parm_OutDoorResPCT(i)*0.01
                        lvf_modParams(7)=1
                      else
                        lvf_YearsToInflection=8
                        span=8
                        lvf_modParams(7)=fInverseLogistic(T%policyYear,lvf_compliance,span,lvf_YearsToInflection)
                        gvf_parm_OutDoorResProp(i) = max(gdf_parms_newValueRetain(i,7), (gdf_parm_OutDoorResPCT(i)*0.01 ) *lvf_modParams(7) )
                      endif
                      !
                    endif
                      !

                else
                !
                 ! Debug Mode
                 ! -------------------
                 call invokeDefault(i)
                 ! -------------------
                endif
              else
                ! use default values
                gvl_catchAPI=.false.
              endif
                !
                10 format(I4,1x,I2,1x,l2,1x,4(F6.2,1x))
                100 format(I4,1x,I2,1x,3(F6.2,1x))
                120 format(I4,1x,I2,1x,14(F7.2,1x))
                !
                if(.not. gvl_parm_shortage(i))then   
                  gvf_hold_WStoDI(i)=gvf_parm_WStoDI(i)
                  gvf_hold_RtoDInjection(i)=gvf_parm_RtoDInjection(i)
                  gvf_hold_ROtoOutput(i)=gvf_parm_ROtoOutput(i)
                  gvf_hold_RtoOutput(i)=gvf_parm_RtoOutput(i)
                  gvf_hold_RtoVadose(i)=gvf_parm_RtoVadose(i)
                  !
                  ! 11.08.15
                endif
               !
            endif
            !
            ! All time
            ! -----------------------
            !
            gvf_hold_WStoDI(i)=gvf_parm_WStoDI(i)
            gvf_hold_RtoDInjection(i)=gvf_parm_RtoDInjection(i)
            gvf_hold_ROtoOutput(i)=gvf_parm_ROtoOutput(i)
            gvf_hold_RtoOutput(i)=gvf_parm_RtoOutput(i)
            gvf_hold_RtoVadose(i)=gvf_parm_RtoVadose(i)
            !
            ! Ajay and Johnston project- 08.08.12 DAS
            ! 
            if(0. < gvi_WaterToAgriculture_acft_a(i))then
                !
                ! Direct no waste water to the Reclaimed WWTP- 
!                gvf_parm_WWtoRWWTP(i)=0.
                ! Sent all reclaimed WW to Reclained Water(i.e., not RO)
                gvf_parm_RWWtoRO(i)=0.0
                ! Use reclaimed water, if available to meet Agricultural demands
                gvf_parm_RtoOutput(i)=1.
                ! Use, what ? 90% of all reclained output (assume a loss of 10%)?
                gvf_parm_RtoInputMaxPct(i)=0.9
                !
                !gvf_parm_ReclaimedOutdoor(i)=0.5
                !
            endif
            !
            T%atStartOfProviderLoop=.false.
            !
        End do
        !
            ! 
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=1
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
            !
        !
       return
 end subroutine pCityParameterControl_a
 ! -------------------------------------

 ! -------------------------------------
 subroutine invokeDefault(i)
   use lms_ParameterControl
   integer :: i
   real :: min=0.01
    !
    gvf_parm_WWtoEffluent(i)=gdf_parm_WWtoEFF(i)
    gvf_parm_WWtoRWWTP(i)=gdf_parm_WWtoRWWTP(i)
    gvf_parm_RWWtoRO(i)=gdf_parm_RWWtoRO(i)
    gvf_parm_EffluentToVadose(i)=gdf_parm_EFFtoVadose(i)
    gvf_parm_EffluentToPP(i)=gdf_parm_EFFtoPP(i)
    gvf_parm_SWtoWBamount(i)=gdf_parm_SWtoWBamt(i)
    gvf_parm_OutDoorResProp(i)=gdf_parm_OutDoorResPCT(i)*0.01
    ! Rounding errors- remove
    if(gvf_parm_WWtoEffluent(i) <= min)gvf_parm_WWtoEffluent(i) =0
    if(gvf_parm_WWtoRWWTP(i) <= min)gvf_parm_WWtoRWWTP(i)=0
    if(gvf_parm_RWWtoRO(i) <= min)gvf_parm_RWWtoRO(i)=0
    if(gvf_parm_EffluentToVadose(i) <=min)gvf_parm_EffluentToVadose(i)=0
    if(gvf_parm_EffluentToPP(i) <= min)gvf_parm_EffluentToPP(i)=0
    if( gvf_parm_SWtoWBamount(i) <= min) gvf_parm_SWtoWBamount(i)=0
    if(gvf_parm_OutDoorResProp(i) <= min)gvf_parm_OutDoorResProp(i)=0
    !
   return
 end subroutine invokeDefault
 ! -------------------------------------
 ! ---------------------------
 subroutine checkDefault(i,T)
   use lms_ParameterControl
    ! --------- Types ---------
    integer :: i
    real :: lvf_diff
    real :: lvf_sum_A,lvf_sum_B
    ! =========================

    ! -- Type Construct --
    type(runTime)T
    ! ====================
    !
    lvf_diff=0
    lvf_sum_A=0
    lvf_sum_B=0
    !
    if(anint(1000*gdf_parms_newValueRetain(i,1)) <= anint(1000*gdf_parm_WWtoRWWTP(i)))then
      gvl_defaultRun(1,i)=.true.   
    else
      gvl_defaultRun(1,i)=.false.
    endif
    if(anint(1000*gdf_parms_newValueRetain(i,2))<= anint(1000*gdf_parm_RWWtoRO(i)))then
      gvl_defaultRun(2,i)=.true.   
    else
      gvl_defaultRun(2,i)=.false.
    endif
     !
     lvf_sum_A=(1000*gdf_parms_newValueRetain(i,3))
     lvf_sum_B=(1000*gdf_parm_WWtoEFF(i))
     lvf_diff=lvf_sum_A-lvf_sum_B
     !
    if(abs(lvf_diff) < 1.0D-3)then
      gvl_defaultRun(3,i)=.true.   
    else
      gvl_defaultRun(3,i)=.false.
    endif
    !
     lvf_sum_A=(1000*gdf_parms_newValueRetain(i,4))
     lvf_sum_B=(1000*gdf_parm_EFFtoVadose(i))
     lvf_diff=lvf_sum_A-lvf_sum_B
    if(abs(lvf_diff) < 1.0D-3)then
      gvl_defaultRun(4,i)=.true.   
    else
      gvl_defaultRun(4,i)=.false.
    endif
    !
     lvf_sum_A=(1000*gdf_parms_newValueRetain(i,5))
     lvf_sum_B=(1000*gdf_parm_EFFtoPP(i))
     lvf_diff=lvf_sum_A-lvf_sum_B
    if(abs(lvf_diff) < 1.0D-3)then
      gvl_defaultRun(5,i)=.true.   
    else
      gvl_defaultRun(5,i)=.false.
    endif
    !
     lvf_sum_A=(1000*gdf_parms_newValueRetain(i,6))
     lvf_sum_B=(1000*gdf_parm_SWtoWBamt(i))
     lvf_diff=lvf_sum_A-lvf_sum_B
    if(abs(lvf_diff) < 1.0D-3)then
      gvl_defaultRun(6,i)=.true.   
    else
      gvl_defaultRun(6,i)=.false.
    endif
    !
     lvf_sum_A=(1000*gdf_parms_newValueRetain(i,7))
     lvf_sum_B=(1000*gdf_parm_OutDoorResPCT(i)*0.01)
     lvf_diff=lvf_sum_A-lvf_sum_B
    if(abs(lvf_diff) < 1.0D-3)then
      gvl_defaultRun(7,i)=.true.   
    else
      gvl_defaultRun(7,i)=.false.
    endif
    !
 end subroutine checkDefault
 ! -------------------------

 ! -------------------------------------
 subroutine sParameterControl_tstep(T,i)
   use lms_ParameterControl

      ! ---- Types ---
      integer :: i
      ! ==============
      !

      ! --- Type Construct ---
      type(runTime)T
      ! ======================
          !
          if(gvl_parm_shortage(i))then        
            gvf_parm_WStoDI(i)=0.0
            gvf_parm_RtoDInjection(i)=0.0  
            gvf_parm_RWWtoRO(i)=0.0
            gvf_parm_RtoVadose(i)=0.0
          else
            gvf_parm_WStoDI(i)= gvf_hold_WStoDI(i)
            gvf_parm_RtoDInjection(i)=gvf_hold_RtoDInjection(i)
            gvf_parm_ROtoOutput(i)=gvf_hold_ROtoOutput(i)
            gvf_parm_RtoOutput(i)=gvf_hold_RtoOutput(i)
            gvf_parm_RtoVadose(i)=gvf_hold_RtoVadose(i)
          endif
        return
       !
 end subroutine sParameterControl_tstep
 ! ------------------------------------

 ! ------------------------
 subroutine ClearInclude(T)
   use lms_ParameterControl
     !
     !-- TYPE constructs --
     type(runTime)T
     ! ====================
      !
        gvf_AG_Banking(T%year)=0
      !
   return
 end subroutine ClearInclude
 ! -------------------------

        !
        ! -------------------------
        subroutine sInputsCity(T,i)
          use gm_ModelControl
           use lms_ParameterControl

            ! ---- Types ----
            integer :: i,j,k
            ! ===============
            !

            ! -- Type Construct --
            type(runTime)T
            ! ====================
                ! Called once
                  gvf_WBankingBalance(T%year,i,1)=0.
                  if(1 < gvi_WBankingOption(i))then
                    gvf_WBankingBalance(T%year,i,1)=gvf_parm_SWtoWBamount(i)
                  endif
                !
                  if(.not. gpl_release)then
                     gvi_WBankingOption(i)=2
                      gvf_parm_SWtoVadoseAmt(i)=0
                      gvf_parm_WStoDIamount(i)=0
                  endif
                !
                ! These will be annual amounts.  Must be defined in the interface

                  if(0 < gvf_parm_WWtoRWWTP(i))then
                    if(0 <  gvf_parm_RtoOutput(i))then
                       if(2000 < T%year)then
                        ! This code has never been called - 04.16.19 - 
                         gvf_reclaimedInput(T%year-1,i,1) =0
                        gvf_reclaimedInput(T%year-1,i,1) = &
                          (gvf_WaterDemand_acft(i,1,1)*(1-gvf_parm_OutDoorResProp(i)) &
                          * gvf_parm_RtoOutput(i)*(1-gvf_parm_RWWtoRO(i))*gvf_parm_WWtoRWWTP(i) &
                          * gvf_parm_RtoInputMaxPct(i) ) * 10 
                        !
                        gvf_reclaimedOutput(T%year-1,i,1) =gvf_reclaimedInput(T%year-1,i,1)
                        !
                       else
                        gvf_reclaimedInput(T%year,i,1) = &
                          (gvf_WaterDemand_acft(i,1,1)*(1-gvf_parm_OutDoorResProp(i)) &
                          * gvf_parm_RtoOutput(i)*(1-gvf_parm_RWWtoRO(i))*gvf_parm_WWtoRWWTP(i) &
                          * gvf_parm_RtoInputMaxPct(i) ) * 10
                        ! 03.05.12 DAS
                        gvf_reclaimedOutput(T%year,i,1) =gvf_reclaimedInput(T%year,i,1)
                       endif
                    else
                        if(2000 < T%year)gvf_reclaimedInput(T%year-1,i,1)  =0
                         gvf_reclaimedInput(T%year,i,1)    =0
                        if(2000 < T%year)gvf_reclaimedOutput(T%year-1,i,1) =0
                         gvf_reclaimedOutput(T%year,i,1)   =0
                    endif
                    if(0 <  gvf_parm_ROtoOutput(i))then
                      if(2000 < T%year)then
                        ! And, this code has never been called - 04.18.19 - 
                         gvf_ROreclaimedInput(T%year,i,1)= &
                           gvf_WaterDemand_acft(i,1,1)* (1-gvf_parm_OutDoorResProp(i)) &
                            *  gvf_parm_RWWtoRO(i)* gvf_parm_ROtoOutput(i)*gvf_parm_WWtoRWWTP(i)

                         gvf_ROreclaimedOutput(T%year-1,i,1) =gvf_ROreclaimedInput(T%year,i,1)
                      else
                        gvf_ROreclaimedInput(T%year,i,1)= &
                           gvf_WaterDemand_acft(i,1,1)* (1-gvf_parm_OutDoorResProp(i)) &
                            *  gvf_parm_RWWtoRO(i)* gvf_parm_ROtoOutput(i)*gvf_parm_WWtoRWWTP(i)

                         gvf_ROreclaimedOutput(T%year,i,1) =gvf_ROreclaimedInput(T%year,i,1)
                      endif
                    endif
                  endif
                !
               ! This is in the code twice. Once here and once in ProviderPopulationAndDemand.f90
                  do j = 1,gpi_unusedNonPotable,1
                    do k = 1,14,1
                      gvf_WaterDemand_acft(i,j,k)=0
                    end do
                  end do
                !
            !
         return
        end subroutine sInputsCity
        ! ------------------------
!
! =============================================================================================================== 
! E.O.F Parameter_control.f90