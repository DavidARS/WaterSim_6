!
! File is Water_CityModel.f90
!
! This file contains the code for modeling the water budgets of a city-provider based on the  
! work by  Chi Chi Choi and my implementation of that work.  Paul Westerhoff (Engineering)
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
!
! Global OUTPUTS:            
!
! Local OUTPUTS:
!   
! Local INPUTS:
!---------------
!
!   Module lms_CitiWaterBudgets
!       subroutine sAquifer(T,lvWB,i,vTstep)
!       subroutine sCommercial(T,lvWB,i,vTstep)
!           subroutine sShowerMinutes(lvWB,i,water,days,minPCD)
!       subroutine sIndustrial(T,lvWB,i,vTstep)
!       subroutine sDirectInjection(T,lvWB,i,vTstep)
!       subroutine sEffluent(T,lvWB,i,vTstep)
!       subroutine sGWtreatment(T,lvWB,i,vTstep)
!       subroutine sIndustrialReuse(T,lvWB,i,vTstep)
!       subroutine sReclaimed(T,lvWB,i,vTstep)
!       subroutine sResidential(T,lvWB,i,vTstep)
!
!
!
!
!
!
! created on 10.09.09
!
! david arthur sampson

! last write was: 08.16.13,07.21.14
! ----------------------------------
!

! ======================================================================================================
!
Module lm_CitiWaterBudgets
 use gm_ModelControl
  use gm_GlobalData
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
  contains
    !
    ! ---------------------
     subroutine initCityModel()
       call openFiles_cm()
       call readFiles_cm()
      return
     end subroutine initCityModel
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
              module="lm_CitiWaterBudgets"
              !
!               Infile='App_Data\Parameters\parm_default.txt'; LU=80
!               call openFiles(module,lvc_DPath,Infile,LU)
              !
               Infile='App_Data\Data\landUseLandCover_2010.txt'; LU=81
               call openFiles(module,lvc_DPath,Infile,LU)
              !
              Infile='App_Data\Data\lULC_Scenario.txt'; LU=82
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
            integer :: i,j,ios
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
         return
1000     continue
           if(gvl_writeLog)then
              string=63
                call sStrings(string,errorString)
                call eWrite(errorString,LU)
            endif
          gvl_errorFlag=.false.
          !
        end subroutine readFiles_cm
        ! --------------------------
!
End Module lm_CitiWaterBudgets

! ---------------------------------
    subroutine initializeCityModel()
        use lm_CitiWaterBudgets
        !
!            call initCityModel()
        !
      return
    end subroutine initializeCityModel
    ! -----------------------------------
!

Module lms_CitiWaterBudgets
 use gm_ModelControl
  use gm_GlobalData
    use gm_TypeControl
      use gm_DataAndSensitivity
        !
        ! ----------------------- Module Global Variable Types ---------------
        !
        integer,private :: Use
        ! Number of flux parameters for each state
        integer,private :: parms_Reclaimed=3,parms_Effluent=2
        integer,private :: parms_WWTP=2,parms_RWWTP=1
        integer,private :: parms_RO=1,parms_SW=2
        integer,private :: parms_WS=3

     !   real, private :: mvf_gallonsPerFlush,lv_flushesPerDay
        real :: lvf_BWtypical
        real, private :: lvf_BWaterUsed,lvf_BWaterNotUsed,mvf_BWpct
        real,private :: gvf_EffluentPP,gvf_RegionalWWSurDischarge
        !
      !  real :: mvf_vK1(gvi_maxProV)=0.207
        real :: mvf_vK2(gvi_maxProV)=0.793  ! Evapotranspiration-outdoor water use Chi Chi Cho/ Westerhoff
        !
        real :: mvf_indoorDemandPotable(gvi_maxProV,gpi_timeStepWB)
        real :: mvf_outdoorDemandPotable(gvi_maxProV,gpi_timeStepWB)
        real :: mvf_indoorDemandNonPotable(gvi_maxProV,gpi_timeStepWB)
        real :: mvf_outdoorDemandNonPotable(gvi_maxProV,gpi_timeStepWB)
        real :: mvf_unusedNonPotable(gvi_maxProV,gpi_timeStepWB)

        !    efficiency/ system losses
!        real,private :: mpf_SWTP_efficiency=0.97
!        real,private :: mpf_WS_efficiency=0.92
!        real,parameter :: mpf_WW_sourceEfficiency=0.92
!        real,parameter :: mpf_WWTP_efficiency=0.97
!        real,parameter :: mpf_RWWTP_efficiency=0.97
!        real,parameter :: mpf_RO_efficiency=0.95
!        real,parameter :: mpf_Rec_efficiency=0.95

        logical,public :: Citi_newYear
        logical, public :: lvl_firstLoop
        logical,public :: initParms
        logical, public :: mvl_waterSim6=.true.
        !
        real :: mvf_lost,mvf_unusedGrayWater
        real, parameter :: mpf_yearsToAdoptAdd=32
        real :: mpf_flushesPerDay=5.05
        real :: mpf_gallonsPerFlush=3.63 
        real :: mpf_showersBathPCT=0.24

        ! Testing -------------------------
        real :: temp_1,temp_2,temp_3,temp_4
        real :: out,in
        ! =================================

        ! min and max fluxes
        character(3):: mvc_use
        ! =====================================================================

        ! ---------------
        ! created locally
        ! ---------------
        ! State variables
        real, private :: vState_WB_CanalWater(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
        real, private :: vState_WB_SWTP(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
        real, private :: vState_WB_WaterSupply(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
        real, private :: vState_WB_WWTP(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
        real, private :: vState_WB_RWWTP(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
        real, private :: vState_WB_GWTP(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
        real, private :: vState_ReclaimedWater(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
        real, private :: vState_DirectInjection(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
        real, private :: vState_WB_Effluent(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
        real, private :: vState_WB_GWB(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)

        real, private :: vState_WB_GW(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
        !
        real, private :: vState_WB_Residential(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
         real, private :: vState_WB_ResidentialIndoor(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
          real, private :: vState_WB_ResidentialOutdoor(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
        !
        real, private :: vState_WB_Industry(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
         real, private :: vState_WB_IndustrialIndoor(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
          real, private :: vState_WB_IndustrialOutdoor(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
        !
        real, private :: vState_WB_Commercial(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
         real, private :: vState_WB_CommercialIndoor(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
          real, private :: vState_WB_CommercialOutdoor(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)

        real, private :: vState_WB_IReuse(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)

        real, private :: vState_ROprocess(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
        ! ------------------------------------------------------------------------------------------
        !
        ! -----
        include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
        !

        ! ------------------------------------------------
        !
        type waterbalance
         real :: vGWtoAg(gvi_maxProV)=0.7

          real :: lvf_municipalPumping_acft_(gvi_maxProV)
           real :: lvf_municipalRecharge_acft_(gvi_maxProV)

          real :: lvf_demand_Res_outdoor_acft_(gvi_maxProV,gpi_timeStepWB)
           real :: lvf_demand_Res_indoor_acft_(gvi_maxProV,gpi_timeStepWB)

          real :: lvf_demand_Ind_outdoor_acft_(gvi_maxProV,gpi_timeStepWB)
           real :: lvf_demand_Ind_indoor_acft_(gvi_maxProV,gpi_timeStepWB)

          real :: lvf_demand_Com_outdoor_acft_(gvi_maxProV,gpi_timeStepWB)
           real :: lvf_demand_Com_indoor_acft_(gvi_maxProV,gpi_timeStepWB)

            real :: lvf_demand_acft(gvi_maxProV,gpi_timeStepWB)

          ! this will read Reclained_input 
          real :: lv_fluxFromWS_nonPotable(gvi_maxProV,gpi_timeStepWB)
          ! This is ROreclaimed water input
          real :: lv_fluxFromWS_Potable(gvi_maxProV,gpi_timeStepWB)
           real :: lv_fluxFromWS_Groundwater(gvi_maxProV,gpi_timeStepWB)
            real :: lv_fluxFromWS_SRPandCAP(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxFromWS_Res_nonPotable(gvi_maxProV,gpi_timeStepWB)
          real :: lv_fluxFromWS_Res_ROPotable(gvi_maxProV,gpi_timeStepWB)
           real :: lv_fluxFromWS_Res_Groundwater(gvi_maxProV,gpi_timeStepWB)
            real :: lv_fluxFromWS_Res_SRPandCAP(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxFromWS_Ind_nonpotable(gvi_maxProV,gpi_timeStepWB)
          real :: lv_fluxFromWS_Ind_ROPotable(gvi_maxProV,gpi_timeStepWB)
           real :: lv_fluxFromWS_Ind_Groundwater(gvi_maxProV,gpi_timeStepWB)
            real :: lv_fluxFromWS_Ind_SRPandCAP(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxFromWS_Com_nonpotable(gvi_maxProV,gpi_timeStepWB)
          real :: lv_fluxFromWS_Com_ROPotable(gvi_maxProV,gpi_timeStepWB)
           real :: lv_fluxFromWS_Com_Groundwater(gvi_maxProV,gpi_timeStepWB)
            real :: lv_fluxFromWS_Com_SRPandCAP(gvi_maxProV,gpi_timeStepWB)

             real :: lvf_fluxDemandDeficit(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxFromWS_Res_Potable(gvi_maxProV,gpi_timeStepWB)
          real :: lv_fluxFromWS_Ind_Potable(gvi_maxProV,gpi_timeStepWB)
          real :: lv_fluxFromWS_Com_Potable(gvi_maxProV,gpi_timeStepWB)

          real :: lvf_fluxEffluentToAg(gvi_maxProV,gpi_timeStepWB)
          real :: lvf_fluxEffluentToPowerPlant(gvi_maxProV,gpi_timeStepWB)
          real :: lvf_fluxEffluentToRWSDischarge(gvi_maxProV,gpi_timeStepWB)

          real :: lv_ROflux_RO_Reclaimed(gvi_maxProV,gpi_timeStepWB)
          real :: lv_ROflux_DirectInject_Potable(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxFromSWTP(gvi_maxProV,gpi_timeStepWB)
          real :: lvf_fluxFromSWTP_WS_acft(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxResidentialWW(gvi_maxProV,gpi_timeStepWB)
           real :: lv_fluxResidential(gvi_maxProV,gpi_timeStepWB)
            real :: lv_fluxResIndoor(gvi_maxProV,gpi_timeStepWB)
             real :: lv_fluxResOutdoor(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxToRWWTP(gvi_maxProV,gpi_timeStepWB)
          real :: lv_fluxRWWTPToReclaimed(gvi_maxProV,gpi_timeStepWB)

          real :: lvf_fluxReclaimedToVadose(gvi_maxProV,gpi_timeStepWB)
           real :: lvf_fluxReclaimedToDirectInject(gvi_maxProV,gpi_timeStepWB)
            real :: lvf_fluxReclaimedToOutput(gvi_maxProV,gpi_timeStepWB)
             real :: lvf_fluxReclaimedToRWWSDischarge(gvi_maxProV,gpi_timeStepWB)
              real :: lvf_fluxReclaimedDifference(gvi_maxProV,gpi_timeStepWB)
               real :: lvf_fluxReclaimedTotal(gvi_maxProV,gpi_timeStepWB)

          real :: lvf_fluxDirectInjectToGW(gvi_maxProV,gpi_timeStepWB)
           real :: lvf_fluxWSupplyToDirectInjectTotal(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxWWTPToEffluent(gvi_maxProV,gpi_timeStepWB)
          real :: lv_fluxToRevOsmosis(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxWWToIRuse(gvi_maxProV,gpi_timeStepWB)
           real :: lv_fluxToWWTPFromIRuse(gvi_maxProV,gpi_timeStepWB)
            real :: lv_fluxWWToSurfaceDischarge(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxIndWW(gvi_maxProV,gpi_timeStepWB)
           real :: lv_fluxInd(gvi_maxProV,gpi_timeStepWB)
            real :: lv_fluxIndindoor(gvi_maxProV,gpi_timeStepWB)
             real :: lv_fluxIndOutdoor(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxComWW(gvi_maxProV,gpi_timeStepWB)
           real :: lv_fluxCom(gvi_maxProV,gpi_timeStepWB)
            real :: lv_fluxComIndoor(gvi_maxProV,gpi_timeStepWB)
             real :: lv_fluxComOutdoor(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxToVadoseFromEffluent(gvi_maxProV,gpi_timeStepWB)
           real :: lv_fluxCioVadose(gvi_maxProV,gpi_timeStepWB)
            real :: lv_fluxResVadose(gvi_maxProV,gpi_timeStepWB)
             real :: lv_fluxVadoseToAquifer(gvi_maxProV,gpi_timeStepWB)
              real :: lv_fluxToVadoseFromAg(gvi_maxProV,gpi_timeStepWB)

          real :: lvf_fluxGWaterToGWaterTreatment(gvi_maxProV,gpi_timeStepWB)
           real :: lvf_fluxGWaterTreatmentToWSupply(gvi_maxProV,gpi_timeStepWB)
            real :: lvf_fluxGWaterToWSupply(gvi_maxProV,gpi_timeStepWB)

          real :: lvf_fluxSWaterToGWaterBanking(gvi_maxProV,gpi_timeStepWB)
           real :: lvf_fluxGWaterBankingToSWater(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)

          real :: lvf_fluxWStoDirectInjection(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxReclaimedGreywaterInd(gvi_maxProV,gpi_timeStepWB)
           real :: lv_fluxReclaimedGreywaterCom(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxReclaimedGreywaterRes(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxGWtoAgIrrigation(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxSurfaceWaterToVadose(gvi_maxProV,gpi_timeStepWB)
           real :: lv_fluxSurfaceWaterToSWTP(gvi_maxProV,gpi_timeStepWB)
            real :: lv_fluxSurfaceWater_annual(gvi_maxProV)

          real :: lvf_fluxSWaterCAP(gvi_maxProV)
          real :: lvf_fluxSWaterSRP(gvi_maxProV)
            real ::  lvf_fluxSWaterSRP_NCS(gvi_maxProV),lvf_fluxSWaterSRP_A(gvi_maxProV)
          real :: lvf_fluxGWaterSRP(gvi_maxProV)
            real :: lvf_fluxBCstorageSRP(gvi_maxProV)

          real :: lv_fluxGWtoWTP(gvi_maxProV,gpi_timeStepWB)
           real :: lv_runoffPerc(gvi_maxProV,gpi_timeStepWB)
            real :: lvf_runoff(gvi_maxProV,gpi_timeStepWB)
             real :: lvf_rainfallEvaporation(gvi_maxProV,gpi_timeStepWB)
              !
              ! WaterSim 6- store surface runoff for use
              real :: lvf_runoffStorage(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxAtmosTotal(gvi_maxProV,gpi_timeStepWB)
          real :: lvf_vadoseFlux(gvi_maxProV,gpi_timeStepWB)

          real :: lv_fluxWWtoPaleoVerde(gvi_maxProV,gpi_timeStepWB)
          real :: lv_transferFluxPaleoVerdeToOther(gvi_maxProV,gpi_timeStepWB)
          real :: lv_SRPtoEnergy(gpi_timeStepWB)

          real :: lv_providerPop(gvi_maxProV)

          real :: lvf_CAPuse_acft_month(gvi_maxProV,gpi_timeStepWB)

          real :: lvf_SRPclassBC_acft_month(gvi_maxProV,gpi_timeStepWB)
          !
          real :: lvf_TmaxResponse(gpi_timeStepWB)

          real :: lvf_GWRecharged(gpi_lBY:gpi_uBY),lvf_GWPumped(gpi_lBY:gpi_uBY)
          ! 11.11.15
          real :: lvf_GPCD_res(gvi_maxProV,2),lvf_GPCD_com(gvi_maxProV,2),lvf_GPCD_ind(gvi_maxProV,2)
          !
          real :: lvf_LCLU_proportions(gvi_maxProV,gpi_LULC)
          !
          real :: lvf_addToRunoffGrayWater(gvi_maxProV)
          !
          ! -------------------------------------------
          ! New Inputs for WaterSim 6
          !
          ! WaterSim 6 additions
          ! ---------------------
          real :: lvf_excessNonPotable(gvi_maxProV),lvf_outdoorNonPotable(gvi_maxProV),lvf_outdoorPotable(gvi_maxProV)
          real :: lvf_indoorNonPotable(gvi_maxProV), lvf_indoorPotable(gvi_maxProV)
          real :: lvf_unmetIndoorPotable(gvi_maxProV),lvf_unmetOutdoorPotable(gvi_maxProV)
          real :: lvf_rainWaterHarvested(gvi_maxProV),lvf_stormWaterCaptured(gvi_maxProV),lvf_grayWaterRecovered(gvi_maxProV)
          !
          integer:: lvi_ResComInd
          ! -----------------------------------------------------------------------------------------
          !
        end type waterbalance
        !
        ! ----- Types imbedded -----------
        type parameters
         real :: In_1,In_2,In_3,In_4
         real :: Out_1,Out_2,Out_3,Out_4
        end type parameters
        ! --------------------------------
        !
  contains
        !
        ! ----------------------------------
        subroutine sAquifer(T,lvWB,i,vTstep)
            !
            ! ----------------------------------- Types ----------------------------------
            integer :: i
            integer :: vTstep
!            integer,parameter :: state=19

            real :: lv_AnnualWB(gvi_maxProV)
            real :: lv_add(gvi_maxProV,gpi_timeStepWB),lv_sbt(gvi_maxProV,gpi_timeStepWB)
            real :: lvf_GWaterOutflow(gvi_maxProV)
            real, parameter :: lpf_otherpumping_acft=179645
            real, parameter :: lpf_stanfieldOutflow_acft=29200
            real,parameter :: lpf_gilaOutflow_acft=7500
            real,parameter :: lpf_allInflows_acft=31800
            real,parameter :: lpf_initialVolume=61918314
            real :: lvf_cagrd_acft
            real :: lvf_outFlowTotal_acft,lvf_otherPumping_acft
            real :: lvf_regionalInputs,lvf_regionalOutflows
            real:: tvf_addIn_1,tvf_addOut_1,tvf_addIn_2,tvf_addOut_2,tvf_addOut_3
            ! ================================================================================
            !

            ! -- Type Constructs --
            type (waterbalance)lvWB
            type(runTime)T
           ! ======================
             !
             call sMask(gvl_mask)
             !
             ! --------------
!             call sRangeCheck(state,i,vTstep,gdf_parm_minGWtoGWT(i),gdf_parm_maxGWtoGWT(i),gvf_parm_GWtoGWTP(i),gvf_parm_GWtoGWTP(i))
             !
              ! variables sent in to this subroutine from Provider.f90
              ! -------------------------------------------------------
 
              !  annual
              !  NO recharge
              !  lvWB%lvf_municipalPumping_acft_(i)
              !  lvWB%lvf_fluxGWaterSRP(i)
             ! =========================================
                !
                lvWB%lvf_SRPclassBC_acft_month(i,vTstep)= lvWB%lvf_fluxGWaterSRP(i)
                !

             ! 07.16.13 das
             lvWB%lvf_fluxGWaterToWSupply(i,vTstep)= lvWB%lvf_municipalPumping_acft_(i)-lvWB%lvf_fluxGWaterTreatmentToWSupply(i,vTstep)

             lvf_GWaterOutflow(i)=0
             ! ------------------------
                !
                lvWB%lvf_municipalRecharge_acft_(i)=lvWB%lvf_fluxDirectInjectToGW(i,vTstep) 
                !
                !
                lv_add(i,vTstep)= lvWB%lv_fluxVadoseToAquifer(i,vTstep) &
                  +  lvWB%lvf_municipalRecharge_acft_(i) !+ gvf_incidentalCredit(T%year,i)              
                !
                lv_sbt(i,vTstep)=  lvWB%lvf_fluxGWaterToWSupply(i,vTstep) &
                  + lvWB%lvf_fluxGWaterTreatmentToWSupply(i,vTstep) &
                  + lvWB%lv_fluxGWtoAgIrrigation(i,vTstep)+lvf_GWaterOutflow(i)           
                !
                 lv_AnnualWB(i)=0
                lv_AnnualWB(i)=lv_add(i,vTstep)-lv_sbt(i,vTstep)
                !
                vState_GWmodel_maf(T%year)=max(0,vState_GWmodel_maf(T%year)+ lv_AnnualWB(i)*gpd_acftTomaf)
                !
                    if(T%atStartOfProviderLoop)then
                        lvWB%lvf_GWRecharged(T%year)=0
                        lvWB%lvf_GWPumped(T%year)=0
                        !
                         temp_1=0;temp_2=0;temp_3=0;temp_4=0
                        out=0;in=0
                        tvf_addIn_1=0; tvf_addOut_1=0;tvf_addIn_2=0;tvf_addOut_2=0; tvf_addOut_3=0
                        !
                    endif
                !
                 lvWB%lvf_GWRecharged(T%year)=lvWB%lvf_GWRecharged(T%year)+gvf_annualGWStored_acft(i)
                lvWB%lvf_GWPumped(T%year)=lvWB%lvf_GWPumped(T%year)+lvWB%lvf_municipalPumping_acft_(i)+ lvWB%lvf_SRPclassBC_acft_month(i,vTstep)
                !
             ! ---------------------------
             !
             call sUpdateState(T,i,vTstep,vState_WB_GW)
             call sUpdateState(T,i,vTstep,vState_WB_Aquifer)
             !
             !  Send an output to the interface- 08.20.10 das    
                ! 05.01.12 changed to a credit model
                go_ProviderGroundWater_a(i)=0
               go_ProviderGroundWater_a(i)=nint(gvd_CreditModel_acft(T%year,i))
             !
             ! Every flux to the vadose EXCEPT muni recharge
             go_fluxVadoseToAquifer(i)=nint(lvWB%lv_fluxVadoseToAquifer(i,vTstep))
             !
            ! 08.07.12 DAS
            ! Our regional estimate of groundwater
            ! ----------------------------------------------------------------------------------------
            !
            lvf_regionalInputs=0
            lvf_regionalOutflows=0
            !
            !
            if(gvl_start)then
              if(lvl_firstLoop)then             
                 gvf_pumpingAg=0
                 gvf_regionalGWrecharged=0
                 gof_regionalGW=0
                ! initialvolume from a memo dated 29 September, 2008 from Adam Freihoefer to me (ADWR)
                ! I used the 1998 estimate and subtracted Maricopa-Stansfield 
                ! 68,326,025 - 6,407,711 AF =61.918 maf
                gof_regionalGW=lpf_initialVolume
              endif
            endif
                    !
                    if(lvl_firstLoop)then
                     gvf_regionalGWrecharged=0
                    endif
                    !
                    gvf_regionalGWrecharged=go_fluxVadoseToAquifer(i) + lvWB%lvf_municipalRecharge_acft_(i)
                    !
                 lvf_cagrd_acft=0
                 lvf_otherPumping_acft=0
                !
                ! Added lvWB%lvf_SRPclassBC_acft_month(i,vTstep) on 11.24.14 - NOT SURE WHY THIS WAS Missing
                lvf_regionalInputs=gvf_regionalGWrecharged
                lvf_regionalOutflows=lvf_GWaterOutflow(i)+lvWB%lvf_municipalPumping_acft_(i)+lvWB%lvf_SRPclassBC_acft_month(i,vTstep)
                !
                 tvf_addIn_1=0
                 tvf_addOut_1=0
                 tvf_addIn_2=0
                 tvf_addOut_2=0
                 tvf_addOut_3=0
                !
                if(i < 22 .or.  23 < i)then
                 tvf_addIn_1=tvf_addIn_1+go_fluxVadoseToAquifer(i)
                 tvf_addOut_1=tvf_addOut_1+lvWB%lvf_municipalPumping_acft_(i)
                 tvf_addIn_2=tvf_addIn_2+lvWB%lvf_municipalRecharge_acft_(i)
                 tvf_addOut_2=tvf_addOut_2+lvWB%lvf_SRPclassBC_acft_month(i,vTstep)
                 tvf_addOut_3=tvf_addOut_3+lvf_GWaterOutflow(i)
                endif
                !
                if(lvl_firstLoop)then
                    !
                     gof_naturalRecharge=0
                     gof_AgAndOtherPumped=0

                     gof_Inflow=0
                     gof_Outflow=0
                     lvf_outFlowTotal_acft=0
                     go_CAGRD_acft=0
                    !
                    ! Set Once- i.e., annual estimate across all providers
                    ! Changed on 01.22.17 (added the if statement and go_pumpingAgTotal_AF(T%year-1))
                    ! From Agriculture.f90 /AgricultureProduction/AgricultureBudgets
                    ! -------------------------------------------------------------------
                    if(T%year < 2001 .and. i == 1)then
                      gvf_pumpingAg=376995.000
                    else
                      gvf_pumpingAg=go_pumpingAgTotal_AF(T%year-1)
                    endif
                    !
                     lvf_otherPumping_acft=gvf_pumpingAg+lpf_otherpumping_acft
                     lvf_outFlowTotal_acft=lpf_stanfieldOutflow_acft+lpf_gilaOutflow_acft
                    !
                    ! CAGRD estimates of recharge
                    if(0 < gii_CAGRD_acft)then
                     lvf_cagrd_acft=gii_CAGRD_acft
                    else
                     lvf_cagrd_acft=li_cagrdRecharge(T%year)*1/gpd_acftTomaf  
                    endif
                    !
                    ! Send to Interface - Most of the following 
                    ! -------------------------------------------------------------------------
                    !
                    gof_regionalGW=gof_regionalGW &
                        - (lvf_outFlowTotal_acft+lvf_otherPumping_acft+lvf_regionalOutflows) &
                        + gvf_naturalRecharge_acft(T%year)+lvf_cagrd_acft+lpf_allInflows_acft+ lvf_regionalInputs
                    !
                    gof_Outflow=lvf_outFlowTotal_acft
                    !
                    gof_Inflow=lpf_allInflows_acft
                    !
                    gof_naturalRecharge= gvf_naturalRecharge_acft(T%year)
                    gof_AgAndOtherPumped=lvf_otherPumping_acft
                    !
                    go_CAGRD_acft=lvf_cagrd_acft
                    !
                    out=lvf_outFlowTotal_acft+lvf_otherPumping_acft &
                      + lvf_regionalOutflows
                    in=gvf_naturalRecharge_acft(T%year)+lvf_cagrd_acft+lpf_allInflows_acft &
                      + lvf_regionalInputs
                    !
                 else
                    !
                    gof_regionalGW=gof_regionalGW + lvf_regionalInputs -lvf_regionalOutflows
                    !
                    gof_Outflow=gof_Outflow + lvf_GWaterOutflow(i)
                    !
                    out=out+lvf_regionalOutflows
                    in=in+lvf_regionalInputs
                    !
                endif
                !
300 format (I4,1x,6(F15.2,1x)) 

          ! -----------------------------------------------------------------------------------------
          !
13        format(i4,1x,i2,1x,i2,1x,F5.3,1x,4(F8.1,1x),2x,F12.1)
          !
         return
        end subroutine sAquifer
        ! ---------------------

       ! ---------------------------------------
        subroutine sCommercial(T,lvWB,i,vTstep)
            !
            !---------------- Types ---------------------------------
            integer :: i
            integer :: vTstep
            integer, parameter :: lvi_BWuse=2

            real :: lv_fluxWaterUseTotal(gvi_maxProV,gpi_timeStepWB)
            real :: lv_providerGPCD(gvi_maxProV,2)
            real :: lv_fluxIndoorUse(gvi_maxProV,gpi_timeStepWB)
            real :: lv_fluxOutdoorUse(gvi_maxProV,gpi_timeStepWB)
            real :: lv_fluxToAtmos(gvi_maxProV,gpi_timeStepWB)

            !real :: vGWtypical=0.535  ! 0.596 was 0.69 Avg prop of indoor water use contrib to grey water -Mayer and DeOreo (1999)
            !    
            real :: lvf_indoorDemand(gvi_maxProV,gpi_timeStepWB)
            real :: lvf_outdoorDemand(gvi_maxProV,gpi_timeStepWB)

            real ::lvf_nonpotable
            real :: lvf_indoor,lvf_indoor_deficit
            real :: lvf_outdoor
            !
            real :: lvf_nonPotable_useOutdoor,lvf_nonPotable_useIndoor
            !
            real :: lvf_leaksIndoor,lvf_leaksOutdoor
            real :: lvf_flushesPerDay
            ! ========================================================
            !

            ! -- Type Constructs --
            type (waterbalance)lvWB
            type(runTime)T
            ! =====================
             !
             mvc_use='COM'
             !
              call sMask(gvl_mask)
              Use=1
             !
             ! -----------------------------------------------------------------------------------------------
             ! See Residential for explanation of this variable
             ! -------------------
              lvWB%lvi_ResComInd=2
              !
                if(mvl_waterSim6)then
                    !
                  mvf_indoorDemandPotable(i,vTstep)=0
                  mvf_outdoorDemandPotable(i,vTstep)=0
                  mvf_indoorDemandNonPotable(i,vTstep)=0
                  mvf_outdoorDemandNonPotable(i,vTstep)=0
                  mvf_unusedNonPotable(i,vTstep)=0
                  !
                  mvf_indoorDemandPotable(i,vTstep)=lvWB%lvf_indoorPotable(i)*gvf_parm_WStoCom_prop(i)
                  mvf_indoorDemandNonPotable(i,vTstep)=lvWB%lvf_indoorNonPotable(i)*gvf_parm_WStoCom_prop(i)
                  mvf_outdoorDemandPotable(i,vTstep)=lvWB%lvf_outdoorPotable(i)*gvf_parm_WStoCom_prop(i)
                  mvf_outdoorDemandNonPotable(i,vTstep)=lvWB%lvf_outdoorNonPotable(i)*gvf_parm_WStoCom_prop(i)
                  mvf_unusedNonPotable(i,vTstep)=lvWB%lvf_excessNonPotable(i)*gvf_parm_WStoCom_prop(i)
                  !
                    lvf_nonPotable_useIndoor=0
                 !
                    lvf_leaksIndoor=0
                    lvf_leaksOutdoor=0
                   lvf_leaksIndoor = mvf_indoorDemandPotable(i,vTstep)* gvf_RateComLeak(i)
                   lvf_leaksOutdoor=  (mvf_outdoorDemandPotable(i,vTstep)+ mvf_outdoorDemandNonPotable(i,vTstep))* gvf_RateComLeak(i)

                  !
                else
                     ! NOTES
                     ! -----------------------------------------
                     ! lvWB%lv_fluxFromWS_SRPandCAP(i,vTstep)        - net water total
                     ! lvWB%lv_fluxFromWS_Com_ROpotable(i,vTstep)    - ROreclaimed water
                     ! lvWB%lv_fluxFromWS_Com_nonpotable(i,vTstep)   - reclaimed water
                     ! lvWB%lv_fluxFromWS_Com_Groundwater(i,vTstep)  - Groundwater
                     ! lvWB%lv_fluxFromWS_Com_Potable(i,vTstep)      - all potable
                     ! lvWB%lvf_demand_Com_outdoor_acft_(i,vTstep)
                     ! lvWB%lvf_demand_Com_indoor_acft_(i,vTstep)
                     !
                    lvf_indoorDemand(i,vTstep)=0
                  lvf_indoorDemand(i,vTstep)=lvWB%lvf_demand_Com_indoor_acft_(i,vTstep)
                  !
                    lvf_outdoorDemand(i,vTstep)=0
                  lvf_outdoorDemand(i,vTstep)=lvWB%lvf_demand_Com_outdoor_acft_(i,vTstep)
                  !
                    lvf_nonpotable=0
                  lvf_nonpotable=lvWB%lv_fluxFromWS_Com_nonpotable(i,vTstep) 
                  !
                   lvf_leaksIndoor=0
                   lvf_leaksOutdoor=0
                  lvf_leaksIndoor= lvf_indoorDemand(i,vTstep)* gvf_RateComLeak(i)
                  lvf_leaksOutdoor= lvf_outdoorDemand(i,vTstep)* gvf_RateComLeak(i)
                  !
                  ! ------------------------------------------------------------------------
                  !

                  ! ========================================================================
                   !
                    lvf_nonPotable_useOutdoor=0
                   lvf_nonPotable_useOutdoor=lvf_nonpotable*gvf_parm_ReclaimedOutdoor(i)

                    lvf_nonPotable_useIndoor=0
                   lvf_nonPotable_useIndoor=max(0.,lvf_nonpotable-lvf_nonPotable_useOutdoor)
                   !
                endif
                    !
                    lvf_BWaterNotUsed=0
                    lvf_BWaterUsed=0
                    lvf_indoor_deficit=0
                    !
                   if(mvl_waterSim6)then
                     lvf_indoorDemand(i,vTstep)=mvf_indoorDemandPotable(i,vTstep)
                     lvf_nonPotable_useIndoor=mvf_indoorDemandNonPotable(i,vTstep)
                     !
                     lvf_outdoorDemand(i,vTstep)= mvf_outdoorDemandPotable(i,vTstep)+ mvf_outdoorDemandNonPotable(i,vTstep)
                    endif
                    !
              ! ========================================================================================
               !
               call sBlackWater(lvWB,i,lvi_BWuse,lvf_indoorDemand(i,vTstep),lvf_nonPotable_useIndoor, &
                  lvf_BWaterUsed,lvf_BWaterNotUsed,lvf_indoor_deficit,lvf_flushesPerDay)
               !
              ! ========================================================================================
              !
              ! Partition the leaked water indoor and outdoor
              ! ====================================================================
               lvf_indoor=0
              lvf_indoor=lvf_indoorDemand(i,vTstep) 
              !
              ! ====================================================================
              !
              if(gpi_timeStepWB /=1)then
              else           
                 lvf_outdoor=0
                lvf_outdoor=lvf_outdoorDemand(i,vTstep)
              endif
              ! 
              ! Outdoor use
              ! 05.05.14 removed leaks from the use
              lv_fluxOutdoorUse(i,vTstep)=lvf_outdoor -lvf_leaksOutdoor

              lv_fluxIndoorUse(i,vTstep)=lvf_indoor + lvf_nonPotable_useIndoor- lvf_leaksIndoor
              lv_fluxWaterUseTotal(i,vTstep)=lvf_indoor+lvf_outdoor
              !
                ! 04.20.16 das new data table structure/implementation
                ! ----------
                if(mvl_waterSim6)then
                  lvWB%lvf_demand_Com_indoor_acft_(i,vTstep)=lv_fluxIndoorUse(i,vTstep)
                  lvWB%lvf_demand_Com_outdoor_acft_(i,vTstep)=lv_fluxOutdoorUse(i,vTstep)
                endif
                ! -----------------------------------------------------------------------

              ! -----------------------------------------------------------------
             !      
             lvWB%lv_fluxCom(i,vTstep)= nint(lv_fluxWaterUseTotal(i,vTstep))
             lvWB%lv_fluxComIndoor(i,vTstep)= nint(lv_fluxIndoorUse(i,vTstep))
             !
             !
                 mvf_lost=0
                 mvf_unusedGrayWater=0
              if(gvl_parm_grayWater)then
               call sGrayWater(T,lvWB,i,vTstep,lvf_indoor,lv_fluxWaterUseTotal(i,vTstep), &
                lvf_BWaterUsed,lv_fluxReclaimedComGreywater,lv_providerGPCD,mvf_lost,mvf_unusedGrayWater)
                 !
                 lvWB%lv_fluxReclaimedGreywaterCom(i,vTstep)=0
                 lvWB%lvf_GPCD_com(i,1)=0
                 lvWB%lvf_GPCD_com(i,2)=0
                 !
                 if( T%year < gvi_baseYear)then
                  mvf_lost=0
                  mvf_unusedGrayWater=0
                  lvWB%lv_fluxReclaimedGreywaterCom(i,vTstep)=0
                 else
                  !
                  lvWB%lv_fluxReclaimedGreywaterCom(i,vTstep)=lv_fluxReclaimedComGreywater(T%year,i,vTstep)
                  !
                  ! GPCD without graywater savings (i,1) and with graywater savings (i,2)
                  lvWB%lvf_GPCD_com(i,1)=lv_providerGPCD(i,1)
                  lvWB%lvf_GPCD_com(i,2)=lv_providerGPCD(i,2)
                  !
                  lvWB%lvf_addToRunoffGrayWater(i)=lvWB%lvf_addToRunoffGrayWater(i)+mvf_unusedGrayWater
                endif
              else
               lv_fluxReclaimedComGreywater(T%year,i,vTstep)=0
              endif
              ! ---------------------------------------------
             !
               lvWB%lv_fluxComOutdoor(i,vTstep)=nint(lv_fluxOutdoorUse(i,vTstep))
      
             ! Evaporative flux of outdoor water
              ! --------------------------------
               call sWaterFluxToAtmos(lvWB,i,vTstep,lvWB%lv_fluxComOutdoor(i,vTstep), lv_fluxToAtmos(i,vTstep))
                lv_fluxToAtmos(i,vTstep)=nint(lv_fluxToAtmos(i,vTstep))
                   gvf_comEvapotrans(i)=0
                  gvf_comEvapotrans(i)=lv_fluxToAtmos(i,vTstep)+mvf_lost
              ! ------------------------------------------------

             ! Outdoor water that moves into the vadose
             lvWB%lv_fluxCioVadose(i,vTstep)=nint(max(0.,(lvWB%lv_fluxComOutdoor(i,vTstep)-lv_fluxToAtmos(i,vTstep) +lvf_leaksOutdoor )))
             !
             ! -------- STATES ----------
             vState_WB_Commercial(T%year,i,vTstep)=vState_WB_Commercial(T%year,i,vTstep) &
              +(lv_fluxWaterUseTotal(i,vTstep)+ lvWB%lv_fluxReclaimedGreywaterCom(i,vTstep)) &
              -(lv_fluxIndoorUse(i,vTstep) + lv_fluxOutdoorUse(i,vTstep))
             !
             lvWB%lv_fluxComWW(i,vTstep)= lvWB%lv_fluxComWW(i,vTstep)+ (lv_fluxIndoorUse(i,vTstep)  &
               -lvWB%lv_fluxReclaimedGreywaterCom(i,vTstep)  ) * gpf_WWsourceEfficiency
             !
            ! =========================== 
             vState_WB_CommercialIndoor(T%year,i,vTstep)= vState_WB_CommercialIndoor(T%year,i,vTstep) &
                +( lv_fluxIndoorUse(i,vTstep)+ lvWB%lv_fluxReclaimedGreywaterCom(i,vTstep)) &
                - lvWB%lv_fluxComWW(i,vTstep)
 
            vState_WB_CommercialOutdoor(T%year,i,vTstep)=vState_WB_CommercialOutdoor(T%year,i,vTstep) &
                + lv_fluxOutdoorUse(i,vTstep) &
                - (lvWB%lv_fluxCioVadose(i,vTstep) +  lv_fluxToAtmos(i,vTstep))
            !
            ! ---------------------
            ! Update states
            ! -----------------
             !
             call sUpdateState(T,i,vTstep, vState_WB_CommercialIndoor)
             call sUpdateState(T,i,vTstep, vState_WB_CommercialOutdoor)
             call sUpdateState(T,i,vTstep, vState_WB_Commercial)
            !
         return
        end subroutine sCommercial
        ! --------------------------

           ! --------------------------------------------------
            subroutine sShowerMinutes(lvWB,i,water,days,minPCD)
                !
                ! ---------------- Types -------------------
                integer :: i
                integer :: days

                real :: water,minPCD
             !   real, parameter :: lpf_gallonsPerMinute=2.3
                ! ==========================================
                !

                ! -------- Type Construct --
                type (waterbalance)lvWB
                ! ==========================
                    !
                    ! minutes per capita per day (minPCD)
                     minPCD=0
!                    minPCD=water * gpd_galperacft * 1./ ( days * lpf_gallonsPerMinute *lvWB%lv_providerPop(i) )
                    minPCD=water * gpd_galperacft * 1./ ( days * gpf_gallonsPerMinute *lvWB%lv_providerPop(i) )

                !
             return
            end subroutine sShowerMinutes
           !------------------------------

        ! -------------------------------------
        subroutine sIndustrial(T,lvWB,i,vTstep)
            !
            ! ------------------------Types ----------------------------
            integer :: i
            integer :: vTstep
            integer, parameter :: lvi_BWuse=3

            real :: lv_fluxWaterUseTotal(gvi_maxProV,gpi_timeStepWB)
            real :: lv_providerGPCD(gvi_maxProV,2)
            real :: lv_fluxIndoorUse(gvi_maxProV,gpi_timeStepWB)
            real :: lv_fluxOutdoorUse(gvi_maxProV,gpi_timeStepWB)
            real :: lv_fluxToAtmos(gvi_maxProV,gpi_timeStepWB)

            !real :: vGWtypical=0.535 ! 0.596 was 0.69 Avg prop of indoor water use contrib to grey water -Mayer and DeOreo (1999)
            real :: lvf_indoorDemand(gvi_maxProV,gpi_timeStepWB)
            real :: lvf_outdoorDemand(gvi_maxProV,gpi_timeStepWB)
            real :: lvf_nonpotable
            real :: lvf_indoor,lvf_indoor_deficit
            real :: lvf_outdoor

            real :: lvf_nonPotable_useOutdoor,lvf_nonPotable_useIndoor
            !
            real :: lvf_vadose
            real :: lvf_leaksIndoor,lvf_leaksOutdoor
            real :: lvf_flushesPerDay
            ! =========================================================
            !         

            ! -- Type Construct ---
            type (waterbalance)lvWB
            type(runTime)T
            ! =====================
             !
             mvc_use='IND'
             !
              call sMask(gvl_mask)
              Use=1
              !
              ! -----------------------------------------------------------------------------------
              ! See Residential for explanaton of this variable
              ! ------------------
              lvWB%lvi_ResComInd=3
                !
                if(mvl_waterSim6)then
                !
                  mvf_indoorDemandPotable(i,vTstep)=0
                  mvf_outdoorDemandPotable(i,vTstep)=0
                  mvf_indoorDemandNonPotable(i,vTstep)=0
                  mvf_outdoorDemandNonPotable(i,vTstep)=0
                  mvf_unusedNonPotable(i,vTstep)=0
                  !
                  mvf_indoorDemandPotable(i,vTstep)=lvWB%lvf_indoorPotable(i)      *gvf_parm_WStoInd_prop(i)
                  mvf_indoorDemandNonPotable(i,vTstep)=lvWB%lvf_indoorNonPotable(i)*gvf_parm_WStoInd_prop(i)
                  mvf_outdoorDemandPotable(i,vTstep)=lvWB%lvf_outdoorPotable(i)    *gvf_parm_WStoInd_prop(i)
                  mvf_outdoorDemandNonPotable(i,vTstep)=lvWB%lvf_outdoorNonPotable(i)*gvf_parm_WStoInd_prop(i)
                  mvf_unusedNonPotable(i,vTstep)=lvWB%lvf_excessNonPotable(i)      *gvf_parm_WStoInd_prop(i)
                  !
                    lvf_nonPotable_useIndoor=0
                 !
                    lvf_leaksIndoor=0
                    lvf_leaksOutdoor=0
                   lvf_leaksIndoor = mvf_indoorDemandPotable(i,vTstep)* gvf_RateIndLeak(i)
                   lvf_leaksOutdoor=  (mvf_outdoorDemandPotable(i,vTstep)+ mvf_outdoorDemandNonPotable(i,vTstep))* gvf_RateIndLeak(i)
                  !
                else
                  ! NOTES:
                  ! -----------------------------------------
                  ! lvWB%lv_fluxFromWS_SRPandCAP(i,vTstep)        - net water total
                  ! lvWB%lv_fluxFromWS_Ind_ROpotable(i,vTstep)    - ROreclaimed water
                  ! lvWB%lv_fluxFromWS_Ind_nonpotable(i,vTstep)   - reclaimed water
                  ! lvWB%lv_fluxFromWS_Ind_Groundwater(i,vTstep)  - Groundwater
                  ! lvWB%lv_fluxFromWS_Ind_Potable(i,vTstep)      - all potable
                  ! lvWB%lvf_demand_Ind_outdoor_acft_(i,vTstep)
                  ! lvWB%lvf_demand_Ind_indoor_acft_(i,vTstep)
                  !
                    lvf_indoorDemand(i,vTstep)=0
                  lvf_indoorDemand(i,vTstep)=lvWB%lvf_demand_Ind_indoor_acft_(i,vTstep)
                  !
                    lvf_outdoorDemand(i,vTstep)=0
                  lvf_outdoorDemand(i,vTstep)=lvWB%lvf_demand_Ind_outdoor_acft_(i,vTstep)
                  !
                    lvf_nonpotable=0
                  lvf_nonpotable=lvWB%lv_fluxFromWS_Ind_nonpotable(i,vTstep) 
                  !
                   lvf_leaksIndoor=0
                   lvf_leaksOutdoor=0
                  lvf_leaksIndoor= lvf_indoorDemand(i,vTstep)* gvf_RateIndLeak(i)
                  lvf_leaksOutdoor= lvf_outdoorDemand(i,vTstep)* gvf_RateIndLeak(i)
                  ! ------------------------------------------------------------------------
                  !
                  ! =========================================================================
                   !
                    lvf_nonPotable_useOutdoor=0
                   lvf_nonPotable_useOutdoor=lvf_nonpotable*gvf_parm_ReclaimedOutdoor(i)

                    lvf_nonPotable_useIndoor=0
                   lvf_nonPotable_useIndoor=lvf_nonpotable-lvf_nonPotable_useOutdoor
                   !
                endif
                    !
                     lvf_BWaterNotUsed=0
                     lvf_BWaterUsed=0
                     lvf_indoor_deficit=0
                    !
                    if(mvl_waterSim6)then
                     lvf_indoorDemand(i,vTstep)=mvf_indoorDemandPotable(i,vTstep)
                     lvf_nonPotable_useIndoor=mvf_indoorDemandNonPotable(i,vTstep)
                     !
                     lvf_outdoorDemand(i,vTstep)= mvf_outdoorDemandPotable(i,vTstep)+ mvf_outdoorDemandNonPotable(i,vTstep)
                    endif

               call sBlackWater(lvWB,i,lvi_BWuse,lvf_indoorDemand(i,vTstep),lvf_nonPotable_useIndoor, &
                  lvf_BWaterUsed,lvf_BWaterNotUsed,lvf_indoor_deficit,lvf_flushesPerDay)
               !
              ! ====================================================================================================
              ! Partition the leaked water indoor and outdoor
               lvf_indoor=0
              lvf_indoor=anint(lvf_indoorDemand(i,vTstep)) 
              ! ================================================
                !
                if(gpi_timeStepWB /=1)then
                else           
                 lvf_outdoor=0
                   lvf_outdoor=lvf_outdoorDemand(i,vTstep)
                endif
                !
              ! Outdoor use
              lv_fluxOutdoorUse(i,vTstep)=lvf_outdoor-lvf_leaksOutdoor


              lv_fluxIndoorUse(i,vTstep)=lvf_indoor+lvf_nonPotable_useIndoor-lvf_leaksIndoor
              lv_fluxWaterUseTotal(i,vTstep)=lvf_indoor+lvf_outdoor
              !
              ! ---------------------------------------------

                ! 04.20.16 das new data table structure/implementation
                ! ----------
                if(mvl_waterSim6)then
                  lvWB%lvf_demand_Ind_indoor_acft_(i,vTstep)=lv_fluxIndoorUse(i,vTstep)
                  lvWB%lvf_demand_Ind_outdoor_acft_(i,vTstep)=lv_fluxOutdoorUse(i,vTstep)
                endif
                ! -----------------------------------------------------------------------

             !      
             lvWB%lv_fluxInd(i,vTstep)= nint(lv_fluxWaterUseTotal(i,vTstep))
             lvWB%lv_fluxIndindoor(i,vTstep)= nint(lv_fluxIndoorUse(i,vTstep))
             !
             !
              if(gvl_parm_grayWater)then
                !
                ! if gvl_modelVinzeJohnston is true, we are removing   from the 
                ! effluent stream for Indiustrial water users
                ! 04.01.13
                ! ------------------------------------------------------------------------------
                  mvf_lost=0
                  mvf_unusedGrayWater=0
                call sGrayWater(T,lvWB,i,vTstep,lvf_indoor,lv_fluxWaterUseTotal(i,vTstep), &
                lvf_BWaterUsed,lv_fluxReclaimedIndGreywater,lv_providerGPCD,mvf_lost,mvf_unusedGrayWater)
                 !
                 lvWB%lv_fluxReclaimedGreywaterInd(i,vTstep)=0
                 lvWB%lvf_GPCD_ind(i,1)=0
                 lvWB%lvf_GPCD_ind(i,2)=0
                 !
                 if( T%year < gvi_baseYear)then
                  mvf_lost=0
                  mvf_unusedGrayWater=0
                  lvWB%lv_fluxReclaimedGreywaterInd(i,vTstep)=0
                 else
                  !
                  lvWB%lv_fluxReclaimedGreywaterInd(i,vTstep)=lv_fluxReclaimedIndGreywater(T%year,i,vTstep)
                  !
                  ! GPCD without graywater savings (i,1) and with graywater savings (i,2)
                  lvWB%lvf_GPCD_ind(i,1)=lv_providerGPCD(i,1)
                  lvWB%lvf_GPCD_ind(i,2)=lv_providerGPCD(i,2)
                  !
                  lvWB%lvf_addToRunoffGrayWater(i)=lvWB%lvf_addToRunoffGrayWater(i)+mvf_unusedGrayWater
                  !
                 endif
              else
                lv_fluxReclaimedIndGreywater(T%year,i,vTstep)=0
                lvWB%lv_fluxReclaimedGreywaterInd(i,vTstep)=0
              endif
              ! ---------------------------------------------
              !
              lvWB%lv_fluxIndOutdoor(i,vTstep)=nint(lv_fluxOutdoorUse(i,vTstep))
              !
              ! Evaporative flux of outdoor water
              ! ---------------------------------
               call sWaterFluxToAtmos(lvWB,i,vTstep,lvWB%lv_fluxIndOutdoor(i,vTstep), lv_fluxToAtmos(i,vTstep))
               lv_fluxToAtmos(i,vTstep)=nint(lv_fluxToAtmos(i,vTstep))
                 gvf_indEvapotrans(i)=0
               gvf_indEvapotrans(i)=lv_fluxToAtmos(i,vTstep) +mvf_lost
              ! -------------------------------------------------------

             ! Outdoor water that moves into the vadose
             ! Added back in here because first defined in sCommercial()
             ! We do not keep tract of these two individually (i.e, only residential and commercial/industrial)
             lvf_vadose=lvWB%lv_fluxCioVadose(i,vTstep)
             lvWB%lv_fluxCioVadose(i,vTstep)= lvf_leaksOutdoor+ lvf_vadose+ nint(max(0.,(lvWB%lv_fluxIndOutdoor(i,vTstep)-lv_fluxToAtmos(i,vTstep))))
             !
             ! -------- STATES ----------
             vState_WB_Industry(T%year,i,vTstep)=vState_WB_Industry(T%year,i,vTstep) &
              +(lv_fluxWaterUseTotal(i,vTstep)+ lvWB%lv_fluxReclaimedGreywaterInd(i,vTstep)) &
              -(lv_fluxIndoorUse(i,vTstep) + lv_fluxOutdoorUse(i,vTstep))
             !
             lvWB%lv_fluxIndWW(i,vTstep)=lvWB%lv_fluxIndWW(i,vTstep)+(lv_fluxIndoorUse(i,vTstep) &
               - lvWB%lv_fluxReclaimedGreywaterInd(i,vTstep) )* gpf_WWsourceEfficiency
             !
            ! =========================== 
             vState_WB_IndustrialIndoor(T%year,i,vTstep)= vState_WB_IndustrialIndoor(T%year,i,vTstep) &
                +( lv_fluxIndoorUse(i,vTstep)+ lvWB%lv_fluxReclaimedGreywaterInd(i,vTstep)) &
                - lvWB%lv_fluxIndWW(i,vTstep)
 
            vState_WB_IndustrialOutdoor(T%year,i,vTstep)=vState_WB_IndustrialOutdoor(T%year,i,vTstep) &
                + lv_fluxOutdoorUse(i,vTstep) &
                - (lvWB%lv_fluxCioVadose(i,vTstep)-lvf_vadose +  lv_fluxToAtmos(i,vTstep))
            !
            ! ---------------------
            ! Update states
            ! -----------------
             !
             call sUpdateState(T,i,vTstep, vState_WB_IndustrialIndoor)
             call sUpdateState(T,i,vTstep, vState_WB_IndustrialOutdoor)
             call sUpdateState(T,i,vTstep, vState_WB_Industry)
            !
         return
        end subroutine sIndustrial
        ! --------------------------

        ! ------------------------------------------
        subroutine sDirectInjection(T,lvWB,i,vTstep)
            !
            ! --------types ----
            integer :: i
            integer :: vTstep
            !integer,parameter :: state=17
            ! ==================
            !

            ! -- Type Constructs --
            type (waterbalance)lvWB
            type(runTime)T
            ! =====================
             !
             lvWB%lvf_fluxWSupplyToDirectInjectTotal(i,vTstep) = &
              lvWB%lv_ROflux_DirectInject_Potable(i,vTstep) &
              + lvWB%lvf_fluxReclaimedToDirectInject(i,vTstep) &
              + lvWB%lvf_fluxWStoDirectInjection(i,vTstep)
             !
             lvWB%lvf_fluxDirectInjectToGW(i,vTstep)=  lvWB%lvf_fluxWSupplyToDirectInjectTotal(i,vTstep) 
             !
             vState_DirectInjection(T%year,i,vTstep)=vState_DirectInjection(T%year,i,vTstep) &
              +  lvWB%lv_ROflux_DirectInject_Potable(i,vTstep) &
              +  lvWB%lvf_fluxReclaimedToDirectInject(i,vTstep) &
              +  lvWB%lvf_fluxWSupplyToDirectInjectTotal(i,vTstep) &
              -  lvWB%lvf_fluxDirectInjectToGW(i,vTstep)
             !
             ! ---------------------
             ! Update states
             ! -----------------
             ! 
             call sUpdateState(T,i,vTstep,vState_DirectInjection)
             !
         return
        end subroutine sDirectInjection
        ! -----------------------------
 
        ! -----------------------------------
        subroutine sEffluent(T,lvWB,i,vTstep)
            !
            ! ---------------------- Types ---------------------------
            integer :: i
            integer :: vTstep
            integer,parameter :: state=16

            real  :: lv_fluxFromWWTP(gvi_maxProV,gpi_timeStepWB)
            real  :: lv_fluxToPowerPlant(gvi_maxProV,gpi_timeStepWB)
            real  :: lv_fluxToVadose(gvi_maxProV,gpi_timeStepWB)
            real  :: lv_fluxToAgIrrigation(gvi_maxProV,gpi_timeStepWB)
            real  :: lvf_propIwaniec
            real :: temp,divy
            !logical :: lvl_mask(gvi_maxProV)

            ! ========================================================
            !
            ! For now, assume these fractions of waste water are sent to the 91st treatment plant
            ! real  :: lv_WWfraction_21=0.89,lv_WWfraction_25=1.0,lv_WWfraction_30=1.0,lv_WWfraction_32=0.95
            !
            ! -- Type Constructs --
            type (waterbalance)lvWB
            type(parameters)P
            type(runTime)T
            ! =====================
             ! 
!             temp=0
!             divy=0
             lvf_propIwaniec=0
             !
             if(T%atStartOfProviderLoop)then
            !  call sMaskAgriculture(lvl_mask)
             endif
             !
                lv_fluxFromWWTP(i,vTstep)=0
                lv_fluxToPowerPlant(i,vTstep)=0
                lv_fluxToVadose(i,vTstep)=0
                lv_fluxToAgIrrigation(i,vTstep)=0
                !
                P%In_1=gvf_parm_EffluentToVadose(i)
                P%In_2= gvf_parm_EffluentToPP(i)
                P%In_3=0
                P%In_4=0
                 call sProportion(state,i,vTstep,parms_Effluent,initParms,P)
                 gvf_parm_EffluentToVadose(i)=P%Out_1
                 gvf_parm_EffluentToPP(i)=P%Out_2
                !
                 gvf_parm_EffluentToVadose(i)=P%Out_1
                 gvf_parm_EffluentToPP(i)=P%Out_2
              !  
              ! Flux from the advanced waste water treatment plant
              lv_fluxFromWWTP(i,vTstep)= lvWB%lv_fluxWWTPToEffluent(i,vTstep)
              !
10 format(I4,1x,I2,1x,3(F10.3,1x))
              ! These parameters will need checking in the interface
               ! to ensure that the sum <=1
                !

                ! 03.20.16 DAS
                ! Scenario Project- transfer effluent going to PVNGS to other uses
                 lv_fluxToAgIrrigation(i,vTstep)=0
                !
                call sMaskAgriculture(gvl_mask)
                !
                call sRampDnParameters(T%policyYear,gdf_parm_EFFtoVadose(i),P%Out_1,P%Out_1)
                !
                ! Hardwired at this point
                    lv_fluxToPowerPlant(i,vTstep)=0
                    lv_fluxToVadose(i,vTstep)=0
                    lv_fluxToAgIrrigation(i,vTstep)=0
                !
                ! Do I intend to use default values here?
                ! 06.21.16
                lvf_propIwaniec=P%Out_2
                !
                if(gvl_IwaniecScenarios_PPtoAg)then
                 lvf_propIwaniec=P%Out_2 * T%propYears_2060

                    lv_fluxToPowerPlant(i,vTstep)=lv_fluxFromWWTP(i,vTstep) * lvf_propIwaniec
                    !
                    if(gvl_IwaniecScenarios_PPtoCities)then
                     lv_fluxToVadose(i,vTstep)= P%Out_1 * lv_fluxFromWWTP(i,vTstep)
                    else
                      lv_fluxToVadose(i,vTstep)=P%Out_1 * lv_fluxFromWWTP(i,vTstep)
                    endif
                    !
                     lv_fluxToAgIrrigation(i,vTstep)=0
                    if(gvl_mask(i))then
                      lv_fluxToAgIrrigation(i,vTstep)=lv_fluxFromWWTP(i,vTstep)&
                      -(lv_fluxToPowerPlant(i,vTstep)+lv_fluxToVadose(i,vTstep))
                    endif
                    !
                else
                  ! Adaptive Drought, Heat, and Flood - will have to account for
                  ! changes in Ag and how that effects these parameters
                   lv_fluxToPowerPlant(i,vTstep)=P%Out_2 * lv_fluxFromWWTP(i,vTstep)
                   ! 
                   lv_fluxToVadose(i,vTstep)=P%Out_1 * lv_fluxFromWWTP(i,vTstep)
                   !
                   !
                     lv_fluxToAgIrrigation(i,vTstep)=0
                    if(gvl_mask(i))then
                      lv_fluxToAgIrrigation(i,vTstep)=lv_fluxFromWWTP(i,vTstep)&
                         -(lv_fluxToPowerPlant(i,vTstep)+lv_fluxToVadose(i,vTstep))
                    endif
                   !
                endif
                ! CHECK THIS CODE 11.21.16
                 temp=lv_fluxToPowerPlant(i,vTstep)+lv_fluxToVadose(i,vTstep)+lv_fluxToAgIrrigation(i,vTstep)

                if(temp < lv_fluxFromWWTP(i,vTstep))then
                   divy=lv_fluxToPowerPlant(i,vTstep)/lv_fluxFromWWTP(i,vTstep)
                   !
                  lvWB%lvf_fluxEffluentToPowerPlant(i,vTstep)= nint(lv_fluxToPowerPlant(i,vTstep)) &
                    + divy * (lv_fluxFromWWTP(i,vTstep)-temp)
                  lvWB%lv_fluxToVadoseFromEffluent(i,vTstep)= nint(lv_fluxToVadose(i,vTstep)) &
                    + (1-divy) * (lv_fluxFromWWTP(i,vTstep)-temp)

                  lvWB%lvf_fluxEffluentToAg(i,vTstep)= nint(lv_fluxToAgIrrigation(i,vTstep))
                else
                 !
                 lvWB%lvf_fluxEffluentToPowerPlant(i,vTstep)= nint(lv_fluxToPowerPlant(i,vTstep))
                 lvWB%lv_fluxToVadoseFromEffluent(i,vTstep)= nint(lv_fluxToVadose(i,vTstep))
                 lvWB%lvf_fluxEffluentToAg(i,vTstep)= nint(lv_fluxToAgIrrigation(i,vTstep))
                 !
                endif
                 !
            ! =================== 
            vState_WB_Effluent(T%year,i,vTstep)=vState_WB_Effluent(T%year,i,vTstep) &
            + lv_fluxFromWWTP(i,vTstep) &
            - (lvWB%lv_fluxToVadoseFromEffluent(i,vTstep) &
            + lv_fluxToPowerPlant(i,vTstep) +  lv_fluxToAgIrrigation(i,vTstep))
            !
            lvWB%lv_fluxWWtoPaleoVerde(i,vTstep)=lv_fluxToPowerPlant(i,vTstep)
            !
            ! ---------------------
            ! Update states
            ! -----------------
            ! 
            call sUpdateState(T,i,vTstep,vState_WB_Effluent)
            !
         return
        end subroutine sEffluent
        ! ----------------------
 
        ! --------------------------------------
        subroutine sGWtreatment(T,lvWB,i,vTstep)
            !
            ! ------------- Types ----------------
            integer :: i
            integer :: vTstep

            logical :: gvl_GWtreatmentPlant=.true.
            ! ====================================
            !

            ! --- Type Constructs ---
            type (waterbalance)lvWB
            type(runTime)T
            !========================
                !
                if(gvl_GWtreatmentPlant)then
                  lvWB%lvf_fluxGWaterToGWaterTreatment(i,vTstep)=gvf_parm_GWtoGWTP(i)*lvWB%lvf_municipalPumping_acft_(i) 
                 !
                  ! Some loss or increase or effect takes place here
                  lvWB%lvf_fluxGWaterTreatmentToWSupply(i,vTstep)=lvWB%lvf_fluxGWaterToGWaterTreatment(i,vTstep)
                else
                  lvWB%lvf_fluxGWaterTreatmentToWSupply(i,vTstep)=0
                endif
                 !
                  vState_WB_GWTP(T%year,i,vTstep)= vState_WB_GWTP(T%year,i,vTstep) &
                    +  lvWB%lvf_fluxGWaterToGWaterTreatment(i,vTstep) &
                    - lvWB%lvf_fluxGWaterTreatmentToWSupply(i,vTstep)
                ! ---------------------
                ! Update states
                ! -----------------
                !
                call sUpdateState(T,i,vTstep,vState_WB_GWTP)
                !
           return
         end subroutine sGWtreatment
        ! --------------------------

        ! ------------------------------------------
        subroutine sIndustrialReuse(T,lvWB,i,vTstep)
            !
            ! ---------------------- Types -------------------
            integer :: i,j,k
            integer :: vTstep

            real  :: lv_fluxToAtmos(gvi_maxProV,gpi_timeStepWB)          
            real :: vK9(gvi_maxProV)=0.7014007467304
            ! =================================================

            ! -- Type Constructs --
            type (waterbalance)lvWB
            type(runTime)T
            ! =====================
            !
                !
                do j = 1,gvi_Providers,1
                  do k = 1,gpi_timeStepWB,1
                    lv_fluxToAtmos(j,k)=0
                  end do
                end do
                !
                ! Evaporative flux to the atmosphere
                ! Add evaporation to the current estimate from Residential, Commercial, and Industrial
                ! ----------------------------------
                lv_fluxToAtmos(i,vTstep)=(vK9(i)*lvWB%lv_fluxWWToIRuse(i,vTstep))
                if(0 < lv_fluxToAtmos(i,vTstep))then
                 lvWB%lv_fluxAtmosTotal(i,vTstep)= lvWB%lv_fluxAtmosTotal(i,vTstep)+ lv_fluxToAtmos(i,vTstep)
                endif

                 ! To waste water treatment plant
                 lvWB%lv_fluxToWWTPFromIRuse(i,vTstep)=lvWB%lv_fluxWWToIRuse(i,vTstep)-lv_fluxToAtmos(i,vTstep)
                 !
                 ! =====================
                 vState_WB_IReuse(T%year,i,vTstep)=vState_WB_IReuse(T%year,i,vTstep) & 
                   +lvWB%lv_fluxWWToIRuse(i,vTstep)-(lv_fluxToAtmos(i,vTstep) &
                   +lvWB%lv_fluxToWWTPFromIRuse(i,vTstep))
                 !
                ! ---------------------
                ! Update states
                ! -----------------
                ! 
                 call sUpdateState(T,i,vTstep,vState_WB_IReuse)
                !
         return
        end subroutine sIndustrialReuse
        ! -----------------------------

        ! ------------------------------------
        subroutine sReclaimed(T,lvWB,i,vTstep)
            !
            ! ------------------- Types -----------------------
            integer :: i
            integer :: vTstep    
            integer,parameter :: state=15

            real :: lvf_ReclaimedIn(gvi_maxProV,gpi_timeStepWB)
            ! =================================================
            !
             
            ! --- Type Constructs ---
            type (waterbalance)lvWB    
            type(parameters)P
            type(runTime)T
            ! =======================
              !
              call sMask(gvl_mask)              
              if(T%atStartOfProviderLoop)then
               gvf_RegionalWWSurDischarge=0
              endif
              !
              lvf_ReclaimedIn(i,vTstep)=lvWB%lv_fluxRWWTPToReclaimed(i,vTstep) * gpf_RecEfficiency
              !
              ! -----------------------------
              !
                P%In_1=gvf_parm_RtoVadose(i)
                P%In_2=gvf_parm_RtoDInjection(i)
                P%In_3=gvf_parm_RtoOutput(i)
                P%In_4=0
              !
                if((P%In_1+P%In_2+P%In_3) <=1)then
                 P%Out_1=gvf_parm_RtoVadose(i)
                 P%Out_2=gvf_parm_RtoDInjection(i)
                 P%Out_3=gvf_parm_RtoOutput(i)
                else
                 call sProportion(state,i,vTstep,parms_Reclaimed,initParms,P)
                 !
                 gvf_parm_RtoVadose(i)=P%Out_1
                 gvf_parm_RtoDInjection(i)=P%Out_2
                 gvf_parm_RtoOutput(i)=P%Out_3
                endif
                !
               lvWB%lvf_fluxReclaimedTotal(i,vTstep)        =lvf_ReclaimedIn(i,vTstep)
               lvWB%lvf_fluxReclaimedToVadose(i,vTstep)       =P%Out_1*lvf_ReclaimedIn(i,vTstep)
               lvWB%lvf_fluxReclaimedToDirectInject(i,vTstep) =P%Out_2*lvf_ReclaimedIn(i,vTstep)
               lvWB%lvf_fluxReclaimedToOutput(i,vTstep)       =P%Out_3*lvf_ReclaimedIn(i,vTstep)
              !
              !
              lvWB%lvf_fluxReclaimedToRWWSDischarge(i,vTstep)=lvf_ReclaimedIn(i,vTstep) &
                -  lvWB%lvf_fluxReclaimedToVadose(i,vTstep) &
                -  lvWB%lvf_fluxReclaimedToDirectInject(i,vTstep) &
                -  lvWB%lvf_fluxReclaimedToOutput(i,vTstep) &
                +  lvWB%lvf_fluxReclaimedDifference(i,vTstep) 
              !
              ! lvf_fluxReclaimedDifference added on 09.02.11- was missing from balance: calculated in Water supply
              ! Sent to Water supply
              ! ----------------------
              vState_ReclaimedWater(T%year,i,vTstep)= vState_ReclaimedWater(T%year,i,vTstep) &
                +  lvf_ReclaimedIn(i,vTstep) - ( lvWB%lvf_fluxReclaimedToVadose(i,vTstep)&
                +  lvWB%lvf_fluxReclaimedToDirectInject(i,vTstep)+ lvWB%lvf_fluxReclaimedToOutput(i,vTstep) &
                +  lvWB%lvf_fluxReclaimedToRWWSDischarge(i,vTstep))
              !
              gvf_RegionalWWSurDischarge=gvf_RegionalWWSurDischarge &
                + lvWB%lvf_fluxReclaimedToRWWSDischarge(i,vTstep) &
                + lvWB%lv_fluxWWToSurfaceDischarge(i,vTstep)              
              !
              gvf_reclaimedOutput(T%year,i,vTstep)= lvWB%lvf_fluxReclaimedToOutput(i,vTstep)
              !

              ! ---------------------
              ! Update states
              ! -----------------
              ! 
              call sUpdateState(T,i,vTstep,vState_ReclaimedWater)
              call sStepUpdate(T,i,vTstep,state)
              !
         return
        end subroutine sReclaimed
        ! -----------------------

        ! --------------------------------------
        subroutine sResidential(T,lvWB,i,vTstep)
            !
            ! ----------------------- Types ------------------------
            integer :: i
            integer :: vTstep
            integer,parameter :: lvi_BWuse=1

            real :: lv_fluxWaterUseTotal(gvi_maxProV,gpi_timeStepWB)
            real :: lv_providerGPCD(gvi_maxProV,2)
            real :: lv_fluxIndoorUse(gvi_maxProV,gpi_timeStepWB)
            real :: lv_fluxOutdoorUse(gvi_maxProV,gpi_timeStepWB)
            real :: lv_fluxToAtmos(gvi_maxProV,gpi_timeStepWB)
            ! Reclaimed Greywater Res Rate =f(indoor avail-i.e. not toilet and leakage)
            !real :: vGWtypical=0.535 ! 0.596  was 0.69 Avg prop of indoor water use contrib to grey water -Mayer and DeOreo (1999)
            !
            real :: lvf_indoorDemand(gvi_maxProV,gpi_timeStepWB)
            real :: lvf_outdoorDemand(gvi_maxProV,gpi_timeStepWB)
            real :: lvf_indoor_deficit 
            real :: lvf_nonpotable
            !
            real :: lvf_indoor,lvf_outdoor    
            real :: lvf_indoorUsable
            real :: lvf_nonPotable_useOutdoor,lvf_nonPotable_useIndoor
            real :: lvf_leaksIndoor,lvf_leaksOutdoor
            real :: lvf_flushesPerDay
            real :: water,minPCD
            !
!            real,parameter :: lpf_effectiveHouses=0.5
!            real :: lvf_actualHouses
!            REAL :: fLogistic
            ! ========================================================
            !

            ! --- Type Constructs ---
            type (waterbalance)lvWB
            type(runTime)T
            ! =======================
              !
              mvc_use='RES'
              !
              call sMask(gvl_mask)
              !
              ! -----------------------------------------------------------------------------------
              !
              Use=2
              ! lvWB%lvi_ResComInd = For Gray water systems separation into Residential=1, Commercial=2, Industrial=3
              ! delineations for parameterization of rate-specific modeling
              ! ------------------------------------------------------------ 
              lvWB%lvi_ResComInd=1
                !               
                if(mvl_waterSim6)then
                    ! lvWB%lvf_excessNonPotable(i)
                    ! lvWB%lvf_outdoorNonPotable(i)
                    ! lvWB%lvf_outdoorPotable(i)
                    ! lvWB%lvf_indoorNonPotable(i)
                    ! lvWB%lvf_indoorPotable(i)

                    ! lvWB%lvf_unmetIndoorPotable(i)
                    ! lvWB%lvf_unmetOutdoorPotable(i)
                !
                  mvf_indoorDemandPotable(i,vTstep)=0
                  mvf_outdoorDemandPotable(i,vTstep)=0
                  mvf_indoorDemandNonPotable(i,vTstep)=0
                  mvf_outdoorDemandNonPotable(i,vTstep)=0
                  mvf_unusedNonPotable(i,vTstep)=0
                  !
                  mvf_indoorDemandPotable(i,vTstep)=lvWB%lvf_indoorPotable(i)*gvf_parm_WStoRes_prop(i)
                  mvf_indoorDemandNonPotable(i,vTstep)=lvWB%lvf_indoorNonPotable(i)*gvf_parm_WStoRes_prop(i)
                  mvf_outdoorDemandPotable(i,vTstep)=lvWB%lvf_outdoorPotable(i)*gvf_parm_WStoRes_prop(i)
                  mvf_outdoorDemandNonPotable(i,vTstep)=lvWB%lvf_outdoorNonPotable(i)*gvf_parm_WStoRes_prop(i)
                  mvf_unusedNonPotable(i,vTstep)=lvWB%lvf_excessNonPotable(i)*gvf_parm_WStoRes_prop(i)
                  !
                    lvf_nonPotable_useIndoor=0
                 !
                    lvf_leaksIndoor=0
                    lvf_leaksOutdoor=0
                   lvf_leaksIndoor = mvf_indoorDemandPotable(i,vTstep)* gvf_RateResLeak(i)
                   lvf_leaksOutdoor=  (mvf_outdoorDemandPotable(i,vTstep)+ mvf_outdoorDemandNonPotable(i,vTstep))* gvf_RateResLeak(i)
                  !
                else
                  !  NOTES:
                  ! -----------------------------------------
                  ! lvWB%lv_fluxFromWS_SRPandCAP(i,vTstep)        - net water total
                  ! lvWB%lv_fluxFromWS_Res_ROpotable(i,vTstep)    - ROreclaimed water
                  ! lvWB%lv_fluxFromWS_Res_nonPotable(i,vTstep)   - reclaimed water
                  ! lvWB%lv_fluxFromWS_Res_Groundwater(i,vTstep)  - Groundwater
                  ! lvWB%lv_fluxFromWS_Res_Potable(i,vTstep)      - all potable

                  ! lvWB%lvf_demand_Res_outdoor_acft_(i,vTstep)
                  ! lvWB%lvf_demand_Res_indoor_acft_(i,vTstep)
                  ! -------------------------------------------
                    lvf_indoorDemand(i,vTstep)=0
                  lvf_indoorDemand(i,vTstep)=lvWB%lvf_demand_Res_indoor_acft_(i,vTstep)
                  !
                     lvf_outdoorDemand(i,vTstep)=0
                  lvf_outdoorDemand(i,vTstep)=lvWB%lvf_demand_Res_outdoor_acft_(i,vTstep)
                  !
                    lvf_nonpotable=0
                  lvf_nonpotable=lvWB%lv_fluxFromWS_Res_nonpotable(i,vTstep) 
                  !
                    lvf_leaksIndoor=0
                    lvf_leaksOutdoor=0
                   lvf_leaksIndoor= lvf_indoorDemand(i,vTstep)* gvf_RateResLeak(i)
                   lvf_leaksOutdoor= lvf_outdoorDemand(i,vTstep)* gvf_RateResLeak(i)
                    !

                  ! =========================================================================
                   !
                    lvf_nonPotable_useIndoor=0
                   !lvf_nonPotable_useIndoor= (1-gvf_parm_ReclaimedOutdoor(i))*lvf_nonpotable
                   lvf_nonPotable_useIndoor= (1.-gvf_parm_ReclaimedOutdoor(i))*lvf_nonpotable
                    lvf_nonPotable_useOutdoor=0
                   lvf_nonPotable_useOutdoor=lvf_nonpotable-lvf_nonPotable_useIndoor
                    !
                    gof_nonPotable_useOutdoor=0
                   gof_nonPotable_useOutdoor=lvf_nonPotable_useOutdoor
                   !
                endif
                    !
                    lvf_BWaterNotUsed=0
                    lvf_BWaterUsed=0
                    lvf_indoor_deficit=0
                    !
                    if(mvl_waterSim6)then
                     lvf_indoorDemand(i,vTstep)=mvf_indoorDemandPotable(i,vTstep)
                     lvf_nonPotable_useIndoor=mvf_indoorDemandNonPotable(i,vTstep)
                     !
                     lvf_outdoorDemand(i,vTstep)= mvf_outdoorDemandPotable(i,vTstep)+ mvf_outdoorDemandNonPotable(i,vTstep)
                    endif
                    !
               call sBlackWater(lvWB,i,lvi_BWuse,lvf_indoorDemand(i,vTstep),lvf_nonPotable_useIndoor, &
                  lvf_BWaterUsed,lvf_BWaterNotUsed,lvf_indoor_deficit,lvf_flushesPerDay)
                !
                gof_flushesPCD(i)=lvf_flushesPerDay
                !
                ! ====================================================================================================
                ! Partition the leaked water indoor and outdoor
                ! ==============================================
                 lvf_indoor=0
                lvf_indoor=anint(lvf_indoorDemand(i,vTstep)) 
                !                
                 lvf_indoorUsable=0
                lvf_indoorUsable=anint(lvf_indoorDemand(i,vTstep)- lvf_leaksIndoor) 
                !
                ! ================================================

                !
                if(gpi_timeStepWB /=1)then
                else           
                  lvf_outdoor=0
                 lvf_outdoor=lvf_outdoorDemand(i,vTstep)
                endif
                !
                ! Outdoor use (changed from plus to minus leaks on 04.19.16)
                lv_fluxOutdoorUse(i,vTstep)=lvf_outdoor-lvf_leaksOutdoor

                lv_fluxIndoorUse(i,vTstep)=lvf_indoor+lvf_nonPotable_useIndoor -lvf_leaksIndoor
                lv_fluxWaterUseTotal(i,vTstep)= lvf_indoor+lvf_outdoor
                !
                  water=0
                 water=  lvf_indoorUsable * gif_showersBathPCT
                if(gpl_runSensitivity)then
                 water=  lvf_indoorUsable * mpf_showersBathPCT
                endif

                call sShowerMinutes(lvWB,i,water,T%days,minPCD)
                 gof_showersBathPCD(i)=minPCD
                !
                lvWB%lv_fluxResidential(i,vTstep)=nint(lv_fluxWaterUseTotal(i,vTstep))
                lvWB%lv_fluxResIndoor(i,vTstep)= nint(lv_fluxIndoorUse(i,vTstep))
                !
                ! 04.20.16 das new data table structure/implementation
                ! ----------
                if(mvl_waterSim6)then
                  lvWB%lvf_demand_Res_indoor_acft_(i,vTstep)=lv_fluxIndoorUse(i,vTstep)
                  lvWB%lvf_demand_Res_outdoor_acft_(i,vTstep)=lv_fluxOutdoorUse(i,vTstep)
                endif
                ! -----------------------------------------------------------------------
                  mvf_lost=0
                  mvf_unusedGrayWater=0
                 ! Residential is first in the loop, thus this additive variable is set to zero each year
                 ! ---------------------------------
                 lvWB%lvf_addToRunoffGrayWater(i)=0
                if(gvl_parm_grayWater)then

                  call sGrayWater(T,lvWB,i,vTstep,lvf_indoor,lv_fluxWaterUseTotal(i,vTstep), &
                    lvf_BWaterUsed,lv_fluxReclaimedResGreywater,lv_providerGPCD,mvf_lost,mvf_unusedGrayWater)
                    !
                      lvWB%lv_fluxReclaimedGreywaterRes(i,vTstep)=0
                      lvWB%lvf_GPCD_res(i,1)=0
                      lvWB%lvf_GPCD_res(i,2)=0
                    !
                    if( T%year < gvi_baseYear)then
                        mvf_lost=0
                        mvf_unusedGrayWater=0
                        lvWB%lv_fluxReclaimedGreywaterRes(i,vTstep)=0
                    else
                      !
                      ! 08.21.16 das
                      ! I added the unused Gray Water parameter, and the actualHouses parameter to simulate the adoption curve for
                      ! implementation of Gray Water Systems.  All are calculated in the subroutine call
                      ! ======================================
                      lvWB%lv_fluxReclaimedGreywaterRes(i,vTstep)=(lv_fluxReclaimedResGreywater(T%year,i,vTstep)) 
                      !
                      ! GPCD without graywater savings (i,1) and with graywater savings (i,2)
                      lvWB%lvf_GPCD_res(i,1)=lv_providerGPCD(i,1)
                      lvWB%lvf_GPCD_res(i,2)=lv_providerGPCD(i,2)
                     !
                     ! This NEW variable should NEVER have a value > 0
                     ! 04.19.16 das
                      lvWB%lvf_addToRunoffGrayWater(i)= mvf_unusedGrayWater
                    endif
                   !
                else
                 lv_fluxReclaimedResGreywater(T%year,i,vTstep)=0
                 lvWB%lvf_GPCD_res(i,1)=0
                 lvWB%lvf_GPCD_res(i,2)=0
                endif
                ! 
                ! ---------------------------------------------
                !
                lvWB%lv_fluxResOutdoor(i,vTstep)= nint(lv_fluxOutdoorUse(i,vTstep))
                !
                ! Evaporative flux of outdoor water
                ! --------------------------------
                call sWaterFluxToAtmos(lvWB,i,vTstep,lvWB%lv_fluxResOutdoor(i,vTstep), lv_fluxToAtmos(i,vTstep))
                  !
                  lv_fluxToAtmos(i,vTstep)=nint(lv_fluxToAtmos(i,vTstep))
                   gvf_resEvapotrans(i)=0
                  gvf_resEvapotrans(i)=lv_fluxToAtmos(i,vTstep)+mvf_lost
                ! ---------------------------------------------

                ! Outdoor water that moves into the vadose
                ! Added leaks on 04.19.16 das
                lvWB%lv_fluxResVadose(i,vTstep)=nint(max(0.,(lvWB%lv_fluxResOutdoor(i,vTstep)-lv_fluxToAtmos(i,vTstep)))) + lvf_leaksOutdoor
                !

                ! States
                ! -----------------------------------------------------------------------------------------------
                !
                vState_WB_Residential(T%year,i,vTstep)=vState_WB_Residential(T%year,i,vTstep) &
                  +(lv_fluxWaterUseTotal(i,vTstep)+  lvWB%lv_fluxReclaimedGreywaterRes(i,vTstep)) &
                  -(lv_fluxIndoorUse(i,vTstep) + lv_fluxOutdoorUse(i,vTstep))
                !
                lvWB%lv_fluxResidentialWW(i,vTstep)=lvWB%lv_fluxResidentialWW(i,vTstep)+(lv_fluxIndoorUse(i,vTstep) &
                 - lvWB%lv_fluxReclaimedGreywaterRes(i,vTstep))* gpf_WWsourceEfficiency
                ! 
10      format(I4,1x,I2,1x,L1,1x,3(F11.3,1x))
                ! =================== 
                vState_WB_ResidentialIndoor(T%year,i,vTstep)=vState_WB_ResidentialIndoor(T%year,i,vTstep) &
                  +( lv_fluxIndoorUse(i,vTstep)+lv_fluxReclaimedResGreywater(T%year,i,vTstep)) &
                  - lvWB%lv_fluxResidentialWW(i,vTstep)

                vState_WB_ResidentialOutdoor(T%year,i,vTstep)=vState_WB_ResidentialOutdoor(T%year,i,vTstep) &
                  + lv_fluxOutdoorUse(i,vTstep) &
                  - (lvWB%lv_fluxResVadose(i,vTstep) + lv_fluxToAtmos(i,vTstep))
                !
                ! ---------------------
                ! Update states
                ! -----------------
                !
                call sUpdateState(T,i,vTstep, vState_WB_ResidentialIndoor)
                call sUpdateState(T,i,vTstep, vState_WB_ResidentialOutdoor)
                call sUpdateState(T,i,vTstep, vState_WB_Residential)
            !
         return
        end subroutine sResidential
        ! -------------------------

        ! ------------------------------------
        subroutine sROprocess(T,lvWB,i,vTstep)
            !
            ! --------------------- Types ----------------------
            integer :: i
            integer :: vTstep   
            integer,parameter :: state=14

            real :: lv_fluxFromRWWTP(gvi_maxProV,gpi_timeStepWB)
            ! ==================================================
            !

            ! -- Type Constructs --
            type (waterbalance)lvWB          
            type(parameters)P
            type(runTime)T
            ! =====================
              !
              call sMask(gvl_mask)
                !
                P%In_1= gvf_parm_ROtoOutput(i)
                P%In_2=0
                P%In_3=0
                P%In_4=0
               !
                if(gvl_parm_shortage(i))Then
                 P%Out_1=gvf_parm_ROtoOutput(i)
                else
                  call sProportion(state,i,vTstep,parms_RO,initParms,P)
                  gvf_parm_ROtoOutput(i)=P%Out_1
                end if
                !
                ! -------------------------------
                ! These need parameterization 05.02.11
                 lvWB%lv_ROflux_RO_Reclaimed(i,vTstep)=0.
                 lvWB%lv_ROflux_DirectInject_Potable(i,vTstep)=0.
                lv_fluxFromRWWTP(i,vTstep)=lvWB%lv_fluxToRevOsmosis(i,vTstep)
                ! 
                lvWB%lv_ROflux_RO_Reclaimed(i,vTstep)=gpf_ROefficiency * lv_fluxFromRWWTP(i,vTstep)
                !
                lvWB%lv_ROflux_DirectInject_Potable(i,vTstep)= &
                  (1.-gvf_parm_ROtoOutput(i))*lvWB%lv_ROflux_RO_Reclaimed(i,vTstep)
                !
                gvf_ROreclaimedOutput(T%year,i,vTstep)= gvf_parm_ROtoOutput(i)*lvWB%lv_ROflux_RO_Reclaimed(i,vTstep)
                !
                vState_ROprocess(T%year,i,vTstep)=vState_ROprocess(T%year,i,vTstep) & 
                  +  (gpf_ROefficiency*lv_fluxFromRWWTP(i,vTstep)) &
                  -  lvWB%lv_ROflux_DirectInject_Potable(i,vTstep) &
                  -  gvf_ROreclaimedOutput(T%year,i,vTstep)
                !
                ! ---------------------
                ! Update states
                ! -----------------
                ! 
                call sUpdateState(T,i,vTstep,vState_ROprocess)
                call sStepUpdate(T,i,vTstep,state)
                !
          return
        end subroutine sROprocess
        ! -------------------------

       ! ---------------------------------
        subroutine sRWWTP(T,lvWB,i,vTstep)
            !
            ! ----------- Types -----------
            integer :: i
            integer :: vTstep   
            integer,parameter :: state=13

            real :: lvf_checkWWtoRWWTP
            ! =============================
            !
            
            ! ---- Type Constructs ---
            type (waterbalance)lvWB          
            type(parameters)P
            type(runTime)T
            ! ========================
              !
              if(lvl_firstLoop)call sMask(gvl_mask)
              !
                P%In_1= gvf_parm_RWWtoRO(i)
                P%In_2=0
                P%In_3=0
                P%In_4=0
                !
               call sProportion(state,i,vTstep,parms_RWWTP,initParms,P)
                gvf_parm_RWWtoRO(i)=P%Out_1
               !
               ! -----------------------------------------------
                lvf_checkWWtoRWWTP=0
               lvf_checkWWtoRWWTP=gvf_parm_WWtoRWWTP(i)
               if(1 < lvf_checkWWtoRWWTP)gvf_parm_WWtoRWWTP(i)=1

               ! -----------------------------------------------
               !
              ! Reclaimed waste water treatmemt plant- 05.02.11 das
              lvWB%lv_fluxToRWWTP(i,vTstep)=gvf_parm_WWtoRWWTP(i)*(lvWB%lv_fluxResidentialWW(i,vTstep) &
                 + lvWB%lv_fluxIndWW(i,vTstep)+ lvWB%lv_fluxComWW(i,vTstep)) * gpf_RWWTPefficiency         

            ! Flux to RO
            lvWB%lv_fluxToRevOsmosis(i,vTstep)=P%Out_1 *  lvWB%lv_fluxToRWWTP(i,vTstep)

            !  Flux to Reclaimed
            lvWB%lv_fluxRWWTPToReclaimed(i,vTstep)=lvWB%lv_fluxToRWWTP(i,vTstep) &
               - lvWB%lv_fluxToRevOsmosis(i,vTstep)               
            !
            ! =================== 
            vState_WB_RWWTP(T%year,i,vTstep)=vState_WB_RWWTP(T%year,i,vTstep) &
              + lvWB%lv_fluxToRWWTP(i,vTstep)-lvWB%lv_fluxToRevOsmosis(i,vTstep)
            !
            ! ---------------------
            ! Update states
            ! -----------------
            ! 
            call sUpdateState(T,i,vTstep,vState_WB_RWWTP)
            !
         return
        end subroutine sRWWTP
        ! -------------------

        ! -------------------------------------------
        subroutine sSurfaceWater(T,vPV,lvWB,i,vTstep)
            !
            ! --------------------------- Types -----------------------------------
            integer :: i
            integer :: vTstep
!           integer,parameter :: state=1

            real :: gvf_fluxFromGWBanking(gvi_maxProV,gpi_timeStepWB)
            real :: lvf_SWaterToVadose(gvi_maxProV),lvf_SWaterToBanking(gvi_maxProV)
            real :: lvf_bankedGWater_acft_a,lvf_newWaterSupplies_acft_a
            ! ======================================================================
            !

            ! --- Type Constructs --
            type (waterbalance)lvWB
            type(Provider)vPV
            type(runTime)T
            ! ======================
                !
                if(lvl_firstLoop)then
                 call sMask(gvl_mask)
                 call sMaskCAP(gvl_maskCAP)
                endif
                ! ------------------------
                ! --------------------------------
                !
               ! lvWB%lvf_excessNonPotable(i)=vPV%lvf_excessNonPotable(i)
               ! lvWB%lvf_outdoorNonPotable(i)=vPV%lvf_outdoorNonPotable(i)
               ! lvWB%lvf_outdoorPotable(i)=vPV%lvf_outdoorPotable(i)
                !lvWB%lvf_indoorNonPotable(i)=vPV%lvf_indoorNonPotable(i)
               ! lvWB%lvf_indoorPotable(i)=vPV%lvf_indoorPotable(i)
                !lvWB%lvf_unmetIndoorPotable(i)=vPV%lvf_unmetIndoorPotable(i)
                !lvWB%lvf_unmetOutdoorPotable(i)=vPV%lvf_unmetOutdoorPotable(i)

                ! CAP and SRP 
                lvWB%lv_fluxSurfaceWater_annual(i)= &
                  lvWB%lvf_fluxSWaterCAP(i)+ lvWB%lvf_fluxSWaterSRP(i)+ lvWB%lvf_fluxGWaterSRP(i)
                !
                if(gpi_timeStepWB /=1)then
                     if(vTstep == 1)then
                     else
                    endif
                else
                  gvf_fluxFromGWBanking(i,vTstep)= gvf_GW_Banking(T%year,i,1)
                endif
                ! 
                lvWB%lvf_fluxGWaterBankingToSWater(T%year,i,vTstep)=gvf_fluxFromGWBanking(i,vTstep)

                ! These (two) were added on 02.21.13 DAS WTF
                ! -------------------------------------------
                 lvf_bankedGWater_acft_a=0
                lvf_bankedGWater_acft_a=lvWB%lvf_fluxGWaterBankingToSWater(T%year,i,vTstep)
                !
                 lvf_newWaterSupplies_acft_a=0
                lvf_newWaterSupplies_acft_a=vPV%lvf_newWaterSupplies_AF_a(i)
                !
                if(gvl_maskCAP(i))then
                  lvf_SWaterToVadose(i) =0
                 lvf_SWaterToVadose(i) =  (gvf_parm_SWtoVadoseAmt(i)) /gpi_timeStepWB
                 !
                 ! Does this actually pass good data?
                  lvf_SWaterToBanking(i)=0               
                 lvf_SWaterToBanking(i)=vPV%gvf_cSetWBanking_a(i)
                 !

                 !
                 lvWB%lv_fluxSurfaceWaterToVadose(i,vTstep)   =  lvf_SWaterToVadose(i)
                 lvWB%lvf_fluxSWaterToGWaterBanking(i,vTstep) =  lvf_SWaterToBanking(i)
                 !
                 ! To the surface water treatment plant
                 lvWB%lv_fluxSurfaceWaterToSWTP(i,vTstep)= lvWB%lv_fluxSurfaceWater_annual(i) &
                  + (lvf_bankedGWater_acft_a + lvf_newWaterSupplies_acft_a ) &
                  - (lvWB%lv_fluxSurfaceWaterToVadose(i,vTstep) &
                  + lvWB%lvf_fluxSWaterToGWaterBanking(i,vTstep))
                 ! 
                endif
                !
                ! ---- STATE -----
                !
                vState_WB_CanalWater(T%year,i,vTstep)=nint(vState_WB_CanalWater(T%year,i,vTstep) &
                    +  lvWB%lv_fluxSurfaceWater_annual(i) +  gvf_fluxFromGWBanking(i,vTstep) &
                    -  lvWB%lv_fluxSurfaceWaterToVadose(i,vTstep) &
                    -  lvWB%lvf_fluxSWaterToGWaterBanking(i,vTstep) &
                    -  lvWB%lv_fluxSurfaceWaterToSWTP(i,vTstep))
                !
                ! ---------------------
                ! Update states
                ! -----------------
                !
                call sUpdateState(T,i,vTstep,vState_WB_CanalWater)
                !       
             return
100       continue
          stop
        end subroutine sSurfaceWater
        ! --------------------------
  
        ! -------------------------------
        subroutine sSWTP(T,lvWB,i,vTstep)
            !
            ! ------------------ Types ----------------------
            integer :: i
            integer :: vTstep

            real :: lv_fluxToSWTP(gvi_maxProV,gpi_timeStepWB)
!            integer,parameter :: state=3
            ! ===============================================
            !

            ! --- Type Constructs --
            type (waterbalance)lvWB
            type(runTime)T
            ! ====================
                !
                ! Has GW banking and SW to Vadose removed- has indoor and outdoor fluxes
                 lv_fluxToSWTP(i,vTstep)=0
                lv_fluxToSWTP(i,vTstep)= lvWB%lv_fluxSurfaceWaterToSWTP(i,vTstep)
                !
                lvWB%lvf_fluxFromSWTP_WS_acft(i,vTstep)= lv_fluxToSWTP(i,vTstep)*gpf_SWTPefficiency
                !

                ! =================== 
                vState_WB_SWTP(T%year,i,vTstep)=vState_WB_SWTP(T%year,i,vTstep)+ &
                  lv_fluxToSWTP(i,vTstep)-lvWB%lvf_fluxFromSWTP_WS_acft(i,vTstep)
                !
                ! ---------------------
                ! Update states
                ! -----------------
                !
                call sUpdateState(T,i,vTstep,vState_WB_SWTP)
                !
           return
          end subroutine sSWTP
        ! --------------------

        ! -------------------------------------
        subroutine sVadose(T,lvWB,i,vTstep)
            !
            ! ---------------------- Types ------------------------
            integer :: i,j,lagVariable
            integer :: vTstep
            integer :: lvi_TimeLag
                
            real :: lv_fluxToVadose(gvi_maxProV,gpi_timeStepWB)  
            real :: lvf_VadoseToAquiferEst
            real :: lvf_fluxOut,lvf_vadoaseEvaporation(gvi_maxProV)
            real, parameter :: lpf_minYears=0
            real, parameter :: lpf_maxYears=15

            real :: lpf_vadoseLoss
            REAL :: lvf_addedPercolation,lvf_addedHarvestedWater
            real :: lvf_postOtherPerc
            real :: lvf_totalDischargeToSurface
            !
!           integer,parameter :: state=18
            ! =====================================================
            !

            ! ------- Type constructs -------
            type (waterbalance)lvWB
            type(runTime)T
            type localVadose
             real :: SW,Effluent,RunoffPerc
             real :: Ag,OutdoorCIOandRes
             real :: Recharge
             real :: surfaceDischarge
             real :: LagFlux
            end type localVadose
            type(localVadose) V
            ! ===============================
                !
                lvi_TimeLag=gpi_Tlag
                lvf_addedPercolation=0
                lvf_addedHarvestedWater=0
                lvf_postOtherPerc=0
                lvf_totalDischargeToSurface=0
                !
                ! Receive Input from the Interface to override default settings
                ! bounds set on 07.16.13 DAS
                ! -------------------------------------------------------------
                if(0 < gvi_timeLagVadose(i))lvi_TimeLag=min(lpf_maxYears,max(lpf_minYears,gvi_timeLagVadose(i)))

                !~ ------------------------------------------------------------------
                ! Add to groundwater credits - annual allowance
                ! pass to Groundwater.f90,   subroutine pProviderGroundwater(T,gvi_order) 
                 gvf_incidentalCredit(T%year,i)=0
                 V%OutdoorCIOandRes=0
                 lvf_fluxOut=0
                 lagVariable=1
                call sLagVadose(T,i,lagVariable,lvi_TimeLag,lvWB%lv_fluxResVadose(i,vTstep),lvf_fluxOut)
                 gvf_incidentalCredit(T%year,i)=lvf_fluxOut
                !
                 lvf_fluxOut=0
                 lagVariable=2
                call sLagVadose(T,i,lagVariable,lvi_TimeLag,lvWB%lv_fluxCioVadose(i,vTstep),lvf_fluxOut)
                ! Add direct inject for credit model here
                 gvf_incidentalCredit(T%year,i)= gvf_incidentalCredit(T%year,i)+lvf_fluxOut ! +  lvWB%lvf_fluxDirectInjectToGW(i,vTstep)

                ! Remove Direct Inject from incidental for Regional Model
                V%OutdoorCIOandRes=gvf_incidentalCredit(T%year,i) !- lvWB%lvf_fluxDirectInjectToGW(i,vTstep)

                 V%Effluent=0
                 lvf_fluxOut=0
                 lagVariable=3
                call sLagVadose(T,i,lagVariable,lvi_TimeLag,lvWB%lv_fluxToVadoseFromEffluent(i,vTstep),lvf_fluxOut)
                 V%Effluent  =lvf_fluxOut !+ gvf_agUnusedEffluent(i)

                 !lvf_fluxOut=0
                 lagVariable=4
                call sLagVadose(T,i,lagVariable,lvi_TimeLag,lvWB%lvf_fluxReclaimedToVadose(i,vTstep),lvf_fluxOut)
                 V%Effluent  = V%Effluent  +lvf_fluxOut 

                 V%RunoffPerc=0
                 lvf_fluxOut=0
                 lagVariable=5
                call sLagVadose(T,i,lagVariable,lvi_TimeLag,lvWB%lv_runoffPerc(i,vTstep),lvf_fluxOut)
                 !
                 ! Added fluxes to the groundwater
                 !  go_HarvestStormWater added on 11.19.15 DAS, go_harvestRainWater_AF added on 03.18.16 DAS
                 !  go_GrayWaterReclaimed_acft(i,1) added on 03.20.16
                  lvf_addedPercolation=0
                  do j = 1,3,1
                   lvf_addedPercolation= lvf_addedPercolation+go_GrayWaterReclaimed_acft(i,j)
                  enddo
                 !
                 ! Storm water added in sRainfallRunoff()
                 ! 03.31.16 das 
                  lvf_addedHarvestedWater=0
                 lvf_addedHarvestedWater= go_harvestRainWater_AF(i) + go_stormWaterUsedUsed_AF(i)
                 !lvf_addedHarvestedWater= go_harvestRainWater_AF(i) + go_HarvestStormWater_AF(i)

                 V%RunoffPerc=lvf_fluxOut + lvf_addedPercolation +  lvf_addedHarvestedWater
                 !
                 ! Do spatially explitic partitioning here?
                 ! 04.03.16
                   gvf_otherOutdoorPerc(i)=0
                 gvf_otherOutdoorPerc(i)=V%RunoffPerc*lvWB%lvf_LCLU_proportions(i,5) &
                   + V%RunoffPerc*lvWB%lvf_LCLU_proportions(i,7)
                 !
                 lvf_postOtherPerc=V%RunoffPerc- gvf_otherOutdoorPerc(i)

                   gvf_resOutdoorPerc(i)= lvf_postOtherPerc* gvf_parm_WStoRes_prop(i)
                   gvf_comOutdoorPerc(i)= lvf_postOtherPerc* gvf_parm_WStoCom_prop(i)
                   gvf_indOutdoorPerc(i)= lvf_postOtherPerc* gvf_parm_WStoInd_prop(i)
                 !
                ! ================================
                ! as of 04.03.16
                ! Scenarios
                ! 1 AG
                ! 2 building
                ! 3 canal
                ! 4 cultivated grass
                ! 5 greenway
                ! 6 impevious
                ! 7 mountain vege
                ! 8 residential
                ! 9 soil
                ! 10 tree
                ! 11 unclassified
                ! 12 wash
                ! 13 water

                 V%Ag=0        
                 lvf_fluxOut=0
                 lagVariable=6
                call sLagVadose(T,i,lagVariable,lvi_TimeLag,lvWB%lv_fluxToVadoseFromAg(i,vTstep),lvf_fluxOut)
                 V%Ag =lvf_fluxOut

                 go_agPercolation=0
                go_agPercolation=nint(V%Ag)

                !  This is from SRP and CAP surface water (or Ag if Vinze-Johnston project)
                ! Check- is lvWB%lv_fluxSurfaceWaterToVadose(i,vTstep)= lvWB%lvf_municipalRecharge_acft_(i) ??????
                 V%SW=0
                 lvf_fluxOut=0
                 lagVariable=7
                call sLagVadose(T,i,lagVariable,lvi_TimeLag,lvWB%lv_fluxSurfaceWaterToVadose(i,vTstep),lvf_fluxOut)
                 V%SW=lvf_fluxOut
                ! Add Provider Recharge AF yr-1

                 gvf_incidentalCredit(T%year,i)= gvf_incidentalCredit(T%year,i) !+lvf_fluxOut
                ! Send to the interface 05.14.12
                 go_incidentalCredit_acft(i)=nint( gvf_incidentalCredit(T%year,i))
                !
                ! I think I should be evaporating a bunch of this... and we are not.....04.05.16 das
                ! I added lvWB%lvf_fluxReclaimedToRWWSDischarge(i,vTstep) on 04.05.15 das
                ! ====================
                 V%surfaceDischarge=0
                 lvf_fluxOut=0
                 lagVariable=8
                lvf_totalDischargeToSurface=lvWB%lvf_fluxReclaimedToRWWSDischarge(i,vTstep)+lvWB%lv_fluxWWToSurfaceDischarge(i,vTstep)
                call sLagVadose(T,i,lagVariable,lvi_TimeLag,lvf_totalDischargeToSurface,lvf_fluxOut)
                ! 
                 V%surfaceDischarge= lvf_fluxOut 
                ! --------------------------------------------------
                ! 05.06.14
                  !lpf_vadoseDepth=1.0
                 lpf_vadoseLoss=fVadoseLoss(lvi_TimeLag)
                !
                ! These are the instantaneous estimates
                 lv_fluxToVadose(i,vTstep)= ( lvWB%lv_fluxResVadose(i,vTstep) + lvWB%lv_fluxCioVadose(i,vTstep) &
                    + lvWB%lv_fluxToVadoseFromEffluent(i,vTstep) &
                    + lvWB%lvf_fluxReclaimedToVadose(i,vTstep) &
                    + lvWB%lv_runoffPerc(i,vTstep) + lvWB%lv_fluxToVadoseFromAg(i,vTstep) &
                    + lvWB%lv_fluxSurfaceWaterToVadose(i,vTstep) &
                    + lvWB%lv_fluxWWToSurfaceDischarge(i,vTstep) ) 

                ! These are the lagged estimates           
                !
                 lvf_VadoseToAquiferEst=0
                 lvf_VadoseToAquiferEst=(V%OutdoorCIOandRes  + V%Effluent &
                    +  V%RunoffPerc + V%Ag + V%SW + V%surfaceDischarge )
                !
                call storeLaggedVadose(T,i,lvi_TimeLag,lvf_VadoseToAquiferEst)

                lvWB%lv_fluxVadoseToAquifer(i,vTstep)= gvf_lagVadoseToAquifer(T%year,i)*lpf_vadoseLoss

                !
                 lvf_vadoaseEvaporation(i)=0
                lvf_vadoaseEvaporation(i)=max(0,lvf_VadoseToAquiferEst-(lvf_VadoseToAquiferEst*lpf_vadoseLoss ))
                lvWB%lvf_vadoseFlux(i,vTstep)= lvf_vadoaseEvaporation(i)
                !
                lvWB%lv_fluxAtmosTotal(i,vTstep)= max(0,lvWB%lv_fluxAtmosTotal(i,vTstep)+lvWB%lvf_vadoseFlux(i,vTstep))

                ! -- STATE ----
                ! =================== 
                vState_WB_Vadose(T%year,i,vTstep)= &
                    lv_fluxToVadose(i,vTstep) - lvWB%lv_fluxVadoseToAquifer(i,vTstep)
                !
3 format(I4,1x,I2,1x,6(F12.3,2x))
                ! ---------------------
                ! Update states
                ! -----------------
                !
                call sUpdateState(T,i,vTstep,vState_WB_Vadose)
                !
                ! 05.02.12 add credits to each providers bucket of credits
                ! for the credit model ( subroutine sCreditBalance(T,i,vTstep) below )

                ! 08.22.12 I added direct injection to this variable.  I am in the process of
                ! verifying this move
                ! =================================
                ! 
                  gvf_annualGWStored_acft(i)=0
                 !gvf_annualGWStored_acft(i)= V%SW + V%Effluent + lvWB%lvf_fluxDirectInjectToGW(i,vTstep)
                 !
                 ! 03.20.16 DAS changed
                 gvf_annualGWStored_acft(i)= lvWB%lv_fluxVadoseToAquifer(i,vTstep)+ lvWB%lvf_fluxDirectInjectToGW(i,vTstep)

                  gof_getAnnualGWstored_acft(i)=0
                 gof_getAnnualGWstored_acft(i)=gvf_annualGWStored_acft(i)
                !
          return
        end subroutine sVadose
        ! --------------------

          ! -----------------------------------------------------------------------------
            subroutine sLagVadose(T,provider,variable,lvi_TimeLag,lvf_fluxIn,lvf_fluxOut)
                !
                ! ------------------- Types ----------------------
                integer :: provider,variable,lvi_TimeLag,lagYear

                real :: current,popPast,increment
                real :: lvf_fluxIn,lvf_fluxOut
                real, parameter :: lvf_growthRate=1
                ! ================================================
                !
    
                ! --- type construct ---
                type(runTime)T
                ! ======================

                ! Three dimensional array- time:provider:variable
                ! Array is labeled: gvf_lagToVadose(T%year,provider,variable)

                ! Variables are:
                !  One=Residential outdoor water to Vadose
                !  Two=CommercialIndustrial outdoor water to Vadose
                !  Three=Effluent to Vadose
                !  Four=Reclaimed water to Vadose
                !  Five=Rainfall to Vadose
                !  Six=Agricultural Irrigation to Vadose
                !  Seven=Surface water to Vadose
                !  Eight=Surfaces Discharge (Waste Water Treatment Plant to Washes)
                ! -----------------------------------------------------------------
                    
                    !
                    gvf_lagToVadose(T%year,provider,variable)=lvf_fluxIn
                    !
                     lagYear=0
                     increment=0
                     lvf_fluxOut=0
                     popPast=0
                     current=0
                     !
                    if(T%year <= T%startyear+lvi_TimeLag-1)then
                     ! Add one because as a procedural language, subroutine sGWater(T,gvi_order) is called first, yet the outputs
                     ! from this subroutine are used in subroutine pProviderGroundwater(T,gvi_order).  So, I need to correct for the
                     ! one year off-set. 05.07.12
                     ! ----------------------------
                       lagYear=T%year-lvi_TimeLag+1
                      !
                      increment=T%startyear-lagYear
                      !
                      if(0 < lid_providerpop(T%startyear,provider))then
                       current=gvf_lagToVadose(T%startyear,provider,variable)/lid_providerpop(T%startyear,provider)
                      endif
                       !
                       popPast=((1.-increment/100)*lvf_growthRate )  * lid_providerpop(T%startyear,provider)
                       !
                       lvf_fluxOut=current*popPast
                       !
                    else
                      lvf_fluxOut= gvf_lagToVadose(T%year-lvi_TimeLag+1,provider,variable)
                    endif
                    !
             return
            end subroutine sLagVadose
          ! --------------------------
          !                 
            subroutine storeLaggedVadose(T,provider,lvi_TimeLag,in)
                ! ----- Types ------
                integer :: lvi_TimeLag,provider
                real :: in
                real :: currentYr
                real :: growth=1.15 ! percent per year
                ! ==================

                ! --- type construct ---
                type(runTime)T
                ! ======================
                    !
                    currentYr=T%year+lvi_TimeLag
                    gvf_lagVadoseToAquifer(currentYr,provider)= in
                    if(T%year < T%startyear+lvi_TimeLag)then
                     gvf_lagVadoseToAquifer(T%year,provider)=(1-(lvi_TimeLag*growth*1/100 ))*in 
                    endif
                    !
              return
            end subroutine storeLaggedVadose


            ! 05.06.14 - SAS regression (b0 and b1 estimates)- 
            !            0.075 is in the API to calculate years from depth
            ! ----------------------------------------------------------------
            function fVadoseInfiltration(lvi_TimeLag) result(InfiltrationRate)
                !
                ! ------------ Types ---------------
                integer :: lvi_TimeLag
                ! Muckel, D.C. 1959 ARS Tech Bull 51 pp
                !
                real :: InfiltrationRate
                !real, parameter :: b1=-0.9167
                real, parameter :: b1=-0.354
                !real, parameter :: b0=70
                real, parameter :: b0=4380 ! 12 years x 365 days year-1 ( m day-1) 
                !real, parameter :: yearsToRate=0.075
                real, parameter :: yearsToRate=0.02 ! convert to feet year-1
                real, parameter :: max=1
                ! ----------------------------------
                    !
                     InfiltrationRate=1
                    InfiltrationRate=min(max,(b0*(lvi_TimeLag * (1/yearsToRate))**b1)*1/1000)
                    !
             return
            end function
            ! -----------------------------

         ! ----------------------------------------------------------------
            function fVadoseLoss(lvi_TimeLag) result(InfiltrationLoss)
                !
                ! ------------ Types ---------------
                integer :: lvi_TimeLag
                ! Muckel, D.C. 1959 ARS Tech Bull 51 pp
                !
                real :: InfiltrationLoss
                real, parameter :: b1=-0.9167
                real, parameter :: b0=20
                real, parameter :: yearsToRate=0.075 ! convert to feet year-1
                real, parameter :: max=1
                ! ----------------------------------
                    !
                     InfiltrationLoss=1
                    InfiltrationLoss=min(max,(b0*(lvi_TimeLag * (1/yearsToRate))**b1))
                    !
             return
            end function
            ! -----------------------------

       ! -----------------------------------
        subroutine sWSupply(T,lvWB,i,vTstep)
            !
            ! ----------------- Types ---------------------
            integer :: i
            integer :: vTstep
            !integer,parameter :: state=4

            real :: lv_fluxToWS(gvi_maxProV,gpi_timeStepWB)
            !==============================================
            !

            ! --------- Type Constructs ----------
            type typeWS
             real :: lvf_Reclaimed,lvf_ROreclaimed
            end type typeWS
            type(typeWS) lvWS
            type (waterbalance)lvWB
            type(parameters)P
            type(runTime)T
            ! ====================================
                !
                mvl_waterSim6=.true.
                !
                if(lvl_firstLoop)then
                 call sMask(gvl_mask)
                 call sMaskCAP(gvl_maskCAP)
                endif
                !
                 P%In_1= gvf_parm_WStoRes_prop(i)
                 P%In_2= gvf_parm_WStoCom_prop(i)
                 P%In_3=gvf_parm_WStoInd_prop(i)
                 P%In_4=0
                 !
                 P%Out_1=0
                 P%Out_2=0
                 P%Out_3=0
                 P%Out_4=0
                 !
                call sProportionNew(P)
                 !
                 gvf_parm_WStoRes_prop(i)=P%Out_1
                 gvf_parm_WStoCom_prop(i)=P%Out_2
                 gvf_parm_WStoInd_prop(i)=P%Out_3
                !
                ! FromSWTP has been corrected for WBanking and Vadose storage
                !
                ! If a shortage occurs, defined in subroutine unmetdemand, then no water sent to DI
                lvWB%lvf_fluxWStoDirectInjection(i,vTstep) =  (gvf_parm_WStoDIamount(i)) /gpi_timeStepWB

                ! Must look at demand and supply here to balance water
                ! -----------------------------------------------------
                if(gpi_timeStepWB /= 1)then
                 call sBalanceWS(T,lvWB,i,vTstep,lvWS)
                else
                 call sBalanceWS(T,lvWB,i,vTstep,lvWS)
                endif
                ! -----------------------------------------------------
                !
     
                     lv_fluxToWS(i,vTstep)=0
                    lv_fluxToWS(i,vTstep)= anint(lvWB%lvf_fluxFromSWTP_WS_acft(i,vTstep) & 
                        + lvWB%lvf_fluxGWaterTreatmentToWSupply(i,vTstep) &
                        + lvWB%lvf_fluxGWaterToWSupply(i,vTstep) &
                        + lvWS%lvf_ROreclaimed &
                        + lvWS%lvf_Reclaimed) * gpf_WSefficiency
                    !
                    lvWB%lv_fluxFromWS_Potable(i,vTstep)=lvWS%lvf_ROreclaimed
                    lvWB%lv_fluxFromWS_nonPotable(i,vTstep)=lvWS%lvf_Reclaimed

                    lvWB%lv_fluxFromWS_Groundwater(i,vTstep)= lvWB%lvf_fluxGWaterTreatmentToWSupply(i,vTstep) &
                        + lvWB%lvf_fluxGWaterToWSupply(i,vTstep)
                    !
                    ! -------------------------------------------------------------------------------
                    ! Includes new supplies and banked water as of 02.21.13 
                    ! -----------------------------------------------------
                    if(gvl_maskCAP(i))then
                     lvWB%lv_fluxFromWS_SRPandCAP(i,vTstep)=lvWB%lvf_fluxFromSWTP_WS_acft(i,vTstep) &
                        -  lvWB%lvf_fluxWStoDirectInjection(i,vTstep)
                    endif
                ! das
                if(mvl_waterSim6)then
                ! lvWB%lvf_excessNonPotable(i)
                ! lvWB%lvf_outdoorNonPotable(i)
                ! lvWB%lvf_outdoorPotable(i)
                !lvWB%lvf_indoorNonPotable(i)
                ! lvWB%lvf_indoorPotable(i)
                !lvWB%lvf_unmetIndoorPotable(i)
                !lvWB%lvf_unmetOutdoorPotable(i)
                else
                    lvWB%lv_fluxFromWS_Res_nonPotable(i,vTstep) =anint(P%Out_1*lvWB%lv_fluxFromWS_nonPotable(i,vTstep))
                    lvWB%lv_fluxFromWS_Res_ROPotable(i,vTstep)   =anint(P%Out_1*lvWB%lv_fluxFromWS_Potable(i,vTstep))
                    lvWB%lv_fluxFromWS_Res_Groundwater(i,vTstep) =anint(P%Out_1*lvWB%lv_fluxFromWS_Groundwater(i,vTstep))
                    lvWB%lv_fluxFromWS_Res_SRPandCAP(i,vTstep)   =anint(P%Out_1*lvWB%lv_fluxFromWS_SRPandCAP(i,vTstep))
                    !
                    ! -----------------------------------------
                    lvWB%lv_fluxFromWS_Ind_nonpotable(i,vTstep) =anint(P%Out_3*lvWB%lv_fluxFromWS_nonPotable(i,vTstep))
                    lvWB%lv_fluxFromWS_Ind_ROPotable(i,vTstep)  =anint(P%Out_3*lvWB%lv_fluxFromWS_Potable(i,vTstep))
                    lvWB%lv_fluxFromWS_Ind_Groundwater(i,vTstep)=anint(P%Out_3*lvWB%lv_fluxFromWS_Groundwater(i,vTstep))
                    lvWB%lv_fluxFromWS_Ind_SRPandCAP(i,vTstep)  =anint(P%Out_3*lvWB%lv_fluxFromWS_SRPandCAP(i,vTstep))
                    ! -----------------------------------------
                    lvWB%lv_fluxFromWS_Com_nonpotable(i,vTstep) =anint(P%Out_2*lvWB%lv_fluxFromWS_nonPotable(i,vTstep))
                    lvWB%lv_fluxFromWS_Com_ROPotable(i,vTstep)  =anint(P%Out_2*lvWB%lv_fluxFromWS_Potable(i,vTstep))
                    lvWB%lv_fluxFromWS_Com_Groundwater(i,vTstep)=anint(P%Out_2*lvWB%lv_fluxFromWS_Groundwater(i,vTstep))
                    lvWB%lv_fluxFromWS_Com_SRPandCAP(i,vTstep)  =anint(P%Out_2*lvWB%lv_fluxFromWS_SRPandCAP(i,vTstep))
                    ! -----------------------------------------
                    lvWB%lv_fluxFromWS_Res_Potable(i,vTstep)    = lvWB%lv_fluxFromWS_Res_ROPotable(i,vTstep) &
                        +  lvWB%lv_fluxFromWS_Res_Groundwater(i,vTstep) +  lvWB%lv_fluxFromWS_Res_SRPandCAP(i,vTstep)

                    lvWB%lv_fluxFromWS_Ind_Potable(i,vTstep)    = lvWB%lv_fluxFromWS_Ind_ROPotable(i,vTstep) &
                        +  lvWB%lv_fluxFromWS_Ind_Groundwater(i,vTstep) +  lvWB%lv_fluxFromWS_Ind_SRPandCAP(i,vTstep)

                    lvWB%lv_fluxFromWS_Com_Potable(i,vTstep)    = lvWB%lv_fluxFromWS_Com_ROPotable(i,vTstep) &
                        +  lvWB%lv_fluxFromWS_Com_Groundwater(i,vTstep) +  lvWB%lv_fluxFromWS_Com_SRPandCAP(i,vTstep)

                    ! ----------------------------------------------
                    ! changed from 12 to 13 on 08.30.11 - changed back
                    lvWB%lvf_fluxDemandDeficit(i,vTstep)= gvf_WaterDemand_acft(i,gpi_unmet,2)
                    !
                endif
                ! ----------------------------------------------

                ! ======================== 
                vState_WB_WaterSupply(T%year,i,vTstep)=nint(vState_WB_WaterSupply(T%year,i,vTstep) &
                    + lv_fluxToWS(i,vTstep) &
                    - (lvWB%lv_fluxFromWS_SRPandCAP(i,vTstep) &
                    + lvWB%lv_fluxFromWS_potable(i,vTstep) &
                    + lvWB%lv_fluxFromWS_nonPotable(i,vTstep) &
                    + lvWB%lvf_fluxWStoDirectInjection(i,vTstep) &
                    + lvWB%lv_fluxFromWS_Groundwater(i,vTstep))) 
                !
                ! ---------------------
                ! Update states
                ! -----------------
                !
                call sUpdateState(T,i,vTstep,vState_WB_WaterSupply)
                !
         return
        end subroutine sWSupply
        ! ---------------------
  
        ! -------------------------------
        subroutine sWWTP(T,lvWB,i,vTstep)
            !
            ! ------------------- Types ----------------------
            integer :: i
            integer :: vTstep    
            integer,parameter :: state=12
            !
            real :: lv_fluxWWinRaw(gvi_maxProV,gpi_timeStepWB)
            real :: lvf_sumP1
            ! ================================================
            !

            ! -- Type Constructs ---
            type (waterbalance)lvWB     
            type(parameters)P
            type(runTime)T
            ! ======================
                !
                if(gvl_modelVinzeJohnston)then
                  P%Out_1=gvf_parm_WWtoEffluent(i)
                  P%Out_2=gvf_parm_WWtoIndReuse(i)
                else
                  P%In_1=gvf_parm_WWtoEffluent(i)
                  P%In_2=gvf_parm_WWtoIndReuse(i)
                  P%In_3=0
                  P%In_4=0
                call sProportion(state,i,vTstep,parms_WWTP,initParms,P)
                  gvf_parm_WWtoEffluent(i)=P%Out_1
                  gvf_parm_WWtoIndReuse(i)=P%Out_2
                !
                endif
                ! -------------------------------
                !
                ! called "Raw" befcause we have looped WW from FSP
                 lv_fluxWWinRaw(i,vTstep)=0
                lv_fluxWWinRaw(i,vTstep)=(max(0, 1.-gvf_parm_WWtoRWWTP(i)))*(lvWB%lv_fluxResidentialWW(i,vTstep) &
                     +lvWB%lv_fluxIndWW(i,vTstep)+lvWB%lv_fluxComWW(i,vTstep)) * gpf_WWTPefficiency
                ! -------------------------------

                ! ==========================
                ! 04.05.16 Iwaniec Scenarios
                if(gvi_baseYear <  T%year)then
                  ! All set to 50% for Transformative Zero Waste
                  lvf_sumP1=sum(gvf_parm_WWtoEffluent) / 33
                  if( 0.49 < lvf_sumP1 .and. lvf_sumP1 < 0.51)then
                    gvl_IwaniecScenarios_PPtoCities=.true.
                  endif
                endif
                ! ==========================
                !
                call sRampUpParameters(T%policyYear,gdf_parm_WWtoEFF(i),P%Out_1,P%Out_1)
                !
                lvWB%lv_fluxWWTPToEffluent(i,vTstep)=P%Out_1*lv_fluxWWinRaw(i,vTstep)
                !
10 format(I4,1xI2,1x,3(F10.3,1x))
                ! Flux to industrial re-use ---- check this formulation
                lvWB%lv_fluxWWToIRuse(i,vTstep)=P%Out_2 * lvWB%lv_fluxIndWW(i,vTstep)

                lvWB%lv_fluxWWToSurfaceDischarge(i,vTstep)=lv_fluxWWinRaw(i,vTstep) &
                    - ( lvWB%lv_fluxWWTPToEffluent(i,vTstep)+lvWB%lv_fluxWWToIRuse(i,vTstep) )
                !
                ! -------------------------------------------------------
                !

                ! =================== 
                vState_WB_WWTP(T%year,i,vTstep)= vState_WB_WWTP(T%year,i,vTstep) &
                    + ( lv_fluxWWinRaw(i,vTstep))&
                    - (lvWB%lv_fluxWWTPToEffluent(i,vTstep)+ lvWB%lv_fluxWWToSurfaceDischarge(i,vTstep)  &
                    + lvWB%lv_fluxWWToIRuse(i,vTstep) ) &
                    + lvWB%lv_fluxToWWTPFromIRuse(i,vTstep)
                !

                ! ---------------------
                ! Update states
                ! -----------------
                ! 
                call sUpdateState(T,i,vTstep, vState_WB_WWTP)
                !
          return
         end subroutine sWWTP
        ! -------------------

        ! -----------------------------------------------------------
        subroutine sRampUpParameters(PolicyYear,InGlobal,InLocal,Out)
            ! ------------- Types -----------
            integer :: PolicyYear
            real :: test,InGlobal,InLocal,Out
            real ::fHyperbola
            ! ===============================
                !
                if(gvl_IwaniecScenarios_PPtoAg)then
                      if(0.999999 < InGlobal)then
                       test = InGlobal
                      else
                       test = max(InGlobal, InLocal* fHyperbola(PolicyYear,gvf_YearsToInflection))
                      endif
                    ! -----
                    !
                    InLocal=test
                    !
                    Out = test
                endif
          return
        end subroutine sRampUpParameters
        !
        ! -----------------------------------------------------------
        subroutine sRampDnParameters(PolicyYear,InGlobal,InLocal,Out)
            ! ------------- Types -----------
            integer :: PolicyYear
            real :: test,InGlobal,InLocal,Out
            real ::fInverseHyperbola
            real,parameter :: minOut=0.01
            ! ===============================
                !
                  if(InGlobal < 0.1)then
                   test = InGlobal
                  else
                   test = min(InLocal, InGlobal * (1-fInverseHyperbola(PolicyYear)))
                   if(test < minOut)test=0
                  endif
                    ! -----
                    !
                    Out=test
                    !
          return
        end subroutine sRampDnParameters
        ! ------------------------------
        !
        ! ================================================================================================
        !
        !  Special states
        ! ---------------


        ! Agriculture
        ! ---------------------------------------
        subroutine sAgriculture(T,lvWB,i,vTstep)
            !
            ! ---------------------- Types ----------------------------
            integer :: i
            integer :: vTstep

            real :: lvf_GWtoAgFromModule,lvf_toVadoseFromAgModule
            real :: lvf_toEvapoFromAg
            real :: lvf_Ag_In
            ! =========================================================
            !

            ! -- Type Constructs ---
            type (waterbalance)lvWB
            type(runTime)T
           ! =======================
                !
                call sProviders(T%providers)
                !
                lvf_GWtoAgFromModule=0
                lvf_toVadoseFromAgModule=0
                !
                if(T%atStartOfProviderLoop)then
                  gvf_AddAgVadose=0
                endif
                !
                call sMaskAgriculture(gvl_mask)
                 gvl_mask(22)=.false.
                gvl_mask(23)=.false.
                !
                 lvf_toEvapoFromAg=0
                 lvf_GWtoAgFromModule=0
                 lvf_toVadoseFromAgModule=0
                if(gvl_mask(i))then
                   lvf_Ag_In=0
                  lvf_Ag_In=lvWB%lvf_fluxEffluentToAg(i,vTstep) 
                  !
                  call sAgProvider(T,i,lvf_Ag_In,lvf_GWtoAgFromModule,lvf_toVadoseFromAgModule,lvf_toEvapoFromAg)
                  call sAgDemand(T,i,lvf_Ag_In) ! Used to create the Credit Transfer Sustainability Index
                  !
                  lvWB%lv_fluxGWtoAgIrrigation(i,vTstep)=  lvf_GWtoAgFromModule
                  lvWB%lv_fluxToVadoseFromAg(i,vTstep)=    lvf_toVadoseFromAgModule
                  !
                endif
                !
                ! To the Interface
                gvf_AddAgVadose=gvf_AddAgVadose+lvf_toVadoseFromAgModule
                gvf_AgToVadoseFlux=(gvf_AddAgVadose)

         return
        end subroutine sAgriculture
        ! -------------------------

        ! Power plant
        ! ------------------------------------
        subroutine sPowerPlant(T,lvWB,i,vTstep)
            !
            ! ----------- Types -------------------
            integer :: i
            integer :: vTstep

            real :: lvf_evaporation(gpi_timeStepWB)
            !real :: vK9=0.79
            real :: vK9a = 0.04 ! Loose 4% on it's way to PV
            ! ============================================
            !
            
            ! --- Type Construct ---
            type (waterbalance)lvWB
            type(runTime)T
           ! =======================
            ! 
            if(T%atStartOfProviderLoop)then
              gvf_EffluentPP=0
            endif
            !   Includes some effluent from Agua Fria via Avondale and El Mirage (no peoria?)
            ! 
            ! 03.20.16 DAS
            lvf_evaporation(vTstep)=lvWB%lv_fluxWWtoPaleoVerde(i,vTstep)*vK9a
            !
             gvf_EffluentPP=gvf_EffluentPP+lvWB%lv_fluxWWtoPaleoVerde(i,vTstep)- lvf_evaporation(vTstep)
            !
          return
         end subroutine sPowerPlant
        ! -------------------------

        ! Utilities
        ! ===================================================================================================
        ! -------------------------------------------
        subroutine sBalanceWS(T,lvWB,i,vTstep,lvWS)
            !
            ! --------- Types --------
            integer :: i,j,k
            integer :: vTstep

            real :: lvf_ROreclaimed
            real :: lvf_BWaterNotUsed
            ! ========================
            !

            ! -------- Type Constructs -----------
            type typeWS
             real :: lvf_Reclaimed,lvf_ROreclaimed
            end type typeWS
            type(typeWS) lvWS
            type (waterbalance)lvWB
            type(runTime)T
           ! =====================================
                ! 
                if(2000 < T%year)then
                else
                  if(gvi_startSimulationYear == T%year)then
                   do j = 1,gvi_maxProV,1
                    do k = gpi_lBY,gpi_uBY,1
                      gvf_reclaimedBalance(k,j,vTstep)=0
                      gvf_ROreclaimedBalance(k,j,vTstep)=0
                    end do
                   end do
                  endif
                endif
                
                 lvf_ROreclaimed=0
                if(2000 < T%year)then
                 lvf_ROreclaimed=gvf_ROreclaimedOutput(T%year-1,i,vTstep)
                else
                 lvf_ROreclaimed=gvf_ROreclaimedOutput(T%year,i,vTstep)
                endif
                !
                 lvf_BWaterNotUsed=0
                 gvf_ROreclaimedInput(T%year,i,vTstep)=0
                ! Not all demand have been met
                if(0 < gvf_WaterDemand_acft(i,12,1))then
                 gvf_reclaimedBalance(T%year,i,vTstep)= gvf_reclaimedBalance(T%year,i,vTstep) &
                    + lvf_BWaterNotUsed
                !
                 gvf_ROreclaimedInput(T%year,i,vTstep)= lvf_ROreclaimed
                !
                else
                  ! Both defined in kernel loop ()
                 gvf_ROreclaimedInput(T%year,i,vTstep)= lvf_ROreclaimed
                endif
                !
                if(2000 < T%year)then
                 lvWB%lvf_fluxReclaimedDifference(i,vTstep)=  gvf_reclaimedOutput(T%year-1,i,vTstep)-gvf_reclaimedInput(T%year-1,i,vTstep)
                else
                 lvWB%lvf_fluxReclaimedDifference(i,vTstep)=  gvf_reclaimedOutput(T%year,i,vTstep)-gvf_reclaimedInput(T%year,i,vTstep)
                endif
                 lvWS%lvf_Reclaimed=gvf_reclaimedInput(T%year,i,vTstep)
                 lvWS%lvf_ROreclaimed=gvf_ROreclaimedInput(T%year,i,vTstep)
                !
          return
        end subroutine sBalanceWS
        ! -----------------------

        ! -------------------------------------------
        subroutine sProportionNew(P)
            !
            ! -------------------------------- Types ---------------------
           ! integer :: state

            real:: sum
            ! =================================================
            !

            ! -- Type Construct --
            type(parameters)P
            ! ====================
                !
                if(0 < P%In_1)then
                  if(0 < P%In_2)then
                    if(0 < P%In_3)then
                      if(0 < P%In_4)then
                        sum=P%In_1+P%In_2+P%In_3+P%In_4
                      else
                       sum=P%In_1+P%In_2+P%In_3
                         P%Out_3=P%In_3
                         P%Out_2=P%In_2
                         P%Out_1=P%In_1

                        if(sum < 1)then
                         P%Out_3=1.0 - P%In_1 - P%In_2
                         P%Out_2=P%In_2
                         P%Out_1=P%In_1
                        else if(1 < sum)then
                         P%Out_3=1.0 - P%In_1 - P%In_2
                         P%Out_2=P%In_2
                         P%Out_1=P%In_1
                        endif
                      endif
                    else
                      sum=P%In_1+P%In_2
                         P%Out_2=P%In_2
                         P%Out_1=P%In_1

                        if(sum < 1)then
                         P%Out_2=1.0 - P%In_1
                        else if(1 < sum)then
                         P%Out_2=1.0 - P%In_1
                        endif
                    endif
                  else
                    sum=P%In_1
                      P%Out_1=P%In_1

                    if(sum < 1)then
                      P%Out_1=1.0
                    else if(1 < sum)then
                      P%Out_1=1.0
                    endif
                  endif
                else
                  if(0 < P%In_2)then
                  else
                  endif
                 
                endif
              
            !
          return
        end subroutine sProportionNew
        ! ----------------------------

        ! -------------------------------------------
        subroutine sProportion(state,i,j,count,init,P)
            !
            ! -------------------------------- Types ---------------------
            integer :: i,j
            integer :: count,state

            ! As of this writing, 25 "state variables" in the City Model
            real :: Last_1(gvi_maxProV,25),Last_2(gvi_maxProV,25)
            real :: Last_3(gvi_maxProV,25),Last_4(gvi_maxProV,25)
            real :: sum,sum2,diff,mod
            real :: lvf_init_1(gvi_maxProV,25),lvf_init_2(gvi_maxProV,25)
            real :: lvf_init_3(gvi_maxProV,25),lvf_init_4(gvi_maxProV,25)
            real :: check_sum

            logical :: init
            ! ============================================================
            !

            ! -- Type Construct --
            type(parameters)P
            ! ====================
            !
            !
            ! If no value is assigned to Last_n()i (T=0)then a value must be assigned
            ! -----------------------------------------------------------------------
            ! Start of a new year, zero out
            !
                if(init)then
                  Last_1(i,state)=0
                  Last_2(i,state)=0
                  Last_3(i,state)=0
                  Last_4(i,state)=0
                endif
                sum2=0
                sum2=Last_1(i,state)+Last_2(i,state)+Last_3(i,state)+Last_4(i,state)
                  !
                  P%Out_1=0
                  P%Out_2=0
                  P%Out_3=0
                  P%Out_4=0
                if(0 < sum2)then
                else
                  mod=0
                 if(sum2 < 2)then
                    sum2=0
                   sum2=P%In_1+P%In_2+P%In_3+P%In_4
                  if(1 < sum2)then
                   if(count == 4)mod=(sum2-1) * 0.25
                   if(count == 3)mod=(sum2-1) * (1./3)
                   if(count == 2)mod=(sum2-1) * 0.5
                   if(count == 1)mod=(sum2-1)
                  else
                  endif
                 else

                 endif
                  P%In_1=max(0,P%In_1-mod)
                  P%In_2=max(0,P%In_2-mod)
                  P%In_3=max(0,P%In_3-mod)
                  P%In_4=max(0,P%In_4-mod)
                endif
              !
                sum=0
               sum=P%In_1+P%In_2+P%In_3+P%In_4
              !
              if(0 < sum .and. sum <= 1)then
                 !
                  if(init)then
                    if(j <2)then
                     lvf_init_1(i,state)= P%In_1
                     lvf_init_2(i,state)= P%In_2
                     lvf_init_3(i,state)= P%In_3
                     lvf_init_4(i,state)= P%In_4
                    endif
                  endif
                 !
                 Last_1(i,state)=P%In_1 * (1./sum)
                 Last_2(i,state)=P%In_2 * (1./sum)
                 Last_3(i,state)=P%In_3 * (1./sum)
                 Last_4(i,state)=P%In_4 * (1./sum)
                !
                P%Out_1=P%In_1
                P%Out_2=P%In_2
                P%Out_3=P%In_3
                P%Out_4=P%In_4
              else
                  if(sum < 2)then
                   diff=sum-1.
                  else
                   ! use  initial values (for the year)
                   ! when no other ara avail
                   ! --------------------
                   P%Out_1= lvf_init_1(i,state)
                   P%Out_2= lvf_init_2(i,state)
                   P%Out_3= lvf_init_3(i,state)
                   P%Out_4= lvf_init_4(i,state)
                   !
                  endif
                   ! Create a proportional reduction in each parameter estimate 
                   ! ----------------------------------------------------------
                   P%Out_1=max(0,P%In_1-(Last_1(i,state)*diff))
                   P%Out_2=max(0,P%In_2-(Last_2(i,state)*diff))
                   P%Out_3=max(0,P%In_3-(Last_3(i,state)*diff))
                   P%Out_4=max(0,P%In_4-(Last_4(i,state)*diff))
              endif
              !
              check_sum=P%Out_1+P%Out_2+P%Out_3+P%Out_4
              if(1 < check_sum)goto 10
              !
          return
10      continue
              !
              if(gvl_writeLog)then
               write(7,*)"State= ",state,"Prov= ",i,"Time step=",j," Check sum > 1 in CitiModel"
              endif
              stop
              !      
        end subroutine sProportion
        ! -------------------------


          ! ----------------------------------------------------------
            subroutine sRangeCheck(state,i,j,minValue,maxValue,in,out)
                !
                ! ------- Types ---------
                integer :: state
                integer :: i,j

                real :: minValue,maxValue
                real :: in,out
                ! =======================
                    !
                if(gvl_writeLog)then
                    out=min(maxValue,max(minValue,in))
                    if(nint(out) .ne. nint(in))then
                      if(gvl_writeLog)write(7,*)"Boundary exceeded: ", state," provider= ",i,"Time step= ",j
                    endif
                endif
                !
             return
            end subroutine sRangeCheck
            ! -------------------------

           ! ------------------------------------------------------------------
            subroutine sWaterFluxToAtmos(lvWB,i,vTstep,lvf_outdoorUse,lvf_flux)
                !
                ! ----------- Types -----------
                integer :: i,vTstep

                real :: lvf_flux,lvf_outdoorUse
                ! =============================
                !

                ! --- Type Constructs ---
                type (waterbalance)lvWB          
                ! =======================
                    !
                    ! ----------------------------------
                    ! From: page 117, Figure 5.15 "Residential End Uses of Water"
                    !       Mayer and DeOreo et al. 1999
                    !              not used as of this time
                     lvf_flux=0
                    lvf_flux=lvf_outdoorUse*mvf_vK2(i)
                    !
                  if(0 < lvf_flux)then
                   lvWB%lv_fluxAtmosTotal(i,vTstep)=lvWB%lv_fluxAtmosTotal(i,vTstep)+lvf_flux
                  endif
                    !
                    ! --------------------------------
                !
           return
          end subroutine sWaterFluxToAtmos
          ! ------------------------------

          ! ----------------------------------------------
            subroutine sUpdateState(T,i,vTstep,stateArray)
                !
                ! ----------------------- Types ----------------------------
                integer :: i,vTstep

                real :: state
                real :: stateArray(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
                ! ==========================================================
                !

                ! -- Type Construct --
                type(runTime)T
                ! ====================
                    !
                     state=0
                    state=max(0,anint(stateArray(T%year,i,vTstep)))
                    if(gpi_timeStepWB /=1)then
                    if(vTstep == 12)then
                      stateArray(T%year+1,i,1)=state
                    else
                      stateArray(T%year,i,vTstep+1)=state
                    endif
                    else
                     stateArray(T%year+1,i,vTstep)=state
                    endif
                  !
             return
            end subroutine sUpdateState
          ! ---------------------------

          ! --------------------------------------
          subroutine sStepUpdate(T,i,vTstep,state)
                !
                ! ----- Types ------
                integer :: i,vTstep
                integer :: state
                ! ==================
                !

                ! -- Type Construct --
                type(runTime)T
                ! ====================
                    !
                    if(gpi_timeStepWB /=1)then
                    else
                      if(state == 2)then
                        ! This variable is NEVER set ---- 02/18/14
                        gvf_GW_Banking(T%year+1,i,1)=gvf_GW_Banking(T%year,i,vTstep)
                      else if(state == 14)then
                        gvf_ROreclaimedOutput(T%year+1,i,1)= gvf_ROreclaimedOutput(T%year,i,vTstep)
                        ! 07.28.11
                        gvf_ROreclaimedInput(T%year+1,i,1)= gvf_ROreclaimedOutput(T%year,i,vTstep)
                        !
                      else if(state == 15)then
                        gvf_reclaimedOutput(T%year+1,i,1)= gvf_reclaimedOutput(T%year,i,vTstep)
                        gvf_reclaimedInput(T%year+1,i,1)= gvf_reclaimedOutput(T%year,i,vTstep)
                      endif
                    endif
                  !
            return
          end subroutine sStepUpdate
          ! ------------------------

          ! ----------------------------------------------------------------------------------------------
          subroutine sGrayWater(T,lvWB,i,vTstep,lvf_indoorWater,lvf_tWater,lvf_BW,lv_flux,lv_providerGPCD, &
            lvf_lost,lvf_unusedGrayWater)
            !
            ! ----------------------- Types ------------------------------
            integer :: i,vTstep
            integer :: model
            integer :: lv_flux(gpi_lBY:gpi_uBY,gvi_maxProV,gpi_timeStepWB)
            integer :: lvi_fluxReclaimedGreywater

            real :: lvf_indoorWater,lvf_tWater,lvf_BW
            real :: lv_GWavailable
            real :: lv_providerGPCD(gvi_maxProV,2)
            real :: lvf_days=0 
            real :: lvf_efficiency=0.97
            real :: lvf_lost,lvf_unusedGrayWater,lvf_timeToImplementation
            !
            real :: fLogistic !,fHyperbola
            !
            real,parameter :: lpf_clothesWasher=0.217 ! Mayer and DeOreo (1999) study average
            ! ============================================================
            !

            ! --- Type Construct ----
            type (waterbalance)lvWB      
            type(runTime) T 
            ! =======================
                !
                if(T%year <= gvi_baseYear)then
                 lvf_timeToImplementation=0
                else
                ! --------------------------------------------------------------------------------------
                ! Special Projects - Scenario's Project with David Iwaniec
                ! 11.21.16
                ! ================================
                  if(lvWB%lvi_ResComInd == 1)then
                    policy=3
                    LCLU=0
                    span=10
                  else if(lvWB%lvi_ResComInd == 2)then
                    policy=4
                    LCLU=0
                    span=10
                  else
                    policy=4
                    LCLU=0
                    span=10
                  endif
                    if(gvl_IwaniecYN)then
                      call sSpanAndInflection(policy,gvi_IwaniecScenario,LCLU,span,gvf_grayWaterCompliance,gvf_YearsToInflection,model)
                    else
                      gvf_grayWaterCompliance=0.9
                      gvf_YearsToInflection=10
                    endif
                  ! End Special Projects
                  ! ====================================================================================================
                  lvf_timeToImplementation=0
                 lvf_timeToImplementation=fLogistic(T%policyYear,gvf_grayWaterCompliance,span,gvf_yearsToAdoptGrayWater)
                endif
                !
                  lvf_lost=0
                  lvf_unusedGrayWater=0
                !
                  lv_GWavailable=0
                 lv_GWavailable=max(0,lvf_indoorWater-lvf_BW) * lvf_efficiency * (1.-lpf_clothesWasher) * lvf_timeToImplementation ! 11.30.15 added the efficiency term
                !
                lvf_lost= (1-lvf_efficiency)* max(0,lvf_indoorWater-lvf_BW)
                lvf_unusedGrayWater=max(0,lvf_indoorWater-lvf_BW-lvf_lost-lv_GWavailable)
                !
                 lvi_fluxReclaimedGreywater=nint(lv_GWavailable)
                  !
                  call daysintheyearK(T%year,T)
                   lvf_days=T%days

                  lv_providerGPCD(i,1)=(lvf_tWater*gpd_galperacft)*(1/lvWB%lv_providerPop(i)) * (1/lvf_days)
                  lv_providerGPCD(i,2)=(lvf_tWater-lv_GWavailable)*gpd_galperacft &
                     *(1/lvWB%lv_providerPop(i))* (1/lvf_days)
                  !
                 if(gvl_IwaniecYN)then
                     if(lvWB%lvi_ResComInd == 1)then
                     else
                        ! Healthy Harvest Hubs, residential Gray Water only
                        if(gvi_IwaniecScenario == 4)then
                         lvi_fluxReclaimedGreywater=0
                        endif
                     endif
                 endif
                    if(T%atStartOfSimulation)lv_flux(T%year,i,vTstep)= lvi_fluxReclaimedGreywater
                  if(1 < gpi_timeStepWB)then
                   if(vTstep == 12)then
                    lv_flux(T%year+1,i,1)= lvi_fluxReclaimedGreywater
                   else
                    lv_flux(T%year,i,vTstep+1)= lvi_fluxReclaimedGreywater
                   endif
                  else
                    lv_flux(T%year+1,i,vTstep)= lvi_fluxReclaimedGreywater
                  endif
                 !
               
            return
          end subroutine sGrayWater
          ! -----------------------

         ! --------------------------------------------------------------------------------------------
          subroutine sBlackWater(lvWB,i,ResComInd,lvf_demand,lvf_nonpotable,lvf_BWused,lvf_BWnotUsed,lvf_deficit,lvf_flushesPerDay)
            !
            ! ----------------- Types -------------------------
            integer :: i,ResComInd

            real :: lvf_BWused,lvf_BWnotUsed
            real :: lvf_deficit,lvf_nonpotable
            real :: lvf_demand
            real,parameter :: lpf_monthsPerYear=12
!            real, parameter :: lpf_peoplePerHouse=2.59
            real :: lvf_flushesPerDay
            real :: lvf_maxFPD=11.05
            real :: lvf_minLPerFlush=3.8 ! 1 gallon per flush
            real :: lvf_maxLPerFlush=30.3 ! 8 gallons per flush
            real :: lvf_tempLPerFlush
            ! =================================================
            !

            ! ---- Type Construct -----
            type (waterbalance)lvWB          
            ! =========================
                !
                 lvf_flushesPerDay=0
                 lvf_BWtypical=0
                !              
                 !mpf_gallonsPerFlush=3.63  !  page 95- Mayer and DeOreo (1999)
                ! mpf_flushesPerDay=5.05    ! std is 3 page 95, Mayer and DeOreo (1999)
                 lvf_tempLPerFlush=0
                !
                if(gvl_parm_toiletPct)then
                    lvf_tempLPerFlush=min(lvf_maxLPerFlush, max(lvf_minLPerFlush,gvf_parm_LitersPFres(i)))

                    if(0 < lvf_tempLPerFlush)mpf_gallonsPerFlush=lvf_tempLPerFlush*1/gpf_gallonsToliters
                      
                    if(0 < gvf_parm_FlushPDres(i))mpf_flushesPerDay=min(lvf_maxFPD,max(0,gvf_parm_FlushPDres(i)))
                      !
                      lvf_BWtypical= lvWB%lv_providerPop(i)*(mpf_gallonsPerFlush*mpf_flushesPerDay*gpf_daysMonth)*1/gpd_galperacft
                else
                    !
                     !mvf_BWpct=0.26696 ! page 88- Mayer and DeOreo (1999)
                    select case(ResComInd)
                     !if(ResComInd == 1)then
                        ! Residential
                      case(1)
                        if(0 < gvf_parm_BWResProp(i))mvf_BWpct=gvf_parm_BWResProp(i)
                      case(2)
                     !else if(ResComInd == 2)then
                        ! Commercial
                        if(0 < gvf_parm_BWCioProp(i))mvf_BWpct=gvf_parm_BWCioProp(i)
                      case(3)
                     !else
                        if(0 < gvf_parm_BWCioProp(i))mvf_BWpct=gvf_parm_BWCioProp(i)
                     !endif
                      case default
                    end select
                  ! 
                  lvf_BWtypical= lvf_demand*mvf_BWpct
                  !
                endif
                !
                ! ==================================================================================================================
                 lvf_flushesPerDay=lvf_BWtypical*gpd_galperacft* ( 1./(gpf_daysMonth*lpf_monthsPerYear*mpf_gallonsPerFlush* lvWB%lv_providerPop(i) ))
                ! ==================================================================================================================

                ! --------------------------------------
                !
                   lvf_BWused=0 ; lvf_BWnotUsed=0; lvf_deficit=0
                if(0 < lvf_nonpotable)then            
                    if(0 < lvf_BWtypical)then
                      if(lvf_BWtypical <= lvf_nonpotable)then
                        lvf_BWused=lvf_BWtypical
                        lvf_BWnotUsed=max(0.,lvf_nonpotable-lvf_BWused)
                      else
                        lvf_BWused=lvf_nonpotable + (lvf_BWtypical-lvf_nonpotable)
                        lvf_deficit=(lvf_BWtypical-lvf_nonpotable)
                      endif
                    else
                    endif
                else
                    lvf_BWused=lvf_BWtypical
                endif
               !
           return
          end subroutine sBlackWater
          ! ------------------------

        ! -----------------------------------------------------
        subroutine sRunoff(RCN,lvf_rainfall,fRunoff,lvf_Storage)
            !
            ! ------ Types --------
            real :: lvf_rainfall
            real :: fRunoff
            real :: lvf_Storage,RCN
            real :: lvc_z=254
            ! =====================
                !
                ! rainfall in mm
                !
                 fRunoff=0.
                 lvf_Storage=0.
                if(0 < RCN)then
                 lvf_Storage = lvc_z *(100 * (1./RCN) -1.)
                else
                 ! Not certain about this...03.27.16
                 lvf_Storage = lvc_z
                endif
                if(0 < lvf_rainfall) &
                 fRunoff = min(lvf_rainfall, max(0, (lvf_rainfall-0.2*lvf_Storage)**2  * (1./ (lvf_rainfall + 0.8*lvf_Storage))))
                !
            !
          return
        end subroutine sRunoff
        ! --------------------

        ! --------------------------------------------
        function fPercolation(lvf_rainfall,lvf_Storage)
            !
            ! ----- Types ------        
            real :: lvf_rainfall
            real :: fPercolation
            real :: lvf_Storage
            real, parameter :: lvc_scale=3,lvc_store=0.05,lvc_slope=0.1
            ! ==================
                !
                ! rainfall in mm, lvf_Storage in mm, fPercolation (mm?)
                !
                 fPercolation=0.
                if(0 < lvf_rainfall) &
                 fPercolation = lvc_scale * (lvf_rainfall / (lvc_store*lvf_Storage + lvc_slope*lvf_rainfall ))
                !
        end function fPercolation
        ! -----------------------

        ! --------------------------------------------------------
        ! 11.19.15 das, modified on 03.31.16
        function fStormWaterStorage(T,lvWB,provider,policyYear,lvf_volumeCapacity_m3,v_Area,lvf_runoff_AF)
            !
            ! -------------------- Types -------------------------        
            integer :: provider,policyYear
            integer:: model
            real :: lvf_runoff_AF,lvf_runoff_m3
            real :: lvf_m3capacity,lvf_AF
            real :: fStormWaterStorage
            real :: lvf_volumeCapacity_m3 ! m3
            real, parameter :: lvc_leakage=0.95
            real, parameter :: lvc_capacity=0.25 ! 25% of peak rainfall..ignores runoff potential and rain water harvested
            !
            real :: v_Area ! m2
            real :: lvf_facilitiesInPlace
            real, parameter :: lpf_squareMeterToSquareMile=0.000000386102
            real, parameter :: lpf_storageFacilities=100 ! for AF, EC
            !
            real :: lvf_capturePCTage,lvf_target_captureAF
            real :: lvf_compliance
            !
            real :: fLogistic !,fHyperbola
            !
            logical :: lvl_bankNotFacility
            !
            type (waterbalance)lvWB
            type(runTime)T
            ! ======================

            ! Algorithm boundaries: 80 m3 to 24,500 m3
            ! ===================================================
                !
                policy=5
                LCLU=0
                span=10
                !
                if(gvl_IwaniecYN)then
                  call sSpanAndInflection(policy,gvi_IwaniecScenario,LCLU,span,lvf_compliance,gvf_YearsToInflection,model)
                else
                endif
                !
                lvf_capturePCTage=fLogistic(policyYear,lvf_compliance,span,gvf_YearsToInflection)
                !
                ! runoff- acre-feet time-step, lvf_Storage in m3
                ! fStormWaterStorage = acre-feet time-step
                ! lvf_runoff_m3 = m3
                ! v_Area = m2
                ! -----------------------
                !
                     fStormWaterStorage=0.
                     lvf_runoff_m3 = 0
                     lvf_AF=0
                !
                ! Peak rainfall amount NOT facility volume driving outputs
                if(lvf_volumeCapacity_m3 <=0)then
                  lvl_bankNotFacility=.true.
                else
                  lvl_bankNotFacility=.false.
                endif
                !
                 fStormWaterStorage =0
                if(lvl_bankNotFacility)then
                ! For the AD and HHH Scenarios
                  !
                  lvf_target_captureAF=lvf_capturePCTage*lvf_runoff_AF*lvc_leakage*lvc_capacity
                  fStormWaterStorage=min(lvf_runoff_AF,lvf_target_captureAF)
                  !
                  if(gvi_IwaniecScenario == 1 .or. gvi_IwaniecScenario == 6)then
                    gvf_WBankingBalance(T%year+1,provider,1)=gvf_WBankingBalance(T%year,provider,1)+fStormWaterStorage
                  else
                    lvWB%lv_fluxComWW(provider,1)=fStormWaterStorage*gvf_parm_WStoCom_prop(provider)
                    lvWB%lv_fluxResidentialWW(provider,1)=fStormWaterStorage*gvf_parm_WStoRes_prop(provider)
                    lvWB%lv_fluxIndWW(provider,1)=fStormWaterStorage*(1-(gvf_parm_WStoCom_prop(provider)+gvf_parm_WStoRes_prop(provider)))
                  endif
                  !
                  fStormWaterStorage=0
                !
                else
                ! For all other Scenarios
                  policy=5
                  LCLU=0
                  span=10
                    if(gvl_IwaniecYN)then
                      call sSpanAndInflection(policy,gvi_IwaniecScenario,LCLU,span,lvf_compliance,gvf_YearsToInflection,model)
                    else
                    endif
                    !
                lvf_facilitiesInPlace=fLogistic(policyYear,lvf_compliance,span,gvf_YearsToInflection)
                !
                     lvf_runoff_m3 = lvf_runoff_AF * (1/gpd_m3Toacft)
                    if(0 < lvf_runoff_m3)then
                       lvf_m3capacity =  lvf_volumeCapacity_m3*v_Area*lpf_squareMeterToSquareMile * lpf_storageFacilities * lvf_facilitiesInPlace
                       lvf_AF = (min(lvf_runoff_m3,lvf_m3capacity)  * lvc_leakage) * gpd_m3Toacft
                       fStormWaterStorage = lvf_AF
                    endif
                ! out: AF timestep-1
                endif
                !

                !
        end function fStormWaterStorage
        ! -----------------------------

        ! --------------------------------
        subroutine sPeakRainfall(i,area_m2,peakRainFall_AF)
        !
        ! --------- types ------
        integer :: i
        real :: peakRainFall_AF
        real :: lvf_unitConversion
        real :: lv_mmTom=0.001
        real :: area_m2
        ! ======================
            !
            lvf_unitConversion=area_m2*lv_mmTom*gpd_m3Toacft
             peakRainFall_AF=0
            peakRainFall_AF=gvf_peakRainfall(i)*lvf_unitConversion
            !
         return
        end subroutine sPeakRainfall
        ! --------------------------------

        ! -----------------------------------------
        subroutine sRainfallRunoff(T,lvWB,i,vTstep)
            !
            ! ----------------------------- Types --------------------------------
            integer :: i,j,k
            integer :: vTstep

            real :: lv_fluxToAtmosRain(gvi_maxProV,gpi_timeStepWB)
            real :: vArea(gvi_maxProV) ! m2

            real :: lv_annualRain(gvi_maxProV,gpi_timeStepWB)
            !real :: lv_monthlyRain(gvi_maxProV,gpi_timeStepWB)
            real :: lv_mmTom=0.001
            !real :: vR1=0.05 ! proportion of runoff leaving cities
            real :: lvf_rainPostEvaporation
            real :: lvf_newRunoff(gvi_maxProV,gpi_timeStepWB)
            real :: lvf_runoffPreStormWater,lvf_runoffPostStormWater
            real :: lvf_xbarRCN(gvi_maxProV),lvf_perc(gvi_maxProV,gpi_timeStepWB)
            real :: lvf_S_mm,lvf_runoff_mm !,lvf_mmConvert
            !
            real :: lvf_stormWaterCapacity
            real :: lvf_annualRainFallData(gvi_maxProV)
            real :: lvf_rawRainfall_AF!,lvf_rainwaterHarvested_AF
            real :: lvf_evapoRate,lvf_evapotrans,lvf_checkEvap
            real :: lpf_unitConversion
            !
            real :: lvf_passRunoff_AF
            real :: fHyperbolaRunoff !,fLogistic
            !
            ! ===================================================================
            !

            ! -- Type Construct ---
            type (waterbalance)lvWB
            type(runTime)T
            ! ======================
                
                !
                 lvf_rainPostEvaporation=0
                 lvf_runoffPreStormWater=0
                 lvf_runoffPostStormWater=0
                 lvf_evapoRate=0
                 lvf_evapotrans=0
                !
                lvf_passRunoff_AF=0
                ! lvf_MetData(i,j,k) array is year,month,met variable;k= 1=Tmax,2=Tmin,3=Precip
                !
                !http://onlinemanuals.txdot.gov/txdotmanuals/hyd/nrcs_runoff_curve_number_methods.htm
                !
                ! Runoff Q = (P-0.2*S)^2 / (P + 0.8*S)
                !
                ! P = precipitation (inches or mm)
                ! S = z * (100/RCN - 1) where z=10 (for inches, 254 for mm)

                ! RCN = Runoff Curve Number
                !
                ! Rainfall array is (year,month,provider)
                do j = 1,gvi_Providers,1
                  do k = 1,gpi_timeStepWB,1
                    lv_annualRain(j,k)=0.
                   ! lv_monthlyRain(j,k)=0.
                  end do
                end do
                !
                ! Provider service area (m2)
                call ProviderArea2012(vArea)
                !
                 lpf_unitConversion=0
                lpf_unitConversion=vArea(i)*lv_mmTom*gpd_m3Toacft
                !
                ! 11.30.15 see immediately below
                ! ---------------
                ! mm year-1
                 lvf_annualRainFallData(i)=gvf_rainFall(i) ! From Demand.f90 line 123 RainHarvesting subroutine
                !
                ! ---------------
                ! Monthly Time-step
                ! =========================
                if(gpi_timeStepWB /=1)then
                 !
                 !
                else
                !
                ! Annual Time-step
                ! ====================
                  !
                   lv_annualRain(i,1)=0 
                    !
                    ! units = mm
                    ! Could, potentially, use monthly rainfall data in the model
                    ! =========================
                    if(1 < gpi_timeStepWB)then
                     do j = 1,12,1
                      lv_annualRain(i,1)= lv_annualRain(i,1)+ lvf_MetData(T%year,j,3)
                     end do
                    else
                      lv_annualRain(i,1)= lvf_annualRainFallData(i)
                    endif
                    !
                     !lvf_convertRain(i,vTstep)=0
                     lvf_runoff_mm=0
                     lvf_rawRainfall_AF=0
                    !
                    lvf_rawRainfall_AF= (lv_annualRain(i,1)*lpf_unitConversion)
                    !
                   ! Hyperbola estimate of evaporation based on rainfall
                    ! 04.28.16
                    lvf_evapoRate=fHyperbolaRunoff(lv_annualRain(i,1))
                    !
                    lvf_rainPostEvaporation=lv_annualRain(i,1)* (1-lvf_evapoRate)
                    !
!                   call ProviderRCN(T,lvWB,i,lvf_xbarRCN) ! Area-weighted (land cover type) Runoff Curve Number
                    call sProviderRCN(T,i,lvf_xbarRCN)
                    !                    
                    call sRunoff(lvf_xbarRCN(i),lvf_rainPostEvaporation,lvf_runoff_mm,lvf_S_mm)
                     !
                     ! Acre-feet annum-1
                     lvf_newRunoff(i,vTstep)=0
                    lvf_newRunoff(i,vTstep)=(lvf_runoff_mm*lpf_unitConversion)
                    lvf_runoffPreStormWater=(lvf_runoff_mm*lpf_unitConversion)
                    !
                     ! lvf_newRunoff(i,vTstep)= lvf_newRunoff(i,vTstep)
                     lvf_passRunoff_AF=lvf_newRunoff(i,vTstep)
                    !
                    ! Estimate stormwater capture - from runoff
                     go_HarvestStormWater_AF(i)=0
                    if(gvl_stormWaterHarvesting)then      
                        call sClearVariables(T,lvWB,i)
                        !
                      if( T%year < gvi_baseYear)then
                      else  
                         lvf_stormWaterCapacity=gii_StormWaterCapacity_m3(i) !100
                        if(gvl_rainWaterHarvestComOnly)then
                          go_HarvestStormWater_AF(i)=nint(fStormWaterStorage(T,lvWB,i,T%policyYear,lvf_stormWaterCapacity,vArea(i),lvf_passRunoff_AF))
                        else
                         ! unit capacity from the interface (default is 100 m3 facility)
                         go_HarvestStormWater_AF(i)=nint(fStormWaterStorage(T,lvWB,i,T%policyYear,lvf_stormWaterCapacity,vArea(i),lvf_passRunoff_AF))
                        endif
                        !
                      endif
                    endif
                    ! Only Commercial and Industrial Use
                    ! Make this more robust next week
                    ! 01.04.17 das
                    go_HarvestStormWater_AF(i)=NINT(go_HarvestStormWater_AF(i)*(gvf_parm_WStoCom_prop(i)+gvf_parm_WStoInd_prop(i) ))
                    !
                    ! Remove storm water capture from the runoff estimate
                    ! AF a-1
                    ! lpf_evapoRealityCheck recognizes that some evaporation and evapotranspiration will occur. I cannot simply
                    ! estimate runoff and percolation and end up with NO evapotranspiration. That that value should be is unknown
                    ! to me at this very moment... I will need to research this.
                    ! 03.27.2016 DAS
                    ! -------------------
                    lvf_newRunoff(i,vTstep)= max(0,lvf_newRunoff(i,vTstep) - go_HarvestStormWater_AF(i))
                    lvf_runoffPostStormWater=max(0,(lvf_runoffPreStormWater-go_HarvestStormWater_AF(i)))
                    ! ------------------
                    !
                    !  outputs of runoff (AF annum-1)
                    lvWB%lvf_runoff(i,vTstep)=lvf_runoffPostStormWater
                    ! ========
                    !
                    lvf_runoff_mm=lvf_runoffPostStormWater * (1./lpf_unitConversion)
                    lv_annualRain(i,1)=max(0,lv_annualRain(i,1)-lvf_runoff_mm)
                    !
                     lvf_perc(i,vTstep)=0
                    lvf_perc(i,vTstep)=fPercolation(lvf_rainPostEvaporation,lvf_S_mm)
                    !
                    ! ----------------------
                    ! Outputs of percolation
                    ! =======
                    !lvWB%lv_runoffPerc(i,vTstep)=((lvf_perc(i,vTstep)*lpf_unitConversion) + go_HarvestStormWater_AF(i) ) 
                    lvWB%lv_runoffPerc(i,vTstep)=((lvf_perc(i,vTstep)*lpf_unitConversion) ) 
                    ! =========
                    !
                    ! Acre-feet annum-1
                    ! lvf_rawRainfall_AF
                    ! 
                     lv_fluxToAtmosRain(i,vTstep)=0.
                    lvf_evapotrans=lvf_evapoRate*lvf_rawRainfall_AF
                    !
                    ! System must balance
                     lvf_checkEvap=0
                    lvf_checkEvap=lvf_rawRainfall_AF-  lvWB%lvf_runoff(i,vTstep)-lvWB%lv_runoffPerc(i,vTstep)-lvf_evapotrans
                    if(0 < lvf_checkEvap)then
                     lvWB%lv_runoffPerc(i,vTstep)= lvWB%lv_runoffPerc(i,vTstep)+lvf_checkEvap
                     !lvf_evapotrans=lvf_evapotrans+lvf_checkEvap
                    else
                     lvWB%lv_runoffPerc(i,vTstep)= max(0,lvWB%lv_runoffPerc(i,vTstep)-lvf_checkEvap)

                     !lvf_evapotrans=max(0,lvf_evapotrans-lvf_checkEvap)
                    endif
                    !

                    lv_fluxToAtmosRain(i,vTstep)=lvf_evapotrans 
                    ! -----------------------
                    ! outputs of evaporation AF annum-1
                    lvWB%lvf_rainfallEvaporation(i,vTstep)= lv_fluxToAtmosRain(i,vTstep)
                    ! =======
                endif
                !

100          format(I4,2x,2(F12.4,1x))
            !
         return
        end subroutine sRainfallRunoff
        ! ----------------------------

        ! --------------------------------------
        subroutine sConverge

         return
        end subroutine sConverge
        ! ------------
        ! --------------------------------------

        ! --------------------------------------
        subroutine sVadoseCurve(fluxIN,fluxOUT)
            !
            ! -------------------- Types -------------------------------
            real :: fluxIN,fluxOUT
            real(8) :: flux
            real,parameter :: lvc_z = 254
            real,parameter :: RCN=65
            real, parameter :: lvc_scale=3,lvc_store=0.05,lvc_slope=0.1
            real, parameter :: AFtomm = 1233481855.3
            real :: lvf_Storage
            ! ==========================================================
                !
                ! mm conversions
                !
                lvf_Storage = lvc_z *(100 * (1.0/RCN) -1.0);
                flux = lvc_scale * ((fluxIN*AFtomm) / (lvc_store*lvf_Storage + lvc_slope*(fluxIN*AFtomm) ))
                fluxOUT=flux * (1./AFtomm)
                !
            !
          return
        end subroutine sVadoseCurve
        ! -------------------------

        ! -----------------------------------
        subroutine sClearVariables(T,lvWB,provider)
            ! --------------- Types ----------
            integer :: provider
            ! ================================
            type (waterbalance)lvWB
            type(runTime)T
            ! ======================
                lvWB%lv_fluxComWW(provider,1)=0
                lvWB%lv_fluxResidentialWW(provider,1)=0
                lvWB%lv_fluxIndWW(provider,1)=0
          return
        end subroutine sClearVariables

        ! =============================================================================================================================
        ! Outputs
        !
        ! -------------------------------------------
        subroutine TypeTransferIN(T,i,vTstep,vPV,lvWB)
            !
            ! --- Types ------
            integer :: i,k
            integer :: vTstep
            real :: lvf_modLeaks
            !real,parameter :: lpf_reduceYears=10
            real,parameter :: one=1
            real, parameter :: start=0.0534
            real :: drawDown = 0.0533
            !
            real :: fLogistic
            !
            ! ================
            !

            ! -- Type Constructs --
            type(waterbalance)lvWB
            type(Provider)vPV
            type(runTime)T
            ! =====================
                !
                  call sMask(gvl_mask)
                !
                ! Define Sensitivity Analyses Provider Variables (parameters) here
                ! 07.12.16 SEN
                ! ==============================
                if(gpl_runSensitivity)then
                    gvf_RateComLeak=gpf_RateComLeak
                    gvf_RateIndLeak=gpf_RateIndLeak
                    gvf_RateResLeak=gpf_RateResLeak
                    !
                    mpf_showersBathPCT=gpf_showersBathPCT
                    mpf_gallonsPerFlush=gpf_gallonsPerFlush
                    mpf_flushesPerDay=gpf_flushesPerDay
                    mvf_BWpct=gpf_BWpercent
                    mvf_vK2= gpf_evapotrans
                endif
                ! ==============================
                !
                lvWB%lv_providerPop(i)=vPV%cPopProvider(i)
                !
                ! 04.08.16 das
                ! ======================
                 lvf_modLeaks=0
                if(gvl_NoleaksYN)then
                 if(gvi_baseYear <= T%year)then
                    span=10
                   lvf_modLeaks= fLogistic(T%policyYear,one,span,gvf_YearsToInflection)
                   gvf_RateResLeak(i)=start- (drawDown*lvf_modLeaks)
                   gvf_RateComLeak(i)=start- (drawDown*lvf_modLeaks)
                   gvf_RateIndLeak(i)=start- (drawDown*lvf_modLeaks)
                 else
                   gvf_RateResLeak(i)=0.0533
                   gvf_RateComLeak(i)=0.0533
                   gvf_RateIndLeak(i)=0.0533

                 endif

                else
                endif
                ! ======================
                !
                ! Water demand on a monthly basis
                if(gpi_timeStepWB /=1)then
                  if(i == 1 .and. vTstep == 1)then
                      !
                  endif
                else
                    ! Annual water demand
                   lvWB%lvf_demand_Res_outdoor_acft_(i,vTstep)=anint(vPV%lvf_cDemand_Res_out_AF_a(i))
                   lvWB%lvf_demand_Res_indoor_acft_(i,vTstep) =anint(vPV%lvf_cDemand_Res_In_AF_a(i))
                   lvWB%lvf_demand_Ind_outdoor_acft_(i,vTstep)=anint(vPV%lvf_cDemand_Ind_out_AF_a(i))
                   lvWB%lvf_demand_Ind_indoor_acft_(i,vTstep) =anint(vPV%lvf_cDemand_Ind_In_AF_a(i))
                   lvWB%lvf_demand_Com_outdoor_acft_(i,vTstep)=anint(vPV%lvf_cDemand_Com_out_AF_a(i))
                   lvWB%lvf_demand_Com_indoor_acft_(i,vTstep) =anint(vPV%lvf_cDemand_Com_In_AF_a(i))
                endif
                  !
                  lvWB%lvf_demand_acft(i,vTstep)=lvWB%lvf_demand_Res_outdoor_acft_(i,vTstep) &
                    +  lvWB%lvf_demand_Res_indoor_acft_(i,vTstep) &
                    +  lvWB%lvf_demand_Ind_outdoor_acft_(i,vTstep) &
                    +  lvWB%lvf_demand_Ind_indoor_acft_(i,vTstep) &
                    +  lvWB%lvf_demand_Com_outdoor_acft_(i,vTstep) & 
                    +  lvWB%lvf_demand_Com_indoor_acft_(i,vTstep)
                  !
                if(gpi_timeStepWB /=1)then
                    
                else
                    !
                    lvWB%lvf_fluxSWaterCAP(i)=(vPV%gvf_cCAP_used_acft_a(i))
                    lvWB%lvf_fluxSWaterSRP_NCS(i)=vPV%gvf_cSRP_NCS_used_acft_a(i)
                    lvWB%lvf_fluxSWaterSRP_A(i)=vPV%gvf_cSRP_A_used_acft_a(i)
                    lvWB%lvf_fluxGWaterSRP(i)=(vPV%gvf_cSRP_BC_used_acft_a(i))  
                    lvWB%lvf_fluxBCstorageSRP(i)=max(0,vPV%gvf_cSRP_BCresAndPump_acft_a(i)-vPV%gvf_cSRP_BC_used_acft_a(i))
                    lvWB%lvf_fluxSWaterSRP(i)=(vPV%gvf_cSRP_A_used_acft_a(i)+vPV%gvf_cSRP_NCS_used_acft_a(i)+lvWB%lvf_fluxBCstorageSRP(i))
                    !                !
                    lvWB%lvf_municipalPumping_acft_(i)=(vPV%gvf_cMunicipalPumping_acft(i))
                    lvWB%lvf_municipalRecharge_acft_(i)=0
                    ! not sure on this....
                    lvWB%lvf_fluxGWaterToWSupply(i,vTstep)=(vPV%gvf_cMunicipalPumping_acft(i))
                    !
                endif
                ! 
                ! 04.20.16 - new data table structure.... indoor and outdoor water use
                ! ----------------------------------------------------------------------------
                lvWB%lvf_excessNonPotable(i)=vPV%lvf_excessNonPotable(i)
                lvWB%lvf_outdoorNonPotable(i)=vPV%lvf_outdoorNonPotable(i)
                lvWB%lvf_outdoorPotable(i)=vPV%lvf_outdoorPotable(i) *gpf_SWTPefficiency
                lvWB%lvf_indoorNonPotable(i)=vPV%lvf_indoorNonPotable(i)
                lvWB%lvf_indoorPotable(i)=vPV%lvf_indoorPotable(i) * gpf_SWTPefficiency
                lvWB%lvf_unmetIndoorPotable(i)=vPV%lvf_unmetIndoorPotable(i)
                lvWB%lvf_unmetOutdoorPotable(i)=vPV%lvf_unmetOutdoorPotable(i)
                !
                lvWB%lvf_rainWaterHarvested(i)=vPV%lvf_rainWaterHarvested(i)
                lvWB%lvf_stormWaterCaptured(i)=vPV%lvf_stormWaterCaptured(i)
                lvWB%lvf_grayWaterRecovered(i)=vPV%lvf_grayWaterRecovered(i)
                !
                ! ----------------------------------------------------------------------------------------------
                ! WaterSim 6 Outputs
                ! 11.21.16
                ! =============================
                go_resIndoorUse=go_resIndoorUse  +((lvWB%lvf_indoorNonPotable(i)+lvWB%lvf_indoorPotable(i))* gvf_parm_WStoRes_prop(i) *(1-gvf_parm_OutDoorResProp(i)))
                go_resOutdoorUse=go_resOutdoorUse+((lvWB%lvf_outdoorNonPotable(i)+lvWB%lvf_outdoorPotable(i))*gvf_parm_WStoRes_prop(i)*gvf_parm_OutDoorResProp(i) )
                go_comIndoorUse=go_comIndoorUse  +((lvWB%lvf_indoorNonPotable(i)+lvWB%lvf_indoorPotable(i))* gvf_parm_WStoCom_prop(i) *(1-gvf_parm_OutDoorComProp(i)))
                go_comOutdoorUse=go_comOutdoorUse+((lvWB%lvf_outdoorNonPotable(i)+lvWB%lvf_outdoorPotable(i))*gvf_parm_WStoCom_prop(i)*gvf_parm_OutDoorComProp(i) )
                go_indIndoorUse=go_indIndoorUse  +((lvWB%lvf_indoorNonPotable(i)+lvWB%lvf_indoorPotable(i))* gvf_parm_WStoInd_prop(i) *(1-gvf_parm_OutDoorIndProp(i)))
                go_indOutdoorUse=go_indOutdoorUse+((lvWB%lvf_outdoorNonPotable(i)+lvWB%lvf_outdoorPotable(i))*gvf_parm_WStoInd_prop(i)*gvf_parm_OutDoorIndProp(i) )
                ! ==============================================================================================
                !
                if(gvl_IncludeMeteorology)then
                  if(T%atStartOfProviderLoop)then
                    do k = 1,gpi_LULC,1
                      lvWB%lvf_LCLU_proportions(k,i)=gvf_waterUseRatioLCLU(k,i)!gvf_LULCProp(k,i)
                    end do
                  endif
                endif
                !
         return
        end subroutine TypeTransferIN
        ! ---------------------------

        ! --------------------------------------------------
        subroutine TypeTransferOUT(T,i,vTstep,lvWB,vEB,vSB)
            !
            ! ------ Types -------
            integer :: i
            integer :: vTstep
            ! ====================
            !

            ! -- Type Constructs --
            type(waterbalance)lvWB
            type(EnergyWB)vEB
            type(SalinityWB)vSB
            type(runTime)T
            ! =====================
                !
                ! Send to Energy_CityModel.f90
                ! ---------------------------------------------------------------------
!               vEB%gv_wSWtoWTP(i,vTstep)=lvWB%lv_fluxSurfaceWaterToSWTP(i,vTstep)
!               vEB%gv_wWTPtoWS(i,vTstep)=lvWB%lv_fluxFromSWTP(i,vTstep)
!               vEB%gv_wGWtoAg(i,vTstep)=lvWB%lv_fluxGWtoAgIrrigation(i,vTstep)
!               vEB%gv_wGWtoWTP(i,vTstep)=lvWB%lv_fluxGWtoWTP(i,vTstep)
!               vEB%gv_wEffluenttoPower(i,vTstep)=lvWB%lv_fluxWWtoPaleoVerde(i,vTstep)
!               !
                vEB%gv_wSRPtoE(vTstep)=lvWB%lv_SRPtoEnergy(vTstep)
                ! Send to TDS_CityModel.f90
!               !---------------------------------------------------------------------------------------
                vSB%gvf_sFluxSWTPtoWS_SW_acft(i,vTstep)    =lvWB%lvf_fluxFromSWTP_WS_acft(i,vTstep)
                vSB%gvf_sFluxWWresToWWTP_acft(i,vTstep)    =lvWB%lv_fluxResidentialWW(i,vTstep)
                vSB%gvf_sFluxWWcioToWWTP_acft(i,vTstep)    =lvWB%lv_fluxIndWW(i,vTstep)+lvWB%lv_fluxCOmWW(i,vTstep)
                vSB%gvf_sFluxWWwwtpToRWWTP_acft(i,vTstep)  =lvWB%lv_fluxToRWWTP(i,vTstep)
                vSB%gvf_sFluxWWtwtpToAg_acft(i,vTstep)     =lvWB%lvf_fluxEffluentToAg(i,vTstep)
!               vSB%gvf_sFluxROToDirectInject(i,vTstep)                =lvWB%lv_fluxFromRevOsToVadose(i,vTstep) 

                vSB%gvf_sFluxRunoffPerc_acft(i,vTstep)     =lvWB%lv_runoffPerc(i,vTstep)
                vSB%gvf_sGwaterToAgToTDS_acft(i,vTstep)    =lvWB%lv_fluxGWtoAgIrrigation(i,vTstep)
                vSB%gvf_sMuniRechargeToTDS_acft(i,vTstep)  =lvWB%lvf_fluxSWaterToGWaterBanking(i,vTstep)
                !  
                vSB%gvf_sFluxRes_acft(i,vTstep)            =lvWB%lv_fluxResidential(i,vTstep)
                vSB%gvf_sFluxResIndoor_acft(i,VTstep)      =lvWB%lv_fluxResIndoor(i,vTstep)
                vSB%gvf_sFluxResOutdoor_acft(i,vTstep)     =lvWB%lv_fluxResOutdoor(i,vTstep)


!               vSB%gvf_sFluxCio_acft(i,vTstep)            =lvWB%lv_fluxInd(i,vTstep)
!               vSB%gvf_sFluxCioIndoor_acft(i,VTstep)      =lvWB%lv_fluxIndindoor(i,vTstep)
!               vSB%gvf_sFluxCioOutdoor_acft(i,vTstep)     =lvWB%lv_fluxIndOutdoor(i,vTstep)

                ! okay
                vSB%gvf_sCAPtoTDS_acft(i,vTstep)            =lvWB%lvf_fluxSWaterCAP(i)
                vSB%gvf_sSRPclassAtoTDS_acft(i,vTstep)      =lvWB%lvf_fluxSWaterSRP_A(i)
                !
                vSB%gvf_sSRPclassBCtoTDS_acft(i,vTstep)     =lvWB%lvf_fluxGWaterSRP(i)
                vSB%gvf_sSRPclassBCstorage_acft(i,vTstep)   =lvWB%lvf_fluxBCstorageSRP(i)
                vSB%gvf_sSRPNCStoTDS_acft(i,vTstep)         =lvWB%lvf_fluxSWaterSRP_NCS(i)
                !
                vSB%gvf_sFluxGWaterBanking(i,vTstep)        =lvWB%lvf_fluxGWaterBankingToSWater(T%year,i,vTstep)
                vSB%gvf_sReclaimed_acft(i,vTstep)           =gvf_reclaimedInput(T%year,i,1)
                vSB%gvf_sROreclaimed_acft(i,vTstep)         =gvf_ROreclaimedInput(T%year,i,1)

                vSB%gvf_sMuniPumpingtoTDS_acft(i,vTstep)    =lvWB%lvf_municipalPumping_acft_(i)
                vSB%gvf_muniPumpTreatedtoTDS_AF(i,vTstep)=lvWB%lvf_fluxGWaterTreatmentToWSupply(i,vTstep)
                !
            !
         return
        end subroutine TypeTransferOUT
        ! ----------------------------

        ! --------------------------------------------
        subroutine sOutputToInterface(T,lvWB,i,vTstep)
            !
            ! ---------- Types -----------
            integer :: i,vTstep,live !,lvi_countSRP
            real :: lvf_rainFallEvapoTrans
            real :: trees
            !
!            real :: lvf_temp_NonPotableOut
!            real :: lvf_temp_NonPotableOutTotal
            real :: lvf_temp_RainToTotal
            real :: lvf_temp_GrayToTotal
            real :: lvf_temp_ReclaimedToTotal
            real :: lvf_addGray,lvf_addRain,lvf_addReclaimed
            real :: lvf_temp,lvf_temp_total
            ! ------------
            !
            ! ==================
            !

            ! - Type Constructs ---
            type(waterbalance)lvWB
            type(runTime)T
            ! =====================
                !
                lvf_rainFallEvapoTrans=0
                trees=0
                lvf_temp=0
                lvf_temp_total=0
                !
                if(T%atStartOfProviderLoop)then
                  go_EffluentAg_acft=0
                  call sMaskSROG(gvl_mask)
                    !
                    lvf_temp_RainToTotal=0
                    lvf_temp_GrayToTotal=0
                    lvf_temp_ReclaimedToTotal=0
                    lvf_addGray=0
                    lvf_addRain=0
                    lvf_addReclaimed=0
                endif
                !
                if(gvl_APIcleared)then
                !
                ! New 03.31.16 das
                lvf_rainFallEvapoTrans  =lvWB%lvf_rainfallEvaporation(i,vTstep)
                trees=lvWB%lvf_LCLU_proportions(i,10)
                ! Send to Demand.f90 for Rainwater Harvesting calculations
                !gvf_LULCProp(2,i)=lvWB%lvf_LCLU_proportions(i,2)

                 ! Add in from Outdoor water use the rainfall evapotransiration
                 ! ------------------------------------------------------------
                gvf_resEvapotrans(i)=gvf_resEvapotrans(i) &
                        + (lvf_rainFallEvapoTrans*lvWB%lvf_LCLU_proportions(i,8)) &
                        + (lvf_rainFallEvapoTrans*(trees* gvf_parm_WStoRes_prop(i)))
                !
                !go_resEvapotrans(i)=nint(gvf_resEvapotrans(i)) ! NOTE- removed from WaterSimDCDC.txt
                ! =====
                gvf_comEvapotrans(i)=gvf_comEvapotrans(i) &
                        + (lvf_rainFallEvapoTrans*lvWB%lvf_LCLU_proportions(i,2)* gvf_parm_WStoCom_prop(i)) &
                        + (lvf_rainFallEvapoTrans*(trees * gvf_parm_WStoCom_prop(i)))
                !
                !go_comEvapotrans(i)=nint(gvf_comEvapotrans(i)) ! NOTE- removed from WaterSimDCDC.txt
                ! =====
                gvf_indEvapotrans(i)=gvf_indEvapotrans(i) &
                        + (lvf_rainFallEvapoTrans*lvWB%lvf_LCLU_proportions(i,2)* gvf_parm_WStoInd_prop(i)) &
                        + (lvf_rainFallEvapoTrans*(trees * gvf_parm_WStoInd_prop(i)))
                !
                !go_indEvapotrans(i)=nint(gvf_indEvapotrans(i)) ! NOTE- removed from WaterSimDCDC.txt
                ! =====
                gvf_otherEvapotrans(i)= &
                        + (lvf_rainFallEvapoTrans*lvWB%lvf_LCLU_proportions(i,3)) &
                        + (lvf_rainFallEvapoTrans*lvWB%lvf_LCLU_proportions(i,4)) &
                        + (lvf_rainFallEvapoTrans*lvWB%lvf_LCLU_proportions(i,5)) &
                        + (lvf_rainFallEvapoTrans*lvWB%lvf_LCLU_proportions(i,6)) &
                        + (lvf_rainFallEvapoTrans*lvWB%lvf_LCLU_proportions(i,7)) &
                        + (lvf_rainFallEvapoTrans*lvWB%lvf_LCLU_proportions(i,11)) &
                        + (lvf_rainFallEvapoTrans*lvWB%lvf_LCLU_proportions(i,12)) 

                !
                !go_otherEvapotrans(i)=nint(gvf_otherEvapotrans(i)) ! NOTE- removed from WaterSimDCDC.txt
                !
                gvf_agEvapotrans = gvf_agEvapotrans + (lvf_rainFallEvapoTrans*lvWB%lvf_LCLU_proportions(i,1))
                !               
                endif
                    ! ===========================================================================
                    if(T%atEndOfProviderLoop)then

                      if(gvl_APIcleared)then
!
                      endif
                    endif
                !
1003            format(I4,2x,I4,2x,I4,2x,42(F10.2,1x))

                !
                if(gpi_timeStepWB /=1)then
                else

                  ! Avoid rounding errors that could make input > output
                  ! 09.01.11 das
                  if(gvf_reclaimedOutput(T%year,i,1) < gvf_reclaimedInput(T%year,i,1)) &
                    gvf_reclaimedInput(T%year,i,1)=gvf_reclaimedOutput(T%year,i,1)

                    go_GWBanked_acft(i)            =nint(gvf_WBankingBalance(T%year,i,1))
                    go_GWBankUsed_acft(i)          =nint(gvf_GW_Banking(T%year,i,1))

                    go_ReclaimedWtotal_acft(i)     =nint(lvWB%lvf_fluxReclaimedTotal(i,vTstep))
                    go_ReclaimedWoutput_acft(i)    =nint(gvf_reclaimedOutput(T%year,i,1))
                    go_ReclaimedWused_acft(i)      =nint(gvf_reclaimedInput(T%year,i,1))
                    !

                    ! Reclaimed water (effluent) to Vadose
                    go_ReclaimedWrecharged_acft(i) =lvWB%lvf_fluxReclaimedToVadose(i,1)
                    go_ReclaimedWdischarged_acft(i)=lvWB%lvf_fluxReclaimedToRWWSDischarge(i,vTstep)
                    go_ReclaimedWDI_acft(i)        =nint(lvWB%lvf_fluxReclaimedToDirectInject(i,vTstep))

                    go_ROReclaimedWoutput_acft(i) =nint(gvf_ROreclaimedOutput(T%year,i,1))
                    go_ROReclaimedWused_acft(i)    =nint(gvf_ROreclaimedInput(T%year,i,1))
                    go_ROReclaimedWDI_acft(i)      =nint(lvWB%lv_ROflux_DirectInject_Potable(i,vTstep))

                    go_Effluent_acft(i)            =nint(lvWB%lv_fluxWWTPToEffluent(i,vTstep))
                    go_EffluentPPlant_acft(i)      =nint(lvWB%lvf_fluxEffluentToPowerPlant(i,vTstep))

                    ! Effluent (WWTP) to Vadose
                    go_EffluentVadose_acft(i)      =nint(lvWB%lv_fluxToVadoseFromEffluent(i,vTstep))
                    !           
                  if(gvl_mask(i))then
                   ! Raw effluent to Ag
                   go_EffluentAg_acft = &
                           go_EffluentAg_acft + nint( lvWB%lvf_fluxEffluentToAg(i,vTstep))

                  endif
                  !
                endif
                ! 02.21.14
                go_TWWTP_acft(i) = nint(lvWB%lv_fluxWWTPToEffluent(i,vTstep)+ lvWB%lv_fluxWWToSurfaceDischarge(i,vTstep)  &
                        + lvWB%lv_fluxWWToIRuse(i,vTstep))
                !
                go_EffluentToDischarge_acft(i) =nint(lvWB%lv_fluxWWToSurfaceDischarge(i,vTstep))
                !
                go_GrayWaterReclaimed_acft(i,1)=nint(lvWB%lv_fluxReclaimedGreywaterRes(i,vTstep))
                go_GrayWaterReclaimed_acft(i,2)=nint(lvWB%lv_fluxReclaimedGreywaterCom(i,vTstep))
                go_GrayWaterReclaimed_acft(i,3)=nint(lvWB%lv_fluxReclaimedGreywaterInd(i,vTstep))
                !
                ! -------------------------
                ! reset variables that get summed over the providers each year
                gvf_AG_Banking(T%year+1)=0
                !
                ! NEW 11.11.15 DAS
                ! ------------------------------
                !
                 go_resGPCDgrayWater(i)=nint(lvWB%lvf_GPCD_res(i,2))
                 go_comGPCDgrayWater(i)=nint(lvWB%lvf_GPCD_com(i,2))
                 go_indGPCDgrayWater(i)=nint(lvWB%lvf_GPCD_ind(i,2))
                !
                ! 01.06.17 das
                ! ------------------------------------------------
                 go_nonpotableUsed(i)=0
!                 go_demand_AG_AF(i)=0
                 go_demand_MD_AF(i)=0
                 go_demand_HD_AF(i)=0
                 go_demand_LD_AF(i)=0
                 go_demand_TurfTree_AF(1,i)=0
                 go_demand_TurfTree_AF(2,i)=0
                 go_demand_TurfTree_AF(3,i)=0
                 !
                if(gvi_baseYear <= T%year)then
                go_nonpotableUsed(i)=lvWB%lvf_outdoorNonPotable(i)+ lvWB%lvf_indoorNonPotable(i)
!                go_demand_AG_AF(i)=gvf_LCLUdemand(1,i)
                go_demand_MD_AF(i)=gvf_LCLUdemand(8,i)
                go_demand_HD_AF(i)=gvf_LCLUdemand(2,i)
                go_demand_LD_AF(i)=gvf_LCLUdemand(13,i)
                go_demand_TurfTree_AF(1,i)=gvf_LCLUdemand(4,i)
                go_demand_TurfTree_AF(2,i)=gvf_LCLUdemand(5,i)
                go_demand_TurfTree_AF(3,i)=gvf_LCLUdemand(10,i)
                endif
                !
                live=i
                if(gvl_APIcleared)then
                    if(21 <live)live=live-2
                    !
                     ! lvf_ratioDemand=lvWB%lvf_demand_acft(i,vTstep)/ sum(gvf_TotalDemand_acft)
                    !
                    if(0 < lvWB%lvf_outdoorPotable(i) .or. 0 < lvWB%lvf_outdoorNonPotable(i))then
                         lvf_temp_total=go_harvRainWaterUsed_AF(i)+go_stormWaterUsedUsed_AF(i)+go_graywaterWaterUsed_AF(i)+gvf_reclaimedUseOutdoors(i)+lvWB%lvf_outdoorPotable(i)
                        lvf_temp=0
                       lvf_temp= go_harvRainWaterUsed_AF(i)
                      if(0 < lvf_temp_total)lvf_temp_RainToTotal= (lvf_temp/lvf_temp_total)
                        lvf_temp=0
                       lvf_temp= go_graywaterWaterUsed_AF(i)
                       if(0 < lvf_temp_total)lvf_temp_GrayToTotal=(lvf_temp/lvf_temp_total)
                        lvf_temp=0
                       lvf_temp=gvf_reclaimedUseOutdoors(i)
                       if(0 < lvf_temp_total)lvf_temp_ReclaimedToTotal=(lvf_temp/lvf_temp_total)
                      !
                      lvf_addRain= lvf_temp_RainToTotal
                      lvf_addGray= lvf_temp_GrayToTotal
                      lvf_addReclaimed= lvf_temp_ReclaimedToTotal
                      !
                    endif
                      !
                      if(gvi_baseYear <= T%year)then
                       go_RainHarvestToTotalOutdoor(i)=NINT(lvf_addRain*100)
                       go_GrayWaterToTotalOutdoor(i)=NINT(lvf_addGray*100)
                       go_ReclaimedToTotalOutdoor(i)=NINT(lvf_addReclaimed*100)
                      endif
                      !
100 format(I4,2x,3(F10.2,1x))
                endif

                ! 08.10.16
                go_agUse=0
                !
                ! ================================
            !
         return
        end subroutine sOutputToInterface
        ! -------------------------------

        ! -------------------------------
!        subroutine sParameterDefaults(T,i,step)
!            ! ----- Types -----
!            integer :: i,j,step
!            ! =================
!            !
!            ! - Type Constructs ---
!            type(waterbalance)lvWB
!            type(runTime)T
!            ! =====================
!                !
!                do j = 1,35,1
!                end do
!
!                !
!            !
!          return
!        end subroutine sParameterDefaults

    ! =====================================================================
    ! ----------------------------------------
    subroutine sCityModelWater(T,i,j,vPV,lvWB)
        !
        ! --- Types ---
        integer :: i,j
        ! =============
        !

        ! -- Type Constructs --
        type(Provider)vPV
        type(waterbalance)lvWB
        type(runTime)T
        ! =====================
            !
            ! ----------------------------------
            if(gvl_IncludeMeteorology)then
              call sRainfallRunoff(T,lvWB,i,j)
            endif
            !
            call LoopEnd(T,i)
            !
             call sSurfaceWater(T,vPV,lvWB,i,j)
               call sSWTP(T,lvWB,i,j)
                call sGWtreatment(T,lvWB,i,j)
                call sWSupply(T,lvWB,i,j)
                 call sResidential(T,lvWB,i,j)
                  call sCommercial(T,lvWB,i,j)
                   call sIndustrial(T,lvWB,i,j)
                    call sWWTP(T,lvWB,i,j)
                     call sIndustrialReuse(T,lvWB,i,j)
                    call sRWWTP(T,lvWB,i,j)
                   call sROprocess(T,lvWB,i,j)
                  call sReclaimed(T,lvWB,i,j)
                 call sEffluent(T,lvWB,i,j)
                call sDirectInjection(T,lvWB,i,j)
               call sAgriculture(T,lvWB,i,j)
              call sVadose(T,lvWB,i,j)
             call sAquifer(T,lvWB,i,j)
             !
            call sPowerPlant(T,lvWB,i,j)
            call sCreditBalance(T,lvWB,i)            
            call sOutputToInterface(T,lvWB,i,j)
           ! ----------------------------------
         !
      return
    end subroutine sCityModelWater
    ! ----------------------------

    ! ----------------------------
    subroutine sCreditBalance(T,lvWB,i)
        !
        ! ------ Types -----
        integer :: i
        real :: temp
        real:: add,subtract
   !     real :: lvf_ratio
        ! ==================
        !

        ! - Type Construct -
        type(runTime)T
        type(waterbalance)lvWB
        !===================
            !
            ! 05.09.12,05.10.12, 08.16.13,06.18.15,06.19.15
            ! So, added water and "other" water are NOW added to the total balance bucket
            ! ============================================================================
            !
            ! gvf_annualGWStored_acft(i) = V%SW + V%effluent + V%direct injection
            ! gvf_otherAnnualGWcredits_acft(T%year,i) = Third column in GWdesignations.txt file, or
            !  grandfather rights, Irrig district GW for Urban, CAGRD, and waterlogged Pumping
            ! gvf_addedAnnualGWcredits(i) = column 1 
            !  that cities get from recharge operations and other transfers.
            ! 
            ! 10.11.15 moved go_WaterFromAgSurface_acft_a(i) from "new supplies" (i.e., Augmentation) 
            ! to here. This needs re-evaluating. Removed on 10.12.15
            ! -------------------------------------------------------------
            !lvWB%lvf_fluxGWaterSRP(i)
            ! 
            temp=0
                if(2000 < T%year)then
                 temp=gvf_EffluentToAg_diff(T%year-1)*gvf_AgCreditWeight(T%year,i)
                endif

              add=0
             add=gvf_annualGWStored_acft(i)+gvf_incidentalCredit(T%year,i)+gvf_addedAnnualGWcredits(i) &
                + gvf_otherAnnualGWcredits_acft(T%year,i)+gvf_addGroundwaterFromAg(i) + temp
             !   + go_WaterFromAgSurface_acft_a(i)
            ! 09.30.15 temp
              subtract=0
             subtract=gvf_usedGWater_acft(i)+lvWB%lvf_fluxGWaterSRP(i) 
            if(T%startyear < T%year)then
              gvd_CreditModel_acft(T%year,i)= max(0, gvd_CreditModel_acft(T%year-1,i)- subtract +add )
            else
              gvd_CreditModel_acft(T%year,i)= gvd_CreditModel_acft(T%year,i)- subtract +add
            endif

            gvd_CreditModel_acft(T%year+1,i)= gvd_CreditModel_acft(T%year,i)
            ! Send to the interface
            go_totalCredits_acft(i)=idint(gvd_CreditModel_acft(T%year,i))
            !
100 format (I4,2x,2(F9.2,1x))
        !
      return
    end subroutine sCreditBalance
    ! ---------------------------

    ! ------------------------
    subroutine outputs(T,lvWB)
        !
        ! -- Type Constructs --
        type(waterbalance)lvWB
        type(runTime)T
        !======================
            !
                !
                vState_GWmodel_maf(T%year)=vState_GWmodel_maf(T%year)+(gvf_naturalRecharge_acft(T%year)*gpd_acftTomaf)
                !
            ! ----------------------------------------------
            if(T%year < gvi_endSimulationYear)then
                vState_GWmodel_maf(T%year+1)=vState_GWmodel_maf(T%year)
            else
            endif
        !
      return
    end subroutine outputs
    ! --------------------
    !
End Module lms_CitiWaterBudgets
!
    ! ----------------------------
    subroutine LoopEnd(T,i)
      use gm_ModelControl
        ! ------ Type ----
        integer:: i
        type(runTime)T
        ! ================

       if(i == 35)T%atEndOfProviderLoop=.true.
      return
    end subroutine LoopEnd
    ! ----------------------------
    ! ----------------------------
    subroutine LoopStartEnd(T,i)
        use gm_ModelControl
        ! ------ Type ----
        integer:: i
        type(runTime)T
        ! ================
        !
        T%atStartOfProviderLoop=.false.
        T%atEndOfProviderLoop=.false.
        if(i == 1)T%atStartOfProviderLoop=.true.
        if(i == 35)T%atEndOfProviderLoop=.true.
        !
      return
    end subroutine LoopStartEnd
    ! ----------------------------

    ! ------------------------------
    subroutine pCitiModel(T,vPV)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        use gm_TypeControl
        !
        ! ---------------- Types ------------------
         integer :: i,j
         real :: effluent
         logical, parameter :: lvl_Salinity=.false.
        ! =========================================
        !

        ! - Type Constructs ----
         type(Provider)vPV
         type(waterbalance)lvWB
         type(EnergyWB)vEB
         type(SalinityWB)vSB
!         type(Energy)vEO
         type(runTime)T
        ! ======================
        !
            ! -------------------------------------------------
                !
                Citi_newYear=.true.
                initParms=.true.
                lvl_firstLoop=.true.
                T%atStartOfProviderLoop=.true.
               do i = 1,gvi_Providers,1
                  !
                  do j= 1,gpi_timeStepWB,1
                    call TypeTransferIN(T,i,j,vPV,lvWB)
                    call sCityModelWater(T,i,j,vPV,lvWB)
                     lvl_firstLoop=.false.
                    !
                    if(lvl_Salinity)then
                     call TypeTransferOUT(T,i,j,lvWB,vEB,vSB)
                    endif
                    !
                  end do
                  !
                  T%atStartOfProviderLoop=.false.
                  !
               end do
                !
                effluent = sum(lvWB%lvf_fluxEffluentToAg)
                call sAgProductionStatic(T,effluent)

                ! 03.01.17 DAS
                do i = 1,gvi_Providers,1
                 go_WaterFromAgPumping_acft_a(i)=0
                go_WaterFromAgPumping_acft_a(i)=go_pumpingAgTotal_AF(T%year)*gvf_relativeAgArea(i)
                ! Move here? 03.02.17 
                !gvf_AgAFGWTotal(T%year,i)=go_pumpingAgTotal_AF(T%year)*gvf_AgCreditWeight(T%year,i)
                end do

                 Citi_newYear=.false.
                 initParms=.false.
                !
                call outputs(T,lvWB)
                !
                if(lvl_Salinity)then
                   do i = 1,gvi_Providers,1
                     do j= 1,gpi_timeStepWB,1
!                      call sCityModelSalinity(T,i,j,vSB)
                     end do
                   end do
                endif
                !
        ! ----------------------------------------------------
        !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=22
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
        !
      return
    end subroutine pCitiModel

    ! ------------------------
       ! -------------------------------------
        function fRainEvaporationRate(lvf_input) result (temp)
            ! --------- types ----------
            real :: lvf_input
            !real :: fRainEvaporationRate
            real :: lpf_hyperbola_a=200
            real :: lpf_hyperbola_b=0.014
            real :: temp
            ! Todd and Mays (3rd edition)
            ! Data adapted from page 25, Table 1.6.1 
            ! Units m3 year-1
            ! ==========================
                !
                !fRainEvaporationRate = 1 - (lvf_input / (lpf_hyperbola_a + lpf_hyperbola_b*lvf_input))*0.01
                temp = 1-(lvf_input / (lpf_hyperbola_a + lpf_hyperbola_b*lvf_input))*0.01
                !
            !
        end function fRainEvaporationRate
        ! -----------------------------

 ! 
! ======================================================================================================
! E.O.F. - Water_CityModel.f90

