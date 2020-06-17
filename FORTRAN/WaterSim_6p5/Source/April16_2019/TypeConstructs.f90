!
! File is TypeConstructs.f90
!
! This file contains the global type constructs for the FORTRAN model
! ---------------------------------------------------------------------
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
! Module:       Module gm_TypeControl
! Subroutines:  none
! Functions:    none

 
! OUTPUTS: No Outputs
!
! created on 01.15.13
!
! david arthur sampson
!
! last write was: 01.16.13,07.21.14
! ---------------------------------
!

! ======================================================================================================

! -------------------------------------------------------------------------------------
!  Global type controls for the model. Separated by module, these are used to pass data
! among modules.
! ==================================================
    Module gm_TypeControl
      use gm_GlobalData
   
        ! Type constructs used throughout the model
        ! Salt-Tonto and Verde River variables
       type RiverA
        real :: gvf_ClassAarray_10_maf(10)
        real :: gvf_ExpectedStorage_A_maf(gpi_lBY:gpi_uBY)
        real :: gvf_releaseExcess_A_maf_yr(gpi_lBY:gpi_uBY)
        !
        real :: gvf_storageAlloted_35_acft(gvi_maxProV),gvf_BCstorageUseMax_acft
        real :: gvf_classBCmax(10),gvf_classBCfromReservoir_acft(gvi_maxProV)
        real :: gvf_pumpingSRP_35_acft(gvi_maxProV),gvf_SRPdemandDifference_10_acft(10)
        real :: gvf_classBCfromReservoir(gvi_maxProV)
        !
        real :: gvf_SRP_NCSmaxAllocations(10),gvf_classBCavailableTopump(gvi_maxProV)
        real :: gvf_SRP_classA_acft_35(gvi_maxProV),gvf_SRP_NCS_acft_35(gvi_maxProV)
        real :: gvf_Unused_Class_A_acft(gvi_maxProV),gvf_Unused_Class_BC_acft(gvi_maxProV)
        real :: gvf_pumpingSRP_maf
        !
        real :: gvf_verdeToSaltTranClassA_AF,gvf_verdeToSaltTranClassBC_AF
        real :: gvf_overFlowVerde_acft,gvf_saltToVerdeTransfer_acft
        real :: gvf_saltTontoBCused_acft,gvf_verdeBCused_acft,gvf_verdeAused_acft,gvf_overFlowSalt_acft
        real :: gvf_classABCfromSaltTonto_acft,gvf_allocateNCS_acft(10)
        !
        real :: gvf_classAVerdeToSaltMonth_AF(12),gvf_classBCVerdeToSaltMonth_AF(12)
        !
        logical :: gvl_callBandCstorage
       end type RiverA
        !
        ! Colorado River variables
       type RiverB
         real(8) :: bFlow,bCAP_maf
       end type RiverB
        !
        ! General Surface Water variables
       type Surfacewater
         real(8) :: sAcftPerAcre,sCAP_maf,sSVTflow_maf
         real(8) :: sSVTPreRelease_maf,sSRPrelease_maf,sMaricopaSWS_maf,lvd_hold
         real :: sNCSmaxAllocations_maf(10),gvf_holdSW1,gvf_holdSW2
         real :: lvf_unusedClass_BC_acft(gvi_maxProV),gvf_pumpBandC_35_acft(gvi_maxProV)
        End type Surfacewater
        !
        ! Provider Variables
        type Provider
         !
         real(8) :: gAclassBCdesignationsMax_10_maf(gpi_lBY:gpi_uBY,10)
         real(8) :: gAclassBCdesignationsUsed_A_maf(gpi_lBY:gpi_uBY,10)
         real(8) :: oGWpumpingNoSRP_a_maf(gvi_maxProV),oGWrecharge_a_maf(gvi_maxProV)
         real(8) :: oSWandSRPused_a_maf(gvi_maxProV),oSRPpumping_a(gvi_maxProV)

         real    :: gApumpingSRP_maf(gpi_lBY:gpi_uBY,10)
         real    :: gRealizedDesignationCAP_acft(gpi_lBY:gpi_uBY,gvi_maxProV)
         real    :: gCAPMandI_II(gpi_lBY:gpi_uBY)
         !
         real(8) :: oDemandNoAg_acft(gvi_maxProV)
         !
         ! generic- used by Water_CitiModel.f90
         ! -----------------------------------------------------------------------
         real :: gvf_cCAP_used_acft_a(gvi_maxProV)
         real :: gvf_cSRP_A_used_acft_a(gvi_maxProV)
         real :: gvf_cSRP_BC_used_acft_a(gvi_maxProV)
         real :: gvf_cSRP_NCS_used_acft_a(gvi_maxProV)
         real :: gvf_cSRP_BCresAndPump_acft_a(gvi_maxProV)
         !
         ! The annual amount sent to the WB- created in sBanking_s() in Kernel.f90
         real :: gvf_cSetWBanking_a(gvi_maxProV),gvf_cGetWBanking_a(gvi_maxProV)
         ! The annual amount sent to the Vadose and DI as set by sVadoseAndDI_s() in Kernel.f90
         real :: gvf_cVadose_a(gvi_maxProV),gvf_cDirectInjection_a(gvi_maxProV)
         !
         ! annual time-step ---------------------------------------
         real :: lvf_cDemand_Res_out_AF_a(gvi_maxProV)
         real :: lvf_cDemand_Res_In_AF_a(gvi_maxProV)
         !
         real :: lvf_cDemand_Ind_out_AF_a(gvi_maxProV)
         real :: lvf_cDemand_Ind_In_AF_a(gvi_maxProV)
         !
         real :: lvf_cDemand_Com_out_AF_a(gvi_maxProV)
         real :: lvf_cDemand_Com_In_AF_a(gvi_maxProV)

         !
         real :: gvf_cCAP_acft(gvi_maxProV)
         real :: gvf_cMunicipalPumping_acft(gvi_maxProV)
         real :: gvf_cSRPclassBC_acft_monthly(gvi_maxProV,12),gvf_cSRPNCS_acft_monthly(gvi_maxProV,12)
         real :: gvf_pMuniPumping_AF_m(gvi_maxProV,12)
     
         real :: gvf_pAddToSRPrelease(gvi_maxProV)
         !
         real :: cPopProvider(gvi_maxProV)
         real :: lvf_newWaterSupplies_AF_a(gvi_maxProV)
         ! 
         ! WaterSim 6 additions
         ! ---------------------
         real :: lvf_excessNonPotable(gvi_maxProV),lvf_outdoorNonPotable(gvi_maxProV),lvf_outdoorPotable(gvi_maxProV)
         real :: lvf_indoorNonPotable(gvi_maxProV), lvf_indoorPotable(gvi_maxProV)
         real :: lvf_unmetIndoorPotable(gvi_maxProV),lvf_unmetOutdoorPotable(gvi_maxProV)
         real :: lvf_rainWaterHarvested(gvi_maxProV),lvf_stormWaterCaptured(gvi_maxProV),lvf_grayWaterRecovered(gvi_maxProV)

         !
        end type Provider
     !
        type Energy
         real :: gv_eCAP_used_GWhMcum(2,12)
         real :: gv_eCMout(gvi_maxProV,12)
         real :: gv_eSWtoWTP(gvi_maxProV,12),gv_eWTPtoWS(gvi_maxProV,12),gv_eGWtoWTP(gvi_maxProV,12),gv_eGWtoAg(gvi_maxProV,12)
        end type Energy
        !
        type EnergyWB
         real :: gv_wSWtoWTP(gvi_maxProV,12),gv_wWTPtoWS(gvi_maxProV,12)
         real :: gv_wGWtoWTP(gvi_maxProV,12),gv_wGWtoAg(gvi_maxProV,12)
         real :: gv_wEffluenttoPower(gvi_maxProV,12)
         real :: gv_wSRPtoE(12)
        end type EnergyWB
        !
        type Demand
         real(8) :: dag_demand(gpi_lBY:gpi_uBY), dres_demand(gpi_lBY:gpi_uBY),dcio_demand(gpi_lBY:gpi_uBY)
         real(8) :: dpopGrowthFactor
        end type Demand
        !
        ! Salinity Module (not used as of 07.21.14)
        type SalinityWB
          real :: gvf_sCAPtoTDS_acft(gvi_maxProV,12),gvf_sSRPclassAtoTDS_acft(gvi_maxProV,12)
          real :: gvf_sFluxSWTPtoWS_SW_acft(gvi_maxProV,12)
          real :: gvf_sFluxWWresToWWTP_acft(gvi_maxProV,12),gvf_sFluxWWcioToWWTP_acft(gvi_maxProV,12)
          real :: gvf_sFluxWWwwtpToRWWTP_acft(gvi_maxProV,12),gvf_sFluxWWtwtpToAg_acft(gvi_maxProV,12)
          real :: gvf_sSRPNCStoTDS_acft(gvi_maxProV,12),gvf_sSRPclassBCtoTDS_acft(gvi_maxProV,12)
          real :: gvf_sSRPclassBCstorage_acft(gvi_maxProV,gpi_timeStepWB)
          !
          real :: gvf_sFluxGWaterBanking(gvi_maxProV,12)
          real :: gvf_sMuniPumpingtoTDS_acft(gvi_maxProV,12), gvf_muniPumpTreatedtoTDS_AF(gvi_maxProV,12)
          real :: gvf_sReclaimed_acft(gvi_maxProV,12), gvf_sROreclaimed_acft(gvi_maxProV,12)
          !
          real :: gvf_sFluxRes_acft(gvi_maxProV,12),gvf_sFluxResIndoor_acft(gvi_maxProV,12)
          real :: gvf_sFluxResOutdoor_acft(gvi_maxProV,12)
          real :: gvf_sFluxCio_acft(gvi_maxProV,12),gvf_sFluxCioIndoor_acft(gvi_maxProV,12)
          real :: gvf_sFluxCioOutdoor_acft(gvi_maxProV,12)
          real :: gvf_sFluxRunoffPerc_acft(gvi_maxProV,12)
          real :: gvf_sGwaterToAgToTDS_acft(gvi_maxProV,12)
          real :: gvf_sMuniRechargeToTDS_acft(gvi_maxProV,12)
        end type SalinityWB
        !
     End Module gm_TypeControl
!
! ======================================================================================================
! E.O.F. TypeConstructs.f90