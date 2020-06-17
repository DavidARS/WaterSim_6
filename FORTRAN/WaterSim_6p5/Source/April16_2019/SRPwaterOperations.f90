!
! File is SRPrelease.f90
!
! This module estimates release from the SRP system of 6 reservoirs
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
! Module:       Module lms_SRPwaterOperations
! Subroutines:  subroutine classA(T,gvi_order,Aout) 
!               subroutine NCS(T,gvi_order,Aout)
!
! No Module:    subroutine pSRP_newClassA(T,gvi_order,Aout)
!               subroutine pSRP_newClassBC(T,gvi_order,Aout)
!               subroutine pSRP_newNCS(T,gvi_order,Aout)
!
!
!
!
!
! File created on: ?
!
!
! Last write was: 01.22.13,07.18.14, 09.09.14
! -------------------------------------------
!

! ======================================================================================================
!
Module lms_SRPwaterOperations
 use gm_ModelControl
  use gm_GlobalData
   use gm_TypeControl
        !
        ! -----
        include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
        !

            ! --- Module Global Type ---
            real :: mvf_storageCapacity
            ! ===============================
    !
   contains
    !
    ! -----------------------------------
      subroutine classA(T,gvi_order,Aout)
        !   
        ! --------------- Types ----------------
        integer(1) :: gvi_order
        integer :: i,lvi_count

        real :: lvf_WaterDemand35(gvi_maxProV)
        ! ======================================
        !

        ! - Type Constructs -
        type(runTime)T
        type(RiverA)Aout
        ! ===================
          !
          lvi_count=1
            !
            ! this subroutine was moved here on 01.03.13
            ! Found in Designations_STV.f90
            call classAdesignations_A(T,Aout)
            !
              call sMaskSRP(gvl_maskSRP)
            if(gpl_DemandDriven)then
              !
              do i = 1,gvi_Providers,1
                lvf_WaterDemand35(i)=0
                gvf_UsedDemand_A_acft(i)=0
               if(gvl_maskSRP(i))then      
                  ! changed on 05.18.11 according to RAY
    !                     07.28.11 das 
                !
                ! 02.07.12 DAS on project demand (array block 3)
                lvf_WaterDemand35(i)=gvf_WaterDemand_acft(i,gvi_order,3)*gpd_acftTomaf
                !
                if(lvf_WaterDemand35(i) <= Aout%gvf_ClassAarray_10_maf(lvi_count))then
                  gvf_UsedDemand_A_acft(i)=lvf_WaterDemand35(i)* (1./gpd_acftTomaf)
                else
                    if(1 <= Aout%gvf_ClassAarray_10_maf(lvi_count)* (1./gpd_acftTomaf)) &
                  gvf_UsedDemand_A_acft(i)=Aout%gvf_ClassAarray_10_maf(lvi_count)* (1./gpd_acftTomaf)
                endif
                !
                lvi_count=lvi_count+1
               endif
              end do  

            else
              do i = 1,gvi_Providers,1
               if(gvl_maskSRP(i))then                 
                  gvf_UsedDemand_A_acft(i)=Aout%gvf_ClassAarray_10_maf(lvi_count)* (1./gpd_acftTomaf)
                lvi_count=lvi_count+1
               endif
              end do  
            endif
          !
      return
    end subroutine classA
    ! ---------------------
 
    ! -----------------------------------
      subroutine NCS(T,gvi_order,Aout)
        !
        ! ------------------ Types --------------------
        integer(1) :: gvi_order
        integer :: i,j,lvi_count

        real :: lvf_WaterDemand35(gvi_maxProV)
        real :: lpf_storageCapacityRoosevelt_acft, &
                lpf_storageMinimumRoosevelt_acft
        real(8) :: lvd_NCSthresholdVolume
        real(8) :: lv_state,lvd_pureState
        real :: cvf_NCS(10), lvf_diffNCS(10)
        real :: fRoundToAF
        real, parameter :: cvf_NCSVolume=0.272500
        ! =============================================
        
        ! ------------------------ Data -------------------------------------------------------
        ! ch=10%,gl=10%,me=15%,ph=50%,sc=10%,te=5%
        ! Maximum available=272,500 acre-feet
        data cvf_NCS(1),cvf_NCS(2),cvf_NCS(3),cvf_NCS(4),cvf_NCS(5),cvf_NCS(6) &
            ,cvf_NCS(7),cvf_NCS(8),cvf_NCS(9),cvf_NCS(10) /0,0.1,0,0.1,0.15,0,0.5,0.1,0.05,0/
        ! =====================================================================================
        !

        ! - Type Constructs --
        type(RiverA)Aout
        type(runTime)T
        ! ====================
          !
            ! 
            lpf_storageCapacityRoosevelt_acft=li_rooseveltmax*1/gpd_acftTomaf
            lpf_storageMinimumRoosevelt_acft=0.78604*((li_deadpoolSVT* 1/gpd_acftTomaf) - 22502)
            !
            do i = 1,gvi_Providers,1
              lvf_WaterDemand35(i)=0
              gvf_Used_NCS_acft(i)=0
            end do
            !
             lv_state=0
            lv_state=lvd_State_Roosevelt_acft(T%year)
            !
             lvd_pureState=0
            lvd_pureState=lv_state 
            !
            lvd_NCSthresholdVolume=lpf_storageCapacityRoosevelt_acft-(cvf_NCSVolume*1e6)
            !
              do i = 1,10,1
               lvf_diffNCS(i)=0
               Aout%gvf_allocateNCS_acft(i)=0
              end do
              !
            if(lvd_pureState  <= lpf_storageMinimumRoosevelt_acft)then
              !
              ! State at dead pool
              ! ========================
              !
            else
              ! Units NOW in AF as of 04.02.13
                ! ------------------------------------------------------------
              if(lvd_pureState  >= lvd_NCSthresholdVolume)then
                if(lvd_pureState  >= lpf_storageCapacityRoosevelt_acft)then
                  do j = 1,10,1
                    Aout%gvf_allocateNCS_acft(j)=0
                    Aout%gvf_allocateNCS_acft(j)=cvf_NCSVolume*cvf_NCS(j)

                  end do
                else
                  do i = 1,10,1
                    lvf_diffNCS(i)=0
                   lvf_diffNCS(i)=(lvd_pureState-lvd_NCSthresholdVolume)*cvf_NCS(i)
                    Aout%gvf_allocateNCS_acft(i)=0 
                   Aout%gvf_allocateNCS_acft(i)=lvf_diffNCS(i)
                  end do
                endif
              else
                do i = 1,10,1
                   Aout%gvf_allocateNCS_acft(i)=0
                end do
              endif
              !
            endif 
            !
            if(gpl_DemandDriven)then
              !
               call sMaskSRP(gvl_maskSRP)
              lvi_count=1
              do i = 1,gvi_Providers,1            
                !
                if(gvl_maskSRP(i))then    
                
                 ! 02.07.12 DAS
                 ! conservation space and gatewater can be used off-project
!                 lvf_WaterDemand35(i)=gvf_WaterDemand_acft(i,gvi_order,1)*gpd_acftTomaf
                !
                ! Found this error on 04.01 13 das
                 lvf_WaterDemand35(i)=gvf_WaterDemand_acft(i,gvi_order,1)

                 if(lvf_WaterDemand35(i) < Aout%gvf_allocateNCS_acft(lvi_count))then
                  gvf_Used_NCS_acft(i)=lvf_WaterDemand35(i)
                  Aout%gvf_allocateNCS_acft(lvi_count)=gvf_Used_NCS_acft(i)
                 else
                    gvf_Used_NCS_acft(i)= fRoundToAF(Aout%gvf_allocateNCS_acft(lvi_count))
                 endif
                 lvi_count=lvi_count+1
                endif
                !
              end do  
            endif
            !
        return
      end subroutine NCS
      ! -----------------
!
End Module lms_SRPwaterOperations
!   
    ! -------------------------------------------------
    subroutine pSRP_newClassA(T,gvi_order,Aout)
      use lms_SRPwaterOperations
        !
        ! ------------------- Types --------------------
        integer(1) :: gvi_order
        integer :: i,lvi_count

        real :: code=gpi_classA
        ! ==============================================
        !

        ! - Type Constructs -
        type(RiverA)Aout
        type(runTime)T
        ! ===================
            !
            call classA(T,gvi_order,Aout)
            !
             call sMaskSRP(gvl_maskSRP)
             lvi_count=1
            do i = 1,gvi_Providers,1
              Aout%gvf_Unused_Class_A_acft(i)=0
              Aout%gvf_SRP_classA_acft_35(i)=0
              Aout%gvf_SRP_classA_acft_35(i)=gvf_UsedDemand_A_acft(i)
              ! 07.28.11
              if(gvl_maskSRP(i))then 
                Aout%gvf_Unused_Class_A_acft(i)=(max(0,(Aout%gvf_ClassAarray_10_maf(lvi_count)*1/gpd_acftTomaf) - gvf_UsedDemand_A_acft(i)))
                if(Aout%gvf_Unused_Class_A_acft(i) < 1)Aout%gvf_Unused_Class_A_acft(i)=0

               lvi_count= lvi_count+1
              endif     
            end do
              !
              ! ===========================================================
              !
              !   New code NOW-07.28.11
              ! Proportionalize so that on-project gets a proportion of available

             ! New again-02.07.12, 02.09.12
             ! Only on-project for class A
!             do i = 1,gvi_Providers,1
!                !
!                lvf_on(i)=gvf_UsedDemand_A_acft(i) 
!                lvf_off(i)= 0. 
!                !
!             end do
              !
!             call sCWaccounting(gvi_order,lvf_off,lvf_on,code)
                gvl_InOutDemandArray=.false.
              call sCWaccountingIndoorOut(gvi_order,code,gvf_UsedDemand_A_acft,gvl_InOutDemandArray)

             !
            gvi_order=gvi_order+1
            !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=12
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
            !
     return
    end subroutine pSRP_newClassA
    ! ----------------------------
    

    ! This is ONLY valid if class BandC for SRP follows CAP
    ! Added 02.22.13
    ! STILL testing......!!!!!
    ! ----------------------------------------------------
!    subroutine sSurfaceWaterBanking(T,i,Aout,gtPV)
!      use lms_Designations_A
!        !
!        ! ------------------------- Types ---------------------------
!        integer :: i,j
!
!        real :: lvf_CAPunused(gvi_maxProV),lvf_SRPunused(gvi_maxProV)
!        ! ===========================================================
!        !
!
!        ! - Type Constructs ---
!        type(runTime)T
!        type(RiverA)Aout
!        type(provider) :: gtPV        
!        ! =====================
!            !
!            ! ----------------------------------------------------------------------------------------------
!            ! This code needs moving to above the call to newClassBC in kernel.f90
!            ! -----------------
!              do j = 1,gpi_unmet,1
!                if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_cap)then
!                  lvf_CAP(i)= gvf_WaterDemand_acft(i,j,4) + gvf_WaterDemand_acft(i,j,5)
!                endif
!                 lvf_CAPunused(i)=0
!                 lvf_SRPunused(i)=0
!                lvf_CAPunused(i)= gvf_parm_SWtoWBamtCAP(i)
!              end do
!                  ! 
!                  lvf_SRPunused(i)=Aout%gvf_Unused_Class_BC_acft(i)
!                   !
!                  gtPV%gvf_cSetWBanking_a(i)= 0
!                  gtPV%gvf_pAddToSRPrelease(i)=0
!                if(gvi_WBankingOption(i) == 1)then
!                  if(0 < gvf_parm_SWtoWB(i) .and. gvf_parm_SWtoWB(i) <= 1)then
!                    gtPV%gvf_cSetWBanking_a(i)= gvf_parm_SWtoWB(i)*(lvf_SRPunused(i)+lvf_CAPunused(i))
!                    gtPV%gvf_pAddToSRPrelease(i)=(gvf_parm_SWtoWB(i)*lvf_SRPunused(i)) * gpd_acftTomaf
!                  endif
!                    ! 07.18.11 
!                    gvf_WBankingBalance(T%year+1,i,1)=gvf_WBankingBalance(T%year,i,1)+gtPV%gvf_cSetWBanking_a(i)
!                else
!                  gvf_WBankingBalance(T%year+1,i,1)=gvf_WBankingBalance(T%year,i,1)+gvf_parm_SWtoWBamount(i)
!                  gtPV%gvf_cSetWBanking_a(i)=0.
!                endif
!                  ! Save balance for Kernel.f90 loop
!                  gvf_GW_Banking_Hold(i)=gvf_WBankingBalance(T%year+1,i,1)
!                  !
!      return
!    end subroutine sSurfaceWaterBanking
    ! ---------------------------------

    ! -----------------------------------------------
    subroutine pSRP_newClassBC(T,gvi_order,Aout,gtPV)
     use lms_Designations_A
        !
        ! --------------------------- Types -------------------------------------
        integer(1) :: gvi_order
        integer :: i,lvi_count

        real(8) :: acfta
        real :: lvf_WaterDemand35(gvi_maxProV)
        real :: lvf_WaterUse35(gvi_maxProV)
        real :: lvf_totalSRP_acft(10)
        real :: lvf_takeFromStorage10_acft(10),lvf_takeFromPumping10_acft(10)
        real :: lvf_ratioUsed35(35),lvf_ratioStorage35(35),lvf_ratioPumping35(35)
        real :: fRoundToAF
      
        real :: code=gpi_ClassBC

        logical :: lvl_proportionalUse=.false.
        logical :: lvl_reservoirsFirst=.true.
        ! =======================================================================
        !

        ! --- Type Constructs ---
        type(runTime)T
        type(RiverA)Aout
        type(provider) :: gtPV        
        type(Designations_A)ltDa
        ! =======================
        !
            ! Acre-foot per acre that SRP will provide (depends on reservoir storage)
             acfta=gvd_acftperAcreSVT 
            Aout%gvl_callBandCstorage=.true.
            !
            call aDesignationsClassBCmax(T,acfta,ltDa,Aout)
            call aDesignations_classBC_Storage(T,Aout,acfta)
            Aout%gvl_callBandCstorage=.false.
            !
            lvi_count=1
            call sMaskSRP(gvl_maskSRP)
             do i = 1,gvi_Providers,1
              !
              lvf_WaterDemand35(i)=0
              lvf_WaterUse35(i)=0
              Aout%gvf_classBCfromReservoir_acft(i)=0
              Aout%gvf_classBCavailableTopump(i)=0
              lvf_ratioStorage35(i)=0
              lvf_ratioPumping35(i)=0
              !
              !
                ! On-project only
                ! 02.08.12 - this code compares On project water demand to total SRP (reservoir and pumping)
                ! if demand is greater than supply than use is supply.  IF supply is greater than demand,
                ! I take the proportion from the reservoirs and then from pumping and subtract each from the 
                ! initial estimate.  i.e., Release from the reservoirs is reduced (< 3 AF per acre) and pumping is reduced
                !
                ! NO LONGER ASSUMED as of 06.21.12
                ! -----------------------------------------------
              if(gvl_maskSRP(i))then  
                 lvf_totalSRP_acft(lvi_count)=0 
                !
                ! 06.21.12 DAS  
                ! Assume that we take all pumping granted based on reservoir level and that release
                ! from storage makes up the difference.
                ! ------------------------------------
                lvf_takeFromStorage10_acft(lvi_count)=0.
                lvf_takeFromPumping10_acft(lvi_count)=0
                !
                lvf_totalSRP_acft(lvi_count)=Aout%gvf_storageAlloted_35_acft(i)+ Aout%gvf_pumpingSRP_35_acft(i)
                !
                lvf_WaterDemand35(i)=gvf_WaterDemand_acft(i,gvi_order,3) 
                !
                if(0 < lvf_WaterDemand35(i))then
                  if(lvf_WaterDemand35(i) <  lvf_totalSRP_acft(lvi_count))then
                    !
                    lvf_ratioUsed35(i)=0
                    lvf_ratioStorage35(i)=0
                    lvf_ratioPumping35(i)=0
                    ! Simple ratio- 03.27.13

                    ! NOTE: 10.04.16 I have found out that this totalSRP_acft goes to zero for at least one water provider
                    ! I need to now find out why...
                    ! ==============================
                    if(0 < lvf_totalSRP_acft(lvi_count))then
                     lvf_ratioUsed35(i)=lvf_WaterDemand35(i)/lvf_totalSRP_acft(lvi_count)
                     lvf_ratioStorage35(i)=Aout%gvf_storageAlloted_35_acft(i)/lvf_totalSRP_acft(lvi_count)
                     lvf_ratioPumping35(i)=Aout%gvf_pumpingSRP_35_acft(i)/lvf_totalSRP_acft(lvi_count)
                    endif
                    !
                    if(lvl_proportionalUse)then
                        !
                        lvf_takeFromStorage10_acft(lvi_count)=fRoundToAF(Aout%gvf_storageAlloted_35_acft(i)*lvf_ratioUsed35(i))
                        lvf_takeFromPumping10_acft(lvi_count)=fRoundToAF(Aout%gvf_pumpingSRP_35_acft(i)*lvf_ratioUsed35(i))
                        !
                    else
                        if(lvl_reservoirsFirst)then
                            if(Aout%gvf_storageAlloted_35_acft(i) < lvf_WaterDemand35(i))then
                              lvf_takeFromStorage10_acft(lvi_count)=Aout%gvf_storageAlloted_35_acft(i)
                              lvf_takeFromPumping10_acft(lvi_count)= Aout%gvf_pumpingSRP_35_acft(i)
                            else
                              lvf_takeFromStorage10_acft(lvi_count)=lvf_WaterDemand35(i)*lvf_ratioStorage35(i)
                              lvf_takeFromPumping10_acft(lvi_count)=lvf_WaterDemand35(i)*lvf_ratioPumping35(i)
                            endif
                            !
                        else
                            if(Aout%gvf_pumpingSRP_35_acft(i) < lvf_WaterDemand35(i))then
                              lvf_takeFromStorage10_acft(lvi_count)=lvf_WaterDemand35(i)  -Aout%gvf_pumpingSRP_35_acft(i)
                              lvf_takeFromPumping10_acft(lvi_count)=Aout%gvf_pumpingSRP_35_acft(i)
                            else
                              lvf_takeFromStorage10_acft(lvi_count)=0
                              lvf_takeFromPumping10_acft(lvi_count)=lvf_WaterDemand35(i)
                            endif
                        endif
                    endif
                     !
                     lvf_WaterUse35(i)=lvf_WaterDemand35(i)  
                     !
                  else
                    ! Use all available SRP water
                    lvf_WaterUse35(i)= lvf_totalSRP_acft(lvi_count)
                    !
                    lvf_takeFromStorage10_acft(lvi_count)=Aout%gvf_storageAlloted_35_acft(i)
                    lvf_takeFromPumping10_acft(lvi_count)=Aout%gvf_pumpingSRP_35_acft(i)
                    !
                  endif
                     !
                     Aout%gvf_classBCavailableTopump(i)=  lvf_takeFromPumping10_acft(lvi_count)
                     Aout%gvf_classBCfromReservoir_acft(i)= lvf_takeFromStorage10_acft(lvi_count)
                     !
                    ! Send to the interface for output
                    ! Maximum for the year that we can take from storage (reservoir) water
                    go_classBCstorageMax_acft(lvi_count)=0
                   go_classBCstorageMax_acft(lvi_count)=Aout%gvf_storageAlloted_35_acft(i)
                    !
                else
                endif
                !
                ! This is the amount used following demand accounting
                 gvf_tempBCstorageWaterUsed_acft(lvi_count)=0.
                gvf_tempBCstorageWaterUsed_acft(lvi_count)=  Aout%gvf_classBCfromReservoir_acft(i)
                !
                lvi_count=lvi_count+1
                !
              endif
             end do
              !
                ! Moved here on 02.01.13 (from above)- calls designations for BCstorage and normal flows
                ! -----------------------------------
                call dSaltTontoVerde(T,Aout,acfta)
              ! =======================================================
!              do i = 1,gvi_Providers,1
!                lvf_on(i)= lvf_WaterUse35(i)
!                lvf_off(i)=0
!              end do
!             !
!             call sCWaccounting(gvi_order,lvf_off,lvf_on,code)

                gvl_InOutDemandArray=.false.
              call sCWaccountingIndoorOut(gvi_order,code,lvf_WaterUse35,gvl_InOutDemandArray)

             !
            gvi_order=gvi_order+1
             !
          ! ================================================================================================
          lvi_count=1
          call sMaskSRP(gvl_maskSRP)
             do i = 1,gvi_Providers,1
              if(gvl_maskSRP(i))then  
            
                 ! New meaning here too..02.08.12
                  gtPV%gApumpingSRP_maf(T%year,lvi_count)=0
                 gtPV%gApumpingSRP_maf(T%year,lvi_count)=  Aout%gvf_classBCavailableTopump(i)*gpd_acftTomaf
                 !
                 gtPV%gAclassBCdesignationsMax_10_maf(T%year,lvi_count)=Aout%gvf_storageAlloted_35_acft(i)
                !
                lvi_count=lvi_count+1
              endif
             end do                  
            !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=13
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
        !
      return
    end subroutine pSRP_newClassBC
    ! ----------------------------
 
    ! --------------------------------------
    subroutine pSRP_newNCS(T,gvi_order,Aout)
      use lms_SRPwaterOperations
        !
        ! ------------------- Types ---------------------
        integer(1) :: gvi_order
        integer :: i,j
        !
        real :: lvf_usedNCS_acft(gvi_maxProV)
        real :: code=gpi_ncs
        ! ===============================================
        !

        ! --- Type Constructs ---
        type(RiverA)Aout
        type(runTime)T
        ! =======================
            !
            call NCS(T,gvi_order,Aout)
            !
            do j = 1,gvi_Providers,1
                lvf_usedNCS_acft(j)=0
                Aout%gvf_SRP_NCS_acft_35(j)=0
            end do
            do i = 1,gvi_Providers,1
                Aout%gvf_SRP_NCS_acft_35(i)=gvf_Used_NCS_acft(i)
                lvf_usedNCS_acft(i)=gvf_Used_NCS_acft(i)
            end do
            !
!             do i = 1,gvi_Providers,1
!                lvf_proportionOn=0
!                lvf_on(i)=0
!                lvf_off(i)=0
!                !
!                if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
!                    lvf_proportionOn= gvf_WaterDemand_acft(i,gvi_order,3)/gvf_WaterDemand_acft(i,gvi_order,1)
!                endif
!                !
!                lvf_on(i)= lvf_usedNCS_acft(i) *lvf_proportionOn
!                lvf_off(i)=  lvf_usedNCS_acft(i)- lvf_on(i)
!                !
!             end do
             !
!             call sCWaccounting(gvi_order,lvf_off,lvf_on,code)
                gvl_InOutDemandArray=.false.
              call sCWaccountingIndoorOut(gvi_order,code,lvf_usedNCS_acft,gvl_InOutDemandArray)

             !
            gvi_order=gvi_order+1
            !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=15
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
            !
     return
    end subroutine pSRP_newNCS
    ! ------------------------
!
! ======================================================================================================
! E.O.F. SRPwaterOperations.f90
