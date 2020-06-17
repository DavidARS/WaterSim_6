!
! File is SRPnewRelease.f90
!
! This file controls the release from the SRP reservoirs in the FORTRAN model
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
! Modules:      Module lms_SRPnewRelease    
! Subroutines:  subroutine nRelease(T,Aout,wsa) 
!               subroutine aReleaseOut(T,wsa,Aout)
!
! No Module:    subroutine pSRPnewRelease(T,Aout,vGP)
!

! Created on 01.16.13 
!
! Global OUTPUTS: 
!
! Local OUTPUTS:   No local outputs
!
! Local INPUTS:    Inputs are outputs
!
!
! Last write was: 04.02.13,07.18.14
! ---------------------------------
!

! ======================================================================================================
!
Module lms_SRPnewRelease
 use gm_ModelControl
  use gm_GlobalData
   use gm_TypeControl
        !
        ! -----
        include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
        !

            ! ------------- Module Global Type Constructs ----------
            type watershed_Ar
             real(8) :: lvd_ExpStorage_A_maf_yr(gpi_lBY:gpi_uBY)
             real(8) :: lvd_releaseExcess_A_maf_yr(gpi_lBY:gpi_uBY)
             real(8) :: gvd_allocateNCS_maf_yr(10)
             real(8) :: lvd_NCSmax_maf(10)
             real :: lvf_estSRPrelease_maf_yr(gpi_lBY:gpi_uBY)
             real :: lvf_align
            end type watershed_Ar
            !=======================================================

            ! ----- Module Global Types ----------
            real,private :: mvf_storageCapacity
            ! ====================================
    !
   contains
    !
        ! ------------------------
        subroutine initialize(wsa)

            ! --- types ----
            integer :: yr,i
            ! ==============

            ! -- Type Constructs ---
            type(watershed_Ar):: wsa
            ! ======================
                !
                do yr = gpi_lBY,gpi_uBY,1
                    wsa%lvd_ExpStorage_A_maf_yr(yr)=0
                    wsa%lvd_releaseExcess_A_maf_yr(yr)=0
                    wsa%lvd_releaseExcess_A_maf_yr(yr)=0
                end do
                !
                do i = 1,10,1
                    wsa%gvd_allocateNCS_maf_yr(i)=0
                    wsa%lvd_NCSmax_maf(i)=0
                end do
                !
            !
         return
        end subroutine initialize
       !------------------------------------------------------
        ! 02.07.12
        !    Conservation space water and gatewater can be used off project. All other
        ! water including normal flow must be used on project.
     
       ! 02.02.12
        !   Water from the Reservoir and Pumped water  are treated the same, allocated based 
        ! on the 3 acre feet per acre.  SRP tries to deliver 3 acre feet per acre using a 
        ! combination of  water flow above normal flow rights, water from the reservoir, and
        ! pumped water.   From day to day how much each contributes to the 3 acre feet will 
        ! vary.  When there is not enough from these three to meet 3 acre feet, then SRP
        ! reduces the allocation it delivers to everyone.
     
        ! Ray Quay
        ! Research Professional
        ! Decision Center for a Desert City
        ! Global Institute of Sustainability
        ! Arizona State University
        ! Phone: 480.965.4394
        ! =======================================

         ! -----------------------------
         subroutine nRelease(T,Aout,wsa)
            !
            ! -------------------------- Types -------------------------------------
            real :: lvf_add_NCS
            real :: lvf_state,lvf_flux,lvf_newState
            real :: lpf_storageCapacityRoosevelt_acft,lpf_storageMinimumRoosevelt_acft
            ! ============================================================================
            !

            ! -- Type Constructs ----
            type(watershed_Ar):: wsa
            type(RiverA)Aout
            type(runTime)T
            ! =======================
              !
                lpf_storageCapacityRoosevelt_acft=li_rooseveltmax*1/gpd_acftTomaf
                lpf_storageMinimumRoosevelt_acft=0.78604*((li_deadpoolSVT* 1/gpd_acftTomaf) - 22502)
                 !
                 lvf_add_NCS=0
                lvf_add_NCS=sum( Aout%gvf_SRP_NCS_acft_35)
                !
                  lvf_flux=0
                 lvf_flux=lvf_add_NCS
                !
                ! NOW in AF
                 lvf_state=0
                lvf_state=lvd_State_Roosevelt_acft(T%year)
              !
                 lvf_newState =0
                lvf_newState=max(0.,lvf_state - lvf_flux)
                !
!                lvf_tempState_maf=0
                !
                 wsa%lvd_releaseExcess_A_maf_yr(T%year)=0
                 !
                if( lvf_newState  <= lpf_storageMinimumRoosevelt_acft)then
                  !
                  ! State at dead pool
                  ! ========================
                   lvd_State_Roosevelt_acft(T%year)=lpf_storageMinimumRoosevelt_acft
                  !
                else
                    !
                    if(lpf_storageCapacityRoosevelt_acft < lvf_newState)then
                       !
                       lvd_State_Roosevelt_acft(T%year)=lpf_storageCapacityRoosevelt_acft
                       !
                    else
                        !
                        lvd_State_Roosevelt_acft(T%year)=lvf_newState
                        !
                    endif
                    !
                    wsa%lvd_releaseExcess_A_maf_yr(T%year)=  &
                         (Aout%gvf_overFlowVerde_acft+ Aout%gvf_overFlowSalt_acft)*gpd_acftTomaf
                    !
                endif
              !
30      format(I4,1x,7(F10.7,1x))
              !
100          continue
            return
200         continue  
                if(gvl_writeLog)write(7,*)"Error in SRPrelease.f90: flux < 0"
         end subroutine nRelease
         ! ---------------------
       
         ! -------------------------------
         subroutine aReleaseOut(T,wsa,Aout)
          use gm_TypeControl

            !  -- Type Constructs ----
            type(runTime)T
            type(watershed_Ar):: wsa
            type(RiverA)Aout
            ! ========================
              !
              Aout%gvf_releaseExcess_A_maf_yr(T%year)=wsa%lvd_releaseExcess_A_maf_yr(T%year)
              !
              gvf_Release= Aout%gvf_classABCfromSaltTonto_acft+ Aout%gvf_verdeAused_acft &
                + Aout%gvf_verdeBCused_acft
              !
           return
         end subroutine aReleaseOut
       ! ----------------------------
        
        ! ------------------------
        subroutine updateState(T)
            !
            !  - Type Constructs -
            type(runTime)T
            ! ====================
                !
                lvd_State_Salt_acft(T%year)= lvd_State_Roosevelt_acft(T%year)+ lvd_State_Others_acft(T%year)
                lvd_State_Salt_acft(T%year+1)= lvd_State_Salt_acft(T%year)
                !
                lv_State_A_maf(T%year)=(lvd_State_Salt_acft(T%year)+lvd_State_Verde_acft(T%year))*gpd_acftTomaf
                lv_State_A_maf(T%year+1)=(lvd_State_Salt_acft(T%year)+lvd_State_Verde_acft(T%year))*gpd_acftTomaf
                !
                ! 01.30.13
                ! --------------------
                gvd_acftperAcreSVT=3.0
                if(lv_State_A_maf(T%year+1) < =0.6)gvd_acftperAcreSVT=2.
                ! 09.11.14: Talk by Charlie Ester on 09.09.14 (PERA Club)
                if(lv_State_A_maf(T%year+1) < =0.8)gvd_acftperAcreSVT=2.
               ! -------------------------------------------------------
               !
            !
         return
        end subroutine updateState
        ! ------------------------------

        ! ----------------------------------
        subroutine outputsToInterface(T,wsa)
            !
            !  -- Type Constructs ----
            type(watershed_Ar):: wsa
            type(runTime)T
            ! ========================
                !
                 go_StateSVT=0
                go_StateSVT=NINT(lv_State_A_maf(T%year)*(1./gpd_acftTomaf))
                go_spillageSVT=nint((wsa%lvd_releaseExcess_A_maf_yr(T%year))*1/gpd_acftTomaf)
                go_rechargeSVTsrvReach= go_spillageSVT
                !
            !
          return
        end subroutine outputsToInterface
        ! -------------------------------
!
End Module lms_SRPnewRelease
!
    ! -------------------------------
    subroutine pSRPnewRelease(T,Aout)
      use lms_SRPnewRelease
        !
        ! -- Type Constructs ---
        type(RiverA)Aout
        type(runTime)T
        type(watershed_Ar):: ws
        ! ======================
        !
            !
            if(T%atStartOfSimulation)then
             call  initialize(ws)
            endif
            !
            call nRelease(T,Aout,ws)
            call aReleaseOut(T,ws,Aout)
            call updateState(T)
            call outputsToInterface(T,ws)
            !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=23
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
            !
        !
      return
    end subroutine pSRPnewRelease
    ! ---------------------------
!
! ======================================================================================================
! E.O.F. SRPnewRelease.f90