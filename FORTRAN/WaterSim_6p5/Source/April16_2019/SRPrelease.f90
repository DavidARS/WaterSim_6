!
! File is SRPrelease.f90
!
! This module estimates release from the SRP system of 6 reservoirs
! -----------------------------------------------------------------------------------
!
!      WaterSimDCDC Regional Water Demand and Supply Model Version 5.0

!       This is the Fortran code for the WaterSim_DCDC FORTRAN dll.

!       Copyright (C) 2011 , The Arizona Board of Regents
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
! Module:       Module lms_SRPrelease
! Subroutines:  subroutine nRelease(T,Aout,BandC,wsa)
!               subroutine classA(T,gvi_order,Aout,lvf_WaterDemand35)
!               subroutine NCS(T,gvi_order,Aout,wsa)
!               subroutine aReleaseOut(T,wsa,Aout)
!
! No Module:    subroutine sSRP_classA(T,gvi_order,Aout)
!               subroutine sSRP_NCS(T,gvi_order,Aout)
!               subroutine sSRPrelease(T,Aout,vGP)
!

!
! File created on: ?
!
! david arthur sampson
!
! Last write was: 01.22.13,07.20.14
! ---------------------------------
!

! ======================================================================================================
!
Module lms_SRPrelease
 use gm_ModelControl
  use gm_GlobalData
   use gm_TypeControl
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_5\WaterSimDCDC.txt"
    !

        ! ----------------- Module Global Type Constructs --------
        type watershed_Ar
         real(8) :: lvd_ExpStorage_A_maf_yr(gpi_lBY:gpi_uBY)
         real(8) :: lvd_releaseExcess_A_maf_yr(gpi_lBY:gpi_uBY)
         real(8) :: gvd_allocateNCS_maf_yr(10)
         real(8) :: lvd_NCSmax_maf(10)
         real :: lvf_estSRPrelease_maf_yr(gpi_lBY:gpi_uBY)
         real :: lvf_align
        end type watershed_Ar
        ! ========================================================

        ! --- Module Global Types ---
        real :: mvf_storageCapacity
        ! ===========================
    !
   contains
   !
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

         ! -----------------------------------
         subroutine nRelease(T,Aout,BandC,wsa)
            !
            ! ---------------------------- Types ------------------------------------
            real :: lvf_add_NCS,lvf_add_classA
            real :: lvf_checkState
            real :: lvf_state,lvf_flux,lvf_storage_fluxOut,lvf_newState,lvf_diffState
            real(8) :: fEvaporation,vEvap_a
            real :: BandC
            ! ============================================================================
            !
        
            ! ----- Type Constructs ----
            type(watershed_Ar):: wsa
            type(RiverA)Aout
            type(runTime)T
            ! ==========================
                !
                ! new code as of 01.21.13
                mvf_storageCapacity=gvd_storageCapacitySVT
              !
              vEvap_a=fEvaporation(lv_State_A_maf(T%year))
              !
              ! Scalers must use raw numbers
!               vFlow=0.
!              vFlow=lv_maf_yr(T%year)                         ! annual Flow (maf a-1)
              !
              ! BandC class A and estimated NCS from run-through
                !
                ! Estimated NCS- will be modified to create mass balance here
                !
                 lvf_add_NCS=0
                lvf_add_NCS=sum(Aout%gvf_SRP_NCS_acft_35)

                ! Normal Flow designations
                 lvf_add_classA=0
                lvf_add_classA=sum(Aout%gvf_SRP_classA_acft_35)

               lvf_storage_fluxOut=0
              lvf_storage_fluxOut=(lvf_add_classA+BandC+lvf_add_NCS)*gpd_acftTomaf 
                !
                ! Can be < 0
               lvf_flux=0
              lvf_flux=lv_maf_yr(T%year)-lvf_storage_fluxOut-vEvap_a
                lvf_state=0
              lvf_state=lv_State_A_maf(T%year)
              !
               lvf_newState =0
              lvf_newState=max(0.,lv_State_A_maf(T%year) + lvf_flux)
                !
            !
              if(gpl_DemandDriven)then
                 wsa%lvd_releaseExcess_A_maf_yr(T%year)=0
                 !
                if( lvf_newState  <= li_deadpoolSVT)then
                  !
                  ! State at dead pool
                  ! ========================
                   lv_State_A_maf(T%year)=li_deadpoolSVT
                  !
                else
                 lvf_diffState=0
                 lvf_checkState=0
                    !
                    if(mvf_storageCapacity < lvf_newState)then
                       !
                       lv_State_A_maf(T%year)=mvf_storageCapacity
                       !
                      if(mvf_storageCapacity < lvf_newState)then
                        lvf_diffState=lvf_newState-mvf_storageCapacity
                        lvf_checkState=(abs((lvf_state-lv_State_A_maf(T%year))/lv_State_A_maf(T%year)))*100
                        ! 
                        if(0.01 < lvf_checkState)then
                          wsa%lvd_releaseExcess_A_maf_yr(T%year)=max(0,lvf_flux-lvf_diffState)                   
                        else
                          ! At storage capacity
                          wsa%lvd_releaseExcess_A_maf_yr(T%year)=lvf_diffState
                        endif
                      else
                       wsa%lvd_releaseExcess_A_maf_yr(T%year)=0
                      endif
                      !
                        ! Note: adding Class A as part of release when I have a passThroughNormalFlow variable...?
                        ! Check for double accounting
                      !
                    else
                      !
                      lv_State_A_maf(T%year)=lvf_newState
                      !                   
                    endif

                endif
              else
!                  call basedOnsupply(lvd_NCSthresholdVolume,cvf_NCS,vFlow,vEvap_a,wsa)
              endif
              !
               go_StateSVT=0
              go_StateSVT=NINT(lv_State_A_maf(T%year)*1/gpd_acftTomaf)
               go_spillageSVT=0
              go_spillageSVT=nint(wsa%lvd_releaseExcess_A_maf_yr(T%year)*1/gpd_acftTomaf)

              ! Moved here on 06.22.12
               go_rechargeSVTsrvReach=0
              go_rechargeSVTsrvReach= go_spillageSVT
                ! 04.30.13
              go_deliveriesSVT=0
              gvf_Release=0
              !
30      format(I4,1x,7(F10.7,1x))
              !
100          continue
            return
200         continue  
                if(gvl_writeLog)write(7,*)"Error in SRPrelease.f90: flux < 0"
         end subroutine nRelease
         ! ---------------------

          ! ---------------------------------
          subroutine classA(T,gvi_order,Aout,lvf_WaterDemand35)
            !
            ! ---- Types ---------------------------
            integer(1) :: gvi_order
            integer :: i,lvi_count
            real :: lvf_WaterDemand35(gvi_maxProV)
            ! ======================================
            !

            ! - Type Construct -
            type(runTime)T
            type(RiverA)Aout
            ! ==================
              !
                 lvi_count=1
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
                      gvf_UsedDemand_A_acft(i)=Aout%gvf_ClassAarray_10_maf(lvi_count)* (1./gpd_acftTomaf)
                    endif

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
          ! --------------------

          ! ----------------------------------
          subroutine NCS(T,gvi_order,Aout,wsa)
            !
            ! -------------------- Types ---------------------
            integer(1) :: gvi_order
            integer :: i,j,lvi_count

            real :: lvf_WaterDemand35(gvi_maxProV)
            real :: lvf_flow,vEvap_a
            real, parameter :: cvf_NCSVolume=0.272500
            real(8) :: lvd_NCSthresholdVolume
            real(8) :: fEvaporation
            real(8) :: lv_state,lv_deltaState,lvd_pureState
            real :: cvf_NCS(10), lvf_diffNCS(10)
            ! =================================================

            ! -------------------------- Data ----------------------------------------------------
            ! ch=10%,gl=10%,me=15%,ph=50%,sc=10%,te=5%
            ! Maximum available=
            data cvf_NCS(1),cvf_NCS(2),cvf_NCS(3),cvf_NCS(4),cvf_NCS(5),cvf_NCS(6) &
                ,cvf_NCS(7),cvf_NCS(8),cvf_NCS(9),cvf_NCS(10) /0,0.1,0,0.1,0.15,0,0.5,0.1,0.05,0/
            ! =========================================================================================
            !

            ! -- Type Constructs ---
            type(watershed_Ar):: wsa
            type(RiverA)Aout
            type(runTime)T
            ! ======================
                !
                mvf_storageCapacity=gvd_storageCapacitySVT
                !
                do i = 1,gvi_Providers,1
                  lvf_WaterDemand35(i)=0
                end do
                !
                 lvf_flow=0.
                lvf_flow=lv_maf_yr(T%year)
                !
                 vEvap_a=0
                 wsa%lvd_ExpStorage_A_maf_yr(T%year)=0
                vEvap_a=fEvaporation(lv_State_A_maf(T%year))
                wsa%lvd_ExpStorage_A_maf_yr(T%year)=lv_State_A_maf(T%year)+lvf_flow-vEvap_a
                !
                 lv_state=0
                lv_state=lv_State_A_maf(T%year)
                !sum(Aout%gvf_classBCfromReservoir)
                 lv_deltaState=0
                lv_deltaState=lvf_flow-(vEvap_a+sum(Aout%gvf_ClassAarray_10_maf)+sum(Aout%gvf_classBCfromReservoir)*gpd_acftTomaf)
                 lvd_pureState=0
                lvd_pureState=lv_state+lv_deltaState
                !
                lvd_NCSthresholdVolume=mvf_storageCapacity-cvf_NCSVolume
                !
                do i = 1,10,1
                 wsa%gvd_allocateNCS_maf_yr(i)=0
                end do    
                !
                if(lvd_pureState  <= li_deadpoolSVT)then
                  !
                  ! State at dead pool
                  ! ========================
                  if(lv_State_A_maf(T%year)-vEvap_a-li_deadpoolSVT > 0)then
                   wsa%lvf_estSRPrelease_maf_yr(T%year)=lv_State_A_maf(T%year)-vEvap_a-li_deadpoolSVT
!                   lv_State_A_maf(T%year)=li_deadpoolSVT
                  else
                   wsa%lvf_estSRPrelease_maf_yr(T%year)=0.
!                   lv_State_A_maf(T%year)=li_deadpoolSVT
                  endif
                  !
                else
                  !
                  if(lvd_pureState  >= lvd_NCSthresholdVolume)then
                    if(lvd_pureState  >= mvf_storageCapacity)then
                      do j = 1,10,1
                        wsa%gvd_allocateNCS_maf_yr(j)=0
                       wsa%gvd_allocateNCS_maf_yr(j)=cvf_NCSVolume*cvf_NCS(j)
                      end do
                    else
                      do i = 1,10,1
                       lvf_diffNCS(i)=0
                       lvf_diffNCS(i)=(lvd_pureState-lvd_NCSthresholdVolume)*cvf_NCS(i)
                       wsa%gvd_allocateNCS_maf_yr(i)=lvf_diffNCS(i)
                      end do

                    endif
                  else
                    do i = 1,10,1
                      wsa%gvd_allocateNCS_maf_yr(i)=0
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
                    gvf_Used_NCS_acft(i)=0
                    !
                    if(gvl_maskSRP(i))then     
                     ! 02.07.12 DAS
                     ! conservation space and gatewater can be used off-project
                     lvf_WaterDemand35(i)=gvf_WaterDemand_acft(i,gvi_order,1)*gpd_acftTomaf
                      ! Was this
!                     lvf_WaterDemand35(i)=gvf_WaterDemand_acft(i,gvi_order,3)*gpd_acftTomaf

                     if(lvf_WaterDemand35(i) < wsa%gvd_allocateNCS_maf_yr(lvi_count))then
                      gvf_Used_NCS_acft(i)=lvf_WaterDemand35(i)*1/gpd_acftTomaf
                     else
                      gvf_Used_NCS_acft(i)=wsa%gvd_allocateNCS_maf_yr(lvi_count)*1/gpd_acftTomaf
                     endif
                      wsa%gvd_allocateNCS_maf_yr(lvi_count)= gvf_Used_NCS_acft(i)*gpd_acftTomaf

                     lvi_count=lvi_count+1
                    endif
                  end do  
                else
                  do i = 1,gvi_Providers,1
                   if(gvl_maskSRP(i))then                 
                      gvf_Used_NCS_acft(i)=wsa%gvd_allocateNCS_maf_yr(lvi_count)*gpd_acftTomaf
                    lvi_count=lvi_count+1
                   endif
                  end do  
                endif
                !
            !
           return
          end subroutine NCS
          ! -----------------
       
         ! -------------------------------
         subroutine aReleaseOut(T,wsa,Aout)
          use gm_TypeControl
            !
            ! --- Type Constructs ---
            type(runTime)T
            type(watershed_Ar):: wsa
            type(RiverA)Aout
            ! =======================
              !
              Aout%gvf_releaseExcess_A_maf_yr(T%year)=wsa%lvd_releaseExcess_A_maf_yr(T%year)
              !
            !
          return
         end subroutine aReleaseOut
       ! --------------------------

        ! ------------------------
        subroutine updateState(T)
            !
            !  -- Type Constructs --
            type(runTime)T
            ! ======================
                !
                lv_State_A_maf(T%year+1)=lv_State_A_maf(T%year)
                !
                ! 01.30.13
                ! ------------------
                gvd_acftperAcreSVT=3.0
                if(lv_State_A_maf(T%year+1) < =0.6)gvd_acftperAcreSVT=2.
                ! -----------------------------------------------------
                !
!                write(2,*)T%year,lv_State_A_maf(T%year)
           !
         return
        end subroutine updateState
        ! ------------------------
!
End Module lms_SRPrelease
!
    ! --------------------------------------
    subroutine sSRP_classA(T,gvi_order,Aout)
      use lms_SRPrelease
        !
        ! ---------------------- Types --------------------
        integer(1) :: gvi_order
        integer :: i,lvi_count
        !
        real :: lvf_WaterDemand35(gvi_maxProV)
        real :: lvf_on(gvi_maxProV),lvf_off(gvi_maxProV)
        real :: code=gpi_classA
        ! =================================================
        !
    
        ! - Type Constructs -
        type(RiverA)Aout
        type(runTime)T
        ! ===================
            !
            call classAdesignations_A(T,Aout)
            call classA(T,gvi_order,Aout,lvf_WaterDemand35)
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
             ! New code NOW-07.28.11
              ! Proportionalize so that on-project gets a proportion of available

             ! New again-02.07.12, 02.09.12
             ! Only on-project for class A
             do i = 1,gvi_Providers,1
                !
                lvf_on(i)=gvf_UsedDemand_A_acft(i) 
                lvf_off(i)= 0. 
                !
             end do
              !
             call sCWaccounting(gvi_order,lvf_off,lvf_on,code)
             !
            gvi_order=gvi_order+1
            !
        !
      return
    end subroutine sSRP_classA
    ! ------------------------

    ! -----------------------------------
    subroutine sSRP_NCS(T,gvi_order,Aout)
      use lms_SRPrelease
        !
        ! ----------------------- Types ----------------------
        integer(1) :: gvi_order
        integer :: i
        !
        real :: lvf_on(gvi_maxProV),lvf_off(gvi_maxProV)
        real :: lvf_proportionOn
        real :: code=gpi_ncs
        ! =====================================================
        !

        ! --- Type Constructs ---
        type(RiverA)Aout
        type(runTime)T
        type(watershed_Ar):: ws
        ! =======================
            !
            call NCS(T,gvi_order,Aout,ws)
            !
            do i = 1,gvi_Providers,1
               Aout%gvf_SRP_NCS_acft_35(i)=0
              Aout%gvf_SRP_NCS_acft_35(i)=gvf_Used_NCS_acft(i)
              gvf_UsedDemand_A_acft(i)=gvf_Used_NCS_acft(i)
            end do
            !
            ! =======================================================
             !
             do i = 1,gvi_Providers,1
                lvf_proportionOn=0
              if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
               lvf_proportionOn= gvf_WaterDemand_acft(i,gvi_order,3)/gvf_WaterDemand_acft(i,gvi_order,1)
              endif
                !
                lvf_on(i)=gvf_UsedDemand_A_acft(i) *lvf_proportionOn
                lvf_off(i)= gvf_UsedDemand_A_acft(i)- lvf_on(i)
                !
             end do
             !
             call sCWaccounting(gvi_order,lvf_off,lvf_on,code)
             !
            gvi_order=gvi_order+1
            !
        !
      return
    end subroutine sSRP_NCS
    ! ---------------------

    ! --------------------------------
    subroutine sSRPrelease(T,Aout,vGP)
      use lms_SRPrelease
      ! 
        ! ---------- Types -----
        real :: BandC
        ! ======================
        !

        ! --- Type Constructs --
        type(RiverA)Aout
        type(Provider)vGP
        type(runTime)T
        type(watershed_Ar):: ws
        ! ======================
            !

            ! 10 providers- BandC to Release is full provider list (maf)
            ! From Provider.f90- represents waterBanked
            !
            ! BCfromReservoir would also need to be reduced if we added a function
            ! to keep water in the river (for biological needs). 12.14.11 DAS
            ! Units are in AF for Aout%gvf_classBCfromReservoir
            !
              BandC=0.
             BandC=sum(vGP%gvf_pAddToSRPrelease) +  sum(Aout%gvf_classBCfromReservoir)
            !
            call nRelease(T,Aout,BandC,ws)
            call aReleaseOut(T,ws,Aout)
            call updateState(T)
            !
     return
    end subroutine sSRPrelease
    ! ------------------------
!
! ======================================================================================================
! E.O.F. SRPrelease.f90
