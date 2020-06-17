!
! File is Designations_SVT.f90
!
! This file contains the calculations of surface water designations for SVT
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

! Module:       Module lm_Designations_A
! Subroutines:  subroutine initAdesignations()
!                 calls:
!                   call openFiles_Adesignations()
!                   call readFiles_Adesignations()
!
! Module:       Module lms_Designations_A       
! Subroutines:  subroutine aClear(T,ltDa) 
!               subroutine aDesignations_classBCmax_A(T,acfta,ltDa,Aout)
!               subroutine aDesignations_classBC_Storage(T,Aout,acfta)
!
! No Module:    subroutine aDesignationsClassBCmax(T,acfta,ltDa,Aout)
!
! Module:       Module lms_Iterate
! Subroutines:  subroutine sIterateBCstorageFinal(T,Aout,acfta)             
!
! No Module:    subroutine pSaltTontoVerde(T,Ain,acfta)
!               subroutine classAdesignations_A(T,Aout)
!               subroutine sSRP_classBC(T,gvi_order,Aout,gtPV)
!

! Local OUTPUTS:
!              
! Local INPUTS:
!
!--------------------------
!
! created on 06.07.10
!
! david arthur sampson

! last write was: 10.17.13,07.20.14
! ---------------------------------
!

! ======================================================================================================
!
Module lm_Designations_A
 use gm_ModelControl
  use gm_GlobalData
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
 contains
    !
    ! ----------------------------
    subroutine initAdesignations()
      call openFiles_Adesignations()
      call readFiles_Adesignations()
     return
    end subroutine initAdesignations
    ! -------------------------------

    ! ----------------------------------
    subroutine openFiles_Adesignations()
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
              module="lm_Designations_A"
              !
               Infile='App_Data\Parameters\SVT_Designations_2012.txt'; LU=40
               call openFiles(module,lvc_DPath,Infile,LU)

               Infile='App_Data\Parameters\SVT_Threshold_2012.txt'; LU=41
               call openFiles(module,lvc_DPath,Infile,LU)

               Infile='App_Data\Parameters\SVTacreageClassA_2012.txt'; LU=42
               call openFiles(module,lvc_DPath,Infile,LU)

               Infile='App_Data\Parameters\SVT_Relative.txt'; LU=44
               call openFiles(module,lvc_DPath,Infile,LU)
            !
        !
      return 
    end subroutine openFiles_Adesignations
    ! -------------------------------------

    ! ----------------------------------
    subroutine readFiles_Adesignations()
        !
        !
        ! ----------------- Types ---------------
!        integer :: LU
        integer :: i,j,ios

        real(8) :: lvd_designationsSVT(42,10)
        ! =======================================
            !
            !---------------------------------------------------------------------
            !
            read(40,*,err=31,iostat=ios)((lvd_designationsSVT(i,j),j=1,10),i=1,42)
            read(41,*,err=32,iostat=ios)((lid_thresholddesigSVT(i,j),j=1,2),i=1,42)
            read(42,*,err=33,iostat=ios)(lid_acreageSVT(j),j=1,10)
            read(44,*,err=35,iostat=ios)((lid_providerTrott(i,j),j=1,10),i=1,42)
            !
               lid_designationsSVT=0.
              lid_designationsSVT=lvd_designationsSVT
31          continue
            if(ios >0)then
             LU=40
             goto 1000
            endif
32          continue
            if(ios >0)then
             LU=41
             goto 1000
            endif
33          continue
            if(ios >0)then
             LU=42
             goto 1000
            endif
34          continue
     
35          continue
            if(ios >0)then
             LU=44
             goto 1000
            endif
            ! NOTE: not closed if an err is returned
            do i = 40,44,1
             close(i)
            end do
         return
1000     continue
            !
            if(gvl_writeLog)then
              string=17
                call sStrings(string,errorString)
                call eWrite(errorString,LU)
            endif
         gvl_errorFlag=.false.
         !
    end subroutine readFiles_Adesignations
    ! ------------------------------------
 !
End Module lm_Designations_A
!
    ! ------------------------------------
    subroutine initializeSTVdesignations()
      use lm_Designations_A
        !
        call initAdesignations()
        !
      return
    end subroutine initializeSTVdesignations
    ! --------------------------------------
!
! =================================================================================================
!
Module lms_Designations_A
 use gm_ModelControl
  use gm_GlobalData
    use gm_TypeControl
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !

        ! ---------------------- Module Global Type Constructs -------------
        type Designations_A
         real(8) :: lvd_realizedDesignations_A_maf(gpi_lBY:gpi_uBY,10)
         real(8) :: lvd_maxBCDesignations_A_maf(gpi_lBY:gpi_uBY,10)
         real(8) :: lvd_classADesignations_A_maf(gpi_lBY:gpi_uBY,10)
         real(8) :: lvd_SRPpumping_maf(gpi_lBY:gpi_uBY,10)
        end type Designations_A
        ! =======================================================================
    !
 contains
    !
    ! -----------------------
    subroutine aClear(T,ltDa)
        !
        !---- Types --
        integer :: i,j
        ! ============
        !

        ! --- Type Consructs ---
        type(runTime)T
        type(Designations_A)ltDa
        ! ===========================
        !
         if(gvl_start)then
            do i = gpi_lBY,gpi_uBY,1
              do j = 1,10,1
                ltDa%lvd_classADesignations_A_maf(i,j)=0.
              end do
            end do
          endif
        !
      ! ===========================
      return
    end subroutine 
    !-------------

    !-------------------------------------------------------
    subroutine aDesignations_classBCmax_A(T,acfta,ltDa,Aout)
        !
        ! ---------------------- Types -----------------------
        integer :: i,k
        real(8) :: acfta,lv_addBC(10)
        ! =========================================================
        ! Changed from 375,000 AF to 350,000 AF on 09.01.11 
        ! Changed, again, to 325kAF (Charlie Ester talk, 09.09.14) on 09.11.14
        ! see function fStorageRelease(state) for more details
        !
        ! - Type Constructs -----
        type(runTime)T
        type(RiverA)Aout
        type(Designations_A)ltDa
        ! ============================
        !
            do i = 1,10,1
              ltDa%lvd_maxBCDesignations_A_maf(T%year,i)=0.
              lv_addBC(i)=0.
            end do
           do k = 1,10,1
             !
             ! acft of water as determined by member land acreage-                                  (acft a-1)
             ! 3 acft acre-1 if storage is above 0.6 maf
             ! THIS HAS NOW changed to 0.8, right?
             ! 2 acft acre-1 if below
             ! Only use class B and C acreages (i.e., second column in the array)
             !
            ! 01.03.12 DAS
            ! 
            ! There can only be ~ 116,000 acres (total) serviced at 3 AF acre-1 ( 350,000 AF max pumping)
            !
            ! 02.06.12 DAS
            ! Not sure how to model this because the sum total of class B lands is 146,416 so never could meet 3 AF per acre from pumping
            ! Heavy reliance on the reservoirs.
                ! Previous code
!                  ltDa%lvd_maxBCDesignations_A_maf(T%year,k)=(acfta*gvd_acreageSVT(k,1))*gpd_acftTomaf
                !
                ! New code
              ltDa%lvd_maxBCDesignations_A_maf(T%year,k)=(acfta*gvd_acreageSVT(k,2))*gpd_acftTomaf
              Aout%gvf_classBCmax(k)=(acfta*gvd_acreageSVT(k,2))*gpd_acftTomaf
              lv_addBC(k)= ltDa%lvd_maxBCDesignations_A_maf(T%year,k)
             !
            end do
            !
            !  Maximum pumping cannot exceed 350,000 AF annually (Phillips et al. 2009)
            !  NOW CHANGED - 325kAF 09.11.14
            ! ------------------------
              !
            Aout%gvf_classBCmax=lv_addBC
            !
        return
    end subroutine aDesignations_classBCmax_A
    !-----------------------------------------
        ! 02.08.12
        ! Rules: Conservation space water (NCS) and gatewater can be used off project.
        !
        !Water from the Reservoir and Pumped water  are treated the same, allocated based 
        !on the 3 acre feet per acre.  SRP tries to deliver 3 acre feet per acre using a 
        !combination of  water flow above normal flow rights, water from the reservoir, and 
        ! pumped water.   From day to day how much each contributes to the 3 acre feet will
        ! vary.  When there is not enough from these three to meet 3 acre feet, then SRP 
        !reduces the allocation it delivers to everyone.
 
        !Ray Quay
        !Research Professional
        !Decision Center for a Desert City
        !Global Institute of Sustainability
        !Arizona State University
        !Phone: 480.965.4394
        !
    ! ----------------------------------------------------
    subroutine aDesignations_classBC_Storage(T,Aout,acfta)
        !
        ! ----------------------------------- Types -------------------------------------
        integer :: i,j,k
        integer :: lvi_list

        real(8) :: acfta
        real :: lvf_ratioAcresForPumping(10),lvf_totalBacres
        real, parameter :: lpf_mafTokAF=1000
        real :: fStorageRelease
        real :: lvf_addStorage,lvf_sum,lvf_ratio
        !                             why two?
        real :: lvf_pumpingTotal_maf,lvf_storageAlloted_maf(10),tvf_storageAlloted_maf(10)
        ! =================================================================================
        !
        ! - Type Constructs -
        type(runTime)T
        type(RiverA)Aout
        ! =======================
        ! Units kAF
        !
             ! Function to estimate release from reservoirs
             ! Pumping as a function of storage level
             ! re-analyzed on 06.21.12
             ! --------------------------------------
             !

             ! Available water for releasing
             ! Prior to 26 March, 2013
             lv_State_A_maf(T%year)=(lvd_State_Salt_acft(T%year)+lvd_State_Verde_acft(T%year) ) * gpd_acftTomaf

             lvf_pumpingTotal_maf=0
            lvf_pumpingTotal_maf= fStorageRelease(acfta,lv_State_A_maf(T%year)) * (1./lpf_mafTokAF)
            !
             lvf_totalBacres=0
            do i = 1,10,1
             lvf_totalBacres=lvf_totalBacres+gvd_acreageSVT(i,2)
            end do
            !
             lvf_addStorage=0
                do j = 1,10,1
                 tvf_storageAlloted_maf(j)=0.
                 lvf_ratioAcresForPumping(j)=0.
                 !
                 lvf_ratioAcresForPumping(j)=gvd_acreageSVT(j,2) * (1./lvf_totalBacres)
                 tvf_storageAlloted_maf(j)=max(0,(gvd_acreageSVT(j,2)*acfta*gpd_acftTomaf)-(lvf_pumpingTotal_maf*lvf_ratioAcresForPumping(j)))      
                 !
                 ! 07.06.12 DAS
                 ! Going with BandC 3 Acre feet per acre as "potential" designations for SRP members
                 ! This is for determination of the 80% rule in subroutine sEightyPercent()
                 ! The sEightyPercent subroutine is called from Groundwater.f90
                 ! ------------------------------------------------------
                if(gvl_start)then
                 gvf_availableSRP_acft(j)=gvd_acreageSVT(j,2)*3.0
                endif
                 !
                end do
            !
            ! START HERE !!!!! 
            ! 05.04.12 
            ! 
            lvf_addStorage=sum(tvf_storageAlloted_maf)

             ! Available water for releasing
             lvf_sum=0
            lvf_sum=lv_State_A_maf(T%year)-li_deadpoolSVT
             ! 
             ! Check to make certain storage allocated does
             ! not exceed that available to distribute; proportionalized, if not
             lvf_ratio=1.
            if(0 < lvf_addStorage)lvf_ratio=min(1.,lvf_sum/lvf_addStorage)
            !
            do i = 1,10,1
             lvf_storageAlloted_maf(i)=0
            end do
            !
           if(li_deadpoolSVT < lv_State_A_maf(T%year))then
              if(lvf_addStorage < lvf_sum)then
                ! Release the total requested
                lvf_storageAlloted_maf= tvf_storageAlloted_maf
              else
                ! cannot release more than what is there
                do i = 1,10,1
                 lvf_storageAlloted_maf(i)=lvf_ratio *  tvf_storageAlloted_maf(i)
                end do
                lvf_addStorage=sum(lvf_storageAlloted_maf)
              endif
           else                 
           endif
            !
              lvf_addStorage=sum(lvf_storageAlloted_maf)
            !
             lvi_list=1
            call sMaskSRP(gvl_maskSRP)
            do k = 1,gvi_Providers,1
              !
              Aout%gvf_storageAlloted_35_acft(k)=0.
              Aout%gvf_pumpingSRP_35_acft(k)=0.
              if(gvl_maskSRP(k))then     
                Aout%gvf_storageAlloted_35_acft(k)=lvf_storageAlloted_maf(lvi_list)* (1/gpd_acftTomaf)
                Aout%gvf_pumpingSRP_35_acft(k)=(lvf_pumpingTotal_maf*lvf_ratioAcresForPumping(lvi_list))* (1/gpd_acftTomaf)
                lvi_list=lvi_list+1
              endif
            end do
             !
             Aout%gvf_BCstorageUseMax_acft=0
            Aout%gvf_BCstorageUseMax_acft= sum(lvf_storageAlloted_maf)* (1./gpd_acftTomaf)
             Aout%gvf_pumpingSRP_maf=0
            Aout%gvf_pumpingSRP_maf=lvf_pumpingTotal_maf
            !
      return
    end subroutine aDesignations_classBC_Storage
    ! ------------------------------------------
!
End Module lms_Designations_A
! ======================================================================================================
!
    ! ---------------------------------------------------
    subroutine aDesignationsClassBCmax(T,acfta,ltDa,Aout)
      use lms_Designations_A
        !
        ! ---- Types ----
        real(8) :: acfta
        ! ===============
        !

        ! -- Type Constructs --
        type(runTime)T
        type(RiverA)Aout
        type(Designations_A)ltDa
        ! ======================
        !
            !
            call aDesignations_classBCmax_A(T,acfta,ltDa,Aout)
            !
        !
      return
    end subroutine aDesignationsClassBCmax
    ! ------------------------------------
    
    ! --------------------------------------
    subroutine dSaltTontoVerde(T,Ain,acfta)
      use lms_Designations_A
       use gm_ModelControl
        use gm_TypeControl
        !
        ! ----- Types -----
        real(8) :: acfta
        ! =================
        !

        ! - Type Constructs -
        type(RiverA)Ain
        type(runTime)T
        ! ===================
            !
            call aDesignations_classBC_Storage(T,Ain,acfta)
            !
             call normalFlowVerde(T,Ain)
             call normalFlowSaltTonto(T,Ain)
            !
        !
      return
    end subroutine dSaltTontoVerde
    ! ----------------------------
 
    ! ---------------------------------------------
    subroutine sBookkeeping(T,gvd_ClassAarray,Aout)
      use lms_Designations_A
        !
        ! ---------- Types ------------
        real(8) :: gvd_ClassAarray(10)
        ! =============================

        ! - Type Constructs -
        type(runTime)T
        type(RiverA)Aout
        ! ===================
        !
            !
            gvi_countYearsSVT=gvi_countYearsSVT+1
            if(gvi_SVTtrace < gvi_countYearsSVT+1)gvi_countYearsSVT=0
            Aout%gvf_ClassAarray_10_maf=gvd_ClassAarray ! maf annum-1
            !
        !
      return
    end subroutine sBookkeeping
    ! -------------------------

  ! -------------------------------------
  subroutine classAdesignations_A(T,Aout)
    use lms_Designations_A
        !
        ! ---------- Types ------------
        real(8) :: gvd_ClassAarray(10)
        ! =============================
        !

        ! - Type Constructs -
        type(runTime)T
        type(RiverA)Aout
        ! ===================
        !
            ! In DailyDesignations.f90
            call extractDaily(T,gvd_ClassAarray)
            call sBookkeeping(T,gvd_ClassAarray,Aout)
            !
        !
    return
  end subroutine classAdesignations_A
  ! ----------------------------------
 
! ----------------------------------------------
  subroutine sSRP_classBC(T,gvi_order,Aout,gtPV)
    use lms_Designations_A
     use gm_ModelControl
      use gm_TypeCOntrol
        !
        ! -------------------------- Types -------------------------------
        integer(1) :: gvi_order
        integer :: i,lvi_count

        real(8) :: acfta
        real :: lvf_WaterDemand35(gvi_maxProV)
        real :: lvf_WaterUse35(gvi_maxProV)
        real :: lvf_totalSRP_acft(10),lvf_ratioResToTotal(10)
        real :: lvf_subtractFromRelease(10),lvf_subtractFromPumping(10)
        real :: lvf_takeFromStorage(10)
        real :: lvf_on(gvi_maxProV),lvf_off(gvi_maxProV)
        real :: code=gpi_ClassBC

        logical :: lvl_proportionalUse=.true.
        logical :: lvl_reservoirsFirst=.false.
        ! =====================================================================
        !

        ! --- Type Constructs ---
        type(runTime)T
        type(RiverA)Aout
        type(provider) :: gtPV        
        type(Designations_A)ltDa
        ! =======================
        !
            !
             acfta=gvd_acftperAcreSVT ! 
            !
             call aDesignations_classBCmax_A(T,acfta,ltDa,Aout)
!             call aDesignations_classBC_Storage(T,Aout,acfta)
            ! transfer local control to global
            do i = 1,10
             gtPV%gAclassBCdesignationsMax_10_maf(T%year,i)=ltDa%lvd_maxBCDesignations_A_maf(T%year,i)
            end do
            !
            lvi_count=1
            call sMaskSRP(gvl_maskSRP)
             do i = 1,gvi_Providers,1
              !
              lvf_WaterDemand35(i)=0
              lvf_WaterUse35(i)=0
              Aout%gvf_classBCfromReservoir(i)=0
              Aout%gvf_classBCavailableTopump(i)=0
              !
              !
                ! On-project only
                ! 02.08.12 - this code compares On project water demand to total SRP (reservoir and pumping)
                ! if demand is greater than supply than use is supply.  IF supply is greater than demand,
                ! I take the proportion from the reservoirs and then from pumping and subtract each from the 
                ! initial estimate.  i.e., Release from the reservoirs is reduced (< 3 AF per acre) and pumping is reduced
                !
                ! NO LONGER ASSUMED as of 06.21.12
                !
                ! Three approaches: 1)proportional, 2) take from storage water first, 3)take from pumping first.
                ! DO NOT KNOW which is correct.... 01.22.13
                ! Default is proportional use
                ! -----------------------------------------------
              if(gvl_maskSRP(i))then  
                 lvf_subtractFromRelease(lvi_count)= 0.
                 lvf_subtractFromPumping(lvi_count)= 0.
                 Aout%gvf_SRPdemandDifference_10_acft(lvi_count)=0. 
                !
                ! ------------------------------------
                lvf_takeFromStorage(lvi_count)=0.
                !
                 lvf_totalSRP_acft(lvi_count)=0
                lvf_totalSRP_acft(lvi_count)=Aout%gvf_storageAlloted_35_acft(i)+ Aout%gvf_pumpingSRP_35_acft(i)
                !
                lvf_WaterDemand35(i)=gvf_WaterDemand_acft(i,gvi_order,3) 
                !
                if(0 < lvf_WaterDemand35(i))then
                  if(lvf_WaterDemand35(i) <  lvf_totalSRP_acft(lvi_count))then
                    if(lvl_proportionalUse)then
                       Aout%gvf_SRPdemandDifference_10_acft(lvi_count)=lvf_totalSRP_acft(lvi_count)-lvf_WaterDemand35(i) 
                    else
                        if(lvl_reservoirsFirst)then
                            if(Aout%gvf_storageAlloted_35_acft(i) < lvf_WaterDemand35(i))then
                              lvf_takeFromStorage(lvi_count)=Aout%gvf_storageAlloted_35_acft(i)
                            else
                              lvf_takeFromStorage(lvi_count)=lvf_WaterDemand35(i)
                            endif
                        else
                            if(Aout%gvf_pumpingSRP_35_acft(i) < lvf_WaterDemand35(i))then
                              lvf_takeFromStorage(lvi_count)=lvf_WaterDemand35(i)  -Aout%gvf_pumpingSRP_35_acft(i)
                            else
                              lvf_takeFromStorage(lvi_count)=0
                            endif
                        endif
                    endif
                    !
                     lvf_WaterUse35(i)=lvf_WaterDemand35(i)  
                  else
                     lvf_WaterUse35(i)= lvf_totalSRP_acft(lvi_count)
                  endif

                    ! 02.08.12 DAS
                    if(lvl_proportionalUse)then
                      if(0 <  lvf_totalSRP_acft(lvi_count))then
                        lvf_ratioResToTotal(lvi_count)=Aout%gvf_storageAlloted_35_acft(i) * (1./ lvf_totalSRP_acft(lvi_count))
                      endif
                          lvf_subtractFromRelease(lvi_count)= lvf_ratioResToTotal(lvi_count)* Aout%gvf_SRPdemandDifference_10_acft(lvi_count)
                          lvf_subtractFromPumping(lvi_count)=max(0,Aout%gvf_SRPdemandDifference_10_acft(lvi_count) &
                            - lvf_subtractFromRelease(lvi_count))

                           ! 02.07.12,02.08.12 DAS
                          Aout%gvf_classBCavailableTopump(i)= anint(Aout%gvf_pumpingSRP_35_acft(i)- lvf_subtractFromPumping(lvi_count))
                          Aout%gvf_classBCfromReservoir(i)=anint( Aout%gvf_storageAlloted_35_acft(i) &
                            -  lvf_subtractFromRelease(lvi_count))
                    else
                      lvf_subtractFromRelease(lvi_count)=0.
                      lvf_subtractFromPumping(lvi_count)=0.
                      ! 01.22.13
                      Aout%gvf_classBCavailableTopump(i)= max(0,lvf_WaterDemand35(i) - lvf_takeFromStorage(lvi_count))
                        ! anint(Aout%gvf_pumpingSRP_35_acft(i))
                      ! passed to SRPrelease.f90 to 
                      Aout%gvf_classBCfromReservoir(i)=anint( lvf_takeFromStorage(lvi_count))             !
                    endif
                    !

                    ! Send to the interface for output
                    ! Maximum for the year that we can take from storage (reservoir) water
                    go_classBCstorageMax_acft(lvi_count)=0
                   go_classBCstorageMax_acft(lvi_count)=Aout%gvf_storageAlloted_35_acft(i)
                    !
                endif
                !
                lvi_count=lvi_count+1
                !
              endif
             end do
              !
              ! =======================================================
              do i = 1,gvi_Providers,1
                lvf_on(i)= lvf_WaterUse35(i)
                lvf_off(i)=0
              end do
             !
             call sCWaccounting(gvi_order,lvf_off,lvf_on,code)
             !
            gvi_order=gvi_order+1
             !
            ! -------------------
            !
          ! ================================================================================================
          lvi_count=1
          call sMaskSRP(gvl_maskSRP)
             do i = 1,gvi_Providers,1
              if(gvl_maskSRP(i))then  
            
                 ! New meaning here too..02.08.12
                 !
                  gtPV%gApumpingSRP_maf(T%year,lvi_count)=0
                 gtPV%gApumpingSRP_maf(T%year,lvi_count)=   Aout%gvf_classBCavailableTopump(i)*gpd_acftTomaf
                !
                 gtPV%gAclassBCdesignationsMax_10_maf(T%year,lvi_count)=gpd_acftTomaf * &
                    (Aout%gvf_storageAlloted_35_acft(i)+ Aout%gvf_pumpingSRP_35_acft(i))
                !
                lvi_count=lvi_count+1
              endif
             end do                  
            !
        !
      return
    end subroutine sSRP_classBC
    ! -------------------------
!
! ======================================================================================================
! E.O.F. Designations_STV.f90