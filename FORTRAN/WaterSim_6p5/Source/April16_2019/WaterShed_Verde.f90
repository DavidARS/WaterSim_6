!
! File is WaterShed_Verde.f90
!
! This module estimates release from the Verde SRP system of 2 reservoirs
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
! Module:       Module lm_SRPverde 
! Subroutines:  subroutine initWSverde()
!                 calls(both present):
!                   call openFiles_v()
!                   call initialState_v()
!
! No Module:    subroutine initializeVerde()
!
! Module:       Module lms_SRPverde
! Subroutines:  subroutine aInFlow_verde(T,acft)
!               subroutine expectedStorageVerde(T)
!               subroutine normalFlowAvailable(T)
!               subroutine iterateBC(T,Ain)
!               subroutine storageVerde(T,Ain)
!               subroutine updateState(T)

! No Module:    subroutine adjustedFlowVerde(T,acft)
!               subroutine normalFlowVerde(T,k,Aout)
!

! File created on: 11.15.12
!
! david arthur sampson

! last write was: 01.16.13,07.20.14
! ----------------------------------
!

! ======================================================================================================
!
Module lm_SRPverde
  use gm_ModelControl
   use gm_GlobalData
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
  contains
     !
     ! -----------------------
     subroutine initWSverde()
        !
        call openFiles_v()
        call initialState_v()
        !
       return
     end subroutine initWSverde
     ! ------------------------

        ! ---------------------------------
        subroutine openFiles_v()
            !
            ! --------- Types --------------
            character(len=200) :: lvc_DPath
            ! ==============================
            !
                if(gpl_release)lvc_DPath=trim(gvc_DPath)
                !
                module="lm_SRPverde"
                !
                !Infile='App_Data\Data\VerdeTango_doy.txt'; LU=21
                Infile='App_Data\Data\VerdeTango2015_doy.txt'; LU=21
                call openFiles(module,lvc_DPath,Infile,LU)
                call readFiles_v(module,Infile,LU)
                !
            !
         return
        end subroutine openFiles_v
        ! ------------------------

        ! ----------------------------------------
        subroutine readFiles_v(module_,file_,LU_)
            !
            ! ------------ Types ------------------
            integer :: LU_
            integer :: i,j
            integer :: ios 
            integer :: gvi_flowV(gpi_dailySVTrec,3)

            character(len=25) :: module_
            character(len=50) :: file_
            ! =====================================
                !
                ! Units cfs
                read(LU_,*,err=1,iostat=ios)((gvi_flowV(i,j),j=1,3),i=1,gpi_dailySVTrec)
                close(LU_)
                ! assign the input to a global array listed in WaterSimDCDC.txt
                 gvf_flowVerde=gvi_flowV
1                continue
                if(ios >0)then
                 goto 1000
                endif
                !
         return
1000        continue
             if(gvl_writeLog)then
                    string=36
                    LU=0
                    call sStrings(string,errorString)
                    call eWrite(errorString,LU)
                endif
             gvl_errorFlag=.false.
            call sErrorCodes(module_,file_,LU_)
            !
         return
        end subroutine readFiles_v
        ! ------------------------

        ! -------------------------
        subroutine initialState_v()
            !    
            ! Assumes that simulations will begin in the year 2000
            ! Correction for storage capacity.
            ! ==========================================================
            ! From empirical data- Mark Hubble, 02.06.13 das
            lvd_State_Verde_acft(2000)=79465.0
            !
         return
        end subroutine initialState_v
        ! ---------------------------
        !
End Module lm_SRPverde
!
    ! --------------------------
    subroutine initializeVerde()
      use lm_SRPverde
        !
        call initWSverde()
        !
      return
    end subroutine initializeVerde
    ! ----------------------------
!
! =================================================================================================
!
Module lms_SRPverde
 use gm_ModelControl
  use gm_GlobalData
   use gm_TypeControl
    use gm_DataAndSensitivity
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !

        ! -------------------- Module Global Variables -------------------
        ! mvf = module variable float
        integer, private :: yr,doy,iRecStart

        real,private :: mvf_stateAdjustDemand(gpi_lBY:gpi_uBY,12)

        real, private :: mvf_verde_maf_yr(gpi_lBY:gpi_uBY),mvf_verde_acft(gpi_lBY:gpi_uBY,12)
        real,private  :: mvf_Verdeflow_acft(gpi_dailySVT), mvf_flowVerde_acft
        real,private  :: mvf_Verdeflow_acft_month(12)
        real,private  :: mvf_StorageVerde_acft, mvf_expectedStorageVerde_acft

        ! mpf = module parameter float
    !    real, parameter :: mpf_storageCapacityVerde_acft=194723
        real,private :: mpf_storageCapacityVerde_acft,mpf_storageMinimumVerde
        real,private :: mvf_addToReservoir(3),mvf_ClassADeficit
        real,private :: mvf_usedBC_acft,mvf_differenceBCVerde(3)
        real,private :: mvf_release,mvf_ratio
        real, private :: mvf_tempClassBCrights_acft
        ! monthly
        real, private :: mvf_verdeFlowMonthly_acft(12),mvf_addFlows
        real, private :: mvf_addToReservoirMonthly(3,12)
        real, private :: mvf_ClassADeficitMonthly(12),mvf_differenceBCVerdeMonthly(3,12)
        real, private :: mvf_usedBCMonthly_acft(12),mvf_addMonthlyBC,mvf_usedA_acft(12)
        ! ====================================================================
        !
        real :: mpf_proportionalSaveVerdeStorage= 40
        real :: mpf_threshStorage=0.5
        real :: mpf_ratioVerde=0.8
 contains
   !
        !
        ! ------------------------------
        ! Must be called yearly
        ! ------------------------------
        subroutine aInFlow_verde(T,acft)
            !
            ! ------------------ Types ---------------------
            integer :: acft,i,j
            integer :: lvi_start=2000
            integer :: lvi_addDays,lvi_month,fmonthsFromDOY
            integer :: lvi_monthHold

            real :: cf

            logical :: lvl_pass=.true.
            ! =============================================
            !

            ! - Type Constructs -
            type(runTime)T
            ! ===================
            !
                if(gv_dataSVT < 1)gv_dataSVT=1
                !
                if(0 < acft)then
                 ! Reserved for test simulations
                 mvf_verde_maf_yr(T%year)=acft*1.*gpd_acftTomaf
                 !
                else
               !
                  if(T%year == T%startyear)then
                    gvi_countYearsVerde=0
                    ! Yearly array, 100 year possible
                    do i = 1,100,1
                     do j = 1,3,1
                       lvf_flowVerde_acft(j,i)=0
                     end do
                    end do
                    !
                    vArecAv=1
                  endif
                  ! ========================================================================
                    ! gvi_countYearsSVT must be incremented each year
                   if(T%year < 2013)then
                     lvi_start=2000
                   else
                     if(T%year < 2014)gvi_countYearsVerde=0
                       if(gvi_SVTtrace-1 < gvi_countYearsVerde)gvi_countYearsVerde=0

                    !
                     lvi_start=gv_indexyearSVT
                      if(1 < gv_dataSVT)lvi_start=1981
                   endif
                    !
                      lvi_monthHold=1
                      lvi_month=1
                      mvf_addFlows=0
                    !
                     do i = 1,gpi_dailySVTrec,1
                      mvf_Verdeflow_acft(i)=0
                      !
                      if(nint(gvf_flowVerde(i,1)) .eq. lvi_start+gvi_countYearsVerde)then
                       if(lvl_pass)iRecStart=i
                         yr=gvf_flowVerde(i,1)
                         doy=gvf_flowVerde(i,2)
                          ! units: cfs used internally
                          mvf_Verdeflow_acft(i)=gvf_flowVerde(i,3)*gpf_ft3sToAcftDay
                          !
                          lvi_monthHold=lvi_month
                           lvi_month=0
                          lvi_month=fmonthsFromDOY(yr,doy)
                          !
                          call monthlyFlow(i,doy,lvi_month,lvi_monthHold)
                          !
                          call countYears(lvi_addDays)
                          !
                        lvl_pass=.false.
                      endif
                     end do
                !
                endif
                    !

                    !  Climate factor - gradual change over the simulation. Value
                    ! input is reached by the end of the simulation cycle.
                    ! -----------------------------------------------------------
                    call ClimateFactorSVT(T%simyear+1,cf)
                    lvf_flowVerde_acft(1,vArecAv)=sum(mvf_Verdeflow_acft)*cf  
                    !
                    do i = 1,12,1
                     mvf_verdeFlowMonthly_acft(i)= mvf_verdeFlowMonthly_acft(i)*cf
                    end do
                    !
                    !  Drought factor - immediate change
                    ! -----------------------------------
                    if(T%year >= gv_droughtyearSVT)then
                      if(T%year <=  gv_droughtyearendSVT)then
                        lvf_flowVerde_acft(1,vArecAv)= lvf_flowVerde_acft(1,vArecAv)*gv_droughtfactorSVT
                        do j = 1,12,1
                          mvf_verdeFlowMonthly_acft(j)= mvf_verdeFlowMonthly_acft(j)*gv_droughtfactorSVT
                          ! ratio to account for water removed for the Environment (in stream) in WaterShed.f90
                          mvf_verdeFlowMonthly_acft(j)=mvf_verdeFlowMonthly_acft(j)
                        end do
                      endif
                    endif
                    !
                    ! These are not adjusted for flows to the Environment (happens in calls following)
                    ! ---------------------------------------------------------------------------------
                    mvf_flowVerde_acft=anint(lvf_flowVerde_acft(1,vArecAv))
                    !
                    gvd_verde_maf_yr(T%year)=(lvf_flowVerde_acft(1,vArecAv) * gpd_acftTomaf)
                    !
                    gvi_countYearsVerde=gvi_countYearsVerde+1
                    vArecAv=vArecAv+1
                    !
                !
          return
        end subroutine aInFlow_verde
        ! --------------------------

      ! ------------------------------
      subroutine countYears(lvi_Days)
        !
        ! ----- Types -------
        integer :: lvi_Days

        real :: fLeapYear
        ! ===================
        !
          if(364 < doy)then
               lvi_Days=fLeapYear(yr)
              if(365 < lvi_Days)then
                  if(365 < doy)then
                    if(2012 < yr)then
                      gvi_countYearsVerde=-1
                    endif
                  endif
              else
                  if(364 < doy)then
                    if(2012 < yr)then
                      gvi_countYearsVerde=-1
                    endif
                  endif
              endif
          endif
        !
        return
      end subroutine countYears
      ! ------------------------

      ! ----------------------------------------------
        subroutine monthlyFlow(i,dayOfYear,month,hold)
            !
            ! ---------- Types --------------
            integer :: i,month,hold,dayOfYear
            ! ===============================
            !  
                !
                if(hold < month)then 
                  mvf_addFlows=0
                 mvf_addFlows=mvf_addFlows+gvf_flowVerde(i,3)*gpf_ft3sToAcftDay
                else
                  if(dayOfYear < 2)mvf_addFlows=0
                 mvf_addFlows=mvf_addFlows+gvf_flowVerde(i,3)*gpf_ft3sToAcftDay
                endif
                !
                mvf_verdeFlowMonthly_acft(month)=anint(mvf_addFlows)
                gvf_verdeMonthly_acft(month)= mvf_verdeFlowMonthly_acft(month)
                !
            !
         return
        end subroutine monthlyFlow
      ! ---------------------------------

      ! --------------------------------
      subroutine expectedStorageVerde(T)
        !
        ! ---------------------- Types --------------------
        real :: vEvap_v
        real(8) :: fEvaporation
        ! =================================================
        !
        ! - Type Constructs -
        type(runTime)T
        ! ===================
            ! 
            mvf_verde_acft(2000,1)=lvd_State_Verde_acft(2000)

            ! I took mine from their web site-12.05.14 das
            ! http://www.srpnet.com/water/dams/bartlett.aspx
            ! http://www.srpnet.com/water/dams/horseshoe.aspx
            ! 12.06.14 calculated minimum using
            ! 0.64 is to 113,453 AF as 0.01 is to x: find x. (using 1% as mimimum)
            mpf_storageCapacityVerde_acft= (li_horseshoemax+li_bartlettmax)*(1./gpd_acftTomaf)
            ! 
            ! See line 379 in WaterShed_SaltTonto.f90
!            mpf_storageMinimumVerde=(li_deadpoolSVT* 1/gpd_acftTomaf) - lpf_saltTontoMinimum_acft
!            mpf_storageMinimumVerde=22502
            mpf_storageMinimumVerde=1773
            !
               mvf_StorageVerde_acft=0
            if(T%year == T%startyear)then
              mvf_release=0
              mvf_StorageVerde_acft=lvd_State_Verde_acft(T%year)
              !
              vEvap_v=0.
              vEvap_v=fEvaporation(lvd_State_Verde_acft(T%year))
              mvf_expectedStorageVerde_acft=lvd_State_Verde_acft(T%year)+ lvf_flowVerde_acft(1,vArecAv)-(vEvap_v+mvf_release)
            else
              vEvap_v=0.
              vEvap_v=fEvaporation(lvd_State_Verde_acft(T%year-1))
              mvf_expectedStorageVerde_acft=lvd_State_Verde_acft(T%year-1)+  lvf_flowVerde_acft(1,vArecAv)-(vEvap_v+mvf_release)
            endif
            !
        return
      end subroutine expectedStorageVerde
      ! ---------------------------------

      ! ---------------------------------------------------
      subroutine normalFlowAvailableMonthly(T,i,sumDemand)
        !
        ! ---------------- Types ---------------
        integer :: i

        real :: flow(12)
        real :: lvf_usedA_acft(12),sumDemand(12)
        ! ======================================
        !

        ! - Type Constructs -
        type(runTime)T  
        ! ===================
            !
            ! Ratio of verde river flow to verde plus salt-tonto
!             lvf_ratioFlowVerde=0
!            lvf_ratioFlowVerde= gvf_verdeMonthly_acft(i) / (gvf_verdeMonthly_acft(i)+gvf_saltTontoMonthly_acft(i))
            !
             ! Class A water
            ! ----------------
             lvf_usedA_acft(i)=0
            lvf_usedA_acft(i)=sumDemand(i) 
            !
            ! Flow minus environmental flow estimates (used as a ratio)
            ! --------
             flow(i)=0
            flow(i)= (mvf_verdeFlowMonthly_acft(i))!*gvf_ratioVerdeFlowsPostEnv 
            !
                !
                ! Globally available estimate of monthly flow
                ! -------------------------------------------------------
                gvf_flowVerde_Monthly_maf(T%year,i)=flow(i)*gpd_acftTomaf
                !
            !
             mvf_addToReservoirMonthly(1,i)=0
             mvf_ClassADeficitMonthly(i)=0
            !
            !  Flow is greater than the sum of the class A rights
            ! Use all of the available flow on the Verde to cover the Class A water
            ! rights (for the combined Salt-Tonto-Verde runoff)
             if(i < 2)gvf_verdeFlowUsed(T%year)=0
            if(lvf_usedA_acft(i) <= flow(i))then
                gvf_verdeFlowUsed(T%year)= gvf_verdeFlowUsed(T%year)+lvf_usedA_acft(i)
                mvf_addToReservoirMonthly(1,i)=flow(i)-lvf_usedA_acft(i)
            else
                ! Flow is less than the rights              
                gvf_verdeFlowUsed(T%year)= gvf_verdeFlowUsed(T%year)+flow(i)
                mvf_addToReservoirMonthly(1,i)=0  
                mvf_ClassADeficitMonthly(i)=flow(i)-lvf_usedA_acft(i)
            endif 
            !
            mvf_usedA_acft(i)=lvf_usedA_acft(i)
            !
          return
      end subroutine normalFlowAvailableMonthly
      ! ----------------------------------------

      ! -------------------------------------
      subroutine monthlyBC(T,i,sumDemand,Ain)
        !
        ! --------------- Types ----------
        integer :: i

        real :: mvf_tempClassBCrights_acft
        real :: sumDemand(12)
        !real,parameter :: lpf_threshStorage=0.5
        ! ========================================
        !

        ! -- Type Construct --
        type(RiverA)Ain
        type(runTime)T
        ! ====================
            !
             mvf_usedBC_acft=0
            !
             mvf_tempClassBCrights_acft=0.
            mvf_tempClassBCrights_acft=sumDemand(i)
            !
             mvf_addToReservoirMonthly(2,i)=0
             mvf_differenceBCVerdeMonthly(1,i)=0
             mvf_usedBCMonthly_acft(i)=0
             !
             Ain%gvf_classAVerdeToSaltMonth_AF(i)=0 
            !
            ! Reduce B and C allocations to Verde if the estimate storage falls below 50 % of Maximum
            ! 11.05.14 DAS
            ! ------------
            if(1 < i)then
              if(mvf_stateAdjustDemand(T%year,i-1) < mpf_threshStorage)then
               mvf_tempClassBCrights_acft =  mvf_tempClassBCrights_acft * (1.-mvf_stateAdjustDemand(T%year,i-1))
              endif
            else
              if(2000 < T%year)then
                if(mvf_stateAdjustDemand(T%year-1,12) < mpf_threshStorage)then
                 mvf_tempClassBCrights_acft =  mvf_tempClassBCrights_acft * (1.-mvf_stateAdjustDemand(T%year-1,12))
                endif
              else

              endif
            endif
            !

            ! No need to draw BandC (from the river flow) when class A has not been met (should never be in this loop)
            ! ----------------------------------------------------------------------------------
            !
            if(mvf_ClassADeficitMonthly(i) < 0)then
              Ain%gvf_verdeToSaltTranClassA_AF  = Ain%gvf_verdeToSaltTranClassA_AF+ABS(mvf_ClassADeficitMonthly(i))
              Ain%gvf_classAVerdeToSaltMonth_AF(i)= abs(mvf_ClassADeficitMonthly(i))
              !
              mvf_differenceBCVerdeMonthly(1,i)     = mvf_tempClassBCrights_acft
            else
                !   Class BandC water removed from remaining flow data.  Use all of the 
                ! available river flow data available to cover the Class B and C water
                ! ---------------------------------------------------------------------
                if(0 < mvf_addToReservoirMonthly(1,i))then
                    if(mvf_tempClassBCrights_acft < mvf_addToReservoirMonthly(1,i))then
                         mvf_usedBCMonthly_acft(i)= mvf_tempClassBCrights_acft
                    else
                         mvf_usedBCMonthly_acft(i)=  mvf_addToReservoirMonthly(1,i)
                    endif
                    !
                    mvf_addToReservoirMonthly(2,i)=max(0,mvf_addToReservoirMonthly(1,i)- mvf_usedBCMonthly_acft(i))
                else
                    ! Should never be here
                endif
                !
                ! Difference in B&C to pass on to the Salt system, if needed
                ! -----------
                 mvf_differenceBCVerdeMonthly(1,i)=mvf_tempClassBCrights_acft- mvf_usedBCMonthly_acft(i)
            !
            endif
            !
            ! Set to zero each year in subroutine initializeLoop(T,Ain)
            ! --------------
            mvf_addMonthlyBC=mvf_addMonthlyBC+ mvf_usedBCMonthly_acft(i)
            !
        !
       return
      end subroutine monthlyBC
      ! ----------------------

         ! -----------------------------
        ! Calculate the current storage
        ! Verde reservoirs (i.e.,
        ! Horseshoe and Bartlett)
        ! ==================================
        ! -------------------------

        ! ==================================
        subroutine storageVerdeMonthly(T,i,Ain)
            !
            ! -------------------- Types -----------------------------
            integer :: i,j,lvi_loop=1000

            real :: annual
            real :: lvf_evapMonthly(12),lvf_overFlowMonthly_acft(12)
            real(8) :: state,lvd_initialState
            real :: lvf_reservoirToBCwater
            real :: fEvaporationMonthly
           ! real :: lvf_ratioVerde
            real,parameter :: lpf_deltaStorage=0.98
            real,parameter :: lpf_rounding=0.98
           ! real :: lpf_proportionalSaveVerdeStorage= 40
!            real(8), parameter :: lpd_exponent=2.71828182845904523536
            real :: lvf_ratioStorage,lvf_differenceBC
            real :: lvf_storageThreshold,cf
            logical :: lvl_pass,lvl_rounding,lvl_reservoir
            ! ========================================================
            !

            ! - Type Constructs -
            type(RiverA)Ain
            type(runTime)T
            ! ===================
                !
                call ClimateFactorSVT(T%simyear+1,cf)
                !
                 state=0.
                state=lvd_State_Verde_acft(T%year)
                !
                 lvd_initialState=0
                lvd_initialState=mvf_verde_acft(T%year,i)
                !
                 lvf_evapMonthly(i)=0
                lvf_evapMonthly(i)=fEvaporationMonthly(lvd_initialState)
                !

                 lvf_differenceBC=0.
                !
                !if(0 < gvf_parm_a)lpf_proportionalSaveVerdeStorage=gvf_parm_a
                !
                !  Use 1-0.43 (0 to 57%) of the difference in Class B and C not met by 
                ! river flow from the reservoir storage
                ! water. This value was calculated using hurestic principles.
                ! --------------------------------------------------------------------
                    !
                    ! Initialize
                    ! -----------------
                   ! lvf_ratioVerde=0.8
                    !if(0 < gvf_parm_b)lvf_ratioVerde=gvf_parm_b

                    lvf_overFlowMonthly_acft(i)=0
                    !
                    lvl_pass=.false.
                    lvl_rounding=.false.
                    lvl_reservoir=.false.
                    !
                do j = 1,lvi_loop,1
                    !
                     lvf_ratioStorage=0
                    lvf_ratioStorage=lvd_initialState/mpf_storageCapacityVerde_acft
                    !
                    mvf_stateAdjustDemand(T%year,i)=lvf_ratioStorage
                    !
                    ! Potentiall some BandC water to be removed from storage water
                    ! (if B&C rights > used from flow)
                    ! ------------------------------------------------------------
                     lvf_reservoirToBCwater=0.
                    lvf_reservoirToBCwater= (1.-mpf_ratioVerde) * mvf_differenceBCVerdeMonthly(1,i)
                    !
                     lvf_differenceBC=0
                    if(lvf_reservoirToBCwater < mvf_differenceBCVerdeMonthly(1,i))then
                     lvf_differenceBC=lvf_ratioStorage * (mvf_differenceBCVerdeMonthly(1,i)-lvf_reservoirToBCwater )
                    else
                     lvf_differenceBC=0
                    endif
                    !
                    lvf_reservoirToBCwater=(lvf_reservoirToBCwater+lvf_differenceBC)
                    !
                     ! If no flow to cover Class A, I shipped all of B&C to Salt-Tonto. So, cannot take B&C here
                    ! 03.28.13 das
                    ! --------------
                    if(mvf_ClassADeficitMonthly(i) < 0)then
                        lvf_reservoirToBCwater=0
                    endif
                     !
                     annual=0.
                    annual = state +  mvf_addToReservoirMonthly(2,i)-lvf_evapMonthly(i)-lvf_reservoirToBCwater
                    !
                      if(lvf_reservoirToBCwater < 1)lvl_reservoir=.true.
                      if(mpf_ratioVerde < (1.-lpf_rounding))lvl_rounding=.true.
                    !
                    if(mpf_storageCapacityVerde_acft <= annual)then
                        !
                        ! Increate the amount of water removed from storage for B&C
                        ! -------------
                        mpf_ratioVerde=(max(0.,mpf_ratioVerde*lpf_deltaStorage))
                        !
                         if(lvl_reservoir .or. lvl_rounding)then
                            !
                            state=nint(mpf_storageCapacityVerde_acft)
                            lvf_overFlowMonthly_acft(i)=annual-mpf_storageCapacityVerde_acft
                            lvl_pass=.true.
                        else
                        endif
                        !
                    else
                        ! ---------------------------------------------------------------------
                        !
                        !   Leave a minimum amount of water in the reservoir.
                        ! Estimate using the 2000-2006 reservoir data value for "lpf_proportionalSaveVerdeStorage"
                        ! was also derived though parameterization) (visual interpretation of the minimum monthly storage 
                        ! observed in the empirical data.
                        ! ---------------------------------------------------------------------------------------------
                        !
                        lvf_storageThreshold=mpf_storageMinimumVerde*mpf_proportionalSaveVerdeStorage*cf
                        !
                        if(lvf_storageThreshold < annual)then
                            !
                            state = nint(annual)
                            !
                          lvl_pass=.true.
                        else
                        endif
                        !
                    endif
                    !

                    ! Exit if there is no BandC to take
                    if(lvl_reservoir)lvl_pass=.true.
                    if(lvl_rounding)lvl_pass=.true.
                    !
                    ! Reduce ratio to reduce BC from storage until a reasonable solution is obtained
                    if(lvl_pass)then
                     exit
                    else
                        ! reduce the amount used to increase storage to the minimum criteria
                        ! -------------------------------------------------------------------
                        !
                        mpf_ratioVerde=min(1.,mpf_ratioVerde*lpf_deltaStorage)
                        !
                    endif
                    !
                end do
                    !
                    !  Transfer remaining class B and C water to the Salt-Tonto Reservoir system
                    !
                    ! --------------------------------------------------------------------------
                    !
                    mvf_differenceBCVerdeMonthly(2,i)= mvf_differenceBCVerdeMonthly(1,i)-lvf_reservoirToBCwater
                    !
                     mvf_addMonthlyBC=mvf_addMonthlyBC+anint(lvf_reservoirToBCwater)

                    Ain%gvf_verdeToSaltTranClassBC_AF   = Ain%gvf_verdeToSaltTranClassBC_AF+anint(mvf_differenceBCVerdeMonthly(2,i))

                ! Pass this along to SRP release
                Ain%gvf_overFlowVerde_acft=Ain%gvf_overFlowVerde_acft+lvf_overFlowMonthly_acft(i)
                !
                ! ---------------------------------------------------------------------------
                !
                if(i == 12)then
                 mvf_verde_acft(T%year+1,1)=state
                else
                 mvf_verde_acft(T%year,i+1)=state
                endif
                !
                lvd_State_Verde_acft(T%year)=state
                !
            !
          return
        end subroutine storageVerdeMonthly
        ! --------------------------------

        ! --------------------------------
        subroutine initializeLoop(T,Ain)
            !
            ! - Type Constructs -
            type(RiverA)Ain
            type(runTime)T
            ! ===================
            !
                !
                mvf_usedBC_acft=0
                gvf_verdeFlowUsed(T%year)=0
                Ain%gvf_verdeToSaltTranClassA_AF=0
                Ain%gvf_verdeToSaltTranClassBC_AF=0
                Ain%gvf_verdeBCused_acft=0
                mvf_addMonthlyBC=0
                go_verdeRiverFlow_acft=0
                Ain%gvf_overFlowVerde_acft=0
                !
            !
        return
      end subroutine initializeLoop
      ! ---------------------------

      ! ---------------------------
      subroutine transferOut(T,Ain)
            !
            ! - Type Constructs -
            type(RiverA)Ain
            type(runTime)T
            ! ==================
            !
                !
                Ain%gvf_verdeAused_acft=anint(gvf_verdeFlowUsed(T%year))
                Ain%gvf_verdeBCused_acft=anint(mvf_addMonthlyBC)
                !
            !
        return
      end subroutine transferOut
      ! ------------------------

        ! ----------------------------------
        subroutine outputsToInterface(T,Ain)
            !
            ! ------- Types -----
            real :: lvf_storage
            ! ===================
            !

            !  -- Type Constructs ----
            type(RiverA)Ain
            type(runTime)T
            ! ========================
            !
                !
                 lvf_storage=0
                lvf_storage=lvd_State_Verde_acft(T%year)
                !
                 go_VerdeStorage_acft=0
                go_VerdeStorage_acft=nint(lvf_storage)
                 go_VerdeClassAused_acft=0
                go_VerdeClassAused_acft=nint(Ain%gvf_verdeAused_acft)
                 go_verdeRiverFlow_acft=0
                go_verdeRiverFlow_acft=nint(mvf_flowVerde_acft)
                 go_VerdeClassBCused_acft=0
                go_VerdeClassBCused_acft=nint( mvf_addMonthlyBC)
                !
            !
        return
      end subroutine outputsToInterface
      ! -------------------------------

      ! ------------------------
        subroutine validate(T,i)
            !
            ! --- Types ---
            integer :: i
            ! =============
            !

            ! - Type Construct -
            type(runTime)T
            ! ==================
            !
                !
                if(gpl_comparisons)then
                  if(T%year < 2013)then
                    lvd_State_Verde_acft(2007)=72619.000000
                     !
                    if(gpl_validate)then
                      if(2006 < T%year .and. T%year < 2013)then
                        write(101,*)T%year,i,lvd_State_Verde_acft(T%year)
                      endif
                    else
                       if(T%year < 2007)then
                         write(101,*)T%year,i,lvd_State_Verde_acft(T%year)
                       endif
                    endif
                 endif
                endif
                !
            !
         return
        end subroutine validate
      ! -----------------------

      ! Update the state variable
      ! ==============================
        ! ----------------------
        subroutine updateState(T) 
            !
            ! Type Construct -
            type(runTime)T
            ! ================
            !
                !
                lvd_State_Verde_acft(T%year+1)=nint(lvd_State_Verde_acft(T%year))
                !
            !
        return
      end subroutine updateState
      ! ------------------------
!
End Module lms_SRPverde

    ! ----------------------------------
    subroutine adjustedFlowVerde(T,acft)
      use lms_SRPverde
        !
        ! ---- Types ---
        integer :: acft
        ! ==============
        !

        ! - Type Constructs -
        type(runTime)T
        ! ===================
        !
            !
            call aInFlow_verde(T,acft)
            !
        !
      return
    end subroutine adjustedFlowVerde
    ! ------------------------------
   
    ! ------------------------------
    subroutine initializeVerdeMonthly(T)
      use lms_SRPverde
        ! ------------ types ---------
        ! ============================
        !

        ! -- type construct --
        type(runTime)T
        ! ====================
            !
            if(gpl_runSensitivity)then
                mpf_proportionalSaveVerdeStorage=gpf_propSaveVerdeStorage
                mpf_threshStorage=gpf_threshStorage
                mpf_ratioVerde=gpf_ratioVerde
            endif
            !
      return
    end subroutine initializeVerdeMonthly

    ! --------------------------------
    subroutine normalFlowVerde(T,Aout)
      use lms_SRPverde
        !
        ! ----------------------- Types -----------------------
        integer :: i

        real :: TotDemand,normal_A_acft(12),normal_BC_acft(12)

        logical :: lvl_passOut
        ! =====================================================
        !

        ! - Type Constructs -
        type(RiverA)Aout
        type(runTime)T
        ! ===================
            !
            lvl_passOut=.true.
            !
            call initializeVerdeMonthly(T)
            !
              TotDemand=0
             TotDemand=sum(gvf_UsedDemand_A_acft)
            if(0 < TotDemand)then
             call monthlyDemand(lvl_passOut,gvf_janUsePropOfTotal,TotDemand,normal_A_acft)
            else
             do i = 1,12,1
               normal_A_acft(i)=0.
             end do
            endif
            !
             TotDemand=sum(gvf_tempBCstorageWaterUsed_acft)
             if(0 < TotDemand)then
                call monthlyDemand(lvl_passOut,gvf_janUsePropOfTotal,TotDemand,normal_BC_acft)
             else
                do i = 1,12,1
                 normal_BC_acft(i)=0.
                end do
             endif
             !
             call initializeLoop(T,Aout)
             call expectedStorageVerde(T)
            !
            do i = 1,12,1
            call normalFlowAvailableMonthly(T,i,normal_A_acft)
              call monthlyBC(T,i,normal_BC_acft,Aout)
             call storageVerdeMonthly(T,i,Aout)
             call validate(T,i)
            end do
            !
             call updateState(T)
             call transferOut(T,Aout)
             call outputsToInterface(T,Aout)
            !
        !
      return
    end subroutine normalFlowVerde
    ! ----------------------------

    ! -------------------------------------------------------
    subroutine monthlyDemand(lvl_pass,lvf_jan,demand,lvf_out)
        !
        ! -------- Types -------------------
        character(10) :: Model='Verde'

        real :: lvf_min,lvf_total,lvf_jan
        real :: lvf_out(12),demand
        real :: lvf_janUseProportional

        logical :: lvl_pass
        ! ==================================
            !
            lvf_janUseProportional=0.75
             lvf_min=0
            lvf_total=demand 
            !

            ! Gives minimum as 6.25 % of total (annual)
            ! Sally calculated 6.28% for total consumption City of Phoenix
            ! 02.15.13 at DCDC
            ! -------------------------------------------------------------
             !
            if(0 < lvf_total)then
                if(lvf_jan < 0.1)lvf_jan=lvf_janUseProportional

                if(lvl_pass)then
                  lvf_min=lvf_jan*(lvf_total/12)
                  call DemandNormal(Model,lvf_min,lvf_total,lvf_out)
                else
                    ! Adjust Gaussian distribution if flow < demand
                  lvf_janUseProportional=lvf_jan*0.98
                  lvf_min=lvf_janUseProportional*(lvf_total/12)
                  lvf_jan=max(0.45,lvf_janUseProportional)
                  call DemandNormal(Model,lvf_min,lvf_total,lvf_out)
                endif
            endif
            !
            if(lvl_pass)then
              lvf_jan=lvf_janUseProportional
            endif
            !
            lvl_pass=.true.
            !
        !
     return
    end subroutine monthlyDemand
    ! --------------------------

    ! ------------------------------------------------------------
    subroutine checkClassAflowAvailable(flow,lvf_demand,lvl_pass)
        !
        ! ---------- Types ----------
        integer :: i

        real :: lvf_demand(12),check
        real :: flow(12)

        logical :: lvl_pass
        ! ===========================
        !
            !
            check=0
            do i = 1,12,1
              check=flow(i)-lvf_demand(i)
                !
                 if(check < 0)then
                  lvl_pass=.false.
                  exit
                else
                  lvl_pass=.true.
                endif
            end do
            !
        !
       return
    end subroutine checkClassAflowAvailable
    ! -------------------------------------
! ======================================================================================================
! E.O.F. WaterShed_Verde.f90