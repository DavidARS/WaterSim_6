!
! File is WaterShed_Verde.f90
!
! This module estimates release from the Salt-Tonto SRP system of 4 reservoirs
! -------------------------------------------------------------------------------------
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
! Module:       Module lm_SRPsaltTonto    
! Subroutines:  subroutine initWSsaltTonto()
!                 calls(also present):
!                   call openFiles_st()
!                   call initialState_st()
!
! No Module:    subroutine initializeSaltTonto()   
!
! Module:       Module lms_SRPsaltTonto
! Subroutines:  subroutine aInFlow_saltTonto(T,acft)
!               subroutine releaseSalt(T,Ain)
!               subroutine expectedStorageSalt(T)
!               subroutine normalFlowAvailable(T,Ain)
!               subroutine iterateBC(T,Ain)
!               subroutine storageSalt(T,Ain)
!               subroutine NCS_new(T,Ain)
!               subroutine updateState(T)
!
! No Module:    subroutine adjustedFlowSaltTonto(T,acft)
!               subroutine normalFlowSaltTonto(T,k,Aout)
!

! File created on: 11.19.12
!
! david arthur sampson

! last write was: 01.24.14,07.20.14
! ----------------------------------

!
! ======================================================================================================
Module lm_SRPsaltTonto
  use gm_ModelControl
   use gm_GlobalData
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    ! 
  contains
     !
     ! --------------------------
      subroutine initWSsaltTonto()
        call openFiles_st()
        call initialState_st()
       return
      end subroutine initWSsaltTonto
      ! ----------------------------

        ! ----------------------
        subroutine openFiles_st()
            !
            ! ------------- Types --------------
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
                module="lm_SRPsaltTonto"
                !
                !Infile='App_Data\Data\SaltTonto_doy.txt'; LU=20
                Infile='App_Data\Data\SaltTonto2015_doy.txt'; LU=20
                call openFiles(module,lvc_DPath,Infile,LU)
                call readFiles_st(module,Infile,LU)
                !
            !
         return
        end subroutine openFiles_st
        ! --------------------------

        ! -----------------------------------------
        subroutine readFiles_st(module_,file_,LU_)
            !
            ! ----------------- Types -----------------
            integer :: LU_,i,j,ios
            integer :: gvi_flowST(gpi_dailySVTrec,3)

            character(len=25) :: module_
            character(len=50) :: file_
            ! =========================================
              ! Units cfs
!              LU=20
              read(LU_,*,err=1,iostat=ios)((gvi_flowST(i,j),j=1,3),i=1,gpi_dailySVTrec)
             close(LU_)
                ! assign the input to a global array listed in WaterSimDCDC.txt
                gvf_flowSaltTonto=gvi_flowST
1           continue
            if(ios >0)then
             goto 1000
            endif
            !
         return
          1000 continue
               if(gvl_writeLog)then
                    string=35
                    LU=0
                    call sStrings(string,errorString)
                    call eWrite(errorString,LU)
                endif
              gvl_errorFlag=.false.
             !
            call sErrorCodes(module_,file_,LU_)
            !
        end subroutine readFiles_st
        ! ------------------------

        ! -------------------------
        subroutine initialState_st()
            !    
            ! Assumes that simulations will begin in the year 2000
            ! ==========================================================
!            lpf_storageCapacitySalt_acft=(li_rooseveltmax+li_stewartmountainmax+li_mormanflatmax+li_horsemesamax)*1/gpd_acftTomaf
            ! From Empirical data directly
            ! Mark Hubble, 02.06.13 das
            lvd_State_Salt_acft(2000)=822988.0
            lvd_State_Others_acft(2000)=342842.0000
            lvd_State_Roosevelt_acft(2000)=480146.0000
            !
          return
        end subroutine initialState_st
        ! ---------------------------
        !
End Module lm_SRPsaltTonto
!
    ! --------------------------------
    subroutine initializeSaltTonto()
      use lm_SRPsaltTonto
        !
        call initWSsaltTonto()
        !
      return
    end subroutine initializeSaltTonto
    ! --------------------------------
!
! ======================================================================================================
Module lms_SRPsaltTonto
 use gm_ModelControl
  use gm_GlobalData
   use gm_TypeControl
    use gm_DataAndSensitivity
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !

        ! -------------------------- Module Global Types -------------------------
        ! mvf = module variable float
        integer, private :: yr,doy,iRecStart
        real(8),private :: mvd_saltOther_acft(gpi_lBY:gpi_uBY,12),mvd_roosevelt_acft(gpi_lBY:gpi_uBY,12)
        real :: mvf_addFromRoosevelt
        real, private :: mvf_saltTonto_maf_yr(gpi_lBY:gpi_uBY)
        real,private  :: mvf_saltTontoFlow_acft(gpi_dailySVT),mvf_flowSaltTonto_acft
        real,private  :: mvf_StorageSalt_acft, mvf_expectedStorageSalt_acft
        ! mpf = module parameter float
        real :: mpf_storageCapacitySalt_acft, mpf_storageMinimumSalt_acft
        real :: mpf_storageCapacityRoosevelt_acft,mpf_storageMinimumRoosevelt_acft
        real :: mpf_storageCapacityOther_acft, mpf_storageMinimumOther_acft
        real :: mvf_totalStorageSRPsystem

        real,private :: mvf_differenceBC
        real,private :: mvf_addToReservoir(3),mvf_potentialSaltDeficit
        real :: mvf_release
        real, private :: mvf_agWaterAndOther_acft,mvf_flowST_acft,mvf_SaltTontoClassAused_acft
        real,parameter :: mpf_ratioRoosevelt=0.81599597
        !
        real :: mpf_modLowerThreshold=0.91
        real :: mpf_upperThreshold=0.96 
        real :: mpf_AgWaterUse=250000
        real :: mpf_addFlowThreshold=0.25
        ! Monthly
        real  :: mvf_saltTontoFlowMonthly_acft(12),mvf_addFlows
        real, private :: mvf_addToReservoirMonthly(3,12)
        real, private :: mvf_usedBCMonthly_acft(12),mvf_addMonthlyBC
        real, private :: mvf_flowToOthers_acft(12)
        real, private :: mvf_flowControl,mvf_reservoirRelease
        !
        logical :: mvl_rooseveltMax,mvl_rooseveltMin,mvl_rooseveltPumpingThresh
        logical :: mvl_othersMax
        ! ======================================================================
        !
 contains
     ! ------------------------------
        ! Must be called yearly
    
    ! ----------------------------------
    subroutine aInFlow_saltTonto(T,acft)
        !
        ! ------------------- Types --------------------
        integer :: acft,i,j
        integer :: lvi_start=2000
        integer :: lvi_addDays,lvi_month,fmonthsFromDOY
        integer :: lvi_monthHold

        real :: cf

        logical :: lvl_pass=.true.
        ! =============================================
        !

        ! - Type Construct -
        type(runTime)T
        ! ==================
        !
            ! Initialize 02.19.15 DAS check to ensure they are being set
           if(T%year <=T%startyear)then
            mvf_totalStorageSRPsystem=0
            mpf_storageCapacityRoosevelt_acft=0
           endif
            !
            if(gv_dataSVT < 1)gv_dataSVT=1
            !
            if(0 < acft)then
             ! Reserved for test simulations
             mvf_saltTonto_maf_yr(T%year)=acft*1.*gpd_acftTomaf
             !
            else
           !
              if(T%year == T%startyear)then
                gvi_countYearsSalt=0
                ! Yearly array, 100 year possible
                do i = 1,100,1
                 do j = 1,3,1
                   lvf_flowSaltTonto_acft(j,i)=0
                 end do
                end do
                !
                vArecAst=1
              endif
                   if(T%year < 2013)then
                     lvi_start=2000
                    else
                     if(T%year < 2014)gvi_countYearsSalt=0
                        if(gvi_SVTtrace-1 < gvi_countYearsSalt)gvi_countYearsSalt=0
                        !
                     lvi_start=gv_indexyearSVT
                      if(1 < gv_dataSVT)lvi_start=1981
                    endif
                    ! ========================================================================
                    !
                      lvi_monthHold=1
                      lvi_month=1
                      mvf_addFlows=0
                    !
                    ! gvi_countYearsSVT must be incremented each year
                  do i = 1,gpi_dailySVTrec,1
                    mvf_SaltTontoflow_acft(i)=0
                    if(nint(gvf_flowSaltTonto(i,1)) == lvi_start+gvi_countYearsSalt)then
                      if(lvl_pass)iRecStart=i
                        yr=gvf_flowSaltTonto(i,1)
                        doy=gvf_flowSaltTonto(i,2)
                        ! units: cfs used internally
                        mvf_SaltTontoflow_acft(i)=gvf_flowSaltTonto(i,3)*gpf_ft3sToAcftDay
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
                  
                 
            endif      
                !
                ! Climate factor - gradual change over the simulation. Value
                ! input is reached by the end of the simulation cycle.
                ! -----------------------------------------------------------
                call ClimateFactorSVT(T%simyear+1,cf)
                lvf_flowSaltTonto_acft(1,vArecAst)=sum(mvf_SaltTontoflow_acft)*cf
                !
                do j = 1,12,1
                 mvf_saltTontoFlowMonthly_acft(j)= mvf_saltTontoFlowMonthly_acft(j)*cf
                end do
                !

                !
                ! Drought factor - immediate change
                ! ---------------------------------
                if(T%year >= gv_droughtyearSVT)then
                  if(T%year <=  gv_droughtyearendSVT)then
                    lvf_flowSaltTonto_acft(1,vArecAst)= lvf_flowSaltTonto_acft(1,vArecAst)*gv_droughtfactorSVT
                    do j = 1,12,1
                         mvf_saltTontoFlowMonthly_acft(j)= mvf_saltTontoFlowMonthly_acft(j)*gv_droughtfactorSVT
                    end do

                  endif
                endif
                !       
                mvf_flowSaltTonto_acft = anint( lvf_flowSaltTonto_acft(1,vArecAst))
                !
                gvd_saltTonto_maf_yr(T%year)= lvf_flowSaltTonto_acft(1,vArecAst) * gpd_acftTomaf
                !
                gvi_countYearsSalt=gvi_countYearsSalt+1
                !
                vArecAst=vArecAst+1
                !
        !
      return
    end subroutine aInFlow_saltTonto
    ! --------------------------

    ! -----------------------------
    subroutine countYears(lvi_Days)
        !
        ! ---- Types -------
        integer :: lvi_Days

        real :: fLeapYear
        ! ==================
        !
            !
            if(364 < doy)then
                   lvi_Days=fLeapYear(yr)
                  if(365 < lvi_Days)then
                      if(365 < doy)then
                        if(2012 < yr)then
                          gvi_countYearsSalt=-1
                        endif
                      endif
                  else
                      if(364 < doy)then
                        if(2012 < yr)then
                          gvi_countYearsSalt=-1
                        endif
                      endif
                  endif
            endif  
            !
        !
      return
    end subroutine countYears
    ! ------------------------

        ! --------------------------------------------
        subroutine monthlyFlow(i,dayOfYear,month,hold)
            !
            ! ----------- Types --------------
            integer :: i,month,hold,dayOfYear
            ! ================================
            !  
                !
                if(hold < month)then 
                  mvf_addFlows=0
                 mvf_addFlows=mvf_addFlows+gvf_flowSaltTonto(i,3)*gpf_ft3sToAcftDay
                else
                  if(dayOfYear < 2)mvf_addFlows=0
                 mvf_addFlows=mvf_addFlows+gvf_flowSaltTonto(i,3)*gpf_ft3sToAcftDay
                endif
                ! Create a module variable for use below
                mvf_saltTontoFlowMonthly_acft(month)=anint(mvf_addFlows)
                ! Create a global variable for use anywhere
                gvf_saltTontoMonthly_acft(month)=mvf_saltTontoFlowMonthly_acft(month)
                !
            !
         return
        end subroutine monthlyFlow
      ! ---------------------------

  ! ==================================================================================================

      ! --------------------------------
!      subroutine releaseSalt(T,Ain)
!        type(RiverA)Ain
!        type(runTime)T
!        ! ===================
!            !
!            !
!        return
!      end subroutine releaseSalt
      ! --------------------------------

    ! --------------------------------
    subroutine expectedStorageSalt(T)
        !
        ! ----------------- Types -------------------
        real :: vEvap_v
        !real, parameter :: lpf_ratioRoosevelt=0.81599597
        !real,parameter :: lpf_verdeMinimum_acft=1773
        real(8) :: fEvaporation
        ! ===========================================
        !

        ! - Type Construct -
        type(runTime)T
        ! ==================
        !
            !
            ! Correction for storage capacity
            mpf_storageCapacitySalt_acft=(li_rooseveltmax+li_stewartmountainmax+li_mormanflatmax+li_horsemesamax)*1/gpd_acftTomaf
            mpf_storageCapacityRoosevelt_acft=li_rooseveltmax*(1./gpd_acftTomaf)
            mpf_storageCapacityOther_acft= (li_stewartmountainmax+li_mormanflatmax+li_horsemesamax)* (1./gpd_acftTomaf)
            !
            ! 
            ! I have ~ 7.8295 % is minimum for total system- I estimate
            ! 158,990 AF minimum on Salt-Tonto 
            !  22,502 AF minimum on Verde (181,492 - 158,990)
            !1773=
            ! 12.06.14 changed these values
            ! -----------------------------------------------------------------
            mpf_storageMinimumRoosevelt_acft=16628
            mpf_storageMinimumOther_acft=3740
            mpf_storageMinimumSalt_acft=mpf_storageMinimumRoosevelt_acft+mpf_storageMinimumOther_acft
            !
            mvd_roosevelt_acft(2000,1)=lvd_State_Roosevelt_acft(2000)
            mvd_saltOther_acft(2000,1)=lvd_State_Others_acft(2000)

               mvf_StorageSalt_acft=0
            if(T%year == T%startyear)then
              mvf_release=0
              mvf_StorageSalt_acft=lvd_State_Salt_acft(T%year)
              !
              vEvap_v=0.
              vEvap_v=fEvaporation(lvd_State_Salt_acft(T%year))
              mvf_expectedStorageSalt_acft=lvd_State_Salt_acft(T%year)+ lvf_flowSaltTonto_acft(1,vArecAv)-(vEvap_v+mvf_release)
            else
              vEvap_v=0.
              vEvap_v=fEvaporation(lvd_State_Salt_acft(T%year-1))
              mvf_expectedStorageSalt_acft=lvd_State_Salt_acft(T%year-1)+  lvf_flowSaltTonto_acft(1,vArecAv)-(vEvap_v+mvf_release)

            endif
            !
        !
      return
    end subroutine expectedStorageSalt
    ! --------------------------------

    ! --------------------------------------------------
    subroutine normalFlowAvailableMonthly(T,i,sumDemand)
        !
        ! ----------------- Types ---------------
        integer :: i

        real :: lvf_remainingA_acft(12),flow(12)
        real :: sumDemand(12)
        ! =======================================
        !

        ! - Type Constructs -
        type(runTime)T
        ! ===================
        !
            !
              lvf_remainingA_acft(i)=0
             lvf_remainingA_acft(i)=sumDemand(i) 
            !
             flow(i)=0
            flow(i)= mvf_saltTontoFlowMonthly_acft(i) !*gvf_ratioSaltTontoFlowsPostEnv
            !
                !
                ! Globally available estimate of monthly flow
                ! -------------------------------------------------------
                gvf_flowSalt_Monthly_maf(T%year,i)=flow(i)*gpd_acftTomaf
                !
            !
             mvf_flowST_acft=0
            mvf_flowST_acft=flow(i)
             mvf_addToReservoirMonthly(1,i)=0
            !
              if(lvf_remainingA_acft(i) < flow(i))then
                ! Normal operations
                ! ----------------------
                mvf_addToReservoirMonthly(1,i)=flow(i)-lvf_remainingA_acft(i)
                !
              else
                ! Would a month here or there slip in? If so I am adjusting the arrays
                !
                if(i <= 11)then
                  ! move the difference to the next flow month
                  sumDemand(i+1)= sumDemand(i+1) + (lvf_remainingA_acft(i)-flow(i))
                else
                  sumDemand(12)=sumDemand(12) + (lvf_remainingA_acft(i)-flow(i))
                endif
                !
                mvf_addToReservoirMonthly(1,i)=0
                !
                !   should never have "remaining" class A water.
                ! -----------
                ! 03.03.15 DAS
                if(gvl_writeLog)then
                    LU=0
                    string=25
                    call sStrings(string,errorString)
                    call eWrite(errorString,LU)
                endif
             endif

            !
            gvf_saltFlowUsed(T%year)=gvf_saltFlowUsed(T%year)+lvf_remainingA_acft(i)
            !
            mvf_SaltTontoClassAused_acft=mvf_SaltTontoClassAused_acft+sumDemand(i)
            !
        !
      return
    end subroutine normalFlowAvailableMonthly
    ! ---------------------------------------

    ! -------------------------------------
    subroutine monthlyBC(T,i,sumDemand,Ain)
        !
        ! ------------- Types --------------
        integer :: i
        real :: sumDemand(12),lvf_BCused(12)
        ! ==================================
        !
        ! - Type Constructs -
        type(RiverA)Ain
        type(runTime)T
        ! ===================
            !
            ! Take BandC water from flow, first
             lvf_BCused(i)=0
             mvf_addToReservoirMonthly(2,i)=0
            !
            lvf_BCused(i)=sumDemand(i) 
            !
            ! Positive or negitive
            mvf_addToReservoirMonthly(2,i)=mvf_addToReservoirMonthly(1,i)-lvf_BCused(i)
            !
            ! Used to pass out the annual total (only)
            ! ------------------------------------------------------------------
            Ain%gvf_saltTontoBCused_acft=Ain%gvf_saltTontoBCused_acft+ lvf_BCused(i)
            !
        !
      return
    end subroutine monthlyBC
    ! -------------------------

    ! ----------------------------------------------
    subroutine storageSaltMonthly(T,i,sumDemand,Ain)
        !
        ! ---------------------------------- Types -----------------------------
        integer :: i,j
        integer :: n=10000

        real :: vEvap_a,annual
        !real :: lvf_modLowerThreshold
        real(8) :: state,initialStateO,initialStateR,lvd_initialState
        real :: sumDemand(12)
        real :: fEvaporationMonthly
        !
        real,parameter :: lpf_criteria=1.02
        real,parameter :: lpf_criteriaTarget=372765.9
        real, parameter :: lpf_minFlowControl=0.001
        !
        real :: lvf_criteria
       ! real :: lvf_upperThreshold,
        real :: lvf_UpperOther
        real :: cf,lvf_addFromRoosevelt
        real :: lvf_modCriteria,lvf_modLowerThreshold
        logical :: lvl_pass,lvl_stopHammer,lvl_stopHammerOne,lvl_stopHammerTwo
        ! =====================================================================
        !

        ! - Type Constructs -
        type(RiverA)Ain
        type(runTime)T
        ! ===================
            !
            lvl_pass=.false.
            lvl_stopHammer=.false.
            lvl_stopHammerOne=.false.
            lvl_stopHammerTwo=.false.
            mvl_othersMax=.false.
            mvl_rooseveltMax=.false.
            mvl_rooseveltMin=.false.
            mvl_rooseveltPumpingThresh=.false.
            lvf_modCriteria=1
            !
            call ClimateFactorSVT(T%simyear+1,cf)
            !
             state=0.
            state=lvd_State_Others_acft(T%year)
            !
             lvd_initialState=0
            lvd_initialState=mvd_saltOther_acft(T%year,i)
            !
             initialStateO=0
            initialStateO=state
            !
             initialStateR=0
            initialStateR=lvd_State_Roosevelt_acft(T%year)
             !
             vEvap_a=0.
            vEvap_a=fEvaporationMonthly(lvd_initialState)
             !
             mvf_agWaterAndOther_acft=sumDemand(i)
             !
             !lvf_upperThreshold=0
            !lvf_upperThreshold=0.96 
            !
            lvf_addFromRoosevelt=0
            ! Move all of the flow to the downstream reservoirs, if possible and feasible, 
            ! while maintaining Lake Roosevelt above minumum criteria
            !  initialize here
            ! -------------------
             mvf_flowControl=1.0
             mvf_reservoirRelease=0.0
            !
            ! Leave 93% of capacity in the "others" reservoirs (if possible); Value is approximate based
            ! on visual agreement of the data for the 2000-2006 period
            !
           !
            ! 01.26.17
            ! ================
              if(cf < 1)then
              else
                cf=1
              endif
            !
             lvf_modLowerThreshold=0
            lvf_modLowerThreshold=0.91*cf

            ! initialize
            mvf_addFromRoosevelt=0
            !
            lvf_UpperOther=mpf_storageCapacityOther_acft*mpf_upperThreshold
            lvf_criteria=0
            !
            do j = 1,n,1
                !
                lvf_criteria=lvf_criteria+0.001
                !
                ! Added on 01.26.17
                ! =================
                if(j > 1000)then
                 lvf_modCriteria=lvf_modCriteria+0.001
                endif
                ! ====================================
                ! End added on 01.26.17

                lvf_addFromRoosevelt=mvf_addFromRoosevelt
                call storageRooseveltMonthly(T,i,initialStateR,lvf_addFromRoosevelt,Ain)
                !
                 annual=0.
                annual=initialStateO + mvf_flowToOthers_acft(i)-vEvap_a -mvf_agWaterAndOther_acft + lvf_addFromRoosevelt
                !
                if(lvf_addFromRoosevelt <=0)then
                  if( mvf_flowControl <= lpf_minFlowControl)  lvl_pass=.true.
                endif
                ! If roosevelt at minimum, remove filling criteria on other reservoirs
                ! ---------------------------------------
                ! Huh?
               ! if(mvl_rooseveltMin .or. mvl_rooseveltMax)mpf_modLowerThreshold=1.
                ! Pre-process
                ! --------------------------------------------
                !
                if(mpf_storageCapacityOther_acft < annual)then
                      mvl_othersMax=.true.    
                    !
                    ! Both at Max, create an overflow of the dam
                    ! ==========================================
                    if(mvl_rooseveltMax)then
                      mvl_othersMax=.true.    
                        !
                       lvd_State_Roosevelt_acft(T%year)=nint(mpf_storageCapacityRoosevelt_acft)
                        Ain%gvf_overFlowSalt_acft=Ain%gvf_overFlowSalt_acft+(annual-mpf_storageCapacityOther_acft)
                        lvl_pass=.true.
                        annual=mpf_storageCapacityOther_acft
                        !
                    else
                    ! Add to Roosevelt from total water budget to reduce "Others" below max
                    ! ======================
                    lvl_stopHammerOne=.true.
                      mvf_reservoirRelease=0
                      mvf_flowControl=max(0,(mvf_flowControl*(1./lpf_criteria)) )
                        ! Added on 01.26.17
                        ! ==========================
                        if(lvl_stopHammerTwo)then
                          mvf_flowControl=max(0,(mvf_flowControl*(1./(lpf_criteria*lvf_modCriteria))) )
                        endif
                    !
                    endif
                    !

                else 
                    !   
                    mvl_othersMax=.false.
                    !
                    if(mpf_storageMinimumOther_acft < annual)then
                        ! Bring reservoirs above minimum (set at 90% of capacity)
                        if(annual < (mpf_storageCapacityOther_acft *lvf_modLowerThreshold))then
                            !
                            if(mvl_rooseveltMin)then
                             lvl_pass=.true.
                            else
                              mvf_flowControl=min(1.,mvf_flowControl*lpf_criteria)
                              if(0.9999999 < mvf_flowControl)mvf_addFromRoosevelt=lpf_criteriaTarget*lvf_criteria
                               lvl_stopHammer=.true.
                               if(lvl_stopHammerOne)lvl_stopHammerTwo=.true.
                            endif
                            !
                        else
                            !                         
                          if(mvl_rooseveltMin)then
                           lvl_pass=.true.
                          else

                            if(lvf_UpperOther < annual)then
                            ! Reduce inputs from Roosevelt
                              mvf_flowControl=max(0,mvf_flowControl*(1./lpf_criteria))
                                !
                            else
                               if(mvl_rooseveltPumpingThresh)then
                                ! Reduce Inputs from Roosevelt
                                 mvf_flowControl=max(0,mvf_flowControl*(1./lpf_criteria))
                                 if(lvl_stopHammer)then
                                  lvl_pass=.true.
                                 endif
                               else
                                lvl_pass=.true.
                               endif
                            endif
                          endif
                        endif
                    else
                      mvf_reservoirRelease= mvf_reservoirRelease*lpf_criteria
                        if(0.9999999 < mvf_flowControl)mvf_addFromRoosevelt=lpf_criteriaTarget*lvf_criteria
                        if(mvl_rooseveltMin)then
                            lvl_pass=.true.
                        endif
                    endif

                endif
                    !
                    if(mpf_storageMinimumOther_acft < annual)then
                      state = nint(annual)
                    else
                      state = nint(mpf_storageMinimumOther_acft)
                    endif
                    !
                    if(lvl_pass)exit
                    if(9998 < j)then    
   
440                   continue
!                      !   
          
                      if(gvl_writeLog)then
                        LU=0
                        string=50
                        call sStrings(string,errorString)
                        call eWrite(errorString,LU)
                      else
                      endif   
                      gvl_errorFlag=.false.
                  
                      exit
                    endif
            end do
             !
            Ain%gvf_classABCfromSaltTonto_acft=  Ain%gvf_saltTontoBCused_acft +  gvf_saltFlowUsed(T%year)
            !
             lvd_State_Others_acft(T%year)=state
            lvd_State_Salt_acft(T%year)= lvd_State_Others_acft(T%year)+lvd_State_Roosevelt_acft(T%year)
            !
             if(i == 12)then
                 mvd_saltOther_acft(T%year+1,1)=state
             else
                 mvd_saltOther_acft(T%year,i+1)=state
             endif
            !
            mvf_totalStorageSRPsystem= lvd_State_Salt_acft(T%year)+ go_VerdeStorage_acft
        !
      return
    end subroutine storageSaltMonthly
    ! -------------------------------
    
    ! -------------------------------------------------
    subroutine storageRooseveltMonthly(T,i,initial,lvf_addFromRoosevelt,Ain)
        !
        !
        ! ------------------ Types ---------------------
        integer :: i,k

        real :: annual
        real(8) :: state,initial,vEvap_a,lvd_initialState
        real :: lvf_available
        real, parameter :: lpf_minFlowControl=0.001
        real :: fEvaporationMonthly
        real :: lvf_addFromRoosevelt

        logical :: lvl_break
        ! ==============================================
        !

        ! - Type Constructs -
        type(RiverA)Ain
        type(runTime)T
        ! ===================
            !
             state=0.
             lvd_initialState=0
            lvd_initialState=mvd_roosevelt_acft(T%year,i)
            !
            state=initial
             !
             vEvap_a=0.
            vEvap_a=fEvaporationMonthly(lvd_initialState)
            !
             lvf_available=0
            lvf_available=mvf_addToReservoirMonthly(2,i)
            !
            ! 04.04.13- added the if block- flow < BandC demand {must come from storage}
            ! -----------------------
             mvf_flowToOthers_acft(i)=0
            if(mvf_addToReservoirMonthly(2,i) < 0)then
                mvf_flowControl=0
            endif
            !
            lvl_break=.false.
            !
            do k = 1,1000,1
                !
                if(mvf_flowControl < lpf_minFlowControl)then
                    lvl_break=.true.
                endif
                 mvf_flowToOthers_acft(i)=0
                mvf_flowToOthers_acft(i)=max(0.,mvf_flowControl*lvf_available) +  mvf_reservoirRelease
                !
                 annual=0.
                annual=state + lvf_available -(mvf_flowToOthers_acft(i)+vEvap_a+lvf_addFromRoosevelt)
                !
                !annual=max(mpf_storageMinimumRoosevelt_acft,annual)
                if(mpf_storageCapacityRoosevelt_acft < annual)then
                    !
                    if(mvl_othersMax)then
                        ! Pass this along to SRP release
                        Ain%gvf_overFlowSalt_acft=Ain%gvf_overFlowSalt_acft+(annual-mpf_storageCapacityRoosevelt_acft)
                        lvl_break=.true.
                    else
                        !
                        Ain%gvf_overFlowSalt_acft=0
                        mvf_reservoirRelease=mvf_reservoirRelease+10*k
                        !
                     endif
                    !
                    state=nint(mpf_storageCapacityRoosevelt_acft)
                    mvl_rooseveltMax=.true.
                    !
                else
                    if(mpf_storageMinimumRoosevelt_acft < annual)then
                        ! At minimum storage when deliveries are
                        lvl_break=.true.                      
                        !
                        state = nint(annual)
                        !
                    else
                      state=nint(mpf_storageMinimumRoosevelt_acft)
                      mvl_rooseveltMin=.true.
                      lvl_break=.true.
                    endif
                    !
                endif
                !
                if(lvl_break)exit
                !
            end do
            !
            lvd_State_Roosevelt_acft(T%year)=state
            !
            if(i == 12)then
                 mvd_roosevelt_acft(T%year+1,1)=state
            else
                 mvd_roosevelt_acft(T%year,i+1)=state
            endif
            !
         return
      end subroutine storageRooseveltMonthly
      ! ------------------------------------

     ! --------------------------------
      subroutine initializeLoop(T,Ain)
            !
            ! - Type Constructs -
            type(RiverA)Ain
            type(runTime)T
            ! ===================
            !
                !
                gvf_saltFlowUsed(T%year)=0.
                Ain%gvf_saltTontoBCused_acft=0.
                mvf_SaltTontoClassAused_acft=0.
                Ain%gvf_overFlowSalt_acft=0
                !
            !
        return
      end subroutine initializeLoop
      ! ----------------------------

      ! --------------------------------
!      subroutine transferOut(T,Ain)
!           ! - Type Constructs -
!            type(RiverA)Ain
!            type(runTime)T
!            ! ========================
!                !
!                !
!        return
!      end subroutine transferOut
      ! -------------------------

      ! ---------------------------------
      subroutine outputsToInterface(T,Ain)
            !
            !  -- Type Constructs ----
            type(RiverA)Ain
            type(runTime)T
            ! ========================
                !
                 go_saltOtherStorage_acft_a=0
                go_saltOtherStorage_acft_a=lvd_State_Others_acft(T%year)
                 go_rooseveltStorage_acft_a=0
                go_rooseveltStorage_acft_a=lvd_State_Roosevelt_acft(T%year)
                 go_SaltTontoStorage_acft=0
                go_SaltTontoStorage_acft=nint(lvd_State_Salt_acft(T%year))
                 go_SaltTontoClassAused_acft=0
                go_SaltTontoClassAused_acft=nint(mvf_SaltTontoClassAused_acft)
                 go_SaltTontoClassBCused_acft=0
                go_SaltTontoClassBCused_acft= nint(Ain%gvf_saltTontoBCused_acft)
                 go_saltTontoRiverFlow_acft=0
                go_saltTontoRiverFlow_acft= nint(mvf_flowSaltTonto_acft)
                !
                ! Sent to function getSVRiverFlow() in KernelInterface.f90
                 go_riverFlowSVT=0
                go_riverFlowSVT=   go_verdeRiverFlow_acft + go_saltTontoRiverFlow_acft
                !
            !
        return
      end subroutine outputsToInterface
      ! -------------------------------

      ! -------------------------------
        subroutine validate(T,i)
            !
            ! --Types ---
            integer :: i
            ! ===========
            !
            
            ! - Type construct -
            type(runTime)T
            ! ==================
                ! 
                if(gpl_comparisons)then
                  if(T%year < 2013)then
                   write(102,*)T%year,i,lvd_State_Others_acft(T%year)*(1./1000),lvd_State_Roosevelt_acft(T%year)*(1./1000)
                  endif                      
                endif
                !
            !
          return
        end subroutine validate
        ! -----------------------

        ! -----------------------
        subroutine updateState(T)
            !
            ! - Type Construct -
            type(runTime)T
            ! ==================
            !
                if(gpl_validate)then
                   lvd_State_Others_acft(2006)=308823
                   lvd_State_Roosevelt_acft(2006)=1028053
                endif
                !
                lvd_State_Roosevelt_acft(T%year+1)=lvd_State_Roosevelt_acft(T%year)
                lvd_State_Others_acft(T%year+1)= lvd_State_Others_acft(T%year)
                lvd_State_Salt_acft(T%year+1)= nint(lvd_State_Salt_acft(T%year))
            !
        return
      end subroutine updateState
      ! ------------------------

    ! ----------------------
    subroutine movingAveage(T,lvf_runningRoosevelt)
        ! ------------ Types -------
        real :: lvf_runningRoosevelt

        !
        ! ---- Type C ------
        type(runTime)T
        ! ==================
            !
            if(gvl_start)lvf_runningRoosevelt=0
            if(2000 < T%year)then
              lvf_runningRoosevelt=(lvd_State_Roosevelt_acft(T%year) &
                + lvd_State_Roosevelt_acft(T%year-1)) /2
            else if(2001 < T%year)then
              lvf_runningRoosevelt=(lvd_State_Roosevelt_acft(T%year) &
                + lvd_State_Roosevelt_acft(T%year-1) &
                + lvd_State_Roosevelt_acft(T%year-2))/3
            else if(2002 < T%year)then
              lvf_runningRoosevelt=(lvd_State_Roosevelt_acft(T%year) &
                + lvd_State_Roosevelt_acft(T%year-1) &
                + lvd_State_Roosevelt_acft(T%year-2) &
                + lvd_State_Roosevelt_acft(T%year-3))/4 
            else if(2003 < T%year)then
              lvf_runningRoosevelt=(lvd_State_Roosevelt_acft(T%year) &
                + lvd_State_Roosevelt_acft(T%year-1) &
                + lvd_State_Roosevelt_acft(T%year-2) &
                + lvd_State_Roosevelt_acft(T%year-3) &
                + lvd_State_Roosevelt_acft(T%year-4))/5
            endif
        !
     return
    end subroutine movingAveage
    ! ----------------------
End Module lms_SRPsaltTonto

    ! --------------------------------------
    subroutine adjustedFlowSaltTonto(T,acft)
        use lms_SRPsaltTonto
        !
        ! -- Types ----
        integer :: acft
        ! =============
        !
        ! - Type Construct -
        type(runTime)T
        ! ==================
        !
            !
            call aInFlow_saltTonto(T,acft)
            !
        !
      return
    end subroutine adjustedFlowSaltTonto
    ! -----------------------------------

    ! --------------------------------
    subroutine initializeNormalFlow(T)
      use lms_SRPsaltTonto
!       use gm_ModelControl
!        use gm_TypeControl
        ! ------------ types ---------
        real :: cf
        ! ============================
        !

        ! -- type construct --
        type(runTime)T
        ! ====================
            !
            ! ==============================
            if(gpl_runSensitivity)then
                call ClimateFactorSVT(T%simyear+1,cf)
                mpf_modLowerThreshold=gpf_modLowerThreshold*cf
                mpf_upperThreshold=gpf_upperThreshold
                mpf_AgWaterUse=gpf_AgWaterUse
                mpf_addFlowThreshold=gpf_addFlowThreshold
            endif
            !
        ! ----
      return
    end subroutine initializeNormalFlow
    ! ---------------------------------

    ! ------------------------------------
    subroutine normalFlowSaltTonto(T,Aout)
      use lms_SRPsaltTonto
       use gm_ModelControl
        use gm_TypeControl
        !
        ! ------------------- Types --------------------------
        integer :: i

        real :: TotDemand,normal_A_acft(12),normal_BC_acft(12)
        real :: normal_ag_acft(12),lvf_ratioStorage
        real :: lvf_demandFromAg,lvf_addFlowToAgRatio,lvf_constAgDemand
        real(8) ::lvf_flowAnnual
        real :: lvf_shiftDemandFromAg(12),lvf_movingAvg
        real :: af,lvf_totalStorage
        real :: fSTVerdeReservoirF,lvf_responseSTVerde
       ! real,parameter :: lpf_Ag=250000

        logical :: lvl_passOut
        ! ====================================================
        !

        ! - Type Contructs -
        type(RiverA)Aout
        type(runTime)T
        ! ==================
            !
            lvl_passOut=.true.
            !
            call initializeNormalFlow(T)
            !
             lvf_flowAnnual=0
            lvf_flowAnnual=gvd_saltTonto_maf_yr(T%year)*(1/gpd_acftTomaf)
             !
             lvf_totalStorage=0
            lvf_totalStorage=mvf_totalStorageSRPsystem * (1./1000)
             ! Reduce the Ag committment based on retirement factor, 
             ! because we still want to meet Ag demand- do not know
             ! what the correction factor should be.. 12.10.14 das
             !
            call agRetirement(T%simyear,af)
             !
             lvf_responseSTVerde=fSTVerdeReservoirF(lvf_totalStorage)
             lvf_constAgDemand= max(0,lvf_responseSTVerde* mpf_AgWaterUse)
             !
            call movingAveage(T,lvf_movingAvg)
             !
             lvf_ratioStorage=0
            if(0 < mpf_storageCapacityRoosevelt_acft)lvf_ratioStorage=(0.5*(lvf_movingAvg/mpf_storageCapacityRoosevelt_acft))
            !
             lvf_addFlowToAgRatio=0.0
            if(mpf_addFlowThreshold < lvf_ratioStorage)lvf_addFlowToAgRatio=lvf_ratioStorage
            !
             lvf_demandFromAg=0
            lvf_demandFromAg=lvf_constAgDemand +lvf_addFlowToAgRatio*lvf_flowAnnual
            !
              TotDemand=0
             TotDemand= Aout%gvf_verdeToSaltTranClassA_AF
            if(0 < TotDemand)then
            !
1           continue 
              call monthlyDemand(lvl_passOut,gvf_janUsePropOfTotal,TotDemand,normal_A_acft)
              call checkClassAflowAvailable(gvf_saltTontoMonthly_acft,normal_A_acft,lvl_passOut)
            !
             if(gvf_janUsePropOfTotal < 0.46)goto 2
             if(.not. lvl_passOut)goto 1
             !
2           continue
            else
                do i = 1,12,1
                  normal_A_acft(i)=0.
                end do
            endif
            !
             TotDemand= Aout%gvf_verdeToSaltTranClassBC_AF
            if(0 < TotDemand)then
             call monthlyDemand(lvl_passOut,gvf_janUsePropOfTotal,TotDemand,normal_BC_acft)
            else
                do i = 1,12,1
                  normal_BC_acft(i)=0.
                end do
            endif
            !
             TotDemand= 0
            TotDemand=lvf_demandFromAg
            if(0 < TotDemand)then
              call monthlyDemand(lvl_passOut,gvf_janUsePropOfTotal,TotDemand,normal_ag_acft)
            else
                do i = 1,12,1
                  normal_ag_acft(i)=0.
                end do
            endif
                !
                lvf_shiftDemandFromAg=CSHIFT(normal_ag_acft,SHIFT=-6)
                !
              normal_ag_acft=lvf_shiftDemandFromAg
            !
             call initializeLoop(T,Aout)
             call expectedStorageSalt(T)
            !
            do i = 1,12,1
              call normalFlowAvailableMonthly(T,i,normal_A_acft)
               call monthlyBC(T,i,normal_BC_acft,Aout)
               !
              call storageSaltMonthly(T,i,normal_ag_acft,Aout)
             call validate(T,i)
            end do
            !
            call updateState(T)
            call outputsToInterface(T,Aout)
            !
      return
    end subroutine normalFlowSaltTonto
    ! --------------------------------


!
! ======================================================================================================
! E.O.F. WaterShed_SaltTonto.f90
