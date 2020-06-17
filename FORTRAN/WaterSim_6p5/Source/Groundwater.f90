!
! File is Groundwater.f90
!
!   This Module models the County-scale groundwater pool and fluxes.  And, it controls
! some of the provider balance-pumping for municipal water providers for the provider-level
! model.  Called from the kernel (call pProviderGroundwater(T,gvi,vPV)).
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
! Module:       Module lm_Groundwater_A
! Subroutines:  subroutine initGW()
!                 calls(locally):
!                   call openFiles_g()
!                   call readParms_g()
!                   call readFiles_g()
!                   call initialState_g()
!
! No Module:    subroutine initializeGWaterData()
!
! Module:       Module lms_Groundwater_A 
! Subroutines:  subroutine initState_g(T)
!               subroutine deltaState_g(T,vGW,lv_demand,overf,acft)
!               subroutine outinitialG(T) 
!
! No Module:    subroutine sInitialGroundwater(T)
!               subroutine pProviderGroundwater(T,gvi_order,vPV)
!

! Global OUTPUTS:  
!
! Local OUTPUTS:
!               
 
! Local INPUTS:
!
! created on 10.01.09
!
! david arthur sampson

! last write was: 08.16.13,07.18.14
! ---------------------------------
!

! =================================================================================================
!
Module lm_Groundwater_A
 use gm_ModelControl
  use gm_GlobalData
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
  contains
        !
        ! -------------------
        subroutine initGW()
          call openFiles_g()
          call readParms_g()
          call readFiles_g()
          call initialState_g()
         return
        end subroutine initGW
        ! -------------------

        ! ----------------------
        subroutine openFiles_g()
            !
            ! --------- Types ------------------
            character(len=200) :: lvc_DPath=' '
            ! ==================================
            !
                if(gpl_release)then
                    lvc_DPath=trim(gvc_DPath)
                else
                    lvc_DPath=gvc_Path
                endif
                !
                module="lm_SRPsaltTonto"
               !
               Infile='App_Data\Data\cagrd.txt'; LU=18
               call openFiles(module,lvc_DPath,Infile,LU)
               !
               Infile='App_Data\Parameters\GWdesignations.txt'; LU=33
               call openFiles(module,lvc_DPath,Infile,LU)
               !
               Infile='App_Data\Parameters\parm_GW.dat'; LU=34
               call openFiles(module,lvc_DPath,Infile,LU)
           !
         return
        end subroutine openFiles_g
        ! ------------------------

        ! ----------------------
        subroutine readFiles_g()
            !   
            ! ---- Types ------
            integer :: i,j,LU
            integer :: ios
            ! =================
            !
            !  Read unit 17 - CAGRD data.  Convert from ac-ft to maf
            !-----------------------------------------------------------------  
            !
            LU=18
            read(LU,*,err=8,iostat=ios)(li_cagrdRecharge(i),i=2000, gpi_uBY)      ! acft a-1
            do j = 2000, gpi_uBY,1
            li_cagrdRecharge(j)=li_cagrdRecharge(j)*gpd_acftTomaf      
           ! maf a-1
            end do
8           continue
            close(18)
            if(ios >0)then
            goto 1000
            endif
            !
            LU=33
            ! Array has three columns.  They are;
            !  Annual credits: total credits: other annual
            do i = 1,gvi_Providers,1
              gvf_otherAnnualGWcredits_acft(2000,i)=0
              gvf_annualCredits_acft(2000,i)=0
              gvd_CreditModel_acft(2000,i)=0
             read(LU,*,err=9,iostat=ios)gvi_designationsGW(i,1),gvi_designationsGW(i,2),gvi_designationsGW(i,3)
              !
              gvf_otherAnnualGWcredits_acft(2000,i)=gvi_designationsGW(i,3)
              gvf_annualCredits_acft(2000,i)=gvi_designationsGW(i,1)
              gvd_CreditModel_acft(2000,i)=gvi_designationsGW(i,2)
              !
            end do
            
9           continue
            close(33)
            if(ios >0)then
            LU=33
            goto 1000
            endif
         return
1000       continue
                if(gvl_writeLog)then
                  string=15
                    call sStrings(string,errorString)
                    call eWrite(errorString,LU)
                endif
            gvl_errorFlag=.false.
            !
          return
         end subroutine readFiles_g
        ! ------------------------

        ! ----------------------
        subroutine readParms_g()
            !
            read(34,*)li_absorptionoverflow
            read(34,*)li_absorptionoverflow
            read(34,*)li_accesstosurfacewater
            read(34,*)li_initialgroundwater
            read(34,*)gvl_rechargeAWBA
            read(34,*)gvl_rechargeCAGRD
            read(34,*)gvl_rechargeNatural
           close(34)
         return
        end subroutine readParms_g
        ! ------------------------

        ! ---------------------------
        subroutine initialState_g()
            !~
             vState_GWmodel_maf(2000)=li_initialgroundwater  
            !~
          return
        end subroutine initialState_g
        ! ---------------------------
!
End Module lm_Groundwater_A
!
    ! ----------------------------------
    subroutine initializeGWaterData()
      use lm_Groundwater_A
        !
        call initGW()
        !
      return
    end subroutine initializeGWaterData
    ! ----------------------------------
!
! =================================================================================================
!
Module lms_Groundwater_A
 use gm_ModelControl
  use gm_GlobalData
   use gm_TypeControl
        !
        ! -----
        include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
        !

            ! ----------------- Module Type Constructs ------------------------------
            type groundwater
             real(8) :: lv_rechargeNatural,lv_GWDifference(gpi_lBY:gpi_uBY)
             real(8) :: lv_GWRecharged(gpi_lBY:gpi_uBY),lv_GWPumped(gpi_lBY:gpi_uBY)
             real(8) :: lv_rechargeNatural_monthly(12)
            end type groundwater
            ! =======================================================================
        !
   contains
        ! -----------------------
        subroutine initState_g(T)
            ! -- Type Construct -
            type(runTime)T
            ! ===================
                !
                if(T%year == T%startyear)then
    !           vState_GWmodel_maf(T%year)=li_initialgroundwater    ! maf
                endif
            !
          return
        end subroutine initState_g
        !---------------------------

      ! -----------------------------------------------------
      subroutine deltaState_g(T,vGW,lv_demand,overf,acft)

        ! --------------------- Types -------------------------------------
        integer :: fLogicalToInteger
        integer :: acft

        real(8) :: lv_demand,overf
        real(8) :: lv_surfacedemand
        real(8) :: lv_directGroundwaterDemand 
        real(8) :: lv_AwbaRecharge,lv_rechargeCAGRD,lv_rechargeobligations
        real(8) :: lv_demandMinusSupply,lv_excess,lv_directPumping
        real(8) :: lv_overflowRechargeRiver_A
        real(8) :: lv_municiplePumping
        ! ===================================================================
        !

        ! - Type Constructs -
        type(groundwater)vGW
        type(runTime)T
       ! ====================
        !
         lv_surfacedemand=(li_accesstosurfacewater)*lv_demand    
         lv_directGroundwaterDemand=(1.-li_accesstosurfacewater)*lv_demand
        !
        if(fLogicalToInteger(gvl_rechargeAWBA) == 1)lv_AwbaRecharge=0.150218 
        if(fLogicalToInteger(gvl_rechargeCAGRD) == 1)then
          if(0 < gii_CAGRD_acft)then
              lv_rechargeCAGRD=gii_CAGRD_acft
          else
            lv_rechargeCAGRD=li_cagrdRecharge(T%year)
          endif
        else
         lv_rechargeCAGRD=0.
        endif
        !
         lv_excess=0
         lv_rechargeobligations=0
         lv_municiplePumping=0.
        lv_rechargeobligations=lv_AwbaRecharge+lv_rechargeCAGRD
         ! acft = maricopa county surface water (acft a-1)
        lv_demandMinusSupply=(lv_surfacedemand+lv_rechargeobligations)-(acft*gpd_acftTomaf)

        if(lv_demandMinusSupply > 0)then
          lv_municiplePumping=lv_demandMinusSupply
        else
          lv_excess=abs(lv_demandMinusSupply)
        endif
        !
         lv_directPumping=0
        lv_directPumping=lv_directGroundwaterDemand
        vGW%lv_GWPumped(T%year)=lv_directPumping+lv_municiplePumping
         lv_overflowRechargeRiver_A=0.
        lv_overflowRechargeRiver_A=overf*li_absorptionoverflow             ! Overflow from the SVT - or "of" (acft a-1)
        !
        if(fLogicalToInteger(gvl_rechargeNatural) == 1)then

        else
          vGW%lv_rechargeNatural=0.
        endif
        !
        vGW%lv_GWRecharged(T%year)=lv_excess+vGW%lv_rechargeNatural+lv_overflowRechargeRiver_A+lv_rechargeobligations
        !
        return
       100 stop
      end subroutine deltaState_g
      !--------------------------

    !------------------------
    subroutine outinitialG(T) 
      ! - Type Construct --
      type(runTime)T
      ! ===================
        !
       if(T%year /= T%startyear)then
       else
       endif
     return
    end subroutine outinitialG
    !-------------------------
!
End Module lms_Groundwater_A
!
 ! -------------------------------
 subroutine sInitialGroundwater(T)
  use gm_ModelControl
   use lms_Groundwater_A
    !
        ! - Type Construct -
        type(runTime)T
        ! ==================
        !
         call initState_g(T)
        !
      return
    end subroutine sInitialGroundwater    
   !---------------------------

    ! Called from the Kernel.f90
    ! ----------------------------------------------
    subroutine pProviderGroundwater(T,gvi_order,vPV)
      use gm_ModelControl
       use lms_Groundwater_A
        !
        ! ------------------------- Types ------------------------------------
          integer(1) :: gvi_order
          integer :: i,lvi_countSRP
          !integer :: lvi_AgRequest(gvi_maxProV)

          ! Units in AF or AF annum-1
          real  :: lvf_WaterDemand35(gvi_maxProV),gvf_usedGW(gvi_maxProV)
          real :: lvf_GW(gvi_maxProV)
          real :: lvf_addGroundwaterFromAg(gvi_maxProV)
          real :: code=gpi_groundWater
         
          real :: lvf_availSRP,lvf_capAvailable,lvf_usedSRP
          real :: lvf_runningAnnualCredits_acft(gvi_maxProV)
          real:: lvf_creditWeight(gvi_maxProV)
        ! =====================================================================

        ! - Type Constructs ---
        type(runTime)T
        type(provider) :: vPV        
        ! =====================
           !
            !  Take water credits from Agriculture to give to municipal water providers
            !  Running groundwater credits NEW Code. 01.29.15 DAS
            !  call sAgGroundWater(T, lvf_addGroundwaterFromAg)
            !
              call sMaskCAP(gvl_maskCAP)
              call sMaskSRP(gvl_maskSRP)
                !
                lvi_countSRP=1
                !
                ! 09.04.16
                call AgAcerageAndCredits(T,lvf_creditWeight)

             ! Main loop
             do i = 1,gvi_Providers,1
              gvf_usedGW(i)=0
              lvf_runningAnnualCredits_acft(i)=0
              !
                !
                ! --------------------------------------------
                lvf_addGroundwaterFromAg(i)=0
              call sAgGroundWater(T,i,lvf_creditWeight,lvf_addGroundwaterFromAg)
              !
              ! Send to City_Model line 3144
              gvf_addGroundwaterFromAg(i)=lvf_addGroundwaterFromAg(i)
             ! ----------------------------------------------------------------
                ! Want an output for Ag water credits added to Urban
                
                ! These are for the 80% rule (see below)
                 lvf_availSRP=0
                 lvf_usedSRP=0
                if(gvl_maskSRP(i))then
                  lvf_availSRP=gvf_availableSRP_acft(lvi_countSRP)
                  lvf_usedSRP=gvf_WaterDemand_acft(i,8,5)
                  !
                  lvi_countSRP=lvi_countSRP+1
                endif 
                !
                !   Pass in added credits to the credit bucket for each provider (from Water_CItyModel.f90)
                ! subroutine sVadose.  05.02.12- variable is added to gvd_CreditModel_acft(T%year,i)
                ! in  subroutine sCreditBalance(T,i,vTstep) in Water_CityModel.f90
                
                    ! Prior to 09.21.15
                    ! 09.30.15 I changed the code so it calculated added every year. Prior to today
                    ! It only calculated it once; in the year 2000
                    if(T%startyear < T%year)then
                     gvf_addedAnnualGWcredits(i)=max(0,(gvf_annualCredits_acft(2000,i) -gvd_CreditModel_acft(T%year,i)* (1/100.)))
                    else                  
                     gvf_addedAnnualGWcredits(i)=max(0,(gvf_annualCredits_acft(2000,i) -gvd_CreditModel_acft(2000,i)* (1/100.)))
                    endif
                    !
                    ! Column Three in the GWdesignations file
                    gvf_otherAnnualGWcredits_acft(T%year,i)=gvf_otherAnnualGWcredits_acft(2000,i)
                    !
                    ! 09.21.15 DAS
!                    gvf_addedAnnualGWcredits(i)=gvf_annualCredits_acft(2000,i)

                    ! Accept inputs from the Interface
                    if(0 < gvf_setAnnualCredits_acft(i))gvf_addedAnnualGWcredits(i)=gvf_setAnnualCredits_acft(i)
                 !
                 ! 100-year annual estimate
                 lvf_runningAnnualCredits_acft(i)=gvd_CreditModel_acft(T%year,i)* (1/100.)
                 !
                 ! --------------------------------------------------------------------------------------------
                 gvf_totalAnnualCredits_acft(T%year,i)=0
                ! in subroutine sCreditBalance()
                gvf_totalAnnualCredits_acft(T%year,i)= max(0,lvf_runningAnnualCredits_acft(i))
                !
                ! Send to the interface 05.14.12
                ! ------------------------------------------------------------------
                  go_annualCredits_acft(i)=0
                go_annualCredits_acft(i)=nint(gvf_totalAnnualCredits_acft(T%year,i))
                 !
                 ! 03.01.14
                 go_addedCreditsAgToMuni_acft(i)=0
                go_addedCreditsAgToMuni_acft(i)= nint(lvf_addGroundwaterFromAg(i))
                !
                 go_totalAddedCredits_acft(i)=0
                go_totalAddedCredits_acft(i)=go_addedCreditsAgToMuni_acft(i)+gvf_addedAnnualGWcredits(i)+gvf_otherAnnualGWcredits_acft(T%year,i)
                ! 
                  !
                    lvf_WaterDemand35(i)=0
                   lvf_WaterDemand35(i)=gvf_WaterDemand_acft(i,gvi_order,1)
                if(0 < lvf_WaterDemand35(i))then
                  if(lvf_WaterDemand35(i) <=  gvd_CreditModel_acft(T%year,i))then
                    if(0 <  gvf_totalAnnualCredits_acft(T%year,i))then
                      if(lvf_WaterDemand35(i) <  gvf_totalAnnualCredits_acft(T%year,i))then
                        gvf_usedGW(i)=lvf_WaterDemand35(i)
                      else
                        !
                        gvf_usedGW(i)=  gvf_totalAnnualCredits_acft(T%year,i)
                        !
                        !   03.07.12 DAS if surface water availalbility hits 80% of needs,
                        ! designations no longer apply
                        !   I changed (i,1,3) to (i,7,5), i.e., demand when "A" is called (used A)
                        ! I added (i,8,5) which is class BandC water (used) NOTE: as of 07.06.12 using
                        ! only BandC designations
                            !
                            ! 07.05.12 DAS
                            ! --------------------------------------------------------------------------
                            if(gvl_maskSRP(i) .or. gvl_maskCAP(i))then
                                !
                                 lvf_capAvailable=0
                                lvf_capAvailable=lid_designationsCAP(i,1) + lid_designationsCAP(i,2)
                                !
                                ! 07.06.12 DAS
                                ! On-project demand NOT met by BandC water
                                if(0 < gvf_WaterDemand_acft(i,9,3))then
                                else
                                    ! Demands met, so why invoke shortage conditions?
                                    lvf_usedSRP=lvf_availSRP
                                endif
                                !
                              call sEightyPercent(lvf_usedSRP,lvf_availSRP, &
                                vPV%gRealizedDesignationCAP_acft(T%year,i),lvf_capAvailable,gvl_removeGWRestrictions(i))
                                !
                             if(gvl_removeGWRestrictions(i))then
                                gvf_usedGW(i)= lvf_WaterDemand35(i)
                             endif
                            !
                            if(gvl_removeGWRestrictions(i))gvi_eightyPercentRule(i)=1
                           endif
                            !
                            ! --------------------------------------------------------------------------

                        !
                      endif
                    else
                        ! No designation for this particular water provider
                        ! Likely never reached. 05.10.12
                        gvf_usedGW(i)= lvf_WaterDemand35(i)
                        !
                    endif
                    !
                  else  ! --------------------------------------------------------
                        ! Credit bucket empty- going netative
                    !
                    if(lvf_WaterDemand35(i) < gvf_totalAnnualCredits_acft(T%year,i))then
                        gvf_usedGW(i)= lvf_WaterDemand35(i)
                    else
                        gvf_usedGW(i)= gvf_totalAnnualCredits_acft(T%year,i)
                    endif
                    ! ------------------------------------------------------------
                  endif
                  !
                  ! Does this work ok here? 07.06.12- seems to work fine
                    ! 06.29.12 DAS
                  if(gvl_parmAllStrawsSucking)then
                    !
                    gvf_usedGW(i)= lvf_WaterDemand35(i)
                    !
                  endif
                 !
                else
                 ! Demand at this point is < 0
                endif
                !
                ! Annual demand exceeds their 100 year AWS credit amount
                ! --------------------------------------------------------
                  gvf_deficitGWcredits_acft(i)=0.
                 gvf_deficitGWcredits_acft(i)= (min(0.,gvf_totalAnnualCredits_acft(T%year,i)-lvf_WaterDemand35(i)))
                !
                !
                !   May move gvf_usedGWater_acft(i) to "physical groundwater" in the future
                ! Use (retain) gvf_usedGW(i) as the credit model accounting variable
                gvf_usedGWater_acft(i)= gvf_usedGW(i)
                !
              end do
                !
              ! ======================
              do i = 1,gvi_Providers,1
                !
                 lvf_GW(i)=0
                ! 10.01.15
                ! Already account for minimum pumping in the previous call in kernel
                !
                lvf_GW(i)=gvf_usedGW(i)
                gvf_usedGWater_acft(i)=  gvf_usedGWater_acft(i)+gvf_minimumPumpingMandI(i)
                !
              end do

!                lvf_demand=gvf_WaterDemand_acft(i,gvi_order,1)
                ! Total demand > 0
!                if(0 < lvf_demand)then
!                    ! Check on-project first
!                   if(0 < gvf_WaterDemand_acft(i,gvi_order,3))then
!                     !
!                     if( 1 < gvf_WaterDemand_acft(i,gvi_order,3))then
!                      lvf_propDemand=gvf_WaterDemand_acft(i,gvi_order,3) * (1./lvf_demand)
!                      ! 
!                      lvf_on(i)=lvf_propDemand* lvf_GW(i)
!                      lvf_off(i)=lvf_GW(i)-lvf_on(i)
!                     else
!                       ! give the 1 AF demand to off-project
!                      lvf_on(i)=0
!                      lvf_off(i)=lvf_GW(i)
!                     endif
!                   else
!                    lvf_on(i)=0
!                    lvf_off(i)=lvf_GW(i)
!                   endif
!                else
!                   lvf_on(i)=0
!                   lvf_off(i)=0
!                endif
              !


!              end do
             !
!              call sCWaccounting(gvi_order,lvf_off,lvf_on,code)
              gvl_InOutDemandArray=.false.
              call sCWaccountingIndoorOut(gvi_order,code,lvf_GW,gvl_InOutDemandArray)

            ! 
            gvi_order=gvi_order+1
            !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=18
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
            !
      return
    end subroutine pProviderGroundwater
    ! ----------------------------------

 ! Called from the Kernel.f90
    ! ----------------------------------------------
    subroutine pProviderDefaultGroundwater(T,gvi_order)
      use gm_ModelControl
       use lms_Groundwater_A
        !
        ! ------------------------- Types ------------------------------------
          integer(1) :: gvi_order
          integer :: i

          ! Units in AF or AF annum-1
          real :: lvf_GW(gvi_maxProV)
          real :: code=gpi_defPump
          real :: lvf_minimumPumpingMandI(gvi_maxProV)
        ! =====================================================================

        ! - Type Constructs ---
        type(runTime)T
        ! =====================
            !
             ! ======================
              do i = 1,gvi_Providers,1
                !
                ! -----------------------------------------------------------------
                !
                lvf_minimumPumpingMandI(i)=(gvi_defaultMandIPumping_Pct(i)*0.01)*gvf_WaterDemand_acft(i,1,1)
                !
                ! ------

                 ! 07.30.15 DAS
                 ! minumum M&I pumping to sustain their wells
                 ! -------------------------------------------
                 gvf_minimumPumpingMandI(i)=lvf_minimumPumpingMandI(i)
                 !04.08.16 ???
                 !gvf_usedGWater_acft(i)=  gvf_minimumPumpingMandI(i)
                 ! -------------
!                 lvf_propDemand=0
!                 lvf_GW(i)=0
                lvf_GW(i)=lvf_minimumPumpingMandI(i)
!                 lvf_on(i)=0
!                 lvf_off(i)=0
!
!                 lvf_propDemand=gvf_WaterDemand_acft(i,1,3) * (1./ gvf_WaterDemand_acft(i,1,1))
!                 lvf_on(i)=lvf_GW(i)*lvf_propDemand
!                 lvf_off(i)=lvf_GW(i)-lvf_on(i)
               !
              end do
             !
!              call sCWaccounting(gvi_order,lvf_off,lvf_on,code)
                gvl_InOutDemandArray=.false.
              call sCWaccountingIndoorOut(gvi_order,code,lvf_GW,gvl_InOutDemandArray)

            ! 
            gvi_order=gvi_order+1
            !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=24
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
            !
      return
    end subroutine pProviderDefaultGroundwater
    ! ----------------------------------------

    ! ----------------------------------------
    subroutine ModFlowGroundwater(vInitGW2006)
           real(4) :: vInitGW2006(35)
            ! Remove this once MODFLOW get connected 11.24.09 das
            !
            vInitGW2006(1)= 472717
            vInitGW2006(2)= 27181
            vInitGW2006(3)= 1402444
            vInitGW2006(4)= 481010
            vInitGW2006(5)= 119409
            vInitGW2006(6)= 519198
            vInitGW2006(7)= 35333
            vInitGW2006(8)= 110742
            vInitGW2006(9)= 0
            vInitGW2006(10)=10291
            vInitGW2006(11)=4212952
            vInitGW2006(12)=0
            vInitGW2006(13)=222185
            vInitGW2006(14)=45556
            vInitGW2006(15)=78967
            vInitGW2006(16)=790266
            vInitGW2006(17)=2854934
            vInitGW2006(18)=2433664
            vInitGW2006(19)=451139
            vInitGW2006(20)=1274914
            vInitGW2006(21)=7629169
            vInitGW2006(22)=41109921
            vInitGW2006(23)=6054963
            vInitGW2006(24)=1637063
            vInitGW2006(25)=7577589
            vInitGW2006(26)=743250
            vInitGW2006(27)=72061
            vInitGW2006(28)=0
            vInitGW2006(29)=87326
            vInitGW2006(30)=2294354
            vInitGW2006(31)=63493
            vInitGW2006(32)=449919
            vInitGW2006(33)=275345
            vInitGW2006(34)=53041
            vInitGW2006(35)=75601
      return
    end subroutine ModFlowGroundwater
    ! ----------------------------------------
!
! =================================================================================================
! E.O.F. Groundwater.f90