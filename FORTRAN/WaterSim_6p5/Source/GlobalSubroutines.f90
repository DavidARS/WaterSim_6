!
! File is GlobalSubroutines.f90
!
! Contains the global subroutines used throughout the model. Can be called from anywhere.
! ---------------------------------------------------------------------------------------
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
! Modules:      none
! No Modules:   subroutine openFiles(module_,lvc_DPath_,file_,LU_)
!               Subroutine sErrorCodes(module_,file_,unit)
!               subroutine daysintheyear(year,T)
!               subroutine sCompareControl(one,two,level,flag)
!               subroutine sEightyPercentCAP(i,lvf_receivedCAP,lvf_eligibleCAP,lvl_NoRestrictions)
!               subroutine sEightyPercentSRP(lvf_receivedSRP,lvf_eligibleSRP,lvl_NoRestrictions)
!               subroutine sEightyPercent(lvf_receivedSRP,lvf_eligibleSRP,lvf_receivedCAP,lvf_eligibleCAP,lvl_NoRestrictions)
!               subroutine sAgGroundWater(T,lvf_creditTransfer)
!               subroutine SESRunningT(T,lvf_Met,lvf_MetProjected,Difference)
!               subroutine gpcdSES(T,k,provider,gvf_AlterGPCD_pct, &
!                   lvf_GPCD1,lvf_GPCD2,lvf_GPCD3,lvf_GPCD4,SESgpcdRunning)
!               subroutine GPCD_eight(lvf_GPCDavg)
!               subroutine GPCD(gv_EmpiricalGPCD)

!                   Utilities
!               subroutine sMaskSRP(lvl_mask)
!               subroutine sMask(lvl_mask)
!               subroutine sMaskCAP(lvl_mask)
!               subroutine sMaskSROG(lvl_mask)
!               subroutine sMaskAgriculture(lvl_mask)
!               subroutine sProviders(providers)
!


! Created on 29 October 2009
!
! david arthur sampson

! Last write was: 10.10.13,06.09.14,07.21.14,01.29.15
! ----------------------------------------------------
!

! ======================================================================================================
! 
    ! -------------------------------
    function int2charact(i) result(c)
        ! ----- Types -----------
        integer, intent(in) :: i

        character(len=10) :: c
        ! =======================
        !
            write(c,'(i0)')i
        !
    end function int2charact
    ! ----------------------

    ! ------------------------------------
    subroutine sDateTimeBuild(LogicalUnit)
     use lm_BuildNotes
        !
        ! ------------------ Types ------------------
        integer :: dt(8),lvi_buildNum, lvi_BuildVer,lvi_BuildSubVer
        integer :: LogicalUnit
        integer(2) :: year,month,day,hour,minute

        character(10) :: b,c,d
        character(5) :: e,f,g,h,i
        character(len=10) :: int2charact
        ! ===========================================
        !
            !
             call date_and_time(values=dt)
              lvi_buildNum=BuildNumK()
              lvi_BuildVer=BuildVerK()
              lvi_BuildSubVer=BuildSubVerK()
              b = int2charact(lvi_buildNum)
              c = int2charact(lvi_BuildVer)
              d = int2charact(lvi_BuildSubVer)
              ! Actual computer-based time
              year=dt(1)
              month=dt(2)
              day=dt(3)
              hour=dt(5)
              minute=dt(6)
              !
              ! Run the validation sequence - 2000 to 2010
              ! Input from the interface 
    !          if(gvl_Validate)gpl_validate=.true.
              !
              ! Test version control date and time stamp
              i = int2charact(YearK())
              e = int2charact(MonthK())
              f = int2charact(DayK())
              g = int2charact(HourK())
              h = int2charact(MinK())
              write(LogicalUnit,*)" "
              write(LogicalUnit,*)"Today is:"," Year=",year," Month=",month," Day=",day," Hour=",hour,"Minute=",minute
              write(LogicalUnit,*) " "
              write(LogicalUnit,*)"Current Build # is: ",trim(b) // '.' // trim(c) // '.' // trim(d)
              write(LogicalUnit,*)"DLL Time Stamp(yy.mm.dd.hh.mm): "  ,trim(i) //'.'// trim(e) //'.' // trim(f) // '.' // trim(g) // '.' // trim(h)
              write(LogicalUnit,*) "----------------------------------------------------------------"
            !
        !
      return
    end subroutine sDateTimeBuild
    ! ---------------------------

    ! ---------------------------------------------------------------
    ! Opens the text files for each of the data units
    ! ===============================================================

    ! ------------------------------------------------
    subroutine openFiles(module_,lvc_DPath_,file_,LU_)
      use gm_GlobalData
        !
        ! ------------------------- Types ---------------------------
        ! ------------------------- Types ---------------------------
        integer :: LU_,length_path,length_fid

        character(len=25) :: module_
        character(len=200) :: lvc_DPath_
        character(len=50) :: file_
        character(len=200) :: lvc_dataPath='App_Data\WaterSim_6_0\'
        logical :: checkFID
        ! ===========================================================
        !
            !
            checkFID=.false.
            if(gpl_release)then
             if(lvc_DPath_ .ne. ' ')then
              lvc_dataPath=lvc_DPath_
             endif
            endif
            !
            if(gpl_release)then
             length_path=index(lvc_dataPath,'*') -1
             length_fid=index(file_,' ') -1
            else
             length_path=index(lvc_dataPath,' ') -1
             length_fid=index(file_,' ') -1
            endif
            !
            inquire(File=trim(lvc_dataPath(1:length_path)//file_(1:length_fid)), exist=checkFID)

            if(checkFID)then
               open(LU_,FILE=trim(lvc_dataPath(1:length_path)//file_(1:length_fid)),status="old")         
            else
               if(gpl_release)then
               else
                 call sErrorCodes(module_,file_,LU_)              
                 goto 10
               endif
              ! return error code to the interface
            endif
            !

            !
        !
      return
10         continue
                if(gvl_writeToLog)then
                    string=100
                    call sStrings(string,errorString)
                    call sWrite(errorString,LU_)
                endif
!            gvl_errorFlag=.false.
                
!               
            gv_exceptionCode=LU_
      !
    end subroutine openFiles
    ! ----------------------

    ! ----------------------------------------
    ! Called from "openFiles"
    ! ========================================

    ! ----------------------------------------
    Subroutine sErrorCodes(module_,file_,unit)
        use gm_GlobalData
        !
        ! ----------- Types ----------
        integer :: unit

        character(len=25) :: module_
        character(len=50) :: file_
        ! ============================
        !
            ! See Parameter_control.f90 for codes
            if(gvl_writeToLog)then
              write(7,*)"Data in module", module_," missing(or corrupted):",file_, " Logical Unit = ", unit 
            endif
            !
        !
      return
    End Subroutine sErrorCodes
    ! ------------------------

    ! -----------------------------------
    ! Self explanatory
    ! ===================================

    ! ------------------------------
    subroutine daysintheyear(year,T)
     use gm_ModelControl
        !
        ! ------ Types -------
        integer :: year
        integer LPyear
        ! ====================

        !--- TYPE constructs ---
         type(runTime)T
        ! ===========================
        !
            !
            T%days=365
            LPyear=(int((year+1)/4)*4-1)+1
            if(year == LPyear)T%days=366
            !
        !
      return
    end subroutine daysintheyear
    ! --------------------------

    ! --------------------------------------------
    subroutine sCompareControl(one,two,level,flag)
        !
        ! -------- Types --------
        real :: level
        real :: one,two,y

        logical :: flag
        ! =======================
            !
            flag=.false.
            if(0 < two)then
            y = abs(((one-two)/two)* 100.)
            if(level < y)flag=.true.
            endif
            !
        !
     return
    end subroutine sCompareControl
    ! ----------------------------

    ! --------------------------------------------------------------------------------
    ! Calculates whether the 80% rule (reductions in surface water supplies reach
    ! 80 % of historical, is invoked. Called from aDemandSupply(T,vGP,vLP)
    ! ================================================================================

    ! --------------------------------------------------------------------------------
    subroutine sEightyPercentCAP(i,lvf_receivedCAP,lvf_eligibleCAP,lvl_NoRestrictions)
      use  gm_GlobalData
        !
        ! ---------------------------- Types ---------------------------------
        integer :: i

        real :: lvf_relativeCAP(gvi_maxProV)
        real :: lvf_receivedCAP(gvi_maxProV),lvf_eligibleCAP(gvi_maxProV)
        real, parameter :: lvp_thresholdCAP=0.80

        logical :: lvl_NoRestrictions(gvi_maxProV)
        ! ====================================================================
        !
            !
               lvl_NoRestrictions(i)=.false.
            if(0 < lvf_eligibleCAP(i))then
              lvf_relativeCAP(i)=lvf_receivedCAP(i)/lvf_eligibleCAP(i)
              !
              if( lvf_relativeCAP(i) < lvp_thresholdCAP)then
                lvl_NoRestrictions(i)=.true.
              endif
            endif
            !
        !
       return
    end subroutine sEightyPercentCAP
    ! ------------------------------

    ! -----------------------------------------------------------------------------
    ! See above [for SRP surface water]
    ! =============================================================================

    ! ------------------------------------------------------------------------------
    subroutine sEightyPercentSRP(lvf_receivedSRP,lvf_eligibleSRP,lvl_NoRestrictions)
        !
        ! --------- Types and parameters -----------
        real :: lvf_relativeSRP
        real :: lvf_receivedSRP,lvf_eligibleSRP
        real, parameter :: lvp_thresholdSRP=0.80

        logical :: lvl_NoRestrictions
        ! ==========================================
        !
            !
              lvl_NoRestrictions=.false.
            if(0 < lvf_eligibleSRP)then
              lvf_relativeSRP=lvf_receivedSRP/lvf_eligibleSRP
              !
              if( lvf_relativeSRP < lvp_thresholdSRP)then
                lvl_NoRestrictions=.true.
              endif
            endif
            !
        !
       return
    end subroutine sEightyPercentSRP
    ! ------------------------------

    ! -----------------------------------------------------------------------------------------------------------
    ! Calculates whether the 80% rule (reductions in surface water supplies reach
    ! 80 % of historical, is invoked. Called from pProviderGroundwater(T,gvi_order,vPV)
    ! ===========================================================================================================

    ! -----------------------------------------------------------------------------------------------------------
    subroutine sEightyPercent(lvf_receivedSRP,lvf_eligibleSRP,lvf_receivedCAP,lvf_eligibleCAP,lvl_NoRestrictions)
        !
        ! ---------- Types and parameters -------
        real :: lvf_receivedSRP,lvf_eligibleSRP
        real :: lvf_receivedCAP,lvf_eligibleCAP
        real :: lvf_relative_1, lvf_relative_2
        real :: lvf_relative,lvf_totalSurf
        real, parameter :: lvp_threshold=0.80

        logical :: lvl_NoRestrictions
        ! =======================================
        !
            !
            lvl_NoRestrictions=.false.
             lvf_relative_1 =0.
             lvf_relative_2 =0.
             lvf_relative   =0.
             lvf_totalSurf  =0.
            !
            lvf_totalSurf=lvf_receivedSRP+lvf_receivedCAP
            if(0 < lvf_totalSurf)then
            endif
            !
            if(0 < lvf_eligibleCAP)lvf_relative_2 = (lvf_receivedCAP/lvf_eligibleCAP)
            if(0 < lvf_eligibleSRP)then
                if(0 < lvf_eligibleSRP)lvf_relative_1 = ((lvf_receivedSRP/lvf_eligibleSRP))
                if(0 < lvf_eligibleCAP)then
                 ! Providers have rights to SRP and CAP water
!                     lvf_relative=  (lvf_relative_1+ lvf_relative_2)* 1/2.
                 lvf_relative=(lvf_receivedCAP+lvf_receivedSRP)/(lvf_eligibleSRP+lvf_eligibleCAP)
!                     lvf_relative=  (lvf_fracSRP*(lvf_relative_1)+lvf_fracCAP*(lvf_relative_2))
                else
                 ! Providers have rights to only SRP water
                  lvf_relative=  lvf_relative_1
                endif
               !
            else
                 if(0 < lvf_eligibleCAP)then
                  ! Providers have rights to CAP water
                  lvf_relative=  lvf_relative_2
                 else
                 endif
            endif
               !
                if( 0 < lvf_relative)then
                    if(lvf_relative < lvp_threshold)then
                      lvl_NoRestrictions=.true.
                    endif
                else
                    if(0 < lvf_eligibleSRP .or. 0 < lvf_eligibleCAP)then
                      lvl_NoRestrictions=.true.
                    endif
                endif
              !
        !
     return
    end subroutine sEightyPercent
   ! ----------------------------

    ! ---------------------------------
    subroutine sAgGroundWater(T,Uprovider,lvf_creditWeight,lvf_creditTransfer)
      use gm_ModelControl
       use  lms_Groundwater_A
        !
        ! -------------  Types ---------------------
        integer:: Uprovider
        integer :: i,j,k,rate(gvi_maxProV)
        integer:: maxRate=30,minRate=0

        real :: agAMAavailable
        real::  pumping(7),pumpingDefault(7),pumpingProvider(7,gvi_maxProV)
        real :: SESalpha
        real:: lvf_defaultAgPumping
        real,parameter:: lpf_defaultIndex=10
        real:: lvf_defaultAlpha
        real, parameter:: modPumping=0.9995
        real:: lvf_creditTransfer(gvi_maxProV)
        real:: lvf_benchMarkCredits
        real:: lvf_creditWeight(gvi_maxProV)
        real :: lvf_subtractEffluent
        !real:: lvf_fallowProportion=0.8
        real:: lvf_fallowProportion=1.0
        real :: lvf_totalCredits
        real:: lvf_diffT
       ! real:: lvf_ratio
        real:: fSES_discount

        !logical:: lvl_capTrue=.false.
        ! ==========================================
        !
        ! - Type Construct -
        type(runTime)T
        ! ==================
        !
        ! Defines the slope of the groundwater pumping curve. An index,
        ! a value of 9 represents the default ADWR estimate
        ! =======================
            !
            !
            ! 02.06.15 DAS
            ! This was the original, base parameter
!            rate(Uprovider)=min(maxRate,gvi_AgPumpCurveIndex)
            !
            if(T%year <= T%startyear)then
              lvf_creditTransfer(Uprovider)=0
              lvf_defaultAgPumping=0
              lvf_benchMarkCredits=0
            endif
            !
            lvf_subtractEffluent=0
            ! I added the provider parameter because of the
            ! Decision Game Integration
            !
             rate(Uprovider)=gvi_AgPumpCurveIndex
            !
            if(gvl_modelVinzeJohnston)then
               rate(Uprovider)=max(minRate,min(maxRate,gvi_ProviderAgCreditCurveIndex(Uprovider)))
            else
                if(gvi_AgPumpCurveIndex < 10 .or. 10 < gvi_AgPumpCurveIndex)then
                  rate(Uprovider)=max(minRate,min(maxRate,rate(Uprovider))) 
                else
                endif
            endif
            !
            k=5 ! k is reserved as a proxy for year
            if(T%startyear == 2000)then
             k=T%year-1995          
              call sProviders(T%providers)
            endif
            !
            ! What percent of the groundwater pumping credits
            ! get transfered.
            ! ==========================================
            !lvf_fallowProportion=0.8
            if(0 < gvi_AgCreditTransferPCT)then
              lvf_fallowProportion=gvi_AgCreditTransferPCT * (1./100)
            endif
            !
            !   lvf_creditWeight(Uprovider)=0.
            !
                ! NOT, technically, a population Mask as of July-August 2016
                ! I now use agricultural lands based on a Land cover land use map
                ! 09.02.16
    !          call sPopulationMask(T,lvl_capTrue,lvf_creditWeight)
            !
            ! gvf_Ag_alphaPump(rate(Uprovider) is in Agriculture.f90
             SESalpha=1
            if(0 < rate(Uprovider))then
             SESalpha=gvf_Ag_alphaPump(rate(Uprovider))
            else
            endif
             !

             lvf_defaultAlpha=0
            lvf_defaultAlpha=gvf_Ag_alphaPump(lpf_defaultIndex)
            !
            if(T%year .gt. 2000)then
             lvf_subtractEffluent=gvf_EffluentToAg_diff(T%year-1)
                !
                if(2002 < T%year)then
                  do i = 1,7,1
                      pumping(i)=gvf_AgPumpingEst_acft_a(2,Uprovider,k-i)
                      pumpingDefault(i)=gvf_AgPumpingEst_acft_a(1,Uprovider,k-i)
                      pumpingProvider(i,Uprovider)=gvf_AgPumpingEst_acft_a(2,Uprovider,k-i)
                  end do
                else
                 do i = 1,5,1
                    do j = 1,2,1
                      pumping(i)=gvf_AgPumpingEst_acft_a(j,Uprovider,k-i)
                      pumpingDefault(i)= pumping(i)
                      pumpingProvider(i,Uprovider)=pumping(i)
                    end do
                  end do
                endif
                !
            else
               !
                do i = 1,7,1
                  do j = 1,2,1
                    pumping(i)=gvf_AgPumpingEst_acft_a(j,Uprovider,k)
                    pumpingDefault(i)= pumping(i)
                    pumpingProvider(i,Uprovider)=pumping(i)
                  end do
                end do
                !
            endif
            !
            lvf_defaultAgPumping=0
            agAMAavailable=gvf_AgBenchMarkGWcredits
            lvf_benchMarkCredits=0
            !
            if(T%year .lt. gvi_baseYear-1)then
              agAMAavailable=gv_AgWaterPumpingSRV(T%year,1)
              lvf_defaultAgPumping=agAMAavailable
              lvf_benchMarkCredits=agAMAavailable
              gvf_AgBenchMarkGWcredits=lvf_benchMarkCredits
            else
              do i = 1,7,1
               pumping(i)= pumpingProvider(i,Uprovider)
              end do
                if(0 < rate(Uprovider))then
                 lvf_defaultAgPumping=fSES_discount(lvf_defaultAlpha,pumpingDefault)*modPumping
                 agAMAavailable=fSES_discount(SESalpha,pumping)*modPumping
                endif
                 lvf_benchMarkCredits=gvf_AgBenchMarkGWcredits
            endif
            !
              ! No transfer of credits
            if(rate(Uprovider) < 1)then
              agAMAavailable=lvf_benchMarkCredits
            else
              ! Transfer credits based on user inputs
              ! agAMAavailable=agAMAavailable
            endif
            !
             gvf_AgPumpingEst_acft_a(1,Uprovider,k)=lvf_defaultAgPumping
            !
            ! 10.21.16
!             gvf_AgPumpingEst_acft_a(2,Uprovider,k)=max(0,agAMAavailable-lvf_subtractEffluent)
            ! 11.14.16
             gvf_AgPumpingEst_acft_a(2,Uprovider,k)=agAMAavailable

            !
            ! Create an estimate for 1999 pumping from year 2000 pumping
            if(T%year <= T%startyear)then
              gvf_AgPumpingEst_acft_a(1,Uprovider,k-1)=lvf_defaultAgPumping*1.001422
                ! 11.20.16
                if(34 < Uprovider)then
                  gv_AgWaterPumpingSRV(T%year,6)=agAMAavailable
                endif
            else
              !
              ! Set a new array to pass to agriculture subroutine
                ! 10.21.16
                if(34 < Uprovider)then
                  gv_AgWaterPumpingSRV(T%year,6)=agAMAavailable
                endif
                  gvf_EffluentToUrban(T%year)=lvf_subtractEffluent

              !
            endif
            !

10 format(I4,1x,4(F9.1,1x))
            !
            ! 11.18.16
            gvf_AgCreditWeight(T%year,Uprovider)=lvf_creditWeight(Uprovider)
            !
             lvf_diffT    =0
            if(T%year .lt. gvi_baseYear-1)then
            ! Actual empirical curve
               lvf_diffT=lvf_benchMarkCredits-agAMAavailable 
               !
            else
            ! Time step differences in credits used - estimated curve
                if(0 < gvf_AgPumpingEst_acft_a(2,Uprovider,k))then
                ! lvf_diffT      =max(0,lvf_benchMarkCredits- (gvf_AgPumpingEst_acft_a(2,Uprovider,k)))
                ! 11.15.16 DAS - I am "removing" the effluent from the pumping, so that the actual credit
                ! transfer reflects GW pumped minus effluent, i.e., the DiffT is smaller so effective
                ! credit transfer is also smaller
                ! ---------------------------------------------------------------------------------------
                 lvf_diffT      =max(0,lvf_benchMarkCredits- (gvf_AgPumpingEst_acft_a(2,Uprovider,k)+ gvf_EffluentToUrban(T%year)*lvf_creditWeight(Uprovider)) )

                else

                endif
            endif
            !
            ! Annual credit transfer
            ! 04.09.15 DAS added for UI
             if(rate(Uprovider) < 1)then
              lvf_creditTransfer(Uprovider)=0
             else
              !lvf_creditTransfer(Uprovider)=abs(lvf_diffT-lvf_diffTminus1)*lvf_creditWeight(Uprovider)*lvf_fallowProportion
                ! 06.19.15 based on conversations with Ray
              lvf_creditTransfer(Uprovider)=lvf_diffT*(lvf_creditWeight(Uprovider))*   lvf_fallowProportion
             endif
             !
!             gvf_cumulativeCredit(Uprovider)=gvf_cumulativeCredit(Uprovider)+lvf_creditTransfer(Uprovider)
             ! Cummulative credit transfer is = lvf_diffT

              gvf_defaultAgPumping(Uprovider) =lvf_creditWeight(Uprovider)*gvf_AgPumpingEst_acft_a(1,Uprovider,k)
              gvf_maxAgPumping(Uprovider) = lvf_creditWeight(Uprovider)*lvf_benchMarkCredits
              !
              ! Moved here on 04.21.15
             ! gvi_WaterFromAgPumpingMax_acft_a(Uprovider)= nint(lvf_creditWeight(Uprovider)*lvf_benchMarkCredits)
              !
              ! 02.17.15 and 02.18.15
              ! -------------------------------
                !
                  go_AgWaterGW_AF(Uprovider)=0
            if(T%year .lt. gvi_baseYear+1)then
               !
               gvf_AgGWpotentialCredits=0
               gvf_AgGWpotential_AF(Uprovider)=0
             go_WaterFromAgPumping_acft_a(Uprovider)= nint(lvf_creditWeight(Uprovider)*agAMAavailable *lvf_fallowProportion)

            else          
!                lvf_ratio=0
                !
              if(0 < rate(Uprovider))then
                go_AgWaterGW_AF(Uprovider)= nint(lvf_diffT*lvf_creditWeight(Uprovider)*lvf_fallowProportion )

                if(0 < lvf_creditWeight(Uprovider))then
               !  lvf_ratio=(gvf_cumulativeCredit(Uprovider)/(lvf_creditWeight(Uprovider)*lvf_fallowProportion*lvf_benchMarkCredits ))
                endif

              endif
             !
             ! 04.21.15 is this correct? This is total
             ! Send to Agriculture.f90
              lvf_totalCredits=gvf_AgEffluent_2015+gvf_AgSurfaceWater_2015+gvf_AgGroundWater_2015

              go_WaterFromAgPumping_acft_a(Uprovider)=0
             go_WaterFromAgPumping_acft_a(Uprovider)= nint(max(0,min(lvf_creditWeight(Uprovider)*lvf_totalCredits-go_AgWaterGW_AF(Uprovider),&
               lvf_creditWeight(Uprovider)*agAMAavailable - (lvf_creditWeight(Uprovider)*gvf_EffluentToAg_diff(T%year-1)))) *lvf_fallowProportion)
             !
              gvf_AgGWpotentialCredits=0
             gvf_AgGWpotentialCredits=lvf_benchMarkCredits*lvf_fallowProportion
             gvf_AgGWpotential_AF(Uprovider)=gvf_AgGWpotentialCredits*lvf_creditWeight(Uprovider)*lvf_fallowProportion
             !
            endif
            !
              gvi_defaultAgPumping_acft_a(Uprovider)=0
            gvi_defaultAgPumping_acft_a(Uprovider)=nint(gvf_defaultAgPumping(Uprovider))
              go_MaxAgPumping(Uprovider)=0
            go_MaxAgPumping(Uprovider)=nint(gvf_maxAgPumping(Uprovider))
            !
        !
      return
    end subroutine 
    ! -----------------------------------------------------------

    ! -----------------------------------------------------------------------
    subroutine sAgSurfaceWaterOther(T,Uprovider,lvf_creditTransCAP)
      use gm_ModelControl
       use  lms_Groundwater_A
        !
        ! ------------------------  Types -----------------------------
        integer :: i,j,k,rate(gvi_maxProV),scaleDefault=10,defaultRate
        integer:: Uprovider
        integer:: maxRate=20,minRate=0,scaledRate

        real,parameter:: lpf_scaleToGW=1.2
        real:: agAMAavailable(2)
        real:: surface_O(7),surface_CAP(7)
        real:: surface_Odefault(7),surface_CAPdefault(7)
        real:: agAMAavailableMax(2),agAMAavailableDefault(2)
        real:: SESalpha,SESdefault
        !real:: lvf_creditTransOther(gvi_maxProV)
        real:: lvf_creditTransCAP(gvi_maxProV)
        real:: lvf_benchMark(4)
        real:: lvf_creditWeight(gvi_maxProV)
        !real:: lvf_fallowProportion=0.8
        real:: lvf_fallowProportion=1.0
        real:: lvf_diffT(2)
        real:: lvf_adjust(2)
        real,parameter :: lpf_minAF=1
        real:: fSES_discount

      !  logical:: lvl_capTrue=.true.
        ! =============================================================
        !
        ! - Type Construct -
        type(runTime)T
        ! ==================
        !
        ! Defines the slope of the groundwater pumping curve. An index,
        ! a value of 9 represents the default ADWR estimate. The "adjustments"
        ! are simply used to align the index results for (10) to match
        ! the ADWR values 02.14.15
        ! =======================
!            rate(Uprovider)= gvi_AgPumpCurveIndex
            lvf_adjust(1)=100
            lvf_adjust(2)=300
            scaledRate=0
            !
             rate(Uprovider)=gvi_AgPumpCurveIndex
             defaultRate=scaleDefault
            !
            call sMaskCAP(gvl_maskCAP)
            !
            if(gvl_modelVinzeJohnston)then
               rate(Uprovider)=max(minRate,min(maxRate,gvi_ProviderAgCreditCurveIndex(Uprovider)))
               scaledRate=nint(lpf_scaleToGW*gvi_ProviderAgCreditCurveIndex(Uprovider))
                rate(Uprovider)=scaledRate
                defaultRate=nint(lpf_scaleToGW*scaleDefault)
            else
                scaledRate=nint(lpf_scaleToGW*gvi_AgPumpCurveIndex)
                rate(Uprovider)=scaledRate
                if(scaledRate < 9 .or. 9 < scaledRate)then
                  rate(Uprovider)=max(minRate,min(maxRate,rate(Uprovider))) 
                else
                endif
            endif
            !
            k=5 ! k is reserved as a proxy for year
            if(T%startyear == 2000)then
             k=T%year-1995
            endif
            !
               lvf_creditWeight(Uprovider)=0.
               lvf_creditTransCAP(Uprovider)=0
            !
            if(T%year <= T%startyear)then

              call sProviders(T%providers)
                !
                !
               if(T%providers(Uprovider) == 'ad')then
                go_AgWaterCAP_PCT=0
                go_AgWaterSRP_PCT=0      

                !call sPopulationMask(T,lvl_capTrue,lvf_creditWeight)   
                call sCAPMaskCredits(T,lvf_creditWeight)
               endif
            else
                !call sPopulationMask(T,lvl_capTrue,lvf_creditWeight)

                ! IF SRP Ag water is added this NEEDS changing
                ! 09.02.16
                call sCAPMaskCredits(T,lvf_creditWeight)
            endif
            !
                 !do i = 1,gvi_Providers,1
!                   lvf_creditWeight(Uprovider)=0.
!                   lvf_creditTransCAP(Uprovider)=0
                ! end do
            !
            !
            gvi_DefaultAgSurface_acft_a(Uprovider)=0
            !

        if(0 < lvf_creditWeight(Uprovider))then
            ! What percent of the surface water credits
            ! get transfered.
            ! Array(1) = Other water
            ! Array(2) = CAP water
            ! ==========================================
            if(0 < gvi_AgCreditTransferPCT)then
              lvf_fallowProportion=gvi_AgCreditTransferPCT * (1./100)
            endif
            !
                ! FOR CAP designations
                ! We will need something different for "other" water
                ! -------------------------
                !
            ! First time through calculate
            !if(T%providers(Uprovider) == 'ad')then
           ! endif
            !
              SESalpha=1
              SESdefault=1
            if(0 < rate(Uprovider))then
             SESalpha= gvf_Ag_alphaSurfOther(rate(Uprovider))
             SESdefault=gvf_Ag_alphaSurfOther(defaultRate)
            else
            endif
            !
            if(T%year .gt. 2000)then
                !
              if(T%year .gt. 2002)then
                do i = 1,7,1
                  do j = 1,2,1
                    surface_O(i)=gvf_AgSurfOtherEst_acft_a(j,k-i)
                    surface_CAP(i)=gvf_AgSurfCAPEst_acft_a(j,Uprovider,k-i)
                    !
                    surface_Odefault(i)=gvf_agSurfOtherEDefault_AF_a(j,k-i)
                    surface_CAPdefault(i)=gvf_AgSurfCAPEstDefault_acft_a(j,k-i)
                  end do
                end do
                !
              else
               do i = 1,5,1
                  do j = 1,2,1
                    surface_O(i)=gvf_AgSurfOtherEst_acft_a(j,k-i)
                    surface_CAP(i)=gvf_AgSurfCAPEst_acft_a(j,Uprovider,k-i)
                    surface_Odefault(i)=gvf_AgSurfCAPEstDefault_acft_a(j,k-i)
                    surface_CAPdefault(i)=gvf_AgSurfCAPEstDefault_acft_a(j,k-i)
                  end do
                end do
              endif
            else
               do i = 1,7,1
                  do j = 1,2,1
                    surface_O(i)=gvf_AgSurfOtherEst_acft_a(j,k)
                    surface_CAP(i)=gvf_AgSurfCAPEst_acft_a(j,Uprovider,k)
                    surface_Odefault(i)=gvf_agSurfOtherEDefault_AF_a(j,k)
                    surface_CAPdefault(i)=gvf_AgSurfCAPEstDefault_acft_a(j,k)
                  end do
                end do
            endif
            if(T%year .lt. 2015)then
                !
                do i = 1,2,1
                agAMAavailableMax(i)=0
                 agAMAavailable(i)=gv_AgWaterPumpingSRV(T%year,i+1)
                 lvf_benchMark(i)=agAMAavailable(i)
                 agAMAavailableDefault(i)=agAMAavailable(i)
                 agAMAavailableMax(i)=agAMAavailable(i)
                end do
                !
                gvf_AgBenchMarkOther=agAMAavailable(1)
                gvf_AgBenchMarkCAP=agAMAavailable(2)

                lvf_benchMark(1)=gvf_AgBenchMarkOther
                lvf_benchMark(2)=gvf_AgBenchMarkCAP
                !
            else
                !
                agAMAavailable(1)=fSES_discount(SESalpha,surface_O)
                agAMAavailable(2)=fSES_discount(SESalpha,surface_CAP)
                !
                agAMAavailableDefault(1)=fSES_discount(SESdefault,surface_Odefault)
                agAMAavailableDefault(2)=fSES_discount(SESdefault,surface_CAPdefault)
                !

                agAMAavailableMax(1)=gvf_AgBenchMarkOther
                agAMAavailableMax(2)=gvf_AgBenchMarkCAP
                !
                lvf_benchMark(1)=gvf_AgBenchMarkOther
                lvf_benchMark(2)=gvf_AgBenchMarkCAP
            endif
            !
            do i = 1,2,2
              agAMAavailable(i)=max(0,agAMAavailable(i)-lvf_adjust(i))
              if(rate(Uprovider) < 1)agAMAavailable(i)=lvf_benchMark(i)
              if(agAMAavailable(i) < lpf_minAF)agAMAavailable(i)=0
              if(agAMAavailable(i) < 0)agAMAavailable(i)=0
            end do
            !

            gvf_AgSurfOtherEst_acft_a(1,k)=gv_AgWaterPumpingSRV(T%year,2)
            gvf_AgSurfOtherEst_acft_a(2,k)=agAMAavailable(1)
            !
            gvf_agSurfOtherEDefault_AF_a(1,k)=gv_AgWaterPumpingSRV(T%year,2)
            gvf_agSurfOtherEDefault_AF_a(2,k)=agAMAavailableDefault(1)
            !
            gvf_AgSurfOtherEstMax_acft_a(1,k)=gv_AgWaterPumpingSRV(T%year,2)
            gvf_AgSurfOtherEstMax_acft_a(2,k)=agAMAavailableMax(1)
            !
            gvf_AgSurfCAPEst_acft_a(1,Uprovider,k)=gv_AgWaterPumpingSRV(T%year,3)
            gvf_AgSurfCAPEst_acft_a(2,Uprovider,k)=agAMAavailable(2)
            !
            gvf_AgSurfCAPEstDefault_acft_a(1,k)=gv_AgWaterPumpingSRV(T%year,3)
            gvf_AgSurfCAPEstDefault_acft_a(2,k)=agAMAavailableDefault(2)

            gvf_AgSurfCAPEstMax_acft_a(1,k)=gv_AgWaterPumpingSRV(T%year,3)
            gvf_AgSurfCAPEstMax_acft_a(2,k)=agAMAavailableMax(2)
            !
            if(T%year <= T%startyear)then
              gvf_AgSurfCAPEst_acft_a(1,Uprovider,k-1)=gvf_AgSurfCAPEst_acft_a(1,Uprovider,k)*1.001422
              gvf_AgSurfOtherEst_acft_a(1,k-1)=gvf_AgSurfOtherEst_acft_a(1,k)*1.001422
              gvf_AgSurfCAPEstDefault_acft_a(1,k-1)=gvf_AgSurfCAPEstDefault_acft_a(1,k)*1.001422
              gvf_AgSurfCAPEstMax_acft_a(1,k-1)= gvf_AgSurfCAPEstMax_acft_a(1,k)*1.001422
            endif
            ! Set a new array to pass to agriculture subroutine
            ! This is CAP only....
            !gv_AgWaterPumpingSRV(T%year,5)=agAMAavailable(2)
            ! Pass to the NEW ag production subroutine (and the AgDemand subroutine)
            ! 10.20.16 das
            gv_AgWaterPumpingSRV(T%year,5)=gvf_AgSurfCAPEst_acft_a(2,Uprovider,k)
            ! -----------------------------
            gvf_defaultAgSurface(Uprovider)=agAMAavailableDefault(2)
            !
            if(T%year .lt. 2015)then
              lvf_diffT(1)=lvf_benchMark(1)-gvf_AgSurfOtherEst_acft_a(1,k)
              !
              lvf_diffT(2)=lvf_benchMark(2)-gvf_AgSurfCAPEst_acft_a(1,Uprovider,k)
            else
              lvf_diffT(1)=lvf_benchMark(1)-gvf_AgSurfOtherEst_acft_a(2,k)
              !
              lvf_diffT(2)=lvf_benchMark(2)-gvf_AgSurfCAPEst_acft_a(2,Uprovider,k)
            endif
            !
              if(T%year .lt. 2015)then
               !lvf_creditTransOther(Uprovider)=0
               lvf_creditTransCAP(Uprovider)= 0
             else
             ! lvf_creditTransOther(Uprovider)=lvf_diffT(1)*lvf_creditWeight(Uprovider)*lvf_fallowProportion

              lvf_creditTransCAP(Uprovider)=lvf_diffT(2) *lvf_creditWeight(Uprovider)*lvf_fallowProportion
             endif
              !
            ! 04.09.15 DAS added for UI
             if(rate(Uprovider) < 1)then
              lvf_creditTransCAP(Uprovider)=0
             else
             endif
              ! 01.29.15 new definition
              go_WaterFromAgSurface_acft_a(Uprovider)=nint(lvf_creditTransCAP(Uprovider))
              !
              ! 02.17.15
              ! DAS ratio Ag water to threshold (now 2014)
                !
                if(0 < rate(Uprovider))then
                  !go_AgWaterCAP_AF(Uprovider)= nint(max(0,lvf_diffT(2)*lvf_creditWeight(Uprovider)*lvf_fallowProportion))
                   go_AgWaterCAP_PCT(Uprovider)=0
                  go_AgWaterCAP_PCT(Uprovider)= nint(100-(max(0, (((agAMAavailable(2)*lvf_creditWeight(Uprovider))/(lvf_benchMark(2)*lvf_creditWeight(Uprovider)))*100) )))
                  !
                  go_AgWaterSRP_AF(Uprovider)= nint(max(0,lvf_diffT(1)*lvf_creditWeight(Uprovider)*lvf_fallowProportion))
                   go_AgWaterSRP_PCT(Uprovider)=0
                  go_AgWaterSRP_PCT(Uprovider)= nint(((agAMAavailable(1)/lvf_benchMark(1))*100))
                    !
                endif
              !
              gvi_DefaultAgSurface_acft_a(Uprovider)=nint(lvf_creditWeight(Uprovider)* agAMAavailableDefault(2)) !gvf_defaultAgSurface(Uprovider))
              go_MaxAgSurface(Uprovider) = nint(lvf_creditWeight(Uprovider)*agAMAavailableMax(2))
              !
        endif
        !
      return
    end subroutine sAgSurfaceWaterOther
    ! -----------------------------------------------------------
    !
    subroutine sPopulationMask(T,lvl_capTrue,lvf_creditWeight)
        use  lms_Groundwater_A
        !
        ! ------------------- Types ---------------------
        integer:: i

        real:: lvf_creditWeight(gvi_maxProV)
        real:: lvf_popTotal,lvf_popProviders(gvi_maxProV)
        !real :: lvf_relArea(gvi_maxProV)

        logical:: lvl_capTrue
        ! ===============================================

        ! - Type Construct -
        type(runTime)T
        ! ==================
            !
            if(lvl_capTrue)then
             call sMaskCAP(gvl_maskCAP)
            else
             gvl_maskCAP=.true.
            endif
            !
             lvf_popTotal=0
            call sMaskAgriculture(gvl_mask)
            !
              do i = 1,gvi_Providers,1
                lvf_popProviders(i)=0
                !
                if(gvl_mask(i))then
                    lvf_popProviders(i)=gvf_populations(T%year,i)
                endif
              end do
                !
                lvf_popTotal=sum(lvf_popProviders)
                !
              do i = 1,gvi_Providers,1
                lvf_creditWeight(i)=0
                if(0 < lvf_popTotal)lvf_creditWeight(i)= lvf_popProviders(i)/lvf_popTotal
              end do
                !
                ! 08.31.16 das 
               ! call AgAcerageAndCredits(T,lvf_relArea)
                !
               ! lvf_creditWeight=lvf_relArea
                !
        !
      return
    end subroutine sPopulationMask

    subroutine sCAPMaskCredits(T,lvf_creditWeight)
        use  lms_Groundwater_A
        !
        ! ------------------- Types ---------------------
        integer :: i
        real:: lvf_creditWeight(gvi_maxProV)
        real :: lvf_relArea(gvi_maxProV)
        ! ===============================================

        ! - Type Construct -
        type(runTime)T
        ! ==================
            !
            call AgAcerageAndCreditsCAP(T,lvf_relArea)
            !
            do i = 1,gvi_maxProV,1
              lvf_creditWeight(i)=0
             lvf_creditWeight(i)=lvf_relArea(i)
            end do
           !
      return
    end subroutine sCAPMaskCredits


    ! das 2016
    ! Calculates the SES running temperature- smoothed T trend
    ! ===========================================================

    ! ----------------------------------------------------------
    subroutine SESRunningT(T,lvf_Met,lvf_MetProjected,Difference)
      use gm_ModelControl
        !
        ! --------------- Types -----------------
        integer :: i,j

        real :: T1,T2,Difference
        real :: lvf_MetProjected(2000:2100,12,3)
        real :: lvf_Met(2000:2100,12,3)
        ! =======================================
        !

        ! -- Type Constructs ---
        type(runTime)T
        ! ===========================
            !
            T1=0
            do i = 1,12,1
              T1=T1+(( (lvf_Met(T%year,i,1)+lvf_Met(T%year-1,i,1))*0.5 + &
                (lvf_Met(T%year,i,2)+lvf_Met(T%year-1,i,2))*0.5  &
                ))*0.5
            end do
            T1=T1/12
            T2=0
            do j = 1,12,1
              T2=T2+(((lvf_MetProjected(T%year,j,1)+lvf_MetProjected(T%year-1,j,1)) + &
                (lvf_MetProjected(T%year,j,2)+lvf_MetProjected(T%year-1,j,2)) &
                )*0.5 )*0.5
            end do
            T2=T2/12
            !
            Difference=0
            Difference=T2-T1
            !
        !
     return
   end subroutine SESRunningT
   ! ------------------------
    
    ! --------------------------------
    subroutine GPCD_eight(lvf_GPCDavg)
        !
        ! ---------- Types ---------
        real :: lvf_GPCDavg(35,5)
        ! ==========================
        !
          ! Five year running mean (array is provider, group year)
          ! 1 is 2000-2004
          ! 2 is 2001-2005
          ! 3 is 2002-2006
          ! 4 is 2003-2007
          ! 5 is 2004-2008

          ! Created using ADWR data - 2000-2008_LP_GPCD_ADWR.xls Sally gave this file to me
          ! 10.27.10 das
          ! ----------------------------------------------------------------------------------------------------------
            lvf_GPCDavg(1,1)= 542     ; lvf_GPCDavg(1,2)= 522   ; lvf_GPCDavg(1,3)=526  ; lvf_GPCDavg(1,4)=522 ; lvf_GPCDavg(1,5)=517
            lvf_GPCDavg(2,1)= 167     ; lvf_GPCDavg(2,2)= 165   ; lvf_GPCDavg(2,3)=169  ; lvf_GPCDavg(2,4)=177 ; lvf_GPCDavg(2,5)=180
            lvf_GPCDavg(3,1)= 1067    ; lvf_GPCDavg(3,2)= 1044  ; lvf_GPCDavg(3,3)=1025 ; lvf_GPCDavg(3,4)=1029; lvf_GPCDavg(3,5)=1053
            lvf_GPCDavg(4,1)= 279     ; lvf_GPCDavg(4,2)= 274   ; lvf_GPCDavg(4,3)=265  ; lvf_GPCDavg(4,4)=283 ; lvf_GPCDavg(4,5)=307
            lvf_GPCDavg(5,1)= 202     ; lvf_GPCDavg(5,2)= 195   ; lvf_GPCDavg(5,3)=191  ; lvf_GPCDavg(5,4)=188 ; lvf_GPCDavg(5,5)=186

            lvf_GPCDavg(6,1)= 159     ; lvf_GPCDavg(6,2)= 157   ; lvf_GPCDavg(6,3)=157  ; lvf_GPCDavg(6,4)=156 ; lvf_GPCDavg(6,5)=154
            lvf_GPCDavg(7,1)= 818     ; lvf_GPCDavg(7,2)= 816   ; lvf_GPCDavg(7,3)=832  ; lvf_GPCDavg(7,4)=835 ; lvf_GPCDavg(7,5)=847
            lvf_GPCDavg(8,1)= 251     ; lvf_GPCDavg(8,2)= 229   ; lvf_GPCDavg(8,3)=221  ; lvf_GPCDavg(8,4)=169 ; lvf_GPCDavg(8,5)=110
            lvf_GPCDavg(9,1)= 409     ; lvf_GPCDavg(9,2)= 403   ; lvf_GPCDavg(9,3)=392  ; lvf_GPCDavg(9,4)=389 ; lvf_GPCDavg(9,5)=391
            lvf_GPCDavg(10,1)= 318    ; lvf_GPCDavg(10,2)= 302  ; lvf_GPCDavg(10,3)=304 ; lvf_GPCDavg(10,4)=296; lvf_GPCDavg(10,5)=297

            lvf_GPCDavg(11,1)= 242    ; lvf_GPCDavg(11,2)= 233  ; lvf_GPCDavg(11,3)=228 ; lvf_GPCDavg(11,4)=225; lvf_GPCDavg(11,5)=216
            lvf_GPCDavg(12,1)= 273    ; lvf_GPCDavg(12,2)= 268  ; lvf_GPCDavg(12,3)=264 ; lvf_GPCDavg(12,4)=256; lvf_GPCDavg(12,5)=250
            lvf_GPCDavg(13,1)= 170    ; lvf_GPCDavg(13,2)= 151  ; lvf_GPCDavg(13,3)=159 ; lvf_GPCDavg(13,4)=172; lvf_GPCDavg(13,5)=178
            lvf_GPCDavg(14,1)= 157    ; lvf_GPCDavg(14,2)= 156  ; lvf_GPCDavg(14,3)=150 ; lvf_GPCDavg(14,4)=147; lvf_GPCDavg(14,5)=142
            lvf_GPCDavg(15,1)= 161    ; lvf_GPCDavg(15,2)= 159  ; lvf_GPCDavg(15,3)=160 ; lvf_GPCDavg(15,4)=155; lvf_GPCDavg(15,5)=150

            lvf_GPCDavg(16,1)= 172    ; lvf_GPCDavg(16,2)= 160  ; lvf_GPCDavg(16,3)=158 ; lvf_GPCDavg(16,4)=160; lvf_GPCDavg(16,5)=160
            lvf_GPCDavg(17,1)= 229    ; lvf_GPCDavg(17,2)= 219  ; lvf_GPCDavg(17,3)=211 ; lvf_GPCDavg(17,4)=205; lvf_GPCDavg(17,5)=195
            lvf_GPCDavg(18,1)= 195    ; lvf_GPCDavg(18,2)= 191  ; lvf_GPCDavg(18,3)=188 ; lvf_GPCDavg(18,4)=184; lvf_GPCDavg(18,5)=179
            lvf_GPCDavg(19,1)= 192    ; lvf_GPCDavg(19,2)= 178  ; lvf_GPCDavg(19,3)=169 ; lvf_GPCDavg(19,4)=170; lvf_GPCDavg(19,5)=172
            lvf_GPCDavg(20,1)= 336    ; lvf_GPCDavg(20,2)= 322  ; lvf_GPCDavg(20,3)=311 ; lvf_GPCDavg(20,4)=312; lvf_GPCDavg(20,5)=306

            lvf_GPCDavg(21,1)= 202    ; lvf_GPCDavg(21,2)= 194  ; lvf_GPCDavg(21,3)=190 ; lvf_GPCDavg(21,4)=189; lvf_GPCDavg(21,5)=182
            lvf_GPCDavg(22,1)= 200    ; lvf_GPCDavg(22,2)= 200  ; lvf_GPCDavg(22,3)=200 ; lvf_GPCDavg(22,4)=200; lvf_GPCDavg(22,5)=200
            lvf_GPCDavg(23,1)= 200    ; lvf_GPCDavg(23,2)= 200  ; lvf_GPCDavg(23,3)=200 ; lvf_GPCDavg(23,4)=200; lvf_GPCDavg(23,5)=200
            lvf_GPCDavg(24,1)= 182    ; lvf_GPCDavg(24,2)= 174  ; lvf_GPCDavg(24,3)=176 ; lvf_GPCDavg(24,4)=176; lvf_GPCDavg(24,5)=176
            lvf_GPCDavg(25,1)= 210    ; lvf_GPCDavg(25,2)= 205  ; lvf_GPCDavg(25,3)=202 ; lvf_GPCDavg(25,4)=198; lvf_GPCDavg(25,5)=189

            lvf_GPCDavg(26,1)= 281    ; lvf_GPCDavg(26,2)= 267  ; lvf_GPCDavg(26,3)=250 ; lvf_GPCDavg(26,4)=237; lvf_GPCDavg(26,5)=228
            lvf_GPCDavg(27,1)= 200    ; lvf_GPCDavg(27,2)= 200  ; lvf_GPCDavg(27,3)=200 ; lvf_GPCDavg(27,4)=200; lvf_GPCDavg(27,5)=200
            lvf_GPCDavg(28,1)= 1462   ; lvf_GPCDavg(28,2)= 1418 ; lvf_GPCDavg(28,3)=1400; lvf_GPCDavg(28,4)=1378;lvf_GPCDavg(28,5)=1386   
            lvf_GPCDavg(29,1)= 203    ; lvf_GPCDavg(29,2)= 193  ; lvf_GPCDavg(29,3)=195 ; lvf_GPCDavg(29,4)=197; lvf_GPCDavg(29,5)=197
            lvf_GPCDavg(30,1)= 336    ; lvf_GPCDavg(30,2)= 327  ; lvf_GPCDavg(30,3)=326 ; lvf_GPCDavg(30,4)=320; lvf_GPCDavg(30,5)=320

            lvf_GPCDavg(31,1)= 235    ; lvf_GPCDavg(31,2)= 241  ; lvf_GPCDavg(31,3)=254 ; lvf_GPCDavg(31,4)=264; lvf_GPCDavg(31,5)=275
            lvf_GPCDavg(32,1)= 322    ; lvf_GPCDavg(32,2)= 302  ; lvf_GPCDavg(32,3)=290 ; lvf_GPCDavg(32,4)=282; lvf_GPCDavg(32,5)=267
            lvf_GPCDavg(33,1)= 576    ; lvf_GPCDavg(33,2)= 566  ; lvf_GPCDavg(33,3)=590 ; lvf_GPCDavg(33,4)=590; lvf_GPCDavg(33,5)=562
            lvf_GPCDavg(34,1)= 148    ; lvf_GPCDavg(34,2)= 146  ; lvf_GPCDavg(34,3)=142 ; lvf_GPCDavg(34,4)=146; lvf_GPCDavg(34,5)=149
            lvf_GPCDavg(35,1)= 200    ; lvf_GPCDavg(35,2)= 200  ; lvf_GPCDavg(35,3)=200 ; lvf_GPCDavg(35,4)=200; lvf_GPCDavg(35,5)=200
        !
     return
    end subroutine
    ! -------------

    ! -------------------------------
    subroutine GPCD(gv_EmpiricalGPCD)
        !
        ! ----------- Types --------------
        integer(4) :: gv_EmpiricalGPCD(35)
        ! ================================
        !
            ! Six year averages
            ! ================================================================================================================
            gv_EmpiricalGPCD(1)=536;   gv_EmpiricalGPCD(2)=168;   gv_EmpiricalGPCD(3)=1035;  gv_EmpiricalGPCD(4)=267;
            gv_EmpiricalGPCD(5)=196;   gv_EmpiricalGPCD(6)=156;   gv_EmpiricalGPCD(7)=823;   gv_EmpiricalGPCD(8)=210;
            gv_EmpiricalGPCD(9)=398;   gv_EmpiricalGPCD(10)=310;   gv_EmpiricalGPCD(11)=233;   gv_EmpiricalGPCD(12)=269;
            gv_EmpiricalGPCD(13)=171;   gv_EmpiricalGPCD(14)=151;   gv_EmpiricalGPCD(15)=158;   gv_EmpiricalGPCD(16)=169;
            gv_EmpiricalGPCD(17)=219;   gv_EmpiricalGPCD(18)=192;   gv_EmpiricalGPCD(19)=180;   gv_EmpiricalGPCD(20)=325;
            gv_EmpiricalGPCD(21)=197;   gv_EmpiricalGPCD(22)=224;   gv_EmpiricalGPCD(23)=224;   gv_EmpiricalGPCD(24)=181;
            gv_EmpiricalGPCD(25)=203;   gv_EmpiricalGPCD(26)=264;   gv_EmpiricalGPCD(27)=224;   gv_EmpiricalGPCD(28)=1455;
            gv_EmpiricalGPCD(29)=202;   gv_EmpiricalGPCD(30)=332;   gv_EmpiricalGPCD(31)=244;   gv_EmpiricalGPCD(32)=304;
            gv_EmpiricalGPCD(33)=568;   gv_EmpiricalGPCD(34)=147;   gv_EmpiricalGPCD(35)=224;  
            ! ================================================================================================================
        !
      return
    end subroutine GPCD
    ! -----------------



    ! In this case, the first array must be rank 35 whilst the second MUST be 10
    ! 
    ! Masks
    ! -------------------------------------------------------
    !  SRP mask. Determines which of the 35 (as of now) are 
    ! the 10 SRP members
    ! ========================================================

    ! ---------------------------
    subroutine sMaskSRP(lvl_mask)
    use gm_GlobalData
    !
    ! ------------------ Types ---------------------------
    integer :: i

    logical :: lvl_mask(gvi_maxProV)

    character(len=2), dimension(gvi_maxProV) :: providers
    ! ====================================================
        !
        call sProviders(providers)
        do i = 1,gvi_Providers,1
          lvl_mask(i)=.false.
          if(providers(i) == 'av' .or. providers(i)=='ch' .or. providers(i)=='gi' &
            .or. providers(i)=='gl'.or. providers(i)=='me'.or. providers(i)=='pe'  &
            .or. providers(i)=='ph'.or. providers(i)=='sc'.or. providers(i)=='te'  &
            .or. providers(i)=='to')then     
            lvl_mask(i)=.true.
          endif
        end do
        !
      !
     return
    end subroutine sMaskSRP
    ! ----------------------

 
    ! -------------------------------------------------------
    ! Removes no provider and other provider from the
    ! list of 35 (presently)
    ! =======================================================

    ! ------------------------
    subroutine sMask(lvl_mask)
      use gm_GlobalData
        !
        ! ------------------ Types -----------------------------
        integer :: i
        character(len=2), dimension(gvi_maxProV) :: providers
        logical :: lvl_mask(gvi_maxProV)
        ! ======================================================
        !
            !
            call sProviders(providers)
            do i = 1,gvi_Providers,1
             lvl_mask(i)=.true.
              if(Providers(i) == 'no' .or. Providers(i) == 'op')lvl_mask(i)=.false.
            end do
            !
        !
       return
     end subroutine sMask
     ! ------------------

! --------------------------------------------------------
! Determines the CAP providers
! ========================================================

! ---------------------------
subroutine sMaskCAP(lvl_mask)
 use gm_GlobalData
    !
    ! ------------------- Types --------------------------
    integer :: i

    logical :: lvl_mask(gvi_maxProV)

    character(len=2), dimension(gvi_maxProV) :: providers
    ! ====================================================
    !
        !
        call sProviders(providers)
        do i = 1,gvi_Providers,1
          lvl_mask(i)=.true.
          if(providers(i) == 'ad' .or. providers(i)=='av' .or. providers(i)=='be' &
            .or. providers(i)=='bu' .or. providers(i)=='sp' .or. providers(i)=='cu' &
            .or. providers(i)=='dh'.or. providers(i)=='em'.or. providers(i)=='go' &
            .or. providers(i)=='lp' .or. providers(i)=='no' &
            .or. providers(i)=='rg'.or. providers(i)=='ry'  &
            .or. providers(i)=='sr'.or. providers(i)=='to'.or. providers(i)=='vu'  &
            .or. providers(i)=='we')then     
            !
            lvl_mask(i)=.false.
            !
          endif
        end do
        !
    !
  return
end subroutine sMaskCAP
! ---------------------

! -------------------------------------------------------
! Sub-Regional Operating Group
! members. AMWUA
! =======================================================

! ----------------------------
subroutine sMaskSROG(lvl_mask)
 use gm_GlobalData
    !
    ! ---------------- Types -----------------------------
    integer :: i

    logical :: lvl_mask(gvi_maxProV)

    character(len=2), dimension(gvi_maxProV) :: providers
    ! ====================================================
    !
        !
        call sProviders(providers)
        do  i = 1,gvi_Providers,1
          lvl_mask(i)=.false.
          if(providers(i) == 'gl' .or. providers(i)=='me' .or. providers(i)=='ph' &
            .or. providers(i)=='sc'.or. providers(i)=='te')then
           lvl_mask(i)=.true.
          endif
        end do
        !
    !
 return
end subroutine sMaskSROG
! ----------------------

! -------------------------------------------------------
! Determines which water providers 
! have ag in their boundary
! ========================================================
! Line 436 in Agriculture.f90
! Line 313 in Agriculture.f90
! Line 255 in Agriculture.f90
! Line 920 in GlobalSubroutines.f90
! Line 480 in Agriculture.f90
! Line 2439 in Water_CityModel.f90
!
! ----------------------------------------------
subroutine sMaskAgriculture(lvl_mask)
 use gm_GlobalData
  use lm_LULC
    !
  !
    ! ------------------- Types --------------------------
    integer :: i
    real :: temp(gvi_maxProV)
    logical :: lvl_mask(gvi_maxProV)

    character(len=2), dimension(gvi_maxProV) :: providers
    ! ====================================================
    !
        !
        call sProviders(providers)
        !
        do i = 1,gvi_Providers,1
          temp(i) = gvf_landCover_2010(1,i)
          !
          lvl_mask(i)=.true.
            ! 05.24.16,09.01.16 - changed to 1% cutoff
            if(temp(i) < 1)then
              lvl_mask(i)=.false.
            endif
            !
            ! This may need changing when we start to use no provider and other provider
            ! 10.18.16 DAS
!            if(i == 22 .or. i == 23)then
!              lvl_mask(i)=.false.
!            endif

        end do
       !
  return
end subroutine sMaskAgriculture
! -----------------------------

! ----------------------------------------------
subroutine sNewMaskAgriculture(lvl_mask,LULCagriculture,totalArea,agArea)
 use gm_GlobalData
    !
    ! ------------------- Types --------------------------
    integer :: i
    real :: LULCagriculture(gvi_maxProV)
    real :: totalArea(gvi_maxProV),agArea(gvi_maxProV)
    logical :: lvl_mask(gvi_maxProV)

    character(len=2), dimension(gvi_maxProV) :: providers
    ! ====================================================
    !
        ! NOTE: LULCagriculture(i)=gvf_LULCProp(10,i)
        call sProviders(providers)
        do i = 1,gvi_Providers,1
          lvl_mask(i)=.true.
          agArea(i)=0
          if(0 < LULCagriculture(i))then     
            !
            agArea(i)=totalArea(i)*LULCagriculture(i)
            !
          else
            !
            lvl_mask(i)=.false.
            !
          endif
        end do
       !
  return
end subroutine sNewMaskAgriculture
! -----------------------------


! ---------------------------------------
subroutine AgAcerageAndCredits(T,lvf_relArea)
 use lms_LULC
    ! ---------- Types -----------
    integer i,j,k
    real :: lvf_totArea
    real :: vArea(gvi_maxProV)
    real :: lvf_totAcreage
    real :: lvf_relArea(gvi_maxProV)
    !real :: lvf_temp(gvi_maxProV)
 !   real :: temp
    real,parameter :: lpf_minAcreage=10
    ! ============================

        ! - Type Construct -
        type(runTime)T
        ! ==================
        !
        call ProviderArea2012(vArea)
        !
        if(gvl_LULCstatus)then
            do j = 1,gvi_Providers,1
              gvf_acerageAg(T%year,j) = 0
             gvf_acerageAg(T%year,j) = gvf_propLandCover(1,j) * gvf_acerageTotal(j)
             if(gvf_acerageAg(T%year,j) < lpf_minAcreage)gvf_acerageAg(T%year,j) =0
            end do
        else
            ! 09.04.16 new code for the new WaterSim 5.9.2
            ! The idea is to add or subtract agricultural lands based on
            ! inputs from the UI using the field "WEBAGTR1"
            ! -----------------------------------------------------------
            if(T%year < 2011)then
                do j = 1,gvi_Providers,1
                  gvf_acerageAg(T%year,j) = 0
                 gvf_acerageAg(T%year,j) = gvf_propLandCover(1,j) * gvf_acerageTotal(j)
                 if(gvf_acerageAg(T%year,j) < lpf_minAcreage)gvf_acerageAg(T%year,j) =0
                    ! This may at some point NEED to be removed
                    if(21 < j .and. j < 24)then
                      gvf_acerageAg(T%year,j) =0
                    endif
                end do
            else
                ! Calculate Agricultural AF per acre for those providers that have
                ! agricultural lands based on gvf_propLandCover(1,k)
                if(T%year <=2015)then
                  do k = 1,gvi_Providers,1
                   gvf_AgACperAF2015(k)=0
                    if(0 < gvf_AgAFGWTotal(T%year-1,k))then
                       gvf_AgACperAF2015(k)=gvf_acerageAg(T%year,k)/gvf_AgAFGWTotal(T%year-1,k)
                    endif
                  end do

                endif
                do k = 1,gvi_Providers,1
                   gvf_acerageAg(T%year,k) = gvf_AgACperAF2015(k)  * gvf_AgAFGWTotal(T%year-1,k)
                   if(gvf_acerageAg(T%year,k) < lpf_minAcreage)gvf_acerageAg(T%year,k) =0
                end do
                !
            endif
        endif
        !
1003 format(I4,1x,L2,1x,I2,1x,4(F10.2,1x))
         lvf_totArea=0
         lvf_totAcreage=0
        !
        do k = 1,gvi_Providers,1
          lvf_totArea=lvf_totArea+vArea(k)  
          lvf_totAcreage = lvf_totAcreage +  gvf_acerageAg(T%year,k) 
        end do
          !
        do i = 1,gvi_Providers,1
          lvf_relArea(i)=0

            if(0 < lvf_totAcreage)then
             lvf_relArea(i)=gvf_acerageAg(T%year,i) / lvf_totAcreage
             ! 11.20.16
             gvf_AgCreditWeight(T%year,i)=lvf_relArea(i)
            else
             ! gvl_errorFlag=.false.
             lvf_relArea(i)=0
            endif
        end do

        do i = 1,gvi_Providers,1
          if(gvl_LULCstatus)then
            if(T%year < 2060)then
              gvf_relativeAgArea(i)=lvf_relArea(i)
            else
              lvf_relArea(i)=gvf_relativeAgArea(i)
            endif
          else
             gvf_relativeAgArea(i)=lvf_relArea(i)
          endif
        end do
        !
  return
end subroutine AgAcerageAndCredits
! ---------------------------------------

! ----------------------------------------------
subroutine AgAcerageAndCreditsCAP(T,lvf_relArea)
 use lms_LULC
    ! ---------- Types -----------
    integer i,j,k
    real :: lvf_totAreaCAP
    real :: vArea(gvi_maxProV)
    real :: lvf_totAcreageCAP
    real :: lvf_relArea(gvi_maxProV)
    real,parameter :: lpf_minAcreage=1
    ! ============================

    ! - Type Construct -
    type(runTime)T
    ! ==================
    !
        call sMaskCAP(gvl_maskCAP)
        !
        call ProviderArea2012(vArea)
        !
        do j = 1,gvi_Providers,1
          gvf_acerageAg(T%year,j) = 0
         gvf_acerageAg(T%year,j) = gvf_propLandCover(1,j) * gvf_acerageTotal(j)
         if(gvf_acerageAg(T%year,j) < lpf_minAcreage)gvf_acerageAg(T%year,j) =0
        end do
        !
         lvf_totAreaCAP=0
         lvf_totAcreageCAP=0
        !
        do k = 1,gvi_Providers,1
          if(gvl_maskCAP(k))then
            lvf_totAreaCAP=lvf_totAreaCAP+vArea(k)  
            lvf_totAcreageCAP = lvf_totAcreageCAP +  gvf_acerageAg(T%year,k) 
          endif
        end do
          !
        do i = 1,gvi_Providers,1
         lvf_relArea(i)=0
         gvf_relativeAgAreaCAP(i)=0
          !
          if(gvl_maskCAP(i))then
            lvf_relArea(i)=gvf_acerageAg(T%year,i) / lvf_totAcreageCAP
            !
            if(T%year < 2060)then
              gvf_relativeAgAreaCAP(i)=lvf_relArea(i)
            else
              lvf_relArea(i)=gvf_relativeAgAreaCAP(i)
            endif
          endif
        end do
        !
  return
end subroutine AgAcerageAndCreditsCAP

! --------------------------------------- 
!subroutine sMaskAgGW(lvl_mask,array_last)
! use gm_GlobalData
!    !
!    ! ------------- Types --------------------------------
!    integer :: i
!    integer :: array_last
!
!    logical :: lvl_mask(gvi_maxProV)
!
!    character(len=2), dimension(gvi_maxProV) :: providers
!    ! ====================================================
!    !
!        ! 03.03.14 after speaking with Ray Quay
!        ! Assuming that all providers have access to Ag groundwater
!        ! ==========================================================
!        call sProviders(providers)
!        do  i = 1,gvi_Providers,1
!             !
!             lvl_mask(i)=.true.
!             array_last=i
!        end do
!        !
!    !
! return
!end subroutine sMaskAgGW
! ----------------------

! --------------------------------------------
!subroutine sMaskAgSurface(lvl_mask,array_last)
! use gm_GlobalData
!    !
!    ! ------------------ Types --------------------------------
!    integer :: i
!    integer :: array_last
!
!    logical :: lvl_mask(gvi_maxProV),lvl_cap(gvi_maxProV)
!    !character(len=2), dimension(gvi_maxProV) :: providers
!    ! =========================================================
!    !
!        !
!        !call sProviders(providers)
!        call sMaskCAP(lvl_cap)
!        !
!        do  i = 1,gvi_Providers,1
!          lvl_mask(i)=.false.
!            !
!         if(providers(i) == 'ad' .or. providers(i)=='av' .or. providers(i)=='be' &
!            .or. providers(i)=='bu' .or. providers(i)=='sp' .or. providers(i)=='cu' &
!            .or. providers(i)=='dh'.or. providers(i)=='em'.or. providers(i)=='go' &
!            .or. providers(i)=='lp' .or. providers(i)=='no' &
!            .or. providers(i)=='rg'.or. providers(i)=='ry'  &
!            .or. providers(i)=='sr'.or. providers(i)=='to'.or. providers(i)=='vu'  &
!            .or. providers(i)=='we')then     
!
!            endif
!        end do
!        !
!    !
!  return
!end subroutine sMaskAgSurface
! ---------------------------

! ------------------------------
! Self explanatory
! ==============================
! ------------------------------
subroutine sProviders(providers)
    !
    ! ----------------- Types ------------------
    character(len=2), dimension(35) :: providers
    ! ==========================================
        !                             
       providers(1) ='ad'
       providers(2) ='wt'
       providers(3) ='pv'
       providers(4) ='su'
       providers(5) ='sw'
       providers(6) ='av'
       providers(7) ='be'
       providers(8) ='bu'
       providers(9) ='cf'
       providers(10)='cc'
       providers(11)='ch'
       providers(12)='cp'
       providers(13)='sp'
       providers(14)='cu'
       providers(15)='dh'
       providers(16)='em'
       providers(17)='gi'
       providers(18)='gl'
       providers(19)='go'
       providers(20)='lp'
       providers(21)='me'
       providers(22)='no'
       providers(23)='op'
       providers(24)='pe'
       providers(25)='ph'
       providers(26)='qk'
       providers(27)='rg'
       providers(28)='rv'
       providers(29)='ry'
       providers(30)='sc'
       providers(31)='sr'
       providers(32)='te'
       providers(33)='to'
       providers(34)='vu'
       providers(35)='we'
    !
  return
end subroutine sProviders
!-------------------------
    
! --------------------------------------------------------------
subroutine sModifyNormalFlow(T,gvi_providerMax,lvf_in,lvf_out)
  use gm_ModelControl
    !
    ! -------------------- Types -----------------
    integer :: i
    integer :: gvi_providerMax(10)

    real :: lvf_in(10),lvf_out(10)
    real :: lvf_normalFlow(10),lvf_modNormalFlow
    real :: cf
    real, parameter :: lpf_max=5.4228
    real :: gvf_providerMax(10)
    ! ===========================================
    !

    !--- TYPE constructs ---
    type(runTime)T
    ! ======================
        !
            ! NO LONGER sure about this assumption
            ! 03.28.14 das
             lvf_modNormalFlow=1.
            call ClimateFactorSVT(T%simyear+1,cf)
            do i = 1,10,1
                !
                lvf_out(i)=0.9
                !
                gvf_providerMax(i)=gvi_providerMax(i)*0.01
                !
                lvf_modNormalFlow=lvf_in(i)*(1./cf)
                !
                lvf_normalFlow(i)=min(lvf_in(i), gvf_providerMax(i))
                !
                if(cf < 0.9999999)then
                 lvf_normalFlow(i)=min(gvf_providerMax(i),lvf_modNormalFlow)
                else
                
                endif
             !
             lvf_out(i)=min(lpf_max,lvf_normalFlow(i))
            end do
        !
    !
  return
end subroutine sModifyNormalFlow
! -------------------------------
!




!
! ======================================================================================================
!E.O.F GlobalSubroutines.f90