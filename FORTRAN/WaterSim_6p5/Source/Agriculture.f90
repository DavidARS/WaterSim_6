!
! File is Agriculture.f90
!
!   This Module models the Regional Agriculture pool and fluxes.  And, it controls
! some of the provider water balance for municipal water providers
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

! Module:       Module lm_AgWaterBudgets
! Subroutines:  subroutine initAg()
!                 calls(also present):
!                   call openFiles_ag()
!                   call readFiles_ag()
!
! No Module:    subroutine initializeAgriculture()
!               subroutine sAgSurfaceWater(lvf_add)

! Module:       lms_AgWaterBudgets
! Subroutines:  subroutine  AgricultureDemand(T,i,effluent,lvf_GWtoAg,VadoseFromAg,lvf_AgEvap)
!
! No Module:    subroutine sAgDemand(T,prov,effluent,GW,Vad,Evap)
!               subroutine sAgInitialize_Water()
!

! Global OUTPUTS:  
!
! Local OUTPUTS:             
!   
! Local INPUTS:
!
! created on 05.09.12
!
! david arthur sampson

! last write was: 01.16.13,10.17.13,06.09.14,07.20.14
! ---------------------------------------------------
!

! ======================================================================================================
!
Module lm_AgWaterBudgets
 use gm_ModelControl
    use gm_GlobalData
   
    implicit none
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
  contains
    !
        ! -------------------
        subroutine initAg()
          call openFiles_ag()
          call readFiles_ag()
         return
        end subroutine initAg
        ! -------------------

        ! -----------------------
        subroutine openFiles_ag()
            !
            ! --------------- Types ------------
             character(len=200) :: lvc_DPath=' '
            ! ==================================
                !
                if(gpl_release)then
                    lvc_DPath=trim(gvc_DPath)
                else
                    lvc_DPath=gvc_Path 
                endif
                !
                module="lm_AgWaterBudgets"
                !
                ! 03.13.14 DAS
                Infile='App_Data\Data\agPumpingSurface_2085.txt'; LU=90
                call openFiles(module,lvc_DPath,Infile,LU)

!                Infile='App_Data\Data\EffluentToAgBaseline.txt'; LU=91
!                call openFiles(module,lvc_DPath,Infile,LU)

                ! moved here on 03.04.14
                Infile='App_Data\Data\providerAcres_2012.txt'; LU=52
                call openFiles(module,lvc_DPath,Infile,LU)
                !
            !
         return
        end subroutine openFiles_ag
        ! ---------------------------

      ! -----------------------
      subroutine readFiles_ag()
        !
        ! ---------------------- Types -------------------------
        integer :: i,j,ios,LU
        real :: lvf_year,lvf_pumpingAg,lvf_surfaceAg,lvf_ag_CAP
        ! ======================================================
        !
            !
            LU=90
          do i = gpi_minPVY,gpi_maxPVY,1
            read(LU,*,err=10,iostat=ios)lvf_year,lvf_pumpingAg,lvf_surfaceAg,lvf_ag_CAP
             gv_AgWaterPumpingSRV(i,1)=0.
            gv_AgWaterPumpingSRV(i,1)=lvf_pumpingAg
             gv_AgWaterPumpingSRV(i,2)=0.
            gv_AgWaterPumpingSRV(i,2)=lvf_surfaceAg
             gv_AgWaterPumpingSRV(i,3)=0
            gv_AgWaterPumpingSRV(i,3)=lvf_ag_CAP
             gv_AgWaterPumpingSRV(i,4)=0
             gv_AgWaterPumpingSRV(i,5)=0
            gv_AgWaterPumpingSRV(i,6)=0
            lvf_year=lvf_year*1 ! Only used to surpress verbose warnings
          end do
10          continue
            close(LU)
            if(0 < ios)then
             goto 1000
            endif
            !
!             do i = gpi_minPVY,gpi_maxPVY,1
!              gvf_EffluentToAg_baseline(i)=0
!             end do
!            LU=91
!            read(LU,*,err=20,iostat=ios)(gvf_EffluentToAg_baseline(i),i=gpi_lBY,gpi_uBY)
!20          continue
!            close(LU)
!            if(0 < ios)then
!             goto 1000
!            endif
            !
            ! Missing "Other Provider"
            do i = 1,gvi_Providers,1
              lid_providerAcres(i,1)=0
              lid_providerAcres(i,2)=0
            end do
            LU=52
            read(LU,*,err=30,iostat=ios)((lid_providerAcres(i,j),j=1,2),i=1,gvi_maxProV)
30          continue
            close(LU)
            !
             !
            if(0 < ios)then
             goto 1000
            endif
            !
         return
1000     continue
            if(gvl_writeLog)then
              string=13
                call sStrings(string,errorString)
                call eWrite(errorString,LU)
            endif
           gvl_errorFlag=.false.
           !
        return
      end subroutine readFiles_ag
      ! -------------------------
!
End Module lm_AgWaterBudgets
!
    ! --------------------------------
    subroutine initializeAgriculture()
      use lm_AgWaterBudgets
        !
        call initAg()
        call sAgInitialize_Water()
        call sAgPumpingSES()
        call sAgSurfaceSES
        !
      return
    end subroutine initializeAgriculture
    !-----------------------------------

! ======================================================================================================
!
Module lms_AgWaterBudgets
 use gm_ModelControl
  use gm_GlobalData
    use gm_TypeControl
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
        real :: lmf_addAgWaterFromSurface(gvi_maxProV)
    !
 contains
    !
        ! --------------------------------------------------------------------
        subroutine AgricultureDemand(T,i,effluent)
            ! ---------------- Types ---------------
            integer:: i
            real :: effluent
!            real :: lvf_Demand(gvi_maxProV)
!            real :: lvf_excessEffluent(gvi_maxProV),lvf_diffDemand(gvi_maxProV)
!            real :: lvf_excessSW(gvi_maxProV),lvf_excessGW(gvi_maxProV),lvf_unmetDemand(gvi_maxProV)
            !
            real :: GW,SW,EFF
            real :: GWtransfers,CAPtransfers!,CAPusedByAg
            real :: lvf_temp
            real :: lvf_weightedRatioGW(gvi_maxProV)
            real :: lvf_weightedRatioSW(gvi_maxProV)
            real :: lvf_temp_1,lvf_temp_2!,lvf_temp_3
            real :: lvf_AgEfficiency
            ! =====================================
            ! - Type Constructs -
            type(runTime)T
            ! ===================
!                !
                GWtransfers=0; CAPtransfers=0; GW=0; SW=0; EFF=0; 
                !
                lvf_AgEfficiency=1.0
                if( 0 < go_AgEfficiency)lvf_AgEfficiency=1. + (1-go_AgEfficiency*1./100)

                if(T%year < gpi_lclu)then
                else
                    !
                  call sMaskCAP(gvl_maskCAP)
                  !
                  GWtransfers=go_AgWaterGW_AF(i)
                  CAPtransfers=gvf_AgCAPused_AF(i)

                    ! =================================================================
                    !  Supplies
                    !
                    ! ------- Set the Raw Data for Ag Pumping here ------
                    ! 
                    ! ---------------------------------------------------
                    !  From Below
                    ! 11.18.16  
                    GW=go_WaterFromAgPumping_acft_a(i)
                    SW= go_AgWaterCAP_AF(i)
                    EFF=effluent
                    !
                    if(0 < lvf_AgEfficiency)EFF=effluent*lvf_AgEfficiency
                    !
                    gvf_AgWaterTotalsP(1,i)=GW
                    gvf_AgWaterTotalsP(2,i)=SW
                    gvf_AgWaterTotalsP(3,i)=EFF
                    !
                    if(T%year == gvi_baseYear-1)then
                      gvf_AgGroundWater_2015p(i) =GW
                      gvf_AgSurfaceWater_2015p(i)=SW
                      gvf_AgEffluent_2015p(i)=EFF    
                    endif
!                  !
!                  ! ===================================================================
!                  ! Demands
!                  !
                    lvf_temp=(GW + max(0,SW-CAPtransfers)+EFF)*lvf_AgEfficiency
                   !
                   go_AgToLCLUdemand(i)=nint(lvf_temp)
!                  !
!                  ! =====================================================================
!                  ! To the interface 09.02.16
!                  ! --------------------------
                    if(gvi_baseYear-1 <= T%year)then
                      GWtransfers=0; CAPtransfers=0; GW=0; SW=0
                     !
                     lvf_weightedRatioGW(i)=0
                     lvf_weightedRatioSW(i)=0
                    ! lvf_weightedRatioEFF(i)=0
                     !
                       lvf_temp=0
                      lvf_temp=gvf_AgWaterTotalsP(1,i)+gvf_AgWaterTotalsP(2,i)
                        if(0 < lvf_temp)then
                          lvf_weightedRatioGW(i) =gvf_AgWaterTotalsP(1,i)/lvf_temp
                          lvf_weightedRatioSW(i) =gvf_AgWaterTotalsP(2,i)/lvf_temp
!                          lvf_weightedRatioEFF(i)=gvf_AgWaterTotals(3,i)/lvf_temp
                        endif
                        !
                          go_AgWaterUsedByMuni_PCT_a(i)=0
!                          !
                          lvf_temp_1=0
                          lvf_temp_2=0
!                          lvf_temp_3=0
                          !
                        if(0 < gvf_AgGroundWater_2015p(i))then
                            GWtransfers=go_AgWaterGW_AF(i)
                            GW=gvf_AgWaterTotalsP(1,i)
                            !
                            if(21 < i .and. i < 24)then
                            else
                              lvf_temp_1= GWtransfers/gvf_AgGroundWater_2015p(i)
                            endif
                        endif
                        !
                        if(0 < gvf_AgSurfaceWater_2015p(i))then
                            CAPtransfers=gvf_AgCAPused_AF(i)
                            SW=gvf_AgWaterTotalsP(2,i)
                            !
                            if(21 < i .and. i < 24)then
                            else

                             lvf_temp_2= CAPtransfers/gvf_AgSurfaceWater_2015p(i)
                            endif
                        endif
                        !
!                        if(0 < gvf_AgEffluent_2015p(i))then
!                            EFF=gvf_AgWaterTotalsP(3,i)
!                            !
!                            if(21 < i .and. i < 24)then
!                            else
!
!                             lvf_temp_3=(EFF/gvf_AgEffluent_2015p(i))* lvf_weightedRatioEFF(i)*100
!                            endif
!                        endif
!                        !
                        go_AgWaterUsedByMuni_PCT_a(i)=nint( ( ((lvf_temp_1*lvf_weightedRatioGW(i))*100)) + ( ((lvf_temp_2*lvf_weightedRatioSW(i))*100)) )*gvf_AgCreditWeight(T%year,i)
!                        !
                  endif
!                  ! =====================================================================
                endif
!                !
                 ! ===================================================================================
                  ! WaterSim 6 addition
                  ! =====================
!                  if(gvl_LULCstatus)then
!                    if(gvi_baseYear <= T%year)then
!!                     lvf_Demand(i)=gvf_agDemand_acft(i)
!                     !  
!!                        gvf_agUsedEffluent(i)=EFF
!!                        gvf_agUsedSW(i)=SW
!                      !  if(0 < GW)go_pumpingAgTotal_AF(i)=GW
!                     !
!                      go_effluentAgProvider_AF(i)=0
!                     if(0 < lvf_Demand(i))then
!                     ! First Level
!                      if(EFF < lvf_Demand(i))then
!                       lvf_diffDemand(i)=lvf_Demand(i)-EFF
!                       go_effluentAgProvider_AF(i)=EFF
!                      else
!                       lvf_excessEffluent(i)=EFF-lvf_Demand(i)
!                       go_effluentAgProvider_AF(i)=lvf_Demand(i)
!                       lvf_diffDemand(i)=0
!                      endif
!
!                     ! Second Level
!                      !lvf_temp=max(0,SW-CAPtransfers)
!                      lvf_temp=SW
!                        go_surfAgProvider_AF(i)=0
!                      if(lvf_temp <  lvf_diffDemand(i))then
!                        lvf_diffDemand(i)=lvf_diffDemand(i)-lvf_temp
!                        go_surfAgProvider_AF(i)=nint(lvf_temp)
!                        lvf_excessSW(i)=0
!                      else
!                        lvf_excessSW(i)=lvf_temp-lvf_diffDemand(i)
!                        go_surfAgProvider_AF(i)=nint(lvf_diffDemand(i))
!                        lvf_diffDemand(i)=0
!                      endif
!
!                     ! Third Level
!                      lvf_temp=GW
!                      if(lvf_temp < lvf_diffDemand(i))then
!                        lvf_diffDemand(i)=lvf_diffDemand(i)-lvf_temp
!                        go_groundwaterAgProvider_AF(i)=nint(lvf_temp)
!                        lvf_excessGW(i)=0
!                        lvf_unmetDemand(i)=lvf_diffDemand(i)
!                      else
!                        lvf_excessGW(i)=lvf_temp-lvf_diffDemand(i)
!                        go_groundwaterAgProvider_AF(i)=nint(lvf_diffDemand(i))
!                        lvf_diffDemand(i)=0
!                      endif
!                    endif
                    !
!                  else
                    go_groundwaterAgProvider_AF(i)=max(0,(gvf_AgCreditWeight(T%year,i)*gvf_pumpingAg))
                    go_surfAgProvider_AF(i)=max(0,SW-CAPtransfers)
                    go_effluentAgProvider_AF(i)=EFF
!                  endif
!                endif

!100 format(I4,1x,I2,1x,5(F9.2,1x))
30  format(I4,1x,I2,1x,4(F10.2,1x))
!                !
          return
        end subroutine AgricultureDemand
        ! --------------------------------------------------------------------

        ! --------------------------------------------------------------------
        subroutine AgricultureProduction(T,effluent)

            ! ----------------- Types ---------------
            real :: GW,SW,effluent
            real :: lvf_agTotalWaterUse
            real :: lvf_agProduction,lvf_agPool
            real :: lvf_AgEfficiency=1
            real :: GWtransfers !,CAPtransfers
            real :: lvf_totalCredits
            real,parameter :: lpf_kilosPerAF=500
            real :: lvf_kilos2015,lvf_kilosCurrent
            real :: lvf_kilosStatic

            real :: lvf_temp_1,lvf_temp_2,lvf_temp_3
            real :: lvf_unPumpedGW
            ! =======================================
            !
            ! - Type Constructs -
            type(runTime)T
            ! ===================
                !
                GWtransfers=0
                GW=0
                SW=0
                lvf_totalCredits=0
                go_AgProductionPCT=0
                lvf_unPumpedGW=0
                !
                     lvf_agPool=0
                    lvf_agPool=gvi_Agpool_acft_a

!                  lvf_AgEfficiency=1.0
!                  if( 0 < go_AgEfficiency)lvf_AgEfficiency=go_AgEfficiency
                  lvf_AgEfficiency=1.0
                  ! Now scalled from 20 to 100
                  if( 0 < go_AgEfficiency)lvf_AgEfficiency=1. + (1-go_AgEfficiency*1./100)

                  !
                  if(T%year <= gvi_baseYear-1)then
                    
                    gvf_AgGroundWater_2015 =gv_AgWaterPumpingSRV(T%year,1) 
                    gvf_AgSurfaceWater_2015=gv_AgWaterPumpingSRV(T%year,5)
                    gvf_AgEffluent_2015=effluent
                    if(0 < lvf_agPool)then
                      gvf_ratioMaricopaAg=gv_AgWaterPumpingSRV(T%year,5)/lvf_agPool
                    endif
                  else
                    if(0 < lvf_agPool)then
                      gvf_ratioMaricopaAg=gv_AgWaterPumpingSRV(T%year,5)/lvf_agPool
                    endif
                  endif
                  !
                   if(gvi_baseYear-1 <= T%year)then
                     !
                      lvf_temp_1=0
                      lvf_temp_2=0
                      lvf_temp_3=0
                     !
                     GWtransfers=sum(go_AgWaterGW_AF)
                    ! CAPtransfers=sum(gvf_AgCAPused_AF)
                    !
                    gvf_EffluentToAg_diff(T%year)=(effluent-gvf_AgEffluent_2015)
                    !
30 format(I4,1x,4(F10.1,1x))
                    !
                    !
                    ! I had been subtracting the GW transfers from GW, but in fact, they have already been subtracted
                    ! by virtue of the curve itself, no?
                    ! 10.21.16
                    ! =======================================
                     lvf_totalCredits=gvf_AgEffluent_2015+gvf_AgSurfaceWater_2015+gvf_AgGroundWater_2015
                    !
                    lvf_kilos2015 = lpf_kilosPerAF*(gvf_AgEffluent_2015+gvf_AgSurfaceWater_2015+gvf_AgGroundWater_2015)

                    ! gv_AgWaterPumpingSRV(T%year,6) is the user defined pumping by Ag (or, credit transfer difference)
                     GW=max(0, min(lvf_totalCredits-GWtransfers,gv_AgWaterPumpingSRV(T%year,6)- gvf_EffluentToAg_diff(T%year))) 
                     SW= max(0, min(gvf_ratioMaricopaAg*lvf_agPool,gv_AgWaterPumpingSRV(T%year,5)))
                     !
                      gvf_AgWaterTotals(1)=GW
                      gvf_AgWaterTotals(2)=SW
                      gvf_AgWaterTotals(3)=effluent
                    !
                    !
                        lvf_agTotalWaterUse=GW+SW+effluent
                         lvf_kilosStatic=0
                         lvf_kilosCurrent=0
                        lvf_kilosStatic=((lpf_kilosPerAF*(1))*lvf_agTotalWaterUse)
                        lvf_kilosCurrent=((lpf_kilosPerAF*(lvf_AgEfficiency**0.93))*lvf_agTotalWaterUse)
                         lvf_agProduction=0
                     !   lvf_agProduction=lvf_agTotalWaterUse*lvf_AgEfficiency
                        lvf_agProduction=lvf_kilosCurrent
                        lvf_unPumpedGW= max(0,lvf_kilosCurrent*((1/lpf_kilosPerAF)-   (1/(lpf_kilosPerAF*(lvf_AgEfficiency**0.93)))))
                    !
                     if(1 < lvf_AgEfficiency)then
                      lvf_temp_1=lvf_kilosStatic
                     else
                      lvf_temp_1=lvf_agProduction
                     endif
                     ! lvf_temp_2=gvf_AgGroundWater_2015+gvf_AgSurfaceWater_2015+gvf_AgEffluent_2015
                      lvf_temp_2=lvf_kilos2015
                      !

!                         lvf_agProduction=0
!                        lvf_agProduction=lvf_agTotalWaterUse*lvf_AgEfficiency
                    !
!                      lvf_temp_1=lvf_agProduction
!                      lvf_temp_2=gvf_AgGroundWater_2015+gvf_AgSurfaceWater_2015+gvf_AgEffluent_2015
                      !
!                      if(0 < lvf_temp_2)lvf_temp_3=lvf_temp_1/lvf_temp_2
                       if(0 < lvf_temp_2)then
                        lvf_temp_3=((lvf_temp_1-lvf_temp_2)/lvf_temp_2)*100
                       endif

                         !
                         lvf_agTotalWaterUse=0
                     !
                     go_AgProductionPCT=nint(lvf_temp_3+100)
                     !
                   else
                    !
                    ! gv_AgWaterPumpingSRV(T%year,6) is the user defined pumping by Ag (or, credit transfer difference)
                     GW=gv_AgWaterPumpingSRV(T%year,6)
                     SW= max(0, min(gvf_ratioMaricopaAg*lvf_agPool,gv_AgWaterPumpingSRV(T%year,5)))
                     !
                      gvf_AgWaterTotals(1)=GW
                      gvf_AgWaterTotals(2)=SW
                      gvf_AgWaterTotals(3)=effluent

                      lvf_temp_1=0
                      lvf_temp_2=0
                      lvf_temp_3=0
                    !
                    go_AgProductionPCT=100
                    !
                   endif
                    !
                     go_surfaceAgTotal_AF(T%year)=0
                     go_pumpingAgTotal_AF(T%year)=0
                    go_surfaceAgTotal_AF(T%year)=nint(SW)
                    !go_pumpingAgTotal_AF(T%year)=nint(GW)
                    !
                     go_pumpingAgTotal_AF(T%year)=nint(max(0, GW-lvf_unPumpedGW))
                    ! If Ag efficiency is invoked, we cannot loose acres just because
                    ! they are pumping less, thus, the need for this damn variable
                    ! 03.14.14 das
                     go_pumpAgTotalEfficiency_AF(T%year)=GW  

10   format(I4,1x,3(F10.1,1x))
100  format(I4,1x,2(F9.0,1x))
1000 format(I4,1x,5(F9.0,1x))

          return
        end subroutine AgricultureProduction
        ! --------------------------------------------------------------------

       ! --------------------------------------------------------------------
        subroutine AgriculturalBudgets(T,i,effluent,GW,lvf_AgToVadose,lvf_AgEvap)

            ! ----------------- Types ---------------
            integer :: i
            real :: GW,SW,effluent
            real :: temp
            real :: lvf_temp
            real :: lvf_AgToVadose,lvf_AgEvap
            real :: lvf_AgEfficiency
            real,parameter :: lpf_AgEvap=0.793
            real,parameter :: lpf_AgToVadose=0.20 
            ! =======================================
            !
            ! - Type Constructs -
            type(runTime)T
            ! ===================
                !
!                  lvf_AgEfficiency=1.0
!                  if( 0 < go_AgEfficiency)lvf_AgEfficiency=go_AgEfficiency
                  lvf_AgEfficiency=1.0
                  ! Now scalled from 20 to 100
                  if( 0 < go_AgEfficiency)lvf_AgEfficiency=1. + (1-go_AgEfficiency*1./100)

                    !
                  GW=0
                  SW=0
                  temp=0
                if(T%startyear < T%year)then
                   temp=gvf_AgCreditWeight(T%year,i)
                    !
                     GW=go_pumpingAgTotal_AF(T%year-1)*temp
                     SW= go_surfa   ceAgTotal_AF(T%year-1)*temp
                     SW=SW + gvi_WaterToAgriculture_acft_a(i)
                     !
                      ! Send data to subroutine AgAcerageAndCredits(T,lvf_relArea)
                      ! to alter acres of agricultural lands in a provider boundary
                      !gvf_AgAFGWTotal(T%year,i)=GW
                      ! Send data to subroutine AgAcerageAndCredits(T,lvf_relArea)
                      ! to alter acres of agricultural lands in a provider boundary
                      gvf_AgAFGWTotal(T%year,i)=go_pumpAgTotalEfficiency_AF(T%year-1)*temp
                      ! ===========================

                      ! ===========================
                else
                    GW=0
                    SW=0
                    temp=0
                endif
                    !
                     go_WaterFromAgPumping_acft_a(i)=0
                    go_WaterFromAgPumping_acft_a(i)= GW 
                     go_AgWaterCAP_AF(i)=0
                    go_AgWaterCAP_AF(i)=SW
                    !
310 format(I4,1x,I2,1x,6(F12.2,1x))
                  ! Demands
                  !
                    lvf_temp=GW+SW+ (lvf_AgEfficiency*effluent)
                  !
                   lvf_AgToVadose=0
                  lvf_AgToVadose=lvf_temp*lpf_AgToVadose
                  !
                   lvf_AgEvap=0
                  lvf_AgEvap=lpf_AgEvap*(lvf_temp)
                  !
10   format(I4,1x,I4)
100  format(I4,1x,2(F9.0,1x))
1000 format(I4,1x,5(F9.0,1x))
          return
        end subroutine AgriculturalBudgets
        ! --------------------------------------------------------------------

! 
End Module lms_AgWaterBudgets
!
    ! ------------------------------
    subroutine sAgDemand(T,prov,effluent)
      use lms_AgWaterBudgets
        ! ----- Types -------
        integer:: prov
        real :: effluent
        ! ===================
        ! - Type Constructs -
        type(runTime)T
        ! ===================
            !
            call AgricultureDemand(T,prov,effluent)
            !
      return
    end subroutine sAgDemand
    ! ------------------------------

    ! ------------------------------
    subroutine sAgProvider(T,i,effluent,AgGW,Tovadose,lvf_toEvapoFromAg)
      use lms_AgWaterBudgets
        ! ----- Types -------
        integer :: i
        real :: effluent
        real :: AgGW,Tovadose
        real :: lvf_toEvapoFromAg
        ! ===================
        ! - Type Constructs -
        type(runTime)T
        ! ===================
            !
            call AgriculturalBudgets(T,i,effluent,AgGW,Tovadose,lvf_toEvapoFromAg)
            !
      return
    end subroutine sAgProvider
    ! ------------------------------

   ! ------------------------------
    subroutine sAgProductionStatic(T,effluent)
      use lms_AgWaterBudgets
        ! ----- Types -------
        real :: effluent
        ! ===================
        ! - Type Constructs -
        type(runTime)T
        ! ===================
            !
            call AgricultureProduction(T,effluent)
            !
      return
    end subroutine sAgProductionStatic
    ! ------------------------------




    ! ------------------------------
    subroutine sAgInitialize_Water()
        use lm_AgWaterBudgets
            ! ------------------------- Types -------------------------
            integer :: i,j,k

            real :: lvf_RelativeAreaGW(gvi_maxProV)! m2
            real :: lvf_AreaAgSurface(gvi_maxProV)
            ! ===============================================================
            !
            ! Start of first provider with Ag acreage
            ! -------------------------------------------------------------------------------------------------------------------
            ! 

            ! Groundwater Ag
            ! ----------------------------
            do i = 1,gvi_Providers,1
                if(i == 1)then
                 do k = 1,gvi_Providers,1
                  lvf_RelativeAreaGW(k)=0
                 end do
                 !
                 ! Extract area of a provider that has Agriculture
                 ! -------------------------------------------------
                  gvf_totalAreaAg=0
                 call sMaskAgriculture(gvl_mask)
                 do j = 1,gvi_Providers,1
                  gvf_AreaAg(j)=0
                !
                  if(gvl_mask(j))then
                   call ProviderAreaAg_2012(j,gvf_AreaAg)
                   gvf_totalAreaAg=gvf_totalAreaAg+gvf_AreaAg(j)
                  endif
                 end do
                !
                ! Total, regional Ag pumping from ADWR
                ! Read in from a text file above
                ! --------------
                 gvf_pumpingAg=0
                gvf_pumpingAg=gv_AgWaterPumpingSRV(2000,1)
                !
                gvf_AG_Banking(2000)=0
                !
              endif
            !
            ! ------------------------------------------------------------------------------
            !
            !  Relative area is at the Provider area. Has NOTHING to do with how
            ! much agriculture acres they have. I will improve this as the model
            ! develops. m2 at the point. 
            if( 0 < gvf_totalAreaAg)lvf_RelativeAreaGW(i)=gvf_AreaAg(i)/gvf_totalAreaAg
            !
               ! 05.08.12
                ! Do I limit the request for water from Ag in the interface, or do I set it here and report 
                ! back the result? 01.09.14
                !
                 gvi_defaultAgPumping_acft_a(i)=0
                gvi_defaultAgPumping_acft_a(i)=nint(lvf_RelativeAreaGW(i)*gvf_pumpingAg)
                !
            ! ==============================================================================================================
            !
            ! Surface Ag
            !
            ! ----------------------------
            if(i == 1)then
                 !
                 ! Extract area of a provider that has Agriculture
                ! -------------------------------------------------
                 gvf_totalAreaAgSURF=0
                 call sMaskAgriculture(gvl_mask)
                !
                do j = 1,gvi_Providers,1
                  lvf_AreaAgSurface(j)=0
                !
                  if(gvl_mask(j))then
                   call ProviderAreaAg_2012(j,lvf_AreaAgSurface)
                    gvf_totalAreaAgSURF=gvf_totalAreaAgSURF+lvf_AreaAgSurface(j)

                  endif
                end do
                !
                do k = 1,gvi_Providers,1
                  gvf_relativeAreaAg(k)=lvf_AreaAgSurface(k)/gvf_totalAreaAgSURF
                end do
            endif

            !
         end do
      return
    end subroutine sAgInitialize_Water
    ! --------------------------------

    ! 01.28.15 DAS
    ! estimates the alpha parameter of 
    ! the SES algorithm to model new
    ! Ag pumping based in UI inputs. Used
    ! in subroutine sAgGroundWater(T,lvf_creditTransfer)
    ! found in GlobalSubroutines.f90 (~ line 396)
    ! --------------------------------------------------
    subroutine sAgPumpingSES()
        use lm_AgWaterBudgets
        !
        ! ------------ Types --------------
        integer:: i,j

        real:: IN
        real:: fSES_alpha
        real:: lvf_Alter
        real:: pumping(7),targetArray(30)
        real:: lvf_target,Nalpha, alphaArray(30)    
        real,parameter:: lpf_modAlpha=1.004
        real,parameter:: lpf_minAlpha=0.2
        real,parameter:: lpf_maxAlpha=0.99

        logical :: lvl_error
        ! ================================        
           !
            IN=2010
            lvl_error=.false.
            pumping(1) = gv_AgWaterPumpingSRV(2014,1)
            pumping(2) = gv_AgWaterPumpingSRV(2013,1)
            pumping(3) = gv_AgWaterPumpingSRV(2012,1)
            pumping(4) = gv_AgWaterPumpingSRV(2011,1)
            pumping(5) = gv_AgWaterPumpingSRV(2010,1)
            pumping(6) = gv_AgWaterPumpingSRV(2009,1)
            pumping(7) = gv_AgWaterPumpingSRV(2008,1)
            !
            ! 12088 AF each step; array(3) is odd, but needed.
            !
            targetArray(30)=0
            targetArray(29)=7
            targetArray(28)=12095
            targetArray(27)=24183
            targetArray(26)=36271
            targetArray(25)=48359
            targetArray(24)=60447
            targetArray(23)=72535
            targetArray(22)=84623
            targetArray(21)=96711
            targetArray(20)=108799
            !
            targetArray(19)=120887 ! ~50 % reduction
            targetArray(18)=132975
            targetArray(17)=145063 ! 40% reduction from ADWR pumping estimate
            targetArray(16)=157151
            targetArray(15)=169240
            targetArray(14)=181328
            targetArray(13)=193417
            targetArray(12)=205505
            targetArray(11)=217594
            targetArray(10)=229682

            targetArray(9)=241771 ! this is the approximate ADWR pumping estimate

            targetArray(8)=253860
            targetArray(7)=265948
            targetArray(6)=278037
            targetArray(5)=290125
            targetArray(4)=302214
            targetArray(3)=314302
            targetArray(2)=326391
            targetArray(1)=338479 ! 5% reduction from flat response
            !                       0% reduction is a flat response
             Nalpha=0.99
            !
            do i = 1,30,1
             lvf_Alter=i*4.0 !3.9
             lvf_target= targetArray(i)
             Nalpha= max(lpf_minAlpha,min(fSES_alpha(IN,lvf_Alter,lvf_target,pumping,lvl_error),lpf_maxAlpha))
             alphaArray(i)=Nalpha
            end do
            do j = 1,30,1
             gvf_Ag_alphaPump(j)=alphaArray(j)
             if(28 < j )gvf_Ag_alphaPump(j)=0.4399
             if(29 < j )gvf_Ag_alphaPump(j)=0.35
             ! ModAlpha brings pumping in line with UI controls
             ! i.e., 50% transfer is 50% of threshold by 2085
             gvf_Ag_alphaPump(j)=gvf_Ag_alphaPump(j)*lpf_modAlpha
             !
            end do
      return
    end subroutine sAgPumpingSES
    ! --------------------------

    ! --------------------------
   subroutine sAgSurfaceSES()
        use lm_AgWaterBudgets
        !
        ! ------------ Types -------------
        integer:: i,j

        real:: Nalpha, alphaArray(20)      
        real:: lvf_AlterPumping
        real:: alpha_t
        real, parameter :: a=90.5916
        real, parameter :: b= 0.9969
        real, parameter :: c=-0.0422
        real,parameter:: lpf_minAlpha=0.2
        real,parameter:: lpf_maxAlpha=0.99
        ! ================================        
           !
            do i = 1,20,1
              lvf_AlterPumping=i*20
                alpha_t=90
                if(0 < abs(lvf_AlterPumping))then
                alpha_t=(a*b**abs(lvf_AlterPumping) * abs(lvf_AlterPumping)**c )
                endif
                Nalpha=max(lpf_minAlpha,min(alpha_t*1/100,lpf_maxAlpha))
             alphaArray(i)=Nalpha
            end do
            do j = 1,20,1
             gvf_Ag_alphaSurfOther(j)=alphaArray(j)
            end do
      return
    end subroutine sAgSurfaceSES
!
! =====================================================================================================================
!E.O.F. - Agriculture.f90
