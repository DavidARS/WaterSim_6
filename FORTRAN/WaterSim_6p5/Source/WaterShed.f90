!
! File is WaterShed.f90 (formally SurfaceWater.f90)
!
!   This file combines outputs from the Salt-Verde-tonto River System with
! outputs from the Colorado River System.
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
! Module:       Module lms_WaterShed    
! Subroutines:  subroutine InitGroundwater(vInitGW2000)
!               subroutine initGW(T)
!
! No Module:    subroutine pInitGroundwater(T)
!               subroutine sFlowsSaltTontoVerde(T)
!               subroutine sFlowForEnvironment(T)
!               subroutine pDesignate(T,Aout,Sout)
!


! OUTPUTS:
!
! created on 09.30.09
!
! david arthur sampson

! last write was: 01.30.13,07.18.14
! =================================
!

! =================================================================================================
!
Module lms_WaterShed
 use gm_ModelControl
  use gm_GlobalData
   use gm_TypeControl  
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !

        real,parameter :: mpf_maxEnvRiver=0.15
    !
 contains
        !
        ! -------------------------------------
        subroutine InitGroundwater(vInitGW2000)
            !
            ! -------------- Types -------------
            integer :: i

            real(4) :: vInitGW2000(gvi_maxProV)
            ! ==================================
            !
            !   Read from common block, filled in Groundwater.f90. line 101, unit 33
            ! Array, first column is groundwater designations for each provider
            ! ----------------------------------
            !
            do i = 1,gvi_Providers,1
             vInitGW2000(i)=gvi_designationsGW(i,2)
            end do  
            !
          return
        end subroutine InitGroundwater
        ! ----------------------------

  ! ------------------
  subroutine initGW(T)
    !
    ! ---------- Types ---------------
    integer i

    real  :: vInitGW2000(gvi_maxProV)
    !=================================

    ! - Type Constructs -
    type(runTime)T
    ! ========================
        !
        !   04.26.12 changed- we now read groundwater credits from the GWdesignations file
        ! new data- now found in Groundwater.f90- unit 33
        ! called once
        ! -------------------------------
        !
        call InitGroundwater(vInitGW2000)
        !
        ! 02.02.12 DAS
        ! --------------------------------------------------------------
       
        do i = 1,gvi_Providers,1
         if(0 < gvi_ProviderGroundWater_a(i))then
          vState_WB_Aquifer(T%startyear,i)= gvi_ProviderGroundWater_a(i)
         else
          vState_WB_Aquifer(T%startyear,i)=vInitGW2000(i)
         endif     !
        end do 
        !
    return
  end subroutine initGW
  ! -------------------
 !
End Module lms_WaterShed
!
  ! ----------------------------------
  subroutine pInitGroundwater(T)
   use gm_ModelControl
    use  lms_WaterShed
        !
        ! -Type Constructs -
        type(runTime)T
        ! ==================
          !
          if(T%startyear == T%year)then
            call initGW(T)
          endif
          !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=5
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
         !
     return
  end subroutine pInitGroundwater
 ! ------------------------------

    ! ------------------------------
    subroutine flows(T)
      use lms_WaterShed
        !
        ! -Type Constructs -
        type(runTime)T
        ! =================
        !
            !
            !  Note: here they are defined as maf-separated by river system. They are 
            ! Combined in subroutine designations (designationsDaily.f90) as cfs 
            ! old code          new code                     new code
            !lv_maf_yr(T%year)= (gvd_saltTonto_maf_yr(T%year)+gvd_verde_maf_yr(T%year))
             gvd_SVT_flow_maf(T%year)=0
            gvd_SVT_flow_maf(T%year)=(gvd_saltTonto_maf_yr(T%year)+gvd_verde_maf_yr(T%year))
            !
        !
     return
    end subroutine flows
    ! -----------------------------

    ! ------------------------------
    subroutine sFlowForEnvironment(T)
      use lms_WaterShed
        !
        ! --------- Types ----------
        real :: maxEnvSVT
        real,parameter:: lvf_minimumAF=0.00001 ! 10 AF threshold
        real(8) lvd_flowV,lvd_flowST
        ! ==========================
        !

        ! -Type Constructs -
         type(runTime)T
         ! =================
            !
            maxEnvSVT=0
            lvd_flowV=gvd_verde_maf_yr(T%year)
            lvd_flowST=gvd_saltTonto_maf_yr(T%year)
            !
            ! ====================================================================================================================
            ! Verde
                maxEnvSVT=0
                ! 02.25.15 hardwired to zero until we use this variable
                gvi_WaterToEnvironVerde_acft_a=0
              if(0 <  gvi_WaterToEnvironVerde_acft_a)then
                     ! 15% is the maximum environmental flow for the environment
                     maxEnvSVT=mpf_maxEnvRiver*lvd_flowV
                    if( (gvi_WaterToEnvironVerde_acft_a *gpd_acftTomaf) < maxEnvSVT)then
                        gvd_verde_maf_yr(T%year)=max(0.,lvd_flowV-(gvi_WaterToEnvironVerde_acft_a*gpd_acftTomaf))
                    else
                        gvd_verde_maf_yr(T%year)=max(0.,lvd_flowV-maxEnvSVT)
                    endif
                    !
                    if(gvd_verde_maf_yr(T%year) < lvf_minimumAF)then
                       if(gvl_writeLog)then
                          string=32
                            LU=0
                            call sStrings(string,errorString)
                            call eWrite(errorString,LU)
                        endif
                        !gvl_errorFlag=.false. Commented out on 02.25.15
                      if(gvl_writeLog)then
                        write(7,*)"Verde Flow at Zero because gvi_WaterToEnvironVerde_acft_a makes it so, line 216, WaterShed.f90"
                      endif
                    endif
                else
                    gvd_verde_maf_yr(T%year)=lvd_flowV
                endif
            ! 

            ! Salt-Tonto
                ! 02.25.15 Hardwired to zero until we use this variable
                 gvi_WaterToEnvironSalt_acft_a=0
               if(0 <  gvi_WaterToEnvironSalt_acft_a)then
                     ! 15% is the maximum environmental flow for the environment
                     maxEnvSVT=mpf_maxEnvRiver*lvd_flowST
                    if((gvi_WaterToEnvironSalt_acft_a *gpd_acftTomaf) < maxEnvSVT)then
                        gvd_saltTonto_maf_yr(T%year)=max(0.,lvd_flowST-(gvi_WaterToEnvironSalt_acft_a*gpd_acftTomaf))
                    else
                        gvd_saltTonto_maf_yr(T%year)=max(0.,lvd_flowST-maxEnvSVT)
                    endif
                    !
                    if(gvd_saltTonto_maf_yr(T%year) < lvf_minimumAF)then
                       if(gvl_writeLog)then
                          string=33
                            LU=0
                            call sStrings(string,errorString)
                            call eWrite(errorString,LU)
                        endif
                     !gvl_errorFlag=.false. Commented out on 02.25.15
                     if(gvl_writeLog)then
                       write(7,*)"SaltTonto Flow at Zero because gvi_WaterToEnvironSalt_acft_a makes it so, line 209, WaterShed.f90"
                     endif
                    endif
                else
                    gvd_saltTonto_maf_yr(T%year)=lvd_flowST
                endif
                !
                ! Used to adjust monthly flows used in WaterShed_Verde.f90 and WaterShed_SaltTonto.f90
                ! ------------------------------------------------------------------------------------
                !     
                !         
!                 gvf_ratioVerdeFlowsPostEnv=1
!                if(0 < gvi_WaterToEnvironVerde_acft_a)then
!                  if(0 < lvd_flowV)gvf_ratioVerdeFlowsPostEnv= gvd_verde_maf_yr(T%year)*(1./lvd_flowV)
!                endif
!                 !
!                 gvf_ratioSaltTontoFlowsPostEnv=1
!                if(0 < gvi_WaterToEnvironSalt_acft_a)then
!                    if(0 < lvd_flowST)gvf_ratioSaltTontoFlowsPostEnv=gvd_saltTonto_maf_yr(T%year)/lvd_flowST
!                endif
                !
                !
                gvd_SVT_flow_maf(T%year)= gvd_verde_maf_yr(T%year)+gvd_saltTonto_maf_yr(T%year)
                !
                ! 01.30.13 DAS
                ! -----------------------------------------------------------------
                ! Case 2 has eroneous data- NOT to be used but kept for comparisons
                 lv_maf_yr(T%year)=0
                 lv_maf_yr(T%year)= gvd_SVT_flow_maf(T%year)
            !
     return
    end subroutine sFlowForEnvironment
    ! --------------------------------

 ! ---------------------------------
  subroutine sFlowsSaltTontoVerde(T)
    use lms_WaterShed
        !
        ! ----- Types ----
        integer :: acft=0
        ! ================
        !

        ! --Type Constructs --
        type(runTime)T
        ! ====================
            !
            call adjustedFlowVerde(T,acft)
            call adjustedFlowSaltTonto(T,acft)
            call flows(T)
            call sFlowForEnvironment(T)
            !
        !
    return
  end subroutine sFlowsSaltTontoVerde
 ! ----------------------------------

  ! --------------------------------
  subroutine pDesignate(T,Aout,Sout)
   use lms_WaterShed
    use gm_ModelControl
     use gm_TypeControl
        !
        ! -------------------------- Types ------------------------------------------------
        integer :: i
        real, parameter :: cvf_NCSVolume=0.272500
        real :: cvf_NCS(10)
        data cvf_NCS(1),cvf_NCS(2),cvf_NCS(3),cvf_NCS(4),cvf_NCS(5),cvf_NCS(6) &
         ,cvf_NCS(7),cvf_NCS(8),cvf_NCS(9),cvf_NCS(10) /0,0.1,0,0.1,0.15,0,0.5,0.1,0.05,0/
        ! ======================================================================================
        !

        ! -- Type Constructs ---
        type(RiverA)Aout
        type(Surfacewater)Sout
        type(runTime)T
        ! ======================
            !
            ! Moved here because release now falls at the end of the cycle
            do i = 1,10,1
                Aout%gvf_SRP_NCSmaxAllocations(i)=cvf_NCS(i)*cvf_NCSVolume
            end do
            !
            Sout%gvf_pumpBandC_35_acft= Aout%gvf_classBCavailableTopump
            Sout%sNCSmaxAllocations_maf= Aout%gvf_SRP_NCSmaxAllocations
            !
            ! NOT USED!!! 02.22.13
            ! This gets passed to subroutine pWaterProviders(T,Sout,vPV)
            do i = 1,gvi_Providers,1
              Sout%lvf_unusedClass_BC_acft(i)= 0 !Aout%gvf_Unused_Class_A_acft(i)
            end do
            !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=20
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
            !
        !
    return
  end subroutine pDesignate
  ! -----------------------
!   
! ======================================================================================================
! E.O.F. WaterShed.f90