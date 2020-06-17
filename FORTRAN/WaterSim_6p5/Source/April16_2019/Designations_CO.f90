!
! File is Designations_CO.f90
!
! This file contains the calculations of surface water designations for Coloroado River 
! Water. CAP designations and CAP additions (for now)
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

!
! Global OUTPUTS: 
!                 
!
! Local OUTPUTS: 
!                 
!                                         
!              
! Local INPUTS:             
!
!------------------------------------------------------------------------------------
!
! created on 06.08.10
!
! david arthur sampson

! last write was: 01.18.13,07.18.14
! -----------------------------------

! ======================================================================================================
!
Module lm_Designations_B
 use gm_ModelControl
  use gm_GlobalData
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
 contains
    ! ------------------------------
    subroutine initBdesignations()
      call openFiles_Bdesignations()
      call readParms_pvD()
      call readFiles_Bdesignations()
     return
    end subroutine initBdesignations
    ! ------------------------------

    ! ----------------------------------
    subroutine openFiles_Bdesignations()
            !
            ! --------- Types -----------
             character(len=200) :: lvc_DPath=' '
            ! ==================================
            !
            if(gpl_release)then
                lvc_DPath=trim(gvc_DPath)
            else
                lvc_DPath=gvc_Path
            endif
            !
            module="lm_Designations_B"
            !
           Infile='App_Data\Parameters\parm_PV.dat'; LU=35
           call openFiles(module,lvc_DPath,Infile,LU)

           Infile='App_Data\Parameters\SurfaceWaterDesignations.txt'; LU=36
           call openFiles(module,lvc_DPath,Infile,LU)

           Infile='App_Data\Parameters\CAPdesignations.txt'; LU=43
           call openFiles(module,lvc_DPath,Infile,LU)
        return
    end subroutine openFiles_Bdesignations
    ! ------------------------------------

      ! ------------------------
      subroutine readParms_pvD()
         !
         read(35,*)lid_align_40(1)
         read(35,*)lid_align_40(2)
         read(35,*)lid_align_40(3)
         read(35,*)lid_capEquationparms(1)
         read(35,*)lid_capEquationparms(2)
         read(35,*)lid_capEquationparms(3)
         read(35,*)lid_capacftthresholds(1)
         read(35,*)lid_capacftthresholds(2)
         read(35,*)lid_capacftthresholds(3)
         read(35,*)lid_capacftthresholds(4)
         read(35,*)lid_capacftthresholds(5)
         read(35,*)lid_capminIPW
         close(35)
         !
         return
        end subroutine readParms_pvD
       ! ---------------------------

      ! ----------------------------------
      subroutine readFiles_Bdesignations()
          ! ----- Types -----
          integer :: j,k,ios
          ! =================
           !
           !----------------------------------------------------------------------------
           ! 12.20.11 DAS
           read(43,*,err=34,iostat=ios)((lid_designationsCAP(j,k),k=1,2),j=1,gvi_maxProV)
    34     continue
           if(ios >0)then
            LU=43
            goto 1000
           endif
           close(43) ! added on 08.27.10
           !
           read(36,*,err=36,iostat=ios)(( gvf_designationsCAPandSRP(j,k),k=1,3),j=1,gvi_maxProV)
    36     continue
           if(ios >0)then
            LU=43
            goto 1000
           endif
           close(36) ! added on 03.05.12
            !   Surface water from the ADWR AWS documents for threshold drought analysis of
            ! groundwater use (see Groundwater.f90) .  Three columns, one = CAP,2=SRP, 3=Other
                !
          return
    1000     continue
                !
                if(gvl_writeLog)then
                  string=21
                    call sStrings(string,errorString)
                    call eWrite(errorString,LU)
                endif
              gvl_errorFlag=.false.
              !
        end subroutine readFiles_Bdesignations
    ! ----------------------------------------
   !
End Module lm_Designations_B
!
    ! -------------------------------------
    subroutine initializeColoradoDesig()
      use lm_Designations_B
        !
        call initBdesignations()
        !
      return
    end subroutine initializeColoradoDesig
    ! ------------------------------------
!
! =================================================================================================
!
Module lms_Designations_B
 use gm_ModelControl
  use gm_GlobalData
   use gm_TypeControl
    !
      implicit none
    
    ! ------
     include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
        !
        real:: lmf_NIA,lmf_AgPool,lmf_Other
        !

        ! -------------------- Module Global Type Constructs ----------------------------------------------------
        type Designations_B
         real(8) :: lv_capMandItiers_acft(5,2),lv_CAPIIexcess_acft
         real(8):: lvf_Designation_IV_B_acft(gpi_lBY:gpi_uBY,gvi_maxProV)
         real(8):: lvf_Designation_V_B_acft(gpi_lBY:gpi_uBY,gvi_maxProV)
         real(8):: lvf_Designation_VI_B_acft(gpi_lBY:gpi_uBY,gvi_maxProV)

         real(8) :: lv_realizedDesignation_B_acft(gpi_lBY:gpi_uBY,gvi_maxProV),lv_CAPMandI_II(gpi_lBY:gpi_uBY)
         real(8) :: lvf_align_B_acft(gpi_lBY:gpi_uBY,gvi_maxProV)

         real :: lvf_indianII(gpi_lBY:gpi_uBY),lvf_indianIII(gpi_lBY:gpi_uBY),lvf_CAPdifference(gpi_lBY:gpi_uBY)
         real:: lvf_CAP !,lvf_align_2
         real:: lvf_unUsedP4,lvf_unUsedP5
         real:: mvf_unUsedP4(gvi_maxProV),mvf_unUsedP5(gvi_maxProV)
        end type Designations_B
        ! ============================================================================================================
    !   
  contains
  !
       !----------------------------------------
       subroutine aPartition_B(T,conveyance,vDb)
            !
            ! -------------------------------- Types ------------------------------------------
            integer :: i,j

            real(8), intent(in) :: conveyance
            real(8) :: CAPw
            real(8) :: lv_threshTiers(5),lv_CAPmandiTiers_acft(5,2),lv_CAPindianTiers_acft(5,2)
            real(8), parameter :: Indian2=93302.729
            real(8), parameter :: lv_capIndianPercents_1=30.555555555555555556
            real(8), parameter :: lv_capIndianPercents_2=63.62482
            !
            ! lvd_MandI=620,678
            ! ======================================================================================
            !
            ! 02.27.15 CAP-ASU meeting held on 25 Feb, 2015

            ! Should have 1,415,000 AF
            ! 638,823 AF M&I
            ! 343,079 AF Indian

            ! -- Type Constructs --
            type(Designations_B)vDb
            type(runTime)T
            ! =====================
            !
                ! as of 24 May, 2010 this is total CAP water
                CAPw=0
                CAPw = dnint(conveyance* (1./gpd_acftTomaf))     ! maf a-1 to acft a-1  
                !
                 vDb%lvf_CAP=CAPw
                lmf_NIA=0
    !               if(gpl_Testing)CAPw = 853079
                do i = 1,5,1
                   lv_threshTiers(i)=0.
                  do j = 1,2
                   lv_CAPmandiTiers_acft(i,j)=0.
                   lv_CAPindianTiers_acft(i,j)=0.
                   vDb%lv_capMandItiers_acft(i,j)=0.
                  end do
                end do
                !
                ! Priority Three
                ! CAPw <= 68400 acft - CAP 1
                if(CAPw <= lid_capacftthresholds(1))then
                  lv_CAPindianTiers_acft(1,1)=CAPw*(lv_capIndianPercents_1/100.)
                  lv_CAPindianTiers_acft(1,2)=CAPw-lv_CAPindianTiers_acft(1,1)
                else
                !

                ! CAP 1 satisfied
                ! ----------------
                lv_CAPindianTiers_acft(1,1)=lid_capacftthresholds(1)*(lv_capIndianPercents_1/100.)
                lv_CAPmandiTiers_acft(1,2)=lid_capacftthresholds(1)-lv_CAPindianTiers_acft(1,1)
                ! ----


                !
                ! Priority Four
                !
                ! // Equation criteria satisfied CAPw > 68400 and <=853,079 AcF  - CAP 2
                ! 
                     lv_threshTiers(1)=CAPw- lid_capacftthresholds(1)
                   if(CAPw <=lid_capEquationparms(2))then         
                   !
                   lv_CAPindianTiers_acft(2,1)=anint((1.-(lv_capIndianPercents_2/100.))*lv_threshTiers(1))
                   lv_CAPmandiTiers_acft(2,1) =lv_threshTiers(1)-lv_CAPindianTiers_acft(2,1) !-lid_capacftthresholds(1)
                   !     
                   else
                   !
                    lv_CAPindianTiers_acft(2,1)=anint((1.-(lv_capIndianPercents_2/100.))*lid_capEquationparms(2))
                    lv_CAPmandiTiers_acft(2,1) =lid_capEquationparms(2)-lv_CAPindianTiers_acft(2,1)-lid_capacftthresholds(1)
                    !
                    !
                       !
                          lmf_AgPool=0
                          lmf_Other=0
                        ! Table 5.4 in "Planning for Recovery copyred.pdf" CAP Feb 16, 2012
                       !  Equation kicks in: I = {[32,770 ÷ (E - 853,079)] x W} + (343,079 – {[32,770 ÷ (E - 853,079)] x E})
                       !  (simplified here)
                       !  CAPw is between 853,079 and 1,050,302 AcF 
                       ! -------------------------------------------
                        lv_threshTiers(2)=CAPw -lid_capacftthresholds(1)
                     if(CAPw <=lid_capacftthresholds(2))then
                      !
                       lv_CAPindianTiers_acft(2,2)=(Indian2+((lid_capminIPW * (1./100.))*lv_threshTiers(2)))
                       lv_CAPmandiTiers_acft(2,2) =lv_threshTiers(2) -  lv_CAPindianTiers_acft(2,2)            
                       !
                         lv_CAPindianTiers_acft(2,2)=anint(lv_CAPindianTiers_acft(2,2))
                         lv_CAPmandiTiers_acft(2,2)=anint(lv_CAPmandiTiers_acft(2,2))
                       !
                       lv_CAPindianTiers_acft(2,1)=0.
                       lv_CAPmandiTiers_acft(2,1)=0.
                       !  
                     else
                          lv_threshTiers(2)=lid_capacftthresholds(2)-lid_capacftthresholds(1)
                          !
                          lv_CAPindianTiers_acft(2,2)=(Indian2+((lid_capminIPW * (1./100.))*lv_threshTiers(2)))
                          lv_CAPmandiTiers_acft(2,2) =lid_capacftthresholds(2) &
                            - lv_CAPindianTiers_acft(2,2)-lid_capacftthresholds(1)                        
                         !
                           lv_CAPindianTiers_acft(2,2)=anint(lv_CAPindianTiers_acft(2,2))
                           lv_CAPmandiTiers_acft(2,2)=anint(lv_CAPmandiTiers_acft(2,2))
                          !
                         lv_CAPindianTiers_acft(2,1)=0.
                         lv_CAPmandiTiers_acft(2,1)=0.
                         !
!                         lv_capMandItiers_acft(3,1)=lv_CAPmandiTiers_acft(2,2)
                         ! NIA Priority
!                         lvf_NIApriority=0
!                         lvf_AgPriority=0
                         ! ------- CAP conveyance > 1,050,302 AcF -----
                        lv_threshTiers(3)=CAPw -lid_capacftthresholds(2)
                       if(lid_capacftthresholds(2) < CAPw)then
                           lmf_NIA=max(0,CAPw-lid_capacftthresholds(2) )
                           lmf_AgPool=0
                           lmf_Other=0
                       else
                        lmf_NIA=0
                       endif
                     !
                     endif
                   endif
                endif
                !
               ! When we want to also use indian water
                ! DAS 01.09.12
                ! Indian Priority
                vDb%lvf_indianII(T%year)= lv_CAPindianTiers_acft(2,1)+lv_CAPindianTiers_acft(2,2)
                ! NIA Priority 
                vDb%lvf_indianIII(T%year)= lv_CAPindianTiers_acft(3,1)+lv_CAPindianTiers_acft(3,2)
                vDb%lv_capMandItiers_acft=lv_CAPmandiTiers_acft
                ! the famous "E" equation estimates more M&I than accounted for (the 33 total)
                !  by 18145 AF (non-ag and non indian)- 02.27.15 I now understand why
                vDb%lv_CAPMandI_II(T%year)=vDb%lv_capMandItiers_acft(2,1)+vDb%lv_capMandItiers_acft(2,2)
                !
         return
       end subroutine aPartition_B
       !--------------------------

       ! -------------------------------------
       subroutine aAllocate_B(T,gvi_order,vDb) 
         ! -------------------------------- Types -------------------------------------------
         integer(1) :: gvi_order

         real :: lv_capMandI,lvf_capNIA
         ! ====================================================================================
         ! 5 October 2009 CAP M&I sub-contracts
         !

         ! --- Type Constructs --
         type(Designations_B)vDb
         type(runTime)T
         ! ======================
            !
            call CAPpriorityWater(T,vDb,lv_capMandI,lvf_capNIA)
            !
            call COriverDeltaWater(lv_capMandI,lvf_capNIA)
            !    
            call deltaCAPwater(lv_capMandI,lvf_capNIA)
            !
            call maxCAPdesignations(T,vDb)
            !
            call reductionsInDesignations(T,vDb,gvi_order,lv_capMandI,lvf_capNIA)
            !
            call waterBank(vDb)
            !
            call CAPout(T,vDb,gvi_order,lv_capMandI,lvf_capNIA)
            !
            ! ============================
            !
5           format(I4,2x,I2,2x,1(F9.0,1x))
         ! 
        return
       end subroutine aAllocate_B
       !-------------------------
 
        ! ------------------------------
        subroutine maxCAPdesignations(T,vDb)
            !
            ! --- Types ---
            integer:: i
            ! =============
            !

            ! --- Type Constructs --
            type(Designations_B)vDb
            type(runTime)T
            ! ======================
             !
              do i = 1,gvi_Providers,1
                !
                 vDb%lvf_Designation_IV_B_acft(T%year,i)=0
                 vDb%lvf_Designation_V_B_acft(T%year,i)=0
                vDb%lvf_Designation_IV_B_acft(T%year,i)= nint(lid_designationsCAP(i,1)) 
                vDb%lvf_Designation_V_B_acft(T%year,i)= nint(lid_designationsCAP(i,2)) 
                !
              end do
             !
          return
        end subroutine maxCAPdesignations
        ! ------------------------------

        ! -------------------------------------------
        subroutine CAPneeded(gvi_order,lvf_CAPneeded)
            ! 
            ! ---------------------- Types --------------------------
            integer:: j
            integer(1) :: gvi_order

            real:: lvf_Demand(gvi_maxProV),lvf_CAPneeded(gvi_maxProV)
            ! =======================================================
            !
                call sMaskCAP(gvl_maskCAP)
                !
                do j = 1,gvi_Providers,1
                  ! Set water demand variable
                  lvf_Demand(j)=(gvf_WaterDemand_acft(j,gvi_order,1))
                  !
                  if(gvl_maskCAP(j))then
                      if(0 < lvf_Demand(j))then
                         lvf_CAPneeded(j)=lvf_Demand(j)
                      else
                        lvf_CAPneeded(j)=0
                      endif
                  else
                    lvf_CAPneeded(j)=0
                  endif
                end do
            !
          return
        end subroutine CAPneeded
        ! -------------------------------

        ! -------------------------------
        subroutine reductionsInDesignations(T,vDb,gvi_order,lv_capMandI,lvf_capNIA)
            !
            ! ------------------------------------- Types ----------------------------------
            integer:: i,j
            integer(1) :: gvi_order

            character(len=2),dimension(35):: provid

            real:: lv_capMandI,lvf_capNIA,lvf_capPfive,lvf_capPsix
            real:: lv_sumdesignationsTierII,lv_sumdesignationsTierIV
            real,parameter:: lpf_addP3rightsToMandI=30824
            !real,parameter:: lpf_rightsPriorityFive=168316
            !real,parameter:: lpf_NIAmax=450498
            !real,parameter:: lpf_MaricopaTotal=1415000
            real,parameter:: lpf_NIA_Priority=215000
            !real,parameter:: lpf_PriorityThree=68400

            real:: add
            real:: lvf_addDesignation_IV,lvf_addDesignation_V,lvf_addAg
            real:: lvf_ratioDesignation_IV(gvi_maxProV)
            real:: lvf_ratioTotalDesig_IV(gvi_maxProV),lvf_ratioTotalDesig_V(gvi_maxProV)
            real:: lvf_ratioAgAdditions(gvi_maxProV)
            real:: lvf_addSurfWaterFromAgCAP(gvi_maxProV)
            real:: lvf_CAP_needed(gvi_maxProV)
            real:: lvf_unusedP4(gvi_maxProV),lvf_unusedP5(gvi_maxProV)
            real:: lvf_usedP4(gvi_maxProV),lvf_usedP5(gvi_maxProV),lvf_usedP6(gvi_maxProV)

            real:: lvf_maxP4(gvi_maxProV),lvf_maxP5(gvi_maxProV),lvf_maxP6(gvi_maxProV)
            real:: lvf_availableP4,lvf_availableP5
            real:: lvf_desired_IV(gvi_maxProV),lvf_desired_V(gvi_maxProV),lvf_desired_VI(gvi_maxProV)
            real:: lvf_demandMinusAv(gvi_maxProV),lvf_available_IV(gvi_maxProV)
            real:: lvf_potentialWaterBank(gvi_maxProV)
            real:: lvf_available_V(gvi_maxProV)
            real:: lvf_AgPool
            real:: lvf_reduceAgMax !lvf_reduceAg
            !
          !  real :: lvf_creditsSIX,lvf_totalSIX
            real :: lvf_relArea(gvi_maxProV)
            !
            ! ====================================================================
            !
            ! --- Type Constructs --
            type(Designations_B)vDb
            type(runTime)T
            ! ======================
                !
                !
                if(T%year < T%startyear+1)then
                  do i = 1,gvi_Providers,1
                    gvf_AgCAPused_AF(i)=0
                    gvf_AgCAPpotential_AF=0
                  end do
!                 lvf_reduceAg=1
                 lvf_reduceAgMax=max(0,lvf_capNIA-lpf_NIA_Priority)
                 gvf_AgMax=lvf_reduceAgMax
                endif
                !
                lvf_reduceAgMax=gvf_AgMax
                !
                ! Some rights (30,824 AF) to P4 that we do not 
                ! currently have in our CAP designations input file
                ! 03.12.15
                ! ---------------------------------------------------
                lv_capMandI=max(0,lv_capMandI-lpf_addP3rightsToMandI)
                !
                ! 08.20.15 added these variables
                ! 10.11.15 das
                ! NOT Yet USED
                !lvf_indianPriorityAF= vDb%lvf_indianII(T%year)+ vDb%lvf_indianIII(T%year)
                lvf_AgPool=max(0,lvf_capNIA-lpf_NIA_Priority)
                !if(0 < lvf_reduceAgMax)lvf_reduceAg=min(1,lvf_AgPool/lvf_reduceAgMax)
                !
                gvi_Agpool_acft_a=NINT(lvf_AgPool)

10 format(I4,1x,2(F9.1,1x),F11.1)
                ! ---------------------------------------------------------------
                ! NOTE! The following three lines were NOT in the code prior to
                ! 08.25.16  - This was a serious error in the coding; i.e., it was
                ! NOT being re-set annually
                ! ============================
                do i = 1,gvi_Providers,1   
                  gvf_AgCAPpotential_AF(i)= 0
                  gvi_agCAPcreditsTransfered(i)=0
                end do
                !
                call sMaskCAP(gvl_maskCAP)
                !
                do i = 1,gvi_Providers,1
                    call sAgSurfaceWaterOther(T,i,lvf_addSurfWaterFromAgCAP)
                     gvf_AgCAPpotential_AF(i)= lvf_addSurfWaterFromAgCAP(i)
                     gvi_agCAPcreditsTransfered(i)=nint(lvf_addSurfWaterFromAgCAP(i))
                end do
                ! ---------------------------------------------------------------
                !

                ! ==================================================================================
                do i = 1,gvi_Providers,1
                 lvf_desired_IV(i)=0
                 lvf_desired_V(i)=0
                 lvf_desired_VI(i)=0
                 lvf_maxP4(i)=0
                 lvf_maxP5(i)=0
                 lvf_maxP6(i)=0
                 lvf_potentialWaterBank(i)=0
                 lvf_available_IV(i)=0
                 lvf_demandMinusAv(i)=0
                 lvf_available_V(i)=0
                 lvf_usedP4(i)=0
                 lvf_unusedP4(i)=0
                 lvf_usedP5(i)=0
                 lvf_unusedP5(i)=0
                end do
                !
                 ! Total designations, M&I contracts (array 1) and those purchased (array 2)
                 lv_sumdesignationsTierII=0
                 lv_sumdesignationsTierIV=0
                 lvf_addDesignation_IV=0
                 lvf_addDesignation_V=0
                 lvf_addAg=0
                 ! =======================================================================================
                 !
                do i = 1,gvi_Providers
                  lv_sumdesignationsTierII=lv_sumdesignationsTierII+vDb%lvf_Designation_IV_B_acft(T%year,i)
                  lv_sumdesignationsTierIV=lv_sumdesignationsTierIV+vDb%lvf_Designation_V_B_acft(T%year,i)
                end do
                    !
                    !
                    ! Priority four and five , here is CAP tier II and tier IV
                    ! Do not here be confused with the tier versus the priority nomenclature
                    ! Checked on 08.12.15 (comes from CAPdesignations.txt).
                    ! 08.12.15 das
                    ! --------------------------------------------
                    lvf_addDesignation_IV=lv_sumdesignationsTierII
                    lvf_addDesignation_V=lv_sumdesignationsTierIV
                    lvf_addAg=sum(gvi_agCAPcreditsTransfered)
                    ! --------------------------------------------
                    !
                 do i = 1,gvi_Providers
                    !
                    lvf_ratioDesignation_IV(i)=0
                    lvf_ratioAgAdditions(i)=0
                    lvf_ratioTotalDesig_IV(i)=0
                    lvf_ratioTotalDesig_V(i)=0
                    !
                    add=0
                   add=vDb%lvf_Designation_IV_B_acft(T%year,i)+vDb%lvf_Designation_V_B_acft(T%year,i) 
                  !
                  if(0 < add)lvf_ratioDesignation_IV(i)=vDb%lvf_Designation_IV_B_acft(T%year,i)*(1/add )
                  if(0 < lvf_addAg)lvf_ratioAgAdditions(i)=gvi_agCAPcreditsTransfered(i)* (1/lvf_addAg)
                  if(0 < lvf_addDesignation_IV)lvf_ratioTotalDesig_IV(i)=vDb%lvf_Designation_IV_B_acft(T%year,i)*(1/lvf_addDesignation_IV)
                  if(0 < lvf_addDesignation_V)lvf_ratioTotalDesig_V(i)=vDb%lvf_Designation_V_B_acft(T%year,i)*(1/lvf_addDesignation_V)
                  !
                 end do
                !

            ! Changed from zero to one on 02.19.15 DAS
!            vReductionII=1.
!            vReductionIV=1.
            !
            ! NEW CODE
            ! as of 03.10.15 DAS
            ! ---------------------
            call sProviders(provid)
            !
            call CAPneeded(gvi_order,lvf_CAP_needed)
            !
                lvf_capPfive=0
                lvf_capPsix=0
            !
            lvf_capPsix=max(0,lvf_AgPool)
            !
            lvf_capPfive=max(0,lvf_capNIA-lvf_capPsix)
            !
            do j = 1,gvi_Providers,1
                !
            !    lvf_creditsSIX=0; lvf_totalSIX=0
                go_AgWaterCAPavailable_AF(j)=0
                gvf_AgCAPused_AF(j)=0
                !
                lvf_maxP4(j)=vDb%lvf_Designation_IV_B_acft(T%year,j)
                lvf_maxP5(j)=vDb%lvf_Designation_V_B_acft(T%year,j)
                lvf_maxP6(j)=gvf_AgCAPpotential_AF(j)
                !
                ! NEW on 08.13.15
                ! new on 01.22.17
                if(0 < lvf_maxP6(j))then
                  lvf_desired_IV(j)= min(lvf_maxP4(j),lvf_CAP_needed(j) * lvf_ratioDesignation_IV(j))
                  lvf_desired_V(j) = min(lvf_maxP5(j),lvf_CAP_needed(j) * (1-lvf_ratioDesignation_IV(j)))
                  if(lvf_CAP_needed(j) > (lvf_maxP4(j)+lvf_maxP5(j)))then
                    lvf_desired_VI(j)=min(lvf_maxP6(j),lvf_CAP_needed(j)-(lvf_maxP4(j)+lvf_maxP5(j)))
                  endif
                else

                lvf_desired_IV(j)= min(lvf_maxP4(j),lvf_CAP_needed(j) * lvf_ratioDesignation_IV(j))
                lvf_desired_V(j) = min(lvf_maxP5(j),lvf_CAP_needed(j) * (1-lvf_ratioDesignation_IV(j)))
                lvf_desired_VI(j)=0
                endif
                !
!                    lvf_capSIX=lvf_capPsix
!                    lvf_creditsSIX=gvi_agCAPcreditsTransfered(j)
!                    lvf_totalSIX=sum(gvi_agCAPcreditsTransfered)
!                    lvf_ratioSIX=lvf_creditsSIX/lvf_totalSIX
                    !
                    call AgAcerageAndCreditsCAP(T,lvf_relArea)
                    !


                    ! Estimate of CAP water used by Ag
                    ! 09.02.16
                    go_AgWaterCAPavailable_AF(j)=nint(lvf_AgPool*lvf_relArea(j))
                    !
                    ! 09.02.16
                    ! -----------------------------------------------------
                    ! This could be wrong, but I am assuming that ADWR had
                    ! a good estimate of CAP use by Ag in 2010. The model
                    ! simulates much higher availability. I use the minimum
                    ! ========
                    lvi_AgPool=nint(lvf_AgPool)
!                    if(0 < lvf_AgPool)gvf_ratioMaricopaAg=gv_AgWaterPumpingSRV(T%year,5)/lvf_AgPool
!                    lvf_AgPool=min(lvf_AgPool*(max(0.13,gvf_ratioMaricopaAg)),gv_AgWaterPumpingSRV(T%year,5))      
                    lvf_AgPool=min(lvf_capPsix*(max(0.13,gvf_ratioMaricopaAg)),gv_AgWaterPumpingSRV(T%year,5))
   
                    !
                    lvf_usedP6(j)=0
                if(0 < gvi_agCAPcreditsTransfered(j))then
                  ! 01.22.17
                  ! ------------------------------
                  if(lvf_desired_VI(j) > 0)then
                    !lvf_usedP6(j)=(min(lvf_desired_VI(j)*lvf_reduceAg,lvf_capPsix*lvf_ratioAgAdditions(j)))

                    lvf_usedP6(j)=(min(lvf_desired_VI(j),lvf_AgPool*lvf_ratioAgAdditions(j)))
                    lvf_capPsix=max(0,lvf_capPsix-lvf_usedP6(j))
                  endif
                    !
!                    lvf_capSIX=lvf_capPsix
!                    lvf_creditsSIX=gvi_agCAPcreditsTransfered(j)
!                    lvf_totalSIX=sum(gvi_agCAPcreditsTransfered)
!                    lvf_ratioSIX=lvf_creditsSIX/lvf_totalSIX
                    !
                    if(0 < lvf_capPsix)then
                    !
                    ! Redefined on 08.31.16 to be CAP water used by AG - (wrong)
                    ! NOTE: gvf_AgCAPused_AF(j) is Credits transfered to MUNI NOT CAP
                    ! water used by Ag
                    ! ==============================================
                    !
                    gvf_AgCAPused_AF(j)= lvf_usedP6(j)
                    !
                    !
                    ! das1
                    else
                      if(sum(lvf_desired_V)< lpf_NIA_Priority)then
                      endif        
                    endif

100 format(I4,1x,I2,1x,4(F12.4,1x),I6)

!1000          format(I4,1x,3(F10.2,1x)) !format(I4,1x,I2,1x,3(F(10.4,1x)))

                endif
                !
             end do
            !
!1003          format(I4,1x,I2,1x,4(F12.3,1x)) !format(I4,1x,I2,1x,3(F(10.4,1x)))
            do i = 1,gvi_Providers,1
                go_lossPotentialP4(i)=0
                go_lossPotentialP5(i)=0
                lvf_potentialWaterBank(i)=0
                !
                ! New on 08.12.15 das
                lvf_available_IV(i)=lv_capMandI*lvf_ratioTotalDesig_IV(i)
                !
                if(lvf_desired_IV(i) <= lvf_maxP4(i))then
                  lvf_demandMinusAv(i)= lvf_desired_IV(i)-lvf_available_IV(i)
                  !
                  if(0 < lvf_demandMinusAv(i))then
                    go_lossPotentialP4(i)=nint(lvf_desired_IV(i)-lvf_available_IV(i))
                    !
                    lvf_usedP4(i)=lvf_available_IV(i)
                  else
                    lvf_potentialWaterBank(i)=lvf_available_IV(i)-lvf_desired_IV(i)
                    !
                    lvf_usedP4(i)=lvf_desired_IV(i)
                    lvf_unusedP4(i)= lvf_potentialWaterBank(i)
                  endif
                else
                ! Never here
                endif
                !
                go_waterBankCAP4(i)=lvf_potentialWaterBank(i)
                !
                lvf_demandMinusAv(i)=0
                lvf_potentialWaterBank(i)=0
               ! lvf_reduceFive=0
                !
                if(lvf_capPfive <=lvf_addDesignation_V)then
                    lvf_available_V(i)=lvf_capPfive*lvf_ratioTotalDesig_V(i)
                    !go_rightsLostP5(i)=nint(lvf_maxP5(i)-lvf_available_V(i))
                    !
                    if(lvf_desired_V(i) <= lvf_maxP5(i))then
                      lvf_demandMinusAv(i)= lvf_desired_V(i)-lvf_available_V(i)
                      !
                      if(0 < lvf_demandMinusAv(i))then
                        go_lossPotentialP5(i)=nint(lvf_desired_V(i)-lvf_available_V(i))
                        !
                        lvf_usedP5(i)=lvf_available_V(i)
                      else
                        lvf_potentialWaterBank(i)= lvf_potentialWaterBank(i) &
                           + (lvf_available_V(i)-lvf_desired_V(i))
                        !
                        lvf_usedP5(i)=lvf_desired_V(i)
                        lvf_unusedP5(i)= lvf_potentialWaterBank(i)
                      endif
                    else
                        ! Never here
                    endif
                else
                    if(0 < lvf_addDesignation_V)then
                      if(lvf_capPfive <=lvf_addDesignation_V)then
                      endif
                    endif
                    !
                    lvf_available_V(i)=lvf_capPfive*lvf_ratioTotalDesig_V(i)     
                    !
                    if(lvf_desired_V(i) <= lvf_maxP5(i))then
                      lvf_demandMinusAv(i)= lvf_desired_V(i)-lvf_available_V(i)
                      !
                      if(0 < lvf_demandMinusAv(i))then
                        go_lossPotentialP5(i)=nint(lvf_desired_V(i)-lvf_available_V(i))
                        !
                        lvf_usedP5(i)=lvf_available_V(i)
                      else
                        lvf_potentialWaterBank(i)= lvf_potentialWaterBank(i) &
                           + (lvf_available_V(i)-lvf_desired_V(i))
                        !
                        lvf_usedP5(i)=lvf_desired_V(i)
                        lvf_unusedP5(i)= lvf_potentialWaterBank(i)
                      endif
                    else
                        ! Never here
                    endif

                endif
                !
                go_waterBankCAP5(i)=nint(lvf_potentialWaterBank(i))
                !
             ! ------------------------------------------------------------------
            end do
            !
         
            !
            do i = 1,gvi_Providers,1
             vDb%mvf_unUsedP4(i)=lvf_unusedP4(i)
             vDb%mvf_unUsedP5(i)=lvf_unusedP5(i)
            end do
            do i = 1,gvi_Providers,1
                vDb%lv_realizedDesignation_B_acft(T%year,i)=lvf_usedP4(i)+lvf_usedP5(i)+lvf_usedP6(i)
            end do
            !
             lvf_availableP4=0
             lvf_availableP5=0
            lvf_availableP4=sum(lvf_unusedP4)
            lvf_availableP5=sum(lvf_unusedP5)
            !
            ! ------------------------------------------------------------------
            ! We do not model all of the priority four contracts (02.27.15)
            !
            vDb%lvf_unUsedP4=lvf_availableP4
            vDb%lvf_unUsedP5=lvf_availableP5
            !
         return
        end subroutine reductionsInDesignations
        ! -------------------------------------

    ! -------------------------------------
    subroutine CAPout(T,vDb,gvi_order,lv_capMandI,lvf_capNIA)
        !
        ! ------------ Types -------------
        integer:: i,j
        integer(1):: gvi_order

        real:: lvf_Demand(gvi_maxProV)
        real:: lvf_CAP_needed(gvi_maxProV)
        real:: lvf_addCAP
        real :: lv_capMandI,lvf_capNIA
        ! ================================
        !

        ! --- Type Constructs --
        type(Designations_B)vDb
        type(runTime)T
        ! ======================

         ! New Code 04.27.11,03.10.15
         ! ======================================
         ! Create a variable to handle added CAP purchased from Tier III and IV supplies
         ! Add to WaterSimDCDC.txt (I suppose) Think about reducing that file.....
         ! Would ultimately be reflected in " gvf_Used_CO_acft(j)"  - use type convert?
         ! 12.14.11 DAS
             if(gpl_DemandDriven)then
              !
              do j = 1,gvi_Providers,1
                gvf_Used_CO_acft(j)=0
                gvf_Unused_CO_acft(j)=0
!                gvf_UnmetDemand_acft(j)=0
                gvf_UnmetDemand_CO(j)=0
                gvf_SWtoWBCAPratioP4(j)=0
                ! Set water demand variable
                lvf_Demand(j)=(gvf_WaterDemand_acft(j,gvi_order,1))
                !
                if(0 < lvf_Demand(j))then
                   lvf_CAP_needed(j)=lvf_Demand(j)
                else
                  lvf_CAP_needed(j)=0
                endif
                !
                If(0 < lvf_CAP_needed(j))then
                  ! Is there CAP water available? 
                  if(0 < vDb%lv_realizedDesignation_B_acft(T%year,j))then
                    !
                    gvf_Used_CO_acft(j)=vDb%lv_realizedDesignation_B_acft(T%year,j)
                    gvf_Unused_CO_acft(j)=vDb%mvf_unUsedP4(j)+vDb%mvf_unUsedP5(j)
!                    gvf_UnmetDemand_acft(j)=nint(lvf_CAP_needed(j)-vDb%lv_realizedDesignation_B_acft(T%year,j))
!                    gvf_UnmetDemand_CO(j)=gvf_UnmetDemand_acft(j)
                   !
                  else
                   ! No CAP water available
                   gvf_Used_CO_acft(j)=0
                   !
                  endif
                endif
              end do
                !
                ! 06.08.12 DAS- Moved below the added credits if block on 02.05.15
                ! ---------------------------------------------------------------
                 lvf_addCAP=0
                do i = 1,gvi_Providers,1
                 lvf_addCAP=lvf_addCAP+vDb%lv_realizedDesignation_B_acft(T%year,i)
                end do
                ! 02.18.15 DAS I subtract Ag water from designations to get CO deliveries here
                !
                go_deliveriesCO=nint(lvf_addCAP)
                !
                vDB%lvf_CAPdifference(T%year)=lv_capMandI+lvf_capNIA-lvf_addCAP
             endif

         return
        end subroutine CAPout
        ! -------------------------------------

        ! -------------------------------
        subroutine waterBank(vDb)
            integer:: i
            type(Designations_B)vDb
            !  10.11.13 das
            ! -------------------------
            ! 02.19.15 DAS
            ! 10.10.15 das
            do i = 1,gvi_Providers,1
               gvf_parm_SWtoWBamtCAP(i)=0
               gvf_SWtoWBCAPratioP4(i)=0
              gvf_parm_SWtoWBamtCAP(i)=(vDb%mvf_unUsedP4(i)+vDb%mvf_unUsedP5(i))
              !
                if(0 < (vDb%mvf_unUsedP4(i)+vDb%mvf_unUsedP5(i)))then
                  gvf_SWtoWBCAPratioP4(i)=vDb%mvf_unUsedP4(i) / (vDb%mvf_unUsedP4(i)+vDb%mvf_unUsedP5(i))
                endif
              !
            end do
            !
          return
        end subroutine waterBank
        ! -------------------------------


        ! ------------------------------
        subroutine CAPpriorityWater(T,vDb,lv_capMandI,lvf_capNIA)
            !
            ! --------------- Types -----------------
            real:: lv_capMandI
            real:: lvf_capNIA
            ! =======================================
            !
           ! --- Type Constructs --
            type(Designations_B)vDb
            type(runTime)T
            ! ======================
                ! Tiers I and II
                lv_capMandI=0
               lv_capMandI=vDb%lv_CAPMandI_II(T%year)
               !
               ! Tiers III and IV
                ! 02.27.15 DAS changed to NIA from tiers III and tiers IV
                    ! 03.09.15 based on CAP pptx (CAP_DCDCPresentation dated 25 Feb, 2015)
                    !lmf_NIA
                    !lmf_AgPool
                    !lmf_Other=
                lvf_capNIA=0
               lvf_capNIA= lmf_NIA+lmf_AgPool+lmf_Other
           !
          return
        end subroutine CAPpriorityWater
        ! ------------------------------

        ! ----------------------------------------------
        subroutine deltaCAPwater(lv_capMandI,lvf_capNIA)
            ! 
            ! --------- Types ------------
            real:: lvf_capNIA,lv_capMandI
            real:: lvf_NIAdifference
            ! ============================           !
                !
            ! 03.02.15 DAS
            ! Take delta water from priority 5 first. If not sufficient,
            ! take remaining from priority four.
            ! ----------------------------------------------------------
              lvf_NIAdifference=0
            if(go_COdeltaWater_az < lvf_capNIA)then
              lvf_capNIA=max(0,lvf_capNIA-go_COdeltaWater_az)
            else
              lvf_NIAdifference=go_COdeltaWater_az-lvf_capNIA
              lvf_capNIA=0
            endif
            !
            lv_capMandI= max(0,lv_capMandI-lvf_NIAdifference)
            !
          return
        end  subroutine deltaCAPwater
        ! ---------------------------

        ! ------------------------------
        subroutine COriverDeltaWater(lv_capMandI,lvf_capNIA)
            !
            ! ------------------ Types ------------------
            integer:: i
              ! inputs
            real:: lv_capMandI,lvf_capNIA

              ! created within
            real:: lvf_ratioCOdeltaBurden,lvf_propDropInBurden
            real:: lvf_propDropInBurdenNIA,lvf_propDropInBurdenMandI
            real, parameter:: lpf_ThresholdMandI=638823
            real, parameter:: lpf_ThresholdNIA=215000
            real, parameter:: lpf_COdeltaBurdenAmt=158088
            real:: lvf_propSharingCOdelta,AZshare=2.8
            real:: lvf_totalCOallocatedLB=9.0        
            real:: lvf_AZnormalShareDelta
            real:: lvf_VinzeJohnstonAvg,countVJ

            real:: lvf_deltaMandIprovider(gvi_maxProV) !,lvf_capNIAprovider(gvi_maxProV)

            ! ===========================================
            !

               !   01.20.15 DAS new "environmental flows" takes water from the CAP water
               ! using the parameter gvi_WaterToCOdelta_acft_a, found in KernelInterface.f90
               ! in subroutine setWaterToCOdelta(value). There are two states to this. When AZ
               ! carries the entire Burden (gvl_AZdeltaBurden = true), then the amount comes out 
               ! of M&I, but proportionalized by what the delivered M&I is relative to "max"(normal).
               !  When the Burden is split, then the amount "charged" to AZ is proportional to the AZ share of CO river water 
               ! with the remaining CO delta water coming from nv,mx, and ca.
               !
                do i = 1,gvi_Providers,1
                  lvf_deltaMandIprovider(i)=0
!                  lvf_capNIAprovider(i)=0
                end do
                countVJ=0
                 lvf_ratioCOdeltaBurden=0
                 lvf_propDropInBurdenNIA=1
                lvf_propDropInBurdenNIA=min(1,lvf_capNIA/lpf_ThresholdNIA)

                 lvf_propDropInBurdenMandI=1
                lvf_propDropInBurdenMandI=lv_capMandI/lpf_ThresholdMandI

                 lvf_propDropInBurden=1
                lvf_propDropInBurden=lvf_propDropInBurdenNIA*lvf_propDropInBurdenMandI
                !
                 lvf_propSharingCOdelta=0
                lvf_propSharingCOdelta=(AZshare*(1/gpd_acftTomaf))/(lvf_totalCOallocatedLB*(1/gpd_acftTomaf) + gvf_upperBasinCUse_acft_a)
                 lvf_AZnormalShareDelta=0
                lvf_AZnormalShareDelta=lvf_propSharingCOdelta*lpf_COdeltaBurdenAmt
                !
                ! NOTE: the famous "E" equation estimates more M&I than accounted for (the 33 total)
                ! by 18145 AF (non-ag and non indian).
                ! NOTE:  the 18145 is priority three given to M&I 02.27.15 DAS

                if(gvl_modelVinzeJohnston)then
                    lvf_VinzeJohnstonAvg=0
                    !
                    do i = 1,gvi_Providers,1
                      lvf_deltaMandIprovider(i)=0
                     if(0 < gvi_ProviderFlowToCOdelta_AF(i))then
                       countVJ=countVJ+1
                        gvi_WaterToCOdelta_acft_a=0
                       gvi_WaterToCOdelta_acft_a=gvi_ProviderFlowToCOdelta_AF(i)
                       if(0 < countVJ)lvf_VinzeJohnstonAvg=(lvf_VinzeJohnstonAvg+gvi_ProviderFlowToCOdelta_AF(i))/countVJ
                     endif     
                        ! 
                        lvf_deltaMandIprovider(i)=gvi_ProviderFlowToCOdelta_AF(i)       
                        !
                    end do
                    !
                  lvf_ratioCOdeltaBurden=(lvf_propDropInBurden*lvf_AZnormalShareDelta)/lvf_AZnormalShareDelta
!                  go_COdeltaWater_az=lvf_propDropInBurden*gvi_WaterToCOdelta_acft_a
                    do i = 1,gvi_Providers,1
                     go_COdeltaRatioProvider_az(i)=nint(100*(lvf_deltaMandIprovider(i)/lvf_AZnormalShareDelta))
                    end do
                  !
                  go_COdeltaWater_az=lvf_propDropInBurden*lvf_VinzeJohnstonAvg
                  !
                else
                    if(gvl_AZdeltaBurden)then
                      !  
                      !lvf_ratioCOdeltaBurden=lvf_propDropInBurden*((gvi_WaterToCOdelta_acft_a*1.0)/lvf_AZnormalShareDelta)     
                      go_COdeltaWater_az=lvf_propDropInBurden*gvi_WaterToCOdelta_acft_a
                      !
                    else
                     ! 
                     if(0 < gvi_WaterToCOdelta_acft_a)then
                       !lvf_ratioCOdeltaBurden=(lvf_propDropInBurden*lvf_AZnormalShareDelta)/lvf_AZnormalShareDelta
                       ! 11.24.15 DAS
                       go_COdeltaWater_az=lvf_propDropInBurden*lvf_propSharingCOdelta*gvi_WaterToCOdelta_acft_a
                     else
                       lvf_ratioCOdeltaBurden=0
                       go_COdeltaWater_az=0
                     endif
                     !
                    endif
                    !
                endif
                    ! Sustainability Indicator; AZ delivered to AZ share of Delta water
                    ! 02.17.15 
                      lvf_ratioCOdeltaBurden=go_COdeltaWater_az/lvf_AZnormalShareDelta
                    if(gvi_WaterToCOdelta_acft_a <=0)lvf_ratioCOdeltaBurden=0
                    !
                    go_ratioCOdeltaBurden=nint(lvf_ratioCOdeltaBurden*100)
                    !
            !
          return
        end subroutine COriverDeltaWater
        ! ------------------------------


       !--------------------------------
       subroutine aTypeConvert(T,vDb,vD)
          ! --- Types ---
          integer :: i
          ! =============

         ! -- Type Constructs --
         type(Designations_B)vDb
         type(Provider)vD
         type(runTime)T
         ! =====================
          !
          do i = 1,gvi_Providers,1
            vD%gRealizedDesignationCAP_acft(T%year,i)=vDb%lv_realizedDesignation_B_acft(T%year,i)
          end do
          !
          vD%gCAPMandI_II(T%year)=vDb%lv_CAPMandI_II(T%year)
          !
        return
       end subroutine aTypeConvert
       !-------------------------------
       !
    ! 
End Module lms_Designations_B
 !
 !  Called from within Kernel.f90
 !   subroutine RunOneYearKernel()
 ! -------------------------------------------------
    subroutine pCAP_water(T,gvi_order,conveyance,vD)
     use lms_Designations_B
       use gm_ModelControl
        !
        ! -------------------------- Types ----------------------------------
        integer :: i
        integer(1) :: gvi_order

        real(8) :: conveyance
        real :: lvf_CAP(gvi_maxProV)
        real :: code=gpi_cap
        ! ====================================================================

        ! --- Type Constructs --
        type(runTime)T
        type(Designations_B)vDb
        type(Provider)vD
        ! ======================
            !
            call sMaskCAP(gvl_maskCAP)
            call aPartition_B(T,conveyance,vDb)
            call aAllocate_B(T,gvi_order,vDb) 
            call aTypeConvert(T,vDb,vD)
            !
            ! Assign demand to Off first- sCWaccounting will track use
            ! ---------------------------------------------------------
            do i = 1,gvi_Providers,1
                lvf_CAP(i)=0
               lvf_CAP(i)=gvf_Used_CO_acft(i)
            end do
            !
!            call sCWaccounting(gvi_order,lvf_off,lvf_on,code)
                gvl_InOutDemandArray=.false.
              call sCWaccountingIndoorOut(gvi_order,code,lvf_CAP,gvl_InOutDemandArray)

            !
            gvi_order=gvi_order+1
            !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=14
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
            !
      return
    end subroutine pCAP_water
    ! -----------------------
!
! ======================================================================================================
! E.O.F. Designations_CO.f90

