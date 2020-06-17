!
! File is WaterShed_SVT.f90
!
! This Infile opens the general files and parameters needed for the combined Salt-Tonto
! and verde rivers. Flow records are opened in other files. These are the global data
! that are common to both riverine systems.
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
! Global OUTPUTS:
!
! Local OUTPUTS:
!   
! Local INPUTS:
!
! Module:       Module lm_River_Aa
! Subroutines:  initWSA()
!                 openFiles_a()
!                 call readParms_a()
!                 call readFiles_a()

! No Module:    subroutine initializeSaltTontoVerde()

! Module:       Module lms_River_Aa
!  Subroutines: subroutine outinitialA(T)
!               subroutine sUnintendedRecharge(T)
! No Module
!  Subroutine(s)
!               subroutine pSaltTontoVerde(T)
!---------------------

! created on 09.30.09
!
! david arthur sampson

! last write was: 10.17.13,07.18.14,07.30.14
! ------------------------------------------
!

! ===============================================================================================
!
Module lm_River_Aa
 use gm_ModelControl
  use gm_GlobalData
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
  contains
        !
        ! ------------------
        subroutine initWSA()
          call openFiles_a()
           if(gvl_errorFlag)then
            call readParms_a()
            call readFiles_a()
            call initialState_a()
           else
           endif
         return
        end subroutine initWSA
        ! --------------------

        ! ----------------------
        subroutine openFiles_a()
            !
            ! -------------- Types -------------
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
                   Infile='App_Data\Data\Initial_storage.txt'; LU=17
                   call openFiles(module,lvc_DPath,Infile,LU)
                  !
                   Infile='App_Data\Parameters\parm_SVT.dat'; LU=31
                   call openFiles(module,lvc_DPath,Infile,LU)
                  !
                  gvl_errorFlag = .true.
                  !
                LU=0
                return
    10         continue
                !
                if(gvl_writeLog)then
                  string=34
                    call sStrings(string,errorString)
                    call eWrite(errorString,LU)
                endif
              gvl_errorFlag=.false.
          !
        end subroutine openFiles_a
       ! -------------------------

        ! ----------------------
        subroutine readFiles_a()
          use gm_GlobalData
            !
            ! --------- Types ---------
            integer :: i,ios,iyear
            real(8) :: B(2000:2010)
            real(8) :: C(2000:2010)
            ! =========================
                !
                ! -----------------------------------------
                !
                ios=0             
                LU=0
                !
                !-----------------------------------------------------
                !
                LU=17
                do i = 2000,2010,1
                 li_initstorageSVT(i)=0
                 read(LU,*,err=7,iostat=ios)iyear,li_initstorageSVT(i),B(i),C(i)
                 iyear = iyear*1 ! place holders 
                 B(i)=B(i)*1
                 C(i)=C(i)*1
                end do
                rewind(17)
!                 lv_firstYear=2000
    7           continue
                close(LU)
                if(ios >0)then
                 goto 1000
                endif
           return
1000 continue
           if(gvl_writeLog)then
                string=8
                call sStrings(string,errorString)
                call eWrite(errorString,LU)
            endif
           gvl_errorFlag=.false.
           !
         end subroutine readFiles_a
        ! -------------------------

        ! ----------------------
        subroutine readParms_a()
            !
            ! ---------------- Types -----------------------
            integer :: li_numdroughtyearsSVT

            real(8) :: vStorageCapacityV,vStorageCapacityST
            ! ==============================================
                !
!                read(31,*)gv_indexyearSVT
!                read(31,*)gv_droughtyearSVT
!                read(31,*)gv_climatefactorSVT
!                read(31,*)li_numdroughtyearsSVT
!                read(31,*)gv_droughtfactorSVT
!                read(31,*)li_normalflowacftperacre
!                read(31,*)li_horsemesamax
!                read(31,*)li_mormanflatmax
!                read(31,*)li_rooseveltmax
!                read(31,*)li_stewartmountainmax
!                read(31,*)li_bartlettmax
!                read(31,*)li_horseshoemax
!                read(31,*)li_deadpoolSVT
                gv_indexyearSVT=1979
                gv_droughtyearSVT=2000
                gv_climatefactorSVT=1
                li_numdroughtyearsSVT=25
                gv_droughtfactorSVT=1
                li_normalflowacftperacre=3
                li_horsemesamax=0.245138
                li_mormanflatmax=0.057852
                li_rooseveltmax=1.653043
                li_stewartmountainmax=0.069765
                li_bartlettmax=0.178186
                li_horseshoemax=0.109217
                li_deadpoolSVT=0.181492
                !
                close(31)
                 gv_droughtyearendSVT=0
                gv_droughtyearendSVT=gv_droughtyearSVT+li_numdroughtyearsSVT
                !
                 vStorageCapacityST=0
                vStorageCapacityST=li_horsemesamax+li_mormanflatmax+li_rooseveltmax+ &
                    li_stewartmountainmax
                 vStorageCapacityV=0
                vStorageCapacityV=li_bartlettmax+li_horseshoemax
                ! 0.98 is calibrated from monthly storage analysis (March 2013)
                 gvd_storageCapacityVerde=0
                gvd_storageCapacityVerde=vStorageCapacityV*0.98
                 gvd_storageCapacitySVT=0
                gvd_storageCapacitySVT=(vStorageCapacityST+vStorageCapacityV)
                !
           return
        end subroutine readParms_a
       ! -------------------------

        ! -------------------------
        subroutine initialState_a()
            !
            ! ------------------- Types -------------------
            real(8) :: vStorageCapacityV,vStorageCapacityST
            real(8) :: vStorageCapacityO
            real :: lvf_Init=0.9
            ! =============================================
            ! 
            vStorageCapacityST=li_horsemesamax+li_mormanflatmax+li_rooseveltmax+ &
                li_stewartmountainmax
            vStorageCapacityV=li_bartlettmax+li_horseshoemax
            vStorageCapacityO=vStorageCapacityST-li_rooseveltmax
            !
            lv_State_A_maf(2000)=li_initstorageSVT(2000)*gvd_storageCapacitySVT
            lv_State_A_maf(2001)=li_initstorageSVT(2001)*gvd_storageCapacitySVT
            lv_State_A_maf(2006)=li_initstorageSVT(2006)*gvd_storageCapacitySVT
            !
            lvd_State_Verde_acft(2000)=li_initstorageSVT(2000)*(vStorageCapacityV*1e6)
            lvd_State_Salt_acft(2000)=li_initstorageSVT(2000)*(vStorageCapacityST*1e6)
            lvd_State_Others_acft(2000)=li_initstorageSVT(2000)* (vStorageCapacityO*1e6)
            lvd_State_Roosevelt_acft(2000)=lvf_Init*(li_rooseveltmax*1e6)
            !
            gv_dataSVT=1
            go_StateSVT=nint(lv_State_A_maf(2000)*1e6)
            go_deliveriesSVT=nint(0.7923488*1e6)
            ! 
          return
        end subroutine initialState_a
        ! ---------------------------
!
End Module lm_River_Aa
!
    ! ----------------------------------
    subroutine initializeSaltTontoVerde()
        use  lm_River_Aa
        !
        call initWSA()
        !
      return
    end subroutine initializeSaltTontoVerde
    ! -------------------------------------
!
! ======================================================================================================
!
Module lms_River_Aa
 use gm_ModelControl
  use gm_GlobalData
   use gm_TypeControl
    use gm_Euler
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !

    !    type(Operations) :: ltSVT
    !
   contains
    ! -----------------------
    subroutine outinitialA(T)
        !
        ! -- Type Constructs -
        type(runTime)T
        ! =========================
        !
        !
      return
    end subroutine outinitialA
    ! ------------------------

    ! ---------------------------------
     subroutine sUnintendedRecharge(T)
        !
        ! ------------------------- Types ---------------------------------------
        !real :: flow_A   ! acft
        real :: lv_artificialLake,lv_majordrainage
        real :: lv_leakageSRP,lv_leakageCAP,lv_mountainfront,lv_ephemeralstreams
        real,parameter :: lvf_otherCanal=50000
!        real, parameter :: lpf_mMountainFront=0.023742302
!        real, parameter :: lpf_mEphemeral=0.061122779
        ! ============================================================================
        !

        ! -- Type Constructs --
        type(runTime)T
        ! =====================
        !
            ! Million acre-feet year-1
            ! ------------------------
!             flow_A=0
!            if(0 < lv_maf_yr(T%year))then
!             flow_A=lv_maf_yr(T%year)
!            endif
            !
            lv_artificialLake=  13000            ! acft
            lv_majordrainage=   0  
            lv_leakageSRP=      34200                ! AF
            lv_leakageCAP=      16700                ! AF
            !
            ! 07.30.14 DAS
            ! Equation(s) are in units k AF; must return result as AF
            ! Median flow Salt=518.499 k AF a-1: Verde=294.733 (Tango Creek) k AF a-1
            ! Slope coefficients (y2-y1) = m(x2-x1) [y1=0; x1=0]
            ! NOT NOW USED!
            ! ------------------------------------------------------
             lv_mountainfront=0
            lv_mountainfront= 19308!  lpf_mMountainFront*(flow_A * 1000)*1000   ! k AF [based on median flow and 19,308 AF yr-1 used by ADWR modeling]
             lv_ephemeralstreams=0
            lv_ephemeralstreams= 49707 !lpf_mEphemeral*(flow_A * 1000)*1000  ! k AF [based on median flow and 49,707 AF yr-1 used by ADWR modeling]
            !

            ! This is used in  subroutine outputs(T,lvWB) found in Water_CityModel.f90
            ! Write to the common block
            ! --------------------------
             gvf_naturalRecharge_acft(T%year)=0
            gvf_naturalRecharge_acft(T%year)=anint(lv_artificialLake+lv_majordrainage & 
            + lv_leakageSRP+lv_leakageCAP+lv_mountainfront+ lv_ephemeralstreams+lvf_otherCanal)
            !
            ! --------------------------------------------
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=3
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
            !
        !
      return
    end subroutine sUnintendedRecharge 
    ! --------------------------------
!
End Module lms_River_Aa
! 
    ! ------------------------------
    subroutine pSaltTontoVerde(T)
     use lms_River_Aa
        !
        !- TYPE constructs -
         type(runTime)T
        ! ==================
            !
            call outinitialA(T)
             call sFlowsSaltTontoVerde(T)                       
            call sUnintendedRecharge(T)
            !
      return
    end subroutine pSaltTontoVerde
    ! -----------------------------
!
! ======================================================================================================
! E.O.F. WaterShed_STV.f90
