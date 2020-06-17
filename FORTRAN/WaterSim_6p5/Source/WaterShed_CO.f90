!   
!  File is WaterShed_CO.f90
!
! This file simulates the Colorado River System
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
! Module:       Module lm_River_ba
! Subroutines:  subroutine initWSB() 
!                 calls (also present):
!                   call openFiles_b()
!                   call readParms_b()
!                   call readFiles_b()
!                   call initialState_b()
!
! No Module:    subroutine initializeColorado()
!
! Module:       Module lms_River_Ba
! Subroutines:  subroutine aFlow_b(T,acft)
!               subroutine aModifyFlow_ba(T)
!               subroutine aModifyFlow_bb(T)
!               subroutine aModifyFlow_bc(T)
!               subroutine aModifyFlow_bd(T)
!               subroutine reservoirs_b(T,wsb)
!               subroutine expectedStorage_ba(T,wsb)
!               subroutine initialStorage_B(T)
!               subroutine expectedStorage_bb(T,wsb)
!               function fBankStoragePowell(lvf_diffStorage)
!               function fBankStorageMead(lvf_diffStorage)
                ! Lake Powell
!               subroutine aModifyStorage_ba(T,wsb)
!               subroutine actualRelease(T,wsb)
!               subroutine overrideRelease(T,wsb)
!               subroutine adwrRelease(T,wsb)
!               subroutine actualStorage(T,lvd_Pp,lvd_Pm)
!               subroutine threshold_b(T,lvd_Pp,lvd_Pm,lvd_balanceamount,lv_Ssr,vTier)
!               subroutine targetAmount(T,lvd_balanceamount,lv_Ssr)
!               subroutine equalizationTier(T,lvf_powell_elevation,lvf_useElevation,lvd_balanceamount,lv_Ssr)
!               subroutine upperElevationTier(T,lvd_balanceamount,lv_Ssr)
!               subroutine targetElevations(T,lvf_useElevation)
!               subroutine balanceContent(T,lvf_Min,lvf_Max,lvd_balance,release)
!               subroutine elevationsPowell(state,lvf_powell_elev)
!               subroutine stateFromElevationPowell(state,lvf_powell_elev)
                ! Lake Mead
!               subroutine elevationsMead(state,lvf_mead_elev)
!               subroutine aModifyStorage_bb(T,wsb) 
!               subroutine sevenstates(T,lvf_meadElevation,lv_PMead,lv_SevenStatesLCshortage)
!               subroutine designations_B(T,wsb)
!               subroutine updateState_b(T,Bout,wsb)
!               subroutine outinitialB(T) 
!               subroutine outputsB(T)
!               end module
!               subroutine pFlowsReservoirsColorado(T,acft,Bout)

! Global OUTPUTS:         
!                     
! Local OUTPUTS:
!            
! Local INPUTS        
!              
!
! created on 10.01.09
!
! david arthur sampson

! last write was: 05.10.13,07.21.14
! ---------------------------------
!

! ======================================================================================================
!
Module lm_River_ba
 use gm_ModelControl
  use gm_GlobalData
    use gm_Euler
    ! 
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
 contains
        ! ------------------
        subroutine initWSB()
          call openFiles_b()
          call readParms_b()
          call readFiles_b()
          call initialState_b()
         return
        end subroutine initWSB
        ! --------------------

        ! ----------------------
        subroutine openFiles_b()
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
                module="lm_River_ba"
               !
               Infile='App_Data\Data\CORiver_BofRbase.txt'; LU=10
!               Infile='App_Data\Data\USGS_CObase.txt'; LU=10

               call openFiles(module,lvc_DPath,Infile,LU)
    !           call readFiles_st(module,Infile,LU)
              !
               Infile='App_Data\Data\CORiver_BofR.txt'; LU=11
               call openFiles(module,lvc_DPath,Infile,LU)

               Infile='App_Data\Data\CORiver_paleo.txt'; LU=12
               call openFiles(module,lvc_DPath,Infile,LU)

               Infile='App_Data\Data\CORiver_scenario.txt'; LU=13
               call openFiles(module,lvc_DPath,Infile,LU)

               Infile='App_Data\Data\COinFlowPowellToMead_year.txt'; LU=14
!               Infile='App_Data\Data\COinFlowPowellToMead.txt'; LU=14
               call openFiles(module,lvc_DPath,Infile,LU)

               Infile='App_Data\Data\UpperBasin.txt'; LU=15
               call openFiles(module,lvc_DPath,Infile,LU)

               Infile='App_Data\Data\powellequalizations.txt'; LU=16
               call openFiles(module,lvc_DPath,Infile,LU)

               Infile='App_Data\Data\Initial_storage.txt'; LU=17
               call openFiles(module,lvc_DPath,Infile,LU)

               Infile='App_Data\Parameters\parm_CO.dat'; LU=30
               call openFiles(module,lvc_DPath,Infile,LU)
               !
        return
10        continue

                if(gvl_writeLog)then
                    string=24
                    call sStrings(string,errorString)
                    call eWrite(errorString,LU)
                endif
            gvl_errorFlag=.false.
            !
            close(103)
        end subroutine openFiles_b
       ! -------------------------

        ! ----------------------
        subroutine readFiles_b()
            !
            ! --------------- Types ------------------
            integer :: i,j,iyear
            integer :: rec,ios
            integer, parameter :: lpi_CObaseRecords=13
            real :: A(2000:2010),temp
            ! ========================================
                !
                ios=0
                rec=0
                LU=0
                ! Prior to 02.08.13
                ! Read Unit 10; Colorado River base data 2000 to 2010 data

                ! 03.04.13 DAS
                ! Read Unit 10; Colorado River base data 2000 through 2012 data

                !-----------------------------------------------------
                !
                LU=10
                do rec=1,lpi_CObaseRecords,1
                 lvd_iflowCObase_acft(rec)=0
                  read(LU,*,err=1)temp,lvd_iflowCObase_acft(rec)
                  temp=temp
                end do
                 close(LU)
                ! 
                !-----------------------------------------------------
                !
                do i =1,1250,1
                  do j = 1,3,1
                    li_iflowCOacft(j,i)=0
                  end do
                end do

                LU=11
                ! Standard climate data
                ! changed from 102 to 105 on 03.14.13
                 do rec=1,105,1
                    li_iyearCO(1,rec)=0.
                    li_iflowCOacft(1,rec)=0.
                  read(LU,*,err=1)li_iyearCO(1,rec),li_iflowCOacft(1,rec)
                 end do
                 close(LU)
                  lv_recordCO(1)=0.
                 lv_recordCO(1)=(gv_indexyearCO-li_iyearCO(1,1)) 

                ! Paleo
                !  1st record is nominally 762 AD
                LU=12
                 do rec=1,1244,1
                  read(LU,*,err=1)li_iyearCO(2,rec),li_iflowCOacft(2,rec)
                 end do
                 close(LU)
                 lv_recordCO(2)=(gv_indexyearCO-li_iyearCO(2,1)) 
              
                 LU=13
                 do rec=1,74,1
                  li_iflowCOacftScenario(rec)=0
                  read(LU,*,err=1)li_iyearCO(3,rec),li_iflowCOacft(3,rec)
                    li_iflowCOacftScenario(rec)=li_iflowCOacft(3,rec)
                 end do
                 close(LU)
                 lv_recordCO(3)=1 
                 !
                 ! Inflows between Powell and Mead
                 ! New on 12.19.12 DAS - Data From Don Gross: ADWR xls Infile labeled 
                 ! "InterveningNaturalFlows1906-2008_withExtensions_1.26.11.xlsx"
                 ! Data columns W trough AA
                 LU=14
    !              read(LU,*,err=1,iostat=ios)((gvf_COInFlowPowellToMead_acft(i,j),j=1,2),i=1906,2012)
                 do rec=1906,2012,1 ! 1906 through 2010
                    gvf_COInFlowPowellToMead_acft(rec,1)=0.
                    gvf_COInFlowPowellToMead_acft(rec,2)=0.
                  read(LU,*,err=1,iostat=ios)gvf_COInFlowPowellToMead_acft(rec,1),gvf_COInFlowPowellToMead_acft(rec,2)
                 end do
                 close(LU)
    1           continue
                if(ios >0)then
                
                goto 1000
                endif
                !
                 LU=15
                 read(LU,*,err=2,iostat=ios)((gvf_upperBasinDeliveries(i,j),j=1,3),i=2000,2060)
                 close(LU)
2           continue
                if(ios >0)then
                
                goto 1000
                endif


                !---------------------------------------------------------------------------
                !
                ! ft3 s-1 on a monthly basis
                ! USGS monthly data from lee's Ferry, AZ
                !http://waterdata.usgs.gov/nwis/monthly?referred_module &
                ! =sw&amp;site_no=09380000&amp;por_09380000_1=19133,00060,&
                ! 1,1921-10,2009-06&amp;start_dt=1922-01&amp;end_dt=2009-01 &
                ! &amp;format=html_table&amp;date_format=YYYY-MM-DD&amp; &
                ! rdb_compression=value&amp;submitted_form=parameter_selection_list
                ! ----------------------------------------------------------
                !---------------------------------------------------------------------------
                !

                ! Read unit 15; powell equalizatoins
                !--------------------------------------------------------------------------
                !
                LU=16
                read(LU,*,err=6,iostat=ios)((li_powellequal(i,j),j=1,3),i=2000,2037)
6           continue
                close(LU)
                if(ios >0)then
                 LU=16
                goto 1000
                endif

                ! http://lakepowell.water-data.com/index2.php
                ! http://lakemead.water-data.com/index2.php 
                ! Read in WaterShed_SVT.f90 first, and then rewind (line 204)
                LU=17
                do i = 2000,2010,1
                  li_initstoragePowell(i)=0
                  li_initstorageMead(i)=0
                 read(LU,*,err=7,iostat=ios)iyear,A(i),li_initstoragePowell(i),li_initstorageMead(i)
                  A(i)=A(i)*1 ! Only used to stop verbose warnings
                  iyear=iyear
                end do
!             lv_firstYear_b=2000
7           continue
                close(17)
                if(ios >0)then
                LU=17
                goto 1000
                endif
                ! LU 18 is CAGRD
             return
             1000 continue
                if(gvl_writeLog)then
                  string=12
                    call sStrings(string,errorString)
                    call eWrite(errorString,LU)
                endif
                gvl_errorFlag=.false.
            !
        end subroutine readFiles_b
        ! -----------------------

        ! ----------------------
        subroutine readParms_b()
            !
            ! ------ Types -------------
            real :: lvf_mod_powell=0.98
            real :: lvf_mod_mead=1.02
            ! ==========================
            !
                !
                read(30,*)li_capcapacity
                read(30,*)li_capevapconst
                read(30,*)li_capmaricopashare
                read(30,*)gv_climatefactorCO
                read(30,*)gv_droughtfactorCO
                read(30,*)gv_droughtyearCO
                read(30,*)gv_indexyearCO 
                read(30,*)li_meaddeadpool
                read(30,*)li_meadmax
                read(30,*)li_numdroughtyearsCO
                read(30,*)li_powelldeadpool
                read(30,*)li_powellmax
                read(30,*)li_yumaallocation
                !
                li_powellmax=li_powellmax*lvf_mod_powell
                li_meadmax=li_meadmax*lvf_mod_mead
                !
                close(30)
                gv_droughtyearendCO=gv_droughtyearCO+li_numdroughtyearsCO
            !
           return
        end subroutine readParms_b
        ! ------------------------

        ! -------------------------
        subroutine initialState_b()
            ! Powell 3681 feet on 1 jan 2000 (total storage = live + 2)
            vState_Ba_maf(2000)=23.435
            vState_Bb_maf(2000)=27.012167

            gv_dataCO=1
             go_riverFlowCO=0.
            go_riverFlowCO=li_iflowCOacft(gv_dataCO,lv_recordCO(1))
             go_StateCO=0.
            go_StateCO=nint(vState_Ba_maf(2000)+vState_Bb_maf(2000))*1e6
             go_deliveriesCO=0.
            go_deliveriesCO=nint(1.5*1e6)
            ! 
          return
        end subroutine initialState_b 
        ! ---------------------------
End Module lm_River_Ba
!
    ! -----------------------------
    subroutine initializeColorado()
      use lm_River_Ba
        !
        call initWSB()
        !
      return
    end subroutine initializeColorado
    ! -------------------------------
!
! ==================
Module lms_River_Ba
 use gm_ModelControl
  use gm_GlobalData
   use gm_TypeControl
    use gm_Euler
     use gm_DataAndSensitivity

        ! -------------
        !    logical, private :: lvl_validate=.false.
        ! -------------------------------------
        ! sent as output to the interface
        ! ---------------
        !    integer, public  :: go_riverFlowCO
        !    integer, public  :: go_StateCO
        !    integer, public  :: go_StatePowell
        !    integer, public  :: go_StateMead
        ! -------------------------------------
      ! -----
      include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
      !

            ! ------------ Module Global Types ----------------------------
            real, private :: lvf_initial_state_Powell,lvf_initial_state_Mead
            ! Total pool, NOT live pool
            real,parameter :: lvp_powellMax=26.217
            real,parameter :: lvp_meadMax=28.12
            real,parameter :: mpf_meadDead=2.0
            real,parameter :: mpf_powellDead=1.89
            !
            real :: mvf_inflow_acft,mvf_FlowsToMead_maf
            real :: mvf_upperBasin_acft,mvf_LeeFerry_acft,mvf_releaseFromPowell_acft
            real :: mvf_meadEvaporation_acft,mvf_powellEvaporation_acft
            real :: mvf_powell_excess_elev
            real :: mvf_powell_flux,mvf_mead_flux,mvf_adjustStorage
            real :: mvf_powellToMeadInflows_maf
            ! Module,parameter,float
            real,parameter :: mpf_seven=7.0
            real,parameter :: mpf_sevenFourEight=7.48
            real,parameter :: mpf_sevenEight=7.8
            real,parameter :: mpf_nine=9.0
            real,parameter :: mpf_nineFive=9.5
            real,parameter :: mpf_targetRelease=8.23
            !
            real :: mpf_pan_mead=2.28092  ! 89.8 inches average (http://pubs.usgs.gov/sir/2006/5252/pdf/sir20065252.pdf)
            real :: mpf_pan_reach=4.5
            real :: mpf_pan_powell=1.763776  ! 0.273386 m or 69.44 inches average (adjusted pan)(http://www.riversimulator.org/Resources/Hydrology/EvaporationAtLakePowellUCRCopt.pdf)
            real :: mpf_mod_BankStorage=1.5
            !
            real, parameter ::  mpf_maxEnvRiver=0.15
            real:: mvf_totalCOallocatedUB
            !
            real :: mpf_bankStoragePowell=0.08
            real :: mpf_bankStorageMead=0.065
            !
            ! =================================================================
            !
        ! ------------- Local Type Constructs -------------------------------------
          type watershed_B
            real(8):: vState_Ba_pred_maf(gpi_lBY:gpi_uBY)
            real(8):: vState_Bb_pred_maf(gpi_lBY:gpi_uBY)
            !
            real(8):: powell_flux_maf(gpi_lBY:gpi_uBY)
            real(8):: mead_flux_maf(gpi_lBY:gpi_uBY)
            !
            real(8):: lv_BaToBb(gpi_lBY:gpi_uBY),lv_OverFlow_ba(gpi_lBY:gpi_uBY)
            real(8):: lv_AZshareCO,lv_CAPmaricopa(gpi_lBY:gpi_uBY)
            real :: lvf_preCAP(gpi_lBY:gpi_uBY),lv_CAP(gpi_lBY:gpi_uBY),lv_AZshortageCO,lvd_upperBasin
          end type watershed_B
        ! ==============================================================================
      ! 
   contains
    ! ======
        !
        ! ------------------------
        subroutine initial_CO(wsb)
            !
            ! ----- Types -----
            integer :: yr
            ! =================
            !

            ! ---- Type Construct ----
            type(watershed_B)::wsb
            ! ========================
                !
                do yr = gpi_lBY,gpi_uBY,1
                    wsb%vState_Ba_pred_maf(yr)=0
                    wsb%vState_Bb_pred_maf(yr)=0
                    wsb%lv_BaToBb(yr)=0
                    wsb%lv_OverFlow_ba(yr)=0
                    wsb%lv_CAPmaricopa(yr)=0
                    wsb%lv_CAP(yr)=0
                end do
                    wsb%lv_AZshareCO=0
                    wsb%lv_AZshortageCO=0
                    wsb%lvd_upperBasin=0
                !
            !
         return
        end subroutine initial_CO
        ! -----------------------

      ! -----------------------
      subroutine aFlow_b(T,acft)
        !
        ! ----------------- Types ---------------
        integer :: acft,LU
        integer :: i,j,lvi_totalyears
        integer :: lvi_addRecords
        integer :: stop=13 ! use 13 for 2000 through 2012

        logical :: lvl_noTrace=.false.
        logical :: lvl_start2000
        ! ========================================
        !

        ! - Type Constructs -
        type(runTime)T  
        ! ===================
            !
            lvl_start2000=.true.
    !        lvi_addRec=0
            lvi_addRecords=0  
            if(gv_dataCO < 1)gv_dataCO=1
             lv_flow_B_maf(T%year)=0
            if(acft > 0)then
             ! For testing purposes (i.e., Testing.f90)
             lv_flow_B_maf(T%year)=acft*gpd_acftTomaf
            else
             !   Actual simulations: gpi_timeStep = 1, use annual data, otherwise, sum the monthly data
             ! (so that annual and monthly simulations are using the same input data (for runoff)
             !
             if(T%year == T%startyear)then
              !
              do i = 1,200,1
                do j = 1,3,1
                 lvi_flowCOacftMod(j,i)=0
                end do
              end do
                !
                vArecB=1208           
                vArecB=(gv_indexyearCO-li_iyearCO(gv_dataCO,1))
                !
                lvi_totalyears=T%endyear-T%startyear  +1
                !
                lvl_noTrace=.false.
              if(gv_dataCO == 1)then
              else if(gv_dataCO == 2)then
              else if(gv_dataCO == 3)then
                lvl_noTrace=.true.
              else if(gv_dataCO == 4)then
                lvl_noTrace=.true.
              endif
                !
                    if(gvi_COtrace < 1)gvi_COtrace=30
                    !
                    ! Force no-trace for Don Gross (ADWR) comparisons
                    ! 05.08.13
                  if(gpl_comparisons)then
                    stop=12
                    lvl_noTrace=.true.
                  endif
                  !
                  if(lvl_noTrace)then
                   !
                    if(gpl_comparisons)then
                   !
                        do i = 1,stop,1
                         lvi_flowCOacftMod(gv_dataCO,i)=lvd_iflowCObase_acft(i)
                        end do !li_iflowCOacft(3,rec)

                        do j = stop+1,86,1
                         lvi_flowCOacftMod(gv_dataCO,j)=li_iflowCOacftScenario(j-stop) 
                        end do
                    !
                    else
                    !
                        do i = 1,stop,1
                         lvi_flowCOacftMod(gv_dataCO,i)=lvd_iflowCObase_acft(i)
                        end do !li_iflowCOacft(3,rec)

                        do j = stop+1,86,1
                         lvi_flowCOacftMod(gv_dataCO,j)=li_iflowCOacftScenario(j-stop) 
                        end do
                    !
                    endif
                    vArecB=0 
                    !
                  else
                    ! 
                    if(lvl_start2000)then
                      ! Actually start in 2001
                      do i = 1,stop,1
                        lvi_flowCOacftMod(gv_dataCO,i)=lvd_iflowCObase_acft(i)
                      end do
                        !
                         do i = stop+1,lvi_totalyears,1

                              lvi_flowCOacftMod(gv_dataCO,i)=li_iflowCOacft(gv_dataCO,vArecB+1+lvi_addRecords)
                              lvi_addRecords=lvi_addRecords+1

                            if(vArecB+lvi_addRecords > vArecB+gvi_COtrace)then
                              ! set back to the "index" year, whatever it was set at in the interface
                              lvi_addRecords=0
                              lvi_flowCOacftMod(gv_dataCO,i)=li_iflowCOacft(gv_dataCO,vArecB+1+lvi_addRecords)
                              lvi_addRecords=lvi_addRecords+1
                            endif
                          end do
                        !
                      vArecB=0 !+ lvi_addRec
                    else
                         do i = 1,lvi_totalyears,1
                              lvi_flowCOacftMod(gv_dataCO,i)=li_iflowCOacft(gv_dataCO,vArecB+1+lvi_addRecords)
                              lvi_addRecords=lvi_addRecords+1
                            if(vArecB+lvi_addRecords >vArecB+gvi_COtrace)then
                              lvi_addRecords=0
                              lvi_flowCOacftMod(gv_dataCO,i)=li_iflowCOacft(gv_dataCO,vArecB+1+lvi_addRecords)
                              lvi_addRecords=lvi_addRecords+1
                            endif
                         end do
                      vArecB=0
                    endif ! end of start2000
                  endif ! of noTrace
               endif ! End of T%startyear
                !
                vArecB=vArecB+1
                !

                ! New Code 06.22.11
                lv_flow_B_maf(T%year)=lvi_flowCOacftMOD(gv_dataCO,vArecB)*gpd_acftTomaf !/1e6
                !
                if(lvl_noTrace)then
                    if(1 < vArecB)then
                        lv_flow_B_maf(T%year-1)=lvi_flowCOacftMOD(gv_dataCO,vArecB-1)*gpd_acftTomaf
                    else
                      ! start year
                        lv_flow_B_maf(T%year)=lvi_flowCOacftMOD(gv_dataCO,vArecB)*gpd_acftTomaf
                    endif
                else
                  if(2000 < T%year)then
                    if(1 < vArecB)then
                        lv_flow_B_maf(T%year-1)=lvi_flowCOacftMOD(gv_dataCO,vArecB-1)*gpd_acftTomaf
                    else
                        lv_flow_B_maf(T%year-1)=lvi_flowCOacftMOD(gv_dataCO,vArecB)*gpd_acftTomaf
                    endif
                  else
                    if(1 < vArecB)then
                        lv_flow_B_maf(T%year)=lvi_flowCOacftMOD(gv_dataCO,vArecB-1)*gpd_acftTomaf
                    else
                        lv_flow_B_maf(T%year)=lvi_flowCOacftMOD(gv_dataCO,vArecB)*gpd_acftTomaf
                    endif
                  endif
                endif
                !
                if( lv_flow_B_maf(T%year) <=0)then
                ! catch this exception
                lv_flow_B_maf(T%year)=0.
                goto 100
               endif

            endif
            !
        return
100     continue
            !
            if(T%year .EQ. T%startyear)then
                if(gvl_WriteLog)then
                    string=11
                    LU=0
                    call sStrings(string,errorString)
                    call eWrite(errorString,LU)
                endif
            endif
            !
        return
      end subroutine aFlow_b
      !---------------------
    
      ! --------------------------
      subroutine aModifyFlow_ba(T)
        !
        ! ------- Types ---------
        real :: cf,lvf_inflows
        real :: lvf_droughtFactor
        integer :: rec,dif,newRec
        ! =======================
        !

        ! - Type Constructs -
        type(runTime)T
        ! ===================
        !
            lvf_droughtFactor=1
            !
               ! Moved here on 12 April, 2013
                ! ----------------------------
!                call aOutToInterface(T)
                 !
                 mvf_LeeFerry_acft=0
                mvf_LeeFerry_acft=lv_flow_B_maf(T%year)*(1/gpd_acftTomaf)
                !
            ! Read into the module inflow data between Powell and Mead
            ! ------------------------------------------------------------------------
             lvf_inflows=0.
            if(T%year <=T%startyear)then
                gvf_averageInFlowCO=0.
                gvf_totalInFlowCO=0.
            endif
            if(.not. gpl_comparisons)then
                if(T%year <= 2012)then
                    lvf_inflows=max(0,gvf_COInFlowPowellToMead_acft(T%year,2))* gpd_acftTomaf
                else
                    dif=2013-gv_indexyearCO
                    rec=T%year-dif
                    newRec=0
                    !
                    if(rec <=2012)then
                     lvf_inflows=max(0,gvf_COInFlowPowellToMead_acft(rec,2))* gpd_acftTomaf
                    else
                        ! Added on 03.14.13
                       newRec=T%year-(dif * 2)
                        if(newRec <=2012)then
                          lvf_inflows=max(0,gvf_COInFlowPowellToMead_acft(newRec,2))* gpd_acftTomaf
                        else
                           newRec=T%year-(dif * 3)
                          lvf_inflows=max(0,gvf_COInFlowPowellToMead_acft(newRec,2))* gpd_acftTomaf
                        endif
                    endif
                endif
            else
                lvf_inflows=0.
                if(T%year <=T%startyear)then
                    gvf_averageInFlowCO=0.
                    gvf_totalInFlowCO=0.
                endif
                if(T%year <= 2011)then
                    lvf_inflows=max(0,gvf_COInFlowPowellToMead_acft(T%year,2))* gpd_acftTomaf
                else
                    dif=2012-gv_indexyearCO
                    rec=T%year-(dif)
                    newRec=0
                    !
                    if(rec <=2011)then
                     lvf_inflows=max(0,gvf_COInFlowPowellToMead_acft(rec,2))* gpd_acftTomaf
                    else
                          newRec=T%year-(dif * 2)
                        if(rec <= 2011)then
                         lvf_inflows=max(0,gvf_COInFlowPowellToMead_acft(rec,2))* gpd_acftTomaf
                        else
                          newRec=T%year-(dif * 3)
                         lvf_inflows=max(0,gvf_COInFlowPowellToMead_acft(newRec,2))* gpd_acftTomaf
                        endif
                    endif
                endif
            endif
                !
                ! Module variable for inflow to be used below
                ! 03.14.13
                  mvf_inflow_acft=0
                mvf_inflow_acft=(lvf_inflows*(1./gpd_acftTomaf))
                !
                ! Add inflow data to the Lee Ferry flow data
                gvf_totalInFlowCO=(gvf_totalInFlowCO+lvf_inflows)
                !
                ! Call the subroutine that calculates the climate factor (cf)
                ! read from the interface
                ! -                                                         -
                ! Cannot index on zero, thus the +1 on T%simyear
                ! ----------------------------------
                call ClimateFactorCO(T%simyear+1,cf)
                !
                lv_flow_B_maf(T%year)=(lv_flow_B_maf(T%year))*cf 
                !
                ! 05.09.13
                 mvf_powellToMeadInflows_maf=0
                mvf_powellToMeadInflows_maf=lvf_inflows*cf 
                ! 06.26.13
                ! 
                 lvf_powToMeadInFlowsYearMinus1(T%jumpYear)=0
                lvf_powToMeadInFlowsYearMinus1(T%jumpYear)=lvf_inflows*cf 
                 !

            ! Drought years and length of drought
            ! -------------------------------------
             lvf_droughtFactor=gv_droughtfactorCO
            if(T%year >= gv_droughtyearCO)then
              if(T%year <=  gv_droughtyearendCO)then
                lv_flow_B_maf(T%year)=lv_flow_B_maf(T%year)*lvf_droughtFactor !gv_droughtfactorCO
                mvf_powellToMeadInflows_maf=   mvf_powellToMeadInflows_maf* lvf_droughtFactor !gv_droughtfactorCO
              else
                lvf_droughtFactor=1
              endif
            else
                lvf_droughtFactor=1
            endif
            ! 
        return
      end subroutine aModifyFlow_ba
      !----------------------------

      ! ------------------------------
      subroutine aModifyFlow_bb(T)
        !
        ! -------------- Types ----------------
!        real :: AfterFlowToEnvironment
        ! =====================================
        !

        ! - Type Constructs -
        type(runTime)T
        ! ===================
            !

            ! Water for the environment
            ! maximum of 15% as of this writing
            ! 01.20.15 now will be determined by the pulse-flow
            ! minute 319 document of a portion of the
            ! 158,088.0 AF
            ! Reference:  http://ibwc.state.gov/Files/Minutes/Minute_319.pdf
            ! -------------------------------------
!             AfterFlowToEnvironment=0
                ! 02.25.15 Hardwired here until we use this variable
!             gvi_WaterToEnvironCO_acft_a=0
!            if(0. < gvi_WaterToEnvironCO_acft_a)then
!                ! 08.02.12 DAS
!                 maxEnvCO=mpf_maxEnvRiver*(lv_flow_B_maf(T%year)*(1./gpd_acftTomaf))
!                if(gvi_WaterToEnvironCO_acft_a < maxEnvCO)then
!                  AfterFlowToEnvironment=max(0.,lv_flow_B_maf(T%year)-(gvi_WaterToEnvironCO_acft_a*gpd_acftTomaf))
!                else
!                  AfterFlowToEnvironment= lv_flow_B_maf(T%year)-(maxEnvCO*gpd_acftTomaf)
!                endif
!                !
!                 lv_flow_B_maf(T%year)=AfterFlowToEnvironment
!                !
!                !
!                ! 08.07.12
!                ! 1000 AF minimum set on 02.25.15
!                if(lv_flow_B_maf(T%year) < 0.001)then
!                    if(gvl_writeLog)then
!                        string=23
!                        LU=0
!                        call sStrings(string,errorString)
!                        call eWrite(errorString,LU)
!                    endif
!                  !gvl_errorFlag=.false. Removed from use on 02.25.15
!                endif
!            else
!            endif
               ! Move here on 01.24.2014 das
                call aOutToInterface(T)
               !
            !
        return
      end subroutine aModifyFlow_bb
      !---------------------------

      ! --------------------------
      subroutine aModifyFlow_bc(T)
        !
        ! ---------- Types --------------
        real(8) :: lv_first=5.5
        real(8) :: lv_second=4.45
        real(8) :: lv_third

        real(8) :: lv_upperBasin  ! maf
        real(8) :: vMax
        real :: UBCU(2000:2012)
        ! ===============================
        !

        ! - Type Construct -
        type(runTime)T
        ! ==================
            ! 
            ! 12.20.12 DAS
            ! Year=1, 
            ! 2=2007 Upper CO River Comission Schedule
            ! 3=Arizona Upper Basin Depletion schedule- Don Gross- ADWR, 30 November 2012 
            ! 4=simulated estimates
            ! ================================================================================ 
             !
             lv_upperBasin=0.
            if(gvf_upperBasinEstimate < 4)then
              if(1 < gvf_upperBasinEstimate)then
                if(T%year < 2061)then
                ! Upper basin data read as thousand acre-feet
                 lv_upperBasin=gvf_upperBasinDeliveries(T%year,int(gvf_upperBasinEstimate))
                else
                 lv_upperBasin=gvf_upperBasinDeliveries(2060,int(gvf_upperBasinEstimate))
                endif
              else
                if(T%year < 2061)then
                 lv_upperBasin=gvf_upperBasinDeliveries(T%year,2)
                else
                 lv_upperBasin=gvf_upperBasinDeliveries(2060,2)
                endif
              endif
              !
                !
                ! 06.26.13 data from James Prairie (BofRec)upperbasinCUL_2015v1.10 1971-2015-4.1.13.xlsm
                ! --------------------
                if(T%year < 2013)then
                  call upperBasinEmpirical(T,UBCU)
                  lv_upperBasin=UBCU(T%year) ! k AF units
                endif
                !
              !
              vMax=lv_flow_B_maf(T%year)-((lv_UpperBasin*1000)* gpd_acftTomaf) 
              !
               go_upperBasinDeliveries=0.
              go_upperBasinDeliveries=anint(lv_UpperBasin*1000)    
              !
            else
              ! million acre-feet
              lv_third=(0.98/55.) * T%simyear
              lv_upperBasin=(min(lv_first,lv_second+lv_third))    
              !
              vMax=lv_flow_B_maf(T%year)-(lv_UpperBasin )
              ! acre-feet
              go_upperBasinDeliveries=anint(lv_UpperBasin*(1/gpd_acftTomaf))    
              !
            endif
                !
                lv_flow_B_maf(T%year)=max(0., (anint(vMax*(1/gpd_acftTomaf)) )* gpd_acftTomaf )
                !
                 mvf_upperBasin_acft=0
                mvf_upperBasin_acft=lv_UpperBasin*1000
                !
                gvf_upperBasinCUse_acft_a=mvf_upperBasin_acft
                mvf_totalCOallocatedUB=mvf_upperBasin_acft
            !
           return
      end subroutine aModifyFlow_bc
      !----------------------------

      subroutine upperBasinEmpirical(T,actualUBCU)
        !
        ! ---- Types ----------------
        ! Upper Basin Consumptive Use
        real :: actualUBCU(2000:2012)
        ! ===========================
        !

        ! - Type Construct -
        type(runTime)T
        ! ==================
            !
            ! Units are Thousand AF
            actualUBCU(2000)=3955.722; actualUBCU(2001)=4218.954
            actualUBCU(2002)=3774.367; actualUBCU(2003)=3789.496
            actualUBCU(2004)=3550.691; actualUBCU(2005)=3645.886
            actualUBCU(2006)=3837.139; actualUBCU(2007)=4120.767
            actualUBCU(2008)=4180.609; actualUBCU(2009)=4117.443
            actualUBCU(2010)=4001.442; actualUBCU(2011)=4100.000
            actualUBCU(2012)=4100.000
            !
!            actualUBCU(2000)=4618.021
!            actualUBCU(2001)=4835.292
!            actualUBCU(2002)=4287.951
!            actualUBCU(2003)=4217.664
!            actualUBCU(2004)=3905.892
!            actualUBCU(2005)=4039.762
!            actualUBCU(2006)=4281.102
!            actualUBCU(2007)=4573.771
!            actualUBCU(2008)=4676.306
!            actualUBCU(2009)=4650.962
!            actualUBCU(2010)=4532.513 
            !
         return
      end subroutine upperBasinEmpirical
      ! --------------------------------

        ! -------------------------
        subroutine aOutToInterface(T)
            !
            ! -- Type Construct --
            type(runTime)T
            ! ====================
                ! 
                ! Send to the interface the river flow data (moved here on 01.21.13)
                 go_riverFlowCO=0
                go_riverFlowCO=NINT((lv_flow_B_maf(T%year)) * (1./gpd_acftTomaf))
                !
            !
          return
        end subroutine aOutToInterface
        ! ----------------------------

        ! ----------------------------
        subroutine reservoirs_b(T,wsb)
            !
            ! -- Type Construct --
            type(watershed_B)::wsb
            type(runTime)T
            ! ====================
            !
                !
                if(T%atStartOfSimulation)then
                  call initial_CO(wsb)
                  !
                  if(gpl_runSensitivity)then
                    call initializeCOsensitivity()
                  endif
                  !
                endif
                !
                call initialStorage_B(T)            ! initial storage powell and mead
                 call expectedStorage_ba(T,wsb)      ! initialize powell: pre-powell
                  call expectedStorage_bb(T,wsb)      ! initialize Mead
                  call aModifyStorage_ba(T,wsb)       ! Powell equalizations - seven states rules
                 call aModifyStorage_bb(T,wsb)       ! lower basin water demand/use
                call designations_B(T,wsb)          ! CAP allocations
                !
            !
        return
      end subroutine reservoirs_b
      !--------------------------

      ! ----------------------------------
      subroutine initializeCOsensitivity()

          mpf_pan_mead=gpf_panMead 
          mpf_pan_reach=gpf_panReach
          mpf_pan_powell=gpf_panPowell  
          mpf_mod_BankStorage=gpf_bankStorage
            
          mpf_bankStoragePowell=gpf_bankStoragePowell
          mpf_bankStorageMead=gpf_bankStorageMead

        return
      end subroutine initializeCOsensitivity
      ! ----------------------------------

      ! ----------------------------------
      subroutine expectedStorage_ba(T,wsb)
            !
            ! ------ Types ---------------
            real(8) :: lv_Evap_ba
            real :: fEvaporation_Powell
            real :: lvf_flux
            real :: lvf_normalRelease=8.23
            ! ============================
            !

            ! ---- Type Construct ---
            type(watershed_B)::wsb
            type(runTime)T
            ! =======================
            !
                 !
                 ! Re-analyzed on 04.02.12 (maf)
                 ! Re-check my addition of Evap back into the estimate (05.04.12)
                 ! --------------------------------------------------------------------
                  lv_Evap_ba=0.
                 lv_Evap_ba=fEvaporation_Powell(vState_Ba_maf(T%year),mpf_pan_powell)  
                 !
                  lvf_flux=0
                 lvf_flux=(max(0.,vState_Ba_maf(T%year) & 
                    + lv_flow_B_maf(T%year)  &
                    - (lvf_normalRelease + lv_Evap_ba) )) 
                !
                if(li_powelldeadpool < lvf_flux)then
                 wsb%vState_Ba_pred_maf(T%year)= lvf_flux
                else
                 wsb%vState_Ba_pred_maf(T%year)=li_powelldeadpool
                endif
                !   
                 ! 04.30.13
                 wsb%powell_flux_maf(T%year)=lv_flow_B_maf(T%year)-lv_Evap_ba
                 !
            !
          return
        end subroutine expectedStorage_ba
        !--------------------------------

        ! ----------------------------
        subroutine initialStorage_B(T)
            !
            ! -- Type Construct --
            type(runTime)T
            ! ====================
            !
                ! Total Volume and NOT live volume (maf)
                ! --------------------------------------
                if(T%year == T%startyear)then
                   lvf_initial_state_Powell=vState_Ba_maf(T%year)
                   lvf_initial_state_Mead= vState_Bb_maf(T%year)
                endif
                !
            !
          return
        end subroutine initialStorage_B
        !------------------------------

        ! ---------------------------------
        subroutine expectedStorage_bb(T,wsb)
            !
            ! ----------- Types and parameters ----------
            real(8) :: lv_Evap_bb
            real :: fEvaporation_Mead
            real :: lvf_flux
            real, parameter :: lvp_normalOut=9.
            real,parameter :: lvp_normalIn=8.23
            ! ===========================================

            ! - Type Constructs ----
            type(watershed_B)::wsb
            type(runTime)T
            ! ======================
            !
                ! Total volume and NOT live volume
                ! BUT, using live storage for predicted for the balancing 
                ! ------------------------------------------------------------------------
                  lv_Evap_bb=0.
                lv_Evap_bb=fEvaporation_Mead(vState_Bb_maf(T%year),gpf_panMead)
                !
                  lvf_flux=0
                lvf_flux=max(0.,vState_Bb_maf(T%year) &
                    + lvp_normalIn + mvf_powellToMeadInflows_maf &
                    - (lvp_normalOut+lv_Evap_bb))
                !
                if(li_meaddeadpool < lvf_flux)then
                 wsb%vState_Bb_pred_maf(T%year)= lvf_flux 
                else
                 wsb%vState_Bb_pred_maf(T%year)=li_meaddeadpool
                endif
                !
                wsb%mead_flux_maf(T%year)= -(lv_Evap_bb + lvp_normalOut) + mvf_powellToMeadInflows_maf
                !
            !
          return
        end subroutine expectedStorage_Bb
        !--------------------------------

        ! Reservoir Bank Storage
        ! 05.10.13
        ! ------------------------------------------
        function fBankStoragePowell(lvf_diffStorage)
            !
            ! ------------- Types -----------------------
            real :: lvf_diffStorage
            real :: fBankStoragePowell
            !real, parameter :: lpf_bankStoragePowell=0.08
            ! ===========================================
            !
                !
                fBankStoragePowell=lvf_diffStorage*mpf_bankStoragePowell
                !
            !
         return
        end function fBankStoragePowell
        ! -----------------------------

        ! ----------------------------------------
        function fBankStorageMead(lvf_diffStorage)
            !
            ! ---------------- Types -------------------
            real :: lvf_diffStorage
            real :: fBankStorageMead
            !real, parameter :: lpf_bankStorageMead=0.065 ! 0.065
            ! ==========================================
                !
                fBankStorageMead=lvf_diffStorage*mpf_bankStorageMead
                !
            !
         return
        end function fBankStorageMead
        ! ----------------------------

        ! ---------------------------------
        subroutine aModifyStorage_ba(T,wsb)
            !
            ! ---------------------- Types -----------------------------
            real(8) :: lv_Evap_ba
            real(8) :: lv_State_ba,lvd_Pp,lvd_Pm,lvd_balanceamount,state
            real(8) :: lv_Ssr
            real :: fEvaporation_Powell
            real :: lvf_diffStorage(3)
            real(8) :: lvd_annual
            real :: lvf_bankStorage_maf
            real :: lvf_fluxPreBanked,lvf_flux

            logical :: lvl_testReservoirOps
            ! ==========================================================

            ! -- Type Constructs ---
            type(watershed_B)::wsb
            type(runTime)T
            ! ======================
             !  
             ! ------------------------------
                lvl_testReservoirOps=.false.
               !
                  state=0
                 state=vState_Ba_maf(T%year) 
                 !                  
                 lv_Evap_ba=0
                lv_Evap_ba=fEvaporation_Powell(vState_Ba_maf(T%year),mpf_pan_powell)  
               !
                 mvf_powellEvaporation_acft=0
                mvf_powellEvaporation_acft=lv_Evap_ba*(1/gpd_acftTomaf)
                !
                 lvd_Pp=0
                lvd_Pp = wsb%vState_Ba_pred_maf(T%year) 
                !
                 lvd_Pm=0
                lvd_Pm= wsb%vState_Bb_pred_maf(T%year) 
                !
                 lvf_diffStorage(1)=0
                lvf_diffStorage(1)=state-lvd_Pp
                !
                ! --------------------------------------------------------------------------
                ! Verification of my code
                ! =========================
                if(gpl_verify)then
                  call actualStorage(T,lvd_Pp,lvd_Pm,lvf_diffStorage)
                endif
                !
                   lvf_diffStorage(3)= lvf_diffStorage(1)*mpf_mod_BankStorage
                !
                ! =========================
                !
                ! Balance amount
                ! --------------------------------------
                 lvd_balanceamount=0
                lvd_balanceamount=(lvd_Pp-lvd_Pm)*0.5
                !
                ! Flux estiamtes used to estimate lv_Ssr
                ! ---------------------------------------
                mvf_powell_flux=wsb%powell_flux_maf(T%year)
                mvf_mead_flux=wsb%mead_flux_maf(T%year)
                !
!               ! Threshold storage and associated release
                ! ---------------------------------------
                 call threshold_b(T,lvd_Pp,lvd_balanceamount,lv_Ssr)
                ! ------------------
                !
                 wsb%lv_BaToBb(T%year)=0
                wsb%lv_BaToBb(T%year)=lv_Ssr
                !
                ! Overright with actual release estimates if running verify simulations
                ! ------------------------
                if(gpl_verify)then
                  call actualRelease(T,wsb)
                else
                    ! Override release (rules unknown 2000 to 2007)
                  call overrideRelease(T,wsb)
                endif
                !
                if(lvl_testReservoirOps)then
                 call adwrRelease(T,wsb)
                endif
                !
                 mvf_releaseFromPowell_acft=0
                mvf_releaseFromPowell_acft=wsb%lv_BaToBb(T%year)
                !
                 lvf_fluxPreBanked=0
                lvf_fluxPreBanked=lv_flow_B_maf(T%year)-mvf_releaseFromPowell_acft-lv_Evap_ba
                !
                 lvf_bankStorage_maf=0
                lvf_bankStorage_maf=fBankStoragePowell(lvf_diffStorage(3))
                !
                 lvf_flux=0
                lvf_flux=lvf_fluxPreBanked+lvf_bankStorage_maf
                !
                 lvd_annual=0
                lvd_annual= state + lvf_flux 
                !
                wsb%lv_OverFlow_ba(T%year)=0
              if(lvd_annual < lvp_powellMax)then
                if(mpf_powellDead <= lvd_annual)then
                  lv_State_ba=lvd_annual
                else
                    wsb%lv_BaToBb(T%year)=max(0,state+lv_flow_B_maf(T%year)+lvf_bankStorage_maf -lv_Evap_ba)
                    lv_State_ba=mpf_powellDead
!                    if(gvl_writeLog)write(7,*)"Powell at Dead Pool - line 1220 in WaterShed_CO.f90"
                endif
              else
                lv_State_ba=lvp_powellMax
                wsb%lv_OverFlow_ba(T%year)=lvd_annual-lvp_powellMax
              endif
                !
                vState_Ba_maf(T%year)=lv_State_ba
                !
            !
          return
        end subroutine aModifyStorage_ba
        ! ---------------------------------

        ! -----------------------------
        subroutine actualRelease(T,wsb)
            !
            ! -- Type Constructs ---
            type(watershed_B)::wsb
            type(runTime)T
            ! ======================
                !
                  ! Where did this information come from? I cannot recall
!               http://www.usbr.gov/lc/region/g4000/archives_report.cfm?DIR=cy2002&FILE=01_2002
                  wsb%lv_BaToBb(2000)=8.492000
                  wsb%lv_BaToBb(2001)=8.009909 ; wsb%lv_BaToBb(2002)=7.795931
                  wsb%lv_BaToBb(2003)=8.222020 ; wsb%lv_BaToBb(2004)=8.473741
                  wsb%lv_BaToBb(2005)=8.252576 ; wsb%lv_BaToBb(2006)=8.408633
                  wsb%lv_BaToBb(2007)=8.228612 ; wsb%lv_BaToBb(2008)=9.123499
                  wsb%lv_BaToBb(2009)=8.296381 ; wsb%lv_BaToBb(2010)=8.173804
                  wsb%lv_BaToBb(2011)=13.645000 ; wsb%lv_BaToBb(2012)=8.217000
                  !
                !
            !
          return
        end subroutine actualRelease 
        ! ---------------------------

        ! ------------------------------
        subroutine overrideRelease(T,wsb)
            !
            ! -- Type Constructs ---
            type(watershed_B)::wsb
            type(runTime)T
            ! ======================
                !
                wsb%lv_BaToBb(2000)=8.492000
                wsb%lv_BaToBb(2001)=8.009909 ; wsb%lv_BaToBb(2002)=7.795931
                wsb%lv_BaToBb(2003)=8.222020 ; wsb%lv_BaToBb(2004)=8.473741
                wsb%lv_BaToBb(2005)=8.252576 ; wsb%lv_BaToBb(2006)=8.408633
                wsb%lv_BaToBb(2007)=8.228612 
                !
            !
          return
        end subroutine overrideRelease 
        ! ---------------------------

        ! -----------------------------
        subroutine adwrRelease(T,wsb)
            !
            ! -- Type Constructs ---
            type(watershed_B)::wsb
            type(runTime)T
            ! ======================
                !
                wsb%lv_BaToBb(2000)=8.492000
                wsb%lv_BaToBb(2001)=8.009909
                wsb%lv_BaToBb(2002)=7.795931
                wsb%lv_BaToBb(2003)=8.222020
                wsb%lv_BaToBb(2004)=8.473741
                wsb%lv_BaToBb(2005)=8.252576 
                wsb%lv_BaToBb(2006)=8.408633
                wsb%lv_BaToBb(2007)=8.228612 
                wsb%lv_BaToBb(2008)=9.123499
                wsb%lv_BaToBb(2009)=8.296381 
                wsb%lv_BaToBb(2010)=8.173804
                wsb%lv_BaToBb(2011)=13.645000 
                wsb%lv_BaToBb(2012)= 11.6452060
                wsb%lv_BaToBb(2013)= 9.2491570
                wsb%lv_BaToBb(2014)= 8.5310280
                wsb%lv_BaToBb(2015)= 10.3970470
                wsb%lv_BaToBb(2016)= 11.1453710
                wsb%lv_BaToBb(2017)= 11.3389100
                wsb%lv_BaToBb(2018)= 12.9610880
                wsb%lv_BaToBb(2019)= 10.1549250
                wsb%lv_BaToBb(2020)= 8.2299960
                wsb%lv_BaToBb(2021)= 8.2299970
                wsb%lv_BaToBb(2022)= 8.6887150
                wsb%lv_BaToBb(2023)= 8.2299970
                wsb%lv_BaToBb(2024)= 8.2299970
                wsb%lv_BaToBb(2025)= 8.2299970
                wsb%lv_BaToBb(2026)= 8.2299970
                wsb%lv_BaToBb(2027)= 8.2299970
                wsb%lv_BaToBb(2028)= 9.0000010
                wsb%lv_BaToBb(2029)= 9.0000010
                wsb%lv_BaToBb(2030)= 7.4800000
                wsb%lv_BaToBb(2031)= 9.0000020
                wsb%lv_BaToBb(2032)= 9.0000020
                wsb%lv_BaToBb(2033)= 9.0000010
                wsb%lv_BaToBb(2034)= 9.0000020
                wsb%lv_BaToBb(2035)= 9.0000020
                wsb%lv_BaToBb(2036)= 9.0000010
                wsb%lv_BaToBb(2037)= 9.0000020
                wsb%lv_BaToBb(2038)= 9.0000010
                wsb%lv_BaToBb(2039)= 9.0000020
                wsb%lv_BaToBb(2040)= 9.0000020
                wsb%lv_BaToBb(2041)= 9.0000020
                wsb%lv_BaToBb(2042)= 9.0000020
                wsb%lv_BaToBb(2043)= 9.0000020
                wsb%lv_BaToBb(2044)= 7.0600000
                wsb%lv_BaToBb(2045)= 7.9000000
                wsb%lv_BaToBb(2046)= 9.0799990
                wsb%lv_BaToBb(2047)= 7.9000000
                wsb%lv_BaToBb(2048)= 8.5800020
                wsb%lv_BaToBb(2049)= 7.9000000
                wsb%lv_BaToBb(2050)= 7.5417060
                wsb%lv_BaToBb(2051)= 9.4933840
                wsb%lv_BaToBb(2052)= 7.0320240
                wsb%lv_BaToBb(2053)= 6.2642940
                wsb%lv_BaToBb(2054)= 10.1212230
                wsb%lv_BaToBb(2055)= 7.5758510
                wsb%lv_BaToBb(2056)= 6.7785710
                wsb%lv_BaToBb(2057)= 7.5094350
                wsb%lv_BaToBb(2058)= 8.4482460
                wsb%lv_BaToBb(2059)= 9.1894930
                wsb%lv_BaToBb(2060)= 8.9231020
                wsb%lv_BaToBb(2061)= 8.1115650
                wsb%lv_BaToBb(2062)= 9.5000010
                wsb%lv_BaToBb(2063)= 9.4645810
                wsb%lv_BaToBb(2064)= 9.5000000
                wsb%lv_BaToBb(2065)= 8.0191600
                wsb%lv_BaToBb(2066)= 6.1058530
                wsb%lv_BaToBb(2067)= 7.7747520
                wsb%lv_BaToBb(2068)= 9.6258480
                wsb%lv_BaToBb(2069)= 8.9914990
                wsb%lv_BaToBb(2070)= 7.9000000
                !
            !
          return
        end subroutine adwrRelease 
        ! ---------------------------

        ! ------------------------------------------------
        subroutine actualStorage(T,lvd_Pp,lvd_Pm,lvd_diff)
            !
            ! ---------------- Types -----------
            real(8) :: lvd_Pp,lvd_Pm
            real :: lvd_diff(2)
            real :: lvf_tempStorage(2000:2012,2)
            ! ==================================
            !

            ! -- Type Construct --
            type(runTime)T
            ! ====================
                !
                 lvf_tempStorage(2000,1)=23.333075 ;  lvf_tempStorage(2000,2)=27.012167
                 lvf_tempStorage(2001,1)=21.710577;  lvf_tempStorage(2001,2)=24.368162
                 lvf_tempStorage(2002,1)=20.010000 ;  lvf_tempStorage(2002,2)=22.680000
                 lvf_tempStorage(2003,1)=14.940000 ;  lvf_tempStorage(2003,2)=20.760000
                 lvf_tempStorage(2004,1)=12.550000 ;  lvf_tempStorage(2004,2)=19.380000
                 lvf_tempStorage(2005,1)=9.180000 ;  lvf_tempStorage(2005,2)=18.280000
                 lvf_tempStorage(2006,1)=13.580000 ;  lvf_tempStorage(2006,2)=17.140000
                 lvf_tempStorage(2007,1)=13.670000 ;  lvf_tempStorage(2007,2)=16.100000
                 lvf_tempStorage(2008,1)=12.000000 ;  lvf_tempStorage(2008,2)=14.870000
                 lvf_tempStorage(2009,1)=14.420000 ;  lvf_tempStorage(2009,2)=14.680000
                 lvf_tempStorage(2010,1)=16.510000 ;  lvf_tempStorage(2010,2)=13.640000
                !
                 lvf_tempStorage(2011,1)=14.457000 ;  lvf_tempStorage(2011,2)=10.3020000
                 lvf_tempStorage(2012,1)=15.958000 ;  lvf_tempStorage(2012,2)=14.8970000
                !
                lvd_Pm=0
                if(T%year <= 2012)then
                    lvd_Pp=lvf_tempStorage(T%year,1)
                    lvd_Pm=lvf_tempStorage(T%year,2)
                     if(T%year < 2012)then
                      lvd_diff(1)= lvf_tempStorage(T%year,1)-lvf_tempStorage(T%year+1,1)
                      lvd_diff(2)= lvf_tempStorage(T%year,2)-lvf_tempStorage(T%year+1,2)
                     endif
                endif   
                !
            !
          return
        end subroutine actualStorage
        ! ---------------------------

            !--------------------------------------------------------
            subroutine threshold_b(T,lvd_Pp,lvd_balanceamount,lv_Ssr)
                !
                ! Data from ModelingWorkshopPresentations.pdf (Bureau of Reclamation- 2007)
                ! Mead at 1075 feet, volume is 9.4 maf (active)
                ! Mead at 1050 feet, Volume is 7.5 maf active
                ! Mead at 1025 feet, volume is 5.8 maf (active)
                !
                ! ------------------------- Types ---------------------
                real(8) :: state,lvd_Pp,lvd_balanceamount,lvd_spill
                real(8) :: lv_Ssr
!                integer :: vTier
                ! 
                ! I model total storage for the reservoirs, but for balancing I
                ! use live storage.
                ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                !  Total storage
                !  real(8) :: vBa_Teir_1=7.82  ! Powell at 3,525 feet
                !  real(8) :: vBa_Teir_2=11.41 ! Powell at 3,575 feet
                !  real(8) :: vBa_Teir_3=17.43 ! Powell at 3,636 feet
                !  real(8) :: vBa_Teir_4=21.18 ! Powell at 3,666 feet
                ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                ! Live storage (active)
                ! -------------------------------------------------
                !                            
                real,parameter :: lvf_TierElevation_1=3525 ! live
                real,parameter :: lvf_TierElevation_2=3575
                real,parameter :: lvf_TierElevation_602a=3630
                real,parameter :: lvf_TierElevation_3=3636
                real,parameter :: lvf_TierElevation_4=3666

                real,parameter :: lvf_TierElevation_s1=3560
                real,parameter :: lvf_TierElevation_s2=3595
                !
                real,parameter :: lvf_TierElevationMead_1=1025 
                real,parameter :: lvf_TierElevationMead_2=1075
                !
                real :: lvf_min,lvf_max
                ! model elevation = intercept + scaling * storage**slope
                real :: lvf_powell_elevation,lvf_mead_elevation
                ! Powell max is 24.322- live 
                ! Mead max is 25.877 -live
                ! --------------------------

                logical :: lvl_RSA,lvl_WSA,lvl_CBSA,lvl_BSA,lvl_NAA
                ! ===============================================================
                !

                ! -- Type Construct --
                type(runTime)T
                ! ====================
                  !
                  ! =========================================================================
                  ! Changed again on 16 November 2010, das
                  ! Changed again on 25 April, 2013, DAS
                  !
                  ! Record of Decison, December 2007 (RecordofDecision.pdf)
                  ! page 50-55
                  !
                  ! -------------------------------------------------------------------
                  !
!                    vTier=3
                    lvl_RSA=.false.
                    lvl_WSA=.false.
                    lvl_CBSA=.false.
                    lvl_BSA=.false.
                    lvl_NAA=.false.
                   !
                    !
                     lvl_RSA=.true.
                    !
                       lvf_powell_elevation=0
                        ! 04.26.13
                        state=lvd_Pp
                        ! 05.02.13
                        state=vState_Ba_maf(T%year)
                      call elevationsPowell(state,lvf_powell_elevation)
                      !
                      ! Grab Mead elevation estimate
                        state=vState_Bb_maf(T%year)
                      call elevationsMead(state,lvf_mead_elevation)
                      !
                      lvd_spill=0
                      if(lvp_powellMax < lvd_Pp)lvd_spill=lvd_Pp-lvp_powellMax
                    !
                    lv_Ssr=0
                    !
                    ! ModelingWorkshopPresentations.pdf
                    ! 05.06.13
                    ! Reservoir Storage Alternative
                    ! ----------------------------------
                  if(T%year < 2008)then
                    if(lvl_RSA)then
                        if(lvf_powell_elevation < lvf_TierElevation_s1)then
                             lvf_min=mpf_sevenEight
                             lvf_max=mpf_nineFive
                            !
                            call balanceContent(T,lvf_min,lvf_max,lvd_balanceamount,lv_Ssr)
                        else
                           if(lvf_powell_elevation < lvf_TierElevation_s2)then                           
                             lv_Ssr=mpf_sevenEight
                           else
                             if(lvf_powell_elevation < lvf_TierElevation_602a)then
                                lv_Ssr=mpf_targetRelease   
                             else 
                                 lvf_min=mpf_targetRelease !mpf_sevenEight
                                 lvf_max=mpf_nineFive
                                call balanceContent(T,lvf_min,lvf_max,lvd_spill,lv_Ssr)
                             endif
                           endif
                        endif      
                    else if(lvl_WSA)then
                       if(lvf_powell_elevation < lvf_TierElevation_2)then
                             lvf_min=mpf_seven
                             lvf_max=mpf_nineFive
                            !
                            call balanceContent(T,lvf_min,lvf_max,lvd_balanceamount,lv_Ssr)
                        else
                           if(lvf_powell_elevation < lvf_TierElevation_602a)then                           
                             lv_Ssr=mpf_targetRelease
                                ! Mead at 1025 feet
                                if(lvf_mead_elevation < lvf_TierElevationMead_2)then
                                      lvf_min=mpf_seven
                                      lvf_max=mpf_nineFive
                                    call balanceContent(T,lvf_min,lvf_max,lvd_balanceamount,lv_Ssr)
                                endif
                           else
                                  lvf_min=mpf_targetRelease
                                  lvf_max=mpf_nineFive
                                call balanceContent(T,lvf_min,lvf_max,lvd_spill,lv_Ssr)
                           endif
                        endif    
                    else if(lvl_CBSA .or. lvl_BSA)then
                       if(lvf_powell_elevation < lvf_TierElevation_1)then
                             lvf_min=mpf_seven
                             lvf_max=mpf_nineFive
                            !
                            call balanceContent(T,lvf_min,lvf_max,lvd_balanceamount,lv_Ssr)
                        else
                           if(lvf_powell_elevation < lvf_TierElevation_2)then                           
                             lv_Ssr=mpf_sevenFourEight
                                ! Mead at 1025 feet
                                if(lvf_mead_elevation < lvf_TierElevationMead_1)then
                                  lv_Ssr=mpf_targetRelease
                                endif
                           else
                             if(lvf_powell_elevation < lvf_TierElevation_602a)then
                                lv_Ssr=mpf_targetRelease   
                                if(lvf_mead_elevation < lvf_TierElevationMead_2)then
                                  lvf_min=mpf_seven
                                  lvf_max=mpf_nine
                                 call balanceContent(T,lvf_min,lvf_max,lvd_spill,lv_Ssr)
                                endif

                             else 
                                 lvf_min=mpf_targetRelease
                                 lvf_max=mpf_nineFive
                                call balanceContent(T,lvf_min,lvf_max,lvd_balanceamount,lv_Ssr)
                             endif
                           endif
                        endif    
                    else if(lvl_NAA)then
                        if(lvf_powell_elevation < lvf_TierElevation_602a)then
                           lv_Ssr=mpf_targetRelease   
                        else
                           lvf_min=mpf_targetRelease
                           lvf_max=mpf_nineFive
                          call balanceContent(T,lvf_min,lvf_max,lvd_spill,lv_Ssr)
                        endif
                    else
                    endif
                  else
                    !  new on 04.29.13
                        !
                         if(lvf_powell_elevation < lvf_TierElevation_1)then
                            !
!                            vTier = 1                          
                            ! Section 6.D (page 53)
                            ! Lower Elevation Balancing Teir
                            ! 3370 < Powell < 3525
                            !
                              lvf_min=mpf_seven
                              lvf_max=mpf_nineFive
                            call balanceContent(T,lvf_min,lvf_max,lvd_balanceamount,lv_Ssr)
                            !
                         else
                           if(lvf_powell_elevation < lvf_TierElevation_2)then
                            !
!                             vTier=2
                                ! Mid-Elevation Balancing Tier
                                ! 3525 < Powell < 3575
                                ! Section 6.C.1
                                ! --------------
                                lv_Ssr=mpf_sevenFourEight
                                !
                                ! Mead at 1025 feet
                                if(lvf_mead_elevation < lvf_TierElevationMead_1)then
                                  lv_Ssr=mpf_targetRelease
                                endif
                            !
                           else
                             if(lvf_powell_elevation < lvf_TierElevation_3)then 
                                !
!                                vTier=3
                                !  new on 04.29.13
                                ! Upper Elevation Balancing Tier and equalization (3636 to 3666)
                                ! 3575 < Powell < 3666
                                ! Section 6.B.?
                                !
                               call targetAmount(T,lvd_balanceamount,lv_Ssr)
                                !
                             else 
                                if(lvf_powell_elevation < lvf_TierElevation_4)then
!                                 vTier=4
                                 call targetAmount(T,lvd_balanceamount,lv_Ssr)
                                else
                                    ! new on 05.02.13
                                    ! Equilization Tier
                                    ! 3666 < Powell < 3700
                                    ! Section 6.A.1
                                    ! --------------               
!                                    vTier=5
                                    if(T%year < 2027)then
                                      call targetAmount(T,lvd_balanceamount,lv_Ssr)
                                    else
                                      lvf_min=mpf_targetRelease
                                      lvf_max=mpf_nineFive
                                     call balanceContent(T,lvf_min,lvf_max,lvd_spill,lv_Ssr)
                                    endif
                                endif
                             endif
                           endif
                         endif      
                  endif
                !
                if(T%year ==2004)lv_Ssr=8.473741
                if(T%year ==2008)lv_Ssr=9.123499
                !
1               format(I4,1x,I1,1x,2(F10.6,1x),1x,2(F6.1,1x))
                !
                ! =========================================================================
                !
              return
            end subroutine threshold_b
            !-------------------------

            ! -------------------------------------------------
            subroutine targetAmount(T,lvd_balanceamount,lv_Ssr)
                !
                ! ---- Types and parameters ----
                real :: lvf_powell_elevation
                real :: lvf_useElevation
                real(8) :: state
                real(8) :: lvd_balanceamount
                real(8) :: lv_Ssr,lvd_tempSsr
                real :: lvf_targetStorage
                !
                real :: lvf_min,lvf_max
                ! ==============================
                !

                ! - Type Constructs -
                type(runTime)T
                ! ===================
                    !
                    lv_Ssr=0
                    lvd_tempSsr=mpf_targetRelease   
                    !
                    state=vState_Ba_maf(T%year)
                    state=vState_Ba_maf(T%year)+mvf_powell_flux-lvd_tempSsr  
                    !
                     lvf_powell_elevation=0
                    call elevationsPowell(state,lvf_powell_elevation)
                    !
                     lvf_useElevation=0
                    !
                    if(T%year < 2027)then
                     ! Table page 51 of the 2007 agreement
                     ! -------------------------------------
                      call targetElevations(T,lvf_useElevation)
                     !
                      call stateFromElevationPowell(lvf_targetStorage,lvf_useElevation)
                      !
                      ! Both Tiers set by elevation of powell, only
                      ! Avoid spill, equalize, or release 8.23
                      ! 05.02.13
                      ! 6.A.1 page 53
                      ! ----------------------------------------------
                        if(lvf_useElevation <= lvf_powell_elevation)then
                            !
                            call equalizationTier(T,lvf_powell_elevation,lvf_useElevation,lvd_balanceamount,lv_Ssr)
                            !
                        else
                            !
                            call upperElevationTier(T,lvd_balanceamount,lv_Ssr)         
                            !
                        endif
                         !
                    else ! 2027 < year
                        !
                           lvf_min=mpf_seven
                           lvf_max=mpf_nine !mpf_nineFive
                        call balanceContent(T,lvf_min,lvf_max,lvd_balanceamount,lv_Ssr)   
                        !
                    endif
                !
              return
            end subroutine targetAmount
            ! -------------------------

            ! -------------------------------------------------------------------------------------------
            subroutine equalizationTier(T,lvf_powell_elevation,lvf_useElevation,lvd_balanceamount,lv_Ssr)
                !
                ! ------------ Types and parameters -----------
                integer :: i,j,k
                integer :: loop=500,lvi_count

                real :: lvf_mead_elev
                real(8) :: state,state_Mead,state_Powell
                real(8) :: truncPowell,truncMead
                real :: lvf_targetStorage
                real(8) :: lvd_balanceamount
                real(8) :: lv_Ssr,lvd_tempSsr
                real(8) :: lvd_holdRelease
                real ::  lvf_powell_elevation
                real :: lvf_useElevation
                real, parameter :: mead_elevationSpecial=1105
                real :: x,y
                real :: lvf_min,lvf_max

                logical :: lvl_pass_i,lvl_pass_j,lvl_pass_k
                ! =============================================
                !

                ! - Type Constructs -
                type(runTime)T
                ! ===================
                    !
                    lvd_holdRelease=0
                    lvl_pass_i=.false. ; lvl_pass_j=.false. ; lvl_pass_k=.false.
                    !
                    lvi_count=0 ; x=0 ; y=0 ; lv_Ssr=0
                    lvd_tempSsr=mpf_targetRelease   
                    !
                    ! ----------------------------------------
!                     !
                        call stateFromElevationPowell(lvf_targetStorage,lvf_useElevation)
                        !
                        ! 6.A.1 page 53
                        ! ----------------------------------------------

                        if(lvf_useElevation <= lvf_powell_elevation)then
                            do i = 1,loop,1
                                !
                                 lvi_count=i
                                ! Place holder to use previous release before storage threshold is broached
                                lvd_holdRelease=lvd_tempSsr
                                lvd_tempSsr=max(mpf_targetRelease,lvd_tempSsr+y)
                                ! 06.25.13 below....
                                lvd_tempSsr=max(max(mpf_targetRelease,lvd_tempSsr+y),lvd_tempSsr+lvd_balanceamount)

                                ! Section 6.A.1
                                ! ---------------------------------------------------------------------------
                                  state_Powell=0
                                 state_Powell = vState_Ba_maf(T%year)+mvf_powell_flux-lvd_tempSsr 
                                 !
                                  state_Mead=0
                                 state_Mead=vState_Bb_maf(T%year)+mvf_mead_flux+lvd_tempSsr 
                                 !
                                if((state_Powell <= lvf_targetStorage) .or. (state_Powell < state_Mead))then
                                    !
                                    if(1 < lvi_count)then
                                        lvd_tempSsr=lvd_holdRelease
                                      lvl_pass_i=.true.
                                      exit
                                    endif
                                    !
                                    lv_Ssr=mpf_targetRelease 
                                     !
                                     lvf_powell_elevation=0
                                     state=state_Powell
                                    call elevationsPowell(state,lvf_powell_elevation)
                                    !   
                                      lvf_mead_elev=0
                                     state=state_Mead
                                    call elevationsMead(state,lvf_mead_elev)
                                    !
                                     do j = 1,loop,1
                                        if(lvf_mead_elev <= mead_elevationSpecial)then
                                          do k = 1,loop,1

                                            lv_Ssr=lv_Ssr+x
                                            !
                                             state_Powell =  vState_Ba_maf(T%year)+mvf_powell_flux-lv_Ssr 
                                             lvf_powell_elevation=0
                                             state=state_Powell
                                            call elevationsPowell(state,lvf_powell_elevation)

                                              state_Mead=vState_Bb_maf(T%year)+mvf_mead_flux+lv_Ssr 
                                             lvf_mead_elev=0
                                             state=state_Mead
                                            call elevationsMead(state,lvf_mead_elev)
                                            !
                                              truncPowell=0
                                             truncPowell=DINT(state_Powell*1000)
                                              truncMead=0
                                             truncMead=DINT(state_Mead*1000)
                                            ! Nearest 1000 acre-feet comparison
                                            ! i.e., Powell and Mead are equilivent at +- 1000 AF
                                            if(truncPowell <= truncMead)then
                                                lvl_pass_k=.true.
                                                exit
                                            else if(lvf_mead_elev >= mead_elevationSpecial)then
                                                lvl_pass_k=.true.
                                                exit
                                            else if (lvf_powell_elevation  <= (lvf_useElevation-20))then
                                                lvl_pass_k=.true.
                                                exit
                                            endif
                                            !
                                           x=x+0.0025
                                          end do
                                         !
                                        else
                                            lvl_pass_j=.true.
                                        endif
                                        !

                                        if(lvl_pass_k .or. lvl_pass_j)exit
                                      end do
                                    !

                                else
                                    !
                                    y=y+0.001
                                    !
                                endif
                                !
                              lv_Ssr=lvd_tempSsr
                              !
                              if(lvl_pass_k .or. lvl_pass_j .or. lvl_pass_i)exit
                            end do
                        !
                        else

                         ! Powell below target elevation
                         ! 04.30.13
                              lvf_min=mpf_seven
                              lvf_max=mpf_nine
                         call balanceContent(T,lvf_min,lvf_max,lvd_balanceamount,lv_Ssr)
                         !
                        endif
                !
              return
            end subroutine equalizationTier
            ! -------------------------------

            ! -------------------------------------------------------
            subroutine upperElevationTier(T,lvd_balanceamount,lv_Ssr)
                !
                ! ----------- Types and parameters --------------
                real :: lvf_powell_elevation,lvf_mead_elev
                real :: lvf_useElevation
                real :: lvf_targetStorage
                real(8) :: state,state_Mead,state_Powell
                real(8) :: lvd_balanceamount
                real(8) :: lv_Ssr
                real, parameter :: lvf_TierElevation_2=3575
                real, parameter :: lvf_TierElevationMead_2=1075
                real :: lvf_min,lvf_max
                ! ===============================================
                !

                ! - Type Constructs -
                type(runTime)T
                ! ===================
                    !
                    ! Upper Elevation Balancing Tier
                    ! 05.02.13
                    !
                     lv_Ssr=mpf_targetRelease
                     state=0
                    !
                     call targetElevations(T,lvf_useElevation)
                     !
                     call stateFromElevationPowell(lvf_targetStorage,lvf_useElevation)
                     !
                      state_Powell = 0   
                     state_Powell = vState_Ba_maf(T%year)+mvf_powell_flux-lv_Ssr     
                     !
                     lvf_powell_elevation=0
                     state=state_Powell
                    call elevationsPowell(state,lvf_powell_elevation)

                      state_Mead=0
                     state_Mead=vState_Bb_maf(T%year)+mvf_mead_flux+lv_Ssr 
                     !
                      lvf_mead_elev=0
                     state=state_Mead
                    call elevationsMead(state,lvf_mead_elev)
                    !
                    if( lvf_powell_elevation < lvf_useElevation)then
                      if(lvf_TierElevation_2 <= lvf_powell_elevation)then
                        ! 6.B.1 page 54
                        if(lvf_TierElevationMead_2 < lvf_mead_elev)then
                          lv_Ssr=mpf_targetRelease   
                        else
                            !
                            ! 6.B.2 page 54
                            ! 04.30.13
                              lvf_min=mpf_seven
                              lvf_max=mpf_nine
                            call balanceContent(T,lvf_min,lvf_max,lvd_balanceamount,lv_Ssr)
                            !
                        endif
                        ! 6.B.4
                        if(lvf_mead_elev < lvf_TierElevationMead_2)then
                          if(lvf_TierElevation_2 <= lvf_powell_elevation)then
                              lvf_min=mpf_targetRelease
                              lvf_max=mpf_nine
                             call balanceContent(T,lvf_min,lvf_max,lvd_balanceamount,lv_Ssr)
                          endif
                        endif
                      endif
                    else
                        ! 6.B.3 page 54 (my best interpretation- cannot match exactly i.e., 
                        !  Water Year versus calendar year and annual versus monthly issues)
                        ! 04.30.13
                        !
                        call equalizationTier(T,lvf_powell_elevation,lvf_useElevation,lvd_balanceamount,lv_Ssr)
                        !
                    endif
                !
              return
            end subroutine upperElevationTier
            ! --------------------------------

            ! ---------------------------------------------
            subroutine targetElevations(T,lvf_useElevation)
                !
                ! ------- Types --------
                real :: lvf_useElevation
                ! ======================
                !

                ! - Type Constructs -
                type(runTime)T
                ! ===================
                    !
                     if(T%year == 2008)lvf_useElevation=3636
                     if(T%year == 2009)lvf_useElevation=3639 
                     if(T%year == 2010)lvf_useElevation=3642 
                     if(T%year == 2011)lvf_useElevation=3643 
                     if(T%year == 2012)lvf_useElevation=3645 
                     if(T%year == 2013)lvf_useElevation=3646 
                     if(T%year == 2014)lvf_useElevation=3648 
                     if(T%year == 2015)lvf_useElevation=3649 
                     if(T%year == 2016)lvf_useElevation=3651 
                     if(T%year == 2017)lvf_useElevation=3652 
                     if(T%year == 2018)lvf_useElevation=3654 
                     if(T%year == 2019)lvf_useElevation=3655 
                     if(T%year == 2020)lvf_useElevation=3657 
                     if(T%year == 2021)lvf_useElevation=3659 
                     if(T%year == 2022)lvf_useElevation=3660 
                     if(T%year == 2023)lvf_useElevation=3662 
                     if(T%year == 2024)lvf_useElevation=3663 
                     if(T%year == 2025)lvf_useElevation=3664 
                     if(T%year == 2026)lvf_useElevation=3666
                !
              return
            end subroutine targetElevations
            ! -----------------------------
 
           ! ---------------------------------------------------------------
            subroutine balanceContent(T,lvf_Min,lvf_Max,lvd_balance,release)
                !
                ! ----------------------- Types ---------------------------
                integer :: i,closure

                real(8) :: release,lvd_balance,lvd_balanceIn,lvd_balanceUse
                real(8) :: lvd_holdRelease
                real(8) :: truncPowell,truncMead
                real(8) :: lvd_addWater
                real(8) :: state_Powell,state_Mead
                real, parameter :: target=8.23
                real :: lvf_Min,lvf_Max

                logical ::  lvl_pass_i
                ! =========================================================
                !

                ! -- Type Constructs -
                type(runTime)T
                ! ====================
                    !
                     release=0.
                     state_Powell=0.
                     state_Mead=0.
                     lvd_addWater=0.
                     lvd_balanceIn=0
                     lvd_balanceUse=0
                    !
                     lvl_pass_i=.false.
                    !
                        state_Powell = vState_Ba_maf(T%year)+mvf_powell_flux-target
                        state_Mead = vState_Bb_maf(T%year)+mvf_mead_flux+target
                        !
                        lvd_balanceIn=(state_Powell-state_Mead)*1/2.
                        lvd_balanceUse=MIN(lvd_balanceIn,lvd_balance)
                        !
                        closure=0
                        lvd_holdRelease=0
                    do i = 1,5000,1
                       closure=i
                       !
                       release=MAX(lvf_Min,MIN(lvf_Max,lvf_Min+lvd_balanceUse+lvd_addWater))

!                        lvd_holdRelease=release
                        !
                        state_Powell = vState_Ba_maf(T%year)+mvf_powell_flux-release
                        state_Mead = vState_Bb_maf(T%year)+mvf_mead_flux+release
                        !
                          truncPowell=0
                         truncPowell=DINT(state_Powell*1000)
                          truncMead=0
                         truncMead=DINT(state_Mead*1000)
                        ! Nearest 1000 acre-feet comparison
                        ! i.e., Powell and Mead are equilivent at +- 1000 AF
                        if(truncPowell <= truncMead)lvl_pass_i=.true.
                        !
                        if(truncPowell < truncMead)then
                            lvd_addWater=0 !lvd_addWater-0.0005
                        else
                            lvd_addWater=lvd_addWater+0.0075
                        endif
                        !
                        if(lvd_balanceUse < 0)then
                            !
                            release=MAX(lvf_Min,mpf_targetRelease+lvd_balanceUse)
                            lvl_pass_i=.true.
                        else
                           if(500 < closure)then
                            if(lvd_holdRelease < release)then
                                if(release < lvd_holdRelease+0.1)lvl_pass_i=.true.
                            else 
                                if(lvd_holdRelease-0.1 < release)lvl_pass_i=.true.
                            endif


                           endif
                           if(4995 < closure)then
                               release=max(mpf_targetRelease,min(lvf_MAX,release))
                           endif
                        endif
                        lvd_holdRelease=release

                      if(lvl_pass_i)exit
                    end do
                    !
              return
            end subroutine balanceContent
            ! -----------------------------

            ! ------------------------------------------------
            subroutine elevationsPowell(state,lvf_powell_elev)
!                                           Sum of        Mean               Approx
!           Source                    DF     Squares      Square    F Value    Pr > F
!
!           Model                      2     3304288     1652144    2.323E8    <.0001
!           Error                    338      2.4037     0.00711
!           Corrected Total          340     3304290
!
!
!                                                Approx
!                  Parameter      Estimate    Std Error    Approximate 95% Confidence Limits
!
!                  B0                382.8       0.3532       382.1       383.5
!                  B1               0.2136     0.000135      0.2134      0.2139
!                  B2               2931.0       0.3783      2930.3      2931.7
!                   y = B2 + B0* volume**B1;

!               State in units: maf
!               elevations in units: feet (msl)
!               New analyses on 04.02.12 DAS
!
!               02.27.13--- I checked, I used total volume for these analyses
!               i.e., MeadElevationFromVolume.sas



                ! ---------------- Types ---------------------
                real,parameter :: lvf_slope=0.2136
                real,parameter :: lvf_scaling=382.8
                real,parameter :: lvf_intercept=2931.0
                real,parameter :: lvp_powellMaxElevation=3710
                real :: lvf_powell_elev
                real(8) :: state
                ! =============================================
                !
                    !
                     lvf_powell_elev=0
                     mvf_powell_excess_elev=0
                    if(1.89500 <= state)then
                      if(state <= lvp_powellMax)then
                        lvf_powell_elev=(lvf_intercept + lvf_scaling*state**lvf_slope)
                      else
                        !lvf_powell_elev=(lvf_intercept + lvf_scaling*27.8659**lvf_slope)
                        lvf_powell_elev=(lvf_intercept + lvf_scaling*state**lvf_slope)

                      endif
                    else
                        lvf_powell_elev=(lvf_intercept + lvf_scaling*1.89500**lvf_slope)
                    endif
                    !
                    if(lvp_powellMaxElevation < lvf_powell_elev)then
                        mvf_powell_excess_elev=lvf_powell_elev
                        lvf_powell_elev=lvp_powellMaxElevation
                    endif
                    !
                !
              return
            end subroutine elevationsPowell
            ! -----------------------------

            ! --------------------------------------------------------
            subroutine stateFromElevationPowell(state,lvf_powell_elev)
                !
                ! -------------- Types and parameters ----------
                real,parameter :: lvf_slope=0.2136
                real,parameter :: lvf_scaling=382.8
                real,parameter :: lvf_intercept=2931.0
                real :: lvf_powell_elev
                real :: state
                ! ==============================================
                    !
                    state=((lvf_powell_elev - lvf_intercept) * (1./lvf_scaling))**(1/lvf_slope) 
                    !
                !
              return
            end subroutine stateFromElevationPowell
            ! --------------------------------------

            ! -------------------------------------------
            subroutine elevationsMead(state,lvf_mead_elev)

!
!                                           Sum of        Mean               Approx
!           Source                    DF     Squares      Square    F Value    Pr > F
!
!           Model                      2     3132617     1566309    1717356    <.0001
!           Error                    332       302.8      0.9120
!           Corrected Total          334     3132920
!
!                                                Approx
!                  Parameter      Estimate    Std Error    Approximate 95% Confidence Limits
!
!                  B0                178.1       1.4980       175.1       181.0
!                  B1               0.3420      0.00161      0.3388      0.3452
!                  B2                665.5       1.7903       662.0       669.0
!                   y = B2 + B0* volume**B1;

!               State in units: maf
!               elevations in units: feet (msl)
!              04.02.12 DAS


                ! ------------------ Types ----------------
                real,parameter :: lvf_slope=0.3420
                real,parameter :: lvf_scaling=178.1
                real,parameter :: lvf_intercept=664
                real,parameter :: lvp_meadMaxElevation=1229
                real :: lvf_mead_elev
                real(8) :: state
                ! =========================================
                !
                     lvf_mead_elev=0
                    if(2.03500 <= state)then
                      if(state <= 29.4182)then
                        lvf_mead_elev=(lvf_intercept + lvf_scaling*state**lvf_slope)
                      else
                        lvf_mead_elev=(lvf_intercept + lvf_scaling*29.4182**lvf_slope)
                      endif
                    else
                     lvf_mead_elev=(lvf_intercept + lvf_scaling*2.035**lvf_slope)
                    endif
                    !
                    if(lvp_meadMaxElevation < lvf_mead_elev)lvf_mead_elev=lvp_meadMaxElevation
                    !
                !
              return
            end subroutine elevationsMead
            ! -----------------------------

        ! ---------------------------------
        subroutine aModifyStorage_bb(T,wsb) 
            !
            ! --------------------------- Types --------------------------------------------------------------
            integer :: lvi_tract

            real(8) :: lv_State_bb,lvd_state,lv_Evap_bb
            real :: fBankStorageReach
            real(8) :: lvd_Pp,lvd_Pm
            real(8) :: lv_overflow_PowellToMead  ! Overflow from the original powersim flow variable
            real(8) :: lv_allocation_nv,lv_allocation_ca,lv_allocation_az,lv_allocation_mx
            real(8) :: vNallocation_nv,vNallocation_ca,vNallocation_az,vNallocation_mx,lv_allocation_normal
            real::     vSsmult_nv,vSsmult_az,vSsmult_mx
            real(8) :: lvd_predictedmead,lv_shortfall_b
            real(8) :: lv_Mead_Add_Flow
            real(8) :: lv_Sss
            real :: lv_SevenStatesLCshortage
            real(8) :: lvd_preCAavail,lvd_postCAavail,lvd_postMXavail
            real(8) :: lv_available
            real :: fEvaporation_reach
            real :: lvf_meadElevation,lvf_powellElevation
            real :: fEvaporation_Mead
            real :: lvf_fluxMead
            real :: lvf_check1,lvf_check2
            real :: lvi_errorLevel=0.001 ! thousandth of a percent error in allocations
            real :: lvf_bankStorage_maf
            real :: lvf_fluxPreBanked,lvf_inFlowBanked_maf,lvf_diffInFlow
            real :: lvf_channelEvaporation
            real :: state,lvf_flux
            real :: lvf_temp
            real :: lvf_diffStorage(3) ! Used for actual storage differences in bank storage estimates
            !
            real:: lvf_requestCOdeltaWater_nv,lvf_requestCOdeltaWater_ca,lvf_requestCOdeltaWater_mx
            ! The current estimate is: 158,088 acre-feet (195 million cubic meters) 
            ! Reference:  http://ibwc.state.gov/Files/Minutes/Minute_319.pdf
            !real,parameter:: lpf_COriverDeltaNeeds=158088
            real:: lvf_totalCOallocatedLB
            !
            logical :: lvl_meadFlag,lvl_normalShortage,lvl_abnormalDifference
            ! =====================================================================================================

            ! ------------------------------ Data ------------------------------------------------------------
            data vNallocation_nv,vNallocation_ca,vNallocation_az,vNallocation_mx /0.3,4.4,2.8,1.5/   
                                                              ! normal flow allocations/ designations
            !data vSsmult_nv,vSsmult_az,vSsmult_mx /0.0325,0.8,0.1675/  
            !
            ! 02.19.15 DAS
            !data vSsmult_nv,vSsmult_az,vSsmult_mx /0.0349,0.8214,0.1437/  
            ! ------------------------------------------------------------------------------------------------
            ! http:/www.drought.gov/imageserver/NIDIS/workshops/copilot/Fulp_Powell_Mead_NIDIS2008-10-01.pps slide 6
            ! and, RecordofDecision.pdf - December 2007, The Secretary of the Interior, Washington
            !      Robert Johnson, Commissioner, Bureau of Rec, and Dirk Kempthorne,Sec of the Dept of Interior
            !
            ! =====================================================================================================
!            data vSsmult_nv,vSsmult_az /0.03903904,0.96096096/  ! Revised on 05.28.10 das using CRC of NV notes from the workshop
                                        ! 1050 to 1075 elev - shortages are 13,000 AF NV and 320,000 AF AZ
                                        ! 1025 to 1050 elev - shortages are 17,000 AF NV and 400,000 AF AZ
                                        ! below 1025   elev - shortages are 20,000 AF NV and 480,000 AF for AZ         
            !
            ! ---- Type Constructs ---
            type(watershed_B)::wsb
            type(runTime)T
            ! ========================
               !
                ! 01.20.15 DAS
                 lvf_totalCOallocatedLB=vNallocation_nv+vNallocation_ca+vNallocation_az+vNallocation_mx 
                if(gvl_AZdeltaBurden)then
                  lvf_requestCOdeltaWater_nv=0
                  lvf_requestCOdeltaWater_ca=0
                  lvf_requestCOdeltaWater_mx=0
                else
                  lvf_requestCOdeltaWater_nv=gvi_WaterToCOdelta_acft_a* &
                      (vNallocation_nv/(lvf_totalCOallocatedLB+(mvf_totalCOallocatedUB*gpd_acftTomaf)))
                  lvf_requestCOdeltaWater_ca=gvi_WaterToCOdelta_acft_a* &
                      (vNallocation_ca/(lvf_totalCOallocatedLB+(mvf_totalCOallocatedUB*gpd_acftTomaf)))
                  lvf_requestCOdeltaWater_mx=gvi_WaterToCOdelta_acft_a* &
                      (vNallocation_mx/(lvf_totalCOallocatedLB+(mvf_totalCOallocatedUB*gpd_acftTomaf)))
                endif
                !
                 lvf_temp=0
                if(T%year < 2003)then
                  lvf_temp=1.4
                endif
                !
                lvl_normalShortage=.false.
                lvl_abnormalDifference=.false.
                !
                wsb%lv_AZshareCO=0.
                !
                 lv_allocation_normal=0
                lv_allocation_normal=vNallocation_nv+vNallocation_ca+vNallocation_az+vNallocation_mx
                !
                 lv_Evap_bb=0
                lv_Evap_bb=fEvaporation_Mead(vState_Bb_maf(T%year),gpf_panMead)
                !
                 lvd_Pm=0
                lvd_Pm=wsb%vState_Bb_pred_maf(T%year) 
                !
                 state=0
                state=vState_Bb_maf(T%year)
                !
                 lvd_state=0
                lvd_state=vState_Bb_maf(T%year)
                lvd_state=vState_Bb_maf(T%year)-lv_Evap_bb

                !
                 lvf_diffStorage(2)=0
                lvf_diffStorage(2)=lvd_state-lvd_Pm

                ! =========================
                if(gpl_verify)then
                  call actualStorage(T,lvd_Pp,lvd_Pm,lvf_diffStorage)
                endif
                !
                lvf_diffStorage(3)=lvf_diffStorage(2) * mpf_mod_BankStorage 
                !
                ! =========================
                !
                 mvf_meadEvaporation_acft=0
                mvf_meadEvaporation_acft=lv_Evap_bb*(1/gpd_acftTomaf)
                !
                ! Overflow from Powell
                 lv_overflow_PowellToMead=0.
                lv_overflow_PowellToMead=wsb%lv_OverFlow_ba(T%year)
                !
                ! Inflows for the Reach Powell to Mead
                 lv_Mead_Add_Flow=0.
                lv_Mead_Add_Flow=lv_overflow_PowellToMead + mvf_powellToMeadInflows_maf 
                !
                ! Flux pre-banked storage (release from powell plus added flows minus evap)
                 lvf_fluxPreBanked=0
                lvf_fluxPreBanked=wsb%lv_BaToBb(T%year) + lv_Mead_Add_Flow - lv_Evap_bb - lvf_temp
                !
                ! Flux from banked storage water in the reservoir (Mead)
                 lvf_bankStorage_maf=0
                lvf_bankStorage_maf=fBankStorageMead(lvf_diffStorage(3))
                !
                ! Flux from bank storage along the reach from Powell to Mead
                 lvf_diffInFlow=0
                if(T%startyear < T%year)then
                 lvf_diffInFlow=mvf_powellToMeadInflows_maf- lvf_powToMeadInFlowsYearMinus1(T%simyear)
                endif
                !
                 lvf_inFlowBanked_maf=0
                lvf_inFlowBanked_maf=fBankStorageReach(-lvf_diffInFlow)
                !         
                 ! 433 km long x 90 m wide = 38,970,000 m2 
                 lvf_channelEvaporation=0
                lvf_channelEvaporation=fEvaporation_reach(mpf_pan_reach)
                ! =================================================================
                    !
                    lvf_flux=0
                    lvf_flux=lvf_fluxPreBanked + lvf_bankStorage_maf + lvf_inFlowBanked_maf - lvf_channelEvaporation
                    lvf_flux=lvf_fluxPreBanked + lvf_bankStorage_maf - lvf_channelEvaporation + lvf_inFlowBanked_maf

                ! delta state
                 lv_State_bb=0.
                lv_State_bb=state + lvf_flux
                !
                 lvf_fluxMead=0
                lvf_fluxMead= lvf_flux 
                !
                ! 02.28.13 changed again (total volume)
                 lvd_predictedmead=0
                lvd_predictedmead=max(0,state+lvf_fluxMead)
                !
                lvd_predictedmead=lvd_Pm
                !

1               format(I4,1x,4(f10.6))
2               format(I4,1x,7(f10.6))
                ! To check for mass balance
                 lv_available=0.
                 lvd_preCAavail=0.
                lv_available=vState_Bb_maf(T%year)-li_meaddeadpool+lvf_fluxMead
                lvd_preCAavail=lv_available
                 !
                 lvf_meadElevation=0.
                call  elevationsMead(lvd_predictedmead,lvf_meadElevation)
                call  elevationsMead(lvd_state,lvf_meadElevation)

                 call  sevenstates(T,lvf_meadElevation,lvd_predictedmead,lv_SevenStatesLCshortage)
                 lv_Sss=lv_SevenStatesLCshortage
                 !
                 call statesShortageRatios(lv_SevenStatesLCshortage,vSsmult_az,vSsmult_nv,vSsmult_mx)
                 !
                 ! New analysis of the shortfall
                  lv_shortfall_b=0.
                 lv_shortfall_b=max(0.,-lv_available+lv_allocation_normal-lv_Sss)

                 ! 12.02.10 das new code
                 lv_Sss=lv_SevenStatesLCshortage
                 !
                 wsb%lv_AZshortageCO=lv_SevenStatesLCshortage*vSsmult_az
                !

                lv_allocation_ca=vNallocation_ca
                 lv_allocation_ca=0
                ! First Level allocations - satisfy California first
                ! ---------------------------------------------------------
                if(li_meaddeadpool < lv_State_bb)then
                    if(li_meaddeadpool < lv_State_bb-lv_allocation_ca)then
                        ! Deliver to CA first
                        vState_Bb_maf(T%year)=lv_State_bb-vNallocation_ca
                        !
                        if(vNallocation_ca < lvf_fluxMead)then
                         lv_allocation_ca=vNallocation_ca
                        else
                          lv_allocation_ca=max(0,lvf_fluxMead)
                        endif
                    else
                      vState_Bb_maf(T%year)=li_meaddeadpool
                      lv_allocation_ca=lvf_fluxMead
                    endif
                else
                  vState_Bb_maf(T%year)=li_meaddeadpool
                  lv_allocation_ca=0.
                endif
                 !
                lv_allocation_nv=0.
                lv_allocation_az=0.
                lv_allocation_mx=0.
                !
                ! lvf_fluxMead
                lvd_postCAavail=max(0., vState_Bb_maf(T%year)-li_meaddeadpool)
               ! New on 03.28.12
                if(lv_allocation_ca < lvd_preCAavail)then
                else
                 lv_allocation_ca=lvd_preCAavail
                 lvd_postCAavail=0
                endif

                if(0 < lv_Sss)then
                  if(li_meaddeadpool < vState_Bb_maf(T%year))then
                   !
                     lvd_postMXavail=0.
                    if(0 < lv_shortfall_b)then
                    !
                    lvi_tract=2
                    ! Abnormal operations of a shortage- i.e. not enough water to meet shortage allocations
                        if(0 < lvd_postCAavail)then
                         lvd_postMXavail=lvd_postCAavail
                         ! 03.24.12
                         lv_allocation_nv=max(0.,(lvd_postMXavail*vSsmult_nv))
                         lv_allocation_az=max(0.,lvd_postMXavail*vSsmult_az)
                         ! 04.04.12 added mexico back in
                         lv_allocation_mx=max(0.,(lvd_postMXavail-(lv_allocation_nv+lv_allocation_az)))
                        endif
                    else
                      lvi_tract=3
                     !
                     lvl_normalShortage=.true.
                     !
                     ! "Normal" operations of a shortage
                     ! -----------------------------------------------------------------
!                     lv_allocation_mx=vNallocation_mx 
                     ! 04.04.12 added mexico back in
                     lv_allocation_mx=max(vNallocation_mx-vSsmult_mx*lv_Sss,0.)
                     lv_allocation_nv=max(vNallocation_nv-vSsmult_nv*lv_Sss,0.)
                     lv_allocation_az=min(2.8,max(vNallocation_az-vSsmult_az*lv_Sss,0.))
                     !
                    endif
                    !
                  else
                    lvi_tract=4

                    !
                    ! Mead At dead pool
                    ! -----------------------------------------------
                    vState_Bb_maf(T%year) = li_meaddeadpool
                    !
                    lvl_abnormalDifference=.true.
                    !
!                   if(gvl_writeLog)&
!                    write(7,*)"Mead Dead Pool- line 2477 in WaterShed_CO.f90--> Tract = ", lvi_tract
                  endif
                  !
                else
                    !
                   if(lvd_postCAavail >= vNallocation_nv+vNallocation_az+vNallocation_mx)then
                     lv_allocation_nv=vNallocation_nv
                     lv_allocation_az=vNallocation_az
                     lv_allocation_mx=vNallocation_mx
                     lvi_tract=5
                   else
                     lvi_tract=6
!                     if(gvl_writeLog)&
!                      write(7,*)"Mead Error- line is 2490 in WaterShed_CO.f90--> Tract= ",lvi_tract
                   endif
                endif
                ! ---------------------------------------------------------------------------------------------------
                 !
                 if(lvl_abnormalDifference)then
                    ! Mead at Dead Pool
                 else
                  vState_Bb_maf(T%year) = vState_Bb_maf(T%year) - (lv_allocation_nv+lv_allocation_az+lv_allocation_mx)
                 endif
                 !
                 if(0 < lv_Sss)then
                   !
                    lvf_check1=lv_allocation_nv+lv_allocation_az+lv_allocation_mx
                    lvf_check2=lvd_postCAavail
                    call sCompareControl(lvf_check1,lvf_check2,lvi_errorLevel,lvl_meadFlag)
                    if(lvl_normalShortage)then

                    else
                     if(lvl_meadFlag)then
                        if(gvl_writeLog)then
                          write(7,*)T%year,"Mead Error in allocations- line 1473 in WaterShed_CO.f90"
                          write(7,*)lv_allocation_nv,lv_allocation_az,lv_allocation_mx,lvd_postCAavail,lvi_tract
                        endif
                     endif
                    endif
                else
                      !
                      if(lvp_meadMax < vState_Bb_maf(T%year))then
                       vState_Bb_maf(T%year) = lvp_meadMax !li_meadmax
                      endif
                    !
                endif
                ! ========================================================================================================
                !
                ! 01.20.15 DAS Colorado River delta water allocation of 158,088.00 from
                ! CAP MandI if available when true, otherwise, share proportionally with 
                ! the other states (plus Mexico)
                if(gvl_AZdeltaBurden)then

                else
                    lv_allocation_nv=max(0,lv_allocation_nv-(lvf_requestCOdeltaWater_nv*gpd_acftTomaf))
                    lv_allocation_mx=max(0,lv_allocation_mx-(lvf_requestCOdeltaWater_mx*gpd_acftTomaf))
                    lv_allocation_ca=max(0,lv_allocation_ca-(lvf_requestCOdeltaWater_ca*gpd_acftTomaf))
                    !
                    go_COdeltaWater_nv=nint(lvf_requestCOdeltaWater_nv*(lv_allocation_nv/vNallocation_nv))
                    go_COdeltaWater_ca=nint(lvf_requestCOdeltaWater_ca*(lv_allocation_ca/vNallocation_ca))
                    go_COdeltaWater_mx=nint(lvf_requestCOdeltaWater_mx*(lv_allocation_mx/vNallocation_mx))
                    !
                endif
                !
                lvf_powellElevation=0.
                 go_powellElevation=0
                call  elevationsPowell(vState_Ba_maf(T%year),lvf_powellElevation)
                go_powellElevation=nint(lvf_powellElevation)

                 lvf_meadElevation=0.
                 go_meadElevation=0
                call  elevationsMead(vState_Bb_maf(T%year),lvf_meadElevation)
                go_meadElevation=nint(lvf_meadElevation)
                !
                if(gpl_validate)then
                    call actualStorage(T,lvd_Pp,lvd_Pm,lvf_diffStorage)
                    call  elevationsPowell(lvd_Pp,lvf_powellElevation)
                    call  elevationsMead(lvd_Pm,lvf_meadElevation)
                else
                    call  elevationsPowell(vState_Ba_maf(T%year),lvf_powellElevation)
                    call  elevationsMead(vState_Bb_maf(T%year),lvf_meadElevation)
                    if(T%year < 2012)then
                    endif
                endif
                ! -----------------------------------------------------

                ! ----------------------------------------------------------------------------------------------------
                !
                wsb%lv_AZshareCO=lv_allocation_az
              !
            return
        end subroutine aModifyStorage_bb
        !-------------------------------
        
        ! ------------------------------
        subroutine statesShortageRatios(shortage,az,nv,mx)
            !
            ! --------- Types ----------
            !real:: shortage_1=0.383
            real:: shortage_2=0.487
            real:: shortage_3=0.625
            real:: shortage
            real:: az,nv,mx
            ! ==========================
                !             
                  az=1
                  nv=1
                  mx=1
                !
                ! NOTE: values (decimal place) chosen to remove rounding errors
                ! i.e., 13k & 320k, 17k & 400k, and 20k & 480k for NV and AZ, respectively
                if(0 < shortage)then
                    if(shortage < shortage_2)then
                      az=0.835510
                      nv=0.033943
                    else if(shortage < shortage_3)then
                      az=0.821357
                      nv=0.034908
                    else  
                      az=0.768
                      nv=0.032
                    endif
                      mx=1.0-az-nv
                endif
                !
         return
        end subroutine statesShortageRatios

        ! ---------------------------------------------------------
        subroutine sevenstates(T,lvf_meadElevation,lv_PMead,lv_SevenStatesLCshortage)
                !
                ! Live is "active"
                !
                ! Heretofore I was simulating with live volumes for the parameters, but
                ! modeling total, so I threshold volumes are off...
                !
!              real(8) :: vMeadVolThresh_2=4.33  ! Mead < 1,025 feet
!              real(8) :: vMeadVolThresh_3=5.8   ! Mead at 1,025 feet
!              real(8) :: vMeadVolThresh_4=7.47  ! mead at 1,050 feet 
!              real(8) :: vMeadVolThresh_5=9.37  ! mead at 1,075 feet (live, not total)
              ! 04.02.12 DAS


            ! ------------------ Types ----------------------------------------
            real(8) :: vMeadVolThresh_2=7.83602  ! Mead < 1,025 feet
            real(8) :: vMeadVolThresh_3=7.83602   ! Mead at 1,025 feet
            real(8) :: vMeadVolThresh_4=9.5059 ! mead at 1,050 feet 
            real(8) :: vMeadVolThresh_5=11.4049  ! mead at 1,075 feet (total)
              !
            real,parameter :: lvp_StepOne=1075
            real,parameter :: lvp_StepTwo=1050
            real,parameter :: lvp_StepThree=1025
              !
            real(8) :: lv_PMead
            real :: lv_SevenStatesLCshortage
            real :: lvf_meadElevation
            logical :: lvl_mass=.false.
            ! ==================================================================
            !

            ! - Type Construct -
            type(runTime)T
            ! ==================
                !           
                 lv_SevenStatesLCshortage=0.
                !
               if(lvl_mass)then
                 if(lv_PMead < vMeadVolThresh_2)then
                   lv_SevenStatesLCshortage=0.6
                   lv_SevenStatesLCshortage=0.625 ! w/ mexico
                 else
                   if(lv_PMead < vMeadVolThresh_3)then
                    lv_SevenStatesLCshortage=0.5 ! w/o mexico
                    lv_SevenStatesLCshortage=0.6
                     ! 02.19.15
                    lv_SevenStatesLCshortage=0.625 ! w/ mexico
                   else
                    if(lv_PMead < vMeadVolThresh_4)then
                     lv_SevenStatesLCshortage=0.417 ! w/o mexico
                     lv_SevenStatesLCshortage=0.5
                     ! 02.19.15
                     lv_SevenStatesLCshortage=0.487 ! w/ mexico

                    else
                     if(lv_PMead < vMeadVolThresh_5)then
                      lv_SevenStatesLCshortage=0.333 ! w/o mexico
                      lv_SevenStatesLCshortage=0.4
                        ! 02.19.15 AZ=320k, nv=13k, mx=50k
                      lv_SevenStatesLCshortage=0.383 ! w/ mexico
                     else
                      lv_SevenStatesLCshortage=0.          
                     endif
                    endif
                   endif
                 endif               
               else
                if(lvf_meadElevation <=lvp_StepThree)then
                   lv_SevenStatesLCshortage=0.6
                   lv_SevenStatesLCshortage=0.625
                else
                    if(lvf_meadElevation <=lvp_StepTwo)then
                     lv_SevenStatesLCshortage=0.5
                     lv_SevenStatesLCshortage=0.487
                    else
                        if(lvf_meadElevation <=lvp_StepOne)then
                          lv_SevenStatesLCshortage=0.4
                          lv_SevenStatesLCshortage=0.383 
                        else
                          lv_SevenStatesLCshortage=0.
                        endif
                    endif
                endif
               endif
              !
           return
        end subroutine sevenstates
        !-------------------------

        ! ------------------------------
        subroutine designations_B(T,wsb)
            !
            ! ------------- Types ------------------
            real(8) :: vPreCap
            real :: lv_onRiver,cv_onRiver=1.2
            real(8) :: lv_CAPwater(gpi_lBY:gpi_uBY)
            real :: lv_capcapacity=2.17
            real :: lv_capmaricopashare=0.75
            ! ======================================
            !

            ! --- Type Constructs ---
            type(watershed_B)::wsb
            type(runTime)T
            ! =======================
                !
                 ! Tim's original code
    !              vYuma=min(wsb%lv_AZshareCO/2,1.2)
    !              vPreCap=wsb%lv_AZshareCO-vYuma
                  !
                  !   Added on 06.09.10 based on a talk last night by Dee Fuerst at the monthly
                  ! AHS meeting in PHX- CAP has to take 90% of the AZ shortage.. On-river take 10%
                  lv_onRiver=min(wsb%lv_AZshareCO,cv_onRiver-(0.1*wsb%lv_AZshortageCO))
                  vPreCap=max(wsb%lv_AZshareCO-lv_onRiver,0)
                  !
                  ! 08.20.15 I removed evaporation from these analyses
                  ! Now, 1.6 maf is delivered
                 if(vPreCap <= lv_capcapacity)then
                    lv_CAPwater(T%year)=vPreCap
                 else
                    lv_CAPwater(T%year)=lv_capcapacity  
                 endif
                !               
                wsb%lv_CAPmaricopa(T%year)=0.
                wsb%lv_CAPmaricopa(T%year)=lv_CAPwater(T%year)*lv_capmaricopashare
                wsb%lv_CAP(T%year)=lv_CAPwater(T%year)
                !
                 go_CAP=0
                go_CAP=nint(lv_CAPwater(T%year)*1e6)
            !
          return
         end subroutine designations_B
        !-----------------------------

        ! ---------------------------------
        subroutine updateState_b(T,Bout,wsb)
            !
            ! ------------- Types -----------
            real :: lvf_mead_acft=13647000
            real :: lvf_powell_acft=12692000
            ! ===============================
            !

            ! -- Type Construct --
            type(runTime)T
            type(RiverB)Bout
            type(watershed_B)::wsb
            ! ====================
                !
                if(gpl_comparisons)then
                        !  
                        vState_Ba_maf(2012)=(lvf_powell_acft/1e6)+mpf_powellDead
                        vState_Bb_maf(2012)=(lvf_mead_acft/1e6)+mpf_meadDead
                        !
                        if(gpl_verify .or. gpl_validate)then
                          if(T%year <=2012)then
                            write(100,*)T%year, mvf_inflow_acft,mvf_LeeFerry_acft,mvf_upperBasin_acft,mvf_releaseFromPowell_acft,&
                            mvf_powellEvaporation_acft,mvf_meadEvaporation_acft,go_powellElevation,go_meadElevation, &
                            vState_Ba_maf(T%year),vState_Bb_maf(T%year)
                          endif
                        else
                          if(2011 < T%year)then
                            write(100,*)T%year, mvf_inflow_acft,mvf_LeeFerry_acft,mvf_upperBasin_acft,mvf_releaseFromPowell_acft,&
                            mvf_powellEvaporation_acft,mvf_meadEvaporation_acft,go_powellElevation,go_meadElevation, &
                            vState_Ba_maf(T%year),vState_Bb_maf(T%year)
                          endif
                        endif
                        !            
                endif
                    !
                    vState_Ba_maf(T%year+1)=vState_Ba_maf(T%year)
                    vState_Bb_maf(T%year+1)=vState_Bb_maf(T%year)
                     vState_B_maf(T%year)=0
                    vState_B_maf(T%year)=vState_Ba_maf(T%year)+vState_Bb_maf(T%year)
                    !
                      Bout%bFlow=0
                     Bout%bFlow=lv_flow_B_maf(T%year)
                      Bout%bCAP_maf=0
                     Bout%bCAP_maf=wsb%lv_CAP(T%year)
                     !
                     ! Send to interface
                      go_AZshareCO=0.
                     go_AZshareCO=nint(wsb%lv_AZshareCO* (1./gpd_acftTomaf))
                     !
                !
            return
        end subroutine updateState_b
        ! --------------------------

        ! -----------------------
        subroutine outinitialB(T) 
            !
            ! - Type Construct -
            type(runTime)T
            ! ==================
            !
                !
                if(T%year /= T%startyear)then
                else
                endif
                !
            !
         return
        end subroutine outinitialB
       ! --------------------------

        ! --------------------
        subroutine outputsB(T)
            !
            ! - Type Construct -
            type(runTime)T
            !===================
            !
                ! Send to the interface
                ! ---------------------
                go_StateCO=NINT((vState_Ba_maf(T%year)+vState_Bb_maf(T%year))*(1./gpd_acftTomaf))
                 go_StatePowell=0.
                go_StatePowell=NINT(vState_Ba_maf(T%year)* (1./gpd_acftTomaf))
                 go_StateMead=0.
                go_StateMead=NINT(vState_Bb_maf(T%year)* (1./gpd_acftTomaf))
                !
                if(T%year .EQ. T%startyear)then
                    if(gvl_writeLog)then
                        string=4
                        LU=0
                        call sOnceThrough(string,streamString)
                        call sWrite(streamString,LU)
                    endif
                endif
                !
            !
          return
        end subroutine outputsB
       ! ----------------------
!
End Module lms_River_Ba
!
   ! -----------------------------------------------
    subroutine pFlowsReservoirsColorado(T,acft,Bout)
      use lms_River_Ba
        !
        ! ----- Types -----
        integer :: acft
        !==================
        !

        ! ---- Type Constructs ---
        type(RiverB)Bout
        type(watershed_B)::wsb
        type(runTime)T
        ! ========================
           !
           IF(gvl_errorFlag)THEN
            call aFlow_b(T,acft)        ! Read historical data
            call aModifyFlow_ba(T)      ! Drought,climate, inflows adjustments
!            call aModifyFlow_bb(T)      ! Water for the Environment 
            call aModifyFlow_bc(T)      ! Upper Basin water use removed
            call reservoirs_b(T,wsb)
             !
            call updateState_b(T,Bout,wsb)
            call outinitialB(T)
            call outputsB(T)
           ELSE
                if(gvl_writeLog)then
                    string=9
                    LU=0
                    call sStrings(string,errorString)
                    call eWrite(errorString,LU)
                endif
             gvl_errorFlag=.false.
           ENDIF
          !
      return
    end subroutine pFlowsReservoirsColorado
   !---------------------------------------
!
! ======================================================================================================
! E.O.F. WaterShed_CO.f90
