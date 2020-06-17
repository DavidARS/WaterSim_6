!
! File is DailyDesignations.f90
!
! this file determines the provider specific normal flow designations from daily runoff estimates
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

! Module:       Module lm_DesignationsDaily_A       
! Subroutines:  subroutine initAdesignationsDaily()
!                 calls (herein):
!                   call openFiles_AdesignationsD()
!                   call readFiles_AdesignationsD()
!
! Module:       Module lms_DesignationsDaily_A     
! Subroutines:  subroutine extract(T)
!               subroutine flow(i,lvf_modClimateDrought)
! Function:     function fmodFlowSVTdaily(T)
! Subroutine:   subroutine designations(T,gvd_ClassAarray)
! Function:     function leapYear(yr)
! Subroutine:   subroutine extractDaily(T,gvd_ClassAarray)
!

!
! created on 01.09.12
!
! david arthur sampson

! last write was: 01.18.13,07.21.14
! ---------------------------------
!

! ======================================================================================================
!
Module lm_DesignationsDaily_A
 use gm_ModelControl
  use gm_GlobalData
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
 contains
    !
    ! ------------------------------------
    subroutine initAdesignationsDaily()
      call openFiles_AdesignationsD()
      call readFiles_AdesignationsD()
     return
    end subroutine initAdesignationsDaily
    ! ------------------------------------

      ! ---------------------------------------
      subroutine openFiles_AdesignationsD()
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
          module="lm_DesignationsDaily_A"
          !
           Infile='App_Data\Parameters\SVT_Designations_Daily.txt'; LU=45
           call openFiles(module,lvc_DPath,Infile,LU)

           Infile='App_Data\Parameters\SVT_Threshold_Daily.txt'; LU=46
           call openFiles(module,lvc_DPath,Infile,LU) 
        return
10      continue
                if(gvl_writeLog)then
                    string=26
                    LU=0
                    call sStrings(string,errorString)
                    call eWrite(errorString,LU)
                endif
             gvl_errorFlag=.false.
            !
       end subroutine openFiles_AdesignationsD
      ! --------------------------------------

      ! -----------------------------------
      subroutine readFiles_AdesignationsD()
            ! ------------------------------- Types  -------------------------------------
!            integer :: LU
            integer :: i,j,ios
!            real, parameter :: lvf_trottConstant=5.282934211 ! this is the AF per acre 
!                                                               entitled on an annual basis
            real(8) :: lvd_dailyDesignationsSVT(42,10)        !
            real(8) :: lvd_dailyThresholdSVT(42,2)
            ! ================================================================================
            !
            read(45,*,err=5,iostat=ios)((lvd_dailyDesignationsSVT(i,j),j=1,10),i=1,42)
            read(46,*,err=6,iostat=ios)((lvd_dailyThresholdSVT(i,j),j=1,2),i=1,42)
            !
             lid_dailyDesignationsSVT=0
            lid_dailyDesignationsSVT=lvd_dailyDesignationsSVT
             lid_dailyThresholdSVT=0.
            lid_dailyThresholdSVT=lvd_dailyThresholdSVT
            !
5          continue
            if(ios >0)then
             LU=45
             goto 1000
            endif
6          continue
            if(ios >0)then
             LU=46
             goto 1000
            endif
            ! NOTE: not closed if an err is returned
            do i = 45,46,1
             close(i)
            end do
            !
          return
1000     continue
            !
            if(gvl_writeLog)then
              string=19
                call sStrings(string,errorString)
                call eWrite(errorString,LU)
            endif
          gvl_errorFlag=.false.
          !
      end subroutine readFiles_AdesignationsD
      ! -------------------------------------
End Module lm_DesignationsDaily_A
!
    ! ---------------------------------
    subroutine initializeDailyDesig()
      use lm_DesignationsDaily_A
        !
        call initAdesignationsDaily()
        !
      return
    end subroutine initializeDailyDesig
    ! ---------------------------------
!
! ======================================================================================================
!
Module lms_DesignationsDaily_A
 use gm_ModelControl
  use gm_GlobalData
    use gm_TypeControl
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !

        ! ---- Module Global Types ---------------
        integer, private :: yr,doy,iRecStart
        real,private  :: SVTflow_acft(gpi_dailySVT)
        integer, private :: gvi_Validate=1
        ! =============================================
    !
 contains
    !--------------------
    subroutine extract(T)
        ! 
        ! ----------- Types ------------
        integer :: i
        integer :: lvi_start
        real :: lvf_modClimateDrought
        real :: cf
        logical :: lvl_pass=.true.
        ! ===================================

        ! - Type Construct ---
        type(runTime)T
        ! =========================
            !
            !  This will have to be incremented each year
            !  flow in units cfs
            lvl_pass=.true.
            if(T%year == T%Startyear)gvi_countYearsSVT=0
            ! Drought considered first, now modify using the climate factor
            call ClimateFactorSVT(T%simyear+1,cf)
                 !
                ! Standard simulations but use 2000 through 2010 flow record always
                if(T%year < 2013)then
                 lvi_start=2000
                else
                 if(T%year < 2014)gvi_countYearsSVT=0
                 lvi_start=gv_indexyearSVT
                  if(1 < gv_dataSVT)lvi_start=1981
                    !
                    if(gvi_SVTtrace-1 < gvi_countYearsSVT)gvi_countYearsSVT=0
                    !
                endif
                  !
                  do i = 1,gpi_dailySVTrec,1
                    gvf_flowSVT(i,3)=0
                    if(nint(gvf_flowSaltTonto(i,1)) == lvi_start+gvi_countYearsSVT)then
                        if(lvl_pass)iRecStart=i
                        lvf_modClimateDrought=fmodFlowSVTdaily(T)*cf
                        !
                        call flow(i,lvf_modClimateDrought)
                        lvl_pass=.false.
                    endif
                  end do
            !
        return
     end subroutine extract
     ! --------------------
    
     ! --------------------------------------
     subroutine flow(i,lvf_modClimateDrought)
        ! ------------ Types -----------
        integer :: i
        real :: lvf_modClimateDrought
        ! ===================================
            !
            ! Using cfs for designations below
            ! ---------------------------------------------
            gvf_flowSVT(i,1)= gvf_flowSaltTonto(i,1) ! year
            gvf_flowSVT(i,2)= gvf_flowSaltTonto(i,2) ! day of year (doy)
            gvf_flowSVT(i,3)=(gvf_flowVerde(i,3)+gvf_flowSaltTonto(i,3))*lvf_modClimateDrought
      return
     end subroutine flow
     ! --------------------

    ! --------------------------
    function fmodFlowSVTdaily(T)
        ! ----------- Types --------
        real :: fmodFlowSVTdaily
        real :: lvf_droughtFactor
        ! ===============================

        ! - Type Constructs-
        type(runTime)T
        ! =======================
        !
           lvf_droughtFactor=1.0
         if(T%year >= gv_droughtyearSVT)then
          if(T%year <  gv_droughtyearendSVT)then
            lvf_droughtFactor=gv_droughtfactorSVT
          endif
         endif
            !
             fmodFlowSVTdaily= lvf_droughtFactor 
            !
    end function fmodFlowSVTdaily
    ! ---------------------------

    ! --------------------------------------
    subroutine designations(T,gvd_ClassAarray)
        !
        ! --------------------------- Types --------------------------------------------------
        integer :: i,j,k,h !,count
        integer(1) :: lvi_vList(42)
        integer :: lvi_addDays,lvi_countDays
        integer :: lvi_normalFlowAvailable(10)

        logical :: lvl_start=.true.
        real(8) :: gvd_ClassAarray(10)
        !
        real ::  lvf_targetDesignationsD(10),lvf_flowDifference(42)
        real :: lvf_rightsNextTrott(10),lvf_addDailyDesignations(10),lvf_addThresholdDiff(10)
        real :: lvf_trottDifference,lvf_differenceToThreshold, lvf_flow
        real :: lvf_sumTrott,lvf_sumTrottPlus1
        real :: fModifyNormalFlow
        real :: lvf_modifyNormalFlow(10)
        real :: lvf_normalFlow(10),lvf_out(10)
        integer :: lvi_max(10)
        real(8),parameter :: lpf_ft3ToAcrefeet=1.983471074380165 != (3600*24)/43560 to convert cfs to acre-feet
        ! ===========================================================================================
        !
    
        !--- TYPE constructs ---
        type(runTime)T
        ! ===========================
        !
            do i = 1,42,1
             lvi_vList(i)=1
            end do
            do j = 1,10,1
             lvf_addDailyDesignations(j)=0.
             lvi_normalFlowAvailable(j)=0.
             lvf_modifyNormalFlow(j)=0
             lvf_out(j)=0.9
            end do
            do i = 1,gpi_dailySVTrec,1
             SVTflow_acft(i)=0.
            end do
            !
            lvi_max=gvi_NormalFlowEmpMax_AFac
            !
            call sModifyNormalFlow(T,lvi_max,gvf_modifyNormalFlow,lvf_out)      
                lvf_normalFlow=lvf_out
!                  lvf_normalFlow=gvf_modifyNormalFlow
                    if(1 < gvi_validate)then

                      ! This variable -lvf_modifyNormalFlow(10) - gets written into memory
                      ! for use when T%year > T%Startyear
                      do j = 1,10,1
                         lvf_modifyNormalFlow(j)=0
                        lvf_modifyNormalFlow(j)=fModifyNormalFlow(lvf_normalFlow(j))
                      end do
                   endif
             ! Find the number of days in the year; Run the loop for one year each day
             lvi_addDays=leapYear(gvf_flowSaltTonto(iRecStart,1))
             lvi_countDays=0
            do h = iRecStart,iRecStart+lvi_addDays-1,1
                lvi_countDays=lvi_countDays+1
                yr =gvf_flowSVT(h,1)
                doy=gvf_flowSVT(h,2)
                lvf_flow=gvf_flowSVT(h,3)
                SVTflow_acft(h)=lvf_flow*gpf_ft3sToAcftDay
                !

                    ! If flow is greater than max, set the threshold designations
                    ! Units MUST be cfs
                    ! -----------------------------------------------
                    if(lvf_flow > lid_dailyThresholdSVT(42,1))then
                        do k = 1,10,1
                             lvf_targetDesignationsD(k)=0
                            lvf_targetDesignationsD(k)= lid_dailyDesignationsSVT(42,k)
                            !
                            lvf_addDailyDesignations(k)=lvf_addDailyDesignations(k)+ lvf_targetDesignationsD(k)
                            !
                            lvf_addThresholdDiff(k)=0
                                !
                                    if(1 < gvi_validate)then
                                    ! output for the interface- have not tried this type of model-interface passing.
                                    ! 02.07.12,02.08.12 DAS
                                      if(0 < lvf_modifyNormalFlow(k))then
                                        lvi_normalFlowAvailable(k)=nint(((lvf_targetDesignationsD(k)+lvf_addThresholdDiff(k))*(1./ lvf_modifyNormalFlow(k)))*gpf_ft3sToAcftDay)
                                      endif
                                    !                         
                                        if(.not. gpl_release)then
                                          if(lvl_start)then
                                            write(5,*)" Year "," Provider "," Normal Flow: AF day-1"
                                            write(5,*)T%year,k,lvi_normalFlowAvailable(k)
                                          else
                                            write(5,*)T%year,k,lvi_normalFlowAvailable(k)
                                          endif
                                         lvl_start=.false.
                                        endif
                                    endif
                                !
                        end do  
                     else
                        ! Find the threshold flow limits and designations
                        ! 03.05.12 changed this from j=1,42,1 to j = 2,42,1
                        do j=2,42,1   
                            if(lvf_flow <= lid_dailyThresholdSVT(j,1))lvi_vList(j)= -1
                            if(lvi_vList(j) < 1)then
                                lvf_flowDifference(j)=0
                                
                                if(lid_dailyThresholdSVT(j-1,1) < lvf_flow)then
                                    lvf_flowDifference(j)=lvf_flow-lid_dailyThresholdSVT(j-1,1)
                                endif
                                !
                                lvf_sumTrott=0. ;lvf_sumTrottPlus1=0.
                                do i = 1,10,1
                                    lvf_sumTrott=lvf_sumTrott+lid_dailyDesignationsSVT(j-1,i)
                                    lvf_sumTrottPlus1=lvf_sumTrottPlus1+lid_dailyDesignationsSVT(j,i)
                                end do
                                !
                                 lvf_trottDifference=0
                                lvf_trottDifference=lid_dailyThresholdSVT(j,1)-lid_dailyThresholdSVT(j-1,1)
                                 lvf_differenceToThreshold=0
                                if(0 < lvf_trottDifference)lvf_differenceToThreshold=lvf_flowDifference(j)/lvf_trottDifference
                                !
                                do k = 1,10,1
                                    lvf_targetDesignationsD(k)=0.
                                    lvf_rightsNextTrott(k)=0.
                                    !
                                    ! Set the normal flow designation (cfs day-1)
                                    lvf_targetDesignationsD(k)=lid_dailyDesignationsSVT(j-1,k)
                                    ! providerTrott() found in Designations_SVT.f90 [listed in the include file]
                                    lvf_rightsNextTrott(k)=lid_providerTrott(j-1,k)  
                                    !
                                     lvf_addThresholdDiff(k)=0.
                                    lvf_addThresholdDiff(k)=lvf_rightsNextTrott(k)*(lid_dailyThresholdSVT(j,2)*lvf_differenceToThreshold) &
                                        *(lvf_sumTrottPlus1-lvf_sumTrott)
                                    !
                                    ! Accumulate daily over the year
                                    lvf_addDailyDesignations(k)=lvf_addDailyDesignations(k)+ lvf_targetDesignationsD(k)+lvf_addThresholdDiff(k)
                                    !
                                    if(1 < gvi_validate)then
                                    ! output for the interface- hav not tried this type of model-interface passing.
                                    ! 02.07.12,02.08.12 DAS
                                      if(0 < lvf_modifyNormalFlow(k))then
                                        lvi_normalFlowAvailable(k)=nint(((lvf_targetDesignationsD(k)+lvf_addThresholdDiff(k))*(1./ lvf_modifyNormalFlow(k)))*gpf_ft3sToAcftDay)
                                      endif
                                    !                    
                                        if(.not. gpl_release)then
                                          if(lvl_start)then
                                            write(5,*)" Year "," Provider ","Water Year"," Normal Flow: AF day-1"
                                            write(5,*)T%year,k,yr,lvi_normalFlowAvailable(k)
                                          else
                                            write(5,*)T%year,k,yr,lvi_normalFlowAvailable(k)
                                          endif
                                         lvl_start=.false.
                                        endif 
                                    endif     
                                end do ! k = 1,10 of the SRP members
                            endif ! lvi_vList < 1
                                if(lvi_vList(j)< 1)exit
                        end do ! 1 to 42 of threshold
                    endif !  flow > thresholddesig(42)
                do i = 1,42,1
                    lvi_vList(j)= 1
                end do
            end do ! file record loop (individual days)
            !
                ! ---------------------
                do i = 1,10,1
                   gvd_ClassAarray(i)=0
                  gvd_ClassAarray(i)=lvf_addDailyDesignations(i)*lpf_ft3ToAcrefeet*gpd_acftTomaf
                end do
            !
      return
    end subroutine designations
    ! -------------------------
    ! DAS
    ! -------------------
    function leapYear(yr)
     integer :: year
     real :: leapYear,yr
        !
        year = yr
        leapYear=365
	    if(year == 1948 .OR. year == 1952 .OR. year == 1956 .OR. year == 1960 .OR. year == 1964 & 
            .OR. year == 1968 .OR. year == 1972 .OR. year == 1976 .OR. year == 1980 .OR. year == 1984 &
            .OR. year == 1988 .OR. year == 1992 .OR. year == 1996 .OR. year == 2000 .OR. year == 2004 &
	        .OR. year == 2008 .OR. year == 2012)then;
            leapYear=366
        endif
        !
    end function leapYear
    ! -------------------

!http://aa.usno.navy.mil/faq/docs/JD_Formula.php
End Module lms_DesignationsDaily_A


! ----------------------------------------
subroutine extractDaily(T,gvd_ClassAarray)
 use lms_DesignationsDaily_A
        ! ----- Types -------------
        real(8) :: gvd_ClassAarray(10)
        ! ==============================
        !

        ! - Type Construct -
        type(runTime)T
        ! =======================
            !
            call extract(T)
            call designations(T,gvd_ClassAarray) ! maf
            !
        !
    return
end subroutine extractDaily
! -------------------------
!
! ======================================================================================================
! E.O.F.