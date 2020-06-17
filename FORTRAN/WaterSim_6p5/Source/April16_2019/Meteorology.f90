!
! Infile is Meteorology.f90
!
! This module reads meteorological data into common memory
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

! Initialized in subroutine InitK() which is found in Kernel.f90
!----------------------------------------------------------------
!
! Module:       Module lm_Meteorology    
! Subroutines:  subroutine initMeteo()
!                 calls (also present):
!                   call openFiles_met()
!                   call readFiles_met()
!
! No Module:    subroutine initializeMeteo()
!
!
! created on 11.19.10
!
! david arthur sampson

! last write was: 01.16.13,07.21.14
! ---------------------------------
!

! ======================================================================================================
!
Module lm_Meteorology
 use gm_ModelControl
   use gm_GlobalData
   
   implicit none
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
  contains
    !
    ! -------------------------------
    subroutine initMeteo()
!     if(.not. gpl_release)gvl_IncludeMeteorology=.true.
      if(gvl_IncludeMeteorology)then
        call openFiles_met()
        call readFiles_met()
      endif
     return
    end subroutine initMeteo
    ! ------------------------------

    ! ------------------------
    subroutine openFiles_met()
        !
        ! ------------- Types --------------
         character(len=200) :: lvc_DPath=' '
        ! ==================================
        !
            if(gpl_release)lvc_DPath=trim(gvc_DPath)
            !
            module="lm_Meteorology"
            ! Sustainable Futures Scenarios Initiative
            ! 11.30.15 DAS
            Infile='App_Data\Data\metData.txt'; LU=70
            call openFiles(module,lvc_DPath,Infile,LU)

            Infile='App_Data\Data\metDataProjected.txt'; LU=71
            call openFiles(module,lvc_DPath,Infile,LU)
            !
            !Infile='App_Data\Data\rainFall_60.txt'; LU=72
            Infile='App_Data\Data\rainFall_60_35.txt'; LU=72
            call openFiles(module,lvc_DPath,Infile,LU)
        !
      return
    end subroutine openFiles_met
    ! --------------------------

        ! ------------------------
        subroutine readFiles_met()
            ! ------------------------------ Types ---------------------------------
            integer :: ios,i,j,k,lvc_length=1044 !,lvc_length=1200
            integer :: lvi_year,lvi_month
            integer :: lvc_years=2060,provider
            integer :: lvc_start=2000
            !integer :: lvc_providersMet=33
            integer :: lvc_providersMet=35

!            real :: lif_MetData(gpi_lBY:gpi_uBY,12,3),lvf_year,lvf_month,lvf_Tmax,lvf_Tmin
            real :: lif_MetData(gpi_lBY:2100,12,3),lvf_year,lvf_month,lvf_Tmax,lvf_Tmin
            real :: lvf_Precip
            real :: lif_MetDataProjected(gpi_lBY:gpi_uBY,12,3)
            real :: lif_MetDataProbability(gpi_lBY:gpi_uBY,gvi_maxProV,3)
            real :: lvf_average(gpi_lBY:gpi_uBY,2),one,two
            real :: lvf_rainFallSeed,lvf_tempArray(gvi_maxProV)
          ! ========================================================================
          ! 
            !The Sustainable Futures Scenarios Initiative special projects - read Tmax,Tmin, and 
            ! Precipitation from RCP 8.5 given to me by David Iwaniec on 11.30.15 
            ! 11.30.15 das unit 70
            ! ==========================================================================================
            !
           
            LU=70
            ! This should be a "do while" construct
            do i = 1,lvc_length,1
             lvf_Tmax=0 
             lvf_Tmin=0
             lvf_Precip=0
             lvi_year=0
             lvi_month=0
             lvf_year=0
             lvf_month=0
             !
               read(LU,*,err=10,iostat=ios)lvf_year,lvf_month,lvf_Tmax,lvf_Tmin,lvf_Precip
               lvi_year=lvf_year
               lvi_month=lvf_month
               lif_MetData(lvi_year,lvi_month,1)=lvf_Tmax
               lif_MetData(lvi_year,lvi_month,2)=lvf_Tmin
               lif_MetData(lvi_year,lvi_month,3)=lvf_Precip
            end do
            !
            close(LU)
            do i = gpi_lBY,gpi_uBY,1
                do j = 1,12,1
                  do k = 1,3,1
                    lvf_MetData(i,j,k)=0.
                  end do
                end do
            end do
              !
            do i = gpi_lBY,gpi_uBY,1
                do j = 1,12,1
                  do k = 1,3,1
                   lvf_MetData(i,j,k)= lif_MetData(i,j,k)
                  end do
                end do
            end do
            !
          ! ========================================================================================
          LU=71
          ! THis should be a "do while" construct
            ! Write to the common block
            do i = 1,lvc_length,1
               read(LU,*,err=10,iostat=ios)lvi_year,lvi_month,lvf_Tmax,lvf_Tmin,lvf_Precip
               lif_MetDataProjected(lvi_year,lvi_month,1)=lvf_Tmax
               lif_MetDataProjected(lvi_year,lvi_month,2)=lvf_Tmin
               lif_MetDataProjected(lvi_year,lvi_month,3)=lvf_Precip
            end do
            !
            close(LU)
            do i = gpi_lBY,gpi_uBY,1
             do j = 1,12,1
              do k = 1,3,1
                lvf_MetDataProjected(i,j,k)=0.
              end do
             end do
           end do
          !
            one=0
            two=0
          do i = gpi_lBY,gpi_uBY,1
            do j = 1,12,1
              do k = 1,3,1
               lvf_MetDataProjected(i,j,k)= lif_MetDataProjected(i,j,k)
              end do
             one=one+(lvf_MetDataProjected(i,j,1))
             two=two+(lvf_MetDataProjected(i,j,2))

            end do
            lvf_average(i,1)=one/12
            lvf_average(i,2)=two/12
          end do
          !
         ! ========================================================================================
          LU=72
          ! 
            ! Write to the common block
            do i = gpi_lBY,gpi_uBY,1
             do j = 1,gvi_maxProV,1
              do k = 1,3,1
                 lif_MetDataProbability(i,j,k)=0
              end do
             end do
           end do
           read(LU,*,err=10,iostat=ios)((lif_MetDataProbability(i,j,3),j=1,lvc_providersMet),i=2000,2060)      
            do i = lvc_start,lvc_years,1
             do provider = 1,gvi_maxProV,1
               lif_MetDataProbability(i,provider,1)=0
               lif_MetDataProbability(i,provider,2)=0
             end do
            end do
            !
            close(LU)
            do i = lvc_start,lvc_years,1
             do j = 1,gvi_maxProV,1
              do k = 1,3,1
                lvf_MetDataProbability(i,j,k)=0.
              end do
             end do
           end do
          !
          do i = lvc_start,lvc_years,1
            do j = 1,gvi_maxProV,1
              !do k = 1,3,1
!              if( j == 22 .or. j == 23)then
!              else
!               lvf_MetDataProbability(i,j,1)=  lvf_average(i,1)
!               lvf_MetDataProbability(i,j,2)=  lvf_average(i,2)
!               lvf_MetDataProbability(i,j,3)=  lif_MetDataProbability(i,j,3)
!if(i == 2000)write(103,*)j,lvf_MetDataProbability(i,j,3)             
!              endif
                !
               lvf_MetDataProbability(i,j,1)=  lvf_average(i,1)
               lvf_MetDataProbability(i,j,2)=  lvf_average(i,2)
               lvf_MetDataProbability(i,j,3)=  lif_MetDataProbability(i,j,3)
            end do
          end do
          !lvf_rainFallSeed
          do j = 1,gvi_maxProV,1
            lvf_rainFallSeed=0
            lvf_tempArray(j)=0
            do i = lvc_start,lvc_years,1
            
             ! used in Water_CityModel for Stormwater capture
                lvf_tempArray(j)=max(lvf_tempArray(j),max(lvf_rainFallSeed,lvf_MetDataProbability(i,j,3)))
              lvf_rainFallSeed=lvf_MetDataProbability(i,j,3)
            end do
             gvf_peakRainfall(j)= lvf_tempArray(j)
          end do
          ! ---------------------------------------------------------------
10          if(ios >0)then  
            goto 1000
          endif
        return
1000    continue
        !
            if(gvl_writeLog)then
                string=7
                call sStrings(string,errorString)
                call eWrite(errorString,LU)
            endif
          gvl_errorFlag=.false.
          !
      end subroutine readFiles_met
      ! --------------------------
!
End Module lm_Meteorology
!
    ! --------------------------
    subroutine initializeMeteo()
      use lm_Meteorology
        !
        call initMeteo()
        !
      return
    end subroutine 
    ! -------------------------
!
! ======================================================================================================
! E.O.F. Meteorology.f90
