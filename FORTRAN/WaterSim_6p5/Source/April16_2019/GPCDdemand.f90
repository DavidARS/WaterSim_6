!
!  File is GPCDdemand.f90
!
!    This file contains the functions for the model. Can be called from anywhere in
! the program.
!
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
!
! Module:       Module lms_GPCDdemand 
!               subroutine MySub (lvi_Scale,provider,lvf_resTotal)
!               function fCalcIndoor(i) result (myout)
!               function fCalcOutdoor(i) result (myout)
! No Module
!               subroutine GPCDfromMetrics(scale,provider,GPCDout)

! OUTPUTS: 
!
! created on 10.11.13
!
! david arthur sampson

! last write was: 10.11.13,06.09.14,07.21.14
! ------------------------------------------
!

! =================================================================================================
!
module lms_GPCDdemand
 use gm_ModelControl
  use gm_GlobalData
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
 contains
    !
    ! ------------------------------------------------
    subroutine MySub (lvi_Scale,provider,lvf_resTotal)
        implicit none
        !
        ! ------------------ Types -----------------------
        integer :: i,j,k,provider
        integer :: lvi_Scale

        real :: lvf_resIndoor,lvf_resOutdoor,lvf_resTotal

        logical :: lvl_singleFamily=.true.
        ! ================================================
        ! 1=shower, 2=bath, 3=other, 4=place holder, 5=place holder, 6=placeholder
        ! 7=faucet, 8 = toilets, 9=dishwasher, 10=clothes washer
        !
        i = provider
        !
        do j = 1,10,1
          do k = 1,3,1
           gvf_gpcdIndoor_behavior(i,j,k)=0.
           gvf_gpcdIndoor_efficiency(i,j,k)=0.
          end do
        end do
        do j = 1,7,1
          do k = 1,3,1
           gvf_gpcdOutdoor_behavior(i,j,k)=0.
           gvf_gpcdOutdoor_efficiency(i,j,k)=0.
           gvf_gpcdOutdoor_infrastructure(i,j,k)=0.
          end do
        end do

        if(lvl_singleFamily)then
            select case(lvi_Scale)
              case(1)
                ! Use default values
                !
                   gvf_gpcdIndoor_behavior(i,1,1)=8.0  ! min
                 gvf_gpcdIndoor_efficiency(i,1,1)=2.32 ! gallons per min
                !
                   gvf_gpcdIndoor_behavior(i,2,1)=0.0 ! baths per day 
                 gvf_gpcdIndoor_efficiency(i,2,1)=30 ! gallons per bath
                !
                  gvf_gpcdIndoor_behavior(i,3,1)=1.0 ! min per day
                 gvf_gpcdIndoor_efficiency(i,3,1)=1.0 !  volume gallons per minute
                !
                  gvf_gpcdIndoor_behavior(i,7,1)=5 ! times per day per capita
                 gvf_gpcdIndoor_efficiency(i,7,1)=1.6  ! 0.5 min per time multiplied by 3.2 gal per minute
                !
                  gvf_gpcdIndoor_behavior(i,8,1)=5.31 ! flushes per cap
                 gvf_gpcdIndoor_efficiency(i,8,1)=3.63 ! gallons per flush
                !
                   gvf_gpcdIndoor_behavior(i,9,1)=1.0 ! once per day
                 gvf_gpcdIndoor_efficiency(i,9,1)=4 ! gallons per load
                !
                   gvf_gpcdIndoor_behavior(i,10,1)=1.0 ! loads per day
                 gvf_gpcdIndoor_efficiency(i,10,1)=40.9 ! volume per load
                !
                ! ========================================================
                ! 1=fountains, 2=pools, 3=turf, 4=shrubs, 5=Xeriscape, 6=trees, 7= misc
                !
                ! Water features
                 gvf_gpcdOutdoor_infrastructure(i,1,1)=0.25 ! units per person
                       gvf_gpcdOutdoor_behavior(i,1,1)=70.0 ! gallons per unit
                     gvf_gpcdOutdoor_efficiency(i,1,1)=1.0 ! 
                !
                ! pools; length x width x depth*7.5 = gallons
                 gvf_gpcdOutdoor_infrastructure(i,2,1)=0.10 ! units per person
                       gvf_gpcdOutdoor_behavior(i,2,1)=67000.0 ! gallons per unit (15 feet x 10 feet by 6 feet)
                     gvf_gpcdOutdoor_efficiency(i,2,1)=1.0 ! 
                !

              case(2)
                ! Use specifics
              case(3)
              case default
            end select
        else
            select case(lvi_Scale)
              case(1)
              case(2)
              case(3)
              case default
            end select
        endif
        !
         lvf_resIndoor=0.
        lvf_resIndoor=fCalcIndoor(i)
        !
         lvf_resOutdoor=0.
        lvf_resOutdoor=fCalcOutdoor(i)
        !
         lvf_resTotal=0.
        lvf_resTotal=lvf_resIndoor+lvf_resOutdoor
        !
     return
    end subroutine MySub
    ! ------------------

    ! -----------------------------------
    function fCalcIndoor(i) result (myout)
        !
        ! ---------- Types ------------------
        integer, intent (in) :: i
        integer :: j

        real :: myout,Unit
        real :: lvf_people=2.9
        real,parameter :: lvf_timesPerDay=2.9
        ! ===================================
        !
            !
             myout=0.
            do j = 1,10,1
              Unit=0.
                if(6 < j)then
                 Unit= (gvf_gpcdIndoor_behavior(i,j,1)*gvf_gpcdIndoor_efficiency(i,j,1))/lvf_people
                else
                 Unit= (lvf_timesPerDay*gvf_gpcdIndoor_behavior(i,j,1)*gvf_gpcdIndoor_efficiency(i,j,1))
                endif
                myout=myout+Unit
            end do
            !
        !
    end function fCalcIndoor
    ! -----------------------

    ! -------------------------------------
    function fCalcOutdoor(i) result (myout)
        !
        ! --------------- Types -----------------
        integer, intent (in) :: i
        integer :: j

        real :: myout,Unit
        real,parameter :: lvf_ratePerDay=0.0027
        ! =======================================
        !
            !
             myout=0.
            do j = 1,7,1
              Unit=0.
                if(j < 3)then
                 Unit=lvf_ratePerDay* (gvf_gpcdOutdoor_behavior(i,j,1)*gvf_gpcdOutdoor_efficiency(i,j,1)*gvf_gpcdOutdoor_infrastructure(i,j,1))
                else
                endif
                myout=myout+Unit
            end do
            !
        !
    end function fCalcOutdoor
    !
! Code Parking ---------------------------------
!    function fDefault(Ivar, Rvar) result (Ires)
!        implicit none
!        !
!        ! variable declarations
!        !
!        integer :: Ires
!        integer, intent (in out) :: Ivar
!        real, intent (in out) :: Rvar
!        !
!        ! executable statements
!        !
!        Ires = 0   ! Assign function result 
!    end function

end module lms_GPCDdemand
!
    ! ------------------------------------------------
    subroutine GPCDfromMetrics(scale,provider,GPCDout)
     use lms_GPCDdemand
        !
        ! ----------- Types ------------
        integer :: scale,provider

        real, intent (out) :: GPCDout
        ! ==============================
        !
            !
             GPCDout=0.
            call MySub(scale,provider,GPCDout)
            !
        !
     return
    end subroutine GPCDfromMetrics
!
! =================================================================================================
! E.O.F. GPCDdemand.f90

