!
!  File is Regressions.f90
!
!    This file contains the statistical regressions for the model. Can be called from anywhere in
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
! No Module
!   Subroutines
!       subroutine typefiveExpSRP(annual,peakmonth,tfe)
!       subroutine typefiveExp(annual,peakmonth,tfe)
!       subroutine typefiveExpRes(Model,E,annual,peakmonth,tfe)
!       subroutine typefiveExpCIO(Model,E,annual,peakmonth,tfe)
!       subroutine Normal(Model,myModel,E,lvf_min,lvf_total,lvf_out)
!       subroutine Zdistribution(tfe)
!       subroutine DoNormal(i,lvf_indoor,lvp_min,parm,lvf_indoorOUT,lvf_outdoorOUT)
!       subroutine DemandNormal(myModel,lvp_min,lvf_annual,lvf_OUT)
!       subroutine Nmodifier(it,lvf_mag,lvf_sum,lvf_tot,lvd_mod)
!       subroutine catchException(Model,E)
!       subroutine catchMinException(min,total,newMin)
!   Module gm_Euler
!       subroutine sEuler(c,t_0,x_0,t_f,n)
!   No Module
!       Function f(x)
!       subroutine ptypefiveExp(start,max,peakmonth,tfe)
!       subroutine gaussian(annual,Gd)
!       function gaussianprime(a,b,flow,y)
!

! OUTPUTS: 
!
! created on 11.25.12
!
! david arthur sampson

! last write was: 01.15.13,07.22.14
! ---------------------------------
!

! ======================================================================================================

! Curve fitting- runtime
!
! ---------------------------------------------
subroutine typefiveExpSRP(annual,peakmonth,tfe)
    !
    ! -------------- Types ---------------
    integer :: i,j,k
    integer, parameter :: iterations=1500

    real(8), parameter :: b=0.85
    real(8) :: max,bstar
    real(8) :: start
    real(8) :: peakmonth
    real(8) :: avg,annual,acft,check,diff
    real(8) :: tfe(12)
    ! ===================================
    !
        ! ----------------
        !   real(8) :: xbarUn(12)
        ! --------------------------
        !
        do i = 1,12,1
          tfe(i)=0
        end do
        if(0 < annual)then
        !
        acft=anint(annual*1e6)
        diff=1
        avg = acft/12.
        max = 1.5*avg ! 1.76
        start=0.2*avg ! 
        bstar=b
        check=0
        !
        max=max-start
        do j = 1,iterations,1
         do i = 1,12,1
          tfe(i) = start+max*bstar**(i-peakmonth)**2
         end do 
         !
         check=sum(tfe)
         if(abs(check - acft) < diff)then
    !       write(7,*)j, "iterations in typefiveExp"
          exit
         else
          if(check - acft > 0)then
            bstar=bstar*0.999
          else
            bstar=bstar*1.001
          endif
         endif
        end do
        !
        do k = 1,12,1
         tfe(k)=anint(tfe(k))
        end do
        !
        else
        endif
 10   continue
  return
 100 continue
    !write(7,*) " iterations failed in typefiveExp"
    stop
 end subroutine typefiveExpSRP
! ----------------------------

! ------------------------------------------
subroutine typefiveExp(annual,peakmonth,tfe)
    !
    ! -------------- Types ----------------
    integer :: i,j,k
    integer, parameter :: iterations=2500
    integer :: one
    !   integer :: dt(8)

    real(8), parameter :: b=0.825
    real(8) :: max
    real(8) :: start
    real(8) :: peakmonth
    real(8) :: avg,annual,acft,check,diff
    real(8) :: tfe(12),difference,bstar
    ! ====================================
    !

        ! --------------------------
        !
        do i = 1,12,1
         tfe(i)=0
        end do
        !
        acft=anint(annual*1e6)
        diff=1  ! One acft
        avg = acft/12.
        max = 1.46*avg ! Chi Chis defense ppt slide #9
        start=0.83*avg ! Chi Chi defense ppt slide #9
        bstar=b
        one=0  
        check=0
        difference=0
        !
    1   max=max-start
        do j = 1,iterations,1
         do i = 1,12,1
          tfe(i) = start+max*bstar**(i-peakmonth)**2
         end do 
         one=one+1
         !
         check=sum(tfe)
         difference=abs(check - acft)
         if(difference < diff)then
          goto 10
         else
          if(check - acft > 0.)then
            bstar=bstar*0.9999
          else
            bstar=bstar*1.001
          endif
         endif
         !
    !     if(diff == 0.)goto 10
         !
        end do
        !
         if(one > iterations)then
    !      call date_and_time(values=dt)
          !write(7,*)bstar," In error loop type five",difference,dt
          one=0
          diff=diff+1
          bstar=0.85
          goto 1
         endif
     
10 continue
        do k = 1,12,1
         tfe(k)=anint(tfe(k))
        end do
    !    if(diff /= 1.)then
    !     !write(7,*)diff, "diff in typefiveExp"
    !    endif
        !
  return
 100 continue
    !write(7,*)bstar,j," Iterations failed in typefiveExp"
    stop
 end subroutine typefiveExp
 ! ------------------------

! -----------------------------------------------------
subroutine typefiveExpRes(Model,E,annual,peakmonth,tfe)
 use gm_GlobalData
  use gm_Exception

    ! inputs maf a-1
    ! outputs AF month-1
    ! ------------------ Types -------------
    integer :: i,j,k
    integer, parameter :: iterations=5000
    integer :: one
    !integer :: dt(8)

    real(8), parameter :: b=0.81
    real(8) :: difference,bstar
    real(8) :: max
    real(8) :: start
    real(8) :: peakmonth
    real(8) :: avg,annual,acft,check,diff
    real(8) :: firstobs,inc 
    real(8) :: tfe(12,2),temp(12)
    real(8) :: xbarUn(12)

    character(10) :: Model
    !======================================
    !

    ! --- Type Construct ----
    type(Exception)E
    ! =======================
    !
        do i = 1,12,1
          do j = 1,2,1
            tfe(i,j)=0
          end do
        end do
        !
        acft=anint(annual*1e6)
        diff=1
        avg = acft/12.
        max = 1.43*avg 
        start=0.65*avg ! (data suggests 0.65) 
        firstobs=0.73*avg ! 0.8
        bstar=b
        inc=0.09
        one=0
        check=0
        difference=0
        !
        ! ------------------------------------------------------------------------------
        ! 95% uncertainty in the estimate, relative to the mean for each month 
        ! (two standard deviations / mean)
        !
        xbarUn(1)=0.08580   ; xbarUn(2)=0.16769 ; xbarUn(3)=0.18918 ; xbarUn(4)=0.12625
        xbarUn(5)=0.10108   ; xbarUn(6)=0.05551 ; xbarUn(7)=0.04710 ; xbarUn(8)=0.08454
        xbarUn(9)=0.05403   ; xbarUn(10)=0.09051; xbarUn(11)=0.24367; xbarUn(12)=0.11092
        ! -------------------------------------------------------------------------------

    1    max=max-start
        do j = 1,iterations,1
          tfe(1,1)=firstobs
         do i = 2,12,1
          tfe(i,1) = start+max*bstar**(i-peakmonth)**2
          tfe(8,1) = (1-inc)*tfe(7,1)
          tfe(9,1) = (1-(inc*2))*tfe(7,1)
          tfe(10,1)= (1-(inc*3))*tfe(7,1)
          tfe(11,1)= (1-(inc*4))*tfe(7,1)
          tfe(12,1)= (1-(inc*5))*tfe(7,1)
         end do 
         one=one+1
         !
         do i = 1,12,1
          temp(i)=tfe(i,1)
         end do
         check=sum(temp)
         difference=abs(check - acft)
         if(difference < diff)then
    !       write(7,*)j, "iterations in typefiveExp"
          exit
         else
          if(check - acft > 0)then
            bstar=bstar*0.999
          else
            bstar=bstar*1.001
          endif
         endif
         !
        end do
        !
        if(one > iterations)then
    !      call date_and_time(values=dt)
          !write(7,*)bstar," In error loop- residential",difference,dt
          one=0
          diff=diff+1
          bstar=bstar+0.1
          goto 1
         endif
10      do k = 1,12,1
         tfe(k,2)= tfe(k,1)*xbarUn(k)
         tfe(k,1)=anint(tfe(k,1))   ! check this result
        end do
    !
   return
 100 continue
    !
    if(gvl_writeToLog)then
     !
     call catchException(Model,E)
     !
    endif
    stop
 end subroutine typefiveExpRes
! ----------------------------

! -----------------------------------------------------
subroutine typefiveExpCIO(Model,E,annual,peakmonth,tfe)
 use gm_GlobalData
  use gm_Exception
    ! -------------- Types ----------------
    integer :: i,j,k
    integer, parameter :: iterations=2500
    integer :: one

    !integer :: dt(8)
    real(8) :: difference
    real(8), parameter :: b=0.85
    real(8) :: max,bstar
    real(8) :: start
    real(8) :: peakmonth
    real(8) :: avg,annual,acft,check,diff
    real(8) :: firstobs,inc 
    real(8) :: tfe(12,2),temp(12)
    real(8) :: xbarUn(12)

    character(10) :: Model
    ! ===================================
    !

    ! ---- Type COnstruct ----
    type(Exception)E
    ! ========================
    !

       ! --------------------------
        !
        do i = 1,12,1
          do j = 1,2,1
           tfe(i,j)=0
          end do
        end do
        !
        acft=anint(annual*1e6)
        diff=1
        avg = acft/12.
        max = 1.41*avg 
        start=0.63*avg ! 
        firstobs=0.69*avg ! 
        bstar=b
        inc=0.05
        check=0
        difference=0
        one=0
        !
        ! ------------------------------------------------------------------------------
        ! 95% uncertainty in the estimate, relative to the mean for each month 
        ! (two standard deviations / mean)
        !
        xbarUn(1)=0.08580   ; xbarUn(2)=0.16769 ; xbarUn(3)=0.18918 ; xbarUn(4)=0.12625
        xbarUn(5)=0.10108   ; xbarUn(6)=0.05551 ; xbarUn(7)=0.04710 ; xbarUn(8)=0.08454
        xbarUn(9)=0.05403   ; xbarUn(10)=0.09051; xbarUn(11)=0.24367; xbarUn(12)=0.11092
        ! -------------------------------------------------------------------------------

1    max=max-start
        do j = 1,iterations,1
          tfe(1,1)=firstobs
          tfe(2,1)=firstobs*(1-inc)
          tfe(3,1)=firstobs*(1-(2*inc))
          tfe(4,1)=firstobs*(1+(2*inc))
        
         do i = 5,12,1
          tfe(i,1) = start+max*bstar**(i-peakmonth)**2
         end do 
         !
         one=one+1
         do i = 1,12,1
          temp(i)=tfe(i,1)
         end do
         check=sum(temp)
       difference=abs(check - acft)
         if(difference < diff)then
    !       write(7,*)j, "Iterations in typefiveExp"
          exit
         else
          if(check - acft > 0)then
            bstar=bstar*0.999
          else
            bstar=bstar*1.001
          endif
         endif
         !
        end do
        !
        if(one > iterations)then
    !      call date_and_time(values=dt)
          if(difference < diff)goto 10
          !write(7,*)bstar,diff," In error loop- cio",difference,dt
          one=0
          diff=diff+1
          bstar=0.85
          goto 1
         endif

10      do k = 1,12,1
         tfe(k,2)= tfe(k,1)*xbarUn(k)
         tfe(k,1)=anint(tfe(k,1))   ! check this result
        end do
        !
   return
 100 continue
    if(gvl_writeToLog)then
     !
     call catchException(Model,E)
     !
    endif
    ! write(7,*) " iterations failed in typefiveExp"
    stop
 end subroutine typefiveExpCIO
! ----------------------------

! ----------------------------------------------------------
subroutine Normal(Model,myModel,E,lvf_min,lvf_total,lvf_out)
 use gm_GlobalData
  use gm_Exception
    ! ---------------------- Types --------------------------
    character(10) :: Model,myModel
    
    integer :: j,k,lvi_threshold
    integer :: iterations=5000

    real :: lvf_adjust,lvf_min,lvf_newMin,lvf_pct
    real(8) :: lvd_mod
    real :: lvf_total,lvf_difference,lvf_sum,lvf_threshPCT
    real :: lvf_magnitude
    real, parameter :: lpf_adjustMin=0.00000000001
    real, dimension(1:12) :: lvf_out !,tfe
    real, dimension(1:12) :: tfe
    ! =======================================================
    !

    ! - Type Construct -
    type(Exception)E
    ! ==================
    !
        ! =======================================================
        !
    !    lvf_Beta=3.5
    !    lvf_alpha=7.2 ! 6.5 and 3
        !
         lvf_newMin=0
        if(0 < lvf_min .and. lvf_total < 1)goto 97
         call catchMinException(lvf_min,lvf_total,lvf_newMin)

        lvf_min=lvf_newMin

        if(lvf_min <=0)goto 95
        if(lvf_total <=0)goto 96
    1   continue
        !
        lvi_threshold=1
        lvf_difference=0
        lvf_magnitude=lvf_total-lvf_min
        lvf_pct=lvf_min/lvf_total
        lvd_mod=0.8
        if(0.01 <= lvf_pct)lvd_mod=0.75
        if(0.02 <= lvf_pct)lvd_mod=0.65
        if(0.03 <= lvf_pct)lvd_mod=0.55
        if(0.04 <= lvf_pct)lvd_mod=0.45
        if(0.05 <= lvf_pct)lvd_mod=0.35
        if(0.06 <= lvf_pct)lvd_mod=0.25
        if(0.07 <= lvf_pct)lvd_mod=0.16
        !    
        if(0.08 < lvf_pct)then
         if(gvl_writeToLog)write(7,*)" In normal curve fit, initial water > 8% of total"
        else
          call Zdistribution(tfe)
        !     -------------------
        do j = 1,iterations,1
          lvf_sum=0
          lvf_adjust=max(lpf_adjustMin,(lvf_total-lvf_min)*lvd_mod)
          ! +23
10        do k = 1,12,1
            lvf_out(k)=0
           lvf_out(k)=nint(tfe(k)*lvf_adjust + lvf_min)   ! check this result
          end do
          !
          lvf_sum=sum(lvf_out)
          lvf_difference=abs(lvf_sum - lvf_total)
          !
           if(0 < lvf_total)then
             lvf_threshPCT=lvf_difference/lvf_total
           endif

          if(2000 < j)then
            if(lvf_threshPCT <= 0.001)then
             exit
            endif
          else
            if(lvf_threshPCT <= 0.0001)then
             exit
            endif
          endif
          if(lvf_difference <=lvi_threshold)then
            exit
          else
            !
            call Nmodifier(j,lvf_magnitude,lvf_sum,lvf_total,lvd_mod)
            !
          endif
         end do
        endif
90  continue
   return
   
 95 continue 
    if(gvl_writeToLog)write(7,*)"Minimum <= 0- call to Normal subroutine"
        !
        ! Added on 01.26.17
          do k = 1,12,1
            lvf_out(k)=0
          end do
        !
      goto 101
 96 continue
    if(gvl_writeToLog)write(7,*)"Total Water <= 0- call to Normal subroutine"
        !
        ! Added on 01.26.17
          do k = 1,12,1
            lvf_out(k)=0
          end do
        !
      goto 101
 97 continue
    if(gvl_writeToLog)write(7,*)"No surface water rights in call to Normal"
        !
        ! Added on 01.26.17
          do k = 1,12,1
            lvf_out(k)=0
          end do
        !
      goto 101
 100 continue
    if(gvl_writeToLog)then
     write(7,*)"No closure-call to Normal", lvf_difference, " = Difference to closure "
     write(7,*)"Min:" ,lvf_min, "Total: ",lvf_total, "pct: ",lvf_min/lvf_total 
     write(7,*)myModel
    endif
    !
 101 continue
     !
     call catchException(Model,E)
     !
 return
end subroutine Normal
! -------------------

! ---------------------------
subroutine Zdistribution(tfe)
    !
    ! ----------------- Types -----------------
    integer :: i 

    real, dimension(1:12) :: tfe
    real,parameter :: lvp_pi=3.1415926535
    real,parameter :: lvf_Beta=8
    real,parameter :: lvf_alpha=7.2 ! 6.5 and 3
    ! =========================================
    !
        !
        do i = 1,12,1
          tfe(i)=0
         tfe(i)=1 / (lvf_Beta*2*lvp_pi)**0.5 * EXP((-1/(2*lvf_Beta))* (i-lvf_alpha)**2) 
        end do
        !
    !
 return
end subroutine Zdistribution
! --------------------------

! -------------------------------------------------------------------------
subroutine DoNormal(i,lvf_indoor,lvp_min,parm,lvf_indoorOUT,lvf_outdoorOUT)
 use gm_GlobalData
 use gm_Exception
    !
    ! ----------------------- Types -----------------------------------
    integer :: i,j

    real :: lvf_indoor,lvp_min(gvi_maxProV)
    real ::  parm(gvi_maxProV),lvf_norm(12)
    real :: lvf_indoorOUT(gvi_maxProV,12),lvf_outdoorOUT(gvi_maxProV,12)
    real :: lvf_annual

    character(10) :: Model='DoNormal'
    character(10) :: myModel='Various'
    ! =================================================================
    !

    ! -- Type Construct --
    type(Exception)E
    ! ====================
    !
      !
       lvf_annual=lvf_indoor * parm(i)
      ! --------------------------------------------------
      E%MyFunction='Normal'
      call Normal(Model,myModel,E,lvp_min(i),lvf_annual,lvf_norm)                 
      ! --------------------------------------------------
      do j = 1,12,1
        lvf_indoorOUT(i,j)=(lvf_indoor -lvf_annual)/12
        lvf_outdoorOUT(i,j)=lvf_norm(j)
      end do
      !
    !
 return
end subroutine DoNormal
! ---------------------

! ---------------------------------------------------------
subroutine DemandNormal(myModel,lvp_min,lvf_annual,lvf_OUT)
 use gm_GlobalData
 use gm_Exception
    !
    ! ----------- Types -------------
    integer :: j

    real :: lvf_annual,lvp_min
    real :: lvf_OUT(12),lvf_norm(12)

    character(10) :: Model='DoNormal'
    character(10) :: myModel
    ! ===============================
    !

    ! - Type Construct -
    type(Exception)E
    ! ==================
      !
      ! --------------------------------------------------
      E%MyFunction='Normal'
      call Normal(Model,myModel,E,lvp_min,lvf_annual,lvf_norm)                 
      ! --------------------------------------------------
      do j = 1,12,1
        lvf_OUT(j)=lvf_norm(j)
      end do
      !
    !
 return
end subroutine DemandNormal
! -------------------------

! ------------------------------------------------------
subroutine Nmodifier(it,lvf_mag,lvf_sum,lvf_tot,lvd_mod)
    !
    ! -------------- Types ---------------
    integer :: it

    real :: lvf_mag
    real(8) :: lvd_mod
    real :: lvf_tot,lvf_sum
    real(8) :: lvd_diffPct
    real,parameter :: lvp_eightTens=0.8
    real,parameter :: lvp_Tenths=0.9
    real,parameter :: lvp_Hundredths=0.99
    real,parameter :: lvp_Thous=.999
    real,parameter :: lvp_TenThous=0.9999
    real,parameter :: lpf_minParameter=0.00000000001
    ! ====================================
    !
      !
      if(0 < lvf_tot)lvd_diffPct=lvf_sum/lvf_tot
      !
      if(lvd_diffPct <= 0.95)then
          !
           if(lvf_sum < lvf_tot)then
              lvd_mod=max(lpf_minParameter, lvd_mod*1/lvp_eightTens)
           else
              lvd_mod=max(lpf_minParameter,lvd_mod*lvp_eightTens)
           endif
           !
       else if(0.95 < lvd_diffPct .and. lvd_diffPct <= 1.05)then
       !
       !
          if(0.999 < lvd_diffPct .and. lvd_diffPct < = 1.001)then
            if(lvf_sum < lvf_tot)then
              lvd_mod=max(lpf_minParameter,lvd_mod*1/0.99999)
            else
              lvd_mod=max(lpf_minParameter,lvd_mod*0.99999)
            endif
          else if(0.99 < lvd_diffPct .and. lvd_diffPct < = 1.01)then
            if(lvf_sum < lvf_tot)then
              !
              if(10000 < lvf_mag)then
               lvd_mod=max(lpf_minParameter,lvd_mod*1/lvp_TenThous)
              else
               lvd_mod=max(lpf_minParameter,lvd_mod*1/lvp_Thous)
                if(500 < it )lvd_mod=max(lpf_minParameter,lvd_mod*1/lvp_TenThous)
              endif
              !
            else
              if(10000 < lvf_mag)then
                lvd_mod=max(lpf_minParameter,lvd_mod*lvp_TenThous)
              else
                lvd_mod=max(lpf_minParameter,lvd_mod*lvp_Thous)
                if(500 < it )lvd_mod=max(lpf_minParameter,lvd_mod*1/lvp_TenThous)
              endif
            endif
          else if(0.98 < lvd_diffPct .and. lvd_diffPct < = 1.02)then
           !
            if(lvf_sum < lvf_tot)then
              lvd_mod=max(lpf_minParameter,lvd_mod*1/lvp_Thous)
            else
              lvd_mod=max(lpf_minParameter,lvd_mod*lvp_Thous)
            endif
          else
            if(lvf_sum < lvf_tot)then

              lvd_mod=max(lpf_minParameter,lvd_mod*1/lvp_Hundredths)
            else
              lvd_mod=max(lpf_minParameter,lvd_mod*lvp_Hundredths)
            endif
          endif
       !
       !
       else if(1.05 < lvd_diffPct .and. lvd_diffPct <= 1.1)then
          !
           if(lvf_sum < lvf_tot)then
              lvd_mod=max(lpf_minParameter,lvd_mod*1/lvp_Tenths)
           else
              lvd_mod=max(lpf_minParameter,lvd_mod*lvp_Tenths)
           endif
           !
       else
         !
           if(lvf_sum < lvf_tot)then
              lvd_mod=max(lpf_minParameter,lvd_mod*1/lvp_eightTens)
           else
              lvd_mod=max(lpf_minParameter,lvd_mod*lvp_eightTens)
           endif
           !
       endif
       !
    !
 return
end subroutine Nmodifier
! ----------------------
       
! --------------------------------
subroutine catchException(Model,E)
 use gm_GlobalData
  use gm_Exception
  !
    ! ----------------------- Types ------------------------
    integer :: i

    character(10) :: Model
    character(len=3), dimension(12) :: MyMonth
    character(len=2), dimension(gvi_maxProV) :: providers
    ! ======================================================
    !

    ! -- Type Construct --
    type(Exception)E
    ! ====================
    !
        !
        MyMonth(1)='Jan' ;  MyMonth(2)='Feb'; MyMonth(3)='Mar';  MyMonth(4)='Apr'
        MyMonth(5)='May';  MyMonth(6)='Jun';  MyMonth(7)='Jul';  MyMonth(8)='Aug'
        MyMonth(9)='Sep'; MyMonth(10)='Oct';  MyMonth(11)='Nov'; MyMonth(12)='Dec'
        !
        call sProviders(providers)
        !
        if(gvl_writeToLog)then
            i=E%Prov
            !
            if(E%TStep /=0)then
              write(7,*)i,"Module: ",Model," Provider: ",providers(i),"  Month: " ,&
              MyMonth(E%TStep)," Subroutine: ", E%MySub," Function: ",E%MyFunction,&
              "Specific Call: ", E%MyCall
            else
              write(7,*)i,"Module: ",Model," Provider: ",providers(i), &
              "  Subroutine: ",E%MySub," Function: ",E%MyFunction," Specific Call: ", E%MyCall
            endif
    !          write(7,*)" "
        !
        endif
        !
    !
 return
end subroutine catchException
! ---------------------------

! --------------------------------------------
subroutine catchMinException(min,total,newMin)
    !
    ! --------- Types ---------
    real :: min,total,newMin
    real :: pct
    ! =========================
    !
        !
        newMin=min
        pct=0
        if(0 < total)then
            pct=min/total
            if(0.07 < pct)then
              newMin = 0.06 * total
            else
                if(pct < 0.01)then
                 newMin = 0.01 * total
                endif
            endif 
        endif
 return
end subroutine catchMinException
!-------------------------------
!
! =================================================================================================
!
Module gm_Euler

 implicit none
    ! --------- Types --------------
    Integer, private :: n,j

    real(8), public :: y,x0
    Real(8), private :: t_0, x_0
    real(8), private :: t_f, k_j, h
    Real(8), private :: t,x
    real(8), public :: outE(12)
    ! =============================

    !
  Contains
     ! ------------------------
     ! n = number of steps
     ! h = step width (delta t)
     ! k_j = dx?
     ! t_f = delta state
     ! x0 = initial state
     ! t_0 = up dated state (new state) at the end of n steps
     ! -------------------------------------------------------


    ! --------------------------------
    subroutine sEuler(c,t_0,x_0,t_f,n)
        !
        ! --------- Types ----------
        integer, intent(in) :: n
        integer :: i,j

        real(8) :: c(12)
        real(8), intent(in) :: t_0
        real(8), intent(in) :: x_0
        real(8) :: f,t_f
        ! ==========================
        !
            !
             h=0
             t=0
             x=0
            do i = 1,n,1
             k_j=0
             c=0
            end do
             h=(t_f-t_0) * 1./n
            !
             x=x_0
             t=t_0
            Do j=1,n,1
             k_j=f(x)
             x=x+ (h*k_j) ! correct ??? 08.17.10 das
             t=t+h
             c(j)=t
            end do
            !
        !
      return
    end subroutine sEuler
    ! -------------------
!
End Module gm_Euler

! -----------
Function f(x)

    ! ------- Types --------
    real(8), intent(in) :: x
    real(8) :: f
    ! ======================
        !
        f=2*x
        !
    !
end function 
! ----------

 ! ---------------------------------------------
 subroutine ptypefiveExp(start,max,peakmonth,tfe)
    !
    ! -------- Types ----------
    integer :: i

    real(8), parameter :: b=0.8
    real(8) :: max
    real(8) :: start
    real(8) :: peakmonth
    real(8) :: tfe(12)
    ! =========================
    !
        !
         max=(max-start)
        do i = 1,12,1
         tfe(i) = start+max*b**(i-peakmonth)**2.
        end do 
        !
    !
  return
 end subroutine ptypefiveExp
! --------------------------

! -----------------------------
 subroutine gaussian(annual,Gd)

    !
    ! ----------- Types-----------
    integer :: i
    
    real(8) :: acft,annual
    real(8) :: pi
    real(8), parameter :: mu=6.5
    real(8), parameter :: sigma=1
    real(8) :: Gd(12)       ! acft month-1 outputs
    real(8) :: g
    ! ============================
    !
        !
        acft=annual*1e6
        pi = atan(1.)*4.
        do i = 1,12,1
         g= ( (1/(sigma*sqrt(2*pi) ))*exp(-((i-mu)**2 * (1/(2*sigma**2)))) )
         Gd(i)=anint(g*acft)
        end do
        !
    !
  return
 end subroutine gaussian
 ! ---------------------

 ! --------------------------------
 function gaussianprime(a,b,flow,y)
    !
    ! ------- Types----------
    integer :: i
    integer :: flow

    real(8) :: a,b,pi,y(12)
    real(8) :: gaussianprime
    ! =======================
    !
        !
        pi = atan(1.)*4.
        !
        do i = -6,6,1
           gaussianprime=anint(-(i-a)/sqrt(2*pi)*b**1.5 )*exp(-(i-a)**2 *(1/2*b) )
           y(i+7)=nint(gaussianprime*flow)
        end do
        !
    !
end function gaussianprime
! ------------------------

! =================================================================================================
! E.O.F. Regressions.f90
!

!
! Some code parking
! ---------------------------------------------------------------
!MODULE IVPSolve 
! use Precision
!!...
!   subroutine Euler(f,y,n_unknowns,n_points,dx,x0,y0)
!      ! The function f is passed as an argument:
!      INTERFACE
!         subroutine f(x,y,y_prime,n_unknowns)
!            USE Precision
!            INTEGER n_points
!            REAL(KIND=wp) :: x
!            REAL(KIND=wp), DIMENSION(n_unknowns) :: y, y_prime
!         end subroutine f
!      END INTERFACE
!      integer :: n_unknowns,n_points
!      integer :: i
!      ! Upon exit, this array should contain the solution
!      real(kind=wp), dimension(n_unknowns,n_points) :: y
!      real(kind=wp) :: dx, x0 ! Step size and initial x
!      real(kind=wp), dimension(n_unknowns) :: y0 ! Initial y
!
!!      ...
!!      ...Declare any needed local variables here...
!!      ...Initialize variables for the DO loop...       
!      DO i=1,n_points-1
!!         ...Calculate x here in steps of dx starting at x0...
!         CALL f(x,y(:,i),y_prime,n_unknowns)
!         y(:,i+1)=y(:,i)+y_prime*dx ! Euler's method
!!         ...
!      END DO
!!      ...
!   end subroutine Euler
!END MODULE IVPSolve

!Another one------
! =================================================================================================
! Actual end to Regressions.f90