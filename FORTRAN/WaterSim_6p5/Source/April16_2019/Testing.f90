!
! File is ProviderPopulationandDemand.f90
!
! This file contains the provider level estimates of water demand and population
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
! Subroutines:  
!                 
!
! Created on 07.23.14 
!

!
! Last write was: 07.23.14
! ------------------------
!

! ======================================================================================================
!

! ---------------------------------------------------------------
subroutine ParameterCheck(T,provider,one)
    use gm_ModelControl
    use gm_GlobalData
    !
    ! ----------- Types ------------
    integer :: provider
    !integer :: int_1,int_2,int_3
    real :: one
    ! ==============================
    !
    ! ---- Type Construct --
    type(runTime)T
    ! ======================
    !
        if(34 < provider)then
          if(gvl_writeLog)then
            write(110,*)T%year,provider,one
          endif
        endif
    !
 return
end subroutine ParameterCheck
! ---------------------------

! -----------------------------
subroutine OpenFilesParmCheck()
    !
    ! -------- Types --------
    character(len=25) :: path
    ! =======================
    !
    path='WaterSim_Output\'
    open(110,FILE=trim(trim(path) //'OneHundredTen.txt'), status='replace')        
    !
 return
end subroutine OpenFilesParmCheck
! -------------------------------

! -------------------------------
subroutine CloseFilesParmCheck()
    !
    close(110)
    !
 return
end subroutine CloseFilesParmCheck
! --------------------------------