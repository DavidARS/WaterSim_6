!
! File is Program.f90
!
! This file controls the debug calls to the FORTRAN model
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

! OUTPUTS: No Outputs
!
! created on 08.12.10
!
! david arthur sampson
!
! last write was: 01.15.13,07.21.14
! ---------------------------------
!

! =================================================================================================
    ! -------------------------
    program Program
          use lm_Initialize
           use lm_Kernel
           ! -- Types ------
           integer :: i=1
           logical :: runOne=.true.
           ! ===============
            !
            call startupK()
            call readFilesCheckK()
            if(runOne)then
                call InitK()
                !             
                do i = 2000,2060,1
                 call RunOneYearKernel()
                end do
                !
            else
                call InitK()
                call RunManyYearKernel()
            endif
           !
            write(6,*)" Normal Program Termination"
           !
    end program Program
    ! -------------------------- 

    ! ---------------------------
    subroutine setParameters()
      use lm_Kernel
        !
        ! ===================
        gvi_IwaniecScenario=6
        gvl_IwaniecYN=.true.
        gvl_rainWaterHarvesting=.true.
        gvl_rainWaterHarvestResOnly=.false.
        gvl_rainWaterHarvestComOnly=.false.
        gvl_stormWaterHarvesting=.false.
        gvl_parm_grayWater=.false.
        !
!        do i = 1,35,1
!         gvf_parm_OutDoorResProp(i)=0.5
!         gvf_parm_OutDoorComProp(i)=0.4
!         gvf_parm_OutDoorIndProp(i)=0.3
!        end do
        !
        gpl_runSensitivity=.false.
        gvl_IwaniecScenarios_PPtoAg=.true.
        gvf_rainfallfactor=140
        !
      return
    end subroutine setParameters
! 
! =================================================================================================
! E.O.F. Program.f90