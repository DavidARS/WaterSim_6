!
! File is Strings.f90
!
! This file controls the strings used within the the FORTRAN model
!
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
!
! No Module:    Subroutine sStrings(code,errorString)
!               Subroutine sOnceThrough(code,String)
!               subroutine sWrite(errString)
!
! OUTPUTS: Error Strings to the interface
!
! created on 06.25.12
!
! david arthur sampson
!
! last write was: 01.15.13,07.18.14,07.20.14
! -------------------------------------------
!

! ======================================================================================================
!
    ! -----------------------------------
    Subroutine sStrings(code,errorString)
        !
        ! ---- Types ----------------
        character(200) :: errorString
        integer :: code
        ! ===========================
            !
             if(code ==0)errorString=" Finished with Sensitivity Runs"
             if(code ==1)errorString=" Error: Global Abort- RunOneYearKernel"
             if(code ==2)errorString=" Error: ProviderPopulationandDemand.f90 "        
             if(code ==3)errorString=" "        
             if(code ==4)errorString=" Error: gvi_ProviderDemandOption- ProviderPopulationandDemand.f90"
             if(code ==5)errorString=" Error: lvl_demandOption(i)-ProviderPopulationandDemand.f90 "
             if(code ==6)errorString=" Error: Total population = zero- ProviderPopulationandDemand.f90"
             if(code ==7)errorString=" Error: Meteorological data; Read Input- Meteorology.f90"
             if(code ==8)errorString=" Error: WaterShed Salt-Tonto_verde; Read Input - WaterShed_SVT.f90"
             if(code ==9)errorString=" Error: Global Error prior to Colorado River Call-WaterShed_CO.f90"
             if(code ==10)errorString=" "
             if(code ==11)errorString=" Error: Flow on Colorado < zero-WaterShed_CO.f90 "
             if(code ==12)errorString=" Error: Read Inputs- WaterShed_CO.f90"
             if(code ==13)errorString=" Error: Read Inputs- Agriculture.f90"
             if(code ==14)errorString=" "
             if(code ==15)errorString=" Error: Read Inputs- Groundwater.f90"
             if(code ==16)errorString=" "
             if(code ==17)errorString=" Error: Read Inputs- Designations_SVT.f90"
             if(code ==19)errorString=" Error: Read Inputs- DailyDesignations.f90"
             if(code ==21)errorString=" Error: Read Inputs- Designations_CO.f90"
             if(code ==23)errorString=" Colorado Flow at Zero- gvi_WaterToEnvironCO_acft_a"
             if(code ==24)errorString=" Open file error- global - WaterShed_CO.f90 line 157"
             !
             if(code ==25)errorString="  "
             if(code ==26)errorString=" Open file error- daily designations - DesignationsDaily.f90 line 88"
             if(code ==27)errorString=" Error- parameter control read Files_pc"
             if(code ==28)errorString=" Error- city model readFiles_cm"
             !
             if(code ==30)errorString=" "
             if(code ==31)errorString="  Remaining class A water in WaterShed_SaltTonto.f90"
             if(code ==32)errorString="  Verde Flow at zero- WaterShed.f90 line 188"
             if(code ==33)errorString="  Salt-Tonto Flow at zero- WaterShed.f90 line 215"
             if(code ==34)errorString="  Open File error - WaterShed_STV.f90 line 98"
             if(code ==35)errorString="  Read Error - WaterShed_SaltTonto.f90 line 115"
             if(code ==36)errorString="  Read Error - WaterShed_Verde.f90 line 117"
             if(code ==37)errorString="  Error - Verde Storage Loop WaterShed_Verde.f90 line 694"
             !
             if(code ==50)errorString="  Exceeded the loop criteria of the Salt reservoir operations"
             !
             if(code ==54)errorString="  Exceeded the loop criteria of the function fSESgpcd_alpha()"
             if(code ==55)errorString="  Exceeded the loop criteria of Holt in loop -sHoltTarget"
             !
             if(code ==60)errorString="  Calculate Demand Error-bypass all pop and demand"
             if(code ==61)errorString="  Open file error- ProviderPopulationandDemand.f90"
             if(code ==62)errorString="  Error- Read LCLU data"
             if(code ==63)errorString="  Error- Citi Model Read Input data"
             if(code ==65)errorString="  Percent Land Cover Estimates > 100: LandUseLandCover.f90 ~ line 500"
             ! 
             if(code ==100)errorString="  generic openFiles subroutine- corrupt"
             !
        return
    End Subroutine sStrings
    ! ---------------------

    ! ---------------------------------
    Subroutine sOnceThrough(code,String)

        ! ---- Types ----------------
        character(50) :: String
        integer :: code
        ! ===========================
             !
             !if(code ==1)String=" pStartUp_k"
             if(code ==1)String="  pCityParameterControl_a"
             if(code ==2)String="  pGlobalDemand " 
             if(code ==3)String="  pSaltTontoVerde"        
             if(code ==4)String="  pFlowsReservoirsColorado"
             if(code ==5)String="  pInitGroundwater "
             if(code ==6)String="  pDemand"
             if(code ==7)String="  pWaterForAg"
             if(code ==8)String="  pWaterForVadose_s"
             if(code ==9)String="  pWaterForDirectInject_s"
            if(code ==10)String="  pWaterReclaimed_s"
            if(code ==11)String="  pROWaterReclaimed_s "
            if(code ==12)String="  pSRP_newClassA"
            if(code ==13)String="  pSRP_newClassBC"
            if(code ==14)String="  pCAP_water"
            if(code ==15)String="  pSRP_newNCS"
            if(code ==16)String="  pNewWaterSupplies_k"
            if(code ==17)String="  pWaterBanking_k"
            if(code ==18)String="  pProviderGroundwater"
            if(code ==19)String="  pUnmetDemand"
            if(code ==20)String="  pDesignate"
            if(code ==21)String="  pWaterProviders"
            if(code ==22)String="  pCitiModel"
            if(code ==23)String="  pSRPnewRelease"
            if(code ==24)String="  pDefaultPumping"
            if(code ==25)String="  pUnusedNonPotable"
            
        return
    End Subroutine sOnceThrough
    ! -------------------------

    ! ---------------------------------
    subroutine sWrite(aString,LUnit)
     use gm_GlobalData
        !
        ! ---- Types ----------------
        integer :: LUnit
        character(50) :: aString
        ! ===========================
            !
            if(0 < LUnit)then
              write(7,*)aString," logical unit:",LUnit
            else
             gv_exceptionCode=1
              write(7,*)aString
            endif
            !
        !
      return
    end subroutine sWrite
    ! -------------------

    ! ---------------------------------
    subroutine eWrite(errString,LUnit)
     use gm_GlobalData
        !
        ! ---- Types ----------------
        integer :: LUnit
        character(200) :: errString
        ! ===========================
            !
            if(0 < LUnit)then
              write(7,*)errString," logical unit: ",LUnit
            else
             gv_exceptionCode=1
              write(7,*)errString," logical unit: ",LUnit
            endif
            !
        !
      return
    end subroutine eWrite
    ! -------------------

!
!==================================================================================================
! E.O.F. Strings.f90