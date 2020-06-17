!
!   File is Provider.f90
!
!       This file contains the provider level calculations; essentialy, most of the final 
!   water balance calculations area conducted here. And, are sent to the interface. i.e.,
!   individual water protfiolios are provided here, as output either to other submodules
!   of to the C# interface, as discussed.
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
! Module:       Module lms_Provider
! Subroutines:  subroutine writetodisk_(T,vLP,vD)
!               subroutine sProvider(T,vGP,vLP)
!               subroutine aDemandSupply(T,vGP,vLP)
!               subroutine aDifference(T,vGP)
!
! No Module:    subroutine NexchangeVariables(Sin,vLP)
!               subroutine pWaterProviders(T,Sin,vGP)
!               subroutine sCalculateAddedDemand(T)
!

! Global OUTPUTS:

!
! Local OUTPUTS:
!                

!               
! Local INPUTS:
!              
!-------------------------------------------------------------------------------------------
!
! created on 06.08.10
!
! david arthur sampson

! last write was: 05.10.14,06.09.14,07.18.14
! ------------------------------------------

!
! ------------------------------------------------------------------------------------------
!
Module lms_Provider
 use gm_ModelControl
   use gm_TypeControl
    use gm_Euler
     use gm_Exception
        !
        ! -----
        include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
        !

            ! ---------------------- Module Global Type Constructs ----------------------------------------------------
            type LocalProvider
             real(8) :: lv_capMandItiers(5,2)
             real(8) :: lv_realizedDesignationCAP_acft(gpi_lBY:gpi_uBY,gvi_maxProV)
             real(8) :: lv_providerDemandAg(gpi_lBY:gpi_uBY,gvi_maxProV)
             real(8) :: lvd_demand_designations(gvi_maxProV)
             real(8) :: lvd_providerSRP_total_35_acft(gpi_lBY:gpi_uBY,gvi_maxProV)
             real(8) :: lvd_providerSRP_classA_35_acft(gpi_lBY:gpi_uBY,gvi_maxProV)
             real(8) :: lv_ProvidersSRPpumping_acft(gpi_lBY:gpi_uBY,gvi_maxProV)
             real :: lvd_align
             !
             real    :: lvf_NCS(10),lvf_maxNCS(10)
             real    :: lvf_providerSRP_NCS_35_acft(gpi_lBY:gpi_uBY,gvi_maxProV)
             real    :: lvf_unusedClass_BC_acft(gvi_maxProV)
             real    :: lvf_reclaimed(gvi_maxProV),lvf_ROreclaimed(gvi_maxProV),lvf_GWbanking(gvi_maxProV)
             real    :: lvf_newSupplies_acft_a(gvi_maxProV)
             real    :: lvf_unMetDemand_acft(gvi_maxProV)
             real    :: lvf_demand_SRP_used_acft(10),lvf_CAP_used_acft(gvi_maxProV),lvf_BandCpumpingPass_35_acft(gvi_maxProV)
             !
             !
            end type LocalProvider
            !
            character(10),parameter :: Model='Provider'
            ! ==============================================================================================================
        !
 contains
    !
        ! ------------------------
        subroutine writetodisk_(T)
            !
            ! -- Types ----
            integer :: ios
            ! =============
            !
            ! -Type Constructs-
            type(runTime)T
            ! =================
            !
            ios=0
            ! --------------------------------------------------------
           return
100         continue
             if(gvl_writeLog)then
                write(7,*)"ios on unit", ios, " Provider.f90 line 237"
             endif
             stop
        end subroutine writetodisk_
        !---------------------------

        !------------------------------
        subroutine sProvider(T,vGP,vLP)
            ! --- Types ----
            integer :: i,j
            ! ==============
            !

            ! - Type Constructs ----
            type(Provider)vGP
            type(LocalProvider)vLP
            type(runTime)T
            ! ======================
                !
                !==============
                ! 
                do j = 1,gvi_Providers,1
                 vGP%cPopProvider(j)     = gvf_populations(T%year,j) 
                 vGP%oDemandNoAg_acft(j) = gvf_WaterDemand_acft(j,1,1)
                end do
                !
                call clearVariables(T,vGP)
                  call aDifference(T,vGP)
                call aDemandSupply(T,vGP,vLP)
                !call aDifference(T,vGP)
                !
                !==============
                ! -------------------------------------------------------------
                ! All Providers
                ! ---------------------
                 ! Send to the interface- Provider water demand and population
                 ! ----------------------
                 do i = 1,gvi_Providers,1
                   go_ProviderDemand(i)      =0
!                  go_ProviderDemand(i)       =NINT(vGP%oDemandNoAg_acft(i))                          ! acft a-1 :integer
!                  go_ProviderDemand(i) = nint(gvf_WaterDemand_acft(i,1,1))
                   go_ProviderDemand(i) = nint(gvf_TotalDemand_acft(i))

                   go_providerpop(i)=0  
                  go_providerpop(i)          =NINT(gvf_populations(T%year,i))                             !  ppl a-1
                 end do
                ! ------------------------
                ! Send to CityModel
                ! --------------------------------------------------------------
                ! vGP%cPopProvider(j) 
                ! vPV%gvf_cCAP_used_acft_a(i)
                ! vPV%gvf_cSRP_A_used_acft_a(i)+vPV%gvf_cSRP_NCS_used_acft_a(i)
                ! vPV%gvf_cSRP_BC_used_acft_a(i)
                ! vPV%gvf_cMunicipalPumping_acft(i)

                ! vPV%lvf_cDemand_Res_out_AF_a(i)
                ! vPV%lvf_cDemand_Res_In_AF_a(i)
                ! vPV%lvf_cDemand_Ind_out_AF_a(i)
                ! vPV%lvf_cDemand_Ind_In_AF_a(i)
                ! --------------------------------------------------------------
                ! Write to the log file, unit 7
                if(T%year .EQ. T%startyear)then
                    if(gvl_writeLog)then
                        string=21
                        LU=0
                        call sOnceThrough(string,streamString)
                        call sWrite(streamString,LU)
                    endif
                endif
            !
          return
        end subroutine sProvider
        ! ---------------------------------

        ! ----------------------------------
         subroutine aDemandSupply(T,vGP,vLP)
            !
            ! ---------------------------------- Types ----------------------------------------------------------------
             integer :: i,j,k,lvi_countSRP
             real  :: lvf_One,lvf_Two,lvf_Three,lvf_Four,lvf_Five,lvf_Six,lvf_Seven,lvf_Eight,lvf_Nine,lvf_Ten
             real  :: lvf_Eleven,lvf_Twelve,lvf_Thirteen,lvf_Fourteen

             real :: lvf_CAP(gvi_maxProV),lvf_SRP_A(gvi_maxProV),lvf_SRP_BandCpump(gvi_maxProV),lvf_SRP_NCS(gvi_maxProV)
             real :: lvf_reservoirSRP(gvi_maxProV)
             real :: lvf_GWpumpageMunicipal_acft(gvi_maxProV),lvf_SRP_BandCresAndPumping(gvi_maxProV)
             real :: lvf_otherWater(gvi_maxProV),lvf_unmetDemand(gvi_maxProV)
!             real :: lvf_GrossOnProjectDemand(gvi_maxProV),lvf_GrossOffProjectDemand(gvi_maxProV)
!             real :: lvf_NetOnProjectDemand(gvi_maxProV),lvf_NetOffProjectDemand(gvi_maxProV)
             real :: lvf_addDefaultPumping(gvi_maxProV)
             !real :: lvf_CAPunused(gvi_maxProV),lvf_SRPunused(gvi_maxProV)
             real :: lvf_addWater,lvf_subtWater(gvi_maxProV)  
!             real :: lvf_Off_CAP(gvi_maxProV),lvf_On_CAP(gvi_maxProV)
!             real :: lvf_Off_NCS(gvi_maxProV),lvf_On_NCS(gvi_maxProV)
!             real :: lvf_Off_GWater(gvi_maxProV),lvf_On_GWater(gvi_maxProV)
             real :: lvf_capAvailable(gvi_maxProV)
             real :: lvf_srpAvailable(gvi_maxProV)

             !real:: lvf_sumAgTotalCredits(gvi_maxProV)
             real:: lvf_actualBankingSW
            !
            !real :: fSimpleMovingAverage ! simple moving average function
            !
            ! ===========================================================================================================
            !

            ! - Type Constructs ----
             type(Provider)vGP
             type(LocalProvider)vLP
             type(runTime)T
            ! ======================
            ! --------------------------------------------------------------------
              !
!                call sCalculateAddedDemand(T)
              !
              if(gpl_DemandDriven)then
                  !
                  gii_regDemand=0;gii_regVadose=0;gii_regDirectInject=0;gii_regReclaimed=0
                  gii_regRO=0;gii_regSRPClassA=0;gii_regSRPClassBC=0; gii_regSRPNCS=0
                  gii_regCAP=0;gii_regUnmet=0;gii_reg_2=0;gii_reg_3=0
                  gii_regNewWater=0;gii_regBankedWater=0;gii_regDefaultPump=0;gii_regMuniPump=0
                  !
                  !
                  !
                  do i = 1,gvi_Providers,1
                   lvf_addDefaultPumping(i)=0
                  end do
                ! ======================================================================================================
                  do i = 1,gvi_Providers,1
                    lvf_CAP(i)=0
                    lvf_SRP_A(i)=0
                    lvf_SRP_NCS(i)=0
                    lvf_SRP_BandCpump(i)=0
                    lvf_GWpumpageMunicipal_acft(i)=0
                    !
                    !do j = 1,gpi_unmet,1
                    do j = 1,gpi_unusedNonPotable,1
                      if(nint(gvf_WaterDemand_acft(i,j,6)) == 1)then
                       ! This is the raw municipial and CIO demand
!                        lvf_GrossOnProjectDemand(i)=gvf_WaterDemand_acft(i,j,3)
!                        lvf_GrossOffProjectDemand(i)=gvf_WaterDemand_acft(i,j,2)
                        !
                        gii_regDemand=gii_regDemand+(gvf_WaterDemand_acft(i,j,3)+gvf_WaterDemand_acft(i,j,2))
                        !
!                      else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_waterOther)then

                        ! Place holder - Water for Agriculture Johnston et al. Study
                      else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_vadose)then
                        vGP%gvf_cVadose_a(i)=gvf_WaterDemand_acft(i,j,1) & 
                          - gvf_WaterDemand_acft(i,1,1) ! OK
                        !
                        gii_regVadose=gii_regVadose+vGP%gvf_cVadose_a(i)
                        !
                      else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_directInject)then
                        vGP%gvf_cDirectInjection_a(i)=gvf_WaterDemand_acft(i,j,1) &
                          - gvf_WaterDemand_acft(i,1,1) ! OK
                        !
                        gii_regDirectInject=gii_regDirectInject+vGP%gvf_cDirectInjection_a(i)
                        !
                      else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_rainWaterHarvest)then
                        vGP%lvf_rainWaterHarvested(i)= gvf_WaterDemand_acft(i,j,12)  &
                            + gvf_WaterDemand_acft(i,j,14)

                      else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_stormWaterCapture)then
                        vGP%lvf_stormWaterCaptured(i)=gvf_WaterDemand_acft(i,j,12)  &
                            + gvf_WaterDemand_acft(i,j,14)

                      else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_grayWater)then
                         vGP%lvf_grayWaterRecovered(i)=gvf_WaterDemand_acft(i,j,12)  &
                            + gvf_WaterDemand_acft(i,j,14)

                      else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_reclaimed)then
                         vLP%lvf_reclaimed(i)=gvf_WaterDemand_acft(i,j,4) &
                          + gvf_WaterDemand_acft(i,j,5)
                         vLP%lvf_reclaimed(i)=gvf_WaterDemand_acft(i,j,12) &
                          + gvf_WaterDemand_acft(i,j,14)

                         gii_regReclaimed=NINT(gii_regReclaimed+vLP%lvf_reclaimed(i))
                        !
                      else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_roReclaimed)then
                         vLP%lvf_ROreclaimed(i)=gvf_WaterDemand_acft(i,j,4) &
                          + gvf_WaterDemand_acft(i,j,5)
                        !
                        gii_regRO=gii_regRO+ vLP%lvf_ROreclaimed(i)
                        !
                      else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_classA)then
                        lvf_SRP_A(i)= gvf_WaterDemand_acft(i,j,4)  & 
                          + gvf_WaterDemand_acft(i,j,5)
                        !
                        gii_regSRPClassA=gii_regSRPClassA+lvf_SRP_A(i)
                        !
                      else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_ClassBC)then
                         lvf_SRP_BandCresAndPumping(i)=gvf_WaterDemand_acft(i,j,5)
                        !
                        gii_regSRPClassBC=gii_regSRPClassBC+lvf_SRP_BandCresAndPumping(i)
                        !
                      else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_cap)then
                       lvf_CAP(i)= gvf_WaterDemand_acft(i,j,4) &
                          + gvf_WaterDemand_acft(i,j,5) 
                        !
                        gii_regCAP=gii_regCAP+lvf_CAP(i)
                        !
!                        lvf_Off_CAP(i)=gvf_WaterDemand_acft(i,j,4) 
!                        lvf_On_CAP(i)=gvf_WaterDemand_acft(i,j,5)
                        !
                      else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_ncs)then
                       lvf_SRP_NCS(i)= gvf_WaterDemand_acft(i,j,4) &
                          + gvf_WaterDemand_acft(i,j,5)
                        !
                        gii_regSRPNCS=gii_regSRPNCS+lvf_SRP_NCS(i)
                        !
!                        lvf_Off_NCS(i)=gvf_WaterDemand_acft(i,j,4) 
!                        lvf_On_NCS(i)=gvf_WaterDemand_acft(i,j,5)
                        !
                      else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_newSupplies)then
                        !
                         vLP%lvf_newSupplies_acft_a(i)=gvf_WaterDemand_acft(i,j,4) &
                          + gvf_WaterDemand_acft(i,j,5)
                        !
                        gii_regNewWater=gii_regNewWater+vLP%lvf_newSupplies_acft_a(i)
                        !
                      else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_banking)then
                        !
                         vLP%lvf_GWbanking(i)=gvf_WaterDemand_acft(i,j,4) &
                          + gvf_WaterDemand_acft(i,j,5)
                         vGP%gvf_cGetWBanking_a(i)=vLP%lvf_GWbanking(i)
                        !
                        gii_regBankedWater=gii_regBankedWater+vLP%lvf_GWbanking(i)
                        !
                      else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_defPump)then
                        lvf_addDefaultPumping(i)=gvf_WaterDemand_acft(i,j,4) &
                          + gvf_WaterDemand_acft(i,j,5)
                        !
                        gii_regDefaultPump=gii_regDefaultPump+lvf_addDefaultPumping(i)
                        !
                      else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_groundWater)then

                        lvf_GWpumpageMunicipal_acft(i)= gvf_WaterDemand_acft(i,j,4) &
                          + gvf_WaterDemand_acft(i,j,5) + lvf_addDefaultPumping(i)
                        !
                        gii_regMuniPump=gii_regMuniPump+lvf_GWpumpageMunicipal_acft(i)
                        !
!                        lvf_Off_GWater(i)=gvf_WaterDemand_acft(i,j,4) 
!                        lvf_On_GWater(i)=gvf_WaterDemand_acft(i,j,5)
                        !
                     else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_unmet)then
                          vLP%lvf_unMetDemand_acft(i)=gvf_WaterDemand_acft(i,j,1)
                         lvf_unmetDemand(i)=0
                        lvf_unmetDemand(i)=gvf_WaterDemand_acft(i,j,1)
                        !
                        gii_regUnmet=gii_regUnmet+lvf_unmetDemand(i)
                        !
                     else


                     endif  
                        !

                        ! 03.16.13 added
                        ! ----------------------------------------------------
                        ! 
!                        lvf_NetOnProjectDemand(i)=lvf_GrossOnProjectDemand(i) &
!                         - (gvf_WaterDemand_acft(i,gpi_reclaimed,5) + gvf_WaterDemand_acft(i,gpi_roReclaimed,5))
!
!                        lvf_NetOffProjectDemand(i)=lvf_GrossOffProjectDemand(i) &
!                         - (gvf_WaterDemand_acft(i,gpi_reclaimed,4) + gvf_WaterDemand_acft(i,gpi_roReclaimed,4))
                        !
                    ! -----------------------------------------------------------------------------------
                    ! Not reall used ... 
                   end do
                  end do
                  !
                  ! 
                  call sMaskCAP(gvl_maskCAP)
                  call sMask(gvl_mask)
                    !
                    ! Leave for now. We may want to revist this output file
                    ! 10.10.2013 das
                    !
 
!                    lvi_One=0; lvi_Two=0; lvi_Three=0; lvi_Four=0; lvi_Five=0; lvi_Six=0
                    ! Changed from 12 to 14 when Agriculture and Environemnt was added on
                    ! 96.25.12 for Erik and Ajay modeling needs (1)

                    ! Now 15 - 08.07.15 das when default groundwater pumping was added
                  do i = 1,gvi_Providers,1
                    if(gvl_writeLog)then
                        ! the variable gpl_writeProviderDotTxt is found in Global.f90
                      if(gpl_writeProviderDotTxt)then
                        if(gvl_APIcleared)then
                    ! ===================================================================================================
                    lvf_One=0; lvf_Two=0; lvf_Three=0; lvf_Four=0; lvf_Five=0; lvf_Six=0; lvf_Seven=0
                    lvf_Eight=0; lvf_Nine=0; lvf_Ten=0; lvf_Eleven=0; lvf_Twelve=0; lvf_Thirteen=0; lvf_Fourteen=0

                          !do j = 1,gpi_unmet ,1
                          do j = 1,gpi_unusedNonPotable,1
                           lvf_One=(gvf_WaterDemand_acft(i,j,1))
                           lvf_Two=(gvf_WaterDemand_acft(i,j,2))
                           lvf_Three=(gvf_WaterDemand_acft(i,j,3))
                           lvf_Four=(gvf_WaterDemand_acft(i,j,4))
                           lvf_Five=(gvf_WaterDemand_acft(i,j,5))
                           !
                           lvf_Six=(gvf_WaterDemand_acft(i,j,6))
                           !
                           lvf_Seven=(gvf_WaterDemand_acft(i,j,7))
                           lvf_Eight=(gvf_WaterDemand_acft(i,j,8))
                           lvf_Nine=(gvf_WaterDemand_acft(i,j,9))
                           lvf_Ten=(gvf_WaterDemand_acft(i,j,10))
                           !
                           lvf_Eleven=(gvf_WaterDemand_acft(i,j,11))
                           lvf_Twelve=(gvf_WaterDemand_acft(i,j,12))
                           lvf_Thirteen=(gvf_WaterDemand_acft(i,j,13))
                           lvf_Fourteen=(gvf_WaterDemand_acft(i,j,14))
                           !
                           ! Write to Provider.txt
                           !  

                           write(4,5)T%year,i,j,lvf_One,lvf_Two,lvf_Three,lvf_Four,lvf_Five,lvf_Six,lvf_Seven, &
                                lvf_Eight,lvf_Nine,lvf_Ten,lvf_Eleven,lvf_Twelve,lvf_Thirteen,lvf_Fourteen

                           !                    
                          end do
                        endif
                      endif
                    endif
                    !
                    !
                  end do
5           format(I4,1x,2(I2,1x),14(F12.1,1x))


                    ! ===================================================================================================
                  !
                  ! -----------------------------------------------------------------------------------
                  ! 10 provider NCS
                  call sMaskSRP(gvl_maskSRP)
                    lvi_countSRP=1
                  do i = 1,gvi_Providers,1
                     lvf_SRP_BandCpump(i)=0
                        lvf_SRP_BandCpump(i)=vLP%lvf_BandCpumpingPass_35_acft(i)
                        !
                         ! DAS 02.10.12
                         lvf_reservoirSRP(i)=0
                        lvf_reservoirSRP(i)=max(0,lvf_SRP_BandCresAndPumping(i)-lvf_SRP_BandCpump(i))
                        !
                       if(gvl_maskSRP(i))then
                         vLP%lvf_NCS(lvi_countSRP)=0 ! 
                         vLP%lvf_NCS(lvi_countSRP)=  lvf_SRP_NCS(i)
                        !
                        vLP%lvf_demand_SRP_used_acft(lvi_countSRP)= lvf_SRP_A(i)+ lvf_SRP_NCS(i)
                        vLP%lvd_providerSRP_classA_35_acft(T%year,i)=lvf_SRP_A(i)
                        vGP%gAclassBCdesignationsUsed_A_maf(T%year,lvi_countSRP)= lvf_SRP_BandCpump(i)* gpd_acftTomaf
                        !
                        lvi_countSRP=lvi_countSRP+1
                       endif
                     !
                     ! WATER BANKING ------
                     call waterBanking(T,i,lvf_actualBankingSW)
                     !
                       vGP%gvf_cSetWBanking_a(i)=lvf_actualBankingSW
                     gvf_WBankingBalance(T%year+1,i,1)=gvf_WBankingBalance(T%year,i,1)+lvf_actualBankingSW

                    ! 08.20.15 
                    ! From Designations_CO.f90; go_waterBankCAP4 = 
                    !  total available to bank priority four, etc.
                    ! -------------------------------------------------------------------------
                    go_waterBankCAP4(i)=nint(vGP%gvf_cSetWBanking_a(i)*gvf_SWtoWBCAPratioP4(i))
                    go_waterBankCAP5(i)=nint(vGP%gvf_cSetWBanking_a(i)* (1-gvf_SWtoWBCAPratioP4(i)))

                    go_CAPunusedPriority4(i)= gvf_parm_SWtoWBamtCAP(i)*gvf_SWtoWBCAPratioP4(i) &
                        - (vLP%lvf_GWbanking(i)*gvf_SWtoWBCAPratioP4(i))
                    go_CAPunusedPriority5(i)= gvf_parm_SWtoWBamtCAP(i)*(1-gvf_SWtoWBCAPratioP4(i)) &
                        - (vLP%lvf_GWbanking(i)*(1-gvf_SWtoWBCAPratioP4(i)))

                    ! -------------------------------------------------------------------------

                      ! Save balance for Kernel.f90 loop
                      gvf_GW_Banking_Hold(i)=gvf_WBankingBalance(T%year+1,i,1)
                      !
                      ! ----------------------------------------------------------------------------------------------

                        ! Magical Water Supplies- sent to Water_CityModel (from Interface)
                        ! 02.21.13
                        ! ---------------------------------------------------
                         vGP%lvf_newWaterSupplies_AF_a(i)=0.
                        vGP%lvf_newWaterSupplies_AF_a(i)=gvf_newSuppliesUsed_acft_a(i) 
                        !
                      ! FOR GPCD calculations; do not include banked or stored water
                     lvf_subtWater(i)=0
                    if(0 < gvf_parm_WStoDIamount(i) .or. 0 < gvf_parm_SWtoVadoseAmt(i))then
                      if(gvl_maskCAP(i))then
                       lvf_subtWater(i)=gvf_parm_WStoDIamount(i)+gvf_parm_SWtoVadoseAmt(i)
                      endif

                      ! Have surface water
                      lvf_otherWater(i)= vLP%lvf_reclaimed(i)+  vLP%lvf_ROreclaimed(i)+  & 
                        lvf_GWpumpageMunicipal_acft(i) 
                    else
                      ! 
                      lvf_otherWater(i)= vLP%lvf_reclaimed(i)+  vLP%lvf_ROreclaimed(i)+  & 
                        lvf_GWpumpageMunicipal_acft(i) 
                    endif

                      ! send SRP pumping to CitiWaterBudgets.f90
                      ! -------------------------------------------------------
                      vLP%lvf_providerSRP_NCS_35_acft(T%year,i)= lvf_SRP_NCS(i)
                      vLP%lvd_providerSRP_total_35_acft(T%year,i)= lvf_SRP_A(i)+ lvf_SRP_NCS(i) + lvf_SRP_BandCresAndPumping(i)
                      vLP%lv_ProvidersSRPpumping_acft(T%year,i)=lvf_SRP_BandCpump(i)
                      vGP%oSRPpumping_a(i)=lvf_SRP_BandCpump(i)*gpd_acftTomaf   
                        !
                        ! 10.16.14 added (then removed) here after the groundwater banking/ CAP designations code above
                        lvf_CAP(i)=lvf_CAP(i) !+  gvf_usedBankedWater_CAP(i)
                        !
                      vLP%lvf_CAP_used_acft(i)=lvf_CAP(i)
                      ! Pass to monthly: all water used
                      vLP%lvd_demand_designations(i)=(lvf_CAP(i)+lvf_SRP_A(i)+ lvf_SRP_NCS(i) + lvf_SRP_BandCresAndPumping(i) &
                        +   vLP%lvf_reclaimed(i)+  vLP%lvf_ROreclaimed(i)+  vLP%lvf_GWbanking(i))*gpd_acftTomaf
                      !
                      vGP%oGWpumpingNoSRP_a_maf(i)= lvf_GWpumpageMunicipal_acft(i)*gpd_acftTomaf
                       vGP%oGWrecharge_a_maf(i)=0.
                      vGP%oSWandSRPused_a_maf(i)=vLP%lvd_demand_designations(i)
                    !
                    vGP%gvf_cCAP_used_acft_a(i)     = vLP%lvf_CAP_used_acft(i)
                    !
                    ! ========================================================================================================
                    ! 
                    ! Send to CityModel
                    ! ------------------
                    ! das
                    ! ==================================================================
                    vGP%gvf_cSRP_A_used_acft_a(i)   =lvf_SRP_A(i)
                    vGP%gvf_cSRP_BC_used_acft_a(i)  =lvf_SRP_BandCpump(i)
                    vGP%gvf_cSRP_NCS_used_acft_a(i) =lvf_SRP_NCS(i)
                    vGP%gvf_cSRP_BCresAndPump_acft_a(i)= lvf_SRP_BandCresAndPumping(i)
                    vGP%gvf_cMunicipalPumping_acft(i)=lvf_GWpumpageMunicipal_acft(i)
                    !
                    ! NEW for WaterSim 6
                    ! 04.19.16
!                     --  see above for where these are set --
!                    vGP%lvf_excessNonPotable;vGP%lvf_outdoorNonPotable ;vGP%lvf_outdoorPotable
!                    vGP%lvf_indoorNonPotable; vGP%lvf_indoorPotable
!                    vGP%lvf_unmetIndoorPotable;vGP%lvf_unmetOutdoorPotable

                    ! ==================================================================
                    !
                    !

1 format(I4,1x,I2,1x,5(F14.6,1x))
                  end do
                  ! -------------------------------------------
                  !
                    call sMask(gvl_mask)
                  do k = 1,gvi_Providers,1

                    ! Send outputs to the interface
                    ! ------------------------------------------------------------------------------
                    !
                    ! Added on 03.16.13 das
                    ! Modified on 10.04.15 das
                     go_newSupplies_acft_a(k)=0
                    if(T%year == T%startyear)then          
                     if(gvf_newSuppliesUsed_acft_a(k) < 1)go_newSupplies_acft_a(k)=gvf_newSuppliesUsed_acft_a(k)
                    else
                     !go_newSupplies_acft_a(k)=nint(vLP%lvf_newSupplies_acft_a(k))

                    ! 10.04.14
                      if(gvi_baseYear <= T%year)then
                        go_newSupplies_acft_a(k)=nint(gvf_newSuppliesUsed_acft_a(k))
                      else
                        go_newSupplies_acft_a(k)=0
                      endif
                    endif
                    ! Added on 03.16.13 (moved from City_model.f90)
                     go_DemandDeficit_acft(k)=0
                    go_DemandDeficit_acft(k)=nint(lvf_unmetDemand(k))
                    !
!                     go_OnProjectNetDemand_acft(k)=0
!                    go_OnProjectNetDemand_acft(k)=  nint(lvf_NetOnProjectDemand(k))
!
!                     go_OffProjectNetDemand_acft(k)=0
!                    go_OffProjectNetDemand_acft(k)=nint(lvf_NetOffProjectDemand(k))
!                    !
                    ! Checked: 02.10.12
                    ! ===========================
                     vGP%gvf_cCAP_acft(k)=0.
                    vGP%gvf_cCAP_acft(k)=vGP%gRealizedDesignationCAP_acft(T%year,k)

                     lvf_capAvailable(k)=lid_designationsCAP(k,1) + lid_designationsCAP(k,2)
                    call sEightyPercentCAP(k,vGP%gvf_cCAP_acft,lvf_capAvailable,gvl_removeRestrictionsCO)

                    ! send SRP pumping to the Interface
                    ! ------------------------------------
                    ! Checked: 02.10.12 lvf_SRP_BandCpump
                     go_GWPumpageSRP_acft(k)=0
                    go_GWPumpageSRP_acft(k)=nint(lvf_SRP_BandCpump(k))

                   ! Not currently used in the API
                   ! Total provider water used
                    ! ------------------------------
                    ! Checked 02.10.12
                     go_ProviderWaterUsed_acft(k)=0
                    go_ProviderWaterUsed_acft(k)= nint(lvf_CAP(k)+lvf_SRP_A(k)+ lvf_SRP_NCS(k)+ lvf_SRP_BandCresAndPumping(k) &
                      + vLP%lvf_reclaimed(k)+  vLP%lvf_ROreclaimed(k)+  vLP%lvf_GWbanking(k) + lvf_GWpumpageMunicipal_acft(k))
                    !
                    gvf_reclaimedWater(k)=vLP%lvf_reclaimed(k)+  vLP%lvf_ROreclaimed(k)
                    !
                     ! Municipal pumping
                    ! --------------------------
                    ! Checked 02.10.12
                     go_GWPumpageMuni_acft(k)=0
                    go_GWPumpageMuni_acft(k)    = anint(lvf_GWpumpageMunicipal_acft(k))

                    ! Not doing groundwater recharge at the moment
                    go_GWRecharge_acft(k)       = 0                 
                    ! --------------------------------

                    ! New definition as of 02.10.12 -das to be consistant with GWDesignations(k)
                    ! ---------------------------
                    ! Checked: 02.10.12
                     go_SWdesignations_acft(k)=0
                    go_SWdesignations_acft(k)   = nint(lvf_CAP(k)+lvf_SRP_A(k)+ lvf_SRP_NCS(k)+ lvf_reservoirSRP(k) ) ! acft a-1
                    ! 

                    ! Legal, available CO River water from CAP
                    ! ----------------------------------------
                    ! Checked: 02.10.12
                     go_annualDeliveriesCO(k)=0
!                    go_annualDeliveriesCO(k)   =NINT(vGP%gRealizedDesignationCAP_acft(T%year,k))     ! acft a-1
                    ! On 09.05.12 I changed this to what they are actually taking.
                    ! Later on we will add a variable to tract legal entitlements to the water
                    go_annualDeliveriesCO(k)   =NINT(lvf_CAP(k))     ! acft a-1

                    ! Going to want to add a variable for USED CO water

                    ! Legal amount of CAP in their contract (and purchased water)
                    ! -------------------------------------
                    ! Checked 02.10.12
                     go_maxDeliveriesCO(k)=0
                    go_maxDeliveriesCO(k)      =NINT(lid_designationsCAP(k,1) +lid_designationsCAP(k,2) )   

                    ! GPCD of various sorts
                      lvf_addWater=0
                      lvf_addWater              =max(0,(lvf_otherWater(k) + lvf_CAP(k)+lvf_SRP_A(k)+ lvf_SRP_NCS(k)+lvf_SRP_BandCpump(k))-lvf_subtWater(k))
                     go_adjustedGPCD(k)=0
                    go_adjustedGPCD(k)          = nint(max(0,lvf_addWater* (1./T%days) *gpd_galperacft* (1./vGP%cPopProvider(k))))
                    !
                     go_providersGPCD(k)=0
                    go_providersGPCD(k)        =nint(max(0,vGP%oDemandNoAg_acft(k)* (1./T%days) *gpd_galperacft*1./vGP%cPopProvider(k)))
                    !
                  end do
2 format(I4,1x,I2,1x,7(I8,1x),2(I4,1x))
!                  do k = 1,10,1
                  call sMaskSRP(gvl_maskSRP)
                    lvi_countSRP=1

                    do i = 1,gvi_Providers,1
                     lvf_srpAvailable(i)=0
                    if(gvl_maskSRP(i))then

                        ! Class A designations
                        ! -------------------------------------
                        ! Checked 02.10.12
                        go_classAdesignations(lvi_countSRP)  =0
                      go_classAdesignations(lvi_countSRP)   =NINT(lvf_SRP_A(i))             ! acft a-1

                         ! Class BC designations
                        ! -------------------------------------
                        ! Checked 02.10.12
                        go_classBCdesignations(lvi_countSRP)  =0
                      go_classBCdesignations(lvi_countSRP)  =NINT( lvf_SRP_BandCresAndPumping(i))             ! acft a-1


                        ! Class BC maximum designations
                        ! -------------------------------------
                        ! Checked 02.10.12
                        go_classBCmax(lvi_countSRP)           =0
                      go_classBCmax(lvi_countSRP)           =NINT(vGP%gAclassBCdesignationsMax_10_maf(T%year,lvi_countSRP)*(1./gpd_acftTomaf) )
             
                        ! Class BC designations from surface water
                        ! -----------------------------------------
                        ! Cannot Check 02.10.12- not in the C# API
                       go_classBCstorage_acft(lvi_countSRP)      =0
                     go_classBCstorage_acft(lvi_countSRP)      =nint(lvf_reservoirSRP(i))

!                        go_NCS_acft(lvi_countSRP)             =0
!                      go_NCS_acft(lvi_countSRP)             =NINT(lvf_SRP_NCS(i))
!
!                        go_NCSmax_acft(lvi_countSRP)          =0
!                      go_NCSmax_acft(lvi_countSRP)          =NINT((1./gpd_acftTomaf)*vLP%lvf_maxNCS(lvi_countSRP) )
!
                        ! Class ABC designations delivered
                        ! -------------------------------------
                        ! Checked 02.10.12
                        go_annualDeliveriesSRP(lvi_countSRP)  =0                  
                      go_annualDeliveriesSRP(lvi_countSRP)  =NINT(lvf_SRP_A(i)+ lvf_SRP_NCS(i)+lvf_SRP_BandCresAndPumping(i))  ! maf a-1
                                            ! Class ABC designations max
                        ! -------------------------------------
                        ! 
                        go_maxDeliveriesSRP(lvi_countSRP)     =0
                      go_maxDeliveriesSRP(lvi_countSRP)     =NINT((1./gpd_acftTomaf)*  (lid_designationsSVT(42,lvi_countSRP) &
                        +vGP%gAclassBCdesignationsMax_10_maf(T%year,lvi_countSRP)+vLP%lvf_maxNCS(lvi_countSRP)))

!                        lvf_srpAvailable(i)=lvf_SRP_A(i)+ lvf_SRP_BandCresAndPumping(i)
                        ! Line 440 in Designations_SVT.f90
                          lvf_srpAvailable(i)= gvf_availableSRP_acft(lvi_countSRP)
                        call sEightyPercentSRP(lvf_srpAvailable(i),gvf_designationsCAPandSRP(i,2),gvl_removeRestrictionsSVT(i))
                        !
                        ! 02.17.15 NEW sustainability indicator for Ag water used relative to total possible using 2014 as threshold
                      !
                      lvi_countSRP=lvi_countSRP+1
                      !
                    end if
                    !
                     ! Standard outputs for checking consistency in simulations across version changes
 
                  end do
                  !
3 format(I4,1x,2(I2,1x),8(I8,1x))
4 format(I4,1x,3(I8,1x),8(I8,1x))
                  ! Send outputs to the interface: annual outputs
                  ! -----------------------------------------------------------------
                    !
                    go_mandiCAP=0
                   go_mandiCAP                  =nint(vGP%gCAPMandI_II(T%year))     ! acft a-1
                  !
                  ! ------------------------------------------------------------------
                !
                    ! ------------------------------------------
                    ! 02.18.15
                    ! Sustainability Indicator for Ag
                    ! "Other" (SRP) water NOT included for now
                    ! DAS
                    ! -------------------------------
                    ! 03.12.15 found in Designations_CO.f90
                    !lvf_sumAgCAPCredits=sum(gvf_AgCAPused_AF) ! sum(go_AgWaterCAPavailable_AF)
                    !lvf_sumAgSRPCredits=sum(go_AgWaterSRP_AF)
                    !lvf_sumAgGWCredits=sum(go_AgWaterGW_AF)
                    !
                    ! gvf_AgCAPpotential_AF=cumulative estimate of lvf_addSurfWaterFromAgCAP(i)
                    ! On 08.25.15 I switched this from potential to actual used..
                    ! lvf_sumAgTotalCredits=lvf_sumAgCAPCredits+ lvf_sumAgGWCredits!  
                    !
!                    do i = 1,gvi_Providers,1
!                      go_AgWaterUsedByMuni_PCT_a(i)=0
!                       lvf_sumAgTotalCredits(i)= 0
!                      lvf_sumAgTotalCredits(i)= go_AgWaterGW_AF(i) + gvf_AgCAPpotential_AF(i)
!                    end do          !
                    !
                    !lvf_countAG=0
!                if(gvl_modelVinzeJohnston)then
!                else
                    !
!                    call sMaskAgriculture(gvl_mask)
!                    !
!                    ! 03.16.15 DAS
!                    ! Mask added on 08.29.16 das
!                    ! --------------------------------------------
!                    
!                    if(0 < sum(lvf_sumAgTotalCredits))then
!                        ! Correct for rounding errors
!                         do i = 1,gvi_Providers,1
!                          lvf_sumAgTotalCredits(i)=0
!                            !
!                           if(gvl_mask(i))then
!                            !
!                             if(gvf_AgCAPpotential_AF(i) < gvf_AgCAPused_AF(i))then
!                              gvf_AgCAPused_AF(i)=gvf_AgCAPpotential_AF(i)
!                             endif
!                             if(gvf_AgGWpotential_AF(i) < go_AgWaterGW_AF(i))then
!                              go_AgWaterGW_AF(i)=gvf_AgGWpotential_AF(i)
!                             endif
!                           endif
!                         end do
!                       !
!                         lvf_agRatio=0
!                        lvf_agRatio=(gvi_AgPumpCurveIndex*3.33333333333)/100                     
!                       do i = 1,gvi_Providers,1
!                          lvf_temp=0
!                          lvf_sumAgTotalCredits(i)= go_AgWaterGW_AF(i) + gvf_AgCAPpotential_AF(i)
!                          !
!                        if(gvl_mask(i))then
!                          !
!                            if(0 < gvf_AgCAPpotential_AF(i))then
!                              lvf_AgCAP_PCT(i)=0
!                              if(0 < lvf_sumAgTotalCredits(i))then
!                                lvf_AgCAP_PCT(i)=((gvf_AgCAPused_AF(i)/gvf_AgCAPpotential_AF(i)) * 100) * (gvf_AgCAPused_AF(i)/lvf_sumAgTotalCredits(i))
!                              endif
!                            endif
!                            if(0 < sum(go_AgWaterGW_AF))then
!                              lvf_AgGW_PCT(i)=0
!                              lvf_temp=go_AgWaterGW_AF(i)
!                                if(0 < lvf_sumAgTotalCredits(i))then
!                                  lvf_AgGW_PCT(i)= (((lvf_temp*lvf_agRatio)/ lvf_temp ) * 100) * (lvf_temp/lvf_sumAgTotalCredits(i))
!                                endif
!                            endif
!                            !
!!                            go_AgWaterUsedByMuni_PCT_a(i)=nint(lvf_AgCAP_PCT(i)+lvf_AgGW_PCT(i))
!                            if(gvi_AgPumpCurveIndex < 1)go_AgWaterUsedByMuni_PCT_a(i)=0
!                        endif
!                      end do
!!1003          format(I4,1x,I2,1x,I4,1x,4(F10.2,1x)) !format(I4,1x,I2,1x,3(F(10.4,1x)))
!
!!                        lvf_temp = fSimpleMovingAverage(lvf_TsimYear,lvf_realOut)
!
!                    else
!                     do i = 1,gvi_Providers,1
!                        go_AgWaterUsedByMuni_PCT_a(i)=0
!                     end do
!                    endif
                !
              !  endif

              endif
             !lvf_AgGW_PCT= ((lvf_agGWused/gvf_AgGWpotential_AF(i) ) * 100) *  (lvf_sumAgGWCredits*(1/lvf_sumAgTotalCredits))

          !
           return
          end subroutine aDemandSupply
          !---------------------------

          ! --------------------------- 
            subroutine waterBanking(T,i,lvf_banked)
                ! ---------------- Types------------------------------------
                integer :: i

                real :: lvf_CAPunused(gvi_maxProV),lvf_SRPunused(gvi_maxProV)
                real :: lvf_cSetWBanking(gvi_maxProV)!,lvf_pAddToSRPrelease(gvi_maxProV)
                real :: lvf_difference(gvi_maxProV),lvf_actualBankingSW(gvi_maxProV)
                real :: lvf_banked
                ! ========================================================

                ! - Type Constructs ----
                type(runTime)T
                ! ======================
                    !
                       lvf_CAPunused(i)=0
                       lvf_SRPunused(i)=0
                        ! gvf_parm_SWtoWBamtCAP(i) is a global parameter
                      lvf_CAPunused(i)= gvf_parm_SWtoWBamtCAP(i)

                       lvf_cSetWBanking(i)= 0
                      ! lvf_pAddToSRPrelease(i)=0
                      ! ----------------------------------------------------------
                      ! 10.02.15 DAS
                      ! No meaning yet
                      ! lvf_SRPunused(i)= NCS water - but not currently connected
                      !
                     lvf_actualBankingSW(i)=0
                    ! This is constant for some water providers
                    !  gvf_parm_SWtoWBamount(i) from the interface kernel
                    !
                    ! if  gvf_parm_SWtoWB(i) is set to zero, default banking still occurs.
                    ! This logic may need re-thinking. 10.11.15 das
                    ! ---------------------------------------------

                    if( 0 < gvf_parm_SWtoWBamount(i))then
                        !
                         if(0 < lvf_CAPunused(i))then
                           if(gvf_parm_SWtoWBamount(i) < lvf_CAPunused(i))then
                             lvf_actualBankingSW(i)=gvf_parm_SWtoWBamount(i)
                             lvf_difference(i)=lvf_CAPunused(i)-lvf_actualBankingSW(i)
                           else
                             lvf_actualBankingSW(i)=lvf_CAPunused(i)
                             lvf_difference(i)=0
                           endif
                         else
                            lvf_actualBankingSW(i)=0
                            lvf_difference(i)=0
                         endif
                       lvf_cSetWBanking(i)= lvf_actualBankingSW(i)
                    endif
                    !
                    ! This is optional
                    if(gvi_WBankingOption(i) == 1)then
                      if(0 < gvf_parm_SWtoWB(i) .and. gvf_parm_SWtoWB(i) <= 1)then
                        ! Because the call to providers follows banking, to make 
                        ! this policy effective on the policy year I subtract one year
                        ! 10.11.15 das
                        if(gvi_baseYear <= T%year-1)then
                            if(0 < lvf_difference(i))then
                              ! Code prior to 10.04.15 das
                              ! lvf_cSetWBanking_a(i)= gvf_parm_SWtoWB(i)*(lvf_SRPunused(i)+lvf_CAPunused(i))
                              ! ---------------------
                              lvf_cSetWBanking(i)= lvf_cSetWBanking(i) + gvf_parm_SWtoWB(i)*(lvf_SRPunused(i)+lvf_difference(i))
!                              lvf_pAddToSRPrelease(i)=(gvf_parm_SWtoWB(i)*lvf_SRPunused(i)) * gpd_acftTomaf
                            else
                            endif
                        else
                             lvf_cSetWBanking(i)=0
!                             lvf_pAddToSRPrelease(i)=0
                        endif
                       
                      endif
                    else
                    endif
                !
                lvf_banked= lvf_cSetWBanking(i)
                !
               !
              return
            end subroutine waterBanking
          ! ---------------------------
            ! ------------------------------------
            subroutine clearVariables(T,vGP)
                ! -------- types -------
                integer :: i
                ! ======================

                ! - Type Constructs ----
                type(Provider)vGP
                type(runTime)T
                ! ======================
                    !
                    do i = 1,gvi_Providers,1
                     vGP%lvf_excessNonPotable(i)=0
                     vGP%lvf_outdoorNonPotable(i)=0
                     vGP%lvf_outdoorPotable(i)=0
                     vGP%lvf_indoorNonPotable(i)=0
                     vGP%lvf_indoorPotable(i)=0
                     vGP%lvf_unmetIndoorPotable(i)=0
                     vGP%lvf_unmetOutdoorPotable(i)=0
                     !

                    enddo
                    !
              return
            end subroutine clearVariables
            ! ------------------------------------
            ! ------------------------------------
            subroutine sendToCityModelNewVar(T,vGP)
                ! -------- types -------
                integer :: i,j
                ! ======================

                ! - Type Constructs ----
                type(Provider)vGP
                type(runTime)T
                ! ======================
                    !
                  do i = 1,gvi_Providers,1
                    !
                    vGP%lvf_outdoorNonPotable(i)=0
                    vGP%lvf_indoorNonPotable(i)=0
                    go_harvRainWaterUsed_AF(i)=0
                    go_stormWaterUsedUsed_AF(i)=0
                    go_graywaterWaterUsed_AF(i)=0
                    gvf_reclaimedUseOutdoors(i)=0
                    !
                    do j = 1,gpi_unusedNonPotable,1

                       if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_rainWaterHarvest)then
                        !
                        vGP%lvf_outdoorNonPotable(i)=gvf_WaterDemand_acft(i,j,12)+gvf_WaterDemand_acft(i,j,14)
                        vGP%lvf_indoorNonPotable(i)=gvf_WaterDemand_acft(i,j,11)+gvf_WaterDemand_acft(i,j,13)
                        !
                        go_harvRainWaterUsed_AF(i)=gvf_WaterDemand_acft(i,j,12)+gvf_WaterDemand_acft(i,j,14)
                        !
                       else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_stormWaterCapture)then
                        !
                        vGP%lvf_outdoorNonPotable(i)= vGP%lvf_outdoorNonPotable(i) &
                         + gvf_WaterDemand_acft(i,j,12)+gvf_WaterDemand_acft(i,j,14)
                        !
                        go_stormWaterUsedUsed_AF(i)=gvf_WaterDemand_acft(i,j,12)+gvf_WaterDemand_acft(i,j,14)
                        !
                        vGP%lvf_indoorNonPotable(i)=  vGP%lvf_indoorNonPotable(i) &
                         + gvf_WaterDemand_acft(i,j,11)+gvf_WaterDemand_acft(i,j,13)
                        !                      
                       else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_grayWater)then
                        !
                        vGP%lvf_outdoorNonPotable(i)= vGP%lvf_outdoorNonPotable(i) &
                          + gvf_WaterDemand_acft(i,j,12)+gvf_WaterDemand_acft(i,j,14)
                        !
                        go_graywaterWaterUsed_AF(i)=gvf_WaterDemand_acft(i,j,12)+gvf_WaterDemand_acft(i,j,14)
                        !
                        vGP%lvf_indoorNonPotable(i)=vGP%lvf_indoorNonPotable(i) &
                          + gvf_WaterDemand_acft(i,j,11)+gvf_WaterDemand_acft(i,j,13)
!                        vGP%lvf_grayWaterRecovered(i)=vGP%lvf_outdoorNonPotable(i)+vGP%lvf_indoorNonPotable(i)
                        !
                       else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_reclaimed)then
                        !
                        vGP%lvf_outdoorNonPotable(i)= vGP%lvf_outdoorNonPotable(i) &
                          + gvf_WaterDemand_acft(i,j,12)+gvf_WaterDemand_acft(i,j,14)
                        !
                        gvf_reclaimedUseOutdoors(i)=gvf_WaterDemand_acft(i,j,12)+gvf_WaterDemand_acft(i,j,14)
                        !
                        vGP%lvf_indoorNonPotable(i)=vGP%lvf_indoorNonPotable(i) &
                          + gvf_WaterDemand_acft(i,j,11)+gvf_WaterDemand_acft(i,j,13)
                        !
                       else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_roReclaimed)then
                        !
                        vGP%lvf_outdoorPotable(i)=gvf_WaterDemand_acft(i,j,12)+gvf_WaterDemand_acft(i,j,14)
                        vGP%lvf_indoorPotable(i)=gvf_WaterDemand_acft(i,j,11)+gvf_WaterDemand_acft(i,j,13)
                       else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_defPump)then
                        !
                        vGP%lvf_outdoorPotable(i)= vGP%lvf_outdoorPotable(i) &
                          + gvf_WaterDemand_acft(i,j,12)+gvf_WaterDemand_acft(i,j,14)
                        vGP%lvf_indoorPotable(i)=vGP%lvf_indoorPotable(i) &
                          + gvf_WaterDemand_acft(i,j,11)+gvf_WaterDemand_acft(i,j,13)
                        !
                       else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_classA)then
                        !
                        vGP%lvf_outdoorPotable(i)= vGP%lvf_outdoorPotable(i) &
                          + gvf_WaterDemand_acft(i,j,12)+gvf_WaterDemand_acft(i,j,14)
                        vGP%lvf_indoorPotable(i)=vGP%lvf_indoorPotable(i) &
                          + gvf_WaterDemand_acft(i,j,11)+gvf_WaterDemand_acft(i,j,13)
                        !
                       else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_ClassBC)then
                        !
                        vGP%lvf_outdoorPotable(i)= vGP%lvf_outdoorPotable(i) &
                          + gvf_WaterDemand_acft(i,j,12)+gvf_WaterDemand_acft(i,j,14)
                        vGP%lvf_indoorPotable(i)=vGP%lvf_indoorPotable(i) &
                          + gvf_WaterDemand_acft(i,j,11)+gvf_WaterDemand_acft(i,j,13)
                        !
                       else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_cap)then
                        !
                        vGP%lvf_outdoorPotable(i)= vGP%lvf_outdoorPotable(i) &
                          + gvf_WaterDemand_acft(i,j,12)+gvf_WaterDemand_acft(i,j,14)
                        vGP%lvf_indoorPotable(i)=vGP%lvf_indoorPotable(i) &
                          + gvf_WaterDemand_acft(i,j,11)+gvf_WaterDemand_acft(i,j,13)
                        !
                       else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_ncs)then
                        !
                        vGP%lvf_outdoorPotable(i)= vGP%lvf_outdoorPotable(i) &
                          + gvf_WaterDemand_acft(i,j,12)+gvf_WaterDemand_acft(i,j,14)
                        vGP%lvf_indoorPotable(i)=vGP%lvf_indoorPotable(i) &
                          + gvf_WaterDemand_acft(i,j,11)+gvf_WaterDemand_acft(i,j,13)
                        !
                       else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_newSupplies)then
                        !
                        vGP%lvf_outdoorPotable(i)= vGP%lvf_outdoorPotable(i) &
                          + gvf_WaterDemand_acft(i,j,12)+gvf_WaterDemand_acft(i,j,14)
                        vGP%lvf_indoorPotable(i)=vGP%lvf_indoorPotable(i) &
                          + gvf_WaterDemand_acft(i,j,11)+gvf_WaterDemand_acft(i,j,13)
                        !
                       else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_banking)then
                        !
                        vGP%lvf_outdoorPotable(i)= vGP%lvf_outdoorPotable(i) &
                          + gvf_WaterDemand_acft(i,j,12)+gvf_WaterDemand_acft(i,j,14)
                        vGP%lvf_indoorPotable(i)=vGP%lvf_indoorPotable(i) &
                          + gvf_WaterDemand_acft(i,j,11)+gvf_WaterDemand_acft(i,j,13)
                        !
                       else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_groundWater)then
                        !
                        vGP%lvf_outdoorPotable(i)= vGP%lvf_outdoorPotable(i) &
                          + gvf_WaterDemand_acft(i,j,12)+gvf_WaterDemand_acft(i,j,14)
                        vGP%lvf_indoorPotable(i)=vGP%lvf_indoorPotable(i) &
                          + gvf_WaterDemand_acft(i,j,11)+gvf_WaterDemand_acft(i,j,13)
                        !
                       else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_unmet)then
                         vGP%lvf_unmetIndoorPotable(i)=gvf_WaterDemand_acft(i,j,7)+gvf_WaterDemand_acft(i,j,9)
                         vGP%lvf_unmetOutdoorPotable(i)=gvf_WaterDemand_acft(i,j,8)+gvf_WaterDemand_acft(i,j,10)
                       else if(nint(gvf_WaterDemand_acft(i,j,6)) == gpi_unusedNonPotable)then
                         vGP%lvf_excessNonPotable(i)=gvf_WaterDemand_acft(i,j,8)+gvf_WaterDemand_acft(i,j,10)
                       endif

                    end do
                    !
                    !
                  end do
                    !
                  do i = 1,gvi_Providers,1
                    gvf_nonPotableTotalUsed(i)=  vGP%lvf_outdoorNonPotable(i)+vGP%lvf_indoorNonPotable(i)
                  end do
                    !
              return
            end subroutine sendToCityModelNewVar
            ! ------------------------------------



         ! ----------------------------
          subroutine aDifference(T,vGP)
            !
            ! ----------- Types --------
            integer :: i
            ! ==========================
            !

            ! - Type Construct -
             type(Provider)vGP
             type(runTime)T
            ! ==================
              !
!              E%MySub='SupplyDemand'
!              E%TStep=1
              !
              do i = 1,gvi_Providers,1
                !
                if(gvl_IwaniecYN)then
                    ! 12.10.16,12.13.16
                    ! gvf_OutdoorResProp(1) = low density, "2" is medium density, and "3" is high density housing
                    ! Thus,  gvf_parm_OutDoorResProp(i) is calculated in subroutine sPartitionPopulation
                    ! as the normalized estimate from all three
                    !
                endif
100 format(I4,1x,I2,3x,F6.3)
                ! need to confirm these assumptions
                ! 04.01.13 
                ! -------------------------------
                  if(gvl_modelVinzeJohnston)then
                    gvf_parm_OutDoorIndProp(i)= 0.1
                    gvf_WaterToCommercialTurf_Prop=max(0.75,gvf_parm_OutDoorComProp(i))
                    gvf_parm_WWtoIndReuse(i)=1-gvf_parm_WWtoEffluent(i)
                  endif
               ! -----------------------------------------------------------------
                    vGP%lvf_cDemand_Res_out_AF_a(i)=anint(gvf_TotalDemand_acft(i) &
                     * gvf_parm_WStoRes_prop(i)*gvf_parm_OutDoorResProp(i))

                    vGP%lvf_cDemand_Res_In_AF_a(i) =anint(gvf_TotalDemand_acft(i) &
                     * gvf_parm_WStoRes_prop(i)*(1-gvf_parm_OutDoorResProp(i)))

                    vGP%lvf_cDemand_Ind_out_AF_a(i)=anint(gvf_TotalDemand_acft(i) &
                     * gvf_parm_WStoInd_prop(i)*gvf_parm_OutDoorIndProp(i))

                    vGP%lvf_cDemand_Ind_In_AF_a(i) =anint(gvf_TotalDemand_acft(i) &
                     * gvf_parm_WStoInd_prop(i)*(1-gvf_parm_OutDoorIndProp(i)))
                    !
                    ! 08.08.12 Ajay & Johnston Project
                    ! --------------------------------------------------------------------------------
                ! - 04.01.13
                    if(gvl_modelVinzeJohnston)then
                        vGP%lvf_cDemand_Com_out_AF_a(i)=anint(gvf_TotalDemand_acft(i) &
                         * gvf_parm_WStoCom_prop(i)*gvf_WaterToCommercialTurf_Prop)

                        vGP%lvf_cDemand_Com_In_AF_a(i) =anint(gvf_TotalDemand_acft(i) &
                         * gvf_parm_WStoCom_prop(i)*(1-(gvf_WaterToCommercialTurf_Prop)))
                    else
                        vGP%lvf_cDemand_Com_out_AF_a(i)=anint(gvf_TotalDemand_acft(i) &
                         * gvf_parm_WStoCom_prop(i)*gvf_parm_OutDoorComProp(i))

                        vGP%lvf_cDemand_Com_In_AF_a(i) =anint(gvf_TotalDemand_acft(i) &
                         * gvf_parm_WStoCom_prop(i)*(1-gvf_parm_OutDoorComProp(i)))
                    endif
                    !
              ! -----------------------------------------------------------------
              end do
                ! New Subroutine to send water use data to Water_CityModel.f90
                ! 04.19.16
                call sendToCityModelNewVar(T,vGP)
                !
            return
          end subroutine aDifference
         ! --------------------------
     !
End Module lms_Provider
!
    ! -----------------------------------
    subroutine NexchangeVariables(Sin,vLP)
     use lms_Provider
      use gm_ModelControl
       use gm_TypeControl
        !
        ! ---- Types ----
        integer :: i,k
        ! ===============
        !
        ! - Type Construct ----
        type(LocalProvider)vLP
        type(Surfacewater)Sin
        ! =====================
           !
          do k = 1,10,1
            ! Are these working?
           vLP%lvf_maxNCS(k)=Sin%sNCSmaxAllocations_maf(k)
          end do
           !
           vLP%lvf_BandCpumpingPass_35_acft=Sin%gvf_pumpBandC_35_acft

          do i = 1,gvi_Providers,1
            vLP%lvf_unusedClass_BC_acft(i)=Sin%lvf_unusedClass_BC_acft(i)
          end do
          !
      return
    end subroutine NexchangeVariables
    ! -------------------------------

    ! -----------------------------------
    subroutine pWaterProviders(T,Sin,vGP)
      use lms_Provider
       use gm_ModelControl
        use gm_TypeControl
          !
          ! -- Type Constructs --
          type(Surfacewater)Sin
          type(Provider)vGP
          type(LocalProvider)vLP
          type(runTime)T
          ! =====================
              ! 
              call NexchangeVariables(Sin,vLP)
              call sProvider(T,vGP,vLP)
              call writetodisk_(T)
              !
                if(T%year .EQ. T%startyear)then
                    if(gvl_writeLog)then
                        string=21
                        LU=0
                        call sOnceThrough(string,streamString)
                        call sWrite(streamString,LU)
                    endif
                endif
              !
      return
    end subroutine pWaterProviders
    ! ----------------------------
!
! ======================================================================================================
! E.O.F. Provider.f90