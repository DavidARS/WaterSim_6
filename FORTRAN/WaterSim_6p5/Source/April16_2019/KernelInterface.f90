!
!  File is KernelInterface.f90
!
!  This is the FORTRAN Interface to the C# interface for the model suite
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
! Created on 06.07.10
!
! Last modified on 10.17.13,07.21.14
! ----------------------------------
!

! ==============================================================================================
!
! Run connection and controls
! ---------------------------
!
!
! Data and output paths
! ==================
! C# interface = private static byte[] StringToFixedBytes(int bufferLengthSpecification, string str)
!
subroutine SetStringData (selector, newValue)
 use lm_Kernel
    dll_export :: SetStringData
    integer, intent (in) :: selector
    character, dimension(200), intent (in) :: newValue
    ! Convert from array of single characters into single string
    character(len=200) :: str
    if(selector < 2)then
      write(str,"(200a1)") newValue
      gvc_DPath=trim(str)
    else
   
    endif

 return
end subroutine SetStringData
!
subroutine SetStringOutput (selector, newValue)
 use lm_Kernel
    dll_export :: SetStringOutput
    integer, intent (in) :: selector
    character, dimension(200), intent (in) :: newValue
    ! Convert from array of single characters into single string
    character(len=200) :: str
    if(selector < 2)then
     write(str,"(200a1)") newValue
  
    ! Do something with string
     gvc_OPath=trim(str)
   else
   endif

 return
end subroutine SetStringOutput
! =======================================================!


! Build version
! =======================================================!
! 
function BuildNum() result(numb)
    use lm_BuildNotes
    implicit none
    integer :: numb
    dll_export :: BuildNum

    numb = BuildNumK()
end function
!
function BuildVer() result(numb)
    use lm_BuildNotes
    implicit none
    integer :: numb
    dll_export :: BuildVer

    numb = BuildVerK()
end function
!
function BuildSubVer() result(numb)
    use lm_BuildNotes
    implicit none
    integer :: numb
    dll_export :: BuildSubVer

    numb = BuildSubVerK()
end function
!
function TestMonth() result(numb)
    use lm_BuildNotes
    implicit none
    integer :: numb
    dll_export :: TestMonth
    numb = MonthK()
end function
function TestDay() result(numb)
    use lm_BuildNotes
    implicit none
    integer :: numb
    dll_export :: TestDay
   
    numb = DayK()
end function
function TestYear() result(numb)
    use lm_BuildNotes
    implicit none
    integer :: numb
    dll_export :: TestYear
   
    numb = YearK()
end function
function TestHour() result(numb)
    use lm_BuildNotes
    implicit none
    integer :: numb
    dll_export :: TestHour
    numb = HourK()
end function
function TestMin() result(numb)
    use lm_BuildNotes
    implicit none
    integer :: numb 
    dll_export :: TestMin
    
    numb = MinK()
end function
!
! End of build routines
!
! =======================================================!
!
! Initialize all variables
! ================
! C# interface = protected internal void ResetYear()
!
subroutine Init()
    use lm_Initialize
    implicit none
    dll_export :: Init
    !
    call initK()
 return
end subroutine
! =======================================================!
!
subroutine CloseFortran()
    use lm_Initialize
    implicit none
    dll_export :: CloseFortran
    !
    call closeK()
 return
end subroutine
! =======================================================!
! ==============
! C# interface = protected internal int Readstatus
!
function getStatus() result(value)
 use gm_GlobalData

    implicit none
    integer :: value
    dll_export :: getStatus

    value = gv_exceptionCode
end function
! =======================================================!
!
! ==============
! C# interface = protected internal int KernelCase
!
!function getKernelCase() result(value)
!  use lm_Kernel
!    implicit none
!    dll_export :: getKernelCase
!    integer :: value
!   
!    value = gvi_kernel_case
!end function
!subroutine setKernelCase(value)
!    use lm_Kernel
!    implicit none
!    dll_export :: setKernelCase
!    integer :: value
!
!   gvi_kernel_case=value
! return
!end subroutine
! =======================================================!
!
! Run the model one year
! ==================
! C# interface = protected internal void RunOneYear()
!
subroutine RunOneYear()
    use lm_Kernel

    implicit none
    dll_export :: RunOneYear

    call RunOneYearKernel()
 return
end subroutine RunOneYear
! =======================================================!
!
! Run the model many years
! ==================
! C# interface = protected internal void RunManyYears()
!
subroutine RunManyYears()
    use lm_Kernel

    implicit none
    dll_export :: RunManyYears

    call RunManyYearKernel()
 return
end subroutine RunManyYears
! =======================================================!

! ==============
! C# interface = protected internal void OpenFiles()
!
subroutine OpenFiles()
 use lm_Start
   implicit none
    dll_export :: OpenFiles

    call OpenFilesK()

 return
end subroutine OpenFiles
!

subroutine ReadFilesErr()
 use lm_Start
   implicit none
    dll_export :: ReadFilesErr

    call ErrorCheckReadK()

 return
end subroutine ReadFilesErr
! ===============
! C# interface = protected internal int get_RegionalNaturalRecharge   
!
function getReadFileError() result(value)
    use lm_Start
    implicit none
    dll_export :: getReadFileError
    logical :: value

    value = gvl_readStatus
end function
! ==============
! C# interface = protected internal void CloseFiles()
!
subroutine CloseFiles()
 use lm_Terminate
  implicit none
    dll_export :: CloseFiles

    call CloseFilesK()

 return
end subroutine CloseFiles
!
! ======================================================!
! 
! ==============
! C# interface = protected internal int SimulationStart
!
function getStartSimulation() result(value)
    use lm_Kernel
    implicit none
    dll_export :: getStartSimulation
    integer :: value

   value=gvi_startSimulationYear
end function
subroutine setStartSimulation(value)
    use lm_Kernel
    implicit none
    dll_export :: setStartSimulation
    integer :: value

   gvi_startSimulationYear=value
 return
end subroutine
! ============================================================!
! 
! ==============
! C# interface = protected internal int SimulationEnd
!
function getEndSimulation() result(value)
    use lm_Kernel
    implicit none
    dll_export :: getEndSimulation
    integer :: value

   value=gvi_endSimulationYear
end function
subroutine setEndSimulation(value)
    use lm_Kernel
    implicit none
    dll_export :: setEndSimulation
    integer :: value

   gvi_endSimulationYear=value
 return
end subroutine
! ============================================================!
! 
! ==============
! C# interface = protected internal bool writeToDisk
!
function getWriteStatus() result(value)
    use lm_Kernel
    implicit none
    dll_export :: getWriteStatus
    logical :: value

   value=gvl_WriteDisk
end function
subroutine setWriteStatus(value)
    use  lm_Initialize
    implicit none
    dll_export :: setWriteStatus
    logical :: value

    gvl_WriteDisk=.false.
   if(value)gvl_WriteDisk=value
 return
end subroutine
! ============================================================!
! 
! ==============
! C# interface = protected internal bool writeLog
!
function getWriteLog() result(value)
    use lm_Kernel
    implicit none
    dll_export :: getWriteLog
    logical :: value

   value=gvl_writeLog
end function
subroutine setWriteLog(value)
    use  lm_Initialize
    implicit none
    dll_export :: setWriteLog
    logical :: value

    gvl_writeLog=.false.
   if(value)gvl_writeLog=value
 return
end subroutine
! ============================================================!
!
! ==============
! C# Interface = protected internal bool get_ValidModelRun
!
function getErrorCode() result(value)
    use lm_Kernel
    implicit none
    dll_export :: getErrorCode
    logical :: value

   value=gvl_errorFlag
end function
!
! ============================================================
!! C# Interface =  protected internal bool set_parmAPIcleared
! 
function getAPIcode() result(value)
    use lm_ParameterControl
    implicit none
    dll_export :: getAPIcode
    logical :: value

   value=gvl_APIcleared
end function
subroutine setAPIcode(value)
    use  lm_ParameterControl
    implicit none
    dll_export :: setAPIcode
    logical :: value

   gvl_APIcleared=value
 return
end subroutine
! ---------------------------------------
subroutine setBaseYear(value)
    use  lm_Initialize
    implicit none
    dll_export :: setBaseYear
    integer :: value

    gvi_baseYear=2014
   if(-1 < value)gvi_baseYear=value
 return
end subroutine


! ==============================================================
!subroutine getErrorString (selector, newValue)
!! use lm_Kernel
!!    dll_export :: getErrorString
!    integer, intent (out) :: selector
!    character, dimension(200), intent (out) :: newValue
!!    ! Convert from array of single characters into single string
!!    character(len=200) :: str
!
!    write(newValue,"(200a1)") errorString
!! 
!!   ! Do something with string
!!   gvc_OPath=str
!!   !
!end subroutine getErrorString


! -------------------------------!
!
! ====================================================================================
!
! Over-arching variable(s)
!
function getClimateFactorEndYear() result(value)
    use  lm_ParameterControl
    implicit none
    dll_export :: getClimateFactorEndYear
    integer :: value

    value=gi_ClimateAdjustmentYearEnd

end function
subroutine setClimateFactorEndYear(value)
    use lm_ParameterControl
    implicit none
    dll_export :: setClimateFactorEndYear
    integer :: value
    gi_ClimateAdjustmentYearEnd=85
    if(0 < value) gi_ClimateAdjustmentYearEnd=value
 return
end subroutine
!
! Inputs - Regional Scale
!
! -----------------------------------------------------------------------------!
! -------------------- Salt/Verde River Input Variables ------------------------!
!
! ==============
! C# interface = protected internal int [] ModifyProviderNormalFlowPct
! ----------------------------------------------------------------------
subroutine getModifyNormalFlow(count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: getModifyNormalFlow
    integer :: count,i
    integer, dimension(1:count) :: values
    !
    forall(i=1:count) values(i) = nint(gvf_modifyNormalFlow(i) * 100)
 return
end subroutine
!
subroutine setModifyNormalFlow(count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setModifyNormalFlow
    integer :: count,i
    real :: mydefault=0.95
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=mydefault
    forall(i=1:count, values(i) .gt. 0)
     generic(i) = values(i) * 0.01

    end forall
    !
    forall(i=1:count) gvf_modifyNormalFlow(i)= generic(i) 
 return
end subroutine
! ======================================================
!
!  Runoff data to use see line 55 in WaterShed_SVT.f90
!
! ==============
! C# interface = protected internal int get_SaltVerdeTrace
function getSTVtrace() result(value)
  use lms_River_Aa
    implicit none
    dll_export :: getSTVtrace
    integer :: value
  
    value=gvi_SVTtrace
end function
! ==============
! C# interface = protected internal int set_SaltVerdeTrace
!
subroutine setSVTtrace(value)
    use lms_River_Aa
    implicit none
    dll_export :: setSVTtrace
    integer :: value

    gvi_SVTtrace=30
    if(0 < value)gvi_SVTtrace = value
 return
end subroutine
! 
! =============================================================
!
! ==============
! C# interface = protected internal int SaltVerdeTontoHistoricalData 
!
function getSVTHistData() result(value)
    use lms_River_Aa
    implicit none
    dll_export :: getSVTHistData
    integer :: value

    gv_dataSVT=1
    value=gv_dataSVT

end function
subroutine setSVTHistData(value)
    use lms_River_Aa
    implicit none
    dll_export :: setSVTHistData
    integer :: value
    
    gv_dataSVT=1
    if(1 < value)gv_dataSVT=value
 return
end subroutine
! =============================================================

! Index year (year to start)
! ==============
!
! C# interface = protected internal int SaltVerdeHistoricalExtractionStartYear 
!
function getSVHistExtractionStartYear() result(value)
    use lms_River_Aa
    implicit none
    integer :: value
    dll_export :: getSVHistExtractionStartYear

    value = gv_indexyearSVT
end function
subroutine setSVHistExtractionStartYear(value)
    use lms_River_Aa
    implicit none
    dll_export :: setSVHistExtractionStartYear
    integer :: value

    gv_indexyearSVT=value
 return
end subroutine
! =============================================================
!
! Climate factor (proportional)
! ==============
! C# interface = protected internal int SaltVerdeClimateAdjustmentPercent
!
function getSVClimateAdjustmentPercent() result(value)
    use lms_River_Aa
    implicit none
    integer :: value
    dll_export :: getSVClimateAdjustmentPercent
    real :: percentage

    percentage = gv_climatefactorSVT * 100.
    value = nint(percentage)
end function
subroutine setSVClimateAdjustmentPercent(value)
    use lms_River_Aa
    implicit none
    dll_export :: setSVClimateAdjustmentPercent
    integer :: value

    gv_climatefactorSVT=value * (1/100.)
 return
end subroutine
! =============================================================
! Drought factor start year
! ==============
! C# interface = protected internal int SaltVerdeUserAdjustmentStartYear
!
function getSVUserAdjustmentStartYear() result(value)
    use lms_River_Aa
    implicit none
    dll_export :: getSVUserAdjustmentStartYear
    integer :: value

    value = gv_droughtyearSVT
end function
subroutine setSVUserAdjustmentStartYear(value)
    use lms_River_Aa
    implicit none
    dll_export :: setSVUserAdjustmentStartYear
    integer :: value
    gv_droughtyearSVT=2013
   if(0 < value)gv_droughtyearSVT = value
  return
end subroutine
!
! =============================================================
! Drought factor stop year
! ==============
! C# interface = protected internal int SaltVerdeUserAdjustmentStopYear
!
function getSVUserAdjustmentStopYear() result(value)
    use lms_River_Aa
    implicit none
    dll_export :: getSVUserAdjustmentStopYear
    integer :: value

    value = gv_droughtyearendSVT
end function
subroutine setSVUserAdjustmentStopYear(value)
    use lms_River_Aa
    implicit none
    dll_export :: setSVUserAdjustmentStopYear
    integer :: value

    gv_droughtyearendSVT = value
 return
end subroutine
! =============================================================
! Drought factor (proportional: 0 to > 1)
! ==============
! C# interface = protected internal int SaltVerdeUserAdjustmentPercent
!
function getSVUserAdjustmentPercent() result(value)
    use lms_River_Aa
    implicit none
    dll_export :: getSVUserAdjustmentPercent
    integer :: value

    value = nint(gv_droughtfactorSVT*100.)
end function
subroutine setSVUserAdjustmentPercent(value)
    use lms_River_Aa
    implicit none
    dll_export :: setSVUserAdjustmentPercent
    integer :: value

    gv_droughtfactorSVT = value * 0.01
 return
end subroutine
! =============================================================
!
! ===============
! C# interface = internal int FlowToEnvironment_Verde
!
!subroutine setWaterToEvironmentVerde(value)
!    use  lms_River_Aa
!    implicit none
!    dll_export :: setWaterToEvironmentVerde
!    integer :: value
!    !
!     gvi_WaterToEnvironVerde_acft_a = 0
!    if(0 < value)gvi_WaterToEnvironVerde_acft_a = value
!
!  return
!end subroutine
! =============================================================
!
! ===============
! C# interface = internal int FlowToEnvironment_Salt
!
!subroutine setWaterToEvironmentSalt(value)
!    use  lms_River_Aa
!    implicit none
!    dll_export :: setWaterToEvironmentSalt
!    integer :: value
!    !
!    gvi_WaterToEnvironSalt_acft_a = 0
!    if(0 < value)gvi_WaterToEnvironSalt_acft_a = value
!
!  return
!end subroutine
! --------------------------------------------------
!
! End of Salt-Tonto and Verde inputs
!
!
! ==============================================================================
!
! --------------------------------------------------------------------------------
! -------------------------- Colorado River Input Variables ----------------------!
!
! Runoff data set
! ================
! C# interface = protected internal int set_ColoradoTrace
!
subroutine setCOtrace(value)
    use lms_River_Ba
    implicit none
    dll_export :: setCOtrace
    integer :: value

    gvi_COtrace=30
    if(0 < value)gvi_COtrace = value
 return
end subroutine
! =============================================================
!
! Runoff data set
! ================
! C# interface = protected internal int ColoradoHistoricalData
!
function getCOHistData() result(value)
    use lms_River_ba
    implicit none
    dll_export :: getCOHistData
    integer :: value

    value=gv_dataCO

end function
subroutine setCOHistData(value)
    use lms_River_ba
    implicit none
    dll_export :: setCOHistData
    integer :: value

    gv_dataCO=1
    if(1 < value) gv_dataCO=value
 return
end subroutine
! =============================================================
!
! Index year (start year for trace)
! ================
!
! C# interface = protected internal int ColoradoHistoricalExtractionStartYear
!
function getCOHistExtractionStartYear() result(value)
    use lms_River_ba
    implicit none
    integer :: value
    dll_export :: getCOHistExtractionStartYear

    value = gv_indexyearCO
end function
subroutine setCOHistExtractionStartYear(value)
    use lms_River_ba
    implicit none
    dll_export :: setCOHistExtractionStartYear
    integer :: value
    gv_indexyearCO=1938
   if(0 < value)gv_indexyearCO=value
 return
end subroutine
! =============================================================
!
! Climate Factor
! ===============
! C# interface = protected internal int ColoradoClimateAdjustmentPercent
!
function getCOClimateAdjustmentPercent() result(value)
    use lms_River_Ba
    implicit none
    integer :: value
    real :: percentage
    dll_export :: getCOClimateAdjustmentPercent

    percentage= gv_climatefactorCO * 100.
    value = nint(percentage)
end function
subroutine setCOClimateAdjustmentPercent(value)
    use lms_River_Ba
    implicit none
    dll_export :: setCOClimateAdjustmentPercent
    integer :: value

    gv_climatefactorCO = value*0.01
 return
end subroutine
! =============================================================
!
! Drought year start
! ===============
! C# interface = protected internal int ColoradoUserAdjustmentStartYear
!
function getCOUserAdjustmentStartYear() result(value)
    use lms_River_Ba
    implicit none
    integer :: value
    dll_export :: getCOUserAdjustmentStartYear

    value = gv_droughtyearCO
end function
subroutine setCOUserAdjustmentStartYear(value)
    use lms_River_Ba
    implicit none
    dll_export :: setCOUserAdjustmentStartYear
    integer :: value
    gv_droughtyearCO=2013
    if(0 < value)gv_droughtyearCO = value
 return
end subroutine
! =============================================================
!
! Drought year end
! =================
! C# interface = protected internal int ColoradoUserAdjustmentStopYear
!
function getCOUserAdjustmentStopYear() result(value)
    use lms_River_Ba
    implicit none
    integer :: value
    dll_export :: getCOUserAdjustmentStopYear

    value = gv_droughtyearendCO
end function
subroutine setCOUserAdjustmentStopYear(value)
    use lms_River_Ba
    implicit none
    dll_export :: setCOUserAdjustmentStopYear
    integer :: value

    gv_droughtyearendCO = value
 return
end subroutine
! =============================================================
!
! Drought factor (proportional)
! =================
! C# interface = protected internal int ColoradoUserAdjustmentPercent
!
function getCOUserAdjustmentPercent() result(value)
    use lms_River_Ba
    implicit none
    integer :: value
    dll_export :: getCOUserAdjustmentPercent

    value = nint(gv_droughtfactorCO * 100.)
end function
subroutine setCOUserAdjustmentPercent(value)
    use lms_River_Ba
    implicit none
    dll_export :: setCOUserAdjustmentPercent
    integer :: value

   gv_droughtfactorCO = value * 0.01
 return
end subroutine
! =============================================================
!
! ===============
! C# interface =    protected internal int set_FlowToEnvironment_CO
!
!subroutine setWaterToEvironmentCO(value)
!    use  lms_River_Ba
!    implicit none
!    dll_export :: setWaterToEvironmentCO
!    integer :: value
!    !
!    gvi_WaterToEnvironCO_acft_a = 0
!    if(0 < value)gvi_WaterToEnvironCO_acft_a = value
!
!  return
!end subroutine
! 01.20.15 DAS
subroutine setWaterToCOdelta(value)
    use lms_Designations_B
    implicit none
    dll_export :: setWaterToCOdelta
    integer :: value
    !
    gvi_WaterToCOdelta_acft_a = 0
    if(0 < value)gvi_WaterToCOdelta_acft_a = value

  return
end subroutine
subroutine setCODeltaBurden(value)
    use lms_Designations_B
    implicit none
    dll_export :: setCODeltaBurden
    logical :: value

    gvl_AZdeltaBurden=.true.
   if(.NOT. value)gvl_AZdeltaBurden=value
 return
end subroutine
!
! Sustainability indicator for the User Interface
! 
! ============
! C# interface =
!
function getRatioCOdeltaBurden() result(value)
    use lms_Designations_B
    implicit none
    dll_export :: getRatioCOdeltaBurden
    integer :: value

    value = go_ratioCOdeltaBurden
end function
!
! ==============
! C# interface = protected internal int
function getAZshareCOdeltaWater() result(value)
  use lms_Designations_B
    implicit none
    dll_export :: getAZshareCOdeltaWater
    integer :: value
  
    value=go_COdeltaWater_az
end function
! ==============
! C# interface = protected internal int
function getCAshareCOdeltaWater() result(value)
  use lms_River_Ba
    implicit none
    dll_export :: getCAshareCOdeltaWater
    integer :: value
  
    value=go_COdeltaWater_ca
end function
! ==============
! C# interface = protected internal int
function getNVshareCOdeltaWater() result(value)
  use lms_River_Ba
    implicit none
    dll_export :: getNVshareCOdeltaWater
    integer :: value
  
    value=go_COdeltaWater_nv
end function
! ==============
! C# interface = protected internal int
function getMXshareCOdeltaWater() result(value)
  use lms_River_Ba
    implicit none
    dll_export :: getMXshareCOdeltaWater
    integer :: value
  
    value=go_COdeltaWater_mx
end function




! ==============
! =============================================================
!
! End of Colorado River input parameters
!
! ===============================================================================
!
! -------------------------------------------------------------------------------------
! ---------- Count-scale variables that are retained, but not used --------------------
!
! Include natural recharge
! ======
! C# interface = protected internal bool NaturalRechargeSwitchF
!
function getNaturalRechargeSwitch() result(value)
    use lm_Groundwater_A
    implicit none
    dll_export :: getNaturalRechargeSwitch
    logical :: value

    value = gvl_rechargeNatural
end function
subroutine setNaturalRechargeSwitch(value)
    use lm_Groundwater_A
    implicit none
    dll_export :: setNaturalRechargeSwitch
    logical :: value

    gvl_rechargeNatural = value
 return
end subroutine
! =============================================================
!
!
!  OUTPUTS 
!
!
! =============================================================
!
!
! ---------------------------------------------------------------
! ---------------- Salt/Verde Output Variables ------------------!
!
! SRP release
! ==============
! C# interface = protected internal int get_SaltVerdeTontoRelease
!
function getSVDelivery() result(value)
    use lms_WaterShed
    implicit none
    dll_export :: getSVDelivery
    integer :: value

    value = go_deliveriesSVT
end function
! =============================================================

! SVT flow (modified by factors)
! ==============
! C# interface = protected internal int SaltVerdeRiverFlow
!
function getSVRiverFlow() result(value)
    use lms_River_Aa
    implicit none
    dll_export :: getSVRiverFlow
    integer :: value

    value = go_riverFlowSVT
end function
! =============================================================
!
! Total Reservoir storage
! ====================
! C# interface = protected internal int get_SaltVerdeStorage
!
function getSVStorage() result(value)
    use lms_River_Aa
    implicit none
    dll_export :: getSVStorage
    integer :: value

    value = go_StateSVT
end function
! =============================================================
!
! Storage overflow
! ====================
! C# interface = protected internal int SaltVerdeSpillage
!
function getSVTspillage() result(value)
    use  lms_SRPnewRelease
    implicit none
    dll_export :: getSVTspillage
    integer :: value

    value = go_spillageSVT
end function
! =============================================================
!
! 
!
!           Verde
! ====================
! C# interface = protected internal int get_VerdeAnnualFlow
!
function getVerdeFlow() result(value)
    use   lms_SRPverde
    implicit none
    dll_export :: getVerdeFlow
    integer :: value

    value = go_verdeRiverFlow_acft
end function
! =============================================================
!
! ====================
! C# interface = protected internal int get_VerdeStorageAnnual
!
function getVerdeStorage() result(value)
    use   lms_SRPverde
    implicit none
    dll_export :: getVerdeStorage
    integer :: value

    value = go_VerdeStorage_acft
end function
! =============================================================
!
! ====================
! C# interface = protected internal int get_VerdeClassAwaterUsed
!
function getVerdeClassAused_acft() result(value)
    use   lms_SRPverde
    implicit none
    dll_export :: getVerdeClassAused_acft
    integer :: value

    value = go_VerdeClassAused_acft
end function
! =============================================================
!
! ====================
! C# interface = protected internal int get_VerdeClassBCwaterUsed
!
function getVerdeClassBCused_acft() result(value)
    use   lms_SRPverde
    implicit none
    dll_export :: getVerdeClassAused_acft
    integer :: value

    value = go_VerdeClassBCused_acft
end function
! =============================================================
!
!       Salt-Tonto
! ======================
! C# interface = protected internal int get_SaltTontoAnnualFlow
!
function getSaltTontoFlow() result(value)
    use   lms_SRPsaltTonto
    implicit none
    dll_export :: getSaltTontoFlow
    integer :: value

    value = go_saltTontoRiverFlow_acft
end function
! =============================================================
!
! ======================
! C# interface = protected internal int get_SaltTontoStorageAnnual 
!
function getSaltTontoStorage() result(value)
    use   lms_SRPsaltTonto
    implicit none
    dll_export :: getSaltTontoStorage
    integer :: value

    value = go_SaltTontoStorage_acft
end function
! =============================================================
!
! ======================
! C# interface = protected internal int get_OtherSaltStorage 
!
function getOtherSaltStorage() result(value)
    use   lms_SRPsaltTonto
    implicit none
    dll_export :: getOtherSaltStorage
    integer :: value

    value = go_saltOtherStorage_acft_a
end function
! =============================================================
!
! ====================
! ======================
! C# interface = protected internal int get_OtherSaltStorage 
!
function getRooseveltStorage() result(value)
    use   lms_SRPsaltTonto
    implicit none
    dll_export :: getRooseveltStorage
    integer :: value

    value = go_rooseveltStorage_acft_a
end function
! =============================================================
!




! C# interface = protected internal int get_SaltTontoClassAwaterUsed 
!
function getSaltTontoClassAused_acft() result(value)
    use lms_SRPsaltTonto  
    implicit none
    dll_export :: getSaltTontoClassAused_acft
    integer :: value

    value = go_SaltTontoClassAused_acft
end function
! =============================================================
!  
! ====================
! C# interface = protected internal int get_SaltTontoClassBCwaterUsed
!
function getSaltTontoClassBCused_acft() result(value)
    use lms_SRPsaltTonto  
    implicit none
    dll_export :: getSaltTontoClassBCused_acft
    integer :: value

    value = go_SaltTontoClassBCused_acft
end function
! =============================================================
!
! -------------------------------------------------
! ------------- Colorado Variables ----------------!
!
! ==============
! C# interface = protected internal int get_AZshareCO
!
function getAZshareCO() result(value)
    use lms_River_Ba
    implicit none
    dll_export :: getAZshareCO
    integer :: value
!
    value = go_AZshareCO
end function
! =============================================================
!
! ==============
! C# interface = protected internal int get_CAP
!
function getCAP() result(value)
    use lms_River_Ba
    implicit none
    dll_export :: getCAP
    integer :: value
!
    value = go_CAP
end function
! =============================================================
!
! Add? Not currently in the interface
! ==============
! C# interface =
!
function getPowellElevation() result(value)
    use lms_River_Ba
    implicit none
    dll_export :: getPowellElevation
    integer :: value
!
    value = go_powellElevation
end function
! =============================================================
!
!
! ==============
! C# interface = protected internal int get_MeadElevation
!
function getMeadElevation() result(value)
    use lms_River_Ba
    implicit none
    dll_export :: getMeadElevation
    integer :: value
!
    value = go_meadElevation
end function
! =============================================================

! =============================================================
! 
!  NOT currently in the interface
! ==============
! C# interface = internal int get_UpperBasinDeliveries
!
function getUpperBasinDeliveries() result(value)
    use lms_River_Ba
    implicit none
    dll_export :: getUpperBasinDeliveries
    integer :: value

    value = go_upperBasinDeliveries
end function
! =============================================================
!
! ===============
! C# interface =  internal int set_upperBasinData
!
subroutine setUpperBasinDeliveriesIndex(value)
    use lms_River_Ba
    implicit none
    dll_export :: setUpperBasinDeliveriesIndex
    integer :: value

    gvf_upperBasinEstimate=2
    if(0 < value)gvf_upperBasinEstimate = value
 return
end subroutine
! =============================================================
!
!
! ==============
! C# interface = protected internal int get_ColoradoDeliveryMandI
!
function getCODelivery() result(value)
    use lms_WaterShed
    implicit none
    dll_export :: getCODelivery
    integer :: value

    value = go_deliveriesCO
end function
! =============================================================
!
! CO flow
! ==============
! C# interface = protected internal int ColoradoRiverFlow
!
function getCORiverFlow() result(value)
    use lms_River_Ba
    implicit none
    dll_export :: getCORiverFlow
    integer :: value

    value = go_riverFlowCO

end function
! =============================================================
!
! CO storage
! ==============
! C# interface = protected internal int get_ColoradoStorage
!
function getCOStorage() result(value)
    use lms_River_Ba
    implicit none
    dll_export :: getCOStorage
    integer :: value

    value = go_StateCO
end function
! =============================================================
!
! Powell reservoir
! ==============
! C# interface = protected internal int get_PowellStorage
!
function getPowellStorage() result(value)
    use lms_River_Ba

    implicit none
    dll_export :: getPowellStorage
    integer :: value

    value = go_StatePowell
end function
! =============================================================
!
! Mead reservoir
! ==============
! C# interface = protected internal int get_MeadStorage
!
function getMeadStorage() result(value)
    use lms_River_Ba
    implicit none
    dll_export :: getMeadStorage
    integer :: value

    value = go_StateMead
end function
!
!
! ------------------------------------------------------------------
! --------------------- Other Variables ----------------------------!
!
!                       Agriculture
!
! 01.27.15 DAS
! ===============
! C# interface = protected internal int set_AgPumpingCurveIndex
!
subroutine setAgPumpingCurve(value)
    use lms_Groundwater_A
    implicit none
    dll_export :: setAgPumpingCurve
    integer :: value
    !
     gvi_AgPumpCurveIndex = 9
     if(0 <= value)gvi_AgPumpCurveIndex = value
    !
  return
end subroutine
!
! 01.27.15 DAS
! ===============
! C# interface = protected internal int set_AgPumpingCurveIndex
!
! This is the fallow percentage (1 minus setAgCreditTransferPCT )
subroutine setAgCreditTransferPCT(value)
    use lms_Groundwater_A
    implicit none
    dll_export :: setAgCreditTransferPCT
    integer :: value
    !
     gvi_AgCreditTransferPCT = 100
     if(0 < value)gvi_AgCreditTransferPCT = value
    !
  return
end subroutine
!
! ===============
! For the DECISION GAME CODE ONLY!!!!!!!
! i.e., scaled from 0 to 100% (transfer to Ag) gives an index
! value of 10 to 0
! 
! C# interface =
!   protected internal int[] set_ProviderAgCAPandPumpingCurveIndex
!
subroutine setProviderAgCreditCurvePCT(count, values)
   use lm_ParameterControl
    implicit none
    dll_export ::  setProviderAgCreditCurvePCT
    integer :: count,i
    integer, dimension(1:count) :: values
    integer, dimension(1:count) :: generic
    ! 
    forall(i=1:count)generic(i)=gvi_AgPumpCurveIndex
    forall(i=1:count, values(i) .gt. -1)
      generic(i) =values(i)
    end forall
    forall(i=1:count) gvi_ProviderAgCreditCurveIndex(i) = generic(i)
 return
end subroutine

! Revisit- 01.28.15,04.06.15,06.02.15 DAS
! ===============
! C# interface = protected internal int [] WaterFromAgPumping
!
! Returns the actual amount received from the request
! Total Credits
subroutine getWaterFromAgPumping(count, values)
   use lms_AgWaterBudgets
    implicit none
    dll_export ::  getWaterFromAgPumping
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
     forall(i=1:count) values(i)=go_AgWaterGW_AF(i)
 return
end subroutine
!
! Request for water from Ag pumping
! Used in Groundwater.f90
! ===============
! C# interface = protected internal int [] WaterFromAgPumping
! 
subroutine setWaterFromAgPumping(count, values)
   use lm_ParameterControl 
    implicit none
    dll_export ::  setWaterFromAgPumping
    integer :: count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=0.0
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i)
    end forall
    
    forall(i=1:count) gvi_WaterFromAgPumping_acft_a(i)=nint(generic(i))
 return
end subroutine
! ============
!
!
! This was a Johnston et al. special projects variable
! ===============
! C# interface = protected internal int[] WaterToAgriculture_AF
!
subroutine setWaterToAgricultureAF(count,values)
    use  lm_ParameterControl
    implicit none
    dll_export :: setWaterToAgricultureAF
    integer :: i,count
    integer, dimension(1:count) :: values    
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=0.
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) 
    end forall
    !
    forall(i=1:count) gvi_WaterToAgriculture_acft_a(i)= generic(i)
  return
end subroutine
! ===========================================================
! This was a Johnston et al. special projects variable
!
! NOTE: 01.21.15 DAS
subroutine getWaterToAgricultureAF(count, values)
   use lm_ParameterControl
    implicit none
    dll_export ::  getWaterToAgricultureAF
    integer :: count,i
    integer, dimension(1:count) :: values
 ! 
        forall(i=1:count) values(i)=  gvi_WaterToAgriculture_acft_a(i)
 return
end subroutine
! ------------------------------------------------------------------------
!
!
subroutine getWaterFromAgPumpingMax(count, values)
   use lms_AgWaterBudgets
    implicit none
    dll_export ::  getWaterFromAgPumpingMax
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= gvi_defaultAgPumping_acft_a(i)
 return
end subroutine
!
! This has been revised 11/20/16, 04.15.19 once again
subroutine getWaterFromAgPumpingTotal(count, values)
   use lms_AgWaterBudgets
    implicit none
    dll_export ::  getWaterFromAgPumpingTotal
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_pumpingAgTotal_AF(i)
 return
end subroutine
! =============================================================
!
! Ag water surface
! ===============
! C# interface = protected internal int[] WaterFromAgSurface
!
subroutine getWaterFromAgSurface(count, values)
   use  lms_River_Ba
    implicit none
    dll_export ::  getWaterFromAgSurface
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_WaterFromAgSurface_acft_a(i)
end subroutine
! 
! Set not used now (as of 3 June, 2015)
subroutine setWaterFromAgSurface(count, values)
   use  lms_River_Ba
    implicit none
    dll_export ::  setWaterFromAgSurface
    integer :: count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=0.0
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i)
    end forall

    forall(i=1:count) gvi_WaterFromAgSurface_acft_a(i)=generic(i)
 return
end subroutine
! ============
! 
! Used as surface water other
! Global Subroutines
subroutine getWaterFromAgSurfaceMax(count, values)
   use  lms_River_Ba
    implicit none
    dll_export ::  getWaterFromAgSurfaceMax
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= gvi_DefaultAgSurface_acft_a(i)
end subroutine
!
! This is new to provide an estimate of gross water used by agriculture
! 01.21.15 DAS in the API this is pumped water 
subroutine getGrossAgricultureWaterAF(count, values)
   use lm_ParameterControl
    implicit none
    dll_export ::  getGrossAgricultureWaterAF
    integer :: count,i
    integer, dimension(1:count) :: values
 ! 
    forall(i=1:count) values(i)= go_WaterFromAgPumping_acft_a(i)
 return
end subroutine
! ===============
! C# interface = protected internal int set_WaterToAgriculture_acft
!
subroutine getNetAgricultureWaterAF(count, values)
   use lm_ParameterControl
    implicit none
    dll_export ::  getNetAgricultureWaterAF
    integer :: count,i
    integer, dimension(1:count) :: values
 ! 
        forall(i=1:count) values(i)=  go_NetAgWater_acft(i)
 return
end subroutine
!
! ==============
! C# interface =   protected internal int get_AgProductionPCT
!
function getAgProductionPCT() result(value)
  use lms_AgWaterBudgets
    implicit none
    dll_export :: getAgProductionPCT
    integer :: value
  
    value=go_AgProductionPCT
end function
!
! ==============
! C# interface =  protected internal int[] get_AgWaterUsedRelativeTo2014_PCT
!
subroutine getAgCreditTransPCT(count, values)
   use lms_AgWaterBudgets
    implicit none
    dll_export ::  getAgCreditTransPCT
    integer :: count,i
    integer, dimension(1:count) :: values
 ! 
        forall(i=1:count) values(i)=  go_AgWaterUsedByMuni_PCT_a(i)
 return
end subroutine
!
! DAS 04.06.15
!
subroutine getProviderMaxAgPumping(count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: getProviderMaxAgPumping
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)=go_MaxAgPumping(i)
    ! 
 return
end subroutine
subroutine getProviderMaxAgSurface(count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: getProviderMaxAgSurface
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)=go_MaxAgSurface(i)
    ! 
 return
end subroutine
!
! =============================================================
!
!
! --------------------------------------------------------
! ---------------- Groundwater Variables -----------------!
!
! ==============
! C# interface = protected internal bool[] get_EightyPercentRule
!
subroutine geteightyPctRule(count, values)
   use lms_Groundwater_A
    implicit none
    dll_export ::  geteightyPctRule
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=   gvi_eightyPercentRule(i)
 return
end subroutine
! ==========================================================
!

! ---------------------------------------------------------------------
!  Provider-scale
! ================
! C# interface= protected internal int[] ProviderPopGrowthRateAdjustPct
!
subroutine getPopGrowthRateAdjustPct(count, values)
    use lms_Provider
    implicit none
    dll_export :: getPopGrowthRateAdjustPct
    integer :: count
    integer :: i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = nint(gv_PopGrowthRateAdjPct(i)*100.)
 return
end subroutine
subroutine setPopGrowthRateAdjustPct(count, values)
    use lms_Provider
    implicit none
    dll_export :: setPopGrowthRateAdjustPct
    integer :: count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=0.
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
   
   forall(i=1:count) gv_PopGrowthRateAdjPct(i)=generic(i) 
 return
end subroutine
! =============================================================
!
! ==============
! C# interface= protected internal int[] ProviderPopGrowthRateOnProjectPct
!
subroutine getPopGrowthRateOnProjectPct(count, values)
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: getPopGrowthRateOnProjectPct
    integer :: count
    integer :: i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = nint(gvd_PopGrowthRateOn(i)*100.)
 return
end subroutine
subroutine setPopGrowthRateOnProjectPct(count, values)
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: setPopGrowthRateOnProjectPct
    integer :: count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=1.
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
   forall(i=1:count) gvd_PopGrowthRateOn(i)=generic(i) 
 return
end subroutine
! =============================================================
!
! ==============
! C# interface=  protected internal int[] ProviderPopGrowthRateOtherPct
!
subroutine getPopGrowthRateOtherPct(count, values)
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: getPopGrowthRateOtherPct
    integer :: count
    integer :: i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = nint(gvd_PopGrowthRateOther(i)*100.)
 return
end subroutine
subroutine setPopGrowthRateOtherPct(count, values)
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: setPopGrowthRateOtherPct
    integer :: count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=1.
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
   forall(i=1:count) gvd_PopGrowthRateOther(i)=generic(i) 
 return
end subroutine
! =============================================================
!
! ==============
! C# interface=   protected internal int[] get_PopulationOnProject

subroutine PopulationOnProject(count, values)
    use  lms_ProviderPopandDemand
    implicit none
    dll_export :: PopulationOnProject
    integer :: count
    integer :: i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = go_PopulationOnProject(i)
 return
end subroutine
! =============================================================
!
! ==============
! C# interface= protected internal int[] get_PopulationOther
!
subroutine PopulationOther(count, values)
    use  lms_ProviderPopandDemand
    implicit none
    dll_export :: PopulationOther
    integer :: count
    integer :: i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = go_PopulationOther(i)
 return
end subroutine
! =============================================================
!
!
! Start here 06.03.15 das
!
! Total water demand (Ag and Muni)
! ==============
! C# interface=  protected internal int[] get_WaterDemand  
!
subroutine getTotalDemands(count, values)
    use lms_Provider
    implicit none
    dll_export :: getTotalDemands
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = go_ProviderDemand(i)
 return
end subroutine
! =============================================================
!
! Provider populations
! ==============
! C# interface=  protected internal int[] get_Populations
!
subroutine getPopulations(count, values)
    use lms_Provider
    implicit none
    dll_export :: getPopulations
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = go_providerpop(i)
 return
end subroutine
! =============================================================
!
! NOT currently in the interface
! ==============
! C# interface =
!
subroutine getPopGrowthRate(count, values)
    use lms_Provider
    implicit none
    dll_export :: getPopGrowthRate
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = go_PopGrowthRate(i)
 return
end subroutine
! =============================================================
! 
! ==============
! C# interface = protected internal int[] set_PopulationsOn
!
subroutine setPopulationsOn(count, values)
    use lms_Provider
    implicit none
    dll_export :: setPopulationsOn
    integer :: count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic

    forall(i=1:count)generic(i)=0.
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) 
    end forall

   forall(i=1:count) gi_ProviderPopulationOn(i) = generic(i) 
 return
end subroutine
! =============================================================
! 
! ==============
! C# interface = protected internal int[] set_PopulationsOther
!
subroutine setPopulationsOther(count, values)
    use lms_Provider
    implicit none
    dll_export :: setPopulationsOther
    integer :: count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic

    forall(i=1:count)generic(i)=0.
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) 
    end forall

   forall(i=1:count) gi_ProviderPopulationOther(i) = generic(i) 
 return
end subroutine
! =============================================================
!
! NOT currently in the interface
! ==============
! C# interface =  
!
subroutine getProviderDeliveriesUsed(count, values)
    use lms_Provider
    implicit none
    dll_export :: getProviderDeliveriesUsed
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = go_ProviderWaterUsed_acft(i)
 return
end subroutine
! =============================================================
!   
!  Colorado River Water Outputs
! -----------------------------
!
!
! Delivered CAP water
! ==============
! C# interface=   protected internal int[] get_ColoradoAnnualDeliveries 

subroutine getCOAnnualDeliveries(count, values)
    use lms_Provider
    implicit none
    dll_export :: getCOAnnualDeliveries
    integer :: count,i
    integer, dimension(1:count) :: values
    ! array = 35
    forall(i=1:count) values(i) = go_annualDeliveriesCO(i)
 return
end subroutine
! =============================================================
!
! In Interface, but not referenced so not in API
! ==============
! C# interface=     protected internal int get_CAP2MandI
!
function getCAPMandI() result(value)
    use lms_Provider
    implicit none
    dll_export :: getCAPMandI
    integer :: value

    value = go_mandiCAP
end function
! =============================================================
!
! Maximum CO - designations
! 
! =================
!
! C# interface=  protected internal int[] get_ColoradoMaxDeliveries  
!
subroutine getCOMaxDeliveries(count, values)
   use lms_Provider
    implicit none
    dll_export :: getCOMaxDeliveries
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i) = go_maxDeliveriesCO(i)
 return
end subroutine
! =============================================================
!
! =================
!
! C# interface=  protected internal int[]get_ColoradoUnusedDeliveries  
!
!subroutine getCOunusedPriority4(count, values)
!   use lms_Provider
!    implicit none
!    dll_export :: getCOunusedPriority4
!    integer :: count,i
!    integer, dimension(1:count) :: values
!
!    forall(i=1:count) values(i) = go_CAPunusedPriority4(i)
! return
!end subroutine
!! C# interface=  protected internal int[]get_ColoradoUnusedDeliveries  
!!
!subroutine getCOunusedPriority5(count, values)
!   use lms_Provider
!    implicit none
!    dll_export :: getCOunusedPriority5
!    integer :: count,i
!    integer, dimension(1:count) :: values
!
!    forall(i=1:count) values(i) = go_CAPunusedPriority5(i)
! return
!end subroutine
! =============================================================
! New variable, not yet implemented
! Will be used to contain "extra" CAP allocations
! 
!subroutine getCOAddDeliveries(count, values)
!    use  lms_Designations_B
!    implicit none
!    dll_export :: getCOAddDeliveries
!    integer :: count,i
!    integer, dimension(1:count) :: values
!    ! array = 35
!    !forall(i=1:count) values(i) = go_addDeliveriesCO(i)
!
!end subroutine
!subroutine setCOAddDeliveries(count, values)
!    use  lms_Designations_B
!    implicit none
!    dll_export :: setCOAddDeliveries
!    integer :: count,i
!    integer, dimension(1:count) :: values
!    ! array = 35
!    !forall(i=1:count) = go_addDeliveriesCO(i)=values(i) 
!
!end subroutine
! =============================================================
!
!  --------------- Salt-Tonto-Verde ------------------
! ==============
!  Class A,B,C, and NCS for the 10
! ==============
!
! C# interface = protected internal int[] get_SVTAnnualDeliveriesSRP
!
subroutine getSRPAnnualDeliveries(count, values)
    use lms_Provider
    implicit none
    dll_export :: getSRPAnnualDeliveries
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i) =go_annualDeliveriesSRP(i)
 return
end subroutine
! =============================================================
!
! Maxmum class A,B,C, and NCS scaled to 10
!
! C# interface= protected internal int[] SaltVerdeMaxDeliveries   
! In interface, but not referenced, so not in API
!
subroutine getSVMaxDeliveriesSRP(count, values)
    use lms_Provider
    implicit none
    dll_export :: getSVMaxDeliveriesSRP
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)=go_maxDeliveriesSRP(i)
 return
end subroutine
! =============================================================
!
! Designations class A, scaled to 10
! These are USED class A
! =================
! C# interface = protected internal int[] get_SaltVerdeClassADesignationsUsed  
! In interface, but not referenced, so not in API
!
subroutine getSVClassADesignations(count, values)
    use lms_Provider
    implicit none
    dll_export :: getSVClassADesignations
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)=go_classAdesignations(i)
return
end subroutine
! =============================================================
!
! Both reservoir and pumping "Used"
! ===============
! C# interface=   protected internal int[] get_SaltVerdeClassBCDesignationsUsed   
!
subroutine getSVClassBCDesignations(count, values)
    use lms_Provider
    implicit none
    dll_export :: getSVClassBCDesignations
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)= go_classBCdesignations(i)
 return
end subroutine
! =============================================================
! 
! Maximum B&C by SRP
! ================
! C# interface = protected internal int[] get_SaltVerdeClassBCmax 
! In interface, but not referenced so not in API
subroutine getSVClassBCmax(count, values)
    use lms_Provider
    implicit none
    dll_export :: getSVClassBCmax
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)= go_classBCmax(i)
 return
end subroutine
! =============================================================
! 

! New Conservation Space, scaled to 33
! ==============
! C# interface = protected internal int[] get_SaltVerdeNCSDesignationsUsed 
! In interface, but not referenced so not in API
!subroutine getSVNCS(count, values)
!    use lms_Provider
!    implicit none
!    dll_export :: getSVNCS
!    integer :: count,i
!    integer, dimension(1:count) :: values
!    ! brought from 7 to 10 in model
!    forall(i=1:count) values(i)=go_NCS_acft(i)
!return
!end subroutine
! =============================================================
!
! ==============
! C# interface = protected internal int[] get_SaltVerdeNCSmax  
!
!subroutine getSVNCSmax(count, values)
!    use  lms_Provider
!    implicit none
!    dll_export :: getSVNCSmax
!    integer :: count,i
!    integer, dimension(1:count) :: values
!    ! brought from 7 to 10 in model
!    forall(i=1:count) values(i)=go_NCSmax_acft(i)
! return
!end subroutine
! =============================================================
! 
! New B and C water removed from storage (reservoir)
! In the interface, NOT in the API as of 10.07.13
! ==============
! C# interface = protected internal int[] get_SaltVerdeClassBCreservoirUsed 
!
subroutine getSVTclassBCstorage(count, values)
    use lms_Provider
    implicit none
    dll_export :: getSVTclassBCstorage
    integer :: count,i
    integer, dimension(1:count) :: values
    ! brought from 7 to 10 in model
    forall(i=1:count) values(i)=go_classBCstorage_acft(i)
 return
end subroutine
! =============================================================
!
! NOT in the interface
! ==============
! C# interface=  
!
subroutine getSVTclassBCstorageMax(count, values)
    use  lms_Provider
    implicit none
    dll_export :: getSVTclassBCstorageMax
    integer :: count,i
    integer, dimension(1:count) :: values
    ! brought from 7 to 10 in model
    forall(i=1:count) values(i)=go_classBCstorageMax_acft(i)
 return
end subroutine
! =============================================================
! 
!
! Designations class A, scaled to 10
! These are USED class A from the Salt-Tonto Basin
!
! Not referenced in the interface, so not in the API
! =================
! C# interface = protected internal int get_SaltTontoClassAwaterUsed
!
function getSTClassAwaterUsed() result(value)
    use lms_SRPsaltTonto
    implicit none
    dll_export :: getSTClassAwaterUsed
    integer :: value

    value = go_SaltTontoClassAused_acft
end function
! =============================================================
! Designations class A, scaled to 10
! These are USED class B&C from the Salt-Tonto Basin
! Not referenced in the interface, so not in the API
! =================
! C# interface = protected internal int  get_SaltTontoClassBCwaterUsed
!
function  getSaltTontoClassBCwaterUsed() result(value)
    use lms_SRPsaltTonto
    implicit none
    dll_export ::  getSaltTontoClassBCwaterUsed
    integer :: value

    value =  go_SaltTontoClassBCused_acft
end function
! =============================================================
!
! =================
! C# interface = protected internal int get_VerdeClassAwaterUsed
! Not referenced in the interface, so not in the API
!
function getVerdeClassAwaterUsed() result(value)
    use lms_SRPverde
    implicit none
    dll_export :: getVerdeClassAwaterUsed
    integer :: value

    value = go_VerdeClassAused_acft
end function
! =============================================================
! Designations class A, scaled to 10
! These are USED class B&C from the Salt-Tonto Basin
! Not referenced in the interface, so not in the API
! =================
! C# interface =protected internal int get_VerdeClassBCwaterUsed
!
function  getVerdeClassBCwaterUsed() result(value)
    use lms_SRPverde
    implicit none
    dll_export ::  getVerdeClassBCwaterUsed
    integer :: value

    value =   go_VerdeClassBCused_acft
end function
! =============================================================








!
!      ---------------------------------
!             General Variables

! Available to pump legally
! ===============
! C# interface =    protected internal int[] get_ProviderGWdesignations 
! IN interface, but not referenced so not in API
subroutine getProviderGWDesignations(count, values)
    use lms_Provider
    implicit none
    dll_export :: getProviderGWDesignations
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)= go_annualCredits_acft(i)
 return
end subroutine
! =============================================================
!
! 08.16.13
! ==============
! C# interface = protected internal int[] set_ProviderAllowanceGWcredits
! IN interface, but not referenced so not in API
subroutine setProviderAnnualCredits(count, values)
    use lms_Groundwater_A
    implicit none
    dll_export :: setProviderAnnualCredits
    integer :: count,i
    integer, dimension(1:count) :: values
    integer, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=-1
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) 
    end forall

    forall(i=1:count) gvf_setAnnualCredits_acft(i)=generic(i)
 return
end subroutine
! =============================================================
!
! ==============
! C# interface = protected internal int[] get_ProviderGWstored 
! IN interface, but not referenced so not in API
subroutine getProviderGWstored(count, values)
    use lms_Provider
    implicit none
    dll_export :: getProviderGWstored
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)= gof_getAnnualGWstored_acft(i)
 return
end subroutine
! =============================================================
!
! ===============

! 02.10.12
!  Legal surface water designations based on storage 
! and flow (includes reservoir water)
! ===============
! C# interface = protected internal int[] get_ProviderSWdesignations 
! IN interface, but not referenced so not in API
subroutine getProviderSWDesignations(count, values)
    use lms_Provider
    implicit none
    dll_export :: getProviderSWDesignations
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)= go_SWdesignations_acft(i)
 return
end subroutine
! ===========================================================
!
!             ----------------------------
!                  Alternate variables  
!
!   Water use after subtracting grey water recycling from demand
!
!  NOT currently in the interface
! ===============
! C# interface =
! Not in the Interface
!subroutine getProviderPostReclaimedUse(count, values)
!    use lms_CitiWaterBudgets
!    implicit none
!    dll_export :: getProviderPostReclaimedUse
!    integer :: count,i
!    integer, dimension(1:count) :: values
!
!    forall(i=1:count) values(i)= go_ProviderPostReclaimed(i)
! return
!end subroutine
! ===========================================================
!
!            --------------------------
!                 Provider-based
!
!  Groundwater
! -------------------------
!  IN: subroutine sAquifer()
!
! CityModel groundwater estimates
! ==============
! C# interface = protected internal int[] ModelGroundwater
!
!~ this is the groundwater credit balance
! In the interface, but not referenced, so NOT in the API
subroutine getAnnualProviderGroundW(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getAnnualProviderGroundW
    integer :: count,i
    integer, dimension(1:count) :: values
    !
    forall(i=1:count) values(i)= go_ProviderGroundWater_a(i)
 return   ! line 1509 in Water_CitiModel.f90
end subroutine
!
subroutine setProviderGroundWater(count, values)
    use lms_WaterShed
    implicit none
    dll_export :: setProviderGroundWater
    integer :: count,i
    integer, dimension(1:count) :: values
    integer, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=1
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) 
    end forall
    !
    forall(i=1:count) gvi_ProviderGroundWater_a(i)=generic(i)
 return
end subroutine
! ===========================================================

! IN: subroutine aDemandSupply()
!
! Municipal pumping
! ==============
! C# interface = protected internal int[] get_ProviderGWPumpedMunicipal 
!
subroutine getProviderGWPumpageMuni(count, values)
    use lms_Provider
    implicit none
    dll_export :: getProviderGWPumpageMuni
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = go_GWPumpageMuni_acft(i)
 return
end subroutine
! ===========================================================
!
! ==============
! C# interface = protected internal int[] get_ClassBCpumpedSRP
!                
subroutine getProviderGWPumpageSRP(count, values)
    use lms_Provider
    implicit none
    dll_export :: getProviderGWPumpageSRP
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = go_GWPumpageSRP_acft(i)
 return
end subroutine
! ===========================================================
!
! ==============
! C# interface = protected internal int[] get_ProviderGWRecharge  
!                
subroutine getProviderGWRecharge(count, values)
    use lms_Provider
    implicit none
    dll_export :: getProviderGWRecharge
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = go_GWRecharge_acft(i)
 return
end subroutine
! ===========================================================
!
!             --------------------------------
!                   CityModel Outputs
!
! =========================================!
! --------------------------------------------!
! Special Projects
! ================

! Provider.f90 - using SES reduces GPCD by x% by the year 2085
! ==============
! C# interface = protected internal int set_AlterGPCDpct
!
subroutine setAlterGPCDPct(value)
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: setAlterGPCDPct
    integer :: value
    !
     gvi_AlterGPCD_pct = 0
    gvi_AlterGPCD_pct = value
 return
end subroutine
! ===========================================================
!
! ==============
! C# interface = protected internal int[] get_ClassBCpumpedSRP
!                
subroutine getProviderAlterGPCDPct(count, values)
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: getProviderAlterGPCDPct
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = gvi_ProviderAlterGPCD_pct(i)
 return
end subroutine
! ===========================================================
!   These are the default values for each provider for reduction
!   in GPCD .. 01.07.15 DAS
! ========================
! 
subroutine setProviderAlterGPCDPct(count, values)
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: setProviderAlterGPCDPct
    integer :: count,i,j
    integer, dimension(1:count) :: values
    integer, dimension(1:count) :: generic
    !
    forall(i=1:count)
      generic(i) = values(i) 
    end forall
    !
    do j = 1,count
     if(generic(j) < 0)then
     else
        ! removed the -generic(j). 05.19.15
       generic(j)=generic(j)
     endif
    end do
    forall(i=1:count) gvi_ProviderAlterGPCD_pct(i)=generic(i)
 return
end subroutine
! ==============
! ===========================================================
!   This is for minimum GPCD provider
!   01.07.15 DAS
! ========================

subroutine setProviderMinGPCD(count, values)
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: setProviderMinGPCD
    integer :: count,i
    integer, dimension(1:count) :: values
    integer, dimension(1:count) :: generic
    !
    forall(i=1:count)
      generic(i) = values(i) 
    end forall
    !
    forall(i=1:count) gvi_ProviderMinGPCD(i)=generic(i)
 return
end subroutine
! ==============
subroutine getProviderMinGPCD(count, values)
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: getProviderMinGPCD
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) =  gvi_ProviderMinGPCD(i)
 return
end subroutine
! ===========================================================

!                --------------------------------
!                       Energy variables
!
! NOT in the API
! ==============
! C# interface = protected internal int get_CAPEnergyUsedInPumping
!

! ===========================================================

! =============================================!
! ------------------------------------------------!
!
!               ------------------------
!                   Provider Inputs
!
! Water demand options 1=read from file
! 2=six year average GPCD
! 3=uses simple exponential smoothing of GPCD and population
!   Three types: 1) increases, 2) constand, 3) decreases
!   see above-  setGPCDmethod(value)
! 4=SES and takes input from the interface for use
!
! ==============
! C# interface = protected internal int ProviderDemandOption
!
subroutine setDemandOption(value)
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: setDemandOption
    integer :: value

   gvi_ProviderDemandOption=value
 return
end subroutine
function getDemandOption() result(value)
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: getDemandOption
    integer :: value

   value = gvi_ProviderDemandOption

end function
! ===========================================================
!
!  see Model_Interface.cs for details
! ==============
! C# interface = protected internal int[] WaterBankingOption
!
subroutine getWaterBankingOption(count, values)
    use lms_Provider
    implicit none
    dll_export :: getWaterBankingOption
    integer :: count,i
    integer, dimension(1:count) :: values
    forall(i=1:count) values(i)=   gvi_WBankingOption(i)
 return
end subroutine
subroutine setWaterBankingOption(count,values)
    use lm_ParameterControl
    implicit none
    dll_export :: setWaterBankingOption
    integer :: count,i
    integer, dimension(1:count) :: values
    integer, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=1
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) 
    end forall
    !
    forall(i=1:count) gvi_WBankingOption(i)=generic(i)
 return
end subroutine
! ===========================================================
!
! Reduce water demand
! ==============
! C# interface = protected internal int DemandPercent
!
subroutine setDemandPercent(value)
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: setDemandPercent
    integer :: value

    gvi_ReductionDem_pctBaseline=1
   gvi_ReductionDem_pctBaseline=value
 return
end subroutine
function getDemandPercent() result(value)
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: getDemandPercent
    integer :: value

   value = gvi_ReductionDem_pctBaseline

end function
! ===========================================================
!

! From line 452 ProviderPopulationandDemand.f90
! Item 70
! Special Projects
! -----------------------------------------------
!
! As a struct labeled "outputs"
! NOT in API
! ==============
! C# interface = internal float get_TemperatureMultiplier
!
subroutine getDemandMultiplier(value)
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: getDemandMultiplier
    integer :: value
    ! note: X 1000 to retain thousandths place
   value = nint(gvf_ResponseToTemperature*1000)
 return
end subroutine
! ===========================================================
!
!
! 
! ==============
! C# interface = protected internal int[] get_WaterDemandOnProject 
!
subroutine getOnProjectDemand(count, values)
    use lms_Provider
    implicit none
    dll_export :: getOnProjectDemand
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = go_OnProjectDemand(i)
 return
end subroutine
! ===========================================================
!
! ==============
! C# interface = protected internal int[] get_WaterDemandOther
! 
!
subroutine getOffProjectDemand(count, values)
    use lms_Provider
    implicit none
    dll_export :: getOffProjectDemand
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = go_OffProjectDemand(i)
 return
end subroutine
! ===========================================================
!
! Do not know why I created this.
! ==============
! C# interface = protected internal int[] get_NetWaterDemandOnProject
!
!subroutine getOnProjectNetDemand(count, values)
!    use lms_Provider
!    implicit none
!    dll_export :: getOnProjectNetDemand
!    integer :: count,i
!    integer, dimension(1:count) :: values
!
!   forall(i=1:count) values(i) = go_OnProjectNetDemand_acft(i)
! return
!end subroutine
! ===========================================================
!
! ==============
! C# Interface = protected internal int[] get_NetWaterDemandOther 
!
! ---------------------------------------------
!subroutine getOffProjectNetDemand(count, values)
!    use lms_Provider
!    implicit none
!    dll_export :: getOffProjectNetDemand
!    integer :: count,i
!    integer, dimension(1:count) :: values
!
!   forall(i=1:count) values(i) = go_OffProjectNetDemand_acft(i)
! return
!end subroutine
! ===========================================================
!
! NOT implemented-will adjust model estimates
! based on MODFLOW estimates
! ----------------------------------------------
!subroutine setProviderGroundwater(count, values)
!    use lms_CitiWaterBudgets
!    implicit none
!    dll_export :: setProviderGroundwater
!    integer :: count,i
!    integer, dimension(1:count) :: values
!
!    forall(i=1:count) gv_providerGWFromInterface(i)=values(i)*1. 
!end subroutine
! ===========================================================

!   Zero to 100 pct: how much grey water recycled each time step
! 
!subroutine setResidentialGWusePct(count, values)
!    use lms_CitiWaterBudgets
!    implicit none
!    dll_export :: setResidentialGWusePct
!    integer :: count,i,j
!    integer, dimension(1:count) :: values
!    forall(i=1:count)gvf_RateGreyWaterRES(i)=0
!    forall(j=1:count) gvf_RateGreyWaterRES(j)=values(j)*0.01 
!end subroutine
!!
!!   Zero to 100 pct: how much grey water recycled each time step
!subroutine setCommercialIOGWusePct(count, values)
!    use lms_CitiWaterBudgets
!    implicit none
!    dll_export :: setCommercialIOGWusePct
!    integer :: count,i,j
!    integer, dimension(1:count) :: values
!    forall(i=1:count) gvf_RateGreyWaterCIO(i)=0
!    forall(j=1:count) gvf_RateGreyWaterCIO(j)=values(j)* (1/100.)
!
!end subroutine
! ===========================================================
!
!
!             -------------------------------------------
!                Set/get CityModel parameters (inputs)
!
! 
! Generic Policy 
! ------------------

! Time lag for water to move through the vadose
! ===============
! C# interface = protected internal int[] TimeLagVadoseYears
!
subroutine getTimeLagVadoseYears(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getTimeLagVadoseYears
    integer :: count,i
    integer, dimension(1:count) :: values
    forall(i=1:count) values(i)=  gvi_timeLagVadose(i)
 return
end subroutine
subroutine setTimeLagVadoseYears(mydefault, count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: setTimeLagVadoseYears
    integer :: mydefault,count,i
    integer, dimension(1:count) :: values
    integer, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=mydefault
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) 
    end forall
    !
    forall(i=1:count)  gvi_timeLagVadose(i)=generic(i)
 return
end subroutine
! ===========================================================
!
! ==============
! C# interface = protected internal int[] parmReclaimedToVadosePct
!
subroutine getParmReclaimedToVadosePct(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmReclaimedToVadosePct
    integer :: count,i
    integer, dimension(1:count) :: values
    forall(i=1:count) values(i)=  nint(gvf_parm_RtoVadose(i)*100.)
 return
end subroutine
subroutine setParmReclaimedToVadosePct(count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmReclaimedToVadosePct
    integer :: count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=0.0
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count) gvf_parm_RtoVadose(i)= generic(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] parmReclaimedToDirectInjectPct
!
subroutine getParmReclaimedToDirectInjPct(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmReclaimedToDirectInjPct
    integer :: count,i
    integer, dimension(1:count) :: values
    forall(i=1:count) values(i)=   nint(gvf_parm_RtoDInjection(i)*100.)
 return
end subroutine
subroutine setParmReclaimedToDirectInjPct(count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmReclaimedToDirectInjPct
    integer :: count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=0.
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count) gvf_parm_RtoDInjection(i)= generic(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] parmReclaimedToOutputPct
!
subroutine getParmReclaimedToOutputPct(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmReclaimedToOutputPct
    integer :: count,i
    integer, dimension(1:count) :: values
    forall(i=1:count) values(i)=   nint(gvf_parm_RtoOutput(i)*100.)
 return
end subroutine
subroutine setParmReclaimedToOutputPct(mydefault,count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmReclaimedToOutputPct
    integer :: mydefault,count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=mydefault*0.01
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count) gvf_parm_RtoOutput(i)= generic(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] parmReclaimedToInputPct
!
subroutine getParmReclaimedToInputPct(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmReclaimedToInputPct
    integer :: count,i
    integer, dimension(1:count) :: values
    forall(i=1:count) values(i)=   nint(gvf_parm_RtoInputMaxPct(i)*100.)
 return
end subroutine
subroutine setParmReclaimedToInputPct(mydefault,count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmReclaimedToInputPct
    integer :: mydefault,count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=mydefault*0.01
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count) gvf_parm_RtoInputMaxPct(i)= generic(i)
 return
end subroutine
! ===========================================================
! 
! ===============
! C# interface = protected internal int[] parmReclaimedOutdoorUsePct 
!
subroutine getReclaimedOutdoorUse(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export :: getReclaimedOutdoorUse
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=   nint(gvf_parm_ReclaimedOutdoor(i)*100)
 return
end subroutine
subroutine setReclaimedOutdoorUse(mydefault,count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setReclaimedOutdoorUse
    integer :: mydefault,count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=mydefault*0.01
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count)  gvf_parm_ReclaimedOutdoor(i)= generic(i)
   return
  end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] parmReclaimedWWtoROpct
!
subroutine getParmRWWToROPct(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmRWWToROPct
    integer :: count,i
    integer, dimension(1:count) :: values
    forall(i=1:count) values(i)=   nint(gvf_parm_RWWtoRO(i)*100.)
 return
end subroutine
subroutine setParmRWWToROPct(count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmRWWToROPct
    integer :: count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=0.
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count) gvf_parm_RWWtoRO(i)=generic(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] parmROReclaimedToOutputPct 
!
subroutine getParmROToOutputPct(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmROToOutputPct
    integer :: count,i
    integer, dimension(1:count) :: values
    forall(i=1:count) values(i)=   nint(gvf_parm_ROtoOutput(i)*100.)
 return
end subroutine
subroutine setParmROToOutputPct(mydefault,count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmROToOutputPct
    integer :: mydefault,count,i
    integer, dimension(1:count) :: values
   real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=mydefault*0.01
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count) gvf_parm_ROtoOutput(i)=  generic(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] parmOutdoorWaterUseResPct
!
subroutine getParmOutDoorResPct(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmOutDoorResPct
    integer :: count,i
    integer, dimension(1:count) :: values
    ! really a proportion
    forall(i=1:count) values(i)=   nint(gvf_parm_OutDoorResProp(i)*100.)
 return
end subroutine

subroutine setParmOutDoorResPct(mydefault,count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmOutDoorResPct
    integer :: mydefault,count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=mydefault*0.01
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count) gvf_parm_OutDoorResProp(i)= generic(i)
 return
end subroutine
! ===========================================================
! 
! ===============
! C# interface = protected internal int[] parmOutdoorWaterUseComPct
!
subroutine getParmOutDoorComPct(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmOutDoorComPct
    integer :: count,i
    integer, dimension(1:count) :: values
    ! really a proportion
    forall(i=1:count) values(i)=   nint(gvf_parm_OutDoorComProp(i)*100.)
 return
end subroutine
subroutine setParmOutDoorComPct(mydefault,count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmOutDoorComPct
    integer :: mydefault,count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=mydefault*0.01
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count) gvf_parm_OutDoorComProp(i)= generic(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] parmOutdoorWaterUseIndPct  
!
subroutine getParmOutDoorIndPct(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmOutDoorIndPct
    integer :: count,i
    integer, dimension(1:count) :: values
    ! really a proportion
    forall(i=1:count) values(i)=   nint(gvf_parm_OutDoorIndProp(i)*100.)
 return
end subroutine
subroutine setParmOutDoorIndPct(mydefault,count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmOutDoorIndPct
    integer :: mydefault,count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=mydefault*0.01
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count) gvf_parm_OutDoorIndProp(i)= generic(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] parmWaterSupplyToResPct
!
subroutine getParmWStoResPct(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmWStoResPct
    integer :: count,i
    integer, dimension(1:count) :: values
    ! really a proportion
    forall(i=1:count) values(i)=   nint(gvf_parm_WStoRes_prop(i)*100.)
 return
end subroutine
subroutine setParmWStoResPct(mydefault,count, values)
    !use lm_ParameterControl
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: setParmWStoResPct
    integer :: mydefault,count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=mydefault*0.01
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count) gvf_parm_WStoRes_prop(i)= generic(i)
 return
end subroutine
! ===========================================================
! 
! ===============
! C# interface =  public int[] get_parmWaterSupplyToCOMpct
!
subroutine getParmWStoComPct(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmWStoComPct
    integer :: count,i
    integer, dimension(1:count) :: values
    ! really a proportion
    forall(i=1:count) values(i)=   nint(gvf_parm_WStoCom_prop(i)*100.)
 return
end subroutine
subroutine setParmWStoComPct(mydefault,count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmWStoComPct
    integer :: mydefault,count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=mydefault*0.01
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count) gvf_parm_WStoCom_prop(i)= generic(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] parmWaterSupplyToIndPct  
!
subroutine getParmWStoIndPct(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmWStoIndPct
    integer :: count,i
    integer, dimension(1:count) :: values
    ! really a proportion
    forall(i=1:count) values(i)=   nint(gvf_parm_WStoInd_prop(i)*100.)
 return
end subroutine
subroutine setParmWStoIndPct(mydefault,count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmWStoIndPct
    integer :: mydefault,count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=mydefault*0.01
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count) gvf_parm_WStoInd_prop(i)= generic(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] parmWWtoRWWTPpct
!
subroutine getParmWWtoRWWTPPct(count, values)
    use lms_CitiWaterBudgets 
    implicit none
    dll_export :: getParmWWtoRWWTPPct
    integer :: count,i
    integer, dimension(1:count) :: values
    ! really a proportion
    forall(i=1:count) values(i)=   nint(gvf_parm_WWtoRWWTP(i)*100.)
 return
end subroutine
subroutine setParmWWtoRWWTPPct(count,values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmWWtoRWWTPPct
    integer :: count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=0.
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count) gvf_parm_WWtoRWWTP(i)=generic(i)
    !
 
 return
end subroutine
! ===========================================================
! 
! ===============
! C# interface = protected internal int[] parmWWtoEffluentPct 
!
subroutine getParmWWtoEffluentPct(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmWWtoEffluentPct
    integer :: count,i
    integer, dimension(1:count) :: values
    ! really a proportion
    forall(i=1:count) values(i)=   nint(gvf_parm_WWtoEffluent(i)*100.)
 return
end subroutine
subroutine setParmWWtoEffluentPct(mydefault,count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmWWtoEffluentPct
    integer :: mydefault,count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=mydefault*0.01
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count) gvf_parm_WWtoEffluent(i)=generic(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] parmSurfaceWaterToWbankPct 
!
subroutine getParmSWtoWBankPct(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmSWtoWBankPct
    integer :: count,i
    integer, dimension(1:count) :: values
    ! really a proportion
    forall(i=1:count) values(i)=   nint(gvf_parm_SWtoWB(i)*100.)
 return
end subroutine
subroutine setParmSWtoWBankPct(count, values)
    use lms_Provider
    implicit none
    dll_export :: setParmSWtoWBankPct
    integer :: count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=0.
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count) gvf_parm_SWtoWB(i)= generic(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] parmSurfaceWaterToWBankAmt  
!
subroutine getParmSWtoWBankAmt(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmSWtoWBankAmt
    integer :: count,i
    integer, dimension(1:count) :: values
    ! really a proportion
    forall(i=1:count) values(i)=  nint(gvf_parm_SWtoWBamount(i))
 return
end subroutine
subroutine setParmSWtoWBankAmt(mydefault,count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmSWtoWBankAmt
    integer :: mydefault,count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=mydefault
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i)*1.
    end forall
    !
    forall(i=1:count) gvf_parm_SWtoWBamount(i)= generic(i)
 return
end subroutine setParmSWtoWBankAmt
! ===========================================================
! 
! ===============
! C# interface = protected internal int[] parmSurfaceWaterToVadoseAmt  
!
subroutine getParmSWtoVadoseAmt(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmSWtoVadoseAmt
    integer :: count,i
    integer, dimension(1:count) :: values
    ! really a proportion
    forall(i=1:count) values(i)=  nint(gvf_parm_SWtoVadoseAmt(i))
return
end subroutine
subroutine setParmSWtoVadoseAmt(mydefault,count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmSWtoVadoseAmt
    integer :: mydefault,count,j
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(j=1:count)generic(j)=mydefault
    forall(j=1:count, values(j) .ne. -1)
      generic(j) = values(j)*1.
    end forall
    !
    forall(j=1:count) gvf_parm_SWtoVadoseAmt(j)= generic(j)
 return
end subroutine setParmSWtoVadoseAmt
! ===========================================================
!
! ===============
! C# interface = protected internal int[] parmWaterSupplyToDirectInjectAmt  
!
subroutine getParmWStoDIAmt(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmWStoDIAmt
    integer :: count,i
    integer, dimension(1:count) :: values
    ! really a proportion
    forall(i=1:count) values(i)=  nint(gvf_parm_WStoDIamount(i))
 return
end subroutine
subroutine setParmWStoDIAmt(mydefault,count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmWStoDIAmt
    integer :: mydefault,count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=mydefault
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i)*1.
    end forall
    !
    forall(i=1:count) gvf_parm_WStoDIamount(i)= generic(i)
 return
end subroutine
! ===========================================================
! 
! ===============
! C# interface = protected internal int[] parmEffluentToPowerPlantPct  
!
subroutine getParmEffluentToPPlantPct(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmEffluentToPPlantPct
    integer :: count,i
    integer, dimension(1:count) :: values
    ! really a proportion
    forall(i=1:count) values(i)=   nint(gvf_parm_EffluenttoPP(i)*100.)
end subroutine
subroutine setParmEffluentToPPlantPct(count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmEffluentToPPlantPct
    integer :: count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=0.0
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
 
    forall(i=1:count) gvf_parm_EffluenttoPP(i)= generic(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface =protected internal int[] parmEffluentToVadosePct
!
subroutine getParmEffluenttoVadosePct(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmEffluenttoVadosePct
    integer :: count,i
    integer, dimension(1:count) :: values
    ! really a proportion
    forall(i=1:count) values(i)=   nint(gvf_parm_EffluentToVadose(i)*100.)
 return
end subroutine
subroutine setParmEffluenttoVadosePct(count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmEffluenttoVadosePct
    integer :: count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=0.0
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count) gvf_parm_EffluentToVadose(i)= generic(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] parmGroundwaterToGWTPlantPct
!
subroutine getParmGWtoGWTPPct(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getParmGWtoGWTPPct
    integer :: count,i
    integer, dimension(1:count) :: values
    ! really a proportion
    forall(i=1:count) values(i)=   nint(gvf_parm_GWtoGWTP(i)*100.)
 return
end subroutine
subroutine setParmGWtoGWTPPct(count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmGWtoGWTPPct
    integer :: count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=0.0
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
    forall(i=1:count) gvf_parm_GWToGWTP(i)= generic(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal bool set_parmIncludeMeteorology
! In the interface, but Not referenced, so not in API
subroutine setParmIncludeMeteo(value)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmIncludeMeteo
    logical :: value
    !
    gvl_IncludeMeteorology=.false.
    if(value)gvl_IncludeMeteorology = value
 return
end subroutine
! ===========================================================
!
!
!           =========================
!              CityModel Outputs
!
! ===============
! C# interface =    protected internal int[] get_GrayWaterResidential 
! In the interface, but Not referenced, so not in API
!
subroutine getGrayWaterResidential(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export :: getGrayWaterResidential
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_graywaterWaterUsed_AF(i)*gvf_parm_WStoRes_prop(i)  
 return
end subroutine
! ===========================================================
! 
! ===============
! C# interface = protected internal int[] get_GrayWaterCommercial      
! In the interface, but Not referenced, so not in API
!
subroutine getGrayWaterCommercial(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export :: getGrayWaterCommercial
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_graywaterWaterUsed_AF(i)*gvf_parm_WStoCom_prop(i)
 return
end subroutine
! ===========================================================
! 
! ===============
! C# interface = protected internal int[] get_GrayWaterIndustrial    
! In the interface, but Not referenced, so not in API
!
subroutine getGrayWaterIndustrial(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export :: getGrayWaterIndustrial
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_graywaterWaterUsed_AF(i)*gvf_parm_WStoInd_prop(i)
 return
end subroutine
! ===========================================================
! 
! ===============
! C# interface = protected internal int[] get_GroundwaterBankUsed   
!
subroutine getGWbankedUsed(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export :: getGWbankedUsed
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_GWBankUsed_acft(i) 
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] get_GroundwaterBankBalance     
!
subroutine getGWbanked(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export :: getGWbanked
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=  go_GWBanked_acft(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] get_ReclaimedWaterTotal  
!
subroutine getReclaimedWaterTotal(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export :: getReclaimedWaterTotal
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=  go_ReclaimedWtotal_acft(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface =  
!
! NOT in the Interface - consider deleting
subroutine getReclaimedWaterOut(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export :: getReclaimedWaterOut
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=  go_ReclaimedWoutput_acft(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] get_ReclaimedWaterUsed    
!
subroutine getReclaimedWaterUsed(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export :: getReclaimedWaterUsed
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=  go_ReclaimedWused_acft(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] get_ReclaimedWaterToVadose 
!
subroutine getReclaimedWaterToVadose(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export :: getReclaimedWaterToVadose
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=  go_ReclaimedWrecharged_acft(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] get_ReclaimedWaterDischarged    
!
subroutine getReclaimedWaterDischarged(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export :: getReclaimedWaterDischarged
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=  go_ReclaimedWdischarged_acft(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] get_ReclaimedWaterDirectInject  
!
subroutine getReclaimedWaterDInject(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export :: getReclaimedWaterDInject
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=   go_ReclaimedWDI_acft(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] get_ROreclaimedWaterCreated     
!
subroutine getROReclaimedWaterOut(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export :: getROReclaimedWaterOut
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=  go_ROReclaimedWoutput_acft(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] get_ROreclaimedWaterUsed     
!
subroutine getROReclaimedWaterUsed(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export :: getROReclaimedWaterUsed
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=  go_ROReclaimedWused_acft(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] get_ROreclaimedWaterDirectInjec   
!
subroutine getROReclaimedWaterDI(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export :: getROReclaimedWaterDI
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=   go_ROReclaimedWDI_acft(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] get_TotalEffluentReused     
!
subroutine getEffluentCreated(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export :: getEffluentCreated
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=    go_Effluent_acft(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] get_TotalTWWTP   
!
subroutine getTWWTPCreated(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export :: getTWWTPCreated
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=    go_TWWTP_acft(i)
 return
end subroutine
! ===========================================================

! ===============
! C# interface =protected internal int[] get_EffluentToPowerPlant     
!
subroutine getEffluentToPowerPlant(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getEffluentToPowerPlant
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=   go_EffluentPPlant_acft(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface =protected internal int[] get_EffluentToVadose    
!
subroutine getEffluentToVadose(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getEffluentToVadose
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=   go_EffluentVadose_acft(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] get_EffluentDischarged  
!
subroutine getEffluentToDischarge(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getEffluentToDischarge
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=   go_EffluentToDischarge_acft(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int get_EffluentToAgriculture  
!
function getEffluentToAg() result(value)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getEffluentToAg
    integer :: value

    value =go_EffluentAg_acft
end function
! ============================================================
!
!                   --------------------------------------
!                            GPCD drivers and outputs
!
!
! ------------------------------------------------------------
!
! ===============
! C# interface = protected internal int get_EffluentToAgriculture  
!
function getNonPotableOutdoorUse() result(value)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getNonPotableOutdoorUse
    integer :: value

    value = nint(gof_nonPotable_useOutdoor)
end function

!
! ===============
! C# interface = internal double[] get_FlushesPCD 
!
subroutine getFlushesPCD(count, values)
   use   lms_CitiWaterBudgets
    implicit none
    dll_export ::  getFlushesPCD
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= nint(gof_flushesPCD(i)*100.)
 return
end subroutine
! ===========================================================
!
! ! 07.02.12
! ==============
! C# interface=    protected internal int[] parmRateResLeakagePct
!
!subroutine getRateResLeakage(count, values)
!   use lm_ParameterControl
!    implicit none
!    dll_export ::  getRateResLeakage
!    integer :: count,i
!    integer, dimension(1:count) :: values
! ! 
!    forall(i=1:count) values(i)=    nint(gvf_RateResLeak(i)*100)
! return
!end subroutine
!subroutine setRateResLeakage(mydefault,count, values)
!    use lm_ParameterControl
!    implicit none
!    dll_export :: setRateResLeakage
!    integer :: count,i,mydefault
!    integer, dimension(1:count) :: values
!    real, dimension(1:count) :: generic
!    ! Note:  
!    forall(i=1:count)generic(i)=mydefault*0.01
!    forall(i=1:count, values(i) .gt. -1)
!      generic(i) = values(i) * 0.01
!    end forall
!    !
!   forall(i=1:count) gvf_RateResLeak(i)=generic(i) 
! return
!end subroutine
! ===========================================================
! 07.02.12
! ==============
! C# interface = protected internal int[] parmBlackWaterResPct
!
subroutine getParmBlackWaterPct(count, values)
   use lm_ParameterControl
    implicit none
    dll_export ::  getParmBlackWaterPct
    integer :: count,i
    integer, dimension(1:count) :: values
 ! 
    forall(i=1:count) values(i)=    nint(gvf_parm_BWResProp(i)*100)
 return
end subroutine
subroutine setParmBlackWaterPct(mydefault,count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: setParmBlackWaterPct
    integer :: count,i,mydefault
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !  Note: 
    forall(i=1:count)generic(i)=mydefault*0.01
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) * 0.01
    end forall
    !
   forall(i=1:count) gvf_parm_BWResProp(i)=generic(i) 
 return
end subroutine
! ===========================================================
! 08.11.12
! ==============
! C# interface = internal double[] get_ShowersBathsMinPCD   
!
subroutine getShowerBathPCD(count, values)
   use   lms_CitiWaterBudgets
    implicit none
    dll_export ::  getShowerBathPCD
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= nint(gof_showersBathPCD(i))
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int set_ShowersBathsPCT
!
subroutine setShowerBathPCT(value)
    use  lm_ParameterControl
    implicit none
    dll_export :: setShowerBathPCT
    integer :: value
    !
    gif_showersBathPCT=0.24
    if(0 < value)gif_showersBathPCT =value
  return
end subroutine
! ===========================================================
!
!
! -----------------------------------------------------------------------------
! ===========================================================
!
!                    -----------------------------
!                           General Provider
!
! ===============
! C# interface = protected internal int[] get_DemandDeficit    
!
subroutine getDemandDeficit(count,values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getDemandDeficit
    integer :: count
    integer :: i
    integer, dimension(1:count) :: values
    !
    forall(i=1:count) values(i)=   go_DemandDeficit_acft(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal bool set_parmAllStrawsSucking 
!
subroutine setParmAllStraws(value)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmAllStraws
    logical :: value
    !
    gvl_parmAllStrawsSucking = value
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] NewWaterSupplies  
!
subroutine getNewWaterSupplies(count, values)
   use lms_Provider
    implicit none
    dll_export ::  getNewWaterSupplies
    integer :: count,i
    integer, dimension(1:count) :: values
    !
    forall(i=1:count) values(i)=gvi_newWaterSupplies_acft_a(i)
 return
end subroutine
subroutine setNewWaterSupplies(count, values)
   use lms_Provider
    implicit none
    dll_export ::  setNewWaterSupplies
    integer :: count,i
    integer, dimension(1:count) :: values
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=0.0
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i)
    end forall

    forall(i=1:count) gvi_newWaterSupplies_acft_a(i)=generic(i)
 return
end subroutine

! ===========================================================
!
! ===============
! C# interface = protected internal int[] get_NewWaterSuppliesUsed
!
subroutine getNewWaterSuppliesUsed(count, values)
   use lms_Provider
    implicit none
    dll_export ::  getNewWaterSuppliesUsed
    integer :: count,i
    integer, dimension(1:count) :: values
    !
    forall(i=1:count) values(i)=go_newSupplies_acft_a(i)

 return
end subroutine


! ===============
! C# interface = protected internal int[] get_WaterCreditDeficits 
!
subroutine getCreditDeficits(count, values)
   use lms_Provider
    implicit none
    dll_export ::  getCreditDeficits
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=  nint(gvf_deficitGWcredits_acft(i))
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] get_WaterCreditTotals 
!
subroutine getTotalCredits(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getTotalCredits
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=   go_totalCredits_acft(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int[] get_WaterCreditIncidental 
!
subroutine getIncidentalCredits(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getIncidentalCredits
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=   go_incidentalCredit_acft(i)
 return
end subroutine

!
! ===============
! C# interface = protected internal int[] get_WaterCreditsAgAdded
!
subroutine getAgToMuniAddedCredits(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getAgToMuniAddedCredits
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=   go_addedCreditsAgToMuni_acft(i)
 return
end subroutine

! ===============
! C# interface = protected internal int[] get_WaterCreditsTotalAdded
!
subroutine getTotalAddedCredits(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getTotalAddedCredits
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=   go_totalAddedCredits_acft(i)
 return
end subroutine


! ===========================================================
!
!  protected internal int[] NormalFlow_rights_max
! ========
! 
subroutine getNormalFlowEmpiricalMax(count, values)
   use lms_ParameterControl
    implicit none
    dll_export :: getNormalFlowEmpiricalMax 
    integer :: count,i
    integer, dimension(1:count) :: values
    !real, dimension(1:count) :: generic
    !
    count=10
    forall(i=1:count) values(i)=gvi_NormalFlowEmpMax_AFac(i)
 return
end subroutine
subroutine setNormalFlowEmpiricalMax(count, values)
   use lms_ParameterControl
    implicit none
    dll_export :: setNormalFlowEmpiricalMax 
    integer :: count,i
    integer, dimension(1:count) :: values
    integer, dimension(1:count) :: generic
    !
    count=10
    forall(i=1:count)generic(i)=90
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i)
    end forall
    !
    forall(i=1:count) gvi_NormalFlowEmpMax_AFac(i)=generic(i)
 return
end subroutine
!
!            -----------------------------------
!               Regional Groundwater model from 
!               regional and provider-specific
!
!
! ===============
! C# interface = protected internal int get_RegionalGroundWaterBalance 
!
function getRegionalGroundwater() result(value)
    use  lms_CitiWaterBudgets
    implicit none
    dll_export :: getRegionalGroundwater
    integer :: value

   value = nint(gof_regionalGW)

end function
! ===========================================================
! 
! ===============
! C# interface = protected internal int get_RegionalAgOtherPumping 
!
function getAgAndOtherPumping() result(value)
    use  lms_CitiWaterBudgets
    implicit none
    dll_export :: getAgAndOtherPumping
    integer :: value

   value = nint(gof_AgAndOtherPumped)

end function
! ===========================================================
!
! ===============
! C# interface = protected internal int get_RegionalInflow 
!
function getRegionalInflow() result(value)
    use  lms_CitiWaterBudgets
    implicit none
    dll_export :: getRegionalInflow
    integer :: value

   value = nint(gof_Inflow)

end function
! ===========================================================
!
! ===============
! C# interface = protected internal int get_RegionalOutflow 
!
function getRegionalOutflow() result(value)
    use  lms_CitiWaterBudgets
    implicit none
    dll_export :: getRegionalOutflow
    integer :: value

   value = nint(gof_Outflow)

end function
! ===========================================================
!
! =============== 
! C# interface = protected internal int get_RegionalProviderRecharge 
!
function getRegionalRecharge() result(value)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getRegionalRecharge
    integer :: value
    ! 
    value =   nint(gvf_regionalGWrecharged)

end function
! ===========================================================
!
! ===============
! C# interface = protected internal int get_RegionalNaturalRecharge   
!
function getNaturalRecharge() result(value)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getNaturalRecharge
    integer :: value

    value = nint(gof_naturalRecharge)
end function
! ===========================================================
!
! ===============
! C# interface = protected internal int[] get_VadoseToAquiferFlux    
!
subroutine getfluxVadoseToAquifer(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getfluxVadoseToAquifer
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)=  go_fluxVadoseToAquifer(i)
 return
end subroutine
! ===========================================================
!
! ===============
! C# interface = protected internal int get_RegionalAgToVadoseFlux   
!
function getAgToVadoseFlux() result(value)
    use lms_AgWaterBudgets
    implicit none
    dll_export :: getAgToVadoseFlux
    integer :: value

    value = nint(gvf_AgToVadoseFlux)
end function
! ===========================================================
!
! ===============
! C# interface = protected internal int CAGRD   
!
function getCAGRD() result(value)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getCAGRD
    integer :: value

    value =go_CAGRD_acft
end function
subroutine setCAGRD(value)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: setCAGRD
    integer :: value
    !
    gii_CAGRD_acft=0.
    if(0 < value)gii_CAGRD_acft = value
    !
 return
end subroutine
! ===========================================================
!
!              ============================================
!               Special Projects- Ajay and Johnston project
!                               08.06.12
!
!               Johnson and Vince will use this parameter
!                   gvf_parm_BWResProp(i)=0.26 
!                   gvf_parm_BWCioProp(i)=0.25 
!                   !
!                   gvf_RateResLeak(i)=0.0553
!                   gvf_RateComLeak(i)=0.0553
!                   gvf_RateIndLeak(i)=0.0553

!                   gvf_parm_OutDoorResProp(i)=0.5
!                   gvf_parm_OutDoorComProp(i)=0.5
!                   gvf_parm_OutDoorIndProp(i)=0.5

!                   gvf_gpcdIndoor_behavior(i,1,1)    
!
! ==============
!
!
! 07.02.12
! ==============
! C# interface= protected internal bool set_parmVinze_Johnston
!
subroutine set_parmVinze_Johnston(value)
    use lm_ParameterControl
    implicit none
    dll_export :: set_parmVinze_Johnston
    logical :: value
    gvl_modelVinzeJohnston=.false.
    if(value)then
      gvl_modelVinzeJohnston = value
    endif
 return
end subroutine
! ===========================================================
!
! Only used if demand option eq 4
!
! ==============
! C# interface = protected internal int[] ProviderGPCD
!
subroutine getProviderGPCD(count, values)
    use lms_Provider
    implicit none
    dll_export :: getProviderGPCD
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)=go_SES_GPCD(i)
    ! Provider.f90 line 796
 return
end subroutine
subroutine setProviderGPCD(count, values)
    use lms_ProviderPopandDemand 
    implicit none
    dll_export :: setProviderGPCD
    integer :: count,i
    integer, dimension(1:count) :: values    
    real, dimension(1:count) :: generic
    !
    forall(i=1:count)generic(i)=0.
    forall(i=1:count, values(i) .gt. -1)
      generic(i) = values(i) 
    end forall
    !
    forall(i=1:count) gv_AdjustedGPCD(i)= generic(i)
 return
end subroutine
! ===========================================================
!
! ==============
! C# interface =  protected internal int[] get_ProviderGPCDraw
!
subroutine getProviderGPCDraw(count, values)
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: getProviderGPCDraw
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)=go_rawGPCD(i)

    ! 
 return
end subroutine
!
! ==============
! C# interface =  protected internal int[] get_ProviderGPCDraw
!
subroutine getProviderGPCDres(count, values)
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: getProviderGPCDres
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)=go_residentialGPCD(i)
    ! 
 return
end subroutine
!
! ==============
! C# interface =  protected internal int[] get_ProviderGPCDraw
!
subroutine getProviderGPCDind(count, values)
    use lms_ProviderPopandDemand
    implicit none
    dll_export :: getProviderGPCDind
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)=go_industrialGPCD(i)
    ! 
 return
end subroutine
!


! ===============
! C# interface = protected internal int set_CommercialTurfPct 
!
subroutine setWaterCommercialTurf(value)
    use  lm_ParameterControl
    implicit none
    dll_export :: setWaterCommercialTurf
    integer :: value
    !
     gvf_WaterToCommercialTurf_Prop =0
     if(0 < value)gvf_WaterToCommercialTurf_Prop = value * 0.01
    !
  return
end subroutine
! ===========================================================
!

! ===============
! C# interface =
!
subroutine getGPCDResBehavior(count,array,values)
   use lms_GPCDdemand
    implicit none
    dll_export :: getGPCDResBehavior
    integer :: count,array,i,j
    integer, dimension(1:count,1:array) :: values
    ! 
    forall(i=1:count,j=1:array) values(i,j)=0
    forall(i=1:count,j=1:array) values(i,j)=   gvf_gpcdIndoor_behavior(i,j,1)
 return
end subroutine
subroutine setGPCDResBehavior(count,array,values)
   use lms_GPCDdemand
    implicit none
    dll_export :: setGPCDResBehavior
    integer :: count,array,i,j
    integer, dimension(1:count,1:array) :: values
    ! 
    forall(i=1:count,j=1:array)  gvf_gpcdIndoor_behavior(i,j,1)=0
    forall(i=1:count,j=1:array)  gvf_gpcdIndoor_behavior(i,j,1)= values(i,j)
 return
end subroutine
! =============================================================================
!
! ===============
! C# interface =  protected internal bool APIdefaultStatus
!
function get_APIinterFaceStatus() result(value)
    use lm_ParameterControl
    implicit none
    dll_export :: get_APIinterFaceStatus
    logical :: value

    value=gvl_APIstatus

end function
subroutine set_APIinterFaceStatus(value)
    use lm_ParameterControl
    implicit none
    dll_export :: set_APIinterFaceStatus
    logical :: value
    gvl_APIstatus=.false.
    if(value)then
      gvl_APIstatus = value
    endif
 return
end subroutine
!
! 02.10.15
! ===============
! C# interface = protected internal int[] set_ProviderFlowToCOdelta
!
subroutine setProviderFlowToCOdelta_AF(count, values)
   use lm_ParameterControl
    implicit none
    dll_export ::  setProviderFlowToCOdelta_AF
    integer :: count,i
    integer, dimension(1:count) :: values
    integer, dimension(1:count) :: generic
    ! 
    forall(i=1:count)generic(i)=0
    forall(i=1:count, values(i) .gt. -1)
      generic(i) =values(i)
    end forall
    forall(i=1:count) gvi_ProviderFlowToCOdelta_AF(i) = generic(i)
 return
end subroutine
!
! 04.07.15 DAS
subroutine getProviderCOdelataRatioAZ(count, values)
    use lms_Designations_B
    implicit none
    dll_export :: getProviderCOdelataRatioAZ
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)=go_COdeltaRatioProvider_az(i)
    ! 
 return
end subroutine
!
!
!
! 07.30.15
! ===============
! C# interface =  protected internal int[] parmDefaultPumpingMandIPct
!
subroutine setDefaultPumpingMandIPct(count, values)
   use lm_ParameterControl
    implicit none
    dll_export :: setDefaultPumpingMandIPct 
    integer :: count,i
    integer, dimension(1:count) :: values
    integer, dimension(1:count) :: generic
    ! 
    forall(i=1:count)generic(i)=0
    forall(i=1:count, values(i) .gt. -1)
      generic(i) =values(i)
    end forall
    forall(i=1:count) gvi_defaultMandIPumping_Pct(i) = generic(i)
 return
end subroutine
subroutine getDefaultPumpingMandIPct(count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: getDefaultPumpingMandIPct
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)=gvi_defaultMandIPumping_Pct(i)
    ! 
 return
end subroutine
!
! 07.30.15, 08.04.15 (renamed)
! ===============
! C# interface =  protected internal int[] parmDefaultPumpingMandIPct
!
subroutine getCAPlossPotentialPriority4_AF(count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: getCAPlossPotentialPriority4_AF
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)= go_lossPotentialP4(i)
    ! 
 return
end subroutine
!
subroutine getCAPlossPotentialPiority5_AF(count, values)
    use lm_ParameterControl
    implicit none
    dll_export :: getCAPlossPotentialPiority5_AF
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)= go_lossPotentialP5(i)
    ! 
 return
end subroutine
!
! 08.04.15
! ===============
! C# interface =  protected internal int[] parmDefaultPumpingMandIPct
!
!subroutine getCAPdemandNotMetPriority4_AF(count, values)
!    use lm_ParameterControl
!    implicit none
!    dll_export :: getCAPdemandNotMetPriority4_AF
!    integer :: count,i
!    integer, dimension(1:count) :: values
!
!    forall(i=1:count) values(i)= go_rightsLostP4(i)
!    ! 
! return
!end subroutine
!!
!subroutine getCAPdemandNotMetPriority5_AF(count, values)
!    use lm_ParameterControl
!    implicit none
!    dll_export :: getCAPdemandNotMetPriority5_AF
!    integer :: count,i
!    integer, dimension(1:count) :: values
!
!    forall(i=1:count) values(i)= go_rightsLostP5(i)
!    ! 
! return
!end subroutine
!
! 10.05.15
!    protected internal int[] get_ColoradoUnusedPriority4  // af for the year by provider
!
subroutine getCAPpriority4NotUsed_AF(count, values)
    use lm_ParameterControl
    implicit none
    dll_export ::getCAPpriority4NotUsed_AF
    integer :: count,i
    integer, dimension(1:count) :: values
    !
    forall(i=1:count) values(i)= go_CAPunusedPriority4(i)
    ! 
 return
end subroutine
!
! 10.05.15
!    protected internal int[] get_ColoradoUnusedPriority5  // af for the year by provider
!
subroutine getCAPpriority5NotUsed_AF(count, values)
    use lm_ParameterControl
    implicit none
    dll_export ::getCAPpriority5NotUsed_AF
    integer :: count,i
    integer, dimension(1:count) :: values
    !
    forall(i=1:count) values(i)= go_CAPunusedPriority5(i)
    ! 
 return
end subroutine
!
!subroutine getCAPpriority4Available_AF(count, values)
!    use lm_ParameterControl
!    implicit none
!    dll_export ::getCAPpriority4Available_AF
!    integer :: count,i
!    integer, dimension(1:count) :: values
!
!    forall(i=1:count) values(i)= go_CAPp4Available(i)
!    ! 
! return
!end subroutine
!!
!subroutine getCAPpriority5Available_AF(count, values)
!    use lm_ParameterControl
!    implicit none
!    dll_export ::getCAPpriority5Available_AF
!    integer, dimension(1:count) :: values
!    integer :: count,i
!
!    forall(i=1:count) values(i)= go_CAPp5Available(i)
!    ! 
! return
!end subroutine

! 09.04.15
! ===============
! C# interface =  protected internal int[]

!subroutine getDataCheckOnMetBC_AF(count, values)
!    use lms_Provider
!    implicit none
!    dll_export :: getDataCheckOnMetBC_AF
!    integer :: count,i
!    integer, dimension(1:count) :: values
!
!    forall(i=1:count) values(i)= go_dataCheckOnMet_10(i)
!    ! 
! return
!end subroutine

! 09.04.15
! ===============
! C# interface =  protected internal int[]

!subroutine getDataCheckOffMetGW_AF(count, values)
!    use lms_Provider
!    implicit none
!    dll_export :: getDataCheckOffMetGW_AF
!    integer :: count,i
!    integer, dimension(1:count) :: values
!
!    forall(i=1:count) values(i)= go_dataCheckOffMet_15(i)
!    ! 
! return
!end subroutine
!
! -------------------------------------------------------------------------------------
! WaterSim 6
!
! Use/ calculate Gray Water as an output 
! ==============
! C# Interface = protected internal bool set_parmCalculateGrayWater
!
! --------------------------------
subroutine setParmGrayWater(value)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmGrayWater
    logical :: value
    !
    gvl_parm_grayWater=.false.
    if(value)gvl_parm_grayWater = value
 return
end subroutine
! ===========================================================
!
! How much of the potential grayWater is being used 
! ==============
! C# Interface =  protected internal int set_parmGrayWaterCompliancePCT
!
! --------------------------------
subroutine setParmGrayWaterCompliance(value)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmGrayWaterCompliance
    integer :: value

    gvf_grayWaterCompliance=value * (1/100.)
 return
end subroutine
!
! --------------------------------
subroutine setParmYearsToAdopt(value)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmYearsToAdopt
    integer :: value

    gvf_yearsToAdopt=value
 return
end subroutine

! Use/ calculate Rain Water Harvesting 
! ==============
! C# Interface = protected internal bool set_parmCalculateRainWaterHarvesting
!
! --------------------------------
subroutine setParmRainWaterHarvesting(value)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmRainWaterHarvesting
    logical :: value
    !
    if(value)gvl_rainWaterHarvesting = value
 return
end subroutine
!
subroutine setParmRainWaterHarvestResOnly(value)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmRainWaterHarvestResOnly
    logical :: value
    !
    if(value)gvl_rainWaterHarvestResOnly = value
 return
end subroutine
!
subroutine setParmRainWaterHarvestComOnly(value)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmRainWaterHarvestComOnly
    logical :: value
    !
    if(value)gvl_rainWaterHarvestComOnly = value
 return
end subroutine
! ===========================================================
!
! Use/ calculate Rain Water Harvesting 
! ==============
! C# Interface = protected internal bool set_parmCalculateRainWaterHarvesting
!
! --------------------------------
subroutine setParmStormWaterHarvesting(value)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmStormWaterHarvesting
    logical :: value
    !
    if(value)gvl_stormWaterHarvesting = value
 return
end subroutine
! ===========================================================
! Use/ calculate Storm Water Capture using this capacity and runoff 
! ==============
! C# Interface =  protected internal int[] set_StormWaterCapacity
! 11.20.15
! --------------------------------
subroutine setStormWaterCapacity_m3(count, values)
   use lm_ParameterControl
    implicit none
    dll_export :: setStormWaterCapacity_m3 
    integer :: count,i
    integer, dimension(1:count) :: values
    integer, dimension(1:count) :: generic
    ! 
    forall(i=1:count)generic(i)=0
    forall(i=1:count, values(i) .gt. -1)
      generic(i) =values(i)
    end forall
    forall(i=1:count) gii_StormWaterCapacity_m3(i) = generic(i)
 return
end subroutine
! --------------------------------
subroutine getStormWaterCapacity_m3(count, values)
   use lm_ParameterControl
    implicit none
    dll_export :: getStormWaterCapacity_m3 
    integer :: count,i
    integer, dimension(1:count) :: values
    
    forall(i=1:count) values(i) = gii_StormWaterCapacity_m3(i) 
 return
end subroutine
!
! ==============
! C# Interface = protected internal bool set_parmCalculateRainWaterHarvesting
!
! --------------------------------
subroutine setParmIwaniecPPtoAgEffluent(value)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmIwaniecPPtoAgEffluent
    logical :: value
    !
    if(value)gvl_IwaniecScenarios_PPtoAg = value
 return
end subroutine
! ===========================================================
!
! ==============
! C# Interface = protected internal bool set_parmIwaniecNoLeaks
!
subroutine setParmIwaniecLeaks(value)
    use lm_ParameterControl
    implicit none
    dll_export :: setParmIwaniecLeaks
    logical :: value
    !
    if(value)gvl_NoleaksYN = value
 return
end subroutine
! ===========================================================
!
! 11.11.15
! ===============
!  C# interface =  protected internal int[]
subroutine getGPCD_resGrayWater(count, values)
    use lms_Provider
    implicit none
    dll_export :: getGPCD_resGrayWater
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)=  go_resGPCDgrayWater(i)
    ! 
 return
end subroutine
!
! 11.11.15
! ===============
!  C# interface =  protected internal int[]
subroutine getGPCD_comGrayWater(count, values)
    use lms_Provider
    implicit none
    dll_export :: getGPCD_comGrayWater
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)=  go_comGPCDgrayWater(i)
    ! 
 return
end subroutine
!
!
! 11.11.15
! ===============
!  C# interface =  protected internal int[]
subroutine getGPCD_indGrayWater(count, values)
    use lms_Provider
    implicit none
    dll_export :: getGPCD_indGrayWater
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)=  go_indGPCDgrayWater(i)
    ! 
 return
end subroutine
!
! =====================================================================================================
! WaterSim 6 additions
! 
!
! ==============
! C# Interface = protected internal bool set_parmIwaniecNoLeaks
!
subroutine setparmIwaniecScenarios(value)
    use lm_ParameterControl
    implicit none
    dll_export :: setparmIwaniecScenarios
    logical :: value
    !
    gvl_IwaniecYN=.false.
    if(value)gvl_IwaniecYN = value
 return
end subroutine
subroutine getparmIwaniecScenarios(value)
    use lm_ParameterControl
    implicit none
    dll_export :: getparmIwaniecScenarios
    logical :: value

     value=gvl_IwaniecYN
 return
end subroutine
!
subroutine setparmWaterSim5(value)
    use lm_ParameterControl
    implicit none
    dll_export :: setparmWaterSim5
    logical :: value
    !
    gvl_waterSim5YN=.false.
    if(value)gvl_waterSim5YN = value
 return
end subroutine
subroutine getparmWaterSim5(value)
    use lm_ParameterControl
    implicit none
    dll_export :: getparmWaterSim5
    logical :: value

     value=gvl_waterSim5YN
 return
end subroutine






! ===========================================================
! ==============
! C# Interface = protected internal 
!
subroutine setScenario(value)
    use lm_ParameterControl
    implicit none
    dll_export :: setScenario
    integer :: value

    gvi_IwaniecScenario=7
    if(0 < value) gvi_IwaniecScenario=value

 return
end subroutine
subroutine getScenario(value)
    use lm_ParameterControl
    implicit none
    dll_export :: getScenario
    integer :: value

     value=gvi_IwaniecScenario
 return
end subroutine
!
! ===============
!  C# interface =  
subroutine get_harvestedStormWater(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: get_harvestedStormWater
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)=  go_HarvestStormWater_AF(i)
    ! 
 return
end subroutine
!
! ==============
! C# Interface = 
!
! --------------------------------
subroutine setAgEfficiency(value)
    use lms_AgWaterBudgets
    implicit none
    dll_export :: setAgEfficiency
    integer :: value
    !
    go_AgEfficiency = value
    !
 return
end subroutine
!
! =====================
! C# interface =  protected internal int[] get_GroundwaterUsed_Agriculture
!
subroutine getAgPumpingTotal(count, values)
   use lms_AgWaterBudgets
    implicit none
    dll_export ::  getAgPumpingTotal
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_groundwaterAgProvider_AF(i)
 return
end subroutine
!
! 06.17.16 - new output
! =====================
! C# interface =   protected internal int[] get_SurfaceUsed_Agriculture
!
subroutine getAgSurfaceTotal(count, values)
   use lms_AgWaterBudgets
    implicit none
    dll_export ::  getAgSurfaceTotal
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_surfAgProvider_AF(i)
 return
end subroutine
!
! 07.16.16 - new output
! 08.11.16 moved here
! =====================
! C# interface =  protected internal int[] get_EffluentUsed_Agriculture  
!
subroutine get_effluentUsedAg(count, values)
    use lms_AgWaterBudgets
    implicit none
    dll_export :: get_effluentUsedAg
    integer :: count,i
    integer, dimension(1:count) :: values

    forall(i=1:count) values(i)=  go_effluentAgProvider_AF(i)
    ! 
 return
end subroutine
!
! ==============
! C# interface =  protected internal bool parmUseLCLU
! 08.18.16
function getUseLCLU() result(value)
    use lm_Kernel
    implicit none
    dll_export :: getUseLCLU
    logical :: value

   value=gvl_utilizeLCLU
end function
subroutine setUseLCLU(value)
    use  lm_Initialize
    implicit none
    dll_export :: setUseLCLU
    logical :: value

   gvl_utilizeLCLU=value
 return
end subroutine
!
! ==============
! C# interface =   protected internal int[] get_RainWaterHarvested  
! 08.18.16
subroutine getRainWaterHarvested(count, values)
   use lm_Kernel
    implicit none
    dll_export ::  getRainWaterHarvested
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_harvRainWaterUsed_AF(i) !go_harvestRainWater_AF(i)
 return
end subroutine
!
! ==============
! C# interface =  protected internal int[] get_RainWaterHarvested_SF
! 12.30.16
subroutine getRainWaterHarvested_SF(count, values)
   use lm_Kernel
    implicit none
    dll_export ::  getRainWaterHarvested_SF
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_rainWaterHarvested_SF_AF(i)
 return
end subroutine
!
! ==============
! C# interface =  protected internal int[] get_RainWaterHarvested_MF 
! 12.30.16
subroutine getRainWaterHarvested_MF(count, values)
   use lm_Kernel
    implicit none
    dll_export ::  getRainWaterHarvested_MF
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_rainWaterHarvested_MF_AF(i)
 return
end subroutine
!
! ==============
! C# interface =  protected internal int[] get_RainWaterHarvested_PU 
! 12.30.16
subroutine getRainWaterHarvested_PU(count, values)
   use lm_Kernel
    implicit none
    dll_export ::  getRainWaterHarvested_PU
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_rainWaterHarvested_PU_AF(i)
 return
end subroutine
!
! ==============
! C# interface =  
! 12.30.16
subroutine getRainWaterHarvested_COM(count, values)
   use lm_Kernel
    implicit none
    dll_export ::  getRainWaterHarvested_COM
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_rainWaterHarvested_COM_AF(i)
 return
end subroutine

! ==============

! ==============
!
!
! ==============
! C# interface =  protected internal int 
! 08.18.16
!function getRainHarvestToTotalOutdoor() result(value)
!    use  lms_CitiWaterBudgets
!    implicit none
!    dll_export :: getRainHarvestToTotalOutdoor
!    integer :: value
!
!   value=go_RainHarvestToTotalOutdoor
!end function
!!
subroutine getRainHarvestToTotalOutdoor(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getRainHarvestToTotalOutdoor
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_RainHarvestToTotalOutdoor(i)
 return
end subroutine
!! ==============
!! C# interface =  protected internal int 
!! 01.04.17
!function getGrayWaterToTotalOutdoor() result(value)
!    use  lms_CitiWaterBudgets
!    implicit none
!    dll_export :: getGrayWaterToTotalOutdoor
!    integer :: value
!
!   value=go_GrayWaterToTotalOutdoor
!end function
!!
subroutine getGrayWaterToTotalOutdoor(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getGrayWaterToTotalOutdoor
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_GrayWaterToTotalOutdoor(i)
 return
end subroutine
!
! ==============
! C# interface =  protected internal int 
! 01.18.17
!
subroutine getReclaimedToTotalOutdoor(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getReclaimedToTotalOutdoor
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_ReclaimedToTotalOutdoor(i)
 return
end subroutine
!
! ==============
! C# interface =  protected internal int 
! 01.09.17
subroutine getAnnualDemand_AG_AF(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getAnnualDemand_AG_AF
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_demand_AG_AF(i)
 return
end subroutine
!!
! ==============
! C# interface =  protected internal int 
! 01.09.17
subroutine getAnnualDemand_MD_AF(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getAnnualDemand_MD_AF
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_demand_MD_AF(i)
 return
end subroutine
!
! ==============
! C# interface =  protected internal int 
! 01.09.17
subroutine getAnnualDemand_HD_AF(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getAnnualDemand_HD_AF
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_demand_HD_AF(i)
 return
end subroutine
!
! ==============
! C# interface =  protected internal int 
! 01.09.17
subroutine getAnnualDemand_LD_AF(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getAnnualDemand_LD_AF
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_demand_LD_AF(i)
 return
end subroutine
!
! ==============
! C# interface =  protected internal int 
! 01.09.17
subroutine getAnnualDemand_Turf_AF(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getAnnualDemand_Turf_AF
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_demand_TurfTree_AF(1,i)
 return
end subroutine
!
! ==============
! C# interface =  protected internal int 
! 01.09.17
subroutine getAnnualDemand_GWay_AF(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getAnnualDemand_GWay_AF
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_demand_TurfTree_AF(2,i)
 return
end subroutine
!
! ==============
! C# interface =  protected internal int 
! 01.09.17
subroutine getAnnualDemand_Tree_AF(count, values)
   use lms_CitiWaterBudgets
    implicit none
    dll_export ::  getAnnualDemand_Tree_AF
    integer :: count,i
    integer, dimension(1:count) :: values
    ! 
    forall(i=1:count) values(i)= go_demand_TurfTree_AF(3,i)
 return
end subroutine
!
! ==============
! C# Interface = protected internal int[]get_NonpotableWaterTotal 
! 01.14.17
! ---------------------------------------------
subroutine getNonpotableTotalUsed_AF(count, values)
    use lms_Provider
    implicit none
    dll_export :: getNonpotableTotalUsed_AF
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = NINT(gvf_nonPotableTotalUsed(i))
 return
end subroutine
!
! ==============
! C# Interface = protected internal int[] 
! 01.14.17
! ---------------------------------------------
subroutine getResOutdoorGPCD_PU(count, values)
    use lms_LULC
    implicit none
    dll_export :: getResOutdoorGPCD_PU
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = NINT(gvf_gpcdLCLUinOut(13,1,2,i))
 return
end subroutine
!
! ==============
! C# Interface = protected internal int[] get_ResidentialLowDenIndoorGPCD
! 01.14.17
! ---------------------------------------------
subroutine getResIndoorGPCD_PU(count, values)
    use lms_LULC
    implicit none
    dll_export :: getResIndoorGPCD_PU
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = NINT(gvf_gpcdLCLUinOut(13,1,1,i))
 return
end subroutine
!
! ==============
! C# Interface = protected internal int[] 
! 01.14.17
! ---------------------------------------------
subroutine getResOutdoorGPCD_MD(count, values)
    use lms_LULC
    implicit none
    dll_export :: getResOutdoorGPCD_MD
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = NINT(gvf_gpcdLCLUinOut(8,1,2,i))
 return
end subroutine
!
! ==============
! C# Interface = protected internal int[] 
! 01.14.17
! ---------------------------------------------
subroutine getResIndoorGPCD_MD(count, values)
    use lms_LULC
    implicit none
    dll_export :: getResIndoorGPCD_MD
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = NINT(gvf_gpcdLCLUinOut(8,1,1,i))
 return
end subroutine
!
! ==============
! C# Interface = protected internal int[] 
! 01.14.17
! ---------------------------------------------
subroutine getResOutdoorGPCD_HD(count, values)
    use lms_LULC
    implicit none
    dll_export :: getResOutdoorGPCD_HD
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = NINT(gvf_gpcdLCLUinOut(2,1,2,i))
 return
end subroutine
!
! ==============
! C# Interface = protected internal int[] 
! 01.14.17
! ---------------------------------------------
subroutine getResIndoorGPCD_HD(count, values)
    use lms_LULC
    implicit none
    dll_export :: getResIndoorGPCD_HD
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = NINT(gvf_gpcdLCLUinOut(2,1,1,i))
 return
end subroutine
! ==============
! C# Interface = protected internal int[] 
! 04.12.17
! ---------------------------------------------
subroutine setRainFallFactor(value)
    use  lm_Initialize
    implicit none
    dll_export :: setRainFallFactor
    integer :: value

   gvf_rainfallfactor=value
 return
end subroutine
!
! --------------------------------
subroutine setGPCDEfficiency(value)
    use lms_LULC
    implicit none
    dll_export :: setGPCDEfficiency
    integer :: value
    !
    gvi_efficiencyLCLUres = value 
    !
 return
end subroutine
! ==============
! C# interface = protected internal int get_ResIndoorWaterUse
!
function getResIndoorUse() result(value)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getResIndoorUse
    integer :: value

    value = go_resIndoorUse
end function
!
! ==============
! C# interface = protected internal int get_ResOutdoorWaterUse
!
function getResOutDoorUse() result(value)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getResOutdoorUse
    integer :: value

    value = go_resOutdoorUse
end function
!
! --------------------------------
!
! ==============
! C# interface = protected internal int NOT SET YET
!
subroutine setRainHarvestCompliance(value)
    use lm_ParameterControl
    implicit none
    dll_export :: setRainHarvestCompliance
    integer :: value
    !
    gvf_RainGrayCompliance = value ! * 1/100
    !

 return
end subroutine
! ==============
! C# interface = protected internal int NOT SET YET
!
subroutine setInflectionPoint(value)
    use lm_ParameterControl
    implicit none
    dll_export :: setInflectionPoint
    integer :: value
    !
    gvf_yearsToAdoptRainWater = value 
    !

 return
end subroutine
! ===========================================================

! =============================================================================
! =============================================================================
! E.O.F. code

! ------------------------------------------------------------
function getAgRetirementPct() result(value)
    use lms_AgWaterBudgets
    implicit none
    dll_export :: getAgRetirementPct
    integer :: value
    real :: percentage
    !
    percentage= gvf_AgRetirementPct * 100.
    value = nint(percentage)
    !
end function
subroutine  setAgRetirementPct(value)
    use  lms_AgWaterBudgets
    implicit none
    dll_export :: setAgRetirementPct
    integer :: value
    !
     gvf_AgRetirementPct =0
     if(0 < value)gvf_AgRetirementPct = value * 0.01
    !
  return
end subroutine
!
! So, this is rainfall runoff after any rainwater harvested.
! Gross in the sense that it does not account for any
! Storm water captured, if such is the case. And, before any evaporation
! 12.10.18
subroutine getGrossStormWater_AF(count, values)
    use lms_CitiWaterBudgets
    implicit none
    dll_export :: getGrossStormWater_AF
    integer :: count,i
    integer, dimension(1:count) :: values

   forall(i=1:count) values(i) = go_GrossStormWater_AF(i)
 return
end subroutine

! ====================================================
! ==========================
! For parameterizing LCLU
! --------------------------
!subroutine set_ppH_M(value)
!    use lms_LULC
!    implicit none
!    dll_export :: set_ppH_M
!    integer :: value
!
!    gvf_ppH_M=value
! return
!end subroutine
!subroutine set_ppH_L(value)
!    use lms_LULC
!    implicit none
!    dll_export :: set_ppH_L
!    integer :: value
!
!    gvf_ppH_L=value
! return
!end subroutine
!subroutine set_provider(value)
!    use lms_LULC
!    implicit none
!    dll_export :: set_provider
!    integer :: value
!
!    gvi_provider=value
! return
!end subroutine
! ============================

! =========================================================================
! 
! Testing code for bringing data into the model from a database to initialize
! the model (June 2012) 
!
! C# interface = internal int [,] CAPinputs
!
!subroutine getCAPinputs(count,array,values)
!   use lms_Designations_B
!    implicit none
!    dll_export ::  getCAPinputs
!    integer :: count,array,i,j
!    integer, dimension(1:count,1:array) :: values
!    ! 
!    forall(i=1:count,j=1:array) values(i,j)=0
!    forall(i=1:count,j=1:array) values(i,j)=   gii_designationsCAP_acft(i,j)
!end subroutine
!subroutine setCAPinputs(count,array,values)
!   use lms_Designations_B
!    implicit none
!    dll_export ::  setCAPinputs
!    integer :: count,array,i,j
!    integer, dimension(1:count,1:array) :: values
!    ! 
!    forall(i=1:count,j=1:array)  gii_designationsCAP_acft(i,j)=0
!    forall(i=1:count,j=1:array)  gii_designationsCAP_acft(i,j)= values(i,j)
!end subroutine
! ===========================================================
!
! ===============
! C# interface = 
!
! --------------------------------------------------
! Generic subroutines
! ========================
!subroutine set(count, values)
!   use  lms_module
!    implicit none
!    dll_export ::  set
!    integer :: count,i
!    integer, dimension(1:count) :: values
!    real, dimension(1:count) :: generic
!    !
!    forall(i=1:count)generic(i)=0.0
!    forall(i=1:count, values(i) .gt. -1)
!      generic(i) = values(i)
!    end forall
!
!    forall(i=1:count) gvi_whatever(i)=generic(i)
!end subroutine

!subroutine get(count, values)
!   use lms_module
!    implicit none
!    dll_export ::  get
!    integer :: count,i
!    integer, dimension(1:count) :: values
!    ! 
!    forall(i=1:count) values(i)=   go_acft(i)
!end subroutine

!subroutine set(value)
!    use lm_module
!    implicit none
!    dll_export :: set
!    logical :: value
!
!    gv_variable = value
!return  
!end subroutine


!function get() result(value)
!    use lms_module
!    implicit none
!    dll_export :: get
!    integer :: value
!
!    value =go__acft
!end function

! End of functions and subroutines
! ==============================================================================================
! E.O.F. KernelInterface.f90