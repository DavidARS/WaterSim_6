!
!  File is Global.f90
!
!  Fhis file contains the global modules and defined types for WaterSim
!
! ---------------------------------------------------------------------------------------
!
!      WaterSimDCDC Regional Water Demand and Supply Model Version 6.0

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

! OUTPUTS: none
!

! Module:       Module gm_GlobalData
! Module:       Module gm_VersionControl
! Module:       Module gm_ModelControl
! Module:       Module gm_Exception

! Subroutines:  none
! Functions:    none

! created on 09.30.09
!
! david arthur sampson
! last write was: 01.15.13,07.21.14,07.22.14
! ------------------------------------------
!

! =========================================================
!
Module gm_GlobalData
    ! for opening text files
   character(len=30) :: gvc_Path='App_Data\WaterSim_6_0\'
   logical :: lexist
   integer :: LU,string
   character(len=25) :: module
   character(len=50) :: Infile
   character(200) :: errorString
   character(50) :: streamString
   ! Exception handeling
   integer :: gv_exceptionCode=0
   ! Fortran Mask
   logical :: gvl_mask(35)
   integer :: Ag_array_last=0
   ! used for defining array bounds
   integer,parameter :: gpi_laggedStartYr=1980
   integer, parameter :: gpi_lBY=2000
   integer, parameter :: gpi_uBY=2086
   ! Used for array declarations
   integer, parameter :: gvi_maxProV=35
    ! Used to conduct "do" loops, etc.
   integer, parameter :: gvi_Providers=35
   !
   ! Used for indexing (and assigning) arrays
   integer, parameter :: gpi_minPVY=2000
   integer, parameter :: gpi_maxPVY=2086
   ! Used in the Include file
   integer, parameter :: gpi_maxCOrec=1250
   integer, parameter :: gpi_maxSVTrec=700

    ! records SVT data
    ! For do loops
   integer, parameter :: gpi_dailySVTrec=24837 !24471
    ! For array defintion
   integer, parameter :: gpi_dailySVT=24837 !24471 !9150 was 366*25 years

    ! Order of when the demand or supply sources are called
    ! ------------------------------------------------------

   integer, parameter :: gpi_vadose=2
   integer, parameter :: gpi_directInject=3

   integer, parameter :: gpi_rainWaterHarvest=4
   integer, parameter :: gpi_stormWaterCapture=5
   integer, parameter :: gpi_grayWater=6

   integer, parameter :: gpi_reclaimed=7
   integer, parameter :: gpi_roReclaimed=8

   integer, parameter :: gpi_defPump=9
   integer, parameter :: gpi_classA=10
   integer, parameter :: gpi_ClassBC=11
   integer, parameter :: gpi_cap=12
   integer, parameter :: gpi_ncs=13
   integer, parameter :: gpi_newSupplies=14
   integer, parameter :: gpi_banking=15

   integer, parameter :: gpi_groundWater=16
   integer, parameter :: gpi_unmet=17
   integer, parameter :: gpi_unusedNonPotable=18
   !
   logical :: gvl_InOutDemandArray
   !
    
    ! GAP (stack) counter balance
    logical :: gpl_holdGD_one
    !
     ! Constants
   ! ----------------------------------------------------------
   real,parameter     :: gpf_daysMonth=30.416666667
   real,parameter     :: gpf_monthsPerYear=12
   real,parameter     :: gpf_ft3sToAcftDay=1.9834710458
   real,parameter     :: gpf_gallonsToliters=3.785411784
   real, parameter    :: gpf_sqftInOnesqm=10.763910417
   real,parameter     :: gpf_sqftInAcre=43560
   real,parameter     :: gpf_sqftTom2=0.09290304
   real(8), parameter :: gpd_acftTomaf=1e-6
   real(8), parameter :: gpd_galperacft=325851.43326
   real(8), parameter :: gpd_ft3sToacftmonth=60.33057764466317
   real(8), parameter :: gpd_m3Toacft=0.00081071318212
   real(8), parameter :: gpd_acftToft3=43560.000627
   !
   real,parameter     :: gpf_mmTometers=0.001
   real(8),parameter     :: gpf_cubicMetersToCubicmm=100000000
   real(8),parameter  :: gpf_cubicMetersToAF= 0.00081071318212
   !
   ! - Time-step of central model (1=annual, 12=monthly)
   integer, parameter :: gpi_timeStep=1
   integer, parameter :: gpi_timeStepWB=1
   !
   integer,parameter  :: gpi_LULC=13

   ! Default Time lag (in years) for vadose to aquifer
   integer, parameter :: gpi_Tlag=10
  ! -----------------------------------------------------------
   logical, parameter :: gpl_DemandDriven=.true.
   logical, parameter :: gpl_ClimateDriven=.false.
   logical, parameter :: gpl_SVT_SESDriven=.false. ! use simple exponential smoothing to reduce Climate factor on SVT 
   !

   ! These are Important
   ! -----------------------------------------------
   ! Start and end default 
   integer, parameter :: gpi_start=2000
   integer, parameter :: gpi_end=2086
   !
   integer, parameter :: gpi_startCF=12
   integer, parameter :: gpi_lclu=2010
   !  Don Gross CO RIver comparisons - ADWR
   ! Also loads and unloads the Logical Units 100 to 105
   ! Release or Debug

    ! ---------------------------------------
    logical :: gvl_writeToLog=.false.   
    ! ---------------------------------------
    ! End of 2014 - it is now 01.16.15 for use 2015
    ! as the start of projection data (in theory 
    ! we could have empirical data through 2014)
    integer, parameter :: gpi_projectionStartYear=2015  
    ! New Testing 2014 Summer
    ! -------------------------------------------
    !   Provider .txt file (Unit 4) writes out
    ! All allocations- source-on and off project for
    ! each provider
   ! logical, parameter :: gpl_writeProviderDotTxt=.false.
    ! ---------------------------------------------------
   ! Write text files as output default
    logical :: gvl_writeError=.false.

   ! Sensitivity Analyses 
   logical :: gpl_runSensitivity
   ! 
 ! Use this for testing units
!   logical, parameter :: gpl_comparisons=.false.
!   logical,parameter ::  gpl_verify=.false.
!   logical, parameter :: gpl_validate=.false.
!   logical, parameter :: gpl_scenarios=.false.
!   logical, parameter :: gpl_testing2016=.false.    
   !
   ! Write to logical unit one
  ! logical, parameter :: gpl_testOne=.true.
   ! for the log file
   ! each provider
    logical, parameter :: gpl_writeProviderDotTxt=.true.
    ! ---------------------------------------------------
   ! Use this for testing units
   logical, parameter :: gpl_comparisons=.false.
   logical,parameter ::  gpl_verify=.false.
   logical, parameter :: gpl_validate=.false.
   !
   logical, parameter :: gpl_testing2016=.false.    
   logical, parameter :: gpl_103=.true.
   !
   logical, parameter :: gpl_1=.true.
   logical, parameter :: gpl_writeLog=.true.
   logical, parameter :: gpl_release=.false.
End Module gm_GlobalData
! =========================================================

! Version Control of the dll
! -----------------------------------------------
Module gm_VersionControl
  ! These are version control for dll labeling
  integer :: num
  integer, parameter :: gdi_Model=6
  integer, parameter :: gdi_Vers=5
  integer, parameter :: gdi_SubVers=1
  !
  integer, parameter :: gdi_Month=05
  integer, parameter :: gdi_Day  =26
  integer, parameter :: gdi_Year =17
  integer, parameter :: gdi_Hour =14
  integer, parameter :: gdi_Min  =01
  !
End Module gm_VersionControl

! ===============================================

! ----------------------------------------------------------
!   Controls over run-time events. Keeps
! tract of time events
! ==========================================================
Module gm_ModelControl
 use gm_GlobalData
    type MyPolicy
      integer :: count
      !
      character(LEN=30) :: variable,a
      character (LEN=12):: outputs,b
      real :: increment,align
      real :: lowValue,highValue
      logical :: finished
      logical :: inSensitivity,sensitvityFinished
      logical :: openFile
    end type
    type MyScenario
      integer:: policy,scenario,LCLU
    end type
    !
    integer :: policy,scenario,LCLU
    real :: span
    !
    ! Simulation Controls
    !
    type runTime
     ! simyear is 0 to N (year number in simulation stream)
     integer :: year=0,month=0,simyear=0,policyYear=0,lcluYear=0
     integer :: jumpYear
     integer startyear,endyear
     integer simulations         
               ! Number of years to simulate (i.e. 25)
     integer :: days,gvi_holdrun,gvi_holdrun2
     real :: propYears,propYears_2060
     character(len=2), dimension(35) :: providers
     character(len=2), dimension(35) :: align

     logical :: atStartOfSimulation,atStartOfProviderLoop
     logical :: atEndOfProviderLoop
      type(MyPolicy) :: Policy
      type(MyScenario) :: Scenario

    end type
!     contains
!       function StartPolicy(this,num)
!         type(runTime) this
!          integer :: StartPolicy
!          integer :: num
!          StartPolicy = this%mypolicy(num)
!        end function
    
End Module gm_ModelControl
! ==========================================================
!
! Sensitivity analyses of internal variables code
! July 2016
! ==========================================================
Module gm_DataAndSensitivity
    !
    use gm_GlobalData
    
    ! New parameters that were heretofore embedded within individual modules
    ! Due to the sensitivity analyses, it was easiest to add them here
    ! 07.07.16
    ! ----------------------------------------------------------------
        !
        real :: gpf_increment
        !
        !       Agriculture.f90
    real :: gpf_AgEvap=0.793,gdf_AgEvap=0.793
    real :: gpf_AgToVadose=0.2, gdf_AgToVadose=0.2
        !
        !       SRP evaporation
    real :: gpf_newRate=0.05,gdf_newRate=0.05
    real :: gpf_srpSlope=0.028,gdf_srpSlope=0.028
    real :: gpf_srpIntercept=0.3,gdf_srpIntercept=0.3
        !
        !      WaterShed_CO.f90
    real :: gpf_panMead=2.28092,gdf_panMead=2.28092
    real :: gpf_panReach=4.5,gdf_panReach=4.5
    real :: gpf_panPowell=1.763776,gdf_panPowell=1.763776
    real :: gpf_bankStorage=1.5,gdf_bankStorage=1.5
    real :: gpf_bankStoragePowell=0.08,gdf_bankStoragePowell=0.08
    real :: gpf_bankStorageMead=0.065,gdf_bankStorageMead=0.065
        !
    !           Water_CityModel - Water Efficiencies
    real :: gpf_gallonsPerMinute=2.3,gdf_gallonsPerMinute=2.3
    real :: gpf_SWTPefficiency=0.97,gdf_SWTPefficiency=0.97
    real :: gpf_WSefficiency=0.92,gdf_WSefficiency=0.92
    real :: gpf_WWsourceEfficiency=0.92,gdf_WWsourceEfficiency=0.92
    real :: gpf_WWTPefficiency=0.97,gdf_WWTPefficiency=0.97
    real :: gpf_RWWTPefficiency=0.97,gdf_RWWTPefficiency=0.97
    real :: gpf_ROefficiency=0.95,gdf_ROefficiency=0.95
    real :: gpf_RecEfficiency=0.95,gdf_RecEfficiency=0.95
    !
    real :: gpf_RateComLeak(gvi_Providers)=0.05,gdf_RateComLeak(gvi_Providers)=0.05
    real :: gpf_RateIndLeak(gvi_Providers)=0.05,gdf_RateIndLeak(gvi_Providers)=0.05
    real :: gpf_RateResLeak(gvi_Providers)=0.05,gdf_RateResLeak(gvi_Providers)=0.05
        !
    real :: gpf_showersBathPCT=0.24,gdf_showersBathPCT=0.24
    real :: gpf_gallonsPerFlush=3.63,gdf_gallonsPerFlush=3.63
    real :: gpf_flushesPerDay=5.05,gdf_flushesPerDay=5.05
    real :: gpf_BWpercent=0.26696,gdf_BWpercent=0.26696
    real :: gpf_evapotrans(gvi_Providers)=0.793,gdf_evapotrans(gvi_Providers)=0.793
        !
        !       WaterShed_SaltTonto.f90
    real :: gpf_modLowerThreshold=0.91,gdf_modLowerThreshold=0.91
    real :: gpf_upperThreshold=0.96,gdf_upperThreshold=0.96
    real :: gpf_AgWaterUse=250000,gdf_AgWaterUse=250000
    real :: gpf_addFlowThreshold=0.25,gdf_addFlowThreshold=0.25
        !
        !       WaterShed_Verde.f90
    real :: gpf_propSaveVerdeStorage=40,gdf_propSaveVerdeStorage=40
    real :: gpf_threshStorage=0.5,gdf_threshStorage=0.5
    real :: gpf_ratioVerde=0.8,gdf_ratioVerde=0.8
        !
    ! ----------------------------------------------------------------
End Module gm_DataAndSensitivity
! ---------------------------------------------
!    Exception to tract run-time errors 
! (use in subroutines or functions)
! =============================================
Module gm_Exception
  type Exception
   integer :: Prov,gvi_hold
   integer :: TStep,hold
   !
   character(8) :: MySub
   character(10) :: MyFunction
   character(20) :: Mycall
   character(12) :: boundary_1
   character(10) :: align
  end type Exception
End Module gm_Exception
! =============================================

!
! ===================================================================
! E.O.F. Global.f90