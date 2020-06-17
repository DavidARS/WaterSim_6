!
! File is BuildNotes.f90
!
! This Infile serves as a repository for Build/ Version comments

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


! Module:       Module lm_BuildNotes
! Subroutines:  none
! Functions:    function BuildNumK() result(num)
!               function BuildVerK() result(num)
!               function MonthK() result(num)
!               function DayK() result(num)
!               function YearK() result(num)
!               function HourK() result(num)
!               function MinK() result(num)

! Created on 22 December 2009
!
!
! David Arthur Sampson
!
! Last Access was: embedded
!                  
! ===========================================================================================================
!
! Version 3.03 was the FORTRAN equivalent of the PowerSim Model
!
! Version 3.04:
!        1) Inclusion of the CitiWaterBudgets file.
!
!        2) Rainwater read input (bin\debug\data\prainfall.txt) - currently a place holder until I receive
!             the provider-level rainfall estimates for the valley (from Katharine Hayhoe- Texas Tech).
!
!        3) A correction on the Mead elevation corresponding to Mid-Elevation release Tier. I spoke with 
!             Dr. Fulp (USBR).  Volume on mead is 5.8 maf for elevation of 1025 feet to be used in the 
!             shortage sharing code, or i.e., subroutine threshold_b
!             T,lv_BaEqualization,lvd_Pp,lvd_balanceamount,vSsr,vTier)
!
! Version 4.05: 
!        1) Changed the shortage sharing on the Colorado River to reflect actual shortage agreements
!             (i.e., see line 885 in WaterShed_B.f90)
!
!        2) Changed the CAP deliveries to represent all CAP delivered along the aqueduct and NOT the
!             75% of CAP as previously modeled.
!
!        3) Changed the estimate of "on-river".  Tim's original code was not correct. On-river is now 
!             "lv_onRiver=min(wsb%lv_AZshareCO,cv_onRiver-(0.1*wsb%lv_AZshortageCO))" while Pre-cap
!             (before evaporation, is "vPreCap=max(wsb%lv_AZshareCO-lv_onRiver,0)".  NOTE: CAP takes 90% 
!             of the shortage (I ascertained this at an AHS meeting in June, from a CAP contact [Dee Fuerst].
!
!        4) I went back to the original Kent Decree, Table 10, (please see the manuscript draft for the
!             reference)to extract the number of miners inches per acre that each provider gets.  Converting
!             this to Acre Feet provided new (correct?) estimates of class A designations based 
!             on river flow.
!
!        5) I changed the acreage used to calculate class B and C in the model. Heretofore we were using 
!             total acreage (for member lands).  We now use only B and C acreage.  I contacted Tim, and 
!             he is in agreement with this.  I had a meeting with Yvonne Reinink and Tim Skarupa 
!             (19 May 2010) who confirmed this.
!
!        6) Changed SRP release to represent estimates of release based on Class A designations for the 
!             flow of that year.
!
!        7) Added the NCS (new conservation space) allocations to the WaterShed_A.f90 file.  i.e., this 
!             represents water above the "old" dam height up to the new dam height.  This variable is called 
!             "wsa%gv_allocateNCS(i)" or "go_NCS_acft(i)"
!
!        8) Added pumping restrictions so that SRP cannot pump more than the maximum of 375000 acft.  The 
!             variable is called "cv_maxPumping". Pumping is "vDa%lv_SRPpumping_maf(T%year,k)" sent to 
!             the interface as "go_SRPpumping_acft(i)"
!
!        9) Changed the structure of the model a bit; I separated the Provider.f90 file to pull out
!             water demand and population, and the designations for SRP and for CO.  This was 
!             necessary so that SRP release could be modeled, and to clean up the code.  This
!             resulted in the added files: ProviderPopulationandDemand.f90, Designations.f90,and
!             Designations_B.f90.
!
!        10) Water demand was re-evaluated based on comments by Pat; I used new population growth projections
!             from MAG (2005 to 2030) and the 2006 GPCD estimate (from the xls spread sheet) to estimate
!             water demand. AS OF 06.15.10.
!
!        11) I changed the way available water is distributed on the CO when available water is less than normal
!             allocation such that the next time step results in a dead pool level for Mead.  i.e., before
!             I divided the difference between dead pool and available among NV, MX, and AZ.  The code now uses
!             the standard formulation (as when available water exceeds normal allocation) see line 994 in 
!             WaterShed_B.f90 for more specifics.
!
!        12) As of 06.16.10 we have new (up-dated) estimates of groundwater designations (DAWS) and of 
!             certificates for most of our modeled water providers.  We are still working on the few remaining.
!
! Version 4.06: 
!        1) TDS model has been included (continued testing to occur-06.25.10)
!
!        2) Increased storage
!             a) Bartlett Reservoir was increased from 178,186 to to 178,490 (Bureau of Rec data;
!               http://www.usbr.gov/projects/Facility.jsp?fac_Name=Bartlett+Dam
!
!             b) Horseshoe was raised from 109,271 to 131,500 (same reference, except for Horseshoe)
!
!             c) Roosevelt: was 1,653,043 ac-ft, raised to 1,654,080 following the inclusion of NCS (272,500)
!
! Version 4.07
!        1) Remove the use of gm_GlobalOpenFiles; files are now opened in the lm_Initialize module.  This was
!             necessary so that we could use the genericPath String to set the operating path
!
!        2) Added the BuildVerK function to create a combined BuildNum Ver variable that mimics a float; i.e.
!             so I can pass 4.07 to the interface
!
!        3) Changed vList(j) in Designations_SVT to an interger, and changed zero to -1.  Removed 
!             ThresholdDifference(j)
!   
!        4) Changed the  "if(lid_thresholddesigSVT(j) < flow)then" to  "if(lid_thresholddesigSVT(j-1) < flow)then" 
!             line 186-Designations_SVT.f90 (correction to the code)
!               
!        5) Added acreage for class B&C to include class A,B, and C as directed by David C Roberts (SRP) in 
!             August 2010 meeting (THIS SEEMS TO BE in conflict with other SRP employees) CHECK THIS AGAIN!
!
!        6) Added NCS interface variables to the model. They are:  "go_NCS_acft(j)" and "go_NCSmax_acft(j)".
!             NCS was then added to lv_Designations in Provider.f90.  So, the difference between supply and 
!             demand includes NCS water. 
!
!        7) The variable "go_SRPpumping_acft(i)" was removed.  Using, instead, "go_classBCdesignations(j)" and
!             "go_classBCmax(j)", both in acft yr-1.
!
! Version 4.08
!      11.03.10
!        1) I added an approach to model a reduction in GPCD using a simple exponential smoothing (SES) algorithm
!             found in subroutines.f90.  The variable - gvf_AlterGPCD_pct=50 - is currently a "fixed" value and must
!             be added to the interface.
!
!        2) I added population estimates for the year 2000 to 2100 (MAG 2007 estimates, reduced by 5% for 2010)
!
!        3) The year 2000 estimates came from www.azcommerce.com/doclib/commune/city.pdf i.e. peoria.pdf
! 
!        4) Together, #22 and #23 give an estimate of water demand, found in Provider.f90 [subroutine  Difference_(T,vGP) ]
!
!        5) I added an approach to model a reduction in outdoor water use (proportion) using the SES algorithm
!             (Brown's).  subroutine is sSES in Water_CityModel.f90
!
! Version 4.09
!      11.16.10
!        1) Added a 25-year trace for climate data and a variable trace.  This is currently hardwired as "lvl_trace25".  When
!             true, 25-years of the historical record are used,  When false, a variable record is used, depending on the index year
!
!        2) I changed the Powell-Mead equalizations as per another look at the Record of Decision dated December 2007.  Specifically,
!             I added the tabular data for 2008 through 2026 as found in the table on page 51.  And, I re-wrote the if blocks for the
!             Tier determinations.  I have kept the original code so that older simulations using the previous balancing method can
!             be examined.  The logical variable is labeled "lvl_RecordOfDecision".  When "true" the modified (new) approach is used.
!             When "false" the code prior to this implimentation is used.  This includes a different approach to estimating the balance
!             amount.  It is my opinion that it (the previous approach) was wrong.
!
!      11.17.10
!        3) Changed the precipitation read input file (Water_CityModel.f90) to include a logical varible (lvl_NCEPmetData) that, when true,
!             reads the formatted meteorological data inputs from the four models/ two emission scenarios used in the NCEP
!             project (one at a time- files need to be copied over into the generic file name 'App_Data\Data\Metdata.txt.'
!
!      12.07.10
!        4) I added a variable to alter the way the min(max()) function for Tier 1 and Tier 3 were operating on balancing Powell and 
!             Mead. I added in 1/2 the flow for the next year to predicted Powell
!             From: 1)  lvd_balanceamount=max(0.,(abs((lvd_Pp-lvd_Pm))/2.)  
!             To:   2)  lvd_balanceamount=max(0.,(abs((lvd_Pp+ (lv_flow_B_maf(T%year+1)/2.))-lvd_Pm))/2.) 
!             This insures that a max of 9.0 maf is achieved when mead drops below 11.8 maf if the balance amount exceeds 9.0 maf
!             This creates a more realistic release when Mead Drops
!
!      01.03.11
!        5) I added subroutine setReductionGPCDPct(value) and subroutine getReductionGPCDPct(value) (in KernelInterface.f90) 
!             in order to control, from the
!             interface, the reduction in GPCD expected by the year 2100 (using SES algorithm of Browns') see above.
!
! Version 4.1
!      06.27.11
!        1) New KernalInterface parameters (and outputs) for the CityModel parameters - FOR the API release scheduled for 1 July, 2011
!
! Version 5.0 
!      07.12.11
!        1) This Version is now a water demand model.  Outlined in Kernal.for, the order of water source use can be changed.
!        2) The model has water sources from Reclaimed water, RO reclaimed water, and water banking.
!        3) The City-model was restructured, somewhat.  Examine the figure for details
!        4) I am now running the model annually, not monthly.
!        5) The user can now pick from one of three river flow files
!        6) I have added a 4th ProviderDemandOption- SES but you can also change indivdual GPCD's for each provider
!
!      07.28.11
!        7) Fixed the estimate of gwater used (gvf_usedGW(i)) when the aquifer level falls below the designation
!             see line 419 in Groundwater.f90
!
!        8) adjusted lvf_otherWater(i) (see line 438 in Provider.f90) to "fix" GPCD calculations [i.e. DO NOT account for direct Injection
!             and surface water to vadose in the GPCD calculations
!
!        9) I BELIEVE the class A designations have been underestimated.  I went back to the original SAS code and saw that the total
!             providers acreage did not add up to the totals for the Valley for the division of Miner's Inches.  The acreage was off
!             by 27.1%.  See "ReadRawTrottAcres_calculateRights.sas" file
!             and see line 71 in Designations_SVT.f90 - NOTE: the adjustment factor was modified to correct rounding errors, 
!             creating 820,345 AF year-1 at threshold flow (> 1.063507 maf year-1)
!             NOTE: I will adjust the raw input files as soon as I verify and validate this ...
!
!        10) I HAD to change how I was partitioning class A water among on and off-project
!             see line 366 in SRPrelease.f90
!
!        11) I added a Class A water not used- see line 356 in SRPrelease.f90 and line 441 in Provider.f90
!        12) I had missed the Reclaimed water to Vadose flux in the calculation of fluxes to the Vadose
!             - added see line 1309 in Water_CityModel.f90
!
!        13) On 02 August I went back to the Kent Decree to look at the tables for class A lands.  I determined that the total
!             acrege in the file was off by 2333 which was indian lands.  I subtracted that from the total to derive a ratio 
!             to correct, (as I must see it- I cannot replicate the individual water provider acreage based on the decree), the
!             acrege.  See the SAS file listed above. ~ 26 % off
!
!      08.11.11-test Version 08.11.14.00
!        14) I added logical checks to the remaining file open statements (so that all data files have a logical check of presence)
!        15) I made a default value for the ReduceGPCDpct; set at 1% test Version is 08.11.15.30
!
!      08.26.11
!        15) Added code to estimate storage release from the SVT reservoirs -still testing
!
! Version 5.1: 12.20.11
!        1) Added On and off project demand for output to the interface ( go_OnProjectDemand(j) +  go_OffProjectDemand(j) ) 
!        2) Changed the calculation of differencepop(T%year,i) for the start year [this was removed on 08.14.12]
!        3) Added an input from the interface for provider population. Still working out the specifics of
!             this implementation (gii_ProviderPopulation(i)) : assigned to a local variable labeled
!             lvf_ProviderPopFromInterface(T%year,i) [This was removed om 08.14.12]
!
!        4) Added CAP water aquired from alternate sources.  This is then CAP water above and beyond the M&I contracts. 
!             This water comes from Tier III and Tier IV CAP water, if available.  I also removed the heretofore "over-estimate"
!             of CAP II water (i.e., 18,145 that was estimated for Tier I and II above and beyond the subcontracts for
!             the 33 water providers.  
!
! Version 5.2: 
!      12.29.11
!        1) Changed the array lid_thresholddesigSVT(42) to lid_thresholddesigSVT(42,2); has threshold flows but also ratio of flow used
!             by the 10 water providers (i.e., the proportion of total flow used by the sum of all water providers (from table 10; Kent Decree
!             in combination with data that Mike must have provided [provider level acreage])
!
!        2) Changed the input files for the SRP designations of class A based on a re-analysis of the Kent Decree.  Still need to account
!             for acreage differences between the sum of the 10 and that shown in Table 10 of the decree.  As is, I am underestimating
!             release- need to account for the remaining acres..... using mikes data for individual providers, and Table 10 from the Kent
!             decree, the 10 do not add up to the total acreage in the table. Also, Ray still thinks normal flow is incorrect but my reading 
!             of the decree refutes his assumptions UNLESS class A includes, somehow, 3 AF per acre for storage water.... I do not see how,
!             but that is the only way it could be off (in my assessment).  The decree, however, does not state that.. The only way this could be
!             is as if today's use of the term Class A rights for total water (normal flow and storage) is somehow incorporated within the class 
!             A of the decree... UNLESS we can only use class A for on-project.....[this code is no longer considered adequate-01.20.12 (now obsolete]
!                   
!        3) I added the code " if(gvi_kernel_case < 1)gvi_kernel_case=1" to the RunOneYearKernel() subroutine.  This parameter is in the include file.
!             it lets the user change the order of water use.  To this end I also, then, added a set statement in KernelInterface.f90 to affect 
!             this parameter.( subroutine setKernelCase(value)).  At present, a value of 2 places CAP water ahead of class ABC from storage (1 is default)
!
!      01.03.12
!        4) Added lv_State_A_maf() for the year 2000 in initialState_a(). 
!        5) CHANGED (lid_acreageSVT(k,1)+lid_acreageSVT(k,2)) TO (gvd_acreageSVT(k,1)+gvd_acreageSVT(k,2)) IN 
!             subroutine aDesignations_classBC_A(T,acfta,ltDa,Aout) (~line 381) in Designations_SVT.f90 (lid_acreageSVT is the input variable-Unit 42)
!
!        6) Removed class A acres [gvd_acreageSVT(k,1)] from the equation (in the line above).  Cannot add both because some of B&C is classified as A, 
!             but we don't know how much. In addition, have more that ~ 116 k acres which equates to ~ 350 k pumping maximum pumping.
!
!        7) Changed (~line 465 in Designations_SVT.f90) lvf_proportion(j)=Aout%gvf_classBCmax(j)/lvf_sumMax TO lvf_proportion(j)=lvf_acreageProportion(j)in 
!             subroutine aDesignations_classBC_Storage(T,Aout) where:
!              lvf_sumAcreage=0
!               do j = 1,10,1
!               lvf_sumAcreage=lvf_sumAcreage+gvd_acreageSVT(j,2)
!                end do
!                do j = 1,10,1
!               lvf_acreageProportion(j)=gvd_acreageSVT(j,2)/lvf_sumAcreage
!               end do
!               i.e., proportional pumping based on a providers acreage relative to total acres
!
!      01.09.12
!        8) Added indian water type varables for use later (in subroutine aPartition_B)
!             (vDb%lvf_indianII(T%year)= lv_CAPindianTiers_acft(2,1)+lv_CAPindianTiers_acft(2,2)
!             (vDb%lvf_indianIII(T%year)= lv_CAPindianTiers_acft(3,1)+lv_CAPindianTiers_acft(3,2)
! Version 5.3
!        1) added  open(19,FILE=trim(trim(lvc_dataPath)//'App_Data\Data\SVT_doy.txt'),status="old")
!        2) added the fortran file DailyDesignations.f90 to conduct daily SVT provider designations
! 
!      01.18.12
!        3) added a function and variable "fModifyNormalFlow(gvf_modifyNormalFlow)" to modify the 
!             SVT normal flow designations from the trott table (as it is read into an array)
!             to adjust the actual deliveries acre-feet per acre (user reduction from the 1910 statute)
!             gvf_modifyNormalFlow = 1 as default
!             found in the subroutine modifyDesignations()(located in Parameter_control.f90)
!
!        4) added "subroutine setModifyNormalFlow(value)" in the KernelInterface to
!             import the value from the C# interface (read as an integer, divided by 100- proportional basis) 
!
!        5) added the equation to handle changes in modifyNormalFlow to the Parameter_control.f90 file
!
!      01.27.12
!        6) added a function to adjust daily flows on the SVT for the Trott table daily flow designation
!             estimate:  function fmodFlowSVTdaily() found in DailyDesignations.f90
!
!        7) I had to add if statements to modify the drought only when the drought period extends. This
!             was done in the function used (i.e., fmodFlowSVTdaily(T) )
!      02.01.12
!        8) I added a line in Groundwater.f90, line 444; if they have no designation, and the aquifer is
!             >= to their demand, they pump their demand (three water providers have no designation)
!
!        9) as of today, a groundwater designation is their sum total in their aquifer.
!
!        10) Also as of today, I changed (up-dated) populatios with Ray's and Sally's help
!
!      02.02.12
!        11) addded a variable to the kernel interface-gvi_ProviderGroundWater_a(i)- that accepts estimates of
!             initial groundwater, by provider, from the interface. Read in WaterShed.f90, set in Water_CityModel.f90
!             in the aquifer subroutine. This is MODFLOW groundwater passed into the model.
! Version 5.4
!      02.06.12
!        1) Removed the variable providerArea_2006 and used that slot to input providerAcres_2012 which 
!             contains "on" and "off" project acres calculated by Ray on 02.03.12 and housed in the file
!             Provider SRP and OFFProject Acres 2 3 2012.xlsx (ProviderPopulationandDemand.f90)
!             Note: from the original data, Anthem was places as Other Provider and Apache Junction was deleted
!
!        2) altered the  calculation of gvd_classABCacreRatio (ProviderPopulationandDemand.f90) to account
!             for "on" and "off" project using the new data (i.e., "on" acres/ "total" acres). 
!
!        3) Changed the subroutine "ProviderArea2006" to "ProviderArea2012" and changed the formally embedded
!             values to an estimate based on acerage (ProviderPopulationandDemand.f90).
!
!      02.07.12
!        4) Deleted the subroutine   "aDesignations_classA_A(T,flow,gvd_ClassAarray)"
!
!        5) Deleted the subroutine "sSRP_classBandC"
!
!        6) Combined the sSRP_classB&Cstorage subroutine to represent water released from the reservoir plus water
!           pumped by SRP
!
!        7) Changed water use for class A from total demand to "on project" - NOW Ray states on project use only for A & B lands
!           ( subroutine classA(T,gvi_order,Aout) in SRPrelease.f90)
!
!        8) for NCS, changed  lvf_WaterDemand35(i)=gvf_WaterDemand_acft(i,gvi_order,3)*gpd_acftTomaf TO
!            gvf_WaterDemand_acft(i,gvi_order,1)*gpd_acftTomaf 

!      02.08.12
!        9) Changed classB&C a)  Aout%gvf_classBCfromReservoir_acft(i)subtracts calculates the actual release based on demand

!      02.09.12, 02.10.12
!       10) Cleaned up all the files to make them consistent in their look.  Did a bunch of variable checking- outputs
!           making it to the C# interface.
!
!       11) Cleaned up the Include block
!   
!      02.17.12
! Version 5.5
!        1) Changed over to the new population estimates.  Now using providerPopulation_2100.txt
!           Please see TimeSeriesForecastPopulation_17Jan2012.sas and JoinProviderPopulations_2012.sas
!           for complete information. 

!      02.20.12
!        2) Changed the runoff calculations so that we always use the 2000 through 2010 estimates of runoff
!           in the simulation.  Any "added" runoff is concatenated to the file. 
!
!      02.22.12
!        3) Added a correction factor to the SES for GPCD reduction - so, reduction is % reduction by 2085 over
!           2010 estimates (gvf_AlterGPCD_pct)
!      03.01.12
!        4) Fixed the correction factor (was not working properly) see alpha_SES.sas and
!           GlobalSubroutines.f90 and subroutine SESgpcdRunning()
!         
!      03.05.12
!        5) Added a method of taking Ag water from pumping to add to a water providers
!           groundwater source.  Set in the Interface as subroutine setWaterFromAgPumping(count, values)
!           and subroutine getWaterFromAgPumping(count, values);  Variable name is gvi_WaterFromAgPumping_acft_a(i)
!
!        6) Added the designations for groundwater back into the model.
!
!        7) Added a unit to read data (unit 36) found in Designations_CO.f90.  This file reads the surface water
!           "alloted" to the water providers via the AWS documents (Table ).  We are using these data to calculate
!           the 80% threshold cut-off when providers may pump unlimited groundwater. May no tbe used.....
!
!       03.14.12
!        8) "fixed" normal flow outputs, and verified.
! 
!        9) Added Interface variables for water from the environmnet from colorado river water and from salt and from
!        verde. These have yet to be assigned in the model proper.
!
!       03.21.12
!       10) Major changes to SRP release- normal flow is not captured by the reservoir- passes through.  New code, and
!           new estimates of Spillage (spill over the dam).
!
!       11) Addded an output variable for the Interface - go_spillageSVT - as function getSVTspillage() 
!
!       03.30.12
!       12) Drastically changed my estimate of release from Powell. See line 844 in WaterShed_CO.f90
!
!       04.02.12
!       13) Added powell and mead elevation algorithms...functions in interface- getMeadElevation() and getPowellElevation()
!           from storage--- SAS analysis embedded in the subroutine (fit statistics)            
!            call  elevationsPowell(vState_Ba_maf(T%year),lvf_powellElevation)
!            call  elevationsMead(vState_Bb_maf(T%year),lvf_meadElevation)
!
!       04.05.12
!       14) Changed go_deliveriesCO to be CAP MandI-  I and II sent to the interface

!       04.09.12
!       15) I added on-project proportion of population (2000 to 2030) based on data from Sally;
!         data are read into unit 53 (onProject_2030.txt), with the global variable "gvd_onProjectPop(T%year,lvi_countSRP)" used
!         to pass the info to subroutine calcDemand(T) in ProviderPopulationandDemand.f90.
!      
!       04.16.12
!       16) Fixed the drought factor parameter for the SVT daily flow records.  Heretofore the drought response kicked in
!           immediately and did not turn off.
!     
!       04.26.12
!       17) Added a column to gvi_designationsGW(i) so it is now gvi_designationsGW(i,2) where the first column holds the provider
!           groundwater designation and the second column holds the groundwater credit; we have changed the model over to use
!           estimates of credits and NOT estimates of aquifer groundwater: slight conceptual difference
!
!       04.30.12
!       18) Changed-updated the GPCD SES algorithms (to clean up the code and create a different baseline)
!
!       05.07.12
!       19) Completely reformulated the approach to estimating the lag in vadose water to the aquifer. The lag
!           is now controled by a three-dimensional array called gvf_lagToVadose(T%year,provider,variable)
!           where "variable" is the specific flux being "impounded on the surface.  The codes for these
!           variables may be found in the    subroutine sLagVadose(T,...) and in the parameter xlsx file
!
!       20) Added the new groundwater credit model and the regional groundwater model as per Figure Two found
!           in the parameter xlsx file.
!
!       05.17.12
!        21) Fixed  lvf_GPCDout(provider,k) - I was declaring it in the common block but also locally.  Now,
!             I pass out  lvf_GPCDout (no array) but keep tract of GPCD using  gvf_GPCDout(provider,k) and
!             I have to do this in providerPopulationandDemand.f90. 
!
!       05.22.12
!        22) Changed the method to calculate relative surfacewater in subroutine "sEightyPercent."  This override
!           factor enables pumping water demand when surface waters are reduced by 20% or more.
!          see code for details
!
!   Version 5.6
!       05.23.12 and 05.25.12
!        1) I changed the way I calculate on- and off-project demand.  Now using on-project population estimates
!           as the ratio with total population.  Off-project is total minus on-project. So, I now read in
!           populations for on- and off-project (Population_On_2085.txt;Population_Off_2085.txt ) through 
!           a three-D array labeled "gvf_population(T%year,i,j)", where i is provider and j is use: 1=on-project, 
!           2=Off-project, and 3=total population (read, still, from the original pop file "providerPopulation_2085.txt"
!
!           NOTE: correction- no longer using off-project population, but using "other" population.
!
!       05.30.12
!        2) I re-formulated population to be not changes in population read in as input from file, but changes in 
!            growth (both on-project and what we are now calling "other" using  lvd_GrowthRateAdjPop(T%year,i) as
!            formulated before we switched to on-project and other population.
!
!       05.31.12
!        3) Fixed an over accounting of SRP groundwater pumped in  subroutine TypeTransferIN(T,i,vTstep,vPV,lvWB). This
!           was causing the squiggly Effluent production (raw Waste water production)
!         
!       06.18.12
!        4) NOTE: I am now using the logical parameter  "gpl_comparisons" to examine Colorado River outputs in relation to
!           reservoir levels simulated by CRSS and CROSS (Bureau of Reclamation model and the open source Simulation
!           model.  Used in conjunction with the Flow data option #3 (scenario file)
!   
!       06.19.12
!        5) COMPLETELY rewrote the release algorithms for the Colorado River.  I greatly
!           simplified, using crss and cross estimates for the 1915 trace to verify my
!           ballpark storage estimates. See function fRelease(tier,low,mid,high,target,balance)
!
!       07.02.12
!        6) Added code and changed code in Water_CityModel.f90- I added a separate subroutine for Commercial use
!           of water, so now there is residential, commercial, and industrial. I also then split up demand
!           to match this change.  I also added   1) gvf_RateResLeak(i), 2)gvf_RateComLeak(i), and 3)gvf_RateIndLeak(i)
!           to enable variable leakage.  And, gvf_parm_OutDoorResProp(i), gvf_parm_OutDoorComProp(i), and  
!           gvf_parm_OutDoorIndProp(i) to enable variable outdoor water use among the three.
!
!       07.05.12
!        7) Moved the call to the subroutine sEightyPercent( ) into the Groundwater.f90 file under the call
!           to subroutine pProviderGroundwater(T,gvi_order,vPV).  This removed the lag in the eightypercent of runoff 
!           calculation (i.e., when surface water drops below 80% of demand to meet obligations).
!
!        8) Change the SRP variable used to estimate available SRP for the above subroutine. Now use
!           the estimate of storage, and pumping, and class A deliverable to estimate maximum available
!
!        9) Changed the Mask for CAP based on Ray's CAP designation file (reduced the number of providers that have CAP)
!
!        10) Added the variable gvl_parmAllStrawsSucking which serves as an override to the groundwater pumping limit. When 
!           set to true providers can pump demand regardless of their annual credit bucket.
!
!       07.06.12
!        11) I added the call to  subroutine sEightyPercent() to Groundwater.f90 within subroutine pProviderGroundwater(T,gvi_order,vPV)
!            so there is no more lag in the determination of a trigger for the groundwater pumping override.
!            Also, I define the "potential" SPR water as class B and C for 3 acre-feet per acre and I placed the global variable
!            in Designations_SVT.f90 in subroutine aDesignations_classBC_Storage(T,Aout,acfta)
!
!        12) I put in an override variable in subroutine pProviderGroundwater(T,gvi_order,vPV) so that even if 80% of the expected "surface"
!            water is met, if SRP demands are met then the override is not invoked. i.e.,
!                            if(0 < gvf_WaterDemand_acft(i,9,3))then
!                            else
!                                lvf_usedSRP=lvf_availSRP
!                            endif
!   Version 5.7
!       07.07.12
!        1) 
!
!       08.06.12
!        2) Added a linear reduction in the climate factor- using a combination of functions and one subroutine
!           found in Module lms_ClimateFactor (Functions.f90).  This necessitated the inclusion of a factor to control
!           the timing of the reduction, or function getClimateFactorEndYear() and subroutine set* . As of today both the
!           Colorado WaterShed and the Salt-Verde-Tonto WaterSheds have their own fClimateFactor
!
!        3) Added Population Other as gi_ProviderPopulationOther(gvi_maxProV) and changed  gi_ProviderPopulation(gvi_maxProV) to
!            gi_ProviderPopulationOn(gvi_maxProV) so that we can input On-project and Other from the interface
!           These are used directly without calculating the popfluxdifference(i)
!
!       08.08.12
!        4) I added outputs to the interface for the regional groundwater accounting.  These include
!               a)   gof_regionalGW - which is the balance of the regions physical water
!               b)   gof_Outflow- The outflow from the region(i.e., the Gila and stanfield outflows)
!               c)   gvf_pumpingGWTotal- This pumping includes ag pumping and "other pumping) as listed
!                    in the ADWR modeling report # 22, page 15 - REMOVED on 08.22.12.  Added gof_AgAndOtherPumped to take its place
!       08.09.12
!        4) I added a bunch of variables to the Interface to handle the Ajay&Johnston project needs to variable control.
!           These include:  1) subroutine setRateResLeakage(mydefault,count, values)
!                           2) subroutine setWaterToEvironmentSalt(value)
!                           3) subroutine setWaterToEvironmentVerde(value)
!                           4) subroutine setWaterToEvironmentCO(value)
!                           5) subroutine setWaterToAgricultureAF(value)
!                           6) subroutine setWaterCommercialTurf(value)
!                           7) subroutine setParmOtherIndoorPct(count, values)
!                           8) subroutine setParmBlackWaterPct(mydefault,count, values)
!
!      08.10.12
!        5) I created a new subroutine that calculates the exact amount of added demand for Water to Agriculture so that
!           I can take only this much from Reclaimed Water. This is sCalculateAddedDemand(T)
!           This is a test example of shunting Waste Water to the Reclaimed WWTP to make up for lost surface/groundwater 
!           to meet the added demand from Agriculture- Based on the Ajay&Johnston project.  This could be expanded.
!
!      08.13.12
!        6) I created a function to estimate outdoor water use based on an assumed flushes per capita per day. This is found
!           in ProviderPopulationandDemand.f90.  May want an override switch so that inputs from the interface are instead
!           used
!
!      08.22.12
!        7) Changed gvf_incidentalCredit to remove direct injection. Incidental is now the Res,Com, and Industrial vadose fluxes
!           and surface water to vadose fluxes.
!
!        8) I added gof_AgAndOtherPumped as an output to the interface.  Defined in   subroutine sAquifer(T,lvWB,i,vTstep) of Water_CityModel.f90
!           to respresent agriculture and other pumping from the regional groundwater aquifer (see below)
!
!        9) I have added to the KernelInterface these subroutines and functions
!           a) subroutine getRegionalRecharge(count, values)
!           b) function getNaturalRecharge() result(value)
!           c) function getRegionalInflow() result(value)
!           d) function getAgAndOtherPumping() result(value)
!           e) function getRegionalGroundwater() result(value)
!
!       08.24.12
!        10) Removed surface water to vadose from Incidental recharge and, thus, incidental credits for
!           Annual pumping credits.
!
!       08.29.12
!        11) Completely re-did On-project and Other populations and renamed the input files.
!
!       08.31.12
!        12)14:12 - providerGPCD_2008.txt now read in to replaced the hard-coded GPCD estimates found
!           in globalsubroutines... the five year running average GPCD for each water provider. Data from ADWR.
!       
!        14) 17:33 Created an interface variable for outputting the Eighty Percent Trigger for groundwater override.. i.e.
!        gvl_removeGWRestrictions(i) is the logical created within WaterSim. I send to the interface the variable
!        this variable using "subroutine getEightyPctRule(count, values)"
!
!        15) 17:42 I added the +1 to T%year on this line:  SESgpcdRunning=lvf_GPCDaverage(provider,5)*(1.-(T%year+1-lpf_refYear)/100.)
!           in global subroutines so that even in the reference year the GPCD declines as intended.
!
!       09.05.12
!         version: 09.05.12_16:38:00
!        16 MAJOR change.   go_annualDeliveriesCO(k) changed from legal rights to actual deliveries. This should have been
!           caught earlier..... I fu@ked up. Holdover from earlier versions of WaterSim
!
!       01.03.13
!         version 01.03.13_
!         17) Added actual estimates of inflow on the Colorado between Mead and Powell- ADWR data (Don Gross)           
!             these data are added to the Lees Ferry flow data.
!
!         18) Changed the way I read the imput files; using a subroutine to read all calls now
!
!         19) Changed several of the unit numbers- see ParameterControl.f90 for details.
!
!         20) Reading in daily data from the Salt and Tonto, and the Verde Rivers.
!
!         21) Added actual estimates of Upper Basin Delivery estimates from Don Gross (ADWR)
!             The user can choose one of three methods using setUpperBasinDeliveriesIndex(value)
!             : See the C# interface code for details (look for:  internal int set_upperBasinData)
!           
!        01.15.13
!       version 
!
!         22) New InitK() subroutine: removed the need for modules. Calling directly to new subroutines
!             that reside just below individual modules (for each lm_*)
!        01.21.13
!         23) New kernel structure case(1) new SRP code, case(2) old code
!        01.22.12
!         24) Restructured the WaterSimDCDC.txt INCLUDE file.
!         25) Leaving out the county-scale code. So, I removed the data associated with it from the data directory.
!             I also removed the parameter file for the county-scale calls.
!         26) I removed the SVT annual data sets from the data directory (no longer used)
! 
!        01.30.13
!         27) I moved Water for the Environment for Salt-Tonto-Verde to WaterShed.f90 into a subroutine
!             I called  sFlowForEnvironment(T)
!         28) I added an update state subroutine for the subroutine sSRPrelease, where I also evaluate storage
!             levels to calculate acre-feet per acre-1 for SRP
!         29) Same for  lms_SRPnewRelease (I added the global variable "gvd_acftperAcreSVT") that is used in the
!             BandC storage subroutine.
!         30) TO WaterShed.f90 I added an if loop that uses the cfs data and NOT the acre-feet data for the combined
!             flows for the Salt-Tonto-Verde. Code, prior to December 2012, was incorrectly using the cfs data
!             and not the AF per day data summed for the year. i.e., DailyDesignations.f90.  This FORCED
!             code is in subroutine sFlowForEnvironment(T)
!         31) We have code that controls BandC water use (from reservoirs first, from pumping first, or proportional). 
!             The default is proportional (i.e., demand for Band C less than available, so some method was necessary
!             to evaluate what water to take to meet demand. I choose proportional (for now)

!      Version 5.8
!          1) I separated the Salt and Tonto Rivers from the Verde River
!           a) Read in daily flow data (cfs) for both systems (SaltTonto_doy.txt & VerdeTango_doy.txt)
!           b) generate monthly estimates of flow (mvf_saltTontoFlowMonthly_acft(month) & mvf_verdeFlowMonthly_acft(month))
!               I have global variables, for each [gvf_saltTontoMonthly_acft(month) & gvf_verdeMonthly_acft(month)]
!           c) set initial storage hard coded for both systems (data from Mark Hubble, SRP- 02.06.13)
!           d) run monthly estimates through a Verde (WaterShed_Verde.f90) and a Salt-Tonto (WaterShed_SaltTonto.f90)
!            subroutines. This is on a monthly time step.
!           e) generate outputs for both systems
!               go_VerdeStorage_acft
!               go_VerdeClassAused_acft
!               go_verdeRiverFlow_acft
!               go_VerdeClassBCused_acft

!               go_SaltTontoStorage_acft
!               go_SaltTontoClassAused_acft
!               go_SaltTontoClassBCused_acft
!               go_saltTontoRiverFlow_acft
!
!           03.04.13
!           2) I updated CO river flow data for 2009 through 2012 for annual data.
!               I am still missing the inflow estimates for that period.
!           03.06.13
!           3) Added new water supplies as subroutine NewWaterSupplies_k(T,gvi_order)in kernel
!               parameters are: gvf_newSuppliesUsed_acft_a(i),gvf_newSuppliesBalance_acft(T%year,i)
!               brought in from the interface as gvi_newWaterSupplies_acft_a(i) where i = num providers
!               gvf_newSuppliesUsed_acft_a(i), then, is passed to city_model via  vGP%lvf_newWaterSupplies_AF_a(i)
!               and brought to  subroutine sSurfaceWater(T,vPV,lvWB,i,vTstep)
!
!           03.22.13
!           4) Moved the Colorado River flow variable to the subroutine aModifyFlow_ba(T); Outputs to the 
!               interface now reflect Lees Ferry Flow. 
!
!           03.27.13
!           5) Changed the calculation of NCS water- I changed the reservoir volume from both Verde and Salt-Tonto 
!               to just Salt-Tonto (legacy code used the combined state variable "lvf_SRP_NCS(i)"
!           6) Re-wrote the calculation of BandC water used in pSRP_newClassBC(T,gvi_order,Aout,gtPV)
!               I added lvf_takeFromStorage10_acft(l0) and lvf_takeFromPumping10_acft(10) and simplified the code
!
!           7) Simplified the calculation of inputs into storage on the Salt-Tonto. I removed mvf_differenceBCMonthly(i)
!               from the calculations (enabling mvf_addToReservoirMonthly(2,i) to be both positive and negative)
!
!       MUCH HAS CHANGED SINCE MY LAST ENTRY
!
!           04.25.13
!           8) Re-worked the Agriculture module. I put the Ag vadose flux and other on a relative area
!             using the Ag Mask. I changed deep percolation to 20% of irrigation (from 15%) and cited a reference.
!            I changes evapotranspiration to 70%. Leaves 10% for biomass production and efficiency of irrigation
!           07.12.2013
!           9) I added bounds on the minimum and maximum liters per flush in sBlackWater

!         07.31.13
!           10) I added minimum criteria for the Salt Reservoirs to exit the "j" loop when the two are both
!               below minimum
!           11) I added a variable called gvf_writeLog that enables the model to write to the log file separate
!               from the gvl_WriteToMyDisk variable. writeToLog is passed in from the Interface.
!               as function getWriteLog() result(value) and subroutine setWriteLog(value)
!
!           12) I added a subroutine to handle assumed increases in the NormalFlow variable "gvf_modifyNormalFlow"
!               Recall, this is the acre-feet acre-1 that STV members can use of Class A water. Default is 0.95
!               which was verified. The subroutine sModifyNormalFlow(T,gvf_modifyNormalFlow,lvf_out) modifies
!               the value based on the climate factor (1/cf). THIS ASSUMES that as surface water availability
!               decreases that water providers would use more of their allocation. The range is 0 to 5.4228 as
!               defined in the Kent Decree
!
!           09.12.13
!            13) I added a function  function fSESgpcd_increase(year,gamma,gpcd,gpcd_1,gpcd_2,gpcd_3) to model
!               increases in GPCD for projecitons
!
!            14) I also added function fGamma(beta) to model # 13 above (takes the proportional increase in GPCD
!               expected by 2085 to greate the inputs for the fSESgpcd_increase function
!
!            15) Added fSESgpcd_equalWeight(N,gpcd) to use the Brown's equal weight algorithms to create a "constant"
!               estimate of GPCD (uses the past 10 estimates to project forward)
!
!            16) I changed the name of fSESgpcd to fSESgpcd_discount to reflect that the Brown's SES was a discount
!               algorithm (i.e., projects decrease through time)
!
!            17) In this process I update the GPCD estimates from ADWR. We now use a flat file to read in these
!               estimates from the file='App_Data\Data\gpcd_2010.txt'; LU=53
!
!           19.09.13
!            18) Added setNormalFlowEmpiricalMax(count, values) and getNormalFlowEmpiricalMax(count, values) to
!               modulate the maximum acre-feet per acre normal flow providers can receive if drought or climate change
!               influences demand for class A water. The variable is gvi_NormalFlowEmpMax_AFac(i)
!           06.09.14
!             19) Cleaned up code
!           07.18.14
!            19) Cleaned up code
!           09.08.14
!            20) I removed the variable go_FlowEnvironment_acft_a from KernelInterface.f90 and from the include file.
!                NOT being used.
!           09.11.14
!            21) Based on a talk by Charlie Ester I changed the SRP reservoir strage/pumping equations. See
!                 function fStorageRelease(state) for details.
!
!           10.16.14 
!            22) gvf_usedBankedWater_CAP(i)added to account for used Banked CAP water in the CAP use classification (i.e., lvf_CAP(i) )
!
!           12.06.14
!             23) Completely re-worked the Verde and the Salt-Tonto WaterShed code. a) I lowered the minimum for all reservoirs using
!                 published (web) data on levels (SRP web site), to 1% of storage as minimum (using ratios), b) changed the parameter estimates of the verde
!                 reservoir(s) (see code), c) changed the structure of the Salt-Tonto - Roosevelt - reservoirs- by c1- forcing the 
!                 mvf_flowControl variable to remain 0 <= value <=  1 as originally intended, c2- changed the annual ampunt of 
!                 reservoir water for Ag, c3 the variable amount based of flow, c4 - added a climate factor reduction of minimum
!                 criteria for both systems so that with reduction in flows comes a reduction in the base-line storage. And, I added a variable
!                 to take water from Roosevelt to add to "Others"  when mvf_flowControl == 1 yet Others not satisfied between min and current.
!           12.09.14
!                 I also added the subroutine that calculates the five-year running average storage for Roosevelt and I use that estimate
!                 in conjunction with the maximum storage to esimate a flow release estimate for Agriculture. This is crude, and likey
!                 "wrong" but it does create a good comparison for now between simulated and empirical estimates of storage. In addition,
!                 an Agricultural estimate of 250,000 AF year is used. That value is fit to a normal distribution, like all demand estimates,
!                 but then I off-set it buy 6 months. This estimate is also reduced by the climate factor.
!
!           01.07.15
!                re-worked the GPCD (SES discount) code.  Added a minimum GPCD - provider level - gvi_ProviderMinGPCD(provider) - ~ line 972 in 
!                providerPopulationandDemand,f90
!
!
!           01.16.15 I added a variable called go_WaterFromAgPumpingTotal which represents the raw ADWR estimate of ag pumping
!                   I also added the local variable lvf_AgMaxPumping which is the maximum pumping starting from the projection
!                   year. That is, 2015 to 2085 the maximum on the curve, which would be 2015 in this case because I added the
!                   variable gpi_projectionStartYear=2015 to grab the data from the curve(REMOVED on 03.03.15)
!                   
!           01.29.15 I completely reworked the agriculture transfer of groundwater credits to urban use. See the subroutine
!                   sAgGroundWater(T, lvf_addGroundwaterFromAg) called from subroutine pProviderGroundwater(T,gvi_order,vPV) in
!                   the file Groundwater.f90 (~ line 346)
!
!           07.29.15 I changed the allocation of CAP water in Designations_CO.f90. I determined that providers were not: 1) using
!                   there full entitlement, and 2) some CAP water was not being used when there was water to use.
!                   Changes may be found on Line 541. Esentially, I was allocating  lvf_CAPneeded_IV(i) as the ratio between
!                   four and five multiplied by lvf_CAP_needed(i). Instead, it should be the maximium entitled given the amount
!                   available (compared ti that "needed-i.e., requested".)
!
!           07.30.15 I added the ability to exaimine CAP water priority four and priority five water NOT used. These can be
!                   found as go_CAPunusedPriority4(j)=vDb%mvf_unUsedP4(j), and go_CAPunusedPriority5(j)=vDb%mvf_unUsedP5(j).
!                   line 682 in Designations_CO.f90. Code that determines the unused is found starting on line 564.
!                   
!       version 5.2
!           08.04.15 I added four output variables:  go_lossPotentialP4(i), go_lossPotentialP5(i)
!                go_rightsLostP4(i) and  go_rightsLostP5(i)
!                see Designations_CO.f90 line 544 (or there abouts). These represent CAP water 
!                NOT delivered when shortages on the CO system prevent the entire right for proiorty four and priority five 
!                water to be allocated. Loss Potential: Demand greater than the right, Demand Not Met: demand < right but amount 
!                available to deliver is less than the demand for the water
!
!       version 5.3
!           09.23.15 Attempting to re-create the code used currently on the wsuied.watersim.org program
!           
!       version 5.4
!          10.05.15 I added a new subroutine in Providers.f90 called waterBanking(). Water banking now enables
!           both the default baning plus the added banking of CO River surface water. Before it was one or the other.
!           now, it uses the default banking request compared with the actual available CO excess water. IF satisfied,
!           the remaining may be used with option = 1 to add to the banking as a percentage of available
!
!           10.05.15 I changed the GPCD code in the API... 
!
!           10.11.15 I put in "BaseYear" as a parameter in which to control when a policy may be initiated. In this
!             case it is 2016 (this will be read from the API)
!           10.11.15 I moved the parameter  go_WaterFromAgSurface_acft_a(i) from Augmentation to be added in the
!           annual groundwater credits. This is ag water from surface (vague)
!
!       version 6.0
!           date unknown: I added again rainfall as an input to the model. Using unit 72; look in Meteorology.f90
!
!           date unknown; using unit 81 to read in landcover data.
!
!           01.10.2016 I was using new water supplies as a "bank" for some unknown REASON! Wrong code, so I changed it
!           to an annual input as it's supposed to be. ~ Line 646 in Demand.f90
!
!       version 6.3.1
!           07.27.2016 1) I have added sensitivity code for running internal variables (see Module gm_DataAndSensitivity)
!                      2) This code has new calls for setting LCLU, scenario specifics for the Scenario's Project
!                      3) parameter_control now has a new read statement for default parameters, and 5 parameters total
!                      4) 
!
!       version 6.3.1.1
!           December-Jan
!                      1) Many changes not listed between my last notes and now.... A) new Land cover-land use, b) new code
!                         to estiamte low, medium, and high density water use, c) equations to estimate indoor and outdoor 
!                         water use (see LandUseLandCover.f90), d) revised water harvesting code, e) new outputs to the
!                         interface
!       version 6.3.2
!           01.27.17
!
!               1)I had to "fix" the Ag transfer of surface water credits - code was added to Designations_CO.f90 on
!                   lines() 665 (lvf_capPsix [ lvf_capPsix=max(0,lvf_AgPool)] is used instead of lvf_AgPool), 667 (I added lvf_usedP6(j) for the Ag
!                   CAP transfers and, thus I added lvf_desired_VI(j) (see corresponding code)
!               2)Designations_CO.f90; Added the intrinsic NINT to passing lvi_AgPool to Ag [i.e., lvi_AgPool=nint(lvf_AgPool)]
!               3)Designations_CO.f90; Line 805- added lvf_usedP6(i) as part of the CAP designations
!               4)WaterShed_SaltTonto.f90; Line 704, added lvf_modCriteria to stop hammering of the mvf_flowControl variable
!               5)WaterShed_SaltTonto.f90; Line 703, added thte logical lvl_stopHammerTwo to add in this code
!               6)WaterShed_SaltTonto.f90; Line 640, I added an if statement for when the climate factor is above one,
!                   to NOT modify the lvf_modLowerThreshold
!               7)Regressions.f90; For the Normal subroutine, I added if do loops to set the output of the subroutine
!                   lvf_out(12) to equal zero when there is no Normal flow on the Salt-Verde to use. These were added in 
!                   the goto statement returns so that no error is thrown
!               8)LandUseLandCover.f90; Changed the file read \lULC_Scenario.txt' to lCLU_Scenario.txt' (unit 82)
!               9)Parameter_Control.f90; Addded a check variable (lvf_sum_A and lvf_sum_B) to check the default values
!                   in order to remove the FORTRAN warning about comparing real's using a logical
!               10) Ag pumbing was actually missing from the Aquifer subroutine in Water_CityModel.f90
!                   such that the groundwater was not accurate

! ---------------------------------------------------------------------------------------------------------------------
!    
    Module lm_BuildNotes
      use gm_VersionControl
      !
      contains
          ! ------------------------------
          function BuildNumK() result(num)
            !
            ! --- Types ---
            integer:: num
            ! =============
            num = gdi_Model
          end function
          function BuildVerK() result(num)
            integer:: num
            num = gdi_Vers
          end function
          function BuildSubVerK() result(num)
            integer:: num
            num = gdi_SubVers
          end function
          function MonthK() result(num)
            integer:: num
            num=gdi_Month !dt(2)
          end function
          function DayK() result(num)
            integer:: num
            num=gdi_Day
          end function
          function YearK() result(num)
            integer:: num
            num=gdi_Year
          end function
          function HourK() result(num)
            integer:: num
            num=gdi_Hour
          end function
          function MinK() result(num)
            integer:: num
            num=gdi_Min
          end function 
      !
    End Module lm_BuildNotes
!
! ======================================================================================================
! E.O.F BuildNotes.f90