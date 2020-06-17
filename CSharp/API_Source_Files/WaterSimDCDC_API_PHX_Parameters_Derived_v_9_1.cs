// ===========================================================
//     WaterSimDCDC Regional Water Demand and Supply Model Version 5.0

//       Adds Parameters that are derived from Model Results or other sources

//       WaterSimDCDC_API_DerivedParameters 
//       Version 5.0
//       Keeper Ray Quay  ray.quay@asu.edu
//       
//       Copyright (C) 2011,2012 , The Arizona Board of Regents
//              on behalf of Arizona State University

//       All rights reserved.

//       Developed by the Decision Center for a Desert City
//       Lead Model Development - David A. Sampson <david.a.sampson@asu.edu>

//       This program is free software: you can redistribute it and/or modify
//       it under the terms of the GNU General Public License version 3 as published by
//       the Free Software Foundation.

//       This program is distributed in the hope that it will be useful,
//       but WITHOUT ANY WARRANTY; without even the implied warranty of
//       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//       GNU General Public License for more details.

//       You should have received a copy of the GNU General Public License
//       along with this program.  If not, please see <http://www.gnu.org/licenses/>.
//
//====================================================================================
        using System;
        using System.Collections.Generic;
        using System.Linq;
        using System.Text;
        using WaterSimDCDC;
        using WaterSimDCDC.Data;
       
       namespace WaterSimDCDC
       {
       
         
           public partial class WaterSimManager
           {
       
               //==================================================
               //Total Supply
               // -----------------------------------------------------------
             
               private int[] get_TotalSupply()
               {
                   ProviderIntArray TSA = new ProviderIntArray(0);
       
                   // get all supplies
                   int[] GW = Groundwater_Pumped_Municipal.getvalues().Values;
                   int[] Salt = SaltVerde_Annual_Deliveries_SRP.getvalues().Values;
                   int[] Coldo = Colorado_Annual_Deliveries.getvalues().Values;
                   int[] GWBank = Groundwater_Bank_Used.getvalues().Values;
                   int[] Reclaimed = Reclaimed_Water_Used.getvalues().Values;
                   int[] ROReclaimed = RO_Reclaimed_Water_Used.getvalues().Values;
                   int[] AugWater = WaterAugmentationUsed.getvalues().Values;
                   // 08.21.16 das
                  // int[] RainHarvested = RainWaterHarvested.getvalues().Values;

                   for (int i = 0; i < TSA.Length; i++)
                   {
                       int TS = 0;
                       //TS = GW[i] + Salt[i] + Coldo[i] + GWBank[i] + Reclaimed[i] + ROReclaimed[i] + AugWater[i]  + RainHarvested[i];
                       TS = GW[i] + Salt[i] + Coldo[i] + GWBank[i] + Reclaimed[i] + ROReclaimed[i] + AugWater[i] ;
                       TSA[i] = TS;
                       //TotalSupply[i] = TS;
                   }
                   return TSA.Values; // TotalSupply.Values; 
               }
             
               
               /// <summary> The total water supply actually used </summary>
               /// <summary> Includes the following
               ///    Groundwater_Pumped_Municipal
               ///    SaltVerde_Annual_Deliveries_SRP
               ///    Colorado_Annual_Deliveries
               ///    Groundwater_Bank_Used
               ///    Reclaimed_Water_Used
               ///    RO_Reclaimed_Water_Used
               ///    
               ///    </summary>
               ///<remarks> </remarks>
               ///    <seealso cref="Groundwater_Pumped_Municipal"/>
               ///    <seealso cref="SaltVerde_Annual_Deliveries_SRP"/>
               ///    <seealso cref="Colorado_Annual_Deliveries"/>
               ///    <seealso cref="Groundwater_Bank_Used"/>
               ///    <seealso cref="Reclaimed_Water_Used"/>
               ///    <seealso cref="RO_Reclaimed_Water_Used"/>
       
               public providerArrayProperty Total_Water_Supply_Used;
       
       
       
               //==================================================
               //Percent GW of demand
               //-------------------------------------------------
       
               private int[] get_PCT_GWofDemand()
               {
                   ProviderIntArray GWofDemand = new ProviderIntArray(0);
                   // get deficit and demand
                   int[] GWPumped = Groundwater_Pumped_Municipal.getvalues().Values;
                   int[] PDemand = Total_Demand.getvalues().Values;

       
                   for (int i = 0; i < GWofDemand.Length; i++)
                   {
                       // caculated percent as integer 100=100%
                       // DAS added the double and moved the *100
                       if (PDemand[i] > 0)
                           GWofDemand[i] = Convert.ToInt32( (Convert.ToDouble( GWPumped[i]) ) / (Convert.ToDouble(PDemand[i]))*100);
                   }
                   return GWofDemand.Values;
               }
       
       
               /// <summary> The pct gw of demand </summary>
               /// <summary>The % of demand met using groundwater (100 = 100%) </summary>
               ///<remarks>0 if GW use os 0%</remarks>
               /// <seealso cref="Groundwater_Pumped_Municipal"/>
               /// 
               public providerArrayProperty PCT_GW_of_Demand;
       
       
               //==================================================
               //Total Reclaimed Water
               //-------------------------------------------------
       
               private int[] get_Reclaimed_Used()
               {
                   ProviderIntArray TSA = new ProviderIntArray(0);
       
                   // get deficit and demand
                   int[] Reclaimed = Reclaimed_Water_Used.getvalues().Values;
                   int[] ROReclaimed = RO_Reclaimed_Water_Used.getvalues().Values;
       
                   for (int i = 0; i < TSA.Length; i++)
                   {
                       int TS = 0;
                       TS = Reclaimed[i] + ROReclaimed[i];
                       TSA[i] = TS;
                       //TotalSupply[i] = TS;
                   }
                   return TSA.Values; // TotalSupply.Values; 
               }
        
       
               /// <summary> The pct gw of demand </summary>
               /// <summary>The % of demand met using groundwater (100 = 100%) </summary>
               ///<remarks>0 if GW use os 0%</remarks>
               /// <seealso cref="Groundwater_Pumped_Municipal"/>
               /// 
               public providerArrayProperty Total_Reclaimed_Used;
               
               //==================================================
               //Effluent To Ag
               //-------------------------------------------------
               private int[] get_Effluent_To_Ag()
               {
                   ProviderIntArray TSA = new ProviderIntArray(0);
       
                   // get deficit and demand
                   int[] Effluent = Total_Effluent_Reused.getvalues().Values;
                   int[] Power =  Effluent_To_PowerPlant.getvalues().Values;
                   int[] Vadose = Effluent_To_Vadose.getvalues().Values;
       
                   for (int i = 0; i < TSA.Length; i++)
                   {
                       int TS = 0;
                       TS = Effluent[i]-(Power[i] + Vadose[i]);
                       TSA[i] = TS;
                       //TotalSupply[i] = TS;
                   }
                   return TSA.Values; // TotalSupply.Values; 
               }

       
               public providerArrayProperty Provider_Effluent_to_AG;
       
               //==================================================
               //Total Effluent
               //-------------------------------------------------
       
               private int[] get_Total_Effluent()
               {
                   ProviderIntArray TSA = new ProviderIntArray(0);
       
                   // get deficit and demand
                   int[] EffluentUsed = Total_Effluent_Reused.getvalues().Values;
                   int[] get_EffluentDischarged = Effluent_Discharged.getvalues().Values;
       
                   for (int i = 0; i < TSA.Length; i++)
                   {
                       int TS = 0;
                       TS = (EffluentUsed[i] + get_EffluentDischarged[i]);
                       TSA[i] = TS;
                       //TotalSupply[i] = TS;
                   }
                   return TSA.Values; // TotalSupply.Values; 
               }
       
       
               public providerArrayProperty Total_Effluent;
               //==================================
               // PopulationClass used to store Projected Pop Data from Population_On and Population_Off data files
               public PopulationClass PopData; 
       
               //==================================================
               // Projected On Project Population
               //-------------------------------------------------
       
               private int[] get_Projected_On_Pop()
               {
                   int year = Sim_CurrentYear;
                   ProviderIntArray PopOn = new ProviderIntArray(0);
                   for (int i = 0; i < PopOn.Length; i++)
                   {
                       PopOn[i] = PopData.GetYearOnPopData(year).Values[i];
                   }
                   return PopOn.Values;
               }
       
       
               /// 
               public providerArrayProperty Projected_OnProject_Pop;
       
               //==================================================
               // Projected Off Project Population
               //-------------------------------------------------
       
               private int[] get_Projected_Off_Pop()
               {
                   int year = Sim_CurrentYear;
                   ProviderIntArray PopOff = new ProviderIntArray(0);
                   for (int i = 0; i < PopOff.Length; i++)
                   {
                       PopOff[i] = PopData.GetYearOffPopData(year).Values[i];
                   }
                   return PopOff.Values;
               }
       
       
               /// 
               public providerArrayProperty Projected_Other_Pop;

               //==================================================
               // Projected Population
               //-------------------------------------------------

               private int[] get_Projected_Pop()
               {
                   int year = Sim_CurrentYear;
                   ProviderIntArray Pop = new ProviderIntArray(0);
                   for (int i = 0; i < Pop.Length; i++)
                   {
                       Pop[i] = PopData.GetYearPopData(year).Values[i];
                   }
                   return Pop.Values;
               }


               /// 
               public providerArrayProperty Projected_Pop;

               //==================================================
               // POP Difference From Projected Off Project Population
               //-------------------------------------------------


               private int[] get_Difference_Projected_Off_Pop()
               {
                   int year = Sim_CurrentYear;
                   ProviderIntArray PopDiff = new ProviderIntArray(0);
                   for (int i = 0; i < PopDiff.Length; i++)
                   {
                       PopDiff[i] = Population_Other[i] - PopData.GetYearOffPopData(year).Values[i];
                   }

                   return PopDiff.Values;
               }


               /// 
               public providerArrayProperty Difference_Projected_Other_Pop;

               //==================================================
               // POP Difference From Projected On Project Population
               //-------------------------------------------------

               private int[] get_Difference_Projected_On_Pop()
               {
                   int year = Sim_CurrentYear;
                   ProviderIntArray PopDiff = new ProviderIntArray(0);
                   for (int i = 0; i < PopDiff.Length; i++)
                   {
                       PopDiff[i] = Population_On_Project[i] - PopData.GetYearOnPopData(year).Values[i];
                   }

                   return PopDiff.Values;
               }


               /// <summary>    The difference between projected and actual other POP. </summary>
               public providerArrayProperty Difference_Projected_OnProject_Pop;

               //==================================================
               // POP Difference From Projected Population
               //-------------------------------------------------

               private int[] get_Difference_Projected_Pop()
               {
                   int year = Sim_CurrentYear;
                   ProviderIntArray PopDiff = new ProviderIntArray(0);
                   for (int i = 0; i < PopDiff.Length; i++)
                   {
                       PopDiff[i] = Population_Used[i] - PopData.GetYearPopData(year).Values[i];
                   }

                   return PopDiff.Values;
               }


               /// <summary>    The difference between projected and actual other POP. </summary>
               public providerArrayProperty Difference_Projected_Pop;    
               
               //==================================================
               // POP Percent Difference From Projected Population
               //-------------------------------------------------

               private int[] get_PCTDifference_Projected_Pop()
               {
                   int year = Sim_CurrentYear;
                   ProviderIntArray PopDiff = new ProviderIntArray(0);
                   ProviderIntArray PopPCTDiff = new ProviderIntArray(0);
                   for (int i = 0; i < PopDiff.Length; i++)
                   {

                       PopDiff[i] = Population_Used[i] - PopData.GetYearPopData(year).Values[i];
                       int pu = Population_Used[i];
                       if (pu == 0)
                       {
                           PopPCTDiff[i] = 100;
                       }
                       else
                         PopPCTDiff[i] = (PopDiff[i] * 100) / Population_Used[i];
                   }

                   return PopPCTDiff.Values;
               }


               /// <summary>    The difference between projected and actual other POP. </summary>
               public providerArrayProperty PCT_Difference_Projected_Pop;
               //==================================         
               partial void initialize_Derived_ModelParameters()
               {
       
                   // add Total_Reclaimed_Used 
                   Total_Reclaimed_Used = new providerArrayProperty(_pm,eModelParam.epTotalReclaimedUsed, get_Reclaimed_Used, eProviderAggregateMode.agSum);
                   _pm.AddParameter(new ModelParameterClass(eModelParam.epTotalReclaimedUsed, "Total Reclaimed Water Used ", "TRECUSED", Total_Reclaimed_Used, 0, 50000)); //modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck, 0, 0, null, get_Reclaimed_Used, null, null, null, null, Total_Reclaimed_Used));
                   // add PCT_GW_of_Demand
                   PCT_GW_of_Demand = new providerArrayProperty(_pm,eModelParam.epPCTGWofDemand, get_PCT_GWofDemand, eProviderAggregateMode.agWeighted);
                   _pm.AddParameter(new ModelParameterClass(eModelParam.epPCTGWofDemand, "PCT Demand met by Groundwater", "PCTGWDEM", PCT_GW_of_Demand, 0, 100)); //modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck, 0, 0, null, get_PCT_GWofDemand, null, null, null, null, PCT_GW_of_Demand));
                   // add Total_Water_Supply_Used 
                   Total_Water_Supply_Used = new providerArrayProperty(_pm,eModelParam.epTotalSupplyUsed, get_TotalSupply, eProviderAggregateMode.agSum);
                   _pm.AddParameter(new ModelParameterClass(eModelParam.epTotalSupplyUsed, "Total Water Supply Used", "TOTSUPUSE", Total_Water_Supply_Used, DepWaterSupplyGroup, 0, 700000)); //modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck, 0, 0, null, get_TotalSupply, null, null, null, null, Total_Water_Supply_Used));
                   // add Total_Water_Supply_Used 
                   Provider_Effluent_to_AG = new providerArrayProperty(_pm, eModelParam.epProvderEffluentToAg, get_Effluent_To_Ag, eProviderAggregateMode.agSum);
                   _pm.AddParameter(new ModelParameterClass(eModelParam.epProvderEffluentToAg, "Provider Effluent to AG", "PEFFTOAG", Provider_Effluent_to_AG, 0, 200000)); //, modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck, 0, 0, null, get_Effluent_To_Ag, null, null, null, null, Provider_Effluent_to_AG));
                   // add Total_Water_Supply_Used 
                   Total_Effluent = new providerArrayProperty(_pm,eModelParam.epTotalEffluent, get_Total_Effluent, eProviderAggregateMode.agSum);
                   _pm.AddParameter(new ModelParameterClass(eModelParam.epTotalEffluent, "Total Effluent Created", "TOTEFF", Total_Effluent,  0 , 300000)); //modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck, 0, 0, null, get_Total_Effluent, null, null, null, null, Total_Effluent));
                   //
                   //Total_Effluent = new providerArrayProperty(_pm, eModelParam.epTotalEffluent, get_Total_Effluent, eProviderAggregateMode.agSum);
                   //_pm.AddParameter(new ModelParameterClass(eModelParam.epTotalEffluent, "Total Effluent Created", "TOTEFF", Total_Effluent, 0, 300000)); //modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck, 0, 0, null, get_Total_Effluent, null, null, null, null, Total_Effluent));
                   
                   // add Projected pop Parameters
                   //
                   // 07.25.2016 ASK Ray why this is in here...
                   //
                   // create Population class add load with data
                  PopData = new PopulationClass(DataDirectory, 2000);
                   // On Project
                  Projected_OnProject_Pop = new providerArrayProperty(_pm,eModelParam.epProjectedOnProjectPop, get_Projected_On_Pop, eProviderAggregateMode.agSum);
                  _pm.AddParameter(new ModelParameterClass(eModelParam.epProjectedOnProjectPop, "Projected Pop OnProject", "PRJPOPON", Projected_OnProject_Pop , 0 , 2000000)); //modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck, 0, 0, null, get_Projected_On_Pop, null, null, null, null, Projected_OnProject_Pop));
                  
                   Difference_Projected_OnProject_Pop = new providerArrayProperty(_pm,eModelParam.epDifferenceProjectedOnPop, get_Difference_Projected_On_Pop, eProviderAggregateMode.agSum);
                   _pm.AddParameter(new ModelParameterClass(eModelParam.epDifferenceProjectedOnPop, "Deviation from Projected Pop On Project", "DIFPOPON", Difference_Projected_OnProject_Pop , 0 , 1000000)); //modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck, 0, 0, null, get_Difference_Projected_On_Pop, null, null, null, null, Difference_Projected_OnProject_Pop));
                  // Other Project
                  
                   Projected_Other_Pop = new providerArrayProperty(_pm,eModelParam.epProjectedOtherPop, get_Projected_Off_Pop, eProviderAggregateMode.agSum );
                   _pm.AddParameter(new ModelParameterClass(eModelParam.epProjectedOtherPop, "Projected Pop Off Project", "PRJPOPOFF", Projected_Other_Pop, 0 , 2000000)); //modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck, 0, 0, null, get_Projected_Off_Pop, null, null, null, null, Projected_Other_Pop));
                  
                   Difference_Projected_Other_Pop = new providerArrayProperty(_pm,eModelParam.epDifferenceProjectedOtherPop, get_Difference_Projected_Off_Pop, eProviderAggregateMode.agSum);
                   _pm.AddParameter(new ModelParameterClass(eModelParam.epDifferenceProjectedOtherPop, "Deviation from Projected Pop Off Project", "DIFPOPOF", Difference_Projected_Other_Pop , 0 , 1000000)); //modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck, 0, 0, null, get_Difference_Projected_Off_Pop, null, null, null, null, Difference_Projected_Other_Pop));
                  // Total
                  
                   Projected_Pop = new providerArrayProperty(_pm,eModelParam.epProjectedTotalPop, get_Projected_Pop, eProviderAggregateMode.agSum);
                   _pm.AddParameter(new ModelParameterClass(eModelParam.epProjectedTotalPop, "Projected Total Pop", "PRJPOPTOT", Projected_Pop , 1000 , 4000000)); //modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck, 0, 0, null, get_Projected_Pop, null, null, null, null, Projected_Pop));
                  
                   Difference_Projected_Pop = new providerArrayProperty(_pm,eModelParam.epDifferenceTotalPop, get_Difference_Projected_Pop, eProviderAggregateMode.agSum);
                  _pm.AddParameter(new ModelParameterClass(eModelParam.epDifferenceTotalPop, "Deviation from Projected Total Pop", "DIFPOPTOT", modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck,   0 , 2000000, null, get_Difference_Projected_Pop, null, null, null, null, Difference_Projected_Pop));
                  
                   PCT_Difference_Projected_Pop = new providerArrayProperty(_pm,eModelParam.epPCTDiffProjectedTotalPop, get_PCTDifference_Projected_Pop, eProviderAggregateMode.agSum);
                   _pm.AddParameter(new ModelParameterClass(eModelParam.epPCTDiffProjectedTotalPop, "PCT Deviation from Projected Total Pop", "PCTPOPTOT", PCT_Difference_Projected_Pop , 0 , 100));//modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck, 0, 0, null, get_PCTDifference_Projected_Pop, null, null, null, null, PCT_Difference_Projected_Pop));

                   ProcessRegistry.addAnnualFeedbackProcess(typeof(Processes.AlterGrowthFeedbackProcess));
                   ProcessRegistry.addAnnualFeedbackProcess(typeof(Processes.AlterGPCDFeedbackProcess));
                   ProcessRegistry.addAnnualFeedbackProcess(typeof(Processes.AlterGPCDAWSFeedbackProcess));
                   ProcessRegistry.addAnnualFeedbackProcess(typeof(Processes.AWSLimitFeedbackProcess));
                   ProcessRegistry.addAnnualFeedbackProcess(typeof(Processes.TrackProviderDeficitsParameter));
                   ProcessRegistry.addAnnualFeedbackProcess(typeof(Processes.Personal_GPCD_WebFeedbackProcess)); 
                   //
                   ProcessRegistry.addAnnualFeedbackProcess(typeof(Processes.PolicyLimitFeedbackProcess));
                   ProcessRegistry.addAnnualFeedbackProcess(typeof(Processes.TrackGroundwaterCreditsProcess));
                   ProcessRegistry.addAnnualFeedbackProcess(typeof(Processes.TrackAvailableGroundwater));
                   ProcessRegistry.addAnnualFeedbackProcess(typeof(Processes.TrackAvailableGroundwater));

               }

           }
       }
