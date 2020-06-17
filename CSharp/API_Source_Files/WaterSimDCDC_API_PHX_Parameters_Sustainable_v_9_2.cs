// ===========================================================
//     WaterSimDCDC Regional Water Demand and Supply Model Version 5.0

//       Adds derived sustainability parameters as measures of model output

//       WaterSimDCDC_API_SustainableParameters
//       Version 4.0
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
using WaterSimDCDC;
using WaterSimDCDC.Documentation;
using UniDB;


// last write: 01.20.15 DAS,
namespace WaterSimDCDC
{

    //================================================
    public partial class WaterSimManager
    {
        #region indicator weighting
        internal class IndicatorWeighting
       {
            double calc = 0;
            internal int Pout = 0;
            int One = 0;
            int Two = 0;
            int Three = 0;
            //
            double _w1 = 0.4;
            double _w2 = 0.6;
            double _w3 = 0;
            //
            string weight = "Weighting factors do not add to 1";
            string sum = "The number of passing weighted is not correct";
        //
            internal IndicatorWeighting() : base()
            {

            }
            internal IndicatorWeighting(int First, int Second, double W1,double W2) 
            {
            One = First;
            Two = Second;
            
            _w1 = W1;
            _w2 = W2;
            }
        
        internal IndicatorWeighting(int First, int Second, int Third,double W1, double W2, double W3)
        {
            One = First;
            Two = Second;
            Three = Third;
            _w1 = W1;
            _w2 = W2;
            _w3 = W3;
        }

        internal int Weight()
        {
             try
            {
                 if(0 <= One && 0 <= Two)
                {
                    if( Pcheck(w1, w2, w3))
                    { calc =( Convert.ToDouble(One) * w1) + (Convert.ToDouble(Two) * w2);}
                     
                }
                 else if (0 <= One && 0 <= Two && 0 <= Three)
                {
                    if (Pcheck(w1, w2, w3)) { calc = (Convert.ToDouble(One) * w1) + (Convert.ToDouble(Two) * w2) + (Convert.ToDouble(Three) * w3); }
                }
                 Pout = Convert.ToInt32(calc);

            }
            catch (Exception e)
             {
               sum = e.Message;
             }

            return Pout;
        }
        //-----------------------------------------------------------
        private bool Pcheck(double one, double two, double three)
        {
            bool check = false;
            double Total = one + two + three;
            if (Total.Equals(1.0))
            {
                try
                {
                    check = true;
                }
                catch(Exception e)
                {
                    weight = e.Message;
                }
            }
            return check;
        }

        internal double w1
        {
            get { return _w1; }
            set { _w1 = value; }
        }
        internal double w2
        {
            get { return _w2; }
            set { _w2 = value; }
        }
        internal double w3
        {
            get { return _w3; }
            set { _w3 = value; }
        }
     }
        #endregion 
        //==================================================
        // Environment Indicator
        // ==================================================
        #region - Env
        int delta = 0;

            public int GetEnvIndicator()
            {
                delta = COdeltaBurdenRatioForAZ;
                  // ----------------------------------------------------------------------------------
                int WaterForEnv_sustainability = delta;
                //------------------------------------------------------------------------------------------
                return WaterForEnv_sustainability;
            }
       #endregion
        //=================================================================
        // Agriculture Sustainability Indicnator
        //=================================================================
        #region - Ag
        //internal int GetAgIndicator()
        //public int  GetAgIndicator()
        internal int GetAgIndicator()
        {
            // was Phoenix until 08.25.15
            // Left over from testing..... dang
           //int  AGWaterUsedAvg = AGUsed[API.ph];
            int AgAvg =  PCT_AgWaterUsedToThresh.RegionalValue(eProvider.Regional);
            return AgAvg;
        }
        #endregion
        // end of Agriculture Sustainability Indicator
        // 

        //==============================================================
        // Personal Water Use Indicator
        //
        #region - PWU

        // GPCD of 30 is undeveloped country stansard
        //  GPCD of 60 is very low
        //  GPCD of 90 is low
        //  GPCD of 120 is low average
        //  GPCD of 150 is High average
        //  GPCD of 180 is high
        //  GPCD of 210 is very high
        //  GPCD of 240> is extravagant
        //  
        ProviderIntArray myGPCD = new ProviderIntArray(0);
        internal int GetPersonalGPCDIndicator()
        {

            for (int i = 0; i < 33; i++) {
                myGPCD[i] = GPCD_Used.getvalues().Values[i];
            }
            int GPCDAvg = GPCD_Used.RegionalValue(eProvider.Regional);
            // ok on the scale above figure out where you are
            //int GPCDScale = GPCDAvg / 30;
            //GPCDScale++;
            //// no higher than nine
            //if (GPCDScale > 9) GPCDScale = 9;
            //int TempInd = (GPCDScale * 100) / 9;
            //return TempInd;
            return GPCDAvg;
        }

        #endregion
        // end of Personal Indicator
         
        //=========================================================
        //Percent Deficit
        // THIS NEEDS CHANGING!!!!!
        // DAS 09.24.15... this is demand deficit
        //---------------------------------------     
        #region
        private int[] get_PCT_Deficit()
        {
            ProviderIntArray FPCT_Deficit = new ProviderIntArray(0);
            // get deficit and demand
            int[] PDeficit = Demand_Deficit.getvalues().Values;
            int[] PDemand = Total_Demand.getvalues().Values;

            for (int i = 0; i < FPCT_Deficit.Length; i++)
            {
                // caculated percent as integer 100=100%
                if (PDemand[i] > 0)
                    FPCT_Deficit[i] = (PDeficit[i] * 100) / PDemand[i];
            }
            return FPCT_Deficit.Values;
        }


        /// <summary> Deficit as a percent of Demand (100 = 100%) </summary>
        ///<remarks>0 if Deficit is 0 </remarks>
        /// <seealso cref="Demand_Deficit"/>
        /// 
        public providerArrayProperty Percent_Deficit;
        #endregion
        //============================================================
        //  Percent Reclaimed of Total Supply
        //---------------------------------------------  
        #region
        private int[] get_PCT_Reclaimed_Of_Total()
        {
            ProviderIntArray FPCT_Rec = new ProviderIntArray(0);
            // get deficit and demand
            ProviderIntArray PTotal = Total_Water_Supply_Used.getvalues();
            ProviderIntArray PRec = Total_Reclaimed_Used.getvalues();

            for (int i = 0; i < PRec.Length; i++)
            {
                // caculated percent as integer 100=100%
                if (PTotal[i] > 0)
                    FPCT_Rec[i] = (PRec[i] * 100) / PTotal[i];
            }
            return FPCT_Rec.Values;
        }


        /// <summary> Deficit as a percent of Demand (100 = 100%) </summary>
        ///<remarks>0 if Deficit is 0 </remarks>
        /// <seealso cref="Demand_Deficit"/>
        /// 
        public providerArrayProperty Percent_Reclaimed_of_Total;
        #endregion
        //==================================
        //====================================================
        // Running Regional Groundwater Amount - returned as a 
        // percent of original reduced(100 minus)
        //=====================================================

        public int GW_PCTinitial = 0;
      //  providerArrayProperty RunningGWregional;
        private int get_PCT_RegGWfromInitial()
        {
            ProviderIntArray RGWPCT = new ProviderIntArray(0);
            //ProviderIntArray TestMe = PCT_Wastewater_Reclaimed.getvalues();

            int UpdatedRegional = this.Regional_Groundwater_Balance;
            double dUpdated = Convert.ToDouble(UpdatedRegional);
            double Initial_GW = Convert.ToDouble(GW_PCTinitial);
            double response = 0;
            int GWout = 0;
           
             for(int p=0; p < ProviderClass.NumberOfProviders; p++)
             {
                 if (0 < Initial_GW)
                 {
                     response = 100 - (100 + (((dUpdated - Initial_GW) / Initial_GW) * 100));
                     GWout = Convert.ToInt32(response);
                 }
                 RGWPCT[p] = GWout;
             }
             return GWout;// RGWPCT.Values;
        }

        //====================================================
        // Years of AWS Sustained pumping
        //=====================================================

        public providerArrayProperty FutureYearsSustainedPumping;

        private int[] get_FutureYearsSustainedPumping()
        {
            ProviderIntArray FYSP = new ProviderIntArray();
            ProviderIntArray Balance = new ProviderIntArray();
            ProviderIntArray Pumped = new ProviderIntArray();
            Balance = Groundwater_Balance.getvalues();
            Pumped = Groundwater_Pumped_Municipal.getvalues();
            foreach (eProvider ep in ProviderClass.providers())
            {
                int yrs = 0;
                if (Pumped[ep] > 0)
                {
                    if (Balance[ep] > 0)
                    {
                        yrs = Balance[ep] / Pumped[ep];
                    }
                    else
                    {
                        yrs = 0;
                    }
                }
                else
                {
                    yrs = 100;
                }
                if (yrs > 100) yrs = 100;
                FYSP[ep] = yrs;
            }
            return FYSP.Values;
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Gets the regional future years sustained pumping. </summary>
        ///
        /// <returns>   The regional future years sustained pumping. </returns>
        ///-------------------------------------------------------------------------------------------------

        //private int get_Regional_FutureYearsSustainedPumping() {
            public int get_Regional_FutureYearsSustainedPumping() { 
            
                //// EDIT QUAY 1/4/14 moved back to be years from end of simulation
                int start = Sim_StartYear;// _Simulation_Start_Year;
            int stop = Sim_CurrentYear;
            int years = (stop - start) + 1;
            //return FutureYearsSustainedPumping.RegionalValue(eProvider.Regional)+years;
            return FutureYearsSustainedPumping.RegionalValue(eProvider.Regional);
            }
        ///

        public int new_FutureYearsSustainedPumping()
        {

            //// EDIT QUAY 1/4/14 moved back to be years from end of simulation
            int start = Sim_StartYear;// _Simulation_Start_Year;
            int stop = Sim_CurrentYear;
            int years = (stop - start) + 1;
            return FutureYearsSustainedPumping.RegionalValue(eProvider.Regional);
        }

        ///

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Gets the regional pct gw of demand. </summary>
        ///
        /// <returns>   The regional pct gw of demand. </returns>
        ///-------------------------------------------------------------------------------------------------

        //private int get_Regional_PCT_GW_of_Demand()
        public int get_Regional_PCT_GW_of_Demand()
        {
            return PCT_GW_of_Demand.RegionalValue(eProvider.Regional);
        }

        ///-------------------------------------------------------
        /// <summary> Mead Reservoir Levels</summary> 
        // Mead DeadPool
        const int MeadDeadPoolLevel = 895;

        private int get_MeadDeadPool()
        {
            return MeadDeadPoolLevel;
        }

        // Mead Lower SNWA intake
        const int MeadSNWALowerIntake = 1000;

        private int get_MeadSNWALowerIntake()
        {
            return MeadSNWALowerIntake;
        }

        // Mead Shortage Sharing Agreement
        const int MeadShortageAgreementTriggerLow = 1025;
        const int MeadShortageAgreementTriggerMed = 1050;
        const int MeadShortageAgreementTriggerHigh = 1075;

        private int get_MeadShortageSharingLevelLow()
        {
            return MeadShortageAgreementTriggerLow;
        }

        private int get_MeadShortageSharingLevelMed()
        {
            return MeadShortageAgreementTriggerMed;
        }

        private int get_MeadShortageSharingLevelHigh()
        {
            return MeadShortageAgreementTriggerHigh;
        }

        // Mead power pool
        const int MeadMinPowerLevel = 1050;

        private int get_MeadMinPowerLevel()
        {
            return MeadMinPowerLevel;
        }

        // Mead capacity
        const int MeadCapacity = 1229;

        private int get_MeadCapacity()
        {
            return MeadCapacity;
        }
        // The Growun Water Sustainabikity Dependency group
        int[] SINYRGW_List = new int[2] { eModelParam.epGroundwater_Balance, eModelParam.epGroundwater_Pumped_Municipal };

        
        partial void     initialize_Sustainable_ModelParameters()
        {
            Percent_Deficit = new providerArrayProperty(_pm,eModelParam.epPCT_Deficit, get_PCT_Deficit, eProviderAggregateMode.agWeighted);
            Percent_Reclaimed_of_Total = new providerArrayProperty(_pm, eModelParam.epPctRecOfTotal, get_PCT_Reclaimed_Of_Total, eProviderAggregateMode.agWeighted);
            FutureYearsSustainedPumping = new providerArrayProperty(_pm, eModelParam.epGWYrsSustain, get_FutureYearsSustainedPumping, eProviderAggregateMode.agWeighted);
            //
            //RunningGWregional = new providerArrayProperty(_pm, eModelParam.epRegGWpctInitial, get_PCT_RegGWfromInitial, eProviderAggregateMode.agWeighted);
            //
            ModelParameterGroupClass SINYRGW_Group = new ModelParameterGroupClass("Sustainable GW Years Dependencies",SINYRGW_List);
            _pm.GroupManager.Add(SINYRGW_Group);
            _pm.AddParameter(new ModelParameterClass(eModelParam.epGWYrsSustain, "Years Pumping Can Be Sustained", "SINYRGW", FutureYearsSustainedPumping, SINYRGW_Group, 0 , 100));

            _pm.AddParameter(new ModelParameterClass(eModelParam.epRegGWYrsSustain, "Years Pumping Can Be Sustained", "SINYRGWR", get_Regional_FutureYearsSustainedPumping, SINYRGW_Group,0, 100));
            //
            _pm.AddParameter(new ModelParameterClass(eModelParam.epRegPctGWDemand, "% Demand met by Groundwater", "SINPCTGW", get_Regional_PCT_GW_of_Demand, SINYRGW_Group, 0, 100));

            _pm.AddParameter(new ModelParameterClass(eModelParam.epPctRecOfTotal, "Percent Supply Reclaimed", "PCTSUPREC", Percent_Reclaimed_of_Total, 0, 100));

            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Deficit, "Percent Demand Deficit", "PCTDEMDEF", Percent_Deficit, 0, 100));

            _pm.AddParameter(new ModelParameterClass(eModelParam.epAgSustainIndicator, "Agricuture Sustainability Indicator", "SINDAG", GetAgIndicator, 0, 100));

            _pm.AddParameter(new ModelParameterClass(eModelParam.epEnvSustainIndicator, "Environment Sustainability Indicator", "SINDENV", GetEnvIndicator, 0, 500));

            _pm.AddParameter(new ModelParameterClass(eModelParam.epWaterUseIndicator, "Water Use Sustainability Indicator", "SINDPC", GetPersonalGPCDIndicator, 0, 100));

            _pm.AddParameter(new ModelParameterClass(eModelParam.epMeadDeadPool, "Lake Mead Dead Pool", "MDDPL", get_MeadDeadPool, 800, 1300));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epMeadLowerSNWA, "Lake Mead Lower SNWA Intake", "MDLIL", get_MeadSNWALowerIntake, 800, 1300));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epMeadShortageSharingLow, "Lake Mead Shortage Sharing Low", "MDSSL", get_MeadShortageSharingLevelLow, 800, 1300));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epMeadShortageSharingMed, "Lake Mead Shortage Sharing Med and Minimum Power Pool", "MDSSM", get_MeadShortageSharingLevelMed, 800, 1300));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epMeadShortageSharingHigh, "Lake Mead Shortage Sharing High", "MDSSH", get_MeadShortageSharingLevelHigh, 800, 1300));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epMeadMinPower, "Lake Mead Minimum Power Pool", "MDMPL", get_MeadMinPowerLevel, 800, 1300));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epMeadCapacity, "Lake Mead Capacity", "MDCPL", get_MeadCapacity, 800, 1300));
            //
            _pm.AddParameter(new ModelParameterClass(eModelParam.epRegGWpctInitial, "% of initial Regional GW", "SINGWPCT", get_PCT_RegGWfromInitial, 0, 150));

            //
            GW_PCTinitial = 63152280;
           // GW_PCTinitial = this.Regional_Groundwater_Balance;
            
            //
        }
    }
}
