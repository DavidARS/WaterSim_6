using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using WaterSimDCDC.Documentation;

namespace WaterSimDCDC
{
    public static partial class eModelParam
    {
        public const int epWebReclaimedOutdoorWater_PCT =126;
        public const int epWebReclaimedToInput_PCT = 127;
    }
    public partial class WaterSimManager
    {
        const string FileBuild = "02.09.17_16:07:00";
        //
        public int PCT_Agriculture = 0;
        public int PCT_FlowForEnvironment_memory = 0;
        public int PCT_Personal = 100;
        public int Local_Pop_Rate = 0;
        public int PCT_Augment = 0;
        public int AF_Augment = 0;
        public int Local_OutDoorWaterUse_Rate = 0;
        public int Local_ReclaimedOutdoorUse_Rate = 0;
        public int Local_ReclaimedtoInput_Rate = 0;
        public int Local_AgEfficiency = 100;
        //
        ProviderIntArray One = new ProviderIntArray(0);
        ProviderIntArray Two = new ProviderIntArray(0);
        ProviderIntArray Three = new ProviderIntArray(0);
        ProviderIntArray Four = new ProviderIntArray(0);
        //
        //
        int[] Add = new int[ProviderClass.NumberOfProviders];
        int[] Zero = new int[ProviderClass.NumberOfProviders];
        //
        public int AgToWebProcess = 0;
        //
        partial void initialize_WebInterface_DerivedParameters()
        {
            bool quiet = true;
            WaterSimManager WSim = (this as WaterSimManager);

            WaterSimDCDC.Processes.AlterGPCDFeedbackProcess alterGPCD = new WaterSimDCDC.Processes.AlterGPCDFeedbackProcess(WSim,quiet);
             WSim.ProcessManager.AddProcess(alterGPCD);

            ParameterManagerClass FPM = ParamManager;
            Extended_Parameter_Documentation ExtendDoc = FPM.Extended;
            //template is: ExtendDoc.Add(new WaterSimDescripItem(eModelParam., "Description", "Short Unit", "Long Unit", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));

            // Group Definitions
            ModelParameterGroupClass EffGroup = new ModelParameterGroupClass("WebEffluent Dependencies", new int[2] { eModelParam.epWebUIeffluent_Ag, eModelParam.epWebUIeffluent_Env });
            ParamManager.GroupManager.Add(EffGroup);
            //
            // Base Inputs
            // Policies in the WaterSim-Phoenix User Interface (as of 06.02.16, 07.21.16)
            // --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
            //  
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epWebReclaimedWater_PCT, " Regional % Reclaimed Wastewater", "REGRECEFF", rangeChecktype.rctCheckRange, 0, 100, geti_Web_Reclaimed_Water, seti_Web_Reclaimed_Water, RangeCheck.NoSpecialBase));
              ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWebReclaimedWater_PCT, "The transfer of waste water to the Reclaimed Waste Water Treatment Plant", "17-100", "0 low to 100 high",
               "% Waste Water for potable Urban use", new string[] {"0", "25", "50", "75", "100" }, new int[] {0, 25, 50, 75, 100 }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));

            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epWebUIAgriculture, "Ag Transfer To Muni", "WEBAGTR1", rangeChecktype.rctCheckRange, 0, 100, geti_WebAg, seti_WebAg, RangeCheck.NoSpecialBase, AgricultureGroup));
              ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWebUIAgriculture, "Transfers water rights from agriculture to urban water providers, based on a scale of 0 to 100, with 100 allocating more water to agriculture and 0 allocating less", "0-100", "0 low to 100 high",
                "% Farming Water Used for Urban", new string[] {"0", "25", "50", "75", "100" }, new int[] {0, 25, 50, 75, 100 }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));

            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epEnvironmentalFlow_AFa, "Environmental Flows", "ENFLOAMT", rangeChecktype.rctCheckRange, 0, 158088, geti_FlowEnvironment_AF, seti_FlowEnvironment_AF, RangeCheck.NoSpecialBase));
              ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epEnvironmentalFlow_AFa, "An absolute amount of CAP water to send to the CO delta: Arizona's share.", "AF a-1", "Acre-feet per annum", 
                "CO River diverted", new string[] {  }, new int[] {  }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));

            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epEnvironmentalFlow_PCT, " Water for Environment", "ENFLOPCT", rangeChecktype.rctCheckRange, 0, 100, geti_FlowEnvironment, seti_FlowEnvironment, RangeCheck.NoSpecialBase));
              ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epEnvironmentalFlow_PCT, "The Percentage of the AZ share of 158,088 AF of CO River water from CAP to send to the Delta.", "%", "Percent of AZ Share",
               "CO River Water Diverted", new string[] {"0", "25", "50", "75", "100" }, new int[] {0, 25, 50, 75, 100 }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));

            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epWebUIPersonal_PCT, "Personal Water Use", "WEBPRPCT", rangeChecktype.rctCheckRange, 0, 100, geti_WebPersonal, seti_WebPersonal, RangeCheck.NoSpecialBase));
              ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWebUIPersonal_PCT, "Adjusts the Gallons per Capita per Day for all water providers on a scale of 20 to 100, with zero a decline to 50 GPCD and 100 a decline based on past trends.", "0-100", "0-Repid decline to 100-Current trends",
               "% Change in Per Capita Water Use", new string[] {"0", "25", "50", "75", "100" }, new int[] {0, 25, 50, 75, 100 }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup, Policy_Demand_Group }));

            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epWebPop_GrowthRateAdj_PCT, "Adjust Pop Growth Rate", "WEBPOPGR", rangeChecktype.rctCheckRange, 0, 150, geti_WebPop, seti_WebPop, RangeCheck.NoSpecialBase));
              ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWebPop_GrowthRateAdj_PCT, "Adjustes the population growth rate for all water providers on a scale of 0-150, with 0-no growth to 150-50% increase in growth rate", "0-150", "0-0% to 150-150%",
               "% Change in Projected Growth Rate", new string[] {"0","50","100","125","150" }, new int[] {0,50,100,125,150 }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup, PopulationGroup, Policy_Demand_Group }));
            // 11.14.16 DAS
              this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epWebOutDoorUseRes_PCT, "Adjust Outdoor Water Use for Residential", "WEBOUTDOOR", rangeChecktype.rctCheckRange, 0, 100, geti_WebOutDoorWaterUse, seti_WebOutDoorWaterUse, RangeCheck.NoSpecialBase));
              ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWebOutDoorUseRes_PCT, "Adjusts the Outdoor Water Use for Residential Water Customers of 0-100, with 0-no use to 100", "0-100", "0-0% to 100",
               "% Change in Outdoor Water Use", new string[] { "0", "25", "50", "75", "100" }, new int[] { 0, 25, 50, 75, 100 }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup,  Policy_Demand_Group }));
            // 11.15.16 DAS
              this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epWebAgEfficiency_PCT, "Adjust Agricultural Efficiency", "WEBAGEFF", rangeChecktype.rctCheckRange, 20, 100, geti_AgEfficiency, seti_AgEfficiency, RangeCheck.NoSpecialBase));
              ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWebAgEfficiency_PCT, "Adjust Agricultural Efficiency where 100 Percent is current efficiency (2015)", "100-200", "100% to 300%",
               "% Change in the Agricultural Efficiency", new string[] { "20", "40", "60", "80", "100" }, new int[] { 20, 40, 60, 80, 100 }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup, Policy_Demand_Group }));
            //
              this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epWebReclaimedOutdoorWater_PCT, "Reclaimed Outdoor Water Use", "WEBRECOUT", rangeChecktype.rctCheckRange, 0, 100, geti_WebReclaimedOutDoorWaterUse, seti_WebReclaimedOutDoorWaterUse, RangeCheck.NoSpecialBase));
              ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWebReclaimedOutdoorWater_PCT, "Reclaimed Water Use for Outdoor Irrigation; 0-100, with 0-no use to 100", "0-100", "0-0% to 100",
               "% Change Reclaimed Water Use Outdoors", new string[] { "0", "25", "50", "75", "100" }, new int[] { 0, 25, 50, 75, 100 }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup, Policy_Demand_Group }));
            //
              this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epWebReclaimedToInput_PCT, "Reclaimed To Input", "WEBRECIN", rangeChecktype.rctCheckRange, 0, 100, geti_WebReclaimedToInputPCT, seti_WebReclaimedToInputPCT, RangeCheck.NoSpecialBase));
              ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWebReclaimedToInput_PCT, "Reclaimed to Water Supply", "%", "Percent",
               "Reclaimed Water Used", new string[] { "0", "25", "50", "75", "100" }, new int[] { 0, 25, 50, 75, 100 }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup, Policy_Demand_Group }));



            // -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
            // Other Inputs NOT CURRENTLY USED
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epWebUIeffluent, "Effluent: reuse/return", "WEBEFPCT", rangeChecktype.rctCheckRange, 0, 100, geti_WebEffluent, seti_WebEffluent, RangeCheck.NoSpecialBase, EffGroup));
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epWebUIeffluent_Ag, "Effluent for Ag", "WEBEFAG", rangeChecktype.rctCheckRange, 0, 100, geti_WebEffluent_Ag, seti_WebEffluent_Ag, RangeCheck.NoSpecialBase));
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epWebUIeffluent_Env, "Effluent for Environment", "WEBEFENV", rangeChecktype.rctCheckRange, 0, 100, geti_WebEffluent_Env, seti_WebEffluent_Env, RangeCheck.NoSpecialBase));
            //
            // Water Bank Parameter Web
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epWebWaterBank_PCT, "Store PCT Excess In Bank", "WEBWBPCT", rangeChecktype.rctCheckRange, 0, 100, geti_WebWaterBankPCT , seti_WebWaterBankPCT, RangeCheck.NoSpecialBase));
            //
            // Water Augmentation Parameter Web
            // 10.05.15 DAS I chenged the maximum to 25%: was 100% but even at 50%, phoenix exceeded the maxim of 100k set in Water_Augmentation
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epWebAugmentation_PCT, "Augment Water Supply PCT", "WEBAUGPCT", rangeChecktype.rctCheckRange, 0, 25, geti_WebAugmentPCT, seti_WebAugmentPCT, RangeCheck.NoSpecialBase));
            // ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
            //
            Web_Parms_setDefaultValues();
            //
        }
        #region Defaults
        /// <summary>
        ///  Default parameter values
        /// </summary>
        private void Web_Parms_setDefaultValues()
        {
            //Web_AgricultureTransferToMuni_PCT = 100;
            Web_FlowForEnvironment_PCT = 0;
            // quay edit
            PCT_FlowForEnvironment_memory = 0;
            // DAS edit
            Web_Personal_PCT = 100;
             PCT_Personal = Web_Personal_PCT;
            //
            Web_PopulationGrowthRate_PCT = 100;
            // Default - ADWR groundwater pumping estimate by 2085 target
            Web_AgricultureTransferToMuni = 30;
            //
            setDefaultGPCD();
            //
            API_Scenario_Presets = 0;
            //
            Local_OutDoorWaterUse_Rate = 100;
            // this.ParamManager.Model_Parameter("WEBOUTDOOR").Value = 100;
            //
            AgEfficiency = 100;
          
        }
        // 07.20.16 DAS - this needs to be envoked to utilize the default slope estimates
        void setDefaultGPCD()
        {
            // 07.19.16 DAS 
            for (int i = 0; i < ProviderClass.NumberOfProviders; i++)
            {
                API_defaultSlope_GPCD[i] = modSlope[i];
            }

        }
        // end 07.20.16 DAS
        #endregion Defaults
        //------------------------------------------------------------------------

        #region Web gets and sets

        /// Web Interface
        /// DAS 02.06.14 
        //------------------------------------------------------------------------
        private int geti_FlowEnvironment()
        { return Web_FlowForEnvironment_PCT; }
        private void seti_FlowEnvironment(int value)
        {
            if (!FModelLocked)
            {
                _pm.CheckBaseValueRange(eModelParam.epEnvironmentalFlow_PCT, value);
                WaterForDelta(value);
                Web_FlowForEnvironment_PCT = value;
            }
        }
        //------------------------------------------------------------------------
        private int geti_FlowEnvironment_AF()
        { return Web_FlowForEnvironment_AF; }
        private void seti_FlowEnvironment_AF(int value)
        {
            _pm.CheckBaseValueRange(eModelParam.epEnvironmentalFlow_AFa, value);
            Web_FlowForEnvironment_AF = value;
        }
        //------------------------------------------------------------------------

        //------------------------------------------------------------------------
        /// <summary>
        /// Effluent use for Web Interface
        /// </summary>
        public int Web_Effluent_PCT
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epWebUIeffluent, value);
                    seti_WebEffluent(value);
                    CoupleEffluentToAPI(value);

                }
                // ELSE do we throw an exception? No Just document that it is ignored
            }
            get { return geti_WebEffluent(); }
        }
        //------------------------------------------------------------------------
        private void CoupleEffluentToAPI(int mod)
        {
            int caseSwitch = 0;

            Effluent_array_11();
            caseSwitch = Effluent_Ag_CaseSwitch_11(mod);

            for (int i = 0; i < ProviderClass.NumberOfProviders; i++)
            {
                temp[i] = Web_Effluent[caseSwitch, 3];
            }
            Temp.Values = temp;
            PCT_Wastewater_to_Effluent.setvalues(Temp);
        }
        // Need a control in the Browser that limits the value to a maximum of 80
        // (the control will only GO to 80 %)
        // 02/18/14
        //ProviderIntArray Web_AgricultureTransferToMuni = new ProviderIntArray();
        //public ProviderIntArray Web_AgricultureTransferToMuni;
       // public providerArrayProperty Web_AgricultureTransferToMuni ;
        public int Web_AgricultureTransferToMuni_PCT
        {
            set
            {
                if (!FModelLocked)
                {
                    _pm.CheckBaseValueRange(eModelParam.epWebUIAgriculture, value);
                    seti_WebAg(value);
                }
                // ELSE do we throw an exception? No Just document that it is ignored
            }
            get { return geti_WebAg(); }
        }
        //------------------------------------------------------------------------
        public int Web_Personal_PCT
        {
            set
            {
                if (value < 20) value = 20;
                if (!FModelLocked)
                {
                    _pm.CheckBaseValueRange(eModelParam.epWebUIPersonal_PCT, value);
                    PCT_Personal = value;
                    seti_WebPersonal(value);
                }
                // ELSE do we throw an exception? No Just document that it is ignored
            }
            get { return geti_WebPersonal(); }
        }
        // 
        //------------------------------------------------------------------------
        public int Web_PopulationGrowthRate_PCT
        {
            set
            {
                int scaled = 0;
                if (!FModelLocked)
                {
                    // zero to 150 ONLY!
                    _pm.CheckBaseValueRange(eModelParam.epWebPop_GrowthRateAdj_PCT, value);
                    scaled = annual(value);
                    seti_WebPop(scaled);
                    Local_Pop_Rate = value;
                }
                // ELSE do we throw an exception? No Just document that it is ignored
            }
            get { return geti_WebPop(); }
        }
        //------------------------------------------------------------------------
        public int Web_WaterBankPercent
        {
            set
            {
                seti_WebWaterBankPCT(value);
            }
            get
            {
                return geti_WebWaterBankPCT();
            }
        }

        //---------------------------------------
        private int annual(int rateIn)
        {
            int Rate = 0;
            double percent = rateIn;
            return Rate = Convert.ToInt32(percent);
        }
        #endregion
        //public const int epEnvironmentalFlow_PCT = 250;
        //public const int epEnvironmentalFlow_AFa = 251;
        //public const int epWebUIeffluent = 252;
        //public const int epWebUIAgriculture = 253;
        //public const int epWebUIPersonal_PCT = 606;
        //public const int epWebUIAgriculture = 255;

        #region WebReclained
        private int CalcNewReclaimedWaterValue(int value)
        {
            int newVal = 0;
            int defaultCnt = 0;
            int defaultTot = 0;
            int NewTotal = 0;

            // calculate the total and count for those with default
            for (int i = 0; i < Default_Wastewater_Reclaimed.Length; i++)
            {
                if (Default_Wastewater_Reclaimed[i] > value)
                {
                    defaultCnt++;
                    defaultTot += Default_Wastewater_Reclaimed[i];
                }
            }
            int NewCnt = Default_Wastewater_Reclaimed.Length - defaultCnt;

            NewTotal = (value * Default_Wastewater_Reclaimed.Length) - defaultTot;
            newVal = NewTotal / NewCnt;
            newVal = Math.Max(0, NewTotal / NewCnt);
            return newVal;
        }
        // 11.11.16 DAS added TestMe here
        //ProviderIntArray TestMe = new ProviderIntArray(0);
        private void seti_Web_Reclaimed_Water(int value)
        {
            ProviderIntArray New_PCT_Wastewater_Reclaimed = new ProviderIntArray();

            int TheNewValueforNonDefaults = CalcNewReclaimedWaterValue(value);

            for (int i = 0; i < New_PCT_Wastewater_Reclaimed.Length; i++)
            {
                if (value > Default_Wastewater_Reclaimed[i])
                {
                    New_PCT_Wastewater_Reclaimed[i] = TheNewValueforNonDefaults;
                }
                else
                {
                    New_PCT_Wastewater_Reclaimed[i] = Default_Wastewater_Reclaimed[i];
                }
            }
            PCT_Wastewater_Reclaimed.setvalues(New_PCT_Wastewater_Reclaimed);
            ProviderIntArray TestMe = PCT_Wastewater_Reclaimed.getvalues();
            //TestMe = PCT_Wastewater_Reclaimed.getvalues();
        }

        private int geti_Web_Reclaimed_Water()
        {
            int Total = 0;
            ProviderIntArray TestMe = PCT_Wastewater_Reclaimed.getvalues();

            int L = TestMe.Length;
            for (int i = 0; i < L; i++)
            {
                Total += TestMe[i];
            }

            //return Total / L;
            // DAS 09.29.16
            // This parameter was returning to the UI a value one less than that being sent
            int temp=(Total / L)+1;
            return Math.Min(100,temp);
        }

        #endregion
        #region Agriculture




        /// <summary>
        /// Agriculture
        /// </summary>
        /// <returns></returns>
        /// 
      
        public int geti_WebAg()
        {
             return PCT_Agriculture;
        }
        //const double slope = -0.2;
        //const double intercept = 30; // 29
        const double slope = 0.3;
        const double intercept = 0; //  -0.6; // 0
        //const double convertSlope = -1;
        //const double convertIntercept = 150;
        int newValue = 0;
        //int Check = 0;
        int myReturnValue = 0;
        //
        // new slope = 0.31; no intercept
        //
        private void seti_WebAg(int value)
        {
            // Why am I using maximum pumping and maximum surface? Check this and comment the code
            if (!FModelLocked)
            {
                // This is the Decision Game Override to the Model
              
                    myReturnValue = value;
                    //    02.24.15 DAS
                    //    This converts the original scale of 50% to 150% 
                    // (50% was hightest transfer, 100 was default, 150 was no transfer)
                    // to 0% to 100% i.e., 0% is no transfer, 100% is 50% of default or maximum transfer
                    // ----------------------------------------------------------------------------------------------------
                    //newValue = Convert.ToInt32(convertSlope * value + convertIntercept);
                    newValue = Convert.ToInt32(slope * value + intercept);
                 //   newValue = 9;
                    _ws.set_AgPumpingCurveIndex = newValue;
                    //
                     // The percentage of credits that can be transfered from Ag- How much ag (in the future) do we leave?
                    _ws.set_AgCreditTransferPCT = 80;
                    _ws.set_AgCreditTransferPCT = 100;
                    //
              
            }
            PCT_Agriculture = myReturnValue;
        }
        /// <summary>
        ///  Transform 50% to 100% into an index scaled from 20 to zero;
        /// </summary>
        public int Web_AgricultureTransferToMuni
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    seti_WebAg(value);
                }
            }
            get
            {
                return geti_WebAg();
            }
        }

        #endregion
        #region Environment
        /// <summary>
        /// Flows for the Environment left ON RIVER - for the CO RIver Delta (as of 01.20.15)
        /// Modified on 07.20.16 DAS
        /// NEED TO SET a default value
        /// 
        /// 08.29.16 changed
        /// </summary>
        public int Web_FlowForEnvironment_PCT
        {
            set
            {
               PCT_FlowForEnvironment_memory = value;
            }
            get
            {
                int returned = COdeltaBurdenRatioForAZ;
                return returned; //PCT_FlowForEnvironment_memory;
            }
        }
        //
        // THIS is the NEW web control variable for the User Interface
        // 02.21.15 DAS
        //
        public int Web_FlowForEnvironment_AF
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epEnvironmentalFlow_AFa, value);
                    _ws.set_FlowToCOdelta = Convert.ToInt32(value);
                 }
            }
            get
            {
              
              return PCT_FlowForEnvironment_memory;
              
            }
        }

        /// <summary>
        /// New Code as of 01.20.15 based on conversations with Ray last week. We are going to use
        /// only Colorado River water, and only water for the Delta. The % in the indicator is simply
        /// going to reflect what percent of the total estimated, needed, is provided for the delta.
        /// The current estimate is: 158,088 acre-feet (195 million cubic meters) 
        /// Reference:  http://ibwc.state.gov/Files/Minutes/Minute_319.pdf
        int EnvironmentOut = 0;
        string FlowDiversion = " Flow diversion Excess";
        internal const double total=158088;
        const double convertToDecmal = 0.01;
        // Units Acre-feet per annum-1
        double Value = 0;

        public void WaterForDelta(int value)
        {
             // Maximum returnable
          
            if (!FModelLocked)
            {
                //
                try
                {
                    Value = value *convertToDecmal* total;
                    Web_FlowForEnvironment_AF = Convert.ToInt32(Value);
                    PCT_FlowForEnvironment_memory = value;
                }
                catch (Exception e)
                {
                    FlowDiversion = e.Message;
                }
                EnvironmentOut = Convert.ToInt32(Value);
            }
        }
     
        #endregion
        #region Personal
        // 10.01.15 DAS
        // October 2015 - in response to a re-evaluation of the GPCD estimates
        // brought upon by the simulations needed for the Gober et al. Paper 
        // being written
        // From User_Mods_2 move to this file
        // 09.02.15 DAS
        
       
        double[] _Reduce_GPCD = new double[ProviderClass.NumberOfProviders];
        double[] _Slope_GPCD = new double[ProviderClass.NumberOfProviders];
        double[] _Intercept_GPCD = new double[ProviderClass.NumberOfProviders];
        double[] _DefaultSlope_GPCD = new double[ProviderClass.NumberOfProviders];
        int[] _Target_GPCD = new int[ProviderClass.NumberOfProviders];
        int _BaseYear = 2015;
        
        public double[] API_Reduce_GPCD
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _Reduce_GPCD = value;
                }
            }
            get { return _Reduce_GPCD; }    // 
        }
        public double[] API_Slope_GPCD
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _Slope_GPCD = value;
                }
            }
            get { return _Slope_GPCD; }    // 
        }
        public double[] API_Intercept_GPCD
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _Intercept_GPCD = value;
                }
            }
            get { return _Intercept_GPCD; }    // 
        }
        public int[] API_Target_GPCD
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _Target_GPCD = value;
                }
            }
            get { return _Target_GPCD; }    // 
        }
        public double[] API_defaultSlope_GPCD
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _DefaultSlope_GPCD = value;
                }
            }
            get { return _DefaultSlope_GPCD; }    // 
        }


           // 
            ProviderIntArray New_slope = new ProviderIntArray(0);
            int[] ReduceGPCD = new int[ProviderClass.NumberOfProviders];
            double[] dNewTarget = new double[ProviderClass.NumberOfProviders];
            double[] Result = new double[ProviderClass.NumberOfProviders];
            int[] addDefault = new int[ProviderClass.NumberOfProviders];
            double[] gpcd2013 = new double[ProviderClass.NumberOfProviders];
            double[] gpcdRAW = new double[ProviderClass.NumberOfProviders];
            //
            private int geti_WebPersonal() { return PCT_Personal; }
            private void seti_WebPersonal(int value)
            {
                // 09.30.16 Sampson - We decided a minimum value was warranted
                // This sets it to 20 even if the user sets it lower in the UI
                if (value < 20)value = 20;
               // END 09.30.16 Sampson
                Personal(value);
                // ADDED QUAY 3/9/2014
                PCT_Personal = value;
                //--------------
            }
            private void Personal(int Switch)
            {
                //
                int year = _CurrentYear;
                //Switch = 40;
                double[] dReduce = new double[ProviderClass.NumberOfProviders];
                //
              
                //    
                 switch (Switch)
                {
   
                    case 100:
                        Provider_Demand_Option = 3;
                        for (int i = 0; i < ProviderClass.NumberOfProviders; i++)
                        {
                            API_Reduce_GPCD[i] = 1 ;
                        }
                         break;
                    default:
                         Provider_Demand_Option = 4;
                         // das 09.13.16
                         // 09.19.16 das - only called once now in the new webservice....
                         if (Switch < 20) Switch = 20;
                         PCT_Personal = Switch;
                            for (int i = 0; i < ProviderClass.NumberOfProviders; i++)
                            {
                                ReduceGPCD[i] = 99;
                                if(0 < Switch)
                                ReduceGPCD[i] = (100 - Switch);
                           }
                       
               // send data to Process_AlterGPCD.cs
                 for (int i = 0; i < ProviderClass.NumberOfProviders; i++)
                 {
                     API_Reduce_GPCD[i] = 1 - (Convert.ToDouble(ReduceGPCD[i]) * 1 / 100);
                     API_Slope_GPCD[i] = SlopeGPCD[i];
                     API_Intercept_GPCD[i] = InterceptGPCD[i];
                     API_Target_GPCD[i] = defaultTargetGPCD[i];
                     API_defaultSlope_GPCD[i] =  modSlope[i];
                     gpcdRAW[i] = GPCD_raw.getvalues().Values[i];
                 }
                 break;
                }
  
            }
            // DAS 06.12.15
        #endregion
        #region Population
        /// <summary>
        ///  Population from the Web UI to impact model parameter, i.e., in WaterSimDCDC_API_ver_7_?.cs
        ///    //int[] NewRates = new int[ProviderClass.NumberOfProviders];
        // for (int i = 0; i < NewRates.Length; i++)
        //    NewRates[i] = value;
        //_ws.ProviderPopGrowthRateOnProjectPct = NewRates;
        //_ws.ProviderPopGrowthRateOtherPct = NewRates;

        /// </summary>
        /// <returns></returns>
        private int geti_WebPop()
        {
            int Rate = 0;
            // QUAY EDIT BEGIN 3 10 14
            //if (!FModelLocked)
            // {
            Rate = Local_Pop_Rate;
            //}
            // QUAY EDIT END 3 10 14
            return Rate;
        }
        private void seti_WebPop(int value)
        {
            Local_Pop_Rate = value;
            if (!FModelLocked)
            {
                int[] NewRates = new int[ProviderClass.NumberOfProviders];
                for (int i = 0; i < NewRates.Length; i++)
                    // 07.20.16 DAS added because a value of zero gives nonsensical numbers
                    // check FORTRAN model for a possible fix
                    if (0 < value)
                    {
                        NewRates[i] = value;
                    }
                    else
                    {
                        NewRates[i] = value+1;
                    }
                    // end 07.20.16 DAS
                _ws.ProviderPopGrowthRateAdjustPct = NewRates;
            }

        }
        // =============================================================
        // Control Outdoor water use
        // 11.15.16 DAS
        private int geti_WebOutDoorWaterUse()
        {
            int Use = 0;
            // DAS EDIT BEGIN 11.14.16
            //if (!FModelLocked)
            // {
            Use = Local_OutDoorWaterUse_Rate;
            //}
            // DAS EDIT END 11.14.16
            return Use;
        }
        private void seti_WebOutDoorWaterUse(int value)
        {
            ProviderIntArray New_PCT_OutDoorWaterUse = new ProviderIntArray();
            ProviderIntArray TestMe = PCT_Outdoor_WaterUseRes.getvalues();
            int[] Rates = new int[ProviderClass.NumberOfProviders];
            double Value = value*0.01;
            Local_OutDoorWaterUse_Rate = value;
            if (!FModelLocked)
            {
                int[] NewRates = new int[ProviderClass.NumberOfProviders];
                for (int i = 0; i < NewRates.Length; i++)
                {
                    New_PCT_OutDoorWaterUse[i] = Convert.ToInt32(Convert.ToDouble(TestMe[i]) *  Value);
                    Rates[i] = Convert.ToInt32(Convert.ToDouble(TestMe[i]) * Value); 
                }
                PCT_Outdoor_WaterUseRes.setvalues(New_PCT_OutDoorWaterUse);
               
             //   _ws.parmOutdoorWaterUseResPct = Rates;
            }
        }
        // ===========================================================================================
        //
        // Control Outdoor water use
        // 11.15.16 DAS
        private int ReclaimedOutdoorUse
        {
            set { Local_ReclaimedOutdoorUse_Rate = value;}
            get { return Local_ReclaimedOutdoorUse_Rate; }

        }
        private int geti_WebReclaimedOutDoorWaterUse()
        {
            int Rate = 0;
            Rate = ReclaimedOutdoorUse;
            return Rate;
        }
        private void seti_WebReclaimedOutDoorWaterUse(int value)
        {
            ProviderIntArray New_PCT_ReclaimedOutDoorWaterUse = new ProviderIntArray();
            ProviderIntArray TestMe = PCT_Reclaimed_Outdoor_Use.getvalues();
            int[] Rates = new int[ProviderClass.NumberOfProviders];
            double Value = value * 0.01;
            Local_ReclaimedOutdoorUse_Rate = value;
            if (!FModelLocked)
            {
                int[] NewRates = new int[ProviderClass.NumberOfProviders];
                for (int i = 0; i < NewRates.Length; i++)
                {
                    New_PCT_ReclaimedOutDoorWaterUse[i] = Convert.ToInt32(Convert.ToDouble(TestMe[i]) * Value);
                    Rates[i] = Convert.ToInt32(Convert.ToDouble(TestMe[i]) * Value);
                }
                PCT_Reclaimed_Outdoor_Use.setvalues(New_PCT_ReclaimedOutDoorWaterUse);

               
            }
        }
        //------------------------------------------------------------------------
        public int Web_ReclaimedOutdoorUse_PCT
        {
            set
            {
                int scaled = 0;
                if (!FModelLocked)
                {
                    // zero to 150 ONLY!
                    _pm.CheckBaseValueRange(eModelParam.epWebReclaimedOutdoorWater_PCT, value);
                    scaled = annual(value);
                    seti_WebReclaimedOutDoorWaterUse(scaled);
                    Local_ReclaimedOutdoorUse_Rate = value;
                }
                // ELSE do we throw an exception? No Just document that it is ignored
            }
            get { return geti_WebReclaimedOutDoorWaterUse(); }
        }
        //------------------------------------------------------------------------
        private int ReclaimedToInput
        {
            set { Local_ReclaimedtoInput_Rate = value; }
            get { return Local_ReclaimedtoInput_Rate; }

        }

        private int geti_WebReclaimedToInputPCT()
        {
            int Rate = 0;
            Rate = ReclaimedToInput;
            return Rate;
        }
        private void seti_WebReclaimedToInputPCT(int value)
        {
            ProviderIntArray New_PCT_ReclaimedToInputUse = new ProviderIntArray();
            ProviderIntArray TestMe = PCT_Reclaimed_to_Water_Supply.getvalues();
            int[] Rates = new int[ProviderClass.NumberOfProviders];
            double Value = value * 0.01;
            ReclaimedToInput = value;
            if (!FModelLocked)
            {
                int[] NewRates = new int[ProviderClass.NumberOfProviders];
                for (int i = 0; i < NewRates.Length; i++)
                {
                    New_PCT_ReclaimedToInputUse[i] = Convert.ToInt32(Convert.ToDouble(TestMe[i]) * Value);
                    Rates[i] = Convert.ToInt32(Convert.ToDouble(TestMe[i]) * Value);
                }
                PCT_Reclaimed_to_Water_Supply.setvalues(New_PCT_ReclaimedToInputUse);


            }
        }





        // ====================================================================
        // Set Agricultural Efficiency
        //
        private int geti_AgEfficiency()
        {
          return AgEfficiency;
        }
        private void seti_AgEfficiency(int value)
        {
            AgEfficiency = value;
            _ws.set_AgEfficiency = value;
        }
        private int AgEfficiency
        {
            set { Local_AgEfficiency = value; }
            get { return Local_AgEfficiency;}
        }
        // ====================================================================
        #endregion
        // Extra Code NOT now used
        // -----------------------
        #region Effluent
            int[] Web_Effluent_Vadose = new int[ProviderClass.NumberOfProviders];
            int[] Web_Effluent_Agriculture = new int[ProviderClass.NumberOfProviders];
            int[] Web_Effluent_Environment = new int[ProviderClass.NumberOfProviders];
            //
            const int array = 11;
            const int Mycase = 5;
            int[,] Web_Effluent = new int[array, Mycase];
            int[] Web_Reclaimed = new int[array];
            //
            #region array
            private void Effluent_array_6()
            {
                // Case, 0=Env, 1=Ag, 2=Vadose, 3=Effluent PCT
                Web_Effluent[6, 0] = 100;
                Web_Effluent[6, 1] = 0;
                Web_Effluent[6, 2] = 0;
                Web_Effluent[6, 3] = 100 - Web_Effluent[6, 0];
                Web_Effluent[6, 4] = 100 - Web_Effluent[6, 1];
                Web_Reclaimed[6] = 0;
                //
                Web_Effluent[5, 0] = 75;
                Web_Effluent[5, 1] = 20;
                Web_Effluent[5, 2] = 5;
                Web_Effluent[5, 3] = 100 - Web_Effluent[5, 0];
                Web_Effluent[5, 4] = 100 - Web_Effluent[5, 1];
                Web_Reclaimed[5] = 20;
                //
                Web_Effluent[4, 0] = 50;
                Web_Effluent[4, 1] = 40;
                Web_Effluent[4, 2] = 10;
                Web_Effluent[4, 3] = 100 - Web_Effluent[4, 0];
                Web_Effluent[4, 4] = 100 - Web_Effluent[4, 1];
                Web_Reclaimed[4] = 40;
                //
                Web_Effluent[3, 0] = 25;
                Web_Effluent[3, 1] = 60;
                Web_Effluent[3, 2] = 15;
                Web_Effluent[3, 3] = 100 - Web_Effluent[3, 0];
                Web_Effluent[3, 3] = 100 - Web_Effluent[3, 1];
                Web_Reclaimed[3] = 60;
                //
                Web_Effluent[2, 0] = 10;
                Web_Effluent[2, 1] = 80;
                Web_Effluent[2, 2] = 10;
                Web_Effluent[2, 3] = 100 - Web_Effluent[2, 0];
                Web_Effluent[2, 4] = 100 - Web_Effluent[2, 1];
                Web_Reclaimed[2] = 80;
                //
                Web_Effluent[1, 0] = 0;
                Web_Effluent[1, 1] = 95;
                Web_Effluent[1, 2] = 5;
                Web_Effluent[1, 3] = 100 - Web_Effluent[1, 0];
                Web_Effluent[1, 3] = 100 - Web_Effluent[1, 1];
                Web_Reclaimed[1] = 95;
                //
                Web_Effluent[0, 0] = 0;
                Web_Effluent[0, 1] = 100;
                Web_Effluent[0, 2] = 0;
                Web_Effluent[0, 3] = 100 - Web_Effluent[0, 0];
                Web_Effluent[0, 4] = 100 - Web_Effluent[0, 1];
                Web_Reclaimed[0] = 100;
            }
            private void Effluent_array_11()
            {
                // Case, 0=Env, 1=Ag, 2=Vadose, 3=Effluent PCT

                Web_Effluent[10, 0] = 100;
                Web_Effluent[10, 1] = 0;
                Web_Effluent[10, 2] = 0;
                Web_Effluent[10, 3] = 100 - Web_Effluent[10, 0];
                Web_Effluent[10, 4] = 0;
                Web_Reclaimed[10] = 0;

                Web_Effluent[9, 0] = 80;
                Web_Effluent[9, 1] = 10;
                Web_Effluent[9, 2] = 10;
                Web_Effluent[9, 3] = 100 - Web_Effluent[9, 0];
                Web_Effluent[9, 4] = 100 - Web_Effluent[9, 1];
                Web_Reclaimed[9] = 10;

                Web_Effluent[8, 0] = 70;
                Web_Effluent[8, 1] = 20;
                Web_Effluent[8, 2] = 10;
                Web_Effluent[8, 3] = 100 - Web_Effluent[8, 0];
                Web_Effluent[8, 4] = 100 - Web_Effluent[8, 1];
                Web_Reclaimed[8] = 20;

                Web_Effluent[7, 0] = 55;
                Web_Effluent[7, 1] = 30;
                Web_Effluent[7, 2] = 15;
                Web_Effluent[7, 3] = 100 - Web_Effluent[7, 0];
                Web_Effluent[7, 4] = 100 - Web_Effluent[7, 1];
                Web_Reclaimed[7] = 30;

                Web_Effluent[6, 0] = 45;
                Web_Effluent[6, 1] = 40;
                Web_Effluent[6, 2] = 15;
                Web_Effluent[6, 3] = 100 - Web_Effluent[6, 0];
                Web_Effluent[6, 4] = 100 - Web_Effluent[6, 1];
                Web_Reclaimed[6] = 40;
                //
                Web_Effluent[5, 0] = 35;
                Web_Effluent[5, 1] = 50;
                Web_Effluent[5, 2] = 15;
                Web_Effluent[5, 3] = 100 - Web_Effluent[5, 0];
                Web_Effluent[5, 4] = 100 - Web_Effluent[5, 1];
                Web_Reclaimed[5] = 50;
                //
                Web_Effluent[4, 0] = 25;
                Web_Effluent[4, 1] = 60;
                Web_Effluent[4, 2] = 15;
                Web_Effluent[4, 3] = 100 - Web_Effluent[4, 0];
                Web_Effluent[4, 4] = 100 - Web_Effluent[4, 1];
                Web_Reclaimed[4] = 60;
                //
                Web_Effluent[3, 0] = 20;
                Web_Effluent[3, 1] = 70;
                Web_Effluent[3, 2] = 10;
                Web_Effluent[3, 3] = 100 - Web_Effluent[3, 0];
                Web_Effluent[3, 4] = 100 - Web_Effluent[3, 1];
                Web_Reclaimed[3] = 70;
                //
                Web_Effluent[2, 0] = 15;
                Web_Effluent[2, 1] = 80;
                Web_Effluent[2, 2] = 5;
                Web_Effluent[2, 3] = 100 - Web_Effluent[2, 0];
                Web_Effluent[2, 4] = 100 - Web_Effluent[2, 1];
                Web_Reclaimed[2] = 80;
                //
                Web_Effluent[1, 0] = 10;
                Web_Effluent[1, 1] = 90;
                Web_Effluent[1, 2] = 0;
                Web_Effluent[1, 3] = 100 - Web_Effluent[1, 0];
                Web_Effluent[1, 4] = 100 - Web_Effluent[1, 1];
                Web_Reclaimed[1] = 90;
                //
                Web_Effluent[0, 0] = 0;
                Web_Effluent[0, 1] = 100;
                Web_Effluent[0, 2] = 0;
                Web_Effluent[0, 3] = 100 - Web_Effluent[0, 0];
                Web_Effluent[0, 4] = 100 - Web_Effluent[0, 1];
                Web_Reclaimed[0] = 100;
            }

            private void count()
            {
                for (int p = 0; p < ProviderClass.NumberOfProviders; p++) { Add[p] = 0; }
                for (int p = 0; p < ProviderClass.NumberOfProviders; p++) { Zero[p] = 0; }
            }
            #endregion
            #region case switches
            private void CaseSwitch_5()
            {

            }
            private int EffluentCaseSwitch_11(int Switch)
            {
                // Case values are basd on the "return" variable for Environment
                // i.e., returnEnvironment (that is: Web_Effluent[*, 4] )
                int caseSwitch = 0;
                if (Switch >= 1 && Switch <= 5) { caseSwitch = 0; }
                if (Switch > 5 && Switch <= 15) { caseSwitch = 1; }
                if (Switch > 15 && Switch <= 25) { caseSwitch = 2; }
                if (Switch > 25 && Switch <= 35) { caseSwitch = 3; }
                if (Switch > 35 && Switch <= 45) { caseSwitch = 4; }
                if (Switch > 45 && Switch <= 55) { caseSwitch = 5; }
                if (Switch > 55 && Switch <= 65) { caseSwitch = 6; }
                if (Switch > 65 && Switch <= 75) { caseSwitch = 7; }
                if (Switch > 75 && Switch <= 85) { caseSwitch = 8; }
                if (Switch > 85 && Switch <= 95) { caseSwitch = 9; }
                if (Switch > 95) { caseSwitch = 10; }
                return caseSwitch;
            }
            private int Effluent_Ag_CaseSwitch_11(int Mod)
            {
                //int Switch = 100 - Mod;
                int Switch = Mod;
                int caseSwitch = 0;
                if (Switch >= 1 && Switch <= 5) { caseSwitch = 0; }
                if (Switch > 5 && Switch <= 15) { caseSwitch = 1; }
                if (Switch > 15 && Switch <= 25) { caseSwitch = 2; }
                if (Switch > 25 && Switch <= 35) { caseSwitch = 3; }
                if (Switch > 35 && Switch <= 45) { caseSwitch = 4; }
                if (Switch > 45 && Switch <= 55) { caseSwitch = 5; }
                if (Switch > 55 && Switch <= 65) { caseSwitch = 6; }
                if (Switch > 65 && Switch <= 75) { caseSwitch = 7; }
                if (Switch > 75 && Switch <= 85) { caseSwitch = 8; }
                if (Switch > 85 && Switch <= 95) { caseSwitch = 9; }
                if (Switch > 95) { caseSwitch = 10; }
                return caseSwitch;
            }
            private int Effluent_Env_CaseSwitch_11(int Switch)
            {

                int caseSwitch = 0;
                if (Switch >= 1 && Switch <= 5) { caseSwitch = 0; }
                if (Switch > 5 && Switch <= 12) { caseSwitch = 1; }
                if (Switch > 12 && Switch <= 17) { caseSwitch = 2; }
                if (Switch > 17 && Switch <= 22) { caseSwitch = 3; }
                if (Switch > 22 && Switch <= 30) { caseSwitch = 4; }
                if (Switch > 30 && Switch <= 40) { caseSwitch = 5; }
                if (Switch > 40 && Switch <= 50) { caseSwitch = 6; }
                if (Switch > 50 && Switch <= 63) { caseSwitch = 7; }
                if (Switch > 63 && Switch <= 75) { caseSwitch = 8; }
                if (Switch > 75 && Switch <= 90) { caseSwitch = 9; }
                if (Switch > 90) { caseSwitch = 10; }
                return caseSwitch;
            }

            //
            private int EffluentCaseSwitch_6(int Switch)
            {
                // Case values are basd on the "return" variable for Environment
                // i.e., returnEnvironment (that is: Web_Effluent[*, 4] )
                int caseSwitch = 0;
                if (Switch >= 1 && Switch <= 15) { caseSwitch = 1; }
                else if (Switch > 15 && Switch <= 30) { caseSwitch = 2; }
                else if (Switch > 30 && Switch <= 50) { caseSwitch = 3; }
                else if (Switch > 50 && Switch <= 70) { caseSwitch = 4; }
                else if (Switch > 70 && Switch <= 90) { caseSwitch = 5; }
                else if (Switch > 90) { caseSwitch = 6; }
                return caseSwitch;
            }
            private int Effluent_Ag_CaseSwitch_6(int Mod)
            {
                int Switch = 100 - Mod;
                // Case values are basd on the "return" variable for Environment
                // i.e., returnEnvironment (that is: Web_Effluent[*, 4] )
                int caseSwitch = 0;
                if (Switch >= 1 && Switch <= 15) { caseSwitch = 1; }
                else if (Switch > 15 && Switch <= 30) { caseSwitch = 2; }
                else if (Switch > 30 && Switch <= 50) { caseSwitch = 3; }
                else if (Switch > 50 && Switch <= 70) { caseSwitch = 4; }
                else if (Switch > 70 && Switch <= 90) { caseSwitch = 5; }
                else if (Switch > 90) { caseSwitch = 6; }
                return caseSwitch;
            }




            //       int caseSwitch = 0;
            //       if (Switch >= 1 && Switch <= 16) { caseSwitch = 1; }
            //else if (Switch > 16 && Switch <= 32) { caseSwitch = 2; }
            //else if (Switch > 32 && Switch <= 48) { caseSwitch = 3; }
            //else if (Switch > 48 && Switch <= 64) { caseSwitch = 4; }
            //else if (Switch > 64 && Switch <= 80) { caseSwitch = 5; }
            //else if (Switch > 80 && Switch <= 100) { caseSwitch = 6; }
            //return caseSwitch;   

            #endregion
            //
            private ProviderIntArray Temp;

            private ProviderIntArray REC;
            internal int agriculture = 0;
            internal int returnAgriculture = 0;
            internal int vadose = 0;
            internal int actualEnvironment = 0;
            internal int reclaimed = 0;
            internal int effluent = 0;
            internal int Effluent = 0;

            internal int returnEnvironment = 0;
            internal int returnEffluent = 0;

            //  internal int[] Env = new int[ProviderClass.NumberOfProviders];
            internal int[] Vad = new int[ProviderClass.NumberOfProviders];
            internal int[] Rec = new int[ProviderClass.NumberOfProviders];
            //internal int[] Total = new int[ProviderClass.NumberOfProviders];
            //
            internal int[] Get = new int[ProviderClass.NumberOfProviders];
            //  internal double[] Reclaimed = new double[ProviderClass.NumberOfProviders];
            //
            private int geti_WebEffluent() { return returnEffluent; }
            private void seti_WebEffluent(int value)
            {
                Set_Effluent(value);
            }
            private void Set_Effluent(int Switch)
            {
                Effluent_array_11();
                int caseSwitch = EffluentCaseSwitch_11(Switch);
                //
                count();
                //
                One.Values = Zero;
                PCT_Effluent_to_PowerPlant.setvalues(One);
                //
                REC = PCT_Wastewater_Reclaimed.getvalues();
                Get = PCT_Reclaimed_to_Water_Supply.getvalues().Values;
                //
                actualEnvironment = Web_Effluent[caseSwitch, 0];
                agriculture = Web_Effluent[caseSwitch, 1];
                vadose = Web_Effluent[caseSwitch, 2];
                effluent = Web_Effluent[caseSwitch, 3];

                returnEnvironment = Web_Effluent[caseSwitch, 4];

                reclaimed = Web_Reclaimed[caseSwitch];
                //
                setAPI();
                //               
                returnEffluent = Switch;
                returnAgriculture = 100 - returnEnvironment;
            }
            private void setAPI()
            {
                for (int p = 0; p < ProviderClass.NumberOfProviders; p++)
                {
                    Add[p] = agriculture + vadose;
                    Vad[p] = vadose;
                    Rec[p] = reclaimed;
                }
                One.Values = Add;
                PCT_Wastewater_to_Effluent.setvalues(One);
                Two.Values = Vad;
                PCT_Effluent_to_Vadose.setvalues(Two);
                Three.Values = Rec;
                PCT_Reclaimed_to_Water_Supply.setvalues(Three);
                Four.Values = Zero;
                PCT_Reclaimed_to_Vadose.setvalues(Four);
                PCT_Reclaimed_to_DirectInject.setvalues(Four);
                PCT_Reclaimed_to_RO.setvalues(Four);
                AgToWebProcess = agriculture;
            }
            // lower level controls for Effluent
            /// <summary>
            /// Sub control for Effluent One
            /// </summary>
            int[] temp = new int[ProviderClass.NumberOfProviders];

            public int Web_Effluent_Ag
            {
                set
                {
                    seti_WebEffluent_Ag(value);
                }
                get
                {
                    return geti_WebEffluent_Ag();
                }

            }
            private void seti_WebEffluent_Ag(int value)
            {
                Effluent_Agriculture(value);
            }
            private void Effluent_Agriculture(int Switch)
            {
                int caseSwitch = 0;
                agriculture = 0;
                Effluent_array_11();
                caseSwitch = Effluent_Ag_CaseSwitch_11(Switch);
                //
                agriculture = Web_Effluent[caseSwitch, 1];
                actualEnvironment = Web_Effluent[caseSwitch, 0];
                vadose = Web_Effluent[caseSwitch, 2];
                returnEnvironment = Web_Effluent[caseSwitch, 4];
                returnEffluent = Web_Effluent[caseSwitch, 4];
                returnAgriculture = Switch;
                //
                setAPI();
            }
            private int geti_WebEffluent_Ag()
            {
                return returnAgriculture; // agriculture;
            }
            public int Web_effluent_PCT
            {
                set
                {
                    effluent = value;
                }
                get
                {
                    return Web_effluent_PCT;
                }
            }
            //
            /// <summary>
            ///  Sub control for Effluent Two
            /// </summary>
            public int Web_Effluent_Env
            {
                set
                {
                    seti_WebEffluent_Env(value);
                }
                get
                {
                    return returnEnvironment;
                }

            }
            private void seti_WebEffluent_Env(int value)
            {
                Effluent_Environment(value);
            }
            private void Effluent_Environment(int Mod)
            {
                Effluent_array_11();
                int Switch = Effluent_Env_CaseSwitch_11(Mod);
                returnAgriculture = 100 - Mod;
                agriculture = Web_Effluent[Switch, 1];
                actualEnvironment = Web_Effluent[Switch, 0];
                vadose = Web_Effluent[Switch, 2];
                returnEnvironment = Mod; // Web_Effluent[Switch, 4];
                setAPI();
                returnEffluent = returnEnvironment;
            }
            private int geti_WebEffluent_Env()
            {
                return actualEnvironment;
            }


            //PCT_Effluent_to_Vadose
            // PCT_Wastewater_to_Effluent
            //
            #endregion
        #region WaterBankPercent

            ///-------------------------------------------------------------------------------------------------
            /// <summary>   Gets the geti web water bank Percent. </summary>
            /// <remarks> Designed for the Web UI, Summarizes all the provide WaterBanlk Pecent values to a regional value, using the regional aggregation</remarks>
            /// <returns>   . </returns>
            ///-------------------------------------------------------------------------------------------------
            /////Use_SurfaceWater_to_WaterBank
            //
            // start here on 10.05.15 das
            //
            int[] totalUnusedCAP = new int[ProviderClass.NumberOfProviders];
            private int CalcNewBankingWaterValue(int value)
            {
                int newVal = 0;
                int defaultCnt = 0;
                int defaultTot = 0;
                double tempPCT = 0;
                // calculate the total and count for those with default
                // amount
                for (int i = 0; i < Default_Banking.Length; i++)
                {
                    //if (0 < Default_Banking[i])
                    //{
                    //    // Percent
                    //    totalUnusedCAP[i] = CAP_Unused_PriorityFour.getvalues().Values[i] +
                    //        CAP_Unused_PriorityFive.getvalues().Values[i];
                    //    tempPCT = Default_Banking[i] / (Convert.ToDouble(value) / 100) * totalUnusedCAP[i];
                    //    // 
                    //    if (tempPCT > value)
                    //    {

                    //    }
                    //}
                }
                int NewCnt = Default_Banking.Length - defaultCnt;
                int NewTotal = (value * Default_Banking.Length) - defaultTot;
                newVal = NewTotal / NewCnt;
                return newVal;
            }

            public int geti_WebWaterBankPCT()
            {
                int regionvalue = 0;
                regionvalue = PCT_SurfaceWater_to_WaterBank[eProvider.Regional];
                return regionvalue;
            }

            ///-------------------------------------------------------------------------------------------------
            /// <summary>   Seti web water bank Percent. </summary>
            /// <remarks> Designed for the Web UI, Sets all the provider WaterBanlk Pecent values to a the value passed </remarks>
            /// <param name="value">    The value. </param>
            ///-------------------------------------------------------------------------------------------------

            public void seti_WebWaterBankPCT(int value)
            {
                CalcNewBankingWaterValue(value);
                ProviderIntArray PIA = new ProviderIntArray(value);
                PCT_SurfaceWater_to_WaterBank.setvalues(PIA);
                ProviderIntArray PITo1 = new ProviderIntArray(1);
                WaterBank_Source_Option.setvalues(PITo1);
            }

            #endregion
        #region Augmentation
        //
        // Start HERE 10.02.15 DAS
        
            public int geti_WebAugmentPCT()
            {
                int regionvalue = 0;
                regionvalue = PCT_Augment;
                return regionvalue;
            }
            public void seti_WebAugmentPCT(int value)
            {
             
                PCT_Augment = value;
                seti_WebAugmentAF(value);
            }
            //
            int[] newVal_AF =  new int[ProviderClass.NumberOfProviders];
            //
            private void seti_WebAugmentAF(int value)
            {
               
                CalcAugAsDemandWaterValue(value);
                // provider value
                ProviderIntArray New_Int = new ProviderIntArray(0);
                New_Int.Values=newVal_AF;
                WaterAugmentation.setvalues(New_Int);
                // regional value
                AF_Augment = geti_Web_Augmentation_Water();
            }
            private void CalcAugAsDemandWaterValue(int value)
            {
                double tempVal_AF = 0;
                double demand = 0;
                double proportion = Convert.ToDouble(value) / 100;
                //
                for (int i = 0; i < ProviderClass.NumberOfProviders; i++)
                {
                    int year = Sim_CurrentYear;
                    demand = Total_Demand[i];
                    if (year == 0)
                    {
                        tempVal_AF = proportion * demand;
                        newVal_AF[i] = Convert.ToInt32(tempVal_AF);
                    }
                    else
                    { //AF_Augment=0;
                        newVal_AF[i] = AF_Augment;
                    }

                  
                }
            
            }
             private int geti_Web_Augmentation_Water()
            {
                int Total = 0;
             
                int L = ProviderClass.NumberOfProviders;
                for (int i = 0; i < L; i++)
                {
                    Total += newVal_AF[i];
                }

                return Total / L;
            }

                
        #endregion
        // -----------------------

        /// Code parking
        /// 
        //internal int[] get_Use_D()
        //{ return _ws.parm; } 
        //internal void set_Use_D(int[] value)
        //{
        //    if (!FModelLocked)
        //    {
        //        _ws.parm = value;
        //    }
        //}

        // Outdoor Recalimed Water Use


        //PCEFFREC
        //PCT_Wastewater_Reclaimed
        //The percent of  Waste water effluent that is sent to a Reclaimed Plant (versus a traditional plant-see figure 1).
        // This muts be set first

        //PCRECOUT
        //PCT_Reclaimed_Outdoor_Use
        //The percent of  reclaimed water to be used outdoors. If all available reclaimed water is not used outdoors (i.e., not 100%) it is used indoors as black water.


        //PCRECRO
        //PCT_Reclaimed_to_RO)
        //The percent of  reclaimed water that is sent to a Reverse Osmosis Plant (thus becomming potable water for direct injection or potable water for use in the next time-step).

        //PCRECDI
        //PCT_Reclaimed_to_DirectInject
        //The percent of  reclaimed water that is used to recharge an aquifer by direct injection into an aquifer.

        //PCERECWS
        //PCT_Reclaimed_to_Water_Supply
        // The percent of  reclaimed water that is used to meet qualified user demands (non-potable).

        //PCRECVAD
        // PCT_Reclaimed_to_Vadose
        // The percent of  reclaimed water that is delivered to a vadoze zone recharge basin.

        //PCDEMREC
        //PCT_Max_Demand_Reclaim
        //The amount of (percent of demand that can be met by) reclaimed water that WILL be used as input for the next year.

    }

}
