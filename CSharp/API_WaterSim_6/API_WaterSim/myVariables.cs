using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using StreamOutputs;
using WaterSimDCDC;
using WaterSim;
namespace API_WaterSim
{
    public class myVariables : WaterSimU
    {
        internal FileOutputsBase FOB;
        //
        ModelParameterClass WAugParm;
        ModelParameterClass WWtoEff;
        ModelParameterClass WWtoReclaimed;
        ModelParameterClass RtoRO;
        //
        ProviderIntArray Out = new ProviderIntArray(0);
        internal int[] OneHundred = new int[ProviderClass.NumberOfProviders];
        internal int[] Ninty = new int[ProviderClass.NumberOfProviders];
        internal int[] SevenFive = new int[ProviderClass.NumberOfProviders];
        internal int[] Fifty = new int[ProviderClass.NumberOfProviders];
        internal int[] ThirtyThree = new int[ProviderClass.NumberOfProviders];
        internal int[] Ten = new int[ProviderClass.NumberOfProviders];
        internal int[] Zero = new int[ProviderClass.NumberOfProviders];

        // ======================================================================================================================
        // WaterSim 6 Scenarios
        // ----------------------
        // Set the parameters used for output to the text file
        public void myParameters()
        {
            FOB = new FileOutputsBase();
            FOB.myParms.Add(eModelParam.epColorado_River_Flow);
            FOB.myParms.Add(eModelParam.epSaltTonto_AnnualFlow);
            FOB.myParms.Add(eModelParam.epVerde_AnnualFlow);
            //
            FOB.myParms.Add(eModelParam.epPopulation_Used);
            FOB.myParms.Add(eModelParam.epTotal_Demand);
            //
            FOB.myParms.Add(eModelParam.epProvider_IwaniecScenario);
            FOB.myParms.Add(eModelParam.epSaltVerde_Annual_Deliveries_SRP);
            FOB.myParms.Add(eModelParam.epColorado_Annual_Deliveries);
            FOB.myParms.Add(eModelParam.epGroundwater_Bank_Used);
            FOB.myParms.Add(eModelParam.epGroundwater_Bank_Balance);
            FOB.myParms.Add(eModelParam.epWaterAugmentationUsed);
            FOB.myParms.Add(eModelParam.epGroundwater_Pumped_Municipal);
            FOB.myParms.Add(eModelParam.epProvider_AgWaterPumped);
            FOB.myParms.Add(eModelParam.epDemand_Agriculture);
            //
            FOB.myParms.Add(eModelParam.epRO_Reclaimed_Water_Used); 
            //
            FOB.myParms.Add(eModelParam.epReclaimed_Water_Used);
            FOB.myParms.Add(eModelParam.epProvider_Nonpotable);
            FOB.myParms.Add(eModelParam.epRainHarvestedToTotalOutdoor);
            FOB.myParms.Add(eModelParam.epGrayWaterToTotalOutdoor);
            FOB.myParms.Add(eModelParam.epReclaimedToTotalOutdoor);
            FOB.myParms.Add(eModelParam.epProvider_RainWaterHarvestedSF);
            FOB.myParms.Add(eModelParam.epProvider_RainWaterHarvestedMF);
            FOB.myParms.Add(eModelParam.epProvider_RainWaterHarvestedPU);
            FOB.myParms.Add(eModelParam.epProvider_ResGrayWaterUsed);
            FOB.myParms.Add(eModelParam.epProvider_ComGrayWaterUsed);
            FOB.myParms.Add(eModelParam.epProvider_IndGrayWaterUsed);
            FOB.myParms.Add(eModelParam.epProvider_RainWaterHarvested);
            //
            FOB.myParms.Add(eModelParam.epProvider_ResIndoorGPCD_LD);
            FOB.myParms.Add(eModelParam.epProvider_ResIndoorGPCD_MD);
            FOB.myParms.Add(eModelParam.epProvider_ResIndoorGPCD_HD);
            FOB.myParms.Add(eModelParam.epProvider_ResOutdoorGPCD_LD);
            FOB.myParms.Add(eModelParam.epProvider_ResOutdoorGPCD_MD);
            FOB.myParms.Add(eModelParam.epProvider_ResOutdoorGPCD_HD);
            //
            // FOB.myParms.Add(eModelParam.epDemand_Agriculture);
            FOB.myParms.Add(eModelParam.epDemand_LowDensity);
            FOB.myParms.Add(eModelParam.epDemand_MediumDensity);
            FOB.myParms.Add(eModelParam.epDemand_HighDensity);
            // FOB.myParms.Add(eModelParam.epDemand_Turf);
            // FOB.myParms.Add(eModelParam.epDemand_Greenway);
            // FOB.myParms.Add(eModelParam.epDemand_Tree);
            FOB.myParms.Add(eModelParam.epResidentialIndoorWaterUse);
            FOB.myParms.Add(eModelParam.epResidentialOutdoorWaterUse);
            //
            FOB.myParms.Add(eModelParam.epRegionalGWBalance);
            //
            //FOB.myParms.Add(eModelParam.epProvider_StormWaterHarvested);
            FOB.myParms.Add(eModelParam.epStormwater);
            //
            // To keep track of simulations
            FOB.myParms.Add(eModelParam.epSaltVerde_Historical_Extraction_Start_Year);
            FOB.myParms.Add(eModelParam.epColorado_Historical_Extraction_Start_Year);
            FOB.myParms.Add(eModelParam.epGPCDefficiencyLCLU);
            FOB.myParms.Add(eModelParam.epRainFallFactor);
            FOB.myParms.Add(eModelParam.epWebPop_GrowthRateAdj_PCT);
            FOB.myParms.Add(eModelParam.epRainHarvestCompliance);
            FOB.myParms.Add(eModelParam.epRainHarvestInflection);
            //
            FOB.eModelParametersForOutput = new int[FOB.myParms.Count];
            for (int i = 0; i < FOB.myParms.Count; i++)
            {
                FOB.eModelParametersForOutput[i] = FOB.myParms[i];
            }
        }

        // WaterSim 6 above
        // ======================================================================================================================


        // ======================================================================================================================
        // WaterSim 5 Scenarios
        // ----------------------
        // Set the parameters used for output to the text file
        public void myComparison()
        {
            FOB = new FileOutputsBase();
            FOB.myParms.Add(eModelParam.epColorado_River_Flow);
            FOB.myParms.Add(eModelParam.epSaltTonto_AnnualFlow);
            FOB.myParms.Add(eModelParam.epVerde_AnnualFlow);
            //
            FOB.myParms.Add(eModelParam.epPopulation_Used);
            FOB.myParms.Add(eModelParam.epTotal_Demand);
            //
            FOB.myParms.Add(eModelParam.epSaltVerde_Annual_Deliveries_SRP);
            FOB.myParms.Add(eModelParam.epColorado_Annual_Deliveries);
            FOB.myParms.Add(eModelParam.epGroundwater_Bank_Used);
            FOB.myParms.Add(eModelParam.epGroundwater_Bank_Balance);
            FOB.myParms.Add(eModelParam.epWaterAugmentationUsed);
            FOB.myParms.Add(eModelParam.epGroundwater_Pumped_Municipal);
            FOB.myParms.Add(eModelParam.epProvider_AgWaterPumped);
            //
            FOB.myParms.Add(eModelParam.epRO_Reclaimed_Water_Used);
            //
            FOB.myParms.Add(eModelParam.epReclaimed_Water_Used);
            FOB.myParms.Add(eModelParam.epProvider_Nonpotable);
             //
            FOB.myParms.Add(eModelParam.epRegionalGWBalance);
            //
              // To keep track of simulations
            FOB.myParms.Add(eModelParam.epSaltVerde_Historical_Extraction_Start_Year);
            FOB.myParms.Add(eModelParam.epColorado_Historical_Extraction_Start_Year);
            FOB.myParms.Add(eModelParam.epWebPop_GrowthRateAdj_PCT);
            //
            FOB.eModelParametersForOutput = new int[FOB.myParms.Count];
            for (int i = 0; i < FOB.myParms.Count; i++)
            {
                FOB.eModelParametersForOutput[i] = FOB.myParms[i];
            }
        }


        // ======================================================================================================================
        // WaterSim 5 Scenarios for Giuseppe
        // -----------------------
        public void MyParameters()
        {
            FOB = new FileOutputsBase();
            FOB.myParms.Add(eModelParam.epColorado_River_Flow);
            FOB.myParms.Add(eModelParam.epSaltTonto_AnnualFlow);
            FOB.myParms.Add(eModelParam.epVerde_AnnualFlow);
            //
            //FOB.myParms.Add(eModelParam.epSaltVerde_Annual_Deliveries_SRP);
            FOB.myParms.Add(eModelParam.epColorado_Annual_Deliveries);
            FOB.myParms.Add(eModelParam.epCAPaqueduct);
            //FOB.myParms.Add(eModelParam.epCAP_LossPotential_Priority4);
            //FOB.myParms.Add(eModelParam.epCAP_LossPotential_Priority5);

            //
            //
            // To keep track of simulations
            FOB.myParms.Add(eModelParam.epSaltVerde_Historical_Extraction_Start_Year);
            FOB.myParms.Add(eModelParam.epColorado_Historical_Extraction_Start_Year);
            //FOB.myParms.Add(eModelParam.epColorado_User_Adjustment_Stop_Year);
            // FOB.myParms.Add(eModelParam.epSaltVerde_User_Adjustment_Stop_Year);
            //FOB.myParms.Add(eModelParam.epGPCDefficiencyLCLU);
            //FOB.myParms.Add(eModelParam.epWebAugmentation_PCT);
            //FOB.myParms.Add(eModelParam.epWebReclaimedWater_PCT);
            //FOB.myParms.Add(eModelParam.epWebPop_GrowthRateAdj_PCT);

            FOB.eModelParametersForOutput = new int[FOB.myParms.Count];
            for (int i = 0; i < FOB.myParms.Count; i++)
            {
                FOB.eModelParametersForOutput[i] = FOB.myParms[i];
            }
        }
        ProviderIntArray One = new ProviderIntArray(0);
        internal int[] aOne = new int[ProviderClass.NumberOfProviders];
        public void init(WaterSimManager_SIO ws)
        {

            parmIwaniecScenariosYN = false;
            set_parmCalculateRainWaterHarvesting = false;
            set_parmCalculateStormWaterHarvesting = false;
            set_parmCalculateGrayWater = false;
            ws.Simulation_Start_Year = 2000;
            ws.Simulation_End_Year = 2060;
            ws.BaseYear = 2016;
            //
            ws.Assured_Water_Supply_Annual_Groundwater_Pumping_Limit = 1;
               //ws.Provider_Demand_Option = 3;
            ws.Provider_Demand_Option = 4;
            //ws.SaltVerde_User_Adjustment_Percent = 81;
            //ws.Colorado_User_Adjustment_Percent = 88;
            //
            //set_ColoradoTrace = 48;
            //set_SaltVerdeTrace = 48;
            ws.Colorado_Historical_Data_Source = 1;
            ws.SaltVerde_Historical_Data_Source = 1;
            for (int i = 0; i < ProviderClass.NumberOfProviders; i++)
            {
                aOne[i] = 1;
            }
            One.Values = aOne;
            ws.WaterBank_Source_Option.setvalues(One);

        }
        // END OF WaterSim 5 Scenario code for Giuseppe Mascaro
        // =======================================================================================================================



        // =======================================================================================================================
        // WaterSim 6 
        public void Initialize(int Scenario,WaterSimManager_SIO ws)
        {
            //
            bool WDworkshop = true;

            ws.Web_PopulationGrowthRate_PCT = 100;
            set_parmIncludeMeteorology = true;
            parmIwaniecScenariosYN = true;
            // ================================================================================
            WAugParm = ws.ParamManager.Model_Parameter(eModelParam.epWebAugmentation_PCT);
            WWtoEff = ws.ParamManager.Model_Parameter(eModelParam.epPCT_Wastewater_to_Effluent);
            WWtoReclaimed = ws.ParamManager.Model_Parameter(eModelParam.epPCT_WasteWater_to_Reclaimed);
            RtoRO = ws.ParamManager.Model_Parameter(eModelParam.epPCT_Reclaimed_to_RO);
            //
            // ========================================
            // RCP8.5 emission scenario
            //CNRM.a2 GCM with a2 emission
            // 60 for the Colorado River
            // 55 (estimate) for the Salt-Verde-Tonto Rivers
            //
            // Climate
            if (WDworkshop)
            {

            }
            else
            {
                ws.Colorado_Climate_Adjustment_Percent = 60;
                ws.SaltVerde_Climate_Adjustment_Percent = 55;
                ws.Simulation_End_Year = 2060;
                // Drought Long-term mean versus since 2000
                // Udall and Overpeck (2017) Climate change is shrinking the Colorado River
                ws.SaltVerde_User_Adjustment_Percent = 81;
                ws.Colorado_User_Adjustment_Percent = 81;
                //
                // Use actual flow data up untill 2015
                ws.SaltVerde_User_Adjustment_Start_Year = 2016;
                ws.Colorado_User_Adjustment_StartYear = 2016;
                ws.SaltVerde_User_Adjustment_Stop_Year = 2025;
                ws.Colorado_User_Adjustment_Stop_Year = 2025;
            }
            // 1=AD; 2 = AF; 3=AH; 4=HHH; 5=EC; 6=ZW; 7=BAU; 8=testing; 9=default
            // int scenario = 7;
            // int scenario = Scenario;
            //set_scenario = Scenario;
            // Default Values
            for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { Zero[i] = 0; }
            Out.Values = Zero;
           // ws.PCT_SurfaceWater_to_WaterBank.setvalues(Out);
            ws.WaterAugmentation.setvalues(Out);
            ws.PCT_Reclaimed_Outdoor_Use.setvalues(Out);
            ws.PCT_Wastewater_Reclaimed.setvalues(Out);
             //
            ws.RainFallFactor = 100;
            
            // ===============================================
            //
            string path = @"BAU\";
            switch (Scenario)
            {
                case 1:
                    path = @"AD\";
                    //
                    // Capture Rain water Residential
                    set_parmCalculateRainWaterHarvesting = true;
                    // Capture Rain water Commercial and Industrial too
                    set_parmCalculateRainWaterResidentialOnly = false;
                    // Capture Storm Water - default = 50% of maximum 60-year values
                    set_parmCalculateStormWaterHarvesting = true;
                    // NOTE: Using a value > 0 for the Structural Capacity overrides the default 50% of rainfall maximum
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { Zero[i] = 0; }
                    Out.Values = Zero;
                    ws.StormWaterStructural_Capacity.setvalues(Out);
                    //
                    // Utilize Gray water
                    set_parmCalculateGrayWater = true;
                    // 90% of the houses implement Gray Water Recycling
                    set_parmGrayWaterCompliancePCT = 90;
                    //
                    // Bank excess CAP surface Water
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { OneHundred[i] = 100; }
                    Out.Values = OneHundred;
                    ws.PCT_SurfaceWater_to_WaterBank.setvalues(Out);
                    ws.PCT_Wastewater_to_Effluent.setvalues(Out); // Not sure about these

                    //
                    set_parmIwaniecNoLeaks = true;
                    // Linearly reduce effluent goint to Paleo Verde over time.
                    set_parmIwaniecPPtoAgEffluent = true;
                    // Use Augmented Water - 10% of demand
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { Ten[i] = 10; }
                    Out.Values = Ten;
                    ws.WaterAugmentation.setvalues(Out);
                    break;
                case 2:
                    path = @"AF\";

                    // Adaptive flood
                    set_parmCalculateRainWaterHarvesting = true;
                    set_parmCalculateRainWaterResidentialOnly = true; // This was imposed by me due to lack of data;
                    set_parmCalculateStormWaterHarvesting = false;
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { OneHundred[i] = 0; }
                    Out.Values = OneHundred;
                    ws.StormWaterStructural_Capacity.setvalues(Out);
                    //
                    //set_parmGrayWaterCompliancePCT = 77;
                    //set_parmYearsToAdopt = 15;
                    set_parmCalculateGrayWater = true;
                    set_parmIwaniecNoLeaks = false;
                    set_parmIwaniecPPtoAgEffluent = false;
                    WAugParm.Value = 0;

                    // 07.12.18 Added based on Table 5
                    // revisited 01.28.19, 02.18.19
                    // -------------------------------------------------------------------------------------------------
                    //for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { Zero[i] = 0; }
                    //Out.Values = Zero;
                    //ws.PCT_Reclaimed_to_RO.setvalues(Out); // Cook and Iwaniec "potable"
                    //ws.PCT_RO_to_Water_Supply.setvalues(Out);

                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { Fifty[i] = 50; }
                    Out.Values = Fifty;
                     ws.PCT_Wastewater_Reclaimed.setvalues(Out);

                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { OneHundred[i] = 100; }
                    Out.Values = OneHundred;
                    ws.PCT_Reclaimed_to_Water_Supply.setvalues(Out);
                    ws.PCT_Reclaimed_Outdoor_Use.setvalues(Out);

 
                    // 07.12.18 
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { Ten[i] = 10; }
                    Out.Values = Ten;
                     ws.WaterAugmentation.setvalues(Out);
                    // ====================================================
                    break;
                case 3:
                    path = @"AH\";

                    // Adaptive Heat
                    set_parmCalculateRainWaterHarvesting = true;
                    set_parmCalculateRainWaterResidentialOnly = false;
                    set_parmCalculateStormWaterHarvesting = false;
                    set_parmCalculateGrayWater = true;
                    set_parmGrayWaterCompliancePCT = 95;
                    set_parmYearsToAdopt = 15;
                    set_parmIwaniecNoLeaks = false;
                    set_parmIwaniecPPtoAgEffluent = false;
                    WAugParm.Value = 0;
                    break;
                case 4:
                    path = @"HHH\";

                    // Transformative Healthy Harvest Hubs
                    set_parmCalculateRainWaterHarvesting = false;
                    set_parmCalculateRainWaterResidentialOnly = true;
                    set_parmCalculateStormWaterHarvesting = true;
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { Zero[i] = 0; }
                    Out.Values = Zero;
                    // Using the Structural Capacity overrides 
                    ws.StormWaterStructural_Capacity.setvalues(Out);

                    set_parmCalculateGrayWater = true;
                    set_parmGrayWaterCompliancePCT = 100;
                    set_parmYearsToAdopt = 15;
                    set_parmIwaniecNoLeaks = false;
                    set_parmIwaniecPPtoAgEffluent = true;
                    WAugParm.Value = 0;

                    break;
                case 5:
                    path = @"EC\";

                    // Transformative Emerald City Necklace
                    set_parmCalculateRainWaterHarvesting = true;
                    set_parmCalculateRainWaterResidentialOnly = true;
                    set_parmCalculateStormWaterHarvesting = false;
                    // Using the Structural Capacity overrides 
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { OneHundred[i] = 1000; }
                    Out.Values = OneHundred;
                    ws.StormWaterStructural_Capacity.setvalues(Out);

                    set_parmCalculateGrayWater = true;
                    set_parmGrayWaterCompliancePCT = 77;
                    set_parmYearsToAdopt = 15;
                    set_parmIwaniecNoLeaks = false;
                    //
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { Zero[i] = 0; }
                    Out.Values = Zero;
                    ws.PCT_Effluent_to_Vadose.setvalues(Out);
                    //
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { Ninty[i] = 80; }
                    Out.Values = Ninty;
                    ws.PCT_Wastewater_to_Effluent.setvalues(Out);
                    //
                    set_parmIwaniecPPtoAgEffluent = true;
                    WAugParm.Value = 0;

                    break;
                case 6:
                    path = @"ZW\";

                    // Transformative Zero Waste
                    set_parmCalculateRainWaterHarvesting = true;
                    set_parmCalculateRainWaterResidentialOnly = true;
                    set_parmCalculateStormWaterHarvesting = false;
                    set_parmCalculateGrayWater = true; // change from initial
                    set_parmIwaniecNoLeaks = true;
                    set_parmIwaniecPPtoAgEffluent = true;

                    // Using the Structural Capacity overrides
                    // ==============================================
                    //
                    WAugParm.Value = 0;
                    //
                    // 01.11.17 based on conversations with Elizabeth (my own decisions for 02.21.19)
                    // revisited 01.28.19, 02.18.19,02.21.19
                    // ==============================================
                     // Reclaimed
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { SevenFive[i] = 75; }
                    Out.Values = SevenFive;
                     ws.PCT_Wastewater_Reclaimed.setvalues(Out);
                    //
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { Zero[i] = 0; }
                    Out.Values = Zero;
                    ws.PCT_Reclaimed_to_Vadose.setvalues(Out);
                    ws.PCT_Reclaimed_to_Water_Supply.setvalues(Out);
   
                    //
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { ThirtyThree[i] = 33; }
                    Out.Values = ThirtyThree;
                     ws.PCT_Reclaimed_to_RO.setvalues(Out); // Cook and Iwaniec "potable"
                    //
                    Out.Values = OneHundred;
                     ws.PCT_Reclaimed_Outdoor_Use.setvalues(Out);

                    //
                    // Effluent
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { OneHundred[i] = 100; }
                    Out.Values = OneHundred;
                    ws.PCT_Wastewater_to_Effluent.setvalues(Out);
                    ws.PCT_RO_to_Water_Supply.setvalues(Out);
                    // 
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { Fifty[i] = 50; }
                    Out.Values = Fifty;
                    ws.PCT_Effluent_to_Vadose.setvalues(Out);
                    //
                    Out.Values = Fifty;
                    ws.PCT_Reclaimed_to_DirectInject.setvalues(Out);
                    ws.PCT_Reclaimed_to_Water_Supply.setvalues(Out); 

                    // ==============================================

                    break;
                case 7:
                    path = @"BAU\";
                    // Testing
                        set_parmCalculateRainWaterHarvesting = true;
                        set_parmCalculateRainWaterResidentialOnly = true;
                        //set_parmCalculateRainWaterCommercialOnly = false;
                        //
                        set_parmCalculateStormWaterHarvesting = false;
                        for (int i = 0; i < ProviderClass.NumberOfProviders; i++) {Zero[i] = 0; }
                        Out.Values = Zero;
                        ws.StormWaterStructural_Capacity.setvalues(Out);

                    // revisited 01.28.19
                    // ==============================================
                        set_parmCalculateGrayWater = true;
                        set_parmIwaniecNoLeaks = false;
                        set_parmIwaniecPPtoAgEffluent = false;

                        // 01.11.17 based on conversations with Elizabeth
                        // revisited 01.28.19
                        // ==============================================
                        for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { Fifty[i] = 25; }
                        Out.Values = Fifty;
                         ws.PCT_Wastewater_Reclaimed.setvalues(Out);

                    //
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { OneHundred[i] = 100; }
                        Out.Values = OneHundred;
                         ws.PCT_Reclaimed_to_Water_Supply.setvalues(Out);
                         ws.PCT_Reclaimed_Outdoor_Use.setvalues(Out);
                      // ==============================================
                    //
                    break;
                 case 8:
                    path = @"BAU\";
                    // Testing
                    // ==============================================
                    path = @"BAU\";
                    // Testing
                    set_parmCalculateRainWaterHarvesting = false;
                    set_parmCalculateRainWaterResidentialOnly = true;
                    //set_parmCalculateRainWaterCommercialOnly = false;
                    //
                    set_parmCalculateStormWaterHarvesting = false;
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { Zero[i] = 0; }
                    Out.Values = Zero;
                    ws.StormWaterStructural_Capacity.setvalues(Out);

                    // revisited 01.28.19
                    // ==============================================
                    set_parmCalculateGrayWater = false;
                    set_parmIwaniecNoLeaks = false;
                    set_parmIwaniecPPtoAgEffluent = false;

                    // 01.11.17 based on conversations with Elizabeth
                    // revisited 01.28.19
                    // ==============================================
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { Fifty[i] = 25; }
                    Out.Values = Fifty;
                    ws.PCT_Wastewater_Reclaimed.setvalues(Out);

                    //
                    for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { OneHundred[i] = 100; }
                    Out.Values = OneHundred;
                    ws.PCT_Reclaimed_to_Water_Supply.setvalues(Out);
                    ws.PCT_Reclaimed_Outdoor_Use.setvalues(Out);
                    // ==============================================
                    //


                    break;
                default:
                    path = @"BAU\";

                    set_parmIncludeMeteorology = true;
                    set_parmCalculateRainWaterHarvesting = false;
                    set_parmCalculateStormWaterHarvesting = false;

                    break;
            }
            //
            ws.IwaniecScenarios = Scenario;
            //
            // Copy the appropriate LCLU classification file into the Data Directory prior to the simulation
            // 07.05.16
            // SimpleFileCopy myFile = new SimpleFileCopy();
            //myFile.CopyMain(path);
            //
            //InitSecondary(gpcdReduce, reuse);
            // ws.Simulation_End_Year = 2060;
            // -------------------------------------
            bool testing = false;
            if (testing)
            {
                // General Testing of NEW array structure
                for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { OneHundred[i] = 1000; }
                Out.Values = OneHundred;
                //ws.SurfaceWater__to_Vadose.setvalues(Out);
                for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { OneHundred[i] = 50; }
                Out.Values = OneHundred;
                //
                for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { OneHundred[i] = 1000; }
                Out.Values = OneHundred;
                //ws.Use_WaterSupply_to_DirectInject.setvalues(Out);
                //
                for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { OneHundred[i] = 50; }
                Out.Values = OneHundred;
                //ws.PCT_Wastewater_Reclaimed.setvalues(Out);
                //
                for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { OneHundred[i] = 50; }
                Out.Values = OneHundred;
                //ws.PCT_Reclaimed_to_RO.setvalues(Out);
                //
                for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { OneHundred[i] = 100; }
                Out.Values = OneHundred;
                //ws.PCT_RO_to_Water_Supply.setvalues(Out);
                //
            }
            // ===================================================================================

        }

    }


}
