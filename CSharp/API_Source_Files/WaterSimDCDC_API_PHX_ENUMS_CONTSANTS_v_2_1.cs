using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace WaterSimDCDC
{
    #region eModelParam

    public static partial class eModelParam
    {
        // Values 0 to 200 reserved for Basic Model Inpus and Outputs
        /// <summary> The colorado river flow.  Parameter </summary>
        public const int epColorado_River_Flow = 0;
        /// <summary> The powell storage.  Parameter </summary>
        public const int epPowell_Storage = 30;
        /// <summary> The mead storage.  Parameter </summary>
        public const int epMead_Storage = 31;
        /// <summary> The population used.  Parameter </summary>
        public const int epPopulation_Used = RegionalWeight;  // = 3
        /// <summary> The salt verde river flow.  Parameter </summary>
        public const int epSaltVerde_River_Flow = 28;
        /// <summary> The salt verde storage.  Parameter </summary>
        public const int epSaltVerde_Storage = 4;
        /// <summary> The effluent to agriculture.  Parameter </summary>
        public const int epEffluent_To_Agriculture = 5;
        /// <summary> The groundwater pumped municipal.  Parameter </summary>
        public const int epGroundwater_Pumped_Municipal = 6;
        /// <summary> The groundwater balance.  Parameter </summary>
        public const int epGroundwater_Balance = 7;
        /// <summary> The salt verde annual deliveries srp.  Parameter </summary>
        public const int epSaltVerde_Annual_Deliveries_SRP = 8;
        /// <summary> The salt verde class bc designations.  Parameter </summary>
        public const int epSaltVerde_Class_BC_Designations = 9;
        /// <summary> The colorado annual deliveries.  Parameter </summary>
        public const int epColorado_Annual_Deliveries = 10;
        /// <summary> The groundwater bank used.  Parameter </summary>
        public const int epGroundwater_Bank_Used = 11;
        /// <summary> The groundwater bank balance.  Parameter </summary>
        public const int epGroundwater_Bank_Balance = 12;
        /// <summary> The reclaimed water used.  Parameter </summary>
        public const int epReclaimed_Water_Used = 13;
        /// <summary> The reclaimed water to vadose.  Parameter </summary>
        public const int epReclaimed_Water_To_Vadose = 14;
        /// <summary> The reclaimed water discharged.  Parameter </summary>
        public const int epReclaimed_Water_Discharged = 15;
        /// <summary> The reclaimed water to direct inject.  Parameter </summary>
        public const int epReclaimed_Water_to_DirectInject = 16;
        /// <summary> The ro reclaimed water used.  Parameter </summary>
        public const int epRO_Reclaimed_Water_Used = 17;
        /// <summary> The ro reclaimed water to direct inject.  Parameter </summary>
        public const int epRO_Reclaimed_Water_to_DirectInject = 18;
        /// <summary> The effluent created.  Parameter </summary>
        public const int epEffluent_Reused = 19;
        /// <summary> The effluent to vadose.  Parameter </summary>
        public const int epEffluent_To_Vadose = 20;
        /// <summary> The effluent to power plant.  Parameter </summary>
        public const int epEffluent_To_PowerPlant = 21;
        /// <summary> The effluent discharged.  Parameter </summary>
        public const int epEffluent_Discharged = 22;
        /// <summary> The demand deficit.  Parameter </summary>
        public const int epDemand_Deficit = 23;
        /// <summary> The total demand.  Parameter </summary>
        public const int epTotal_Demand = 24;
        /// <summary> The on project demand.  Parameter </summary>
        public const int epOnProjectDemand = 25;
        /// <summary> The off project demand.  Parameter </summary>
        public const int epOffProjectDemand = 26;
        /// <summary> The gpcd used.  Parameter </summary>
        public const int epGPCD_Used = 27;

        /// <summary> The salt verde spillage.  Parameter </summary>
        public const int epSaltVerde_Spillage = 29;
        ///// <summary> The simulation start year.  Parameter </summary>
        //public const int epSimulation_Start_Year = 30;
        ///// <summary> The simulation end year.  Parameter </summary>
        //public const int epSimulation_End_Year = 31;
        /// <summary> The colorado historical extraction start year.  Parameter </summary>
        public const int epColorado_Historical_Extraction_Start_Year = 32;
        /// <summary> The colorado historical data source.  Parameter </summary>
        public const int epColorado_Historical_Data_Source = 33;
        /// <summary> The colorado climate adjustment percent.  Parameter </summary>
        public const int epColorado_Climate_Adjustment_Percent = 34;
        /// <summary> The colorado user adjustment percent.  Parameter </summary>
        public const int epColorado_User_Adjustment_Percent = 35;
        /// <summary> The colorado user adjustment start year.  Parameter </summary>
        public const int epColorado_User_Adjustment_StartYear = 36;
        /// <summary> The colorado user adjustment stop year.  Parameter </summary>
        public const int epColorado_User_Adjustment_Stop_Year = 37;
        /// <summary> The salt verde historical extraction start year.  Parameter </summary>
        public const int epSaltVerde_Historical_Extraction_Start_Year = 38;
        /// <summary> Information describing The salt verde historical.  Parameter </summary>
        public const int epSaltVerde_Historical_Data = 39;
        /// <summary> The salt verde climate adjustment percent.  Parameter </summary>
        public const int epSaltVerde_Climate_Adjustment_Percent = 40;
        /// <summary> The salt verde user adjustment percent.  Parameter </summary>
        public const int epSaltVerde_User_Adjustment_Percent = 41;
        /// <summary> The salt verde user adjustment start year.  Parameter </summary>
        public const int epSaltVerde_User_Adjustment_Start_Year = 42;
        /// <summary> The salt verde user adjustment stop year.  Parameter </summary>
        public const int epSaltVerde_User_Adjustment_Stop_Year = 43;
        /// <summary> The provider demand option.  Parameter </summary>
        public const int epProvider_Demand_Option = 44;
        /// <summary> The pct reduce gpcd.  Parameter </summary>
        public const int epPCT_Alter_GPCD = 45;
        /// <summary> The modfy normal flow.  Parameter </summary>
        public const int epModfyNormalFlow = 46;

        /// <summary> An Amount of Water supply Augmentation. Parameter </summary>
        public const int epWaterAugmentation = 47;
        /// <summary> The undefined 48.  Parameter </summary>
        public const int epWaterAugmentationUsed = 48;
        //Undefined_48 = 48;

        /// <summary> The use gpcd.  Parameter </summary>
        public const int epUse_GPCD = 49;
        /// <summary> The pct waste water to reclaimed.  Parameter </summary>
        public const int epPCT_WasteWater_to_Reclaimed = 50;
        /// <summary> The pct wastewater to effluent.  Parameter </summary>
        public const int epPCT_Wastewater_to_Effluent = 51;
        /// <summary> The pct reclaimed to ro.  Parameter </summary>
        public const int epPCT_Reclaimed_to_RO = 52;
        /// <summary> The pct ro to water supply.  Parameter </summary>
        public const int epPCT_RO_to_Water_Supply = 53;
        /// <summary> The pct reclaimed to direct inject.  Parameter </summary>
        public const int epPCT_Reclaimed_to_DirectInject = 54;
        /// <summary> The pct reclaimed to water supply.  Parameter </summary>
        public const int epPCT_Reclaimed_to_Water_Supply = 55;
        /// <summary> The pct reclaimed to vadose.  Parameter </summary>
        public const int epPCT_Reclaimed_to_Vadose = 56;
        /// <summary> The pct effluent to vadose.  Parameter </summary>
        public const int epPCT_Effluent_to_Vadose = 57;
        /// <summary> The pct effluent to power plant.  Parameter </summary>
        public const int epPCT_Effluent_to_PowerPlant = 58;
        /// <summary> The surface water to vadose.  Parameter </summary>
        public const int epSurfaceWater__to_Vadose = 59;
        /// <summary> The surface to vadose time lag.  Parameter </summary>
        public const int epSurface_to_Vadose_Time_Lag = 60;
        /// <summary> The water bank source option.  Parameter </summary>
        public const int epWaterBank_Source_Option = 61;
        /// <summary> The pct surface water to water bank.  Parameter </summary>
        public const int epPCT_SurfaceWater_to_WaterBank = 62;
        /// <summary> The use surface water to water bank.  Parameter </summary>
        public const int epUse_SurfaceWater_to_WaterBank = 63;
        /// <summary> The pct water supply to residential.  Parameter </summary>
        public const int epPCT_WaterSupply_to_Residential = 64;
        /// <summary> The pct water supply to commercial.  Parameter </summary>
        public const int epPCT_WaterSupply_to_Commercial = 65;
        /// <summary> The use water supply to direct inject.  Parameter </summary>
        public const int epUse_WaterSupply_to_DirectInject = 66;
        /// <summary> The pct outdoor water use.  Parameter </summary>
        public const int epPCT_Outdoor_WaterUse = 67;
        /// <summary> The pct groundwater treated.  Parameter </summary>
        public const int epPCT_Groundwater_Treated = 68;
        /// <summary> The pct reclaimed outdoor use.  Parameter </summary>
        public const int epPCT_Reclaimed_Outdoor_Use = 69;
        /// <summary> The pct growth rate adjustment on project.  Parameter </summary>
        public const int epPCT_Growth_Rate_Adjustment_OnProject = 70;
        /// <summary> The pct maximum demand reclaim.  Parameter </summary>
        public const int epPCT_Max_Demand_Reclaim = 71;
        /// <summary> The pct register growth rate adjustment.  Parameter </summary>
        public const int epPCT_REG_Growth_Rate_Adjustment = 72;
        // ***** Changed 8 13 12 
        // public const int epSetPopulations = 73;
        // public const int Undefined_74 = 74;
        /// <summary> The set populations on.  Parameter </summary>
        public const int epSetPopulationsOn = 73;
        /// <summary> The set populations other.  Parameter </summary>
        public const int epSetPopulationsOther = 74;
        //*********
        /// <summary> The pct water supply to industrial.  Parameter </summary>
        public const int epPCT_WaterSupply_to_Industrial = 75;
        /// <summary> The pct outdoor water use resource.  Parameter </summary>
        public const int epPCT_Outdoor_WaterUseRes = 76;
        /// <summary> The pct outdoor water use com.  Parameter </summary>
        public const int epPCT_Outdoor_WaterUseCom = 77;
        /// <summary> The pct outdoor water use ind.  Parameter </summary>
        public const int epPCT_Outdoor_WaterUseInd = 78;
        /// <summary> The on project population.  Parameter </summary>
        public const int epOnProjectPopulation = 79;
        /// <summary> The other population.  Parameter </summary>
        public const int epOtherPopulation = 80;
        /// <summary> The mead level.  Parameter </summary>
        public const int epMeadLevel = 81;
        /// <summary> The the ws annual gw limit.  Parameter </summary>
        public const int epAWSAnnualGWLimit = 82;
        /// <summary> The pct growth rate adjustment other.  Parameter </summary>
        public const int epPCT_Growth_Rate_Adjustment_Other = 83;
        /// <summary> The annual incidental.  Parameter </summary>
        public const int epAnnualIncidental = 84;
        /// <summary> The vadose to aquifer.  Parameter </summary>
        public const int epVadoseToAquifer = 85;
        /// <summary> The regional natural recharge.  Parameter </summary>
        public const int epRegionalNaturalRecharge = 86;
        /// <summary> The regional cagrd recharge.  Parameter </summary>
        public const int epRegionalCAGRDRecharge = 87;
        /// <summary> The regional inflow.  Parameter </summary>
        public const int epRegionalInflow = 88;
        /// <summary> The regional ag to vadose.  Parameter </summary>
        public const int epRegionalAgToVadose = 89;
        /// <summary> The regional provider recharge.  Parameter </summary>
        public const int epRegionalProviderRecharge = 90;
        /// <summary> The regional ag other pumping.  Parameter </summary>
        public const int epRegionalAgOtherPumping = 91;
        /// <summary> The regional outflow.  Parameter </summary>
        public const int epRegionalOutflow = 92;
        /// <summary> The regional gw balance.  Parameter </summary>
        public const int epRegionalGWBalance = 93;

        // DAVID Add 94 - 104 2/24/14
        public const int epPowellLevel = 94;
        /// <summary> The provider level adjustment of the temporal trend in GPCD </summary>
        public const int epAlterGPCDpct = 95;
        public const int epGPCDraw = 96;
        public const int epRESdefault = 97;
        public const int epCOMdefault = 98;
        public const int epINDdefault = 99;
        public const int epProvider_Max_NormalFlow = 100;
        // added water to add to agriculture - added in the demand stream
        public const int epNewAgWaterFromSurface = 101;
        public const int epNetAgWater = 102;
        //
        public const int epProvider_WaterFromAgPumping = 103;
        public const int epProvider_WaterFromAgSurface = 104;
        public const int epProvider_WaterFromAgPumpingMax = 105;
        public const int epProvider_WaterFromAgSurfaceMax = 106;
        // 07.29.15 das
        public const int epColorado_Max_Deliveries = 107;
        public const int epColorado_Unused_P4 = 108;
        public const int epColorado_Unused_P5 = 109;
        public const int epPCT_Default_MnI_Pumping = 110;
        public const int epCAP_LossPotential_Priority4 = 111;
        public const int epCAP_LossPotential_Priority5 = 112;
        // 07.27.16 das
        public const int epProvider_ShowersMinPCD = 113;
        // 01.04.15 das
        public const int epCAP_availForWB_Priority4 = 115;
        public const int epCAP_availForWB_Priority5 = 116;
        // 07.16.18 added by DAS
        public const int epCAPaqueduct = 117;


        //  SRP Salt Other Storage
        public const int epSaltOther_Storage = 119;
        // SRP Roosevelt Storage'
        public const int epRoosevelt_Storage = 120;
        //
        public const int epSalt_Storage = 121;
        // SRP Verde Storage
        public const int epVerde_Storage = 122;
        // Annual Flow of the Verde River
        public const int epVerde_AnnualFlow = 123;
        // Annual Flow of Salt River and Tonto Creek
        public const int epSaltTonto_AnnualFlow = 124;

        // Web Interface control for Reclaimed Water
        public const int epWebReclaimedWater_PCT = 125;
        //       126 is used- see PHX_WebInterface_7_*
        //       127 is used- see PHX_WebInterface_7_*
        // Minimum GPCD for each provider
        // Using Demand option 3- 01.07.15 DAS
        public const int epDefault_Status = 129;
        public const int epProvider_GPCDmin = 130;
        // 01.21.15 DAS
        public const int epProvider_GPCDres = 131;
        public const int epProvider_AgWaterPumped = 132;
        // 02.17.15
        public const int epProvider_AgWaterUsedvsThresh = 133;
        //04.06.15 DAS
        public const int epSaltVerde_Annual_SurfaceDeliveries_SRP = 134;
        public const int epSaltVerde_Annual_GW_Deliveries_SRP = 135;

        public const int epProvider_GPCDcomInd = 136;
        public const int epProvider_GPCDslope = 137;
        // DAS 01.26.16
        public const int epProvider_DroughtScenarios = 138;
        public const int epProvider_SRPDroughtScenarios = 139;
        //
        // 10.03.16 DAS
        public const int epBase_Preset_Scenarios = 140;
       // public const int epProvider_DataCheckOnMetBandC = 140;
       // public const int epProvider_DataCheckOffMetGW = 141;
        //==================================

        // Values 700 to 900 Reserved for GroundWaterModel
        /// <summary> The first gw parameter.  Parameter </summary>
        /// <remarks> Reserved Value</remarks>
        public const int FirstGWParameter = 701;
        /// <summary> The last gw parameter.  Parameter </summary>
        /// <remarks> Reserved Value</remarks>
        public const int LastGWParameter = 900;
        //----------------------------   
        /// <summary> The groundwater model.  Parameter </summary>
        public const int epGroundwater_Model = 701;
        /// <summary> The credit deficits.  Parameter </summary>
        public const int epCreditDeficits = 702;

        //======================================
        // Values 201 to 300 Reserved for Derived Parameters

        /// <summary> The total supply used.  Parameter </summary>
        public const int epTotalSupplyUsed = 201;
        /// <summary> The pctg wof demand.  Parameter </summary>
        public const int epPCTGWofDemand = 202;
        /// <summary> The total reclaimed used.  Parameter </summary>
        public const int epTotalReclaimedUsed = 203;
        /// <summary> The provder effluent to ag.  Parameter </summary>
        public const int epProvderEffluentToAg = 204;
        /// <summary> The total effluent.  Parameter </summary>
        public const int epTotalEffluent = 205;
        /// <summary> The projected on project POP.  Parameter </summary>
        public const int epProjectedOnProjectPop = 206;
        /// <summary> The projected other POP.  Parameter </summary>
        public const int epProjectedOtherPop = 207;
        /// <summary> The projected total POP.  Parameter </summary>
        public const int epProjectedTotalPop = 208;
        /// <summary> The difference projected other POP.  Parameter </summary>
        public const int epDifferenceProjectedOtherPop = 209;
        /// <summary> The difference projected on POP.  Parameter </summary>
        public const int epDifferenceProjectedOnPop = 210;
        /// <summary> The difference total POP.  Parameter </summary>
        public const int epDifferenceTotalPop = 211;
        /// <summary> The pct difference projected on POP.  Parameter </summary>
        public const int epPCTDiffProjectedOnPop = 212;
        /// <summary> The pct difference projected other POP.  Parameter </summary>
        public const int epPCTDiffProjectedOtherPop = 213;
        /// <summary> The pct difference projected total POP.  Parameter </summary>
        public const int epPCTDiffProjectedTotalPop = 214;
        /// DAVID Added 250 to 261 2/24/14
        public const int epEnvironmentalFlow_PCT = 250;
        public const int epEnvironmentalFlow_AFa = 251;
        //01.20.15 DAS
        //-------------------------------------------------------------
        public const int epCOdeltaBurden = 252;
        public const int epCOdeltaRatioOfBurden = 253;

        // DAS: Ray does not have these in his code as of 01.07.15
        // -------------------------------------------------------------
        public const int epDecisonGame2014 = 257;
        public const int epDecisonGameRes = 258;
        public const int epDecisonGameCom = 259;
        public const int epDecisonGameInd = 260;
        // -----------------------------------------------
        // 02.05.15 DAS
        public const int epDecisionGameAg = 261;
        public const int epDecisionGameEnv = 262;
        public const int epDecisionGameEnvAF = 263;
        public const int epDecisionGameEnvRatio = 266;
        // -------------------------------------------------------------

        // DAS: changed these two by adding one on 01.07.15
        public const int epTotalReclaimedCreated_AF = 264;
        public const int epTWWTPCreated_AF = 265;

        public const int epAlterGPCDall = 267;
        //
        public const int epAlterGPCDdifference = 270;
        public const int epAlterGPCDslope = 271;
        /// DAVID  
        public const int epBaseYear = 272;
        public const int epReduceGPCD = 273;

        //----------------------------   


        //======================================
        // Values 301 to 400 Reserved for Sustianbel Parameters

        /// <summary> The pct deficit.  Parameter </summary>
        public const int epPCT_Deficit = 301;
        /// <summary> The deficit in years.  Parameter </summary>
        public const int epDeficit_Years = 302;
        /// <summary> The deficit total.  Parameter </summary>
        public const int epDeficit_Total = 303;
        /// <summary> The pct gw available.  Parameter </summary>
        public const int epPCT_GWAvailable = 304;
        /// <summary> The yrs gw zero.  Parameter </summary>
        public const int epYrsGWZero = 305;
        /// <summary> The year gw goes zero.  Parameter </summary>
        public const int epYearGWGoesZero = 306;
        /// <summary> The years not assured.  Parameter </summary>
        public const int epYearsNotAssured = 307;
        /// <summary> The percent of total supply that is reclaimed</summary>
        public const int epPctRecOfTotal = 308;
        /// <summary> Regional Agricultural Sustainability Indicator</summary>
        public const int epAgSustainIndicator = 309;
        /// <summary> Regional Environment Sustainability Indicator</summary>
        public const int epEnvSustainIndicator = 310;
        /// <summary> Provider Personal Water Use Sustainability Indicator</summary>
        public const int epWaterUseIndicator = 311;
        /// <summary> Provider Groundwater Sustainability Indicator</summary>
        public const int epGWYrsSustain = 312;
        /// <summary> Regional Groundwater Sustainability Indicator</summary>
        public const int epRegGWYrsSustain = 313;
        /// <summary> Regional Personal Water Use Sustainability Indicator</summary>
        public const int epRegPctGWDemand = 314;
        ///<summary> Mead Dead Power </summary> 
        public const int epMeadDeadPool = 315;
        ///<summary> Mead lower SNWA Intake </summary> 
        public const int epMeadLowerSNWA = 316;
        ///<summary> Mead Shortage Sharing </summary> 
        public const int epMeadShortageSharingLow = 317;
        ///<summary> Mead Minimum Power </summary> 
        public const int epMeadMinPower = 318;
        ///<summary> Mead Capacity </summary> 
        public const int epMeadCapacity = 319;
        ///<summary> Mead Shortage Sharing </summary> 
        public const int epMeadShortageSharingMed = 320;
        ///<summary> Mead Shortage Sharing </summary> 
        public const int epMeadShortageSharingHigh = 321;
        /// <summary> SI regional groundwater percent of initial</summary>das 10.22.15
        public const int epRegGWpctInitial = 322;
        // 08.23.16 das
        public const int epCreditDeficit_Years = 323;
        // 08.23.16 das
        public const int epPCT_CreditsAvailable = 324;
        //
        public const int epYearCreditsGoesNeg = 325;
        //======================================
        // Values 501 to 600 Reserved for Process Parameters
        
        /// <summary> The percent deficit limit.  Parameter </summary>
        public const int epPercentDeficitLimit = 502;
        /// <summary> The minimum gpcd.  Parameter </summary>
        public const int epMinimumGPCD = 503;
        /// <summary> The years of non the ws trigger.  Parameter </summary>
        public const int epYearsOfNonAWSTrigger = 504;
        /// <summary> The years of non a wsgpcd trigger.  Parameter </summary>
        public const int epYearsOfNonAWSGPCDTrigger = 505;
        /// <summary>
        /// Years of negative credits (deficits)
        /// </summary>
        public const int epYearsOfCreditsTrigger = 506;


        //----------------------------   
        // 
        // Web Parameters 601 to 700
        // 


        // Adjusts growth rate for all providers based on single value from WEB UI
        public const int epWebPop_GrowthRateAdj_PCT = 601;
        // Adjuste effluent allocation 
        public const int epWebUIeffluent = 602;
        public const int epWebUIeffluent_Ag = 603;
        public const int epWebUIeffluent_Env = 604;
        // adjusrs ag to urban
        public const int epWebUIAgriculture = 605;
        // adjusts all provider GPCD goals based in singel value fom WEB UI
        public const int epWebUIPersonal_PCT = 606;

        // adjusts all providers water bank pct based on single value from Web UI
        public const int epWebWaterBank_PCT = 607;
        //
        public const int epWebAugmentation_PCT = 608;
        // 11.14.16 DAS
        public const int epWebOutDoorUseRes_PCT = 609;
        // 11.15.16 DAS
        public const int epWebAgEfficiency_PCT = 610;
        //===================================
        // Values 1501 to 1700 Reserved for User Defined Parameters
        //-------------

        //=========================================

        /// <summary> The names  of the Parameters. </summary>

        const int MaxParameters = 173;
        internal static int[] ParmNameIndex = new int[MaxParameters]
        {
            0 ,    //     		epColorado_River_Flow
            1 ,    //     		epSimulation_Start_Year
            2 ,    //     		epSimulation_End_Year
            3 ,    //     		epPopulation_Used
            4 ,    //     		epSaltVerde_Storage
            5 ,    //     		epEffluent_To_Agriculture
            6 ,    //     		epGroundwater_Pumped_Municipal
            7 ,    //     		epGroundwater_Balance
            8 ,    //     		epSaltVerde_Annual_Deliveries_SRP
            9 ,    //     		epSaltVerde_Class_BC_Designations
            10 ,    //     		epColorado_Annual_Deliveries
            11 ,    //     		epGroundwater_Bank_Used
            12 ,    //     		epGroundwater_Bank_Balance
            13 ,    //     		epReclaimed_Water_Used
            14 ,    //     		epReclaimed_Water_To_Vadose
            15 ,    //     		epReclaimed_Water_Discharged
            16 ,    //     		epReclaimed_Water_to_DirectInject
            17 ,    //     		epRO_Reclaimed_Water_Used
            18 ,    //     		epRO_Reclaimed_Water_to_DirectInject
            19 ,    //     		epEffluent_Reused
            20 ,    //     		epEffluent_To_Vadose
            21 ,    //     		epEffluent_To_PowerPlant
            22 ,    //     		epEffluent_Discharged
            23 ,    //     		epDemand_Deficit
            24 ,    //     		epTotal_Demand
            25 ,    //     		epOnProjectDemand
            26 ,    //     		epOffProjectDemand
            27 ,    //     		epGPCD_Used
            28 ,    //     		epSaltVerde_River_Flow
            29 ,    //     		epSaltVerde_Spillage
            30 ,    //     		epPowell_Storage
            31 ,    //     		epMead_Storage
            32 ,    //     		epColorado_Historical_Extraction_Start_Year
            33 ,    //     		epColorado_Historical_Data_Source
            34 ,    //     		epColorado_Climate_Adjustment_Percent
            35 ,    //     		epColorado_User_Adjustment_Percent
            36 ,    //     		epColorado_User_Adjustment_StartYear
            37 ,    //     		epColorado_User_Adjustment_Stop_Year
            38 ,    //     		epSaltVerde_Historical_Extraction_Start_Year
            39 ,    //     		epSaltVerde_Historical_Data
            40 ,    //     		epSaltVerde_Climate_Adjustment_Percent
            41 ,    //     		epSaltVerde_User_Adjustment_Percent
            42 ,    //     		epSaltVerde_User_Adjustment_Start_Year
            43 ,    //     		epSaltVerde_User_Adjustment_Stop_Year
            44 ,    //     		epProvider_Demand_Option
            45 ,    //     		epPCT_Alter_GPCD
            46 ,    //     		epModfyNormalFlow
            47 ,    //     		epWaterAugmentation
            48 ,    //     		epWaterAugmentationUsed
            49 ,    //     		epUse_GPCD
            50 ,    //     		epPCT_WasteWater_to_Reclaimed
            51 ,    //     		epPCT_Wastewater_to_Effluent
            52 ,    //     		epPCT_Reclaimed_to_RO
            53 ,    //     		epPCT_RO_to_Water_Supply
            54 ,    //     		epPCT_Reclaimed_to_DirectInject
            55 ,    //     		epPCT_Reclaimed_to_Water_Supply
            56 ,    //     		epPCT_Reclaimed_to_Vadose
            57 ,    //     		epPCT_Effluent_to_Vadose
            58 ,    //     		epPCT_Effluent_to_PowerPlant
            59 ,    //     		epSurfaceWater__to_Vadose
            60 ,    //     		epSurface_to_Vadose_Time_Lag
            61 ,    //     		epWaterBank_Source_Option
            62 ,    //     		epPCT_SurfaceWater_to_WaterBank
            63 ,    //     		epUse_SurfaceWater_to_WaterBank
            64 ,    //     		epPCT_WaterSupply_to_Residential
            65 ,    //     		epPCT_WaterSupply_to_Commercial
            66 ,    //     		epUse_WaterSupply_to_DirectInject
            67 ,    //     		epPCT_Outdoor_WaterUse
            68 ,    //     		epPCT_Groundwater_Treated
            69 ,    //     		epPCT_Reclaimed_Outdoor_Use
            70 ,    //     		epPCT_Growth_Rate_Adjustment_OnProject
            71 ,    //     		epPCT_Max_Demand_Reclaim
            72 ,    //     		epPCT_REG_Growth_Rate_Adjustment
            73 ,    //     		epSetPopulationsOn
            74 ,    //     		epSetPopulationsOther
            75 ,    //     		epPCT_WaterSupply_to_Industrial
            76 ,    //     		epPCT_Outdoor_WaterUseRes
            77 ,    //     		epPCT_Outdoor_WaterUseCom
            78 ,    //     		epPCT_Outdoor_WaterUseInd
            79 ,    //     		epOnProjectPopulation
            80 ,    //     		epOtherPopulation
            81 ,    //     		epMeadLevel
            82 ,    //     		epAWSAnnualGWLimit
            83 ,    //     		epPCT_Growth_Rate_Adjustment_Other
            84 ,    //     		epAnnualIncidental
            85 ,    //     		epVadoseToAquifer
            86 ,    //     		epRegionalNaturalRecharge
            87 ,    //     		epRegionalCAGRDRecharge
            88 ,    //     		epRegionalInflow
            89 ,    //     		epRegionalAgToVadose
            90 ,    //     		epRegionalProviderRecharge
            91 ,    //     		epRegionalAgOtherPumping
            92 ,    //     		epRegionalOutflow
            93 ,    //     		epRegionalGWBalance
            94 ,    //     		epPowellLevel
            95 ,    //     		epAlterGPCDpct
            96 ,    //     		epGPCDraw
            97 ,    //     		epRESdefault
            98 ,    //     		epCOMdefault
            99 ,    //     		epINDdefault
            100 ,    //     		epProvider_Max_NormalFlow
            101 ,    //     		epProvider_WaterFromAgPumping
            102 ,    //     		epProvider_WaterFromAgSurface
            103 ,    //     		epProvider_WaterFromAgPumpingMax
            104 ,    //     		epProvider_WaterFromAgSurfaceMax
            
            110,     //         epSaltOther_Storage;
            111,     //         epRoosevelt_Storage;
            112,     //         epVerde_Storage;
            113,     //         epVerde_AnnualFlow
            114,    //          epSaltTonto_AnnualFlow

            200 ,    //     		MaxBasicParameter
            201 ,    //     		FirstDerivedParameter
            201 ,    //     		epTotalSupplyUsed
            202 ,    //     		epPCTGWofDemand
            203 ,    //     		epTotalReclaimedUsed
            204 ,    //     		epProvderEffluentToAg
            205 ,    //     		epTotalEffluent
            206 ,    //     		epProjectedOnProjectPop
            207 ,    //     		epProjectedOtherPop
            208 ,    //     		epProjectedTotalPop
            209 ,    //     		epDifferenceProjectedOtherPop
            210 ,    //     		epDifferenceProjectedOnPop
            211 ,    //     		epDifferenceTotalPop
            212 ,    //     		epPCTDiffProjectedOnPop
            213 ,    //     		epPCTDiffProjectedOtherPop
            214 ,    //     		epPCTDiffProjectedTotalPop
            250 ,    //     		epEnvironmentalFlow_PCT
            251 ,    //     		epEnvironmentalFlow_AFa

            260 ,    //     		epTotalReclaimedCreated_AF
            261 ,    //     		epTWWTPCreated_AF
            300 ,    //     		LastDerivedParameter
            301 ,    //     		FirstSustainableParameter
            301 ,    //     		epPCT_Deficit
            302 ,    //     		epDeficit_Years
            303 ,    //     		epDeficit_Total
            304 ,    //     		epPCT_GWAvailable
            305 ,    //     		epYrsGWZero
            306 ,    //     		epYearGWGoesZero
            307 ,    //     		epYearsNotAssured
            308 ,    //     		epPctRecOfTotal
            309 ,    //     		epAgSustainIndicator
            310 ,    //     		epEnvSustainIndicator
            311 ,    //     		epWaterUseIndicator
            312 ,    //     		epGWYrsSustain
            313 ,    //     		epRegGWYrsSustain
            314 ,    //     		epRegPctGWDemand
            315 ,    //     		epMeadDeadPool
            316 ,    //     		epMeadLowerSNWA
            317 ,    //     		epMeadShortageSharingLow
            318 ,    //     		epMeadMinPower
            319 ,    //     		epMeadCapacity
            320 ,    //             epMeadShortageSharingMed;
            321, //                  epMeadShortageSharingHigh;

            400 ,    //     		LastSustainableParameter
            501 ,    //     		FirstProcessParameter
            502 ,    //     		epPercentDeficitLimit
            503 ,    //     		epMinimumGPCD
            504 ,    //     		epYearsOfNonAWSTrigger
            505 ,    //     		epYearsOfNonAWSGPCDTrigger
            600 ,    //     		LastProcessParameter
            601, //                 epWebPop_GrowthRateAdj_PCT
            602 ,    //     		epWebUIeffluent
            603 ,    //     		epWebUIeffluent_Ag
            604 ,    //     		epWebUIeffluent_Env
            605 ,    //     		epWebUIAgriculture
            606 ,    //     		epWebUIPersonal_PCT
            607 ,   //               epWebWaterBank_PCT
            701 ,    //     		FirstGWParameter
            701 ,    //     		epGroundwater_Model
            702 ,    //     		epCreditDeficits
            900 ,    //     		LastGWParameter
            1501 ,    //     		FirstUserDefinedParameter
            1700     //     		LastUserDefinedParameter


        };


        internal static string[] ParmNames = new string[MaxParameters]
        {
            "epColorado_River_Flow",  //     			        = 0"
            "epSimulation_Start_Year",  //     					 = 1"
            "epSimulation_End_Year",  //     					 = 2"
            "epPopulation_Used",  //     					    = 3"
            "epSaltVerde_Storage",  //     					    = 4"
            "epEffluent_To_Agriculture",  //     				= 5"
            "epGroundwater_Pumped_Municipal",  //     			= 6"
            "epGroundwater_Balance",  //     					= 7"
            "epSaltVerde_Annual_Deliveries_SRP",  //     		= 8"
            "epSaltVerde_Class_BC_Designations",  //     		= 9"
            "epColorado_Annual_Deliveries",  //     			= 10"
            "epGroundwater_Bank_Used",  //     					= 11"
            "epGroundwater_Bank_Balance",  //     				= 12"
            "epReclaimed_Water_Used",  //     					= 13"
            "epReclaimed_Water_To_Vadose",  //     				= 14"
            "epReclaimed_Water_Discharged",  //     			 = 15"
            "epReclaimed_Water_to_DirectInject",  //     		 = 16"
            "epRO_Reclaimed_Water_Used",  //     				 = 17"
            "epRO_Reclaimed_Water_to_DirectInject",  //     	 = 18"
            "epEffluent_Reused",  //     					    = 19"
            "epEffluent_To_Vadose",  //     					 = 20"
            "epEffluent_To_PowerPlant",  //     				 = 21"
            "epEffluent_Discharged",  //     					 = 22"
            "epDemand_Deficit",  //     					    = 23"
            "epTotal_Demand",  //     					        = 24"
            "epOnProjectDemand",  //     					    = 25"
            "epOffProjectDemand",  //     					    = 26"
            "epGPCD_Used",  //     					            = 27"
            "epSaltVerde_River_Flow",  //     					= 28"
            "epSaltVerde_Spillage",  //     					 = 29"
            "epPowell_Storage",  //     			    		= 1"
            "epMead_Storage",  //     					        = 2"
            "epColorado_Historical_Extraction_Start_Year",  //  = 32"
            "epColorado_Historical_Data_Source",  //     		= 33"
            "epColorado_Climate_Adjustment_Percent",  //     	 = 34"
            "epColorado_User_Adjustment_Percent",  //     		 = 35"
            "epColorado_User_Adjustment_StartYear",  //     	 = 36"
            "epColorado_User_Adjustment_Stop_Year",  //     	 = 37"
            "epSaltVerde_Historical_Extraction_Start_Year",  //  = 38"
            "epSaltVerde_Historical_Data",  //     				= 39"
            "epSaltVerde_Climate_Adjustment_Percent",  //     	 = 40"
            "epSaltVerde_User_Adjustment_Percent",  //     		 = 41"
            "epSaltVerde_User_Adjustment_Start_Year",  //     	 = 42"
            "epSaltVerde_User_Adjustment_Stop_Year",  //     	 = 43"
            "epProvider_Demand_Option",  //     				 = 44"
            "epPCT_Alter_GPCD",  //     					    = 45"
            "epModfyNormalFlow",  //     					    = 46"
            "epWaterAugmentation",  //     					    = 47"
            "epWaterAugmentationUsed",  //     					= 48"
            "epUse_GPCD",  //     					            = 49"
            "epPCT_WasteWater_to_Reclaimed",  //     			 = 50"
            "epPCT_Wastewater_to_Effluent",  //     			 = 51"
            "epPCT_Reclaimed_to_RO",  //     					 = 52"
            "epPCT_RO_to_Water_Supply",  //     				= 53"
            "epPCT_Reclaimed_to_DirectInject",  //     			 = 54"
            "epPCT_Reclaimed_to_Water_Supply",  //     			 = 55"
            "epPCT_Reclaimed_to_Vadose",  //     				= 56"
            "epPCT_Effluent_to_Vadose",  //     				 = 57"
            "epPCT_Effluent_to_PowerPlant",  //     			 = 58"
            "epSurfaceWater__to_Vadose",  //     				 = 59"
            "epSurface_to_Vadose_Time_Lag",  //     			 = 60"
            "epWaterBank_Source_Option",  //     				 = 61"
            "epPCT_SurfaceWater_to_WaterBank",  //     			 = 62"
            "epUse_SurfaceWater_to_WaterBank",  //     			 = 63"
            "epPCT_WaterSupply_to_Residential",  //     		 = 64"
            "epPCT_WaterSupply_to_Commercial",  //     			 = 65"
            "epUse_WaterSupply_to_DirectInject",  //     			 = 66"
            "epPCT_Outdoor_WaterUse",  //     					= 67"
            "epPCT_Groundwater_Treated",  //     				 = 68"
            "epPCT_Reclaimed_Outdoor_Use",  //     				 = 69"
            "epPCT_Growth_Rate_Adjustment_OnProject",  //     	 = 70"
            "epPCT_Max_Demand_Reclaim",  //     				 = 71"
            "epPCT_REG_Growth_Rate_Adjustment",  //     		 = 72"
            "epSetPopulationsOn",  //     					    = 73"
            "epSetPopulationsOther",  //     					 = 74"
            "epPCT_WaterSupply_to_Industrial",  //     			 = 75"
            "epPCT_Outdoor_WaterUseRes",  //     				 = 76"
            "epPCT_Outdoor_WaterUseCom",  //     				 = 77"
            "epPCT_Outdoor_WaterUseInd",  //     				 = 78"
            "epOnProjectPopulation",  //     					 = 79"
            "epOtherPopulation",  //     					    = 80"
            "epMeadLevel",  //     					            = 81"
            "epAWSAnnualGWLimit",  //     					    = 82"
            "epPCT_Growth_Rate_Adjustment_Other",  //     		 = 83"
            "epAnnualIncidental",  //     					    = 84"
            "epVadoseToAquifer",  //     					    = 85"
            "epRegionalNaturalRecharge",  //     				 = 86"
            "epRegionalCAGRDRecharge",  //     					 = 87"
            "epRegionalInflow",  //     					    = 88"
            "epRegionalAgToVadose",  //     					 = 89"
            "epRegionalProviderRecharge",  //     				= 90"
            "epRegionalAgOtherPumping",  //     				= 91"
            "epRegionalOutflow",  //     					    = 92"
            "epRegionalGWBalance",  //     					    = 93"
            "epPowellLevel",  //     					        = 94"
            "epAlterGPCDpct",  //     					        = 95"
            "epGPCDraw",  //     					            = 96"
            "epRESdefault",  //     					        = 97"
            "epCOMdefault",  //     					        = 98"
            "epINDdefault",  //     					        = 99"
            "epProvider_Max_NormalFlow",  //     				= 100"
            "epProvider_WaterFromAgPumping",  //     			 = 101"
            "epProvider_WaterFromAgSurface",  //     			 = 102"
            "epProvider_WaterFromAgPumpingMax",  //     		 = 103"
            "epProvider_WaterFromAgSurfaceMax",  //     		 = 104"
            //"epWebPop_GrowthRateAdj_PCT",  //     				 = 105"
            "epSaltOther_Storage", //                            =  110,     //         ;
            "epRoosevelt_Storage", //                            = 111,     //         ;
            "epVerde_Storage",    //                             = 112   
            "epVerde_AnnualFlow", //                            = 113;
            "epSaltTonto_AnnualFlow", //                         = 114;

            "MaxBasicParameter",  //     					    = 200"
            "FirstDerivedParameter",  //     					 = 201"
            "epTotalSupplyUsed",  //     					    = 201"
            "epPCTGWofDemand",  //     					        = 202"
            "epTotalReclaimedUsed",  //     					 = 203"
            "epProvderEffluentToAg",  //     					 = 204"
            "epTotalEffluent",  //     					        = 205"
            "epProjectedOnProjectPop",  //     					 = 206"
            "epProjectedOtherPop",  //     					    = 207"
            "epProjectedTotalPop",  //     					    = 208"
            "epDifferenceProjectedOtherPop",  //     			 = 209"
            "epDifferenceProjectedOnPop",  //     				 = 210"
            "epDifferenceTotalPop",  //     					 = 211"
            "epPCTDiffProjectedOnPop",  //     					 = 212"
            "epPCTDiffProjectedOtherPop",  //     					 = 213"
            "epPCTDiffProjectedTotalPop",  //     					 = 214"
            "epEnvironmentalFlow_PCT",  //     					 = 250"
            "epEnvironmentalFlow_AFa",  //     					 = 251"
            "epTotalReclaimedCreated_AF",  //     			 = 260"
            "epTWWTPCreated_AF",  //     					 = 261"
            "LastDerivedParameter",  //     				= 300"
            "FirstSustainableParameter",  //     			 = 301"
            "epPCT_Deficit",  //     					    = 301"
            "epDeficit_Years",  //     					    = 302"
            "epDeficit_Total",  //     					    = 303"
            "epPCT_GWAvailable",  //     					 = 304"
            "epYrsGWZero",  //     					         = 305"
            "epYearGWGoesZero",  //     					 = 306"
            "epYearsNotAssured",  //     					 = 307"
            "epPctRecOfTotal",  //     					    = 308"
            "epAgSustainIndicator",  //     				 = 309"
            "epEnvSustainIndicator",  //     				= 310"
            "epWaterUseIndicator",  //     					 = 311"
            "epGWYrsSustain",  //     					    = 312"
            "epRegGWYrsSustain",  //     					 = 313"
            "epRegPctGWDemand",  //     					 = 314"
            "epMeadDeadPool",  //     					     = 315"
            "epMeadLowerSNWA",  //     					    = 316"
            "epMeadShortageSharingLow",  //     				 = 317"
            "epMeadMinPower",  //     					    = 318"
            "epMeadCapacity",  //     					    = 319"
            "epMeadShortageSharingMed",                   //  = 320;
            "epMeadShortageSharingHigh",                  // = 321;

            "LastSustainableParameter",  //     			 = 400"
            "FirstProcessParameter",  //     				= 501"
            "epPercentDeficitLimit",  //     				= 502"
            "epMinimumGPCD",  //     					    = 503"
            "epYearsOfNonAWSTrigger",  //     				 = 504"
            "epYearsOfNonAWSGPCDTrigger",  //     			 = 505"
            "LastProcessParameter",  //     				= 600"
            "epWebPop_GrowthRateAdj_PCT",  //     	   	 = 601"
            "epWebUIeffluent",  //     					    = 602"
            "epWebUIeffluent_Ag",  //     					 = 603"
            "epWebUIeffluent_Env",  //     					 = 604"
            "epWebUIAgriculture",  //     					 = 605"
            "epWebUIPersonal_PCT",  //     					    = 606"
            "epWebWaterBank_PCT",  //     					    = 607"
            
            "FirstGWParameter",  //     					 = 701"
            "epGroundwater_Model",  //     					 = 701"
            "epCreditDeficits",  //     					 = 702"
            "LastGWParameter",  //     					    = 900"
            "FirstUserDefinedParameter",  //     			 = 1501"
            "LastUserDefinedParameter"  //     				 = 1700"

        };

        //-------------------------------------------------------------------------------------------------
        // <summary>   Names of the Parameters. </summary>
        
        // <param name="modelparam">   The modelparam. </param>
        
        // <returns>   . </returns>
        //-------------------------------------------------------------------------------------------------

        //public static string Names(int modelparam)
        //{
        //    int index = Array.BinarySearch(ParmNameIndex, modelparam);
        //    if (index > -1)
        //    {
        //        return ParmNames[index];
        //    }
        //    else
        //    {
        //        return "Undefined #" + modelparam.ToString();
        //    }
        //}
        /// <summary>   Sets the names. </summary>
        /// <remarks> Use this partial method to assign the names of the parameter values assigned</remarks>
        static partial void SetNames()
        {
            for (int i = 0; i < ParmNames.Length; i++)
            {
                FNames[ParmNameIndex[i]] = ParmNames[i];
            }
        }

    };

    #endregion

    #region Providers
    /// <summary>
    /// enum values for each of the Water Providers
    /// </summary>
    /// <remarks>This is a key enumerator for all the provide input and output paramters, data stored in 
    /// provider arrays is stored in the order of these enum values.  Most provider data structures allow indexing based
    /// on these enum values and the ProviderClass provides an IEnumerator that returns enum values using a foreach () loop.  
    /// The exception to this is the providers past the ProviderClass.LastProvider eProvider value which are not considered a valid provider by most routines
    /// Test for a valid provider using ProviderClass.valid(int index)</remarks>
    public enum eProvider
    {
        /// <summary>Adaman Mutual Utility Company</summary>
        Adaman_Mutual,
        /// <summary> White Tanks Utility Company</summary>
        White_Tanks,
        /// <summary> Paradise Valley Utility Company</summary>
        Paradise_Valley,
        /// <summary> Sun City Utility Company </summary>
        Sun_City,
        /// <summary> Sun City West Utility Company </summary>
        Sun_City_West,
        /// <summary> City of Avondale. </summary>
        Avondale,
        /// <summary> Berneil Utility Company </summary>
        Berneil,
        /// <summary> City of Buckeye. </summary>
        Buckeye,
        /// <summary> Carefree Utility Company </summary>
        Carefree,
        /// <summary> Town of Cave Creek. </summary>
        Cave_Creek,
        /// <summary> City of Chandler. </summary>
        Chandler,
        /// <summary>Chaparral City Utility Company  . </summary>
        Chaparral_City,
        /// <summary> City of Surprise. </summary>
        Surprise,
        /// <summary> Utility Company . </summary>
        Clearwater_Utilities,
        /// <summary> Utility Company  </summary>
        Desert_Hills,
        /// <summary> Town of El Mirage . </summary>
        El_Mirage,
        /// <summary> City of Gilbert. </summary>
        Gilbert,
        /// <summary> City of Glendale. </summary>
        Glendale,
        /// <summary> City of Goodyear. </summary>
        Goodyear,
        /// <summary> Utility Company  </summary>
        Litchfield_Park,
        /// <summary> City of Mesa. </summary>
        Mesa,
        /// <summary> City of Peoria. </summary>
        Peoria,
        /// <summary> City of Phoenix. </summary>
        Phoenix,
        /// <summary> Town of Queen Creek. </summary>
        Queen_Creek,
        /// <summary> Rigby Utility Company  </summary>
        Rigby,
        /// <summary> Rio Verde Utility Company . </summary>
        Rio_Verde,
        /// <summary> Rose Valley Utility Company . </summary>
        Rose_Valley,
        /// <summary> City of Scottsdale. </summary>
        Scottsdale,
        /// <summary> Sunrise Utility Company . </summary>
        Sunrise,
        /// <summary> City of Tempe. </summary>
        Tempe,
        /// <summary> City of Tolleson. </summary>
        Tolleson,
        /// <summary> Valley Utility Company . </summary>
        Valley_Utilities,
        /// <summary> West End Utility Company . </summary>
        West_End,
        // All eProviders from this point on are not consider a valid provider enum by most routines
        /// <summary> Regional Summary. </summary>
        /// <remarks>Currently Not Supported</remarks>
        Regional,
        /// <summary> Summary of Areas On Project. </summary>
        /// <remarks>Currently Not Supported</remarks>
        OnProject,
        /// <summary> Summary of Areas Off Project. </summary>
        /// <remarks>Currently Not Supported</remarks>
        OffProject

    };
    //---------------------------------------------------------

   

    public static partial class ProviderClass
    {
        // Provider Routines, Constants and enums
        /// <summary>
        /// The last valid provider enum value
        /// </summary>
        /// <value>eProvider enum</value>
        public const eProvider LastProvider = eProvider.West_End;

        /// <summary>
        /// The first valid enum value
        /// </summary>
        /// <value>eProvider enum</value>
        public const eProvider FirstProvider = eProvider.Adaman_Mutual;

        /// <summary>
        /// The Last valid Aggregator value
        /// </summary>
        /// <value>eProvider enum</value>
        public const eProvider LastAggregate = eProvider.OffProject;

        /// <summary>
        /// The number of valid Provider (eProvider) enum values for use with WaterSimModel and ProviderIntArray.
        /// </summary>
        /// <value>count of valid eProvider enums</value>
        /// <remarks>all providers after LastProvider are not considered one of the valid eProvider enum value</remarks>
        public const int NumberOfProviders = (int)LastProvider + 1;

        /// <summary>
        /// The number of valid Provide Aggregate (eProvider) enum values.
        /// </summary>
        /// <value>count of valid eProvider enums</value>
        /// <remarks>all providers after LastProvider are not considered one of the valid eProvider enum value</remarks>
        public static int NumberOfAggregates = ((int)LastAggregate - (int)LastProvider);

        internal const int TotalNumberOfProviderEnums = ((int)LastAggregate)+ 1;

        private static string[] ProviderNameList = new string[TotalNumberOfProviderEnums]    {       
  
            "Adaman Mutual",
            "White Tanks",
            "Paradise Valley",
            "Sun City",
            "Sun City West",
            "Avondale",
            "Berneil",
            "Buckeye",
            "Carefree",
            "Cave Creek",
            "Chandler",
            "Chaparral City",
            "Surprise",
            "Clearwater Utilities",
            "Desert Hills",
            "El Mirage",
            "Gilbert",
            "Glendale",
            "Goodyear",
            "Litchfield Park",
            "Mesa",
            "Peoria",
            "Phoenix",
            "Queen Creek",
            "Rigby",
            "Rio Verde",
            "Rose Valley",
            "Scottsdale",
            "Sunrise",
            "Tempe",
            "Tolleson",
            "Valley Utilities",
            "West End",
            "Region",

            "On Project",
            "Off Project"
             };

        private static string[] FieldNameList = new string[TotalNumberOfProviderEnums]  {       
  
       "ad", //providers.Adaman_Mutual;
       "wt", //providers.White_Tanks;
       "pv", //providers.Paradise_Valley;
       "su", //providers.Sun_City;
       "sw", //providers.Sun_City_West;
       "av", //providers.Avondale;
       "be", //providers.Berneil;
       "bu", //providers.Buckeye;
       "cf", //providers.Carefree;
       "cc", //providers.Cave_Creek;
       "ch", //providers.Chandler;
       "cp", //providers.Chaparral_City;
       "sp", //providers.Surprise;
       "cu", //providers.Clearwater_Utilities;
       "dh", //providers.Desert_Hills;
       "em", //providers.El_Mirage;
       "gi", //providers.Gilbert;
       "gl", //providers.Glendale;
       "go", //providers.Goodyear;
       "lp", //providers.Litchfield_Park;
       "me", //providers.Mesa;
       "pe", //providers.Peoria;
       "ph", //providers.Phoenix;
       "qk", //providers.Queen_Creek;
       "rg", //providers.Rigby;
       "rv", //providers.Rio_Verde;
       "ry", //providers.Rose_Valley;
       "sc", //providers.Scottsdale;
       "sr", //providers.Sunrise;
       "te", //providers.Tempe;
       "to", //providers.Tolleson;
       "vu", //providers.Valley_Utilities;
       "we", //providers.West_End;
       "reg", // eProvider.Region 
       "onp", // eProvider.OnProject
       "ofp" // eProvider.OffProject
       // "aj", // Apache Junction
       // "an", // American Water Anthem
    };

        private static eProvider[] RegionProviders = new eProvider[NumberOfProviders] {
            eProvider.Adaman_Mutual,
            eProvider.White_Tanks,
            eProvider.Paradise_Valley,
            eProvider.Sun_City,
            eProvider.Sun_City_West,
            eProvider.Avondale,
            eProvider.Berneil,
            eProvider.Buckeye,
            eProvider.Carefree,
            eProvider.Cave_Creek,
            eProvider.Chandler,
            eProvider.Chaparral_City,
            eProvider.Surprise,
            eProvider.Clearwater_Utilities,
            eProvider.Desert_Hills,
            eProvider.El_Mirage,
            eProvider.Gilbert,
            eProvider.Glendale,
            eProvider.Goodyear,
            eProvider.Litchfield_Park,
            eProvider.Mesa,
            eProvider.Peoria,
            eProvider.Phoenix,
            eProvider.Queen_Creek,
            eProvider.Rigby,
            eProvider.Rio_Verde,
            eProvider.Rose_Valley,
            eProvider.Scottsdale,
            eProvider.Sunrise,
            eProvider.Tempe,
            eProvider.Tolleson,
            eProvider.Valley_Utilities,
            eProvider.West_End 
        };

        private static eProvider[] OnProjectProviders = new eProvider[10] {
            eProvider.Avondale,
            eProvider.Chandler,
            eProvider.Gilbert,
            eProvider.Glendale,
            eProvider.Mesa,
            eProvider.Peoria,
            eProvider.Phoenix,
            eProvider.Scottsdale,
            eProvider.Tempe,
            eProvider.Tolleson,
        };

        private static eProvider[] OffProjectProviders = new eProvider[23] {
            eProvider.Adaman_Mutual,
            eProvider.White_Tanks,
            eProvider.Paradise_Valley,
            eProvider.Sun_City,
            eProvider.Sun_City_West,
            eProvider.Berneil,
            eProvider.Buckeye,
            eProvider.Carefree,
            eProvider.Cave_Creek,
            eProvider.Chaparral_City,
            eProvider.Surprise,
            eProvider.Clearwater_Utilities,
            eProvider.Desert_Hills,
            eProvider.El_Mirage,
            eProvider.Goodyear,
            eProvider.Litchfield_Park,
            eProvider.Queen_Creek,
            eProvider.Rigby,
            eProvider.Rio_Verde,
            eProvider.Rose_Valley,
            eProvider.Sunrise,
            eProvider.Valley_Utilities,
            eProvider.West_End 
        
        };

        public static eProvider[] GetRegion(eProvider ep)
        {
            switch (ep)
            {
                case eProvider.Regional:
                    return RegionProviders;
                case eProvider.OffProject:
                    return OffProjectProviders;
                case eProvider.OnProject:
                    return OnProjectProviders;
                default:
                    return null;
            }
        }

    }

    enum ColoradoRiverRecord { eUnspecified, ePaleoRecord, eBureauRecord, eUserSupplied };

#endregion
            #region Utils

        internal static class util
    {
        #region apiconstants
        //====================================================================
        // CONSTANTS  ENUMS STRUCTS
        // THESE CAN NOT BE CHANGED WITH OUT CHANGING DATA INPUTS!
        // There is no error checking for this
        internal const int ModelParamterUseDefault = -1;

        internal const int WaterSimDCDC_Default_Simulation_StartYear = 2000; // changed RQ 4 15 2012  OLD - 2006;
        //internal const int WaterSimDCDC_Default_Simulation_EndYear = 2085;
        internal const int WaterSimDCDC_Default_Simulation_EndYear = 2084;
        internal const int WaterSimDCDC_Default_Simulation_Max_StartYear = 2013;
        internal const int WaterSimDCDC_Default_Simulation_Min_StopYear = WaterSimDCDC_Default_Simulation_StartYear+1;
        //internal const int WaterSimDCDC_Default_Colorado_Historical_Extraction_Start_Year = 1978;
        //internal const int WaterSimDCDC_Default_SaltVerde_Historical_Extraction_Start_Year = 1978;

        internal const int WaterSimDCDC_Default_ProviderDemandOption = 0;
        internal const int WaterSimDCDC_Default_SaltVerde_Historical_Data = 1; // changed RQ 4 15 2012  OLD - 2;
        //internal const int WaterSimDCDC_Default_Colorado_Historical_Data = 1; 
        // 05.11.12
        internal const int WaterSimDCDC_Default_SaltVerde_Trace_length = 30;
        internal const int WaterSimDCDC_Default_Colorado_Trace_length = 30;
 
        // Fixes
        internal const int WaterSimDCDC_Default_Colorado_User_Adjustment_StartYear = 2013;
        //internal const int WaterSimDCDC_Default_SaltVerde_Historical_Data = 1;
        internal const int WaterSimDCDC_Default_SaltVerde_User_Adjustment_Start_Year = 2013;
        internal const int WaterSimDCDC_Provider_Demand_Option = 3;
        internal const int DefaultReduceGPCDValue = 0;  // changed rq 8 10 15 old was 10// changed RQ 4 15 2012 OLD - 1;
        // Fix for provider inputs
        internal const int WaterSimDCDC_Default_Use_GPCD = 150;
        internal const int WaterSimDCDC_Default_Surface_to_Vadose_Time_Lag_in_Years = 25;
        internal const int WaterSimDCDC_Default_SurfaceWater_to_WaterBank = 0;

        //=================================================================================
        internal const int WaterSimDCDC_Default_Colorado_Historical_Data_Source = (int)ColoradoRiverRecord.eBureauRecord;

        //public const int    ProviderClass.NumberOfProviders  = 33;

        public const int UseDefaultInput = 0;
        #endregion

        #region  utilroutines
        /************************************************
         * ROUTINES
         * 
         * 
         * *********************************************/
        //----------------------------------------------------------------

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Fillzeroes. </summary>
        ///
        /// <remarks> Fills an int array with zeros</remarks>
        ///
        /// <param name="values"> The int array to fill </param>
        ///-------------------------------------------------------------------------------------------------

        public static void fillzero(int[] values)
        {
            int cnt = values.Length;
            for (int i = 0; i < cnt; i++)
            { values[i] = 0; }
        }
        //----------------------------------------------------------------

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Fillvalues. </summary>
        ///
        /// <remarks> Fills and int array wit the specified value </remarks>
        ///
        /// <param name="values"> The int array to fill. </param>
        /// <param name="value">  The value to use for fill. </param>
        ///-------------------------------------------------------------------------------------------------

        public static void fillvalues(int[] values, int value)
        {
            int cnt = values.Length;
            for (int i = 0; i < cnt; i++)
            { values[i] = value; }
        }

        //----------------------------------------------------------------


        const double GALLONSINACREFOOT = 325851.429;

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Fill default. </summary>
        /// <remarks> Fills an int array with the default int value = UseDefaultInput </remarks>
        /// <param name="values"> The int array to fill. </param>
        ///-------------------------------------------------------------------------------------------------

        public static void fillDefault(int[] values)
        {
            int cnt = values.Length;
            for (int i = 0; i < cnt; i++)
            { values[i] = UseDefaultInput; }
        }
        #endregion

        public static double ConvertAFtoGallons(int Acrefeet)
        {
            return Convert.ToDouble(Acrefeet) * GALLONSINACREFOOT;
        }

        public static int ConvertGallonstoAF(double Gallons)
        {
            return Convert.ToInt32(Gallons / GALLONSINACREFOOT);
        }
        public static double CalcGPCD(double Gallons, int Pop)
        {
            return ((Gallons / Convert.ToDouble(Pop)) / 365.0);
        }
    }

        internal static class utilities
        {
            public static double Difference(double Response, double Control)
            {
                double Out = 0;
                if (0 < Control)
                {
                    Out =((Response - Control) / Control) * 100;
                }
                return Out;
            }
            //
            public static double Slope(double X1, double X2, double Y1, double Y2)
            {
                return (Y1 - Y2) / (X1 - X2);
            }
            //
            public static double Calc_GPCD(double Gallons, int Pop,int year)
            {
               
                return ((Gallons / Convert.ToDouble(Pop)) / leapyear(year));
            }
            internal static double leapyear(int y)
            {
                double days = 0;
              
                    if ((y % 4 == 0 && y % 100 != 0) || (y % 400 == 0))
                    {
                        // is a Leap Year
                        days = 366.0;
                    }
                    else
                    {
                        // Is not a Leap Year
                        days = 365.0;
                    }
                
            return days;
            }

        }

        
        #endregion
}
