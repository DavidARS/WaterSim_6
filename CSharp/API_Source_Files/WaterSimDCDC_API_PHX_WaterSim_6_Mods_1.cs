using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using WaterSimDCDC.Documentation;


namespace WaterSimDCDC
{
    public static partial class eModelParam
    {
        public const int epProvider_IwaniecScenario = 145;
        public const int epProvider_RainWaterHarvested = 150;
        public const int epProvider_StormWaterHarvested = 151;
        public const int epProvider_RainWaterHarvestedSF = 152;
        public const int epProvider_RainWaterHarvestedMF = 153;
        public const int epProvider_RainWaterHarvestedPU = 154;
        public const int epProvider_ResGrayWaterUsed = 155;
        public const int epProvider_ComGrayWaterUsed = 156;
        public const int epProvider_IndGrayWaterUsed = 157;
        public const int epProvider_ResIndoorGPCD_LD = 158;
        public const int epProvider_ResIndoorGPCD_MD = 159;
        public const int epProvider_ResIndoorGPCD_HD = 160;
        public const int epProvider_ResOutdoorGPCD_LD = 161;
        public const int epProvider_ResOutdoorGPCD_MD = 162;
        public const int epProvider_ResOutdoorGPCD_HD = 163;
        //
        //public const int epOutdoorUsePct_reclaimed = 164;
        //
        public const int epRainHarvestedToTotalOutdoor = 167;
        public const int epGrayWaterToTotalOutdoor = 168;
        public const int epReclaimedToTotalOutdoor = 169;
        public const int epDemand_Agriculture = 170;
        public const int epDemand_MediumDensity = 175;
        public const int epDemand_HighDensity = 176;
        public const int epDemand_LowDensity = 177;
        public const int epDemand_Turf = 178;
        public const int epDemand_Greenway = 179;
        public const int epDemand_Tree = 180;
        public const int epProvider_Nonpotable = 181;
        //
        public const int epRainFallFactor = 182;
        public const int epRainHarvestCompliance = 183;
        public const int epRainHarvestInflection = 184;

        public const int epResidentialIndoorWaterUse = 171;
        public const int epResidentialOutdoorWaterUse = 172;
        //
        public const int epGPCDefficiencyLCLU = 173;
        // 08.25.18
        public const int epStormwater = 174;

    }
    public partial class WaterSimManager 
    {
       // -------------------------------------------------------------------------------------------------------------------------------------------
       //-------------------------------------------------------------
       // Values 0 to 200 reserved for Basic Model Inpus and Outputs
        /// <summary> The maximum basic parameter. </summary>
        public const int MaxBasicParameter = 200;
        //
        //public const int mpProvider_IwaniecScenario = 145;
        public const int mpProvider_StormWaterCapacity = 146;
        public const int mpProvider_EffluentUsedAg = 147;
        public const int mpProvider_SurfaceUsedAg = 148;
        public const int mpProvider_GroundwaterUsedAg = 149;
         //
        public const int mpNonpotableToTotalOutdoorUse = 165;
        public const int mpNonpotableToTotalDemandUse = 166;

        public const int mpResidentialIndoorWaterUse = 171;
        public const int mpResidentialOutdoorWaterUse = 172;
        public const int mpCommercialWaterUse = 173;
        public const int mpIndustrialWaterUse = 174;
        //
        public const int mpCAPaqueduct = 175;
        //
        //public const int mpAPIcleard = 147 - in User_Mods_3.cs;
        ///// <summary>
        /// Creates a Directory of directoryName if it does not exist.  Relative references is from the directory the program is executing from.
        /// This is primarily here as a demonstration of how the WaterSimDECDC_User_Mods file can be used to add user methods and parameters to\
        /// the WaterSimManager class.  The User can add their own methods and parameters using this file. 
        /// </summary>
        /// <param name="directoryName">Name of new or existing directory</param>
         //---------------------------------------------------------------------------
        //
        ModelParameterClass WAugParm;
        ModelParameterClass Scenario;
        //
        ProviderIntArray Out = new ProviderIntArray(0);
        internal int[] OneHundred = new int[ProviderClass.NumberOfProviders];
        internal int[] Ninty = new int[ProviderClass.NumberOfProviders];
        internal int[] Fifty = new int[ProviderClass.NumberOfProviders];
        internal int[] zero = new int[ProviderClass.NumberOfProviders];

       
        // This routine is called by initialize_ModelParameters
        // This is how User Defined Model Parameters are added to WaterSimManager Class Parameter Manager
        //   ExtendDoc.Add(new WaterSimDescripItem(eModelParam., "Description", "Short Unit", "Long Unit", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));

        partial void  initialize_WaterSim_6_ModelParameters()
        {
            ParameterManagerClass FPM = ParamManager;
            Extended_Parameter_Documentation ExtendDoc = FPM.Extended;
            WaterSimU WSU =  new WaterSimU();
           // WaterSimManager WSim = (this as WaterSimManager);
            //
            // Base Inputs
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_IwaniecScenario, "Iwaniec Scenario: ", "IWANIEC", rangeChecktype.rctCheckRange, 0, 8, geti_IwaniecScenario, seti_IwaniecScenario, RangeCheck.NoSpecialBase));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_IwaniecScenario, "Iwaniec et al. Scenarios", "Unitless", "Unitless", "", new string[] {"G", "AD", "AF", "AH", "HHH", "EC", "ZW", "BAU" }, new int[] { 0,1, 2, 3, 4, 5, 6, 7 }, new ModelParameterGroupClass[] { }));
            //
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epRainFallFactor, "Rainfall factor: ", "RAINFAC", rangeChecktype.rctCheckRange, 40, 150, geti_RainFallFactor, seti_RainFallFactor, RangeCheck.NoSpecialBase));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRainFallFactor, "Rainfall factor- decrease or increase rainfall", "Unitless", "Unitless", "", new string[] { "Low", "ModLow", "NoChange", "Moderate", "High" }, new int[] { 60, 80, 100, 120, 140 }, new ModelParameterGroupClass[] { }));
            //
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epGPCDefficiencyLCLU, "Efficiency of water use: ", "GPCDEFF", rangeChecktype.rctCheckRange, 50, 150, geti_GPCDEfficiency,seti_GPCDEfficiency, RangeCheck.NoSpecialBase));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epGPCDefficiencyLCLU, "LCLU GPCD Efficienc- increase", "Unitless", "Unitless", "", new string[] { "High", "Moderate", "Some", "NoChange" }, new int[] {70 ,80, 90, 100}, new ModelParameterGroupClass[] { }));
            //
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epRainHarvestCompliance, "Rainwater Harvest Adoption %: ", "RAINADOP", rangeChecktype.rctCheckRange, 0, 100, geti_RainFallHarvestAdopt, seti_RainFallHarvestAdopt, RangeCheck.NoSpecialBase));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRainHarvestCompliance, "Rainfall harvest compliance percent", "Percent", "%", "", new string[] { "Low", "ModLow", "Moderate", "High" }, new int[] { 20, 40, 60, 100 }, new ModelParameterGroupClass[] { }));
            //
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epRainHarvestInflection, "Rainwater Harvest years to Inflection: ", "RAININF", rangeChecktype.rctCheckRange, 10, 50, geti_RainHarvestInflection, seti_RainHarvestInflection, RangeCheck.NoSpecialBase));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRainHarvestInflection, "Rainfall harvest years to inflection point - year", "Year", "yr", "", new string[] { "Low", "ModLow", "Moderate", "High" }, new int[] { 10, 20, 30, 40 }, new ModelParameterGroupClass[] { }));



            // Provider Outputs
            // WaterSim 5 DOES NOT HAVE THIS CAPACITY
            // ---------------------------------------------------------------------------------------------------------------------------------------------------
            Effluent_Used_Ag = new providerArrayProperty(_pm, mpProvider_EffluentUsedAg, get_EffluentUsedAgriculture, null, eProviderAggregateMode.agSum);
             this.ParamManager.AddParameter(new ModelParameterClass(mpProvider_EffluentUsedAg, "Agriculture", "AGEFF", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 500000, null, get_EffluentUsedAgriculture, null, null, null, null, Effluent_Used_Ag));
             ExtendDoc.Add(new WaterSimDescripItem(mpProvider_EffluentUsedAg, "Effluent Used by Ag within a providers boundary", "AF a-1", "Acre-Feet per annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));

             Surface_Used_Ag = new providerArrayProperty(_pm, mpProvider_SurfaceUsedAg, get_SurfaceUsedAgriculture, null, eProviderAggregateMode.agSum);
             this.ParamManager.AddParameter(new ModelParameterClass(mpProvider_SurfaceUsedAg, "Surface water used by Agriculture", "AGSURFWAT", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 500000, null, get_SurfaceUsedAgriculture, null, null, null, null, Surface_Used_Ag));
             ExtendDoc.Add(new WaterSimDescripItem(mpProvider_SurfaceUsedAg, "Surface water Used by Ag within a providers boundary", "AF a-1", "Acre-Feet per annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
             //
             Groundwater_Used_Ag = new providerArrayProperty(_pm, mpProvider_GroundwaterUsedAg, get_GroundwaterUsedAgriculture, null, eProviderAggregateMode.agSum);
             this.ParamManager.AddParameter(new ModelParameterClass(mpProvider_GroundwaterUsedAg, "Groundwater used by Agriculture", "AGGRNDWAT", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 500000, null, get_GroundwaterUsedAgriculture, null, null, null, null, Groundwater_Used_Ag));
             ExtendDoc.Add(new WaterSimDescripItem(mpProvider_GroundwaterUsedAg, "Groundwater Used by Ag within a providers boundary", "AF a-1", "Acre-Feet per annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
             // ========================================================================================================================================================
            //

             RainWaterHarvested = new providerArrayProperty(_pm,eModelParam.epProvider_RainWaterHarvested, get_RainWaterharvested, null, eProviderAggregateMode.agSum);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_RainWaterHarvested, "Rain Water Harvested", "RAINHARV", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 50000, null, get_RainWaterharvested, null, null, null, null, RainWaterHarvested));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_RainWaterHarvested, "Rain Water Harvested- depends on switches to which source it represents", "AF a-1", "Acre-Feet per annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
            //
              StormWaterHarvested = new providerArrayProperty(_pm, eModelParam.epProvider_StormWaterHarvested, get_StormWaterharvested, null, eProviderAggregateMode.agSum);
              this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_StormWaterHarvested, "Storm Water Harvested", "STORMHARV", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 100000, null, get_StormWaterharvested, null, null, null, null, StormWaterHarvested));
              ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_RainWaterHarvested, "Storm Water Harvested- ", "AF a-1", "Acre-Feet per annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
             //
              StormWaterStructural_Capacity = new providerArrayProperty(_pm, mpProvider_StormWaterCapacity, get_StormWaterStructure_Capacity, set_StormWaterStructure_Capacity, eProviderAggregateMode.agSum);
              this.ParamManager.AddParameter(new ModelParameterClass(mpProvider_StormWaterCapacity, "Storm Water Structural Capacity m3", "STWCAP", modelParamtype.mptInputProvider, rangeChecktype.rctCheckRange, 0, 24500, null, get_StormWaterStructure_Capacity, null, set_StormWaterStructure_Capacity, null, null, StormWaterStructural_Capacity));
              ExtendDoc.Add(new WaterSimDescripItem(mpProvider_StormWaterCapacity, "Maximum Capacity of a Commercial Storm Water Retention Unit", "m3", "cubic meters", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
            //
              NonpotableWaterUsed = new providerArrayProperty(_pm, eModelParam.epProvider_Nonpotable, geti_nonpotable_Total, null, eProviderAggregateMode.agWeighted);
              this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_Nonpotable, "Nonpotable", "NONPOTAB", NonpotableWaterUsed, 0, 100000));
              ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_Nonpotable, "Total Non-potable Water Used ", "AF a-1", "Acre-Feet Annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));

            // Land Cover Land Use estimates of Water Demand
            // ------------------------------------------------
            AgriculturalDemand = new providerArrayProperty(_pm, eModelParam.epDemand_Agriculture, geti_demand_Agriculture, null, eProviderAggregateMode.agSum);
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epDemand_Agriculture, "Agriculture", "AGDEM", AgriculturalDemand, 0, 100000));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epDemand_Agriculture, "Agriculture Demand", "AF a-1", "Acre-Feet Annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));

            LowDensityDemand = new providerArrayProperty(_pm, eModelParam.epDemand_LowDensity, geti_demand_LowDensity, null, eProviderAggregateMode.agSum);
              this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epDemand_LowDensity, "Low Density", "LDENDEM", LowDensityDemand, 0, 100000));
              ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epDemand_LowDensity, "Res, Com, and Ind demand from low density (peri-urban) cells", "AF a-1", "Acre-Feet Annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));

             MediumDensityDemand = new providerArrayProperty(_pm,eModelParam.epDemand_MediumDensity, geti_demand_MediumDensity, null, eProviderAggregateMode.agSum);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epDemand_MediumDensity, "Medium Density", "MDENDEM", MediumDensityDemand, 0, 100000));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epDemand_MediumDensity, "Res, Com, and Ind demand from medium density cells", "AF a-1", "Acre-Feet Annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));

             HighDensityDemand = new providerArrayProperty(_pm, eModelParam.epDemand_HighDensity, geti_demand_HighDensity, null, eProviderAggregateMode.agSum);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epDemand_HighDensity, "High Density", "HDENDEM", HighDensityDemand, 0, 100000));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epDemand_HighDensity, "Res, Com, and Ind demand from high density cells", "AF a-1", "Acre-Feet Annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));

             TurfWaterDemand = new providerArrayProperty(_pm, eModelParam.epDemand_Turf, geti_demand_Turf, null, eProviderAggregateMode.agSum);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epDemand_Turf, "Turf", "TURFDEM", TurfWaterDemand, 0, 100000));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epDemand_Turf, "Turf Water Demand ", "AF a-1", "Acre-Feet Annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));

             GreenwayWaterDemand = new providerArrayProperty(_pm, eModelParam.epDemand_Greenway, geti_demand_Greenway, null, eProviderAggregateMode.agSum);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epDemand_Greenway, "Greenway", "GREENDEM", GreenwayWaterDemand, 0, 100000));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epDemand_Greenway, "Greenway Water Demand ", "AF a-1", "Acre-Feet Annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));

             TreeWaterDemand = new providerArrayProperty(_pm, eModelParam.epDemand_Tree, geti_demand_Tree, null, eProviderAggregateMode.agSum);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epDemand_Tree, "Tree", "TREEDEM", TreeWaterDemand, 0, 100000));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epDemand_Tree, "Tree Water Demand ", "AF a-1", "Acre-Feet Annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
             // ======================================================================================================================================================
             //
             // GPCD
             //
             ResidentialLowDensityIndoorGPCD = new providerArrayProperty(_pm, eModelParam.epProvider_ResIndoorGPCD_LD, geti_resIndoorGPCD_LD, null, eProviderAggregateMode.agWeighted);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_ResIndoorGPCD_LD, "LD Indoor", "LRINGPCD", ResidentialLowDensityIndoorGPCD, 0, 100));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_ResIndoorGPCD_LD, "Residential Indoor GPCD for Low Density Housing", "GPCD", "Gallons per Capita per Day", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
                //
             ResidentialLowDensityOutdoorGPCD = new providerArrayProperty(_pm, eModelParam.epProvider_ResOutdoorGPCD_LD, geti_resOutdoorGPCD_LD, null, eProviderAggregateMode.agWeighted);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_ResOutdoorGPCD_LD, "LD Outdoor", "LROUTGPCD", ResidentialLowDensityOutdoorGPCD, 0, 200));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_ResOutdoorGPCD_LD, "Residential Outdoor GPCD for Low Density Housing", "GPCD", "Gallons per Capita per Day", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
             //
             ResidentialMediumDensityIndoorGPCD = new providerArrayProperty(_pm, eModelParam.epProvider_ResIndoorGPCD_MD, geti_resIndoorGPCD_MD, null, eProviderAggregateMode.agWeighted);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_ResIndoorGPCD_MD, "MD Indoor", "MRINGPCD", ResidentialMediumDensityIndoorGPCD, 0, 100));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_ResIndoorGPCD_MD, "Residential Indoor GPCD for Medium Density Housing", "GPCD", "Gallons per Capita per Day", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
             //
             ResidentialMediumDensityOutdoorGPCD = new providerArrayProperty(_pm, eModelParam.epProvider_ResOutdoorGPCD_MD, geti_resOutdoorGPCD_MD, null, eProviderAggregateMode.agWeighted);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_ResOutdoorGPCD_MD, "MD Outdoor", "MROUTGPCD", ResidentialMediumDensityOutdoorGPCD, 0, 200));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_ResOutdoorGPCD_MD, "Residential Outdoor GPCD for Medium Density Housing", "GPCD", "Gallons per Capita per Day", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
            //
             ResidentialHighDensityIndoorGPCD = new providerArrayProperty(_pm, eModelParam.epProvider_ResIndoorGPCD_HD, geti_resIndoorGPCD_HD, null, eProviderAggregateMode.agWeighted);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_ResIndoorGPCD_HD, "HD Indoor", "HRINGPCD", ResidentialHighDensityIndoorGPCD, 0, 100));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_ResIndoorGPCD_HD, "Residential Indoor GPCD for High Density Housing", "GPCD", "Gallons per Capita per Day", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
             //
             ResidentialHighDensityOutdoorGPCD = new providerArrayProperty(_pm, eModelParam.epProvider_ResOutdoorGPCD_HD, geti_resOutdoorGPCD_HD, null, eProviderAggregateMode.agWeighted);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_ResOutdoorGPCD_HD, "HD Outdoor", "HROUTGPCD", ResidentialHighDensityOutdoorGPCD, 0, 200));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_ResOutdoorGPCD_HD, "Residential Outdoor GPCD for High Density Housing", "GPCD", "Gallons per Capita per Day", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));




            //
             RainWaterHarvested_SF = new providerArrayProperty(_pm, eModelParam.epProvider_RainWaterHarvestedSF, get_RainWaterharvestedSF, null, eProviderAggregateMode.agSum);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_RainWaterHarvestedSF, "Single Family", "RAINSF", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 25000, null, get_RainWaterharvestedSF, null, null, null, null, RainWaterHarvested_SF));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_RainWaterHarvestedSF, "Rain Water Harvested- depends on switches to which source it represents", "AF a-1", "Acre-Feet Annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
            //
             RainWaterHarvested_MF = new providerArrayProperty(_pm, eModelParam.epProvider_RainWaterHarvestedMF, get_RainWaterharvestedMF, null, eProviderAggregateMode.agSum);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_RainWaterHarvestedMF, "Multi-Family", "RAINMF", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 25000, null, get_RainWaterharvestedMF, null, null, null, null, RainWaterHarvested_MF));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_RainWaterHarvestedMF, "Rain Water Harvested- depends on switches to which source it represents", "AF a-1", "Acre-Feet Annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
            //
             RainWaterHarvested_PU = new providerArrayProperty(_pm, eModelParam.epProvider_RainWaterHarvestedPU, get_RainWaterharvestedPU, null, eProviderAggregateMode.agSum);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_RainWaterHarvestedPU, "Peri-Urban", "RAINPU", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 25000, null, get_RainWaterharvestedPU, null, null, null, null, RainWaterHarvested_PU));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_RainWaterHarvestedPU, "Rain Water Harvested- depends on switches to which source it represents", "AF a-1", "Acre-Feet Annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
            //
             ResGrayWater = new providerArrayProperty(_pm, eModelParam.epProvider_ResGrayWaterUsed, get_resGrayWater, null, eProviderAggregateMode.agSum);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_ResGrayWaterUsed, "Residential", "RESGRAY", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 50000, null, get_resGrayWater, null, null, null, null, ResGrayWater));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_ResGrayWaterUsed, "Residential Gray Water Used", "AF a-1", "Acre-feet Annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));

             ComGrayWater = new providerArrayProperty(_pm, eModelParam.epProvider_ComGrayWaterUsed, get_comGrayWater, null, eProviderAggregateMode.agSum);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_ComGrayWaterUsed, "Commercial", "COMGRAY", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 50000, null, get_comGrayWater, null, null, null, null, ComGrayWater));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_ComGrayWaterUsed, "Commercial", "AF a-1", "Acre-feet per annum", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));

             IndGrayWater = new providerArrayProperty(_pm, eModelParam.epProvider_IndGrayWaterUsed, get_indGrayWater, null, eProviderAggregateMode.agSum);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_IndGrayWaterUsed, "Industrial", "INDGRAY", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 50000, null, get_indGrayWater, null, null, null, null, IndGrayWater));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_IndGrayWaterUsed, "Industrial Gray Water Used", "AF a-1", "Acre-feet per annum", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
           
            //
             RainWaterHarvtoTotalOutdoorUse = new providerArrayProperty(_pm, eModelParam.epRainHarvestedToTotalOutdoor, geti_RainHarvestedToTotalOutdoor, null, eProviderAggregateMode.agWeighted);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epRainHarvestedToTotalOutdoor, "Rainwater", "RAINOUT", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 100, null, geti_RainHarvestedToTotalOutdoor, null, null, null, null, RainWaterHarvtoTotalOutdoorUse));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRainHarvestedToTotalOutdoor, "Rain Water to Total Outdoor Use Percent", "PCT", "Percent", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
            //
             GrayWaterToTotalOutdoorUse = new providerArrayProperty(_pm, eModelParam.epGrayWaterToTotalOutdoor, geti_GrayWaterToTotalOutdoor, null, eProviderAggregateMode.agWeighted);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epGrayWaterToTotalOutdoor, "Graywater", "GRAYOUT", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 100, null, geti_GrayWaterToTotalOutdoor, null, null, null, null, GrayWaterToTotalOutdoorUse));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epGrayWaterToTotalOutdoor, "Gray Water Captured to Total Outdoor Demand", "percent", "Percent", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
            //
             ReclaimedToTotalOutdoorUse = new providerArrayProperty(_pm, eModelParam.epReclaimedToTotalOutdoor, geti_ReclaimedToTotalOutdoor, null, eProviderAggregateMode.agWeighted);
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epReclaimedToTotalOutdoor, "Reclaimed", "RECOUT", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 100, null, geti_ReclaimedToTotalOutdoor, null, null, null, null, ReclaimedToTotalOutdoorUse));
             ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epReclaimedToTotalOutdoor, "Reclaimed Water Collected to Total Outdoor Demand", "percent", "Percent", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
            // 12.10.18
            GrossStormwater = new providerArrayProperty(_pm, eModelParam.epStormwater, geti_GrossStormwaterGenerated, null, eProviderAggregateMode.agWeighted);
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epStormwater, "Stormwater", "STORM", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 100, null, geti_GrossStormwaterGenerated, null, null, null, null, GrossStormwater));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epStormwater, "The total gross amount of stormwater created", "AF a-1", "acre-feet annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));


            // 08.25.18 
            //_pm.AddParameter(new ModelParameterClass(eModelParam.epRainWaterAdoption, "Rain Water Harvesting Adoption", "RWHADOPT", rangeChecktype.rctCheckRangeSpecial, 0, 100, geti_RainFallHarvestAdopt, seti_RainFallHarvestAdopt, null));
            //ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRainWaterAdoption, "", "Percent", "Rain Harvest Adopt", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));


            //
            // Base Outputs
            // 05.16.17 commented out

            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epResidentialIndoorWaterUse, "Residential Indoor Water Use", "RESINDR", get_ResIndoorWater, 0, 300000));
             ExtendDoc.Add(new WaterSimDescripItem(mpResidentialIndoorWaterUse, "Potable and non-potable residential water used Indoors", "AF a-1", "Acre-feet per annum", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
             // 
             this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epResidentialOutdoorWaterUse, "Residential Outdoor Water Use", "RESOUTDR", get_ResOutdoorWater, 0, 300000));
             ExtendDoc.Add(new WaterSimDescripItem(mpResidentialOutdoorWaterUse, "Potable and non-potable residential water used Outdoors", "AF a-1", "Acre-feet per annum", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
            //// 
            //this.ParamManager.AddParameter(new ModelParameterClass(mpCommercialWaterUse, "Commercial Water Use", "COMTOTUSE", get_ComTotalWaterUse, 0, 300000));
            //ExtendDoc.Add(new WaterSimDescripItem(mpCommercialWaterUse, "Potable and non-potable commercial water used", "AF a-1", "Acre-feet per annum", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
            //// 
            //this.ParamManager.AddParameter(new ModelParameterClass(mpIndustrialWaterUse, "Industrial Water Use", "INDTOTUSE", get_IndTotalWaterUse, 0, 300000));
            //ExtendDoc.Add(new WaterSimDescripItem(mpIndustrialWaterUse, "Potable and non-potable industrial water used", "AF a-1", "Acre-feet per annum", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
            //
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epCAPaqueduct, "Cap aqueduct", "CAPADUCT", get_CAPaqueduct, 0, 1700000));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epCAPaqueduct, "Water volume delivered to Granite Reef", "AF a-1", "Million Acre-feet per annum", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));

            //----------------------------------------------------------------------------------------------------
            //
            int scen = IwaniecScenarios = 7;
             IntitializeModel(scen,WSU);
            //
        }
        // WaterSim_6
        //
        private int iwaniecScenario = 1;
        private void seti_IwaniecScenario(int value) { IwaniecScenarios = value;
        }
        internal int geti_IwaniecScenario() { return IwaniecScenarios; }      
        public int IwaniecScenarios
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epProvider_IwaniecScenario, value);
                    _ws.set_scenario = value;
                    iwaniecScenario = value;
                }
            }
            get { return iwaniecScenario; }
        }
        //
        // ================================================================
        // 06.03.2018
        
        private int _GPCDEfficiency = 100;
        internal int gpcdEfficiency
        {
            get { return _GPCDEfficiency; }
            set { _GPCDEfficiency = value; }
        }
        private void seti_GPCDEfficiency(int value)
        {
            GPCDEfficiencylclu = value;
        }
        internal int geti_GPCDEfficiency() { return gpcdEfficiency; }
        //  internal int geti_IwaniecScenario() { return IwaniecScenarios; }
        public int GPCDEfficiencylclu
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epGPCDefficiencyLCLU, value);
                    _ws.set_GPCDEfficiency = value;
                    gpcdEfficiency = value;
                }
            }
            get { return gpcdEfficiency; }
        }

        //
        private int _rainFallFactor = 100;
        internal int rainFallFactor
        {
            get { return _rainFallFactor; }
            set { _rainFallFactor = value; }
        }
        private void seti_RainFallFactor(int value)
        {
            RainFallFactor = value;
        }
        internal int geti_RainFallFactor() { return rainFallFactor; }     
      
       // internal int geti_IwaniecScenario() { return IwaniecScenarios; }
        public int RainFallFactor
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    //_pm.CheckBaseValueRange(eModelParam.epRainFallFactor, value);
                    _ws.set_RainFallFactor = value;
                    rainFallFactor = value;
                }
            }
            get { return rainFallFactor; }
        }
        //
        private int _rainHarvestAdoption = 0;
        internal int rainHarvestAdoption
        {
            get { return _rainHarvestAdoption; }
            set { _rainHarvestAdoption = value; }
        }
        private void seti_RainFallHarvestAdopt(int value)
        {

            RainHarvestAdoption = value;

        }
        internal int geti_RainFallHarvestAdopt() { return rainHarvestAdoption; }
 
        // internal int geti_IwaniecScenario() { return IwaniecScenarios; }
        public int RainHarvestAdoption
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    //_pm.CheckBaseValueRange(eModelParam.epRainFallFactor, value);
                    _ws.set_parmRainwaterHarvestCompliance = value;
                    rainHarvestAdoption = value;
                }
            }
            get { return rainFallFactor; }
        }
        //

        private int _rainHarvestInflection = 0;
        internal int rainHarvestInflection
        {
            get { return _rainHarvestInflection; }
            set { _rainHarvestInflection = value; }
        }
        internal void seti_RainHarvestInflection(int value)
        {

            RainHarvestInflection = value;

        }
        internal int geti_RainHarvestInflection() { return rainHarvestInflection; }

        // internal int geti_IwaniecScenario() { return IwaniecScenarios; }
        public int RainHarvestInflection
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    //_pm.CheckBaseValueRange(eModelParam.epRainFallFactor, value);
                    _ws.set_parmRainwaterInflection = value;
                    rainHarvestInflection = value;
                }
            }
            get { return rainHarvestInflection; }
        }



        //==================================
        // Storm water potential storage
        /// <summary>
        /// 03.30.2016
        /// </summary>
        /// <returns></returns>
        internal int[] get_StormWaterStructure_Capacity() { return _ws.set_StormWaterCapacity; }
         internal void set_StormWaterStructure_Capacity(int[] value)
         {
             if (!FModelLocked) _ws.set_StormWaterCapacity = value;
         }
         /// <summary>Capacity of an individual Storm Water capture facility  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty StormWaterStructural_Capacity;
         //
         internal int[] get_EffluentUsedAgriculture() { return _ws.get_EffluentUsed_Agriculture; }
        
         /// <summary>Capacity of an individual Storm Water capture facility  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty Effluent_Used_Ag;
        //
         internal int[] get_SurfaceUsedAgriculture() { return _ws.get_SurfaceUsed_Agriculture; }
         /// <summary>Capacity of an individual Storm Water capture facility  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty Surface_Used_Ag;
         //
         internal int[] get_GroundwaterUsedAgriculture() { return _ws.get_GroundwaterUsed_Agriculture; }
         /// <summary>Capacity of an individual Storm Water capture facility  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty Groundwater_Used_Ag;
        //
         internal int[] get_RainWaterharvested() { return _ws.get_RainWaterHarvested; }
         /// <summary>Capacity of an individual Storm Water capture facility  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty RainWaterHarvested;
         //
         internal int[] get_StormWaterharvested() { return _ws.get_StormWaterHarvested; }
         /// <summary>Capacity of an individual Storm Water capture facility  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty StormWaterHarvested;
         //

         internal int[] get_RainWaterharvestedSF() { return _ws.get_RainWaterHarvested_SF; }
         /// <summary>Capacity of an individual Storm Water capture facility  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty RainWaterHarvested_SF;
         //
         internal int[] get_RainWaterharvestedMF() { return _ws.get_RainWaterHarvested_MF; }
         /// <summary>Capacity of an individual Storm Water capture facility  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty RainWaterHarvested_MF;
        //
         internal int[] get_RainWaterharvestedPU() { return _ws.get_RainWaterHarvested_PU; }
         /// <summary>Capacity of an individual Storm Water capture facility  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty RainWaterHarvested_PU;
        //
         internal int[] get_GPCD_resGrayWaterU() { return _ws.get_GPCD_resGrayWater; }
         /// <summary>Capacity of an individual Storm Water capture facility  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty GPCDresGrayWater;
        //
         internal int[] get_GPCD_comGrayWaterU() { return _ws.get_GPCD_comGrayWater; }
         /// <summary>Capacity of an individual Storm Water capture facility  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty GPCDcomGrayWater;
        //
         internal int[] get_resGrayWater() { return _ws.get_GrayWaterResidential; }
         /// <summary>  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty ResGrayWater;
        //
         internal int[] get_comGrayWater() { return _ws.get_GrayWaterCommercial; }
         /// <summary>  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty ComGrayWater;
         //
         internal int[] get_indGrayWater() { return _ws.get_GrayWaterIndustrial; }
         /// <summary>  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty IndGrayWater;
        //
        /// <summary>
        ///  LCLU
        /// </summary>
         /// AgriculturalDemand
        /// 
        /// 
        /// 
        internal int[] geti_demand_Agriculture() { return _ws.get_WaterDemand_Agriculture; }
         /// <summary>  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
        public providerArrayProperty AgriculturalDemand;

        /// <returns></returns>
         internal int[] geti_demand_MediumDensity() { return _ws.get_WaterDemand_MediumDensity; }
         /// <summary>  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty MediumDensityDemand;
         //
         internal int[] geti_demand_HighDensity() { return _ws.get_WaterDemand_HighDensity; }
         /// <summary>  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty HighDensityDemand;
        //
         internal int[] geti_demand_LowDensity() { return _ws.get_WaterDemand_LowDensity; }
         /// <summary>  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty LowDensityDemand;
         //
         internal int[] geti_demand_Turf() { return _ws.get_WaterDemand_Turf; }
         /// <summary>  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty TurfWaterDemand;
         //
         internal int[] geti_demand_Greenway() { return _ws.get_WaterDemand_Greenway; }
         /// <summary>  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty GreenwayWaterDemand;
         //
         internal int[] geti_demand_Tree() { return _ws.get_WaterDemand_Tree; }
         /// <summary>  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty TreeWaterDemand;
         //
         internal int[] geti_nonpotable_Total() { return _ws.get_NonpotableWaterTotal; }
         /// <summary>  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty NonpotableWaterUsed;
        // ---------------------------------------------------------------------------------------------
        // GPCD for each density class for Residential Water Users
        // 01.14.2017
        //
        /// <summary>
        /// GPCD for residential water users Indoors
        /// </summary>
        /// <returns></returns>
         internal int[] geti_resIndoorGPCD_LD() { return _ws.get_ResidentialLowDenIndoorGPCD; }
         /// <summary>  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty ResidentialLowDensityIndoorGPCD;
         //
         internal int[] geti_resOutdoorGPCD_LD() { return _ws.get_ResidentialLowDenOutdoorGPCD; }
         /// <summary>  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty ResidentialLowDensityOutdoorGPCD;

         internal int[] geti_resIndoorGPCD_MD() { return _ws.get_ResidentialMediumDenIndoorGPCD; }
         /// <summary>  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty ResidentialMediumDensityIndoorGPCD;
         //
         //
         internal int[] geti_resOutdoorGPCD_MD() { return _ws.get_ResidentialMediumDenOutdoorGPCD; }
         /// <summary>  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty ResidentialMediumDensityOutdoorGPCD;
        //
        // High Density GPCD
        //
         internal int[] geti_resIndoorGPCD_HD() { return _ws.get_ResidentialHighDenIndoorGPCD; }
         /// <summary>  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty ResidentialHighDensityIndoorGPCD;
         //
         internal int[] geti_resOutdoorGPCD_HD() { return _ws.get_ResidentialHighDenOutdoorGPCD; }
         /// <summary>  </summary>
         /// <remarks>m3  </remarks>  
         /// <exception cref="WaterSim_Exception"></exception>
         ///
         public providerArrayProperty ResidentialHighDensityOutdoorGPCD;
        //
        //
         internal int[] geti_RainHarvestedToTotalOutdoor() { return _ws.get_RainHarvestedToTotalOutdoor; }
         public providerArrayProperty RainWaterHarvtoTotalOutdoorUse;
        //
         internal int[] geti_GrayWaterToTotalOutdoor() { return _ws.get_GrayWaterToTotalOutdoor; }
         public providerArrayProperty GrayWaterToTotalOutdoorUse;
        //
         internal int[] geti_ReclaimedToTotalOutdoor() { return _ws.get_ReclaimedToTotalOutdoor; }
         public providerArrayProperty ReclaimedToTotalOutdoorUse;
         //

         private int get_ResIndoorWater() { return ResIndoorWaterUse; }
         public int ResIndoorWaterUse
         { get { return _ws.get_ResIndoorWaterUse; } }
         //
         private int get_ResOutdoorWater() { return ResOutdoorWaterUse; }
         public int ResOutdoorWaterUse
         { get { return _ws.get_ResOutdoorWaterUse; } }
         //
         private int get_CAPaqueduct() { return CAP; }
         public int CAP
         { get { return _ws.get_CAP_BO; } }

        //private int get_IndTotalWaterUse() { return IndWaterUse; }
        //public int IndWaterUse
        //{ get { return _ws.get_IndWaterUsed; } }
        ////
        // 12.10.18 das
        internal int[] geti_GrossStormwaterGenerated() { return _ws.get_GrossStormwaterCreated; }
        public providerArrayProperty GrossStormwater;

        //
        // =================================================================================
        public void IntitializeModel(int scenario, WaterSimU WSU)
         {
            
             Initial(scenario,WSU);
             copyScenarioFile(scenario, DataDirectory);
           // this.Sim_StartYear = 2017;
         }
         public void SetModelScenarios(int scen,WaterSimU WSU)
         {
             Init(scen, WSU);
             copyScenarioFile(scen, DataDirectory);
         }
        // ================================================================================

        // Copy Files
         public void copyScenarioFile(int scenario, string DataDir)
         {
             SimpleFileCopy myFile = new SimpleFileCopy();
             // string[] sScenario = new string[FScenario] { "AD", "AF", "AH", "HHH", "EC", "ZW", "BAU" };

             string path = @"BAU\";
             switch (scenario)
             {
                 case 1:
                     path = @"AD\";
                     break;
                 case 2:
                     path = @"AF\";
                     break;
                 case 3:
                     path = @"AH\";
                     break;
                 case 4:
                     path = @"HHH\";
                     break;
                 case 5:
                     path = @"EC\";
                     break;
                 case 6:
                     path = @"ZW\";
                     break;
                 case 7:
                     path = @"BAU\";
                     break;
                case 8:
                    path = @"BAU\";

                    break;
                case 9:
                    path = @"ZW\";

                    break;
                 default:
                     path = @"BAU\";

                     break;
             }

             myFile.CopyMain(path,DataDir);

         }
        //
         void Initial(int Scenario, WaterSimU WSU)
         {
              this.Colorado_Climate_Adjustment_Percent = 100;
              this.SaltVerde_Climate_Adjustment_Percent = 100;
              this.RainFallFactor = 100;

             switch (Scenario)
             {  
                 case 7:
                    WSU.set_parmIwaniecNoLeaks = false;
                    WSU.set_parmIwaniecPPtoAgEffluent = false;

                      break;
                  default:
                    // path = @"BAU\";

                     WSU.set_parmIncludeMeteorology = true;
                     WSU.set_parmCalculateRainWaterHarvesting = false;
                     WSU.set_parmCalculateStormWaterHarvesting = false;

                     break;
             }
             //     
             // ===================================================================================

         }
         void Init(int Scenario, WaterSimU WSU)
         {
             //
             WSU.set_parmIncludeMeteorology = true;
             WSU.parmAPIcleared = true;
             // ================================================================================
             WAugParm = this.ParamManager.Model_Parameter(eModelParam.epWebAugmentation_PCT);
             // ========================================
             // RCP8.5 emission scenario
             //CNRM.a2 GCM with a2 emission
             // 60 for the Colorado River
             // 55 (estimate) for the Salt-Verde-Tonto Rivers
             //this.Colorado_Climate_Adjustment_Percent = 100;
             //this.SaltVerde_Climate_Adjustment_Percent = 100;
             this.Simulation_End_Year = 2060;
             //            
             // 1=AD; 2 = AF; 3=AH; 4=HHH; 5=EC; 6=ZW; 7=BAU; 8=testing; 9=default
             // int scenario = 7;
             // int scenario = Scenario;
             //WSU.set_scenario = Scenario;
             //
             string path = @"BAU\";
             switch (Scenario)
             {
                 case 0:

                     break;
                 case 1:
                     path = @"AD\";

                     break;
                 case 2:
                     path = @"AF\";
                    break;
                 case 3:
                     path = @"AH\";

                     // Adaptive Heat
                     break;
                 case 4:
                     path = @"HHH\";

                     // Transformative Healthy Harvest Hubs
                     break;
                 case 5:
                     path = @"EC\";

                     // Transformative Emerald City Necklace
                    break;
                 case 6:
                     path = @"ZW\";

                     // Transformative Zero Waste
                     break;
                 case 7:
                     path = @"BAU\";
                    break;
                 case 8:
                    path = @"BAU\";

                    break;

                    // Testing
                 case 9:
                    path = @"ZW\";

                    break;
                 default:
                     path = @"BAU\";

                   break;
             }
             //
             this.IwaniecScenarios = Scenario;
            //
             // Copy the appropriate LCLU classification file into the Data Directory prior to the simulation
             // 07.05.16
             // SimpleFileCopy myFile = new SimpleFileCopy();
             //myFile.CopyMain(path);
             //

             // ===================================================================================

         }

         public void ResetScenarioData(WaterSimU WSU)
         {

             Scenario = this.ParamManager.Model_Parameter(eModelParam.epProvider_IwaniecScenario); 
               int scenario = IwaniecScenarios;
                 SetModelScenarios(scenario,WSU);     
         }
         public class SimpleFileCopy
         {
             public void CopyMain(string scenario,string Data)
             {
                 //string fileName = "LULC_Scenario.txt";
                 string fileName = "LCLU_Scenario.txt";
                //string data =  Data
                //                 string sourcePath = @"C:\WaterSim\CSharp\API_10_0\API_Projects\API_WaterSim_6\ModelTesting\bin\Debug\App_Data\WaterSim_6_0\App_Data\Data\NewLCLU\" + scenario;
                //                string targetPath = @"C:\WaterSim\CSharp\API_10_0\API_Projects\API_WaterSim_6\ModelTesting\bin\Debug\App_Data\WaterSim_6_0\App_Data\Data";
                string stripslash =  scenario.TrimEnd('\\');
                string sourcePath = Data + @"\App_Data\Data\NewLCLU\" + stripslash;
                 string targetPath = Data + @"\App_Data\Data";

                 // Use Path class to manipulate file and directory paths.
                 string sourceFile = System.IO.Path.Combine(sourcePath, fileName);
                 string destFile = System.IO.Path.Combine(targetPath, fileName);

                 // To copy a folder's contents to a new location:
                 // Create a new target folder, if necessary.
                 if (!System.IO.Directory.Exists(targetPath))
                 {
                     System.IO.Directory.CreateDirectory(targetPath);
                 }


                 // To copy a file to another location and 
                 // overwrite the destination file if it already exists.
                 if (System.IO.File.Exists(sourceFile))
                 {
                     System.IO.File.Copy(sourceFile, destFile, true);
                    // Console.WriteLine("Successful Copy.");
                 }
                 else
                 {
                    // Console.WriteLine("Source file does not exist!");
                   //  System.Threading.Thread.Sleep(700);
                 }
               
                // System.Threading.Thread.Sleep(1000);
               
             }
         }
        // stop
        // ---------------------------------------------------------------------------------------------------------
        //
    }

   
}
