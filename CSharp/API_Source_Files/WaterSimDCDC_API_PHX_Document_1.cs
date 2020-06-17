using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using WaterSimDCDC.Documentation;          

namespace WaterSimDCDC
{
    public partial class WaterSimManager
    {
        
         public ModelParameterGroupClass WaterSupplyGroup;

        /// <summary>  List of Supply Related ModelParameters <remarks> Used for Topic sorting</remarks> </summary>
        internal int[] WaterSupplyGroupList = new[] {
                eModelParam.epPCT_Reclaimed_to_Water_Supply,
                eModelParam.epPCT_SurfaceWater_to_WaterBank,
                eModelParam.epTotalReclaimedUsed,
                eModelParam.epTotalReclaimedCreated_AF,
                eModelParam.epTotalSupplyUsed,
                eModelParam.epUse_SurfaceWater_to_WaterBank,
                eModelParam.epWaterAugmentation,
                eModelParam.epWaterAugmentationUsed,
        };


        public ModelParameterGroupClass MetricWaterGroup;

        /// <summary>
        ///  Surface Water Group
        /// </summary>
        public ModelParameterGroupClass SurfaceWaterGroup;
    
        /// <summary>
        ///  Salt Verde SUrface Water Group 
        /// </summary>
        public ModelParameterGroupClass SRVSurfaceWaterGroup;

        // list of Altverde surface water parameters
        internal int[] SRVSurfaceWaterGroupList = new[] {
                eModelParam.epSaltVerde_Annual_Deliveries_SRP,
                eModelParam.epSaltVerde_Class_BC_Designations
        };

        public ModelParameterGroupClass GroundWaterGroup;

        internal int[] GroundWaterGroupList = new[] {
            eModelParam.epGroundwater_Pumped_Municipal,
            eModelParam.epGroundwater_Balance,
            eModelParam.epGroundwater_Bank_Used,
            eModelParam.epGroundwater_Bank_Balance,
            eModelParam.epWaterBank_Source_Option,
            eModelParam.epPCT_SurfaceWater_to_WaterBank,
            eModelParam.epUse_SurfaceWater_to_WaterBank,
            eModelParam.epRegionalAgOtherPumping,
            eModelParam.epRegionalGWBalance,
            eModelParam.epProvider_WaterFromAgPumping,
            eModelParam.epProvider_WaterFromAgPumpingMax,
            eModelParam.epPCT_GWAvailable,
            eModelParam.epYrsGWZero,
            eModelParam.epYearGWGoesZero,
        };

        // Colorado Storage Group
        public ModelParameterGroupClass Colorado_StorageGroup;

        internal int[] Colorado_StorageGroupGroupList = new[] {
          eModelParam.epPowell_Storage,
          eModelParam.epMead_Storage 
        };

        // Salt Verde Storage Group
        public ModelParameterGroupClass SRP_StorageGroup;

        internal int[] SRP_StorageGroupGroupList = new[] {
            eModelParam.epSaltOther_Storage,
            eModelParam.epRoosevelt_Storage,
            eModelParam.epVerde_Storage
        };

        public ModelParameterGroupClass Reservoir_Storage;

        // Salt Verde River flows
        public ModelParameterGroupClass SRP_River_Flow;

        internal int[] SRP_River_FlowGroupList = new[] {
            eModelParam.epSaltTonto_AnnualFlow,
            eModelParam.epVerde_AnnualFlow
        };

        public ModelParameterGroupClass Colorado_River_FlowGroup;
        // Demand

        // Provider

        // Base

        // Sustainable

       // Model COntrol
        public ModelParameterGroupClass Model_ControlGroup;

        // Population

        public ModelParameterGroupClass PopulationGroup;

        // Demand

        public ModelParameterGroupClass DemandGroup;
        // Climate

        public ModelParameterGroupClass ClimateGroup;

        // System 

        public ModelParameterGroupClass UrbanSystemGroup;

        // Inidcator

        public ModelParameterGroupClass IndicatorGroup;

        // feedback
        // 

        public ModelParameterGroupClass FeedbackGroup;

        // Policy
        // 
        public ModelParameterGroupClass PolicyGroup;

        public ModelParameterGroupClass Policy_Demand_Group;

        public ModelParameterGroupClass Policy_UrbanSystem_Group;

        protected override void initialize_ExtendedDocumentation()
        {
            ParameterManagerClass FPM = ParamManager;

            Extended_Parameter_Documentation ExtendDoc = FPM.Extended;


            // groundwater group
            GroundWaterGroup = new ModelParameterGroupClass("Groundwater Supply Group", GroundWaterGroupList);
            FPM.GroupManager.Add(GroundWaterGroup);
            // Salt Verde Surface water group
            SRVSurfaceWaterGroup = new ModelParameterGroupClass("SRV Surface Water", SRVSurfaceWaterGroupList);
            FPM.GroupManager.Add(SRVSurfaceWaterGroup);

            // Metrics of water need Group DAS
            MetricWaterGroup = new ModelParameterGroupClass("Metrics of need");
            MetricWaterGroup.Add(eModelParam.epCAP_LossPotential_Priority4);
            //
            // Total Surface Water Group
            SurfaceWaterGroup = new ModelParameterGroupClass("Surface Water");
            SurfaceWaterGroup.Add(SRVSurfaceWaterGroup);
            SurfaceWaterGroup.Add(eModelParam.epColorado_Annual_Deliveries);
            SurfaceWaterGroup.Add(eModelParam.epProvider_WaterFromAgSurface);
            SurfaceWaterGroup.Add(eModelParam.epProvider_WaterFromAgSurfaceMax);


            FPM.GroupManager.Add(SurfaceWaterGroup);

            // Total Water Supply Group
            WaterSupplyGroup = new ModelParameterGroupClass("Water Supply", WaterSupplyGroupList);
            WaterSupplyGroup.Add(SurfaceWaterGroup);
            WaterSupplyGroup.Add(GroundWaterGroup);
            FPM.GroupManager.Add(WaterSupplyGroup);

            // Colorado STorage Gorup
            Colorado_StorageGroup = new ModelParameterGroupClass("Colorado Reservoir Storage", Colorado_StorageGroupGroupList);
            FPM.GroupManager.Add(Colorado_StorageGroup);

            // Salt Verde Storage Group
            SRP_StorageGroup = new ModelParameterGroupClass("Salt Verde Reservoir Storage", SRP_StorageGroupGroupList);
            FPM.GroupManager.Add(SRP_StorageGroup);

            // Total Reservoir Storage
            Reservoir_Storage = new ModelParameterGroupClass("Total Reservoir Storage");
            Reservoir_Storage.Add(Colorado_StorageGroup);
            Reservoir_Storage.Add(SRP_StorageGroup);
            FPM.GroupManager.Add(Reservoir_Storage);

            SRP_River_Flow = new ModelParameterGroupClass("Total Salt/Verde River Flow", SRP_River_FlowGroupList);
            FPM.GroupManager.Add(SRP_River_Flow);

            Colorado_River_FlowGroup = new ModelParameterGroupClass("Colorado River FLow");
            FPM.GroupManager.Add(Colorado_River_FlowGroup);

            // Model Control
            Model_ControlGroup = new ModelParameterGroupClass("Model Control");
            FPM.GroupManager.Add(Model_ControlGroup);

            // Climate

            ClimateGroup = new ModelParameterGroupClass("Climate");
            FPM.GroupManager.Add(ClimateGroup);

            PopulationGroup = new ModelParameterGroupClass("Population");
            FPM.GroupManager.Add(PopulationGroup);

            DemandGroup = new ModelParameterGroupClass("Demand");
            DemandGroup.Add(PopulationGroup);

            FPM.GroupManager.Add(DemandGroup);

            UrbanSystemGroup = new ModelParameterGroupClass("Urban System");
            FPM.GroupManager.Add(UrbanSystemGroup);

            IndicatorGroup = new ModelParameterGroupClass("Sustainable Indicators");
            FPM.GroupManager.Add(IndicatorGroup);

            FeedbackGroup = new ModelParameterGroupClass("Feedback");
            FPM.GroupManager.Add(FeedbackGroup);

            Policy_Demand_Group = new ModelParameterGroupClass("Policy Demand");
            FPM.GroupManager.Add(Policy_Demand_Group);

            Policy_UrbanSystem_Group = new ModelParameterGroupClass("Policy System");
            FPM.GroupManager.Add(Policy_UrbanSystem_Group);

            PolicyGroup = new ModelParameterGroupClass("Policy");
            PolicyGroup.Add(Policy_Demand_Group);
            PolicyGroup.Add(Policy_UrbanSystem_Group);
            FPM.GroupManager.Add(PolicyGroup);

            
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSimulation_Start_Year, "The first year of the Simulation (NOTE: in this release the start year SHOULD BE 2006 or 2000).", "Year", "Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { Model_ControlGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSimulation_End_Year, "The last year of the Simulation.", "Year", "Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { Model_ControlGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epColorado_Historical_Extraction_Start_Year, "The first year of the Colorado River flow record that will be used to create a 25 year trace to simulate river flow conditions (from the text file input, the flow record that corresponds to the year chosen, with that year flow and the next 24 years duplicated throughout the entire simulation period).", "Year", "Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { ClimateGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epColorado_Historical_Data_Source, "The source of the Colorado River flow record: Value 1 uses the Bureau of Reclamation recorded record, Value 2 uses the tree ring reconstructed paleo record, Value 3 uses a user supplied river flow trace record (created by the user, and representing the flow for 2011 through 2085 [the historical record is used for 2000-2010]).", "", "Code", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { ClimateGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epColorado_Climate_Adjustment_Percent, "The percent (Value=50 is 50%) which is used to modify the Colorado river flow record, simulating impacts of climate change.  Change starts (or impacts the flow record) in any year that the value differs from 100%.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { ClimateGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epColorado_User_Adjustment_Percent, "The percent (Value=50 is 50%) which is used to modify the Colorado River flow record (decrease, typically), starting and stopping in the years specified (i.e., User_Adjustment_StartYear, User_Adjustment_StopYear).  This is used to simulate a drought condition. ", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { ClimateGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epColorado_User_Adjustment_StartYear, "Determines the year that the [Colorado User Adjustment %] will be first be applied. ", "Year", "Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { ClimateGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epColorado_User_Adjustment_Stop_Year, "Determines the year the [Colorado User Adjustment %] will stop being applied.", "Year", "Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { ClimateGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSaltVerde_Historical_Extraction_Start_Year, "The first year of the Salt Verde River flow record that will be used to create a 25 year trace to simulate river flow conditions (see above: COEXTSTYR).", "Year", "Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { ClimateGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSaltVerde_Historical_Data, "The source of the Salt Verde Rivers flow record: Value=1 uses the Bureau of Reclamation recorded record, Value=2 uses the tree ring reconstructed paleo record, Value=3 uses a user supplied river flow trace record (created by the user, and representing the flow for 2011 through 2085[the historical record is used for 2000-2010]).", "", "Code", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { ClimateGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSaltVerde_Climate_Adjustment_Percent, "The percent (Value=50 is 50%) which is used to modify the Salt Verde River flow record, simulating impacts of climate change.  Change starts at beginning of Simulation (or in any year where the value departs from 100%).", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { ClimateGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSaltVerde_User_Adjustment_Percent, "The percent (Value=50 is 50%) which is used to modify the Salt Verde River flow record, starting and stopping in the years specified.  This is used to (typically) simulate a drought condition. ", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { ClimateGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSaltVerde_User_Adjustment_Start_Year, "Determines the year the [SaltVerde User Adjustment %] will first be applied. ", "Year", "Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { ClimateGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSaltVerde_User_Adjustment_Stop_Year, "Determines the year the [SaltVerde User Adjustment %] will stop being applied.", "Year", "Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { ClimateGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_REG_Growth_Rate_Adjustment, "For all providers adjusts the growth rate for areas on and off project to this value.  100 (100%) leaves the rate at the projected rate.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup, Policy_Demand_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_Demand_Option, "The method that will be used to estimate annual demand  for all providers.  Value=1 reads demand values from an external file, Value=2 calculates demand based on a six year average GPCD and population (separate for each water provider) read from a file, Value=3 estimates demand based on population estimates read from an external file and a smoothing function that slowly declines GPCD (i.e., using ReduceGPCDpct), Value=4 uses same method as 3, but allows the user to manually chanage the GPCD used for each provider at any time; please note that once a change is made that value is maintained throughout the simulation.", "", "Code", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup, Policy_Demand_Group }));
            //ExtendDoc.Add(new WaterSimDescripItem( eModelParam.epPCT_Reduce_GPCD,"The amount by which GPCD is expected to decrease by the end of the simulation (i.e., 2085). Use this when Provider Demand Option is = 3 or Provider Demand Option=4.", "%","Percentage",""));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epModfyNormalFlow, "This parameter was added to the model when it was determined that not all class A (normal flow) water is used by a provider on a given day and, thus, for the year.  This variable simply adjusts the Trott Table estimated designations for each provider for each threshold of river flow. This is done at the start of the simulation.  Units: acre feet per acre x 10 were needed because we are using integers (this is  float as used). That is, a user would enter 15 for 1.5 AF acre-1.  or 9 for a 0.9 AF acre-1., etc. (note true upper estimate is 5.4288 [see Kent Decree]).", "AF*10", "acre feet per acre * 10", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_WaterFromAgSurface, "Water can be removed from the Phoenix AMA (SRV) estimate of agricultural surface water provided by Dale Mason (Fall 2011). This water is added to a water providers groundwater designation", "AF", "Acre Feet", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { WaterSupplyGroup, SurfaceWaterGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_WaterFromAgPumping, "Water can be removed from the Phoenix AMA (SRV) estimate of agricultural pumping of groundwater provided by Dale Mason (Fall 2011). This water is added to a water providers groundwater designation", "AF", "Acre Feet", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[1] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epUse_GPCD, "The GPCD that will be used if [Provider Demand Option] is set to Value=4.", "GPCD", "Gal / Capita / Day", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup, Policy_Demand_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_Wastewater_to_Effluent, "The percent of  Waste water effluent that is used and not discharged into a water course (note: if PCEFFREC [below] is set to 100% no waste water is sent to the traditional WWTP and, so, no effluent will be available for partitioning).", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_WasteWater_to_Reclaimed, "The percent of  Waste water effluent that is sent to a Reclaimed Plant (versus a traditional plant-see figure 1).", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_Reclaimed_to_RO, "The percent of  reclaimed water that is sent to a Reverse Osmosis Plant (thus becomming potable water for direct injection or potable water for use in the next time-step).", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_RO_to_Water_Supply, "The percent of  water from Reverse Osmosis Plant that is used for potable water.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_Reclaimed_to_DirectInject, "The percent of  reclaimed water that is used to recharge an aquifer by direct injection into an aquifer.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_Max_Demand_Reclaim, "The amount of (percent of demand that can be met by) reclaimed water that WILL be used as input for the next year.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_Reclaimed_to_Water_Supply, "The percent of  reclaimed water that is used to meet qualified user demands (non-potable).", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_Reclaimed_to_Vadose, "The percent of  reclaimed water that is delivered to a vadoze zone recharge basin.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_Effluent_to_Vadose, "The percent of  wastewater effluent delivered to a vadose zone recharge basin.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_Effluent_to_PowerPlant, "The percent of  wastewater effluent delivered to a power plants for cooling towers.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSurfaceWater__to_Vadose, "The amount of surface water supply delivered to a vadose zone basin.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSurface_to_Vadose_Time_Lag, "The time in years it takes water recharged to the vadose zone to reach the aquifer to be used as  groundwater.", "yrs", "Years", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWaterBank_Source_Option, "The source of water used for external water banking (outside provider groundwater): Value=1 a percent [% SurfaceWater to WaterBank] of 'unused' surface water is sent to a water bank, Value= 2 a fixed amount[Amount of SurfaceWater to WaterBank] of an unknown extra source of water is sent to a water bank.", "", "Code", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_SurfaceWater_to_WaterBank, "The percent of extra [excess] surface water that is sent to a water bank if [WaterBank Source Option] is set to a Value=1.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[1] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epUse_SurfaceWater_to_WaterBank, "The amount of water (source unknown) sent to a water bank if [WaterBank Source Option] is set to a Value=2.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[1] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_WaterSupply_to_Residential, "The percent of total water supply used by residential customers.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup, Policy_Demand_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_WaterSupply_to_Commercial, "The percent of  total water supply used by commercial users", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup, Policy_Demand_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_WaterSupply_to_Industrial, "The percen of total water supply used by industrial users", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup, Policy_Demand_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epUse_WaterSupply_to_DirectInject, "A fixed amount of potable water supply used for aquifer recharge by directly injecting into a potable aquifer.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_Outdoor_WaterUseRes, "The percent of  potable water supply used for outdoor water uses Residential (indoor water use = 1 - outdoor use).", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { DemandGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_Outdoor_WaterUseCom, "The percent of  potable water supply used for outdoor water uses Commercial (indoor water use = 1 - outdoor use).", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { DemandGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_Outdoor_WaterUseInd, "The percent of  potable water supply used for outdoor water uses Industrial (indoor water use = 1 - outdoor use).", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { DemandGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_Groundwater_Treated, "The percent of  groundwater that is treated before it is used for potable water supply.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_Reclaimed_Outdoor_Use, "The percent of  reclaimed water to be used outdoors. If all available reclaimed water is not used outdoors (i.e., not 100%) it is used indoors as black water.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_Growth_Rate_Adjustment_Other, "For each provder adjusts the growth rate for areas off project (other) to this value.  100 (100%) leaves the rate at the projected rate.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup, PopulationGroup, Policy_Demand_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_Growth_Rate_Adjustment_OnProject, "For each provider adjusts the growth rate for areas on project (SRP Lands) to this value.  100 (100%) leaves the rate at the projected rate.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup, PopulationGroup, Policy_Demand_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSetPopulationsOn, "For each provider sets the population for areas off project (other) to this value. ", "Pers", "People", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup, PopulationGroup, Policy_Demand_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSetPopulationsOther, "For each provider sets the population for areas on project (SRP lands) to this value. ", "Pers", "People", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup, PopulationGroup, Policy_Demand_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epColorado_River_Flow, "The total annual flow in the Colorado River above Lake Powell (the record from  Lees Ferry).", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { Colorado_River_FlowGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPowell_Storage, "The total water storage in Lake Powell.", "AF", "Acre Feet", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { Colorado_StorageGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epMead_Storage, "The total water storage in Lake Mead", "AF", "Acre Feet", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { Colorado_StorageGroup }));

            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSaltVerde_River_Flow, "The total annual flow of the Salt and Verde (and Tonto) Rivers.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { SRP_River_Flow }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSaltTonto_AnnualFlow, "The total annual flow of the Salt River and Tonto Creek.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { SRP_River_Flow }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epVerde_AnnualFlow, "The total annual flow of the Verde River and Tonto Creek.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { SRP_River_Flow }));

            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSaltVerde_Storage, "The total annual storage in the Salt River Project reservoirs.", "AF", "Acre Feet", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { SRP_StorageGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSaltVerde_Spillage, "Spill water over the SVT system- all reservoirs", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epEffluent_To_Agriculture, "The total amount of wastewater effluent delivered to agriculural users.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epMeadLevel, "Elevation of Lake Mead's water surface", "Ft-msl", "Feet (Mean Sea Level)", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { Colorado_StorageGroup }));
            //ExtendDoc.Add(new WaterSimDescripItem( eModelParam.epBasinPumpage,"Salt River Valley pumpage over all model grids", "",new string[] {},new int[] {},""));
            //ExtendDoc.Add(new WaterSimDescripItem( eModelParam.epBasinNetChange,"Net change in the basin over all model grids", "",new string[] {},new int[] {},""));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epGroundwater_Pumped_Municipal, "The total amount of annual groundwater pumped.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[1] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epGroundwater_Balance, "The total groundwater credits available at end of year.", "AF", "Acre Feet", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSaltVerde_Annual_Deliveries_SRP, "The total annual surface water and pumped groundwater delivered by SRP.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { WaterSupplyGroup, SRVSurfaceWaterGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSaltVerde_Class_BC_Designations, "The total annual B & C designated ground water delivered by SRP.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { WaterSupplyGroup, SRVSurfaceWaterGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epColorado_Annual_Deliveries, "The total annual surface water deliveries by CAP, does not included banked water.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { WaterSupplyGroup, SurfaceWaterGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epGroundwater_Bank_Used, "The total annual amount of water delivered from water banking facilities.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epGroundwater_Bank_Balance, "The total banked water supply available at end of year.", "AF", "Acre Feet", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epReclaimed_Water_Used, "The total annual amount of reclaimed water used.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epReclaimed_Water_To_Vadose, "The annual amount of reclaimed water used for vadose zone recharge.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epReclaimed_Water_Discharged, "The annual amount of reclaimed water discharged to a water course (environment).", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epReclaimed_Water_to_DirectInject, "The annual amount of reclaimed water recharged to an aquifer using direct injection.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRO_Reclaimed_Water_Used, "The annual amount of reverse osmosis reclaimed water used for potable water supply.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRO_Reclaimed_Water_to_DirectInject, "The annual amount of reverse osmosis reclaimed water used for aquifer recharge using direct injection.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epEffluent_Reused, "The total annual amount of wastewater effluent reused.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epEffluent_To_Vadose, "The annual amount of effluent used for vadose zone recharge.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epEffluent_To_PowerPlant, "The annual amount of effluent delivered to power plants.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epEffluent_Discharged, "The annual amount of wastewater effluent discharged to a water course (envirionment).", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epDemand_Deficit, "The annual difference between demand and supply (demand - supply), 0 if supply is larger than demand.  This is only valid if pumping is limited to AWS anual allocation", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epTotal_Demand, "The total annual demand from all water customers.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { DemandGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epGPCD_Used, "The GPCD used to estimate demand for the completed simulation year.", "GPCD", "Gallons Per Capita Per Day", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { DemandGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epOnProjectPopulation, "Population for SRP member lands", "Pers", "People", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { DemandGroup, PopulationGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epOtherPopulation, "All other population within the provider boundary", "Pers", "People", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { DemandGroup, PopulationGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPopulation_Used, "The population used (along with GPCD) to estimate demand for the completed simulation year.", "Pers", "People", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { DemandGroup, PopulationGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRegionalGWBalance, "MODFLOW estimates of groundwater on a water provider basis.", "AF", "Acre Feet", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[1] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epDeficit_Total, "Total amount of annual deficit (Total supply minus total demand when total demand exceeds supply, else 0).", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epDeficit_Years, "Number of years to date (cumulative) in which there has been a deficit (not enough water supply to meet demand)", "yrs", "Years", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_GWAvailable, "Percent of original groundwater credits that are available each year.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[1] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epYrsGWZero, "Number of years to date (cumulative) that groundwater credits have been zero or less.", "yrs", "Years", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epYearGWGoesZero, "The earliest year that groundwater credits became zero (or less).", "Year", "Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epYearsNotAssured, "Number of years to date (cumulative) that a 100 year Assured Water Supply could not be demonstrated.", "yrs", "Years", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPercentDeficitLimit, "Percent deficit trigger used to begin reducing GPCD to respond to deficit.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { FeedbackGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epMinimumGPCD, "Lowest value to which GPCD can be reduced.", "GPCD", "Gallons Per Capita Per Day", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { FeedbackGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epYearsOfNonAWSTrigger, "Number of years with not being able to demonstrate 100 year Assured Water Supply before some policy action occurs.", "yrs", "Years", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { FeedbackGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epYearsOfNonAWSGPCDTrigger, "Number of years with not being able to demonstrate 100 year Assured Water Supply before GPCD is reduced.", "Yr", "Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epTotalSupplyUsed, "The total of all supply sources used to meet demand.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[1] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCTGWofDemand, "The percent of demand that is met by ground water pumping.", "%F", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epTotalReclaimedUsed, "The total amount of reclaimed effluent produced.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[1] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvderEffluentToAg, "The amount of effluent that is used for agricutural.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epTotalEffluent, "The total amount of effluent produced (does not include reclaimed).", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { UrbanSystemGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProjectedOnProjectPop, "The annual population initially projected for SRP membership lands.", "Pers", "People", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { DemandGroup, PopulationGroup, FeedbackGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProjectedOtherPop, "The annual population initially projected for non-SRP membership lands.", "Pers", "People", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { DemandGroup, PopulationGroup, FeedbackGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProjectedTotalPop, "The total annual population initially projected.", "Pers", "People", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { DemandGroup, PopulationGroup, FeedbackGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epDifferenceProjectedOnPop, "The deviation (difference) between annual projected population and population used for SRP membership lands.", "Pers", "People", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup, FeedbackGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epDifferenceProjectedOtherPop, "The deviation (difference) between annual projected population and population used for non-SRP membership lands.", "Pers", "People", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup, FeedbackGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epDifferenceTotalPop, "The deviation (difference) between annual total projected population and total population used ", "Pers", "People", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup, FeedbackGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCTDiffProjectedOnPop, "The percent deviation (difference) between annual projected population and population used for SRP membership lands.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup, FeedbackGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCTDiffProjectedOtherPop, "The percent deviation (difference) between annual projected population and population used for non-SRP membership lands.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup, FeedbackGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCTDiffProjectedTotalPop, "The percent deviation (difference) between annual total projected population and total population used ", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup, FeedbackGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPctRecOfTotal, "The percent of reclaimed water used to meet demand annually.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epCreditDeficits, "The annual negative balance in groundwater credits, if groundwater credits are greater than 0, this value is 0.", "AF", "Acre Feet", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPctRecOfTotal, "The annual amount of reclaimed water used as a percent of total supply.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_Deficit, "The annual total deficit (how much demand exceeds supply) as a percent of total demand.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWaterAugmentation, "The annual amount of water supply available from a new source of water not included in the original water portfolio.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[1] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRegionalNaturalRecharge, "The amount of water that is naturally recharged to the regional aquifer annually.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { GroundWaterGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRegionalCAGRDRecharge, "The amount of water that the Central Arizona Groundwater Replishment District recharges to the regional aquifer annually.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { GroundWaterGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRegionalInflow, "The amount of water the moves into the regional aquifer annually from other aquifers.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { GroundWaterGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRegionalAgToVadose, "The amount of water used by aggriculture irrigation that ends up recharging the regional aquifer annually.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { GroundWaterGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRegionalProviderRecharge, "The total amount of water that is recharged to the aquifer annually by all water providers.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { GroundWaterGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRegionalAgOtherPumping, "The total amount of water that is pumped from the regional aquifer annually by all non-water provider users (agricultural and other).", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { GroundWaterGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRegionalOutflow, "The total amount of water that annually leaves the regional aquifer as stream flow or flow to another aquifer.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { GroundWaterGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epOnProjectDemand, "The annual demand from SRP member lands.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { DemandGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epOffProjectDemand, "The annual demand from non-SRP member lands.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { DemandGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epVadoseToAquifer, "The total amount of water that is added annually to the regional aquifer from vadose recharge.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { GroundWaterGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epAnnualIncidental, "The amount of water that is added to groundwater credits annually to account for incidental (outdoor water use) aquifer recharge.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { GroundWaterGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epAlterGPCDpct, "The goal in percent reduction in Gallons per Capita per Day to achieve by the end of the simulation run. 100 = no reduction, 80 = 20% reduction.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPCT_Alter_GPCD, "The regional goal in percent reduction in Gallons per Capita per Day to achieve by the end of the simulation run. 100 = no reduction, 80 = 20% reduction.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epAWSAnnualGWLimit, "Indicates if the AWS rule to limit annual pumping to AWS designated annual credits should be applied (Code=0) or ignored (Code=1)", "Code", "Code", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, GroundWaterGroup, Policy_UrbanSystem_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRegPctGWDemand, "Regional Sustainability Indicator:The percent of demand met by groundwater pumping.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epGWYrsSustain, "Provider Sustainability Indicator:Years that provider groundwater pumping can be sustained until total groundwater credits reach zero.", "Yr", "Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRegGWYrsSustain, "Regional Sustainability Indicator:Average years for the region that provider groundwater pumping can be sustained until total groundwater credits reach zero.", "Yr", "Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epAgSustainIndicator, "Regional Sustainability Indicator: The amount of water agriculture uses as a percentage of total water rights that could be used for agriculture.", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epEnvSustainIndicator, "Regional Sustainability Indicator: The amount of water left in the Colorado and Salt-Verde Rivers as a percent of a maximum amount that could be allocated", "%", "Percentage", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWaterUseIndicator, "Regional Sustainability Indicator: The regional average water use expressed on a scale of 0 to 100 calculated as ((GPCD/30)+1))*100)/9 with GPCDs over 299 = 100", "0-100", "0 Very Low to 100 Very High", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { IndicatorGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epPowellLevel, "Elevation of of Lake Powell's water surface", "Ft-msl", "Feet (Mean Sea Level)", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { Colorado_StorageGroup }));

            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWebUIeffluent, "Allocates effluent to agriculture, urban reuse, environment, or aquifer recharge based on a scale of 0 to 100, with 100 allocating more water to agriculture and urban reusem=, and 0 allocating more water to the environment and aquifer recharge", "0-100", "0 environment/recharge to 100 agriculture/urban reuse",
                //   "NNAA5678901234567890123456789012345 ,new string[] {},new int[] {}
                "% Effluent Used for Environment", new string[] {  }, new int[] {}, new ModelParameterGroupClass[] { PolicyGroup })); // "NNAA5678901234567890123456789012345"));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWebUIeffluent_Ag, "Allocates effluent to agriculture based on a scale of 0 to 100, with 100 allocating more water to agriculture and 0 allocating less", "0-100", "0 low to 100 high",
                "% Effluent Used for Farming", new string[] {  }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWebUIeffluent_Env, "Allocates effluent to environment based on a scale of 0 to 100, with 100 allocating more water to environment and 0 allocating less", "0-100", "0 low to 100 high",
                "% Effluent Used for Environment", new string[] {  }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));
            //ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWebUIAgriculture, "Transfers water rights from agriculture to urban water providers, based on a scale of 0 to 100, with 100 allocating more water to agriculture and 0 allocating less", "0-100", "0 low to 100 high",
            //    "% Farming Water Used for Urban", new string[] {  }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));
            //ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWebPop_GrowthRateAdj_PCT, "Adjustes the population growth rate for all water providers on a scale of 0-150, with 0-no growth to 150-50% increase in growth rate", "0-150", "0-0% to 150-150%",
            //    "% Change in Projected Growth Rate", new string[] {  }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup, PopulationGroup, Policy_Demand_Group }));
            //ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWebUIPersonal_PCT, "Adjusts the Gallons per Capita per Day for all water providers on a scale of 0 to 100, with zero a decline to 50 GPCD and 100 a decline based on past trends.", "0-100", "0-Repid decline to 100-Current trends",
            //    "% Change in Per Capita Water Use", new string[] {  }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, DemandGroup, Policy_Demand_Group }));
            // 07.20.16 DAS start added documentation
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epColorado_Climate_Adjustment_Percent, "A climate factor to adjust flows on the Colorado river (used as a % of historical).", "Unitless", "Unitless", 
                "Climate factor: river flow adjustment ", new string[] { "25", "50", "75", "100" }, new int[] { 25, 50, 75, 100 }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));

            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSaltVerde_Climate_Adjustment_Percent, "A climate factor to adjust flows on the Salt-Verde Rivers (used as a % of historical).", "Unitless", "Unitless", 
                "Climate factor: river flow adjustment ", new string[] { "25", "50", "75", "100" }, new int[] { 25, 50, 75, 100 }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));

            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_DroughtScenarios, "A canned drought scenario.", "Unitless", "Unitless", "Numeric drought scenario", new string[] { "None", "Low", "Mod", "Extreme", "Exceptional" }, new int[] { 0, 1, 2, 3, 4 }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));
            // end 07.20.16 DAS

            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWebWaterBank_PCT, "Store this percent of excess surface water in a longerterm water bank.", "%", "Percentage", "% Excess Water to Bank", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));
           // ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epEnvironmentalFlow_PCT, "Allocates water for the Colorado River Delta based on a scale of 0 to 100 % of the AZ share, with 0=no water and 100=23% of 158088 AF/yr.", "% of AZ share", "0=0 af/yr to 100=23% of 158,088 af/yr", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epEnvironmentalFlow_AFa, "Allocates a specified amount of water for envirionmental by leaving it in the rivers", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWaterAugmentationUsed, "Amount of water that is used from augmented water supplies.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epMeadDeadPool, "The elevation at which stored water can no longer be physcally released from Lake Mead.", "Ft-msl", "Feet (Mean Sea Level)", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { Colorado_StorageGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epMeadLowerSNWA, "The elevation of the lower intake the Southern Nevada Water Authority uses to withdraw water from Lake Mead", "Ft-msl", "Feet (Mean Sea Level)", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, Colorado_StorageGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epMeadShortageSharingLow, "The last elevation that may trigger shortage sharing among lower basin states as per the 2007 Seven Basin States Shortage Sharing Agreement", "Ft-msl", "Feet (Mean Sea Level)", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, Colorado_StorageGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epMeadShortageSharingMed, "The second elevation that may trigger shortage sharing among lower basin states as per the 2007 Seven Basin States Shortage Sharing Agreement", "Ft-msl", "Feet (Mean Sea Level)", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, Colorado_StorageGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epMeadShortageSharingHigh, "The first elevation that may trigger shortage sharing among lower basin states as per the 2007 Seven Basin States Shortage Sharing Agreement", "Ft-msl", "Feet (Mean Sea Level)", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, Colorado_StorageGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epMeadMinPower, "The minimum elevation at which the hydraulic turbines at Lake Mead can produce power.", "Ft-msl", "Feet (Mean Sea Level)", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { Colorado_StorageGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epMeadCapacity, "The maximum elevation that water can be stored in Lake Mead, after which water will spill from the dam.", "Ft-msl", "Feet (Mean Sea Level)", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { Colorado_StorageGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epTotalReclaimedCreated_AF, "The total amount of reclaimed water that was produced.", "AF/yr", "Acre Feet per Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { WaterSupplyGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSaltOther_Storage, "The Acre Fet of water stored in reservoirs of the Salt River other than Lake Roosevelt", "AF", "Acre Feet", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { SRP_StorageGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epRoosevelt_Storage, "The Acre Fet of water stored in Lake Roosevelt reservoir", "AF", "Acre Feet", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { SRP_StorageGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epVerde_Storage, "The Acre Fet of water stored in reservoirs on the Verde River", "AF", "Acre Feet", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { SRP_StorageGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epWebReclaimedWater_PCT, "Sets Regional Wastewater to Reclaimed Values for all providers", "%", "% Wastewater Reclaimed", "% of Wastewater Reclaimed", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { PolicyGroup, UrbanSystemGroup, Policy_UrbanSystem_Group }));
            // DAS 01.26.16
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_DroughtScenarios, "Sets the drought scenarios for both riverine systems", "N.A.", "Severity", "Drought Severity Indexed", new string[4] {"Dry","Moderate", "Extreme", "Exceptional" }, new int[4] {0,1,2,3 }, new ModelParameterGroupClass[] { WaterSupplyGroup }));
            // DAS 07.27.16
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epCAP_LossPotential_Priority4, "Lost potential for CAP water, or desired minus available for priority four water", "AF yr-1", "Acre-feet per year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { MetricWaterGroup }));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epCAP_LossPotential_Priority5, "Lost potential for CAP water, or desired minus available for priority five water", "AF yr-1", "Acre-feet per year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { MetricWaterGroup }));


            //
        }
    }
}
