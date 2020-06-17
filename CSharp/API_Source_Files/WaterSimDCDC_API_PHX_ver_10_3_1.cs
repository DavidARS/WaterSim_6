//      WaterSimDCDC Regional Water Demand and Supply Model Version 5.0
       
//       This is the C# API wrapper for the C# interface to the Fortran Model.
       
//       Version 10
//       12/14/15
//       
//      

//       Keeper: Ray Quay ray.quay@asu.edu
//       
//       Copyright (C) 2011,2012.2013 , The Arizona Board of Regents
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
// NOTES
//   9.0.6 Fixed WebPop parameter bug 
//   9.0.7 Fixed Regional Value Bug for Input Parameters
//   9.0.8 Fixed Stopyr bug, if set for one run and not set in next run, did not use default 2085 used value from previous run
//   9.4.1 Reads External file which can change extended documentation values, see WaterSimDCDC_API_Documentation_V_7_2.cs
//
// Sampson 06.11.15,07.30.15,08.04.15,09.30.16
//====================================================================================
using System;
using System.Collections.Generic;
using System.Text;
#if ExtendedParameter
using WaterSimDCDC.Documentation;
#endif       
       

namespace WaterSimDCDC
{

    /// <summary>
    ///  This is the root class for interface with the WaterSimManager model.
    /// </summary>
    public partial class WaterSimManager : WaterSimManagerClass
    {
        #region  Model dll
          public static bool _FortranOutput =false;           // used to set Fortran debug mode
      public const string FortranDLL = "WaterSimDCDC_model_6.dll";
       //  public const string FortranDLL = "WaterSimDCDC_model_5.dll";
        #endregion
        //
        ProviderIntArray zeroOut = new ProviderIntArray(0);
        //
        #region Fields_Constructor_Base_Methods
        // General Internal Constants
        const int _defaultEndYear = 2001;  // Used to Set Default Start Year
         
        // private fields
        //string _TempDirectoryName;
        //string _DataDirectoryName;
        
        // public debug fields
        //
        static bool FSuspendRangeCheck = false;

        // KLUDGE FOR USE GPCD
        int[] GPCDDefaults = new int[ProviderClass.NumberOfProviders];
        int[] GPCDBuffer = new int[ProviderClass.NumberOfProviders];
        // 06.02.16 DAS
        public bool _COdeltaAZ = false;
        int COdeltaBurdenToAZint = 0;
        // "Static" is the memory location for "public int COdeltaBurdenRatioForAZ" 
        int Static;
        // end 06.02.16 DAS

        // STORAGE FOR SET POPULATION
        // **** Changed 8 13 12 int[] SetPopultaionsValues = new int[ProviderClass.NumberOfProviders];
        int[] SetPopultaionsValuesOn = new int[ProviderClass.NumberOfProviders];
        int[] SetPopultaionsValuesOther = new int[ProviderClass.NumberOfProviders];
        // **** 
        // 
        // STORAGE FOR Wateraugmentation
        // **** Added on 03.15.14
        // DAS
        int[] WaterAugmentationMemory = new int[ProviderClass.NumberOfProviders];
        
        // This is the fortran model, for public access use DeepWaterSim property 
        internal WaterSimU _ws;
        //
        /// <summary>  List of Reclaimed ModelParameters <remarks>used in range check to check to see if their values add up to more than 100</remarks> </summary>
        //int[] reclaimedchecklist = new int[3] {
        //        //eModelParam.epPCT_Reclaimed_to_RO, // Deleted 9/1/11 added back 2/12/14
        //        eModelParam.epPCT_Reclaimed_to_DirectInject,
        //        eModelParam.epPCT_Reclaimed_to_Water_Supply,
        //        eModelParam.epPCT_Reclaimed_to_Vadose 
        //};
        int[] reclaimedchecklist = new int[3] {
                eModelParam.epPCT_Reclaimed_to_DirectInject,
                eModelParam.epPCT_Reclaimed_to_Water_Supply,
                eModelParam.epPCT_Reclaimed_to_Vadose 
        };
      

        /// <summary>  List of Effluent ModelParameters <remarks>used in range check to check to see if their values add up to more than 100</remarks> </summary>
        internal int[] effluentchecklist = new[] {
 //             eModelParam.epPCT_Effluent_Reclaimed,  // opps this is mislabled, should be wasetWater to reclaimed, not effluent
                eModelParam.epPCT_Effluent_to_PowerPlant,
                eModelParam.epPCT_Effluent_to_Vadose 
                
        };
        /// <summary>  List of Effluent ModelParameters <remarks>used in dependencies for Total Effluent</remarks> </summary>
        internal int[] Totaleffluentlist = new[] {
            eModelParam.epEffluent_Discharged,
            eModelParam.epEffluent_To_Agriculture,
            eModelParam.epEffluent_To_PowerPlant,
            eModelParam.epEffluent_To_Vadose
        };
        /// <summary>  List of Effluent ModelParameters <remarks>used in dependencies for Effluent Used</remarks> </summary>
        internal int[] ReUsedeffluentlist = new[] {
            eModelParam.epEffluent_To_Agriculture,
            eModelParam.epEffluent_To_PowerPlant,
            eModelParam.epEffluent_To_Vadose
        };
        /// <summary>  List of Water Use ModelParameters <remarks>used in range check to check to see if their values add up to more than 100</remarks> </summary>
        internal int[] wateruserchecklist = new[] {
                eModelParam.epPCT_WaterSupply_to_Commercial,
                eModelParam.epPCT_WaterSupply_to_Residential,
                eModelParam.epPCT_WaterSupply_to_Industrial
        };
        /// <summary>  List of Water Supply ModelParameters <remarks>used in dependencies for Total Water Supply</remarks> </summary>
        internal int[] watersupplylist = new[] {
               eModelParam.epColorado_Annual_Deliveries,
               eModelParam.epSaltVerde_Annual_Deliveries_SRP,
               eModelParam.epGroundwater_Pumped_Municipal,
               eModelParam.epGroundwater_Bank_Used,
               eModelParam.epWaterAugmentation,
               eModelParam.epTotalReclaimedUsed
        };

        /// <summary>  List of Reclaimed ModelParameters <remarks>used in dependencies for Total Reclaimed</remarks> </summary>
        internal int[] Totalreclaimedlist = new[] {
//             eModelParam.epReclaimed_Water_Used,
            eModelParam.epReclaimed_Water_To_Vadose,
            eModelParam.epReclaimed_Water_Discharged,
            eModelParam.epReclaimed_Water_to_DirectInject,
            eModelParam.epRO_Reclaimed_Water_Used,
            eModelParam.epRO_Reclaimed_Water_to_DirectInject
        };
        internal int[] AgEfficiencylist = new[] {
            eModelParam.epWebAgEfficiency_PCT
         };
        internal int[] Populationlist = new[] {
            eModelParam.epOnProjectDemand,
            eModelParam.epOffProjectDemand
         };
        // 10.11.16 DAS
        // ----------------------------------------------------
      
        // END 10.11.16 DAs
        //
        /// <summary> A Model Parameter Group for Reclaimed Uses. </summary>
        public ModelParameterGroupClass ReclaimedGroup;
        /// <summary> A Model Parameter Group for Effluent Uses. </summary>
        public ModelParameterGroupClass EffluentGroup;
        /// <summary> A Model Parameter Group for Water Demand Uses. </summary>
        public ModelParameterGroupClass WaterUseGroup;
        /// <summary> A Model Parameter Group for Water Supply. </summary>
        public ModelParameterGroupClass DepWaterSupplyGroup;
        /// <summary> A Model Parameter Group for Total Effluent. </summary>
        public ModelParameterGroupClass DepTotalEffluentGroup;
        /// <summary> A Model Parameter Group for Used Effluent. </summary>
        public ModelParameterGroupClass DepUsedEffluentGroup;
        /// <summary> A Model Parameter Group for Total Reclaimed. </summary>
        public ModelParameterGroupClass DepTotalReclaimedGroup;
        // 01.26.17 DAS
        public ModelParameterGroupClass AgricultureGroup;
        // END 10.11.16 DAS
        //----------------------------------------------------------------------------------------------------
        // Constructor
        /// <summary>
        ///  Constructor - Loads DLL (if not yet loaded) and initializes the model, only one instance of this class can be created.  Will throw an exception if more than one is created 
        /// </summary>
        /// <param name="TempDirectoryName">This is where the model places its output files, Creates a diectoy (TempDirectoryName) if it does not exist</param>
        /// <param name="DataDirectoryName">Location of data</param>
        /// <exception cref="WaterSim_Exception"> If an object has already been instantiated</exception>
        /// <exception cref="WaterSim_Exception">if the data directory is invalid</exception>
        /// <exception cref="WaterSim_Exception">Internal Error - If Model Parameters as not initialized properly </exception>
        public string DLL = "";
        public WaterSimManager(string DataDirectoryName, string TempDirectoryName) : base(DataDirectoryName, TempDirectoryName)
        {
            
            // test if the data directory is there.  Critical, model will not load if this is bad
            if (!TestDataDirectory(DataDirectoryName)) throw new WaterSim_Exception(WS_Strings.wsBadDataDirectory);
       
            // Ok Get started with create
            // does not create if alread exists
            UniDB.FileSupport.CreateDirectory(TempDirectoryName);
            string stop = "*";
            _TempDirectoryName = TempDirectoryName;
            //_DataDirectoryName = DataDirectoryName;
            _DataDirectoryName = DataDirectoryName;
            //string DLL = "";
            //FortranDllFileName =   FortranDll.WaterSimDCDC_model_6; //.dll
            //DLL = FortranDllFileName + ".dll";
            // Instantiate WaterSimU - 
          //  _ws = new WaterSimU(_DataDirectoryName, _TempDirectoryName, _FortranOutput);
            _ws = new WaterSimU(_DataDirectoryName, _TempDirectoryName, CreateModelOutputFiles);

            // do not add any if these to the group manager, only used for range checking
            ReclaimedGroup = new ModelParameterGroupClass("Reclaimed", reclaimedchecklist);

            ////// do not add any if these to the group manager, only used for range checking
            //ReclaimedGroup = new ModelParameterGroupClass("Reclaimed",reclaimedchecklist);
            EffluentGroup = new ModelParameterGroupClass("Effluent", effluentchecklist);
            WaterUseGroup = new ModelParameterGroupClass("WaterUse", wateruserchecklist);
            //----
            //
            //--- These are used for dependencies
            DepWaterSupplyGroup = new ModelParameterGroupClass("Dep:TotalSupply", watersupplylist);
            ParamManager.GroupManager.Add(DepWaterSupplyGroup);
            DepTotalEffluentGroup = new ModelParameterGroupClass("Dep:TotalEffluent", Totaleffluentlist);
            ParamManager.GroupManager.Add(DepTotalEffluentGroup);
            DepUsedEffluentGroup = new ModelParameterGroupClass("Dep:TotalReusedEffluent", ReUsedeffluentlist);
            ParamManager.GroupManager.Add(DepUsedEffluentGroup);
            DepTotalReclaimedGroup = new ModelParameterGroupClass("Dep:TotalReclaimed", Totalreclaimedlist);
            ParamManager.GroupManager.Add(DepTotalReclaimedGroup);
            // 01.26.17 DAS
            AgricultureGroup = new ModelParameterGroupClass("Ag Efficiency", AgEfficiencylist);
            ParamManager.GroupManager.Add(AgricultureGroup);
            // End 01.26.17 DAS

            //// Add extended documentation
            initialize_ExtendedDocumentation();
            
            //// Initialize all of the Model Parameters
            initialize_ModelParameters();

            // now test that they are all there
            string TestMsg = "";
            if (!testParmeters(ref TestMsg, false)) throw new WaterSim_Exception(WS_Strings.wsModelParameterMissing + " " + TestMsg);

             
            // Last thing before setup  Call the GPCD Fix  
            GPCDFIX();

            }
       
        //----------------------------------------------------------------------------------------------------
        // finalizer
       
        /// <summary>   Finaliser. </summary>
        /// <remarks>Do not want finalizer disposing of orphaned WaterSim_model.dll stuff</remarks>
        ~WaterSimManager()
        {
            Dispose(false);
        }
        //------------------------------------------------------------------------------------------
        /// <summary>
        /// Called by Dispose() and ~WaterSimManager() Finalizer  Prevents Finalizer from closing model files.
        /// </summary>
        /// <param name="disposing">Finalizer will call with a false</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                WaterSimManager._isWaterSimInstatiated = false;
                //  This is now done by WaterSimU class   _ws.CloseFiles();
                _ws.Dispose();
                       
       
            }
            // Get rid of unmanged stuff, like dbConnections
            base.Dispose(disposing);
        }
        //------------------------------------------------------------------------------------------
        /// <summary>
        /// Must be called before WaterSim object loses scope or is lost (reassigned).
        /// </summary>
        public new void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

       
       
        /*****************************************************
        * General Properties
        * ***************************************************/
        
        
        // DAS September 2014
        /// <summary>
        /// Retains the core WaterSim properties of WaterSim_U - FORTRAN
        /// </summary>
        
        public WaterSimU DeepWaterSim
        {
            get { return _ws; }
        }
      

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Gets the model version. </summary>
        ///
        /// <returns>   The model version. </returns>
        ///
        /// <seealso cref="WaterSimDCDC.WaterSimManagerClass.GetModelVersion()"/>
        ///-------------------------------------------------------------------------------------------------

        protected override string GetModelVersion()
        {
            if (_ws != null)
            {
               // string _ModelBuild = _ws.get_Build + " " + _ws.get_TestVersion;
                string _ModelBuild = _ws.get_Build +  _ws.get_TestVersion;
                return  _ModelBuild.TrimStart(' ');
            }
            else
            {
                return base.GetModelVersion();
            }
        }
        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Gets the default start year. </summary>
        ///
        /// <returns>   . </returns>
        ///
        /// <seealso cref="WaterSimDCDC.WaterSimManagerClass.DefaultStartYear()"/>
        ///-------------------------------------------------------------------------------------------------

        public override int DefaultStartYear()
        {
            return util.WaterSimDCDC_Default_Simulation_StartYear;         
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Default maximum start year. </summary>
        ///
        /// <returns>   An int. </returns>
        ///-------------------------------------------------------------------------------------------------

        public override int DefaultMaxStartYear()
        {
            return util.WaterSimDCDC_Default_Simulation_Max_StartYear;
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Gets the default stop year. </summary>
        ///
        /// <returns>   . </returns>
        ///
        /// <seealso cref="WaterSimDCDC.WaterSimManagerClass.DefaultStopYear()"/>
        ///-------------------------------------------------------------------------------------------------

        public override int DefaultStopYear()
        {
            return util.WaterSimDCDC_Default_Simulation_EndYear; 
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Default minimum stop year. </summary>
        ///
        /// <returns>   An int. </returns>
        ///-------------------------------------------------------------------------------------------------

        public override int DefaultMinStopYear()
        {
            return util.WaterSimDCDC_Default_Simulation_Min_StopYear;
        }
        ///-------------------------------------------------------------------------------------------------
        /// <summary>   If true, suspends Range checking for special range checks. </summary>
        ///
        /// <value> true if suspend range check, false if not. </value>
        ///-------------------------------------------------------------------------------------------------

        public bool Suspend100PctRangeCheck
        {
            get { return FSuspendRangeCheck; }
            set { FSuspendRangeCheck = value; }
        }
                       
        // ------------------------------------------- 
        /// <summary>
        /// Version of the API interface
        /// </summary>
        //public string APiVersion { get { return _APIVersion + " "+DateTime.Now.ToString("(M/d/y H:mm)"); } }
        // ------------------------------------------- 
        /// <summary>
        /// Verson of the Fortran Model
        /// </summary>
        public string ModelBuild { get { return Model_Version;}}// _ModelBuild; } }
        
        // ------------------------------------------- 
        /// <summary>
        /// Tells FORTRAN model to write debug files.  Must be set TRUE before WaterSimManager constructor is called.
        /// </summary>
        public static bool CreateModelOutputFiles
        { 
            get { return _FortranOutput; }  
            set {_FortranOutput = value; } 
        }
        // ------------------------------------------- 
        // DAS June 30, 2016 start
        //public FortranDll FortranDllFileName
        //{
        //    get;
        //    set;
        //}
        //public enum FortranDll
        //{
        //    WaterSimDCDC_model,
        //    WaterSimDCDC_model_5,
        //    WaterSimDCDC_model_6
        //};

        // DAS June 30, 2016 end
        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Gets the pathname of the data directory. </summary>
        /// <value> The pathname of the data directory. </value>
        ///-------------------------------------------------------------------------------------------------
       
        public string DataDirectory
        { get { return _DataDirectoryName; } }
       
        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Gets the pathname of the temp directory. </summary>
        /// <value> The pathname of the temp directory. </value>
        ///-------------------------------------------------------------------------------------------------
       
        public string TempDirectory
        { get { return _TempDirectoryName; } }
       
               
        //---------------------------------------------------
        /// <summary>
        /// Tests to make sure the specified data directoy (path) does contain input files the model needs.
        /// </summary>
        /// <param name="path">path to data directory</param>
        /// <returns>True if data files found, false if not.</returns>
        protected bool TestDataDirectory(string path)
        {
            //string datafile = path + "App_Data\\Data\\Initial_storage.txt";
            //return System.IO.File.Exists(datafile);
            return true;
        }

       
#endregion
        //
        #region ClassInitialization
        /*************************************************************
        * Class Initialzation routines
        * 
        * ************************************************************/
        //----------------------------------------------------------------------------------------------------
         // This method is defined in WaterSimDCDC_User_Mods.cs
        partial void initialize_Provider_Default_ModelParameters();
        //----------------------------------------------------------------------------------------------------
        // This method is defined in WaterSimDCDC_GWParameters.cs
        partial void initialize_GWModelParameters();
        //-----------------------------------------------------------------------
        partial void initialize_Sustainable_ModelParameters();
        //-----------------------------------------------------------------------
        partial void initialize_Derived_ModelParameters();
        //-----------------------------------------------------------------------
        partial void initialize_WebInterface_DerivedParameters();
        //-----------------------------------------------------------------------
        // This method is defined in WaterSimDCDC_User_Mods.cs
        partial void initialize_Other_ModelParameters();
        //----------------------------------------------------------------------------------------------------

        // 06.16.16
        partial void initialize_WaterSim_6_ModelParameters();
        // ----------------------------------------------------------------------
        protected override void initialize_ModelParameters()
        {
            // 06.02.16 DAS added 
            ParameterManagerClass FPM = ParamManager;
            Extended_Parameter_Documentation ExtendDoc = FPM.Extended;
            // end 06.02.16 DAS

            // call the base class method
            base.initialize_ModelParameters();
            // 03.15.15 DAS
          //  CreateModelOutputFiles = false;
            // DAS
            // Build some dependency groups
            ModelParameterGroupClass DemandGroup = new ModelParameterGroupClass("Drill Down for OnOff Demand", new int[2] { eModelParam.epOnProjectDemand, eModelParam.epOffProjectDemand });
            ParamManager.GroupManager.Add(DemandGroup);
            ModelParameterGroupClass PopGroup = new ModelParameterGroupClass("Drill Down for OnOf Population", new int[2] { eModelParam.epOnProjectPopulation, eModelParam.epOtherPopulation });
            ParamManager.GroupManager.Add(PopGroup);
            //
       
            //
            // Initialize Model ProviderArray Input properties
            PCT_alter_GPCD = new providerArrayProperty(_pm, eModelParam.epAlterGPCDpct, get_alterGPCDpct, set_alterGPCDpct, eProviderAggregateMode.agWeighted);
       
            Provider_GPCD_Min = new providerArrayProperty(_pm, eModelParam.epProvider_GPCDmin, get_GPCDmin, set_GPCDmin, eProviderAggregateMode.agWeighted);
            //
            Use_GPCD = new providerArrayProperty(_pm, eModelParam.epUse_GPCD, get_Use_GPCD, set_Use_GPCD, eProviderAggregateMode.agWeighted);
            PCT_Wastewater_Reclaimed = new providerArrayProperty(_pm, eModelParam.epPCT_WasteWater_to_Reclaimed, get_PCT_Wastewater_Reclaimed, set_PCT_Wastewater_Reclaimed, eProviderAggregateMode.agWeighted);
            PCT_Wastewater_to_Effluent = new providerArrayProperty(_pm, eModelParam.epPCT_Wastewater_to_Effluent, get_PCT_Wastewater_to_Effluent, set_PCT_Wastewater_to_Effluent, eProviderAggregateMode.agWeighted);
            PCT_Reclaimed_to_RO = new providerArrayProperty(_pm, eModelParam.epPCT_Reclaimed_to_RO, get_PCT_Reclaimed_to_RO, set_PCT_Reclaimed_to_RO, eProviderAggregateMode.agWeighted);
            PCT_RO_to_Water_Supply = new providerArrayProperty(_pm, eModelParam.epPCT_RO_to_Water_Supply, get_PCT_RO_to_Water_Supply, set_PCT_RO_to_Water_Supply, eProviderAggregateMode.agWeighted);
            PCT_Reclaimed_to_DirectInject = new providerArrayProperty(_pm, eModelParam.epPCT_Reclaimed_to_DirectInject, get_PCT_Reclaimed_to_DirectInject, set_PCT_Reclaimed_to_DirectInject, eProviderAggregateMode.agWeighted);
            PCT_Reclaimed_to_Water_Supply = new providerArrayProperty(_pm, eModelParam.epPCT_Reclaimed_to_Water_Supply, get_PCT_Reclaimed_to_Water_Supply, set_PCT_Reclaimed_to_Water_Supply, eProviderAggregateMode.agWeighted);
            PCT_Reclaimed_to_Vadose = new providerArrayProperty(_pm, eModelParam.epPCT_Reclaimed_to_Vadose, get_PCT_Reclaimed_to_Vadose, set_PCT_Reclaimed_to_Vadose, eProviderAggregateMode.agWeighted);
            PCT_Effluent_to_Vadose = new providerArrayProperty(_pm, eModelParam.epPCT_Effluent_to_Vadose, get_PCT_Effluent_to_Vadose, set_PCT_Effluent_to_Vadose, eProviderAggregateMode.agWeighted);
            PCT_Effluent_to_PowerPlant = new providerArrayProperty(_pm, eModelParam.epPCT_Effluent_to_PowerPlant, get_PCT_Effluent_to_PowerPlant, set_PCT_Effluent_to_PowerPlant, eProviderAggregateMode.agWeighted);
            SurfaceWater__to_Vadose = new providerArrayProperty(_pm, eModelParam.epSurfaceWater__to_Vadose, get_SurfaceWater__to_Vadose, set_SurfaceWater__to_Vadose, eProviderAggregateMode.agSum);
            Surface_to_Vadose_Time_Lag = new providerArrayProperty(_pm, eModelParam.epSurface_to_Vadose_Time_Lag, get_Surface_to_Vadose_Time_Lag, set_Surface_to_Vadose_Time_Lag, eProviderAggregateMode.agAverage);
            WaterBank_Source_Option = new providerArrayProperty(_pm, eModelParam.epWaterBank_Source_Option, get_WaterBank_Source_Option, set_WaterBank_Source_Option, eProviderAggregateMode.agNone);
            PCT_SurfaceWater_to_WaterBank = new providerArrayProperty(_pm, eModelParam.epPCT_SurfaceWater_to_WaterBank, get_PCT_SurfaceWater_to_WaterBank, set_PCT_SurfaceWater_to_WaterBank, eProviderAggregateMode.agWeighted);
            Use_SurfaceWater_to_WaterBank = new providerArrayProperty(_pm, eModelParam.epUse_SurfaceWater_to_WaterBank, get_Use_SurfaceWater_to_WaterBank, set_Use_SurfaceWater_to_WaterBank, eProviderAggregateMode.agSum);
            PCT_WaterSupply_to_Residential = new providerArrayProperty(_pm, eModelParam.epPCT_WaterSupply_to_Residential, get_PCT_WaterSupply_to_Residential, set_PCT_WaterSupply_to_Residential, eProviderAggregateMode.agWeighted);
            PCT_WaterSupply_to_Commercial = new providerArrayProperty(_pm, eModelParam.epPCT_WaterSupply_to_Commercial, get_PCT_WaterSupply_to_Commercial, set_PCT_WaterSupply_to_Commercial, eProviderAggregateMode.agWeighted);
            Use_WaterSupply_to_DirectInject = new providerArrayProperty(_pm, eModelParam.epUse_WaterSupply_to_DirectInject, get_Use_WaterSupply_to_DirectInject, set_Use_WaterSupply_to_DirectInject, eProviderAggregateMode.agWeighted);
            //PCT_Outdoor_WaterUse = new providerArrayProperty(_pm, eModelParam.epPCT_Outdoor_WaterUse, get_PCT_Outdoor_WaterUse, set_PCT_Outdoor_WaterUse, eProviderAggregateMode.agNone);
            PCT_Groundwater_Treated = new providerArrayProperty(_pm, eModelParam.epPCT_Groundwater_Treated, get_PCT_Groundwater_Treated, set_PCT_Groundwater_Treated, eProviderAggregateMode.agWeighted);
            PCT_Reclaimed_Outdoor_Use = new providerArrayProperty(_pm, eModelParam.epPCT_Reclaimed_Outdoor_Use, get_PCT_Reclaimed_Outdoor_Use, set_PCT_Reclaimed_Outdoor_Use, eProviderAggregateMode.agWeighted);
            PCT_Growth_Rate_Adjustment_OnProject = new providerArrayProperty(_pm, eModelParam.epPCT_Growth_Rate_Adjustment_OnProject, get_PCT_Growth_Rate_Adjustment_OnProject, set_PCT_Growth_Rate_Adjustment_OnProject, eProviderAggregateMode.agWeighted);
            PCT_Growth_Rate_Adjustment_Other = new providerArrayProperty(_pm, eModelParam.epPCT_Growth_Rate_Adjustment_Other, get_PCT_Growth_Rate_Adjustment_Other, set_PCT_Growth_Rate_Adjustment_Other, eProviderAggregateMode.agWeighted);
            PCT_Max_Demand_Reclaim = new providerArrayProperty(_pm, eModelParam.epPCT_Max_Demand_Reclaim, get_PCT_Max_Demand_Reclaim, set_PCT_Max_Demand_Reclaim, eProviderAggregateMode.agWeighted);
            // *** Changed 8 12 12 Population_Override = new providerArrayProperty(_pm, eModelParam.epSetPopulations, get_populations, set_populations, eProviderAggregateMode.agNone);
            Population_Override_On = new providerArrayProperty(_pm, eModelParam.epSetPopulationsOn, get_populationsOn, set_populationsOn, eProviderAggregateMode.agSum);
            Population_Override_Other = new providerArrayProperty(_pm, eModelParam.epSetPopulationsOther, get_populationsOther, set_populationsOther, eProviderAggregateMode.agSum);
            //******
            //**** QUAY Added 3/6/13
            WaterAugmentation = new providerArrayProperty(_pm, eModelParam.epWaterAugmentation, get_NewWater, set_NewWater, eProviderAggregateMode.agSum);
            //******
            // DAS Added 03.15.14
            WaterAugmentationUsed = new providerArrayProperty(_pm, eModelParam.epWaterAugmentationUsed, get_NewWaterUsed, null, eProviderAggregateMode.agSum);
            //
            Maximum_normalFlow_rights = new providerArrayProperty(_pm, eModelParam.epProvider_Max_NormalFlow, get_normalFlow_rights_max, set_normalFlow_rights_max, eProviderAggregateMode.agSum);
 
            PCT_modify_normalFlow = new providerArrayProperty(_pm, eModelParam.epModfyNormalFlow, get_modifyNormalFlow, set_modifyNormalFlow, eProviderAggregateMode.agWeighted);
       
            // Initialize Model ProviderArray Output properties
               
            Groundwater_Pumped_Municipal = new providerArrayProperty(_pm,eModelParam.epGroundwater_Pumped_Municipal, get_Groundwater_Pumped_Municipal, eProviderAggregateMode.agSum);
            // This is actually total credits not any regional water balance
            Groundwater_Balance = new providerArrayProperty(_pm, eModelParam.epGroundwater_Balance, get_Groundwater_Balance, eProviderAggregateMode.agSum);
            SaltVerde_Annual_Deliveries_SRP = new providerArrayProperty(_pm, eModelParam.epSaltVerde_Annual_Deliveries_SRP, get_SaltVerde_Annual_Deliveries_SRP, eProviderAggregateMode.agSum);
            SaltVerde_Class_BC_Designations = new providerArrayProperty(_pm, eModelParam.epSaltVerde_Class_BC_Designations, get_SaltVerde_Class_BC_Designations, eProviderAggregateMode.agSum);
            Colorado_Annual_Deliveries = new providerArrayProperty(_pm, eModelParam.epColorado_Annual_Deliveries, get_Colorado_Annual_Deliveries, eProviderAggregateMode.agSum);
            Groundwater_Bank_Used = new providerArrayProperty(_pm, eModelParam.epGroundwater_Bank_Used, get_Groundwater_Bank_Used, eProviderAggregateMode.agSum);           
            Groundwater_Bank_Balance = new providerArrayProperty(_pm, eModelParam.epGroundwater_Bank_Balance, get_Groundwater_Bank_Balance, eProviderAggregateMode.agSum);
            Reclaimed_Water_Used = new providerArrayProperty(_pm, eModelParam.epReclaimed_Water_Used, get_Reclaimed_Water_Used, eProviderAggregateMode.agSum);          
            Reclaimed_Water_To_Vadose = new providerArrayProperty(_pm, eModelParam.epReclaimed_Water_To_Vadose, get_Reclaimed_Water_To_Vadose, eProviderAggregateMode.agSum);            
            Reclaimed_Water_Discharged = new providerArrayProperty(_pm, eModelParam.epReclaimed_Water_Discharged, get_Reclaimed_Water_Discharged, eProviderAggregateMode.agSum);          
            Reclaimed_Water_to_DirectInject = new providerArrayProperty(_pm, eModelParam.epReclaimed_Water_to_DirectInject, get_Reclaimed_Water_to_DirectInject, eProviderAggregateMode.agSum);
            RO_Reclaimed_Water_Used = new providerArrayProperty(_pm, eModelParam.epRO_Reclaimed_Water_Used, get_RO_Reclaimed_Water_Used, eProviderAggregateMode.agSum);          
            RO_Reclaimed_Water_to_DirectInject = new providerArrayProperty(_pm, eModelParam.epRO_Reclaimed_Water_to_DirectInject, get_RO_Reclaimed_Water_to_DirectInject, eProviderAggregateMode.agSum);            
            Total_Effluent_Reused = new providerArrayProperty(_pm, eModelParam.epEffluent_Reused, get_Total_Effluent_Reused, eProviderAggregateMode.agSum);            
            Effluent_To_Vadose = new providerArrayProperty(_pm, eModelParam.epEffluent_To_Vadose, get_Effluent_To_Vadose, eProviderAggregateMode.agSum);            
            Effluent_To_PowerPlant = new providerArrayProperty(_pm, eModelParam.epEffluent_To_PowerPlant, get_Effluent_To_PowerPlant, eProviderAggregateMode.agSum);            
            Effluent_Discharged = new providerArrayProperty(_pm, eModelParam.epEffluent_Discharged, get_Effluent_Discharged, eProviderAggregateMode.agSum);
            Demand_Deficit = new providerArrayProperty(_pm, eModelParam.epDemand_Deficit, get_Demand_Deficit, eProviderAggregateMode.agSum);
            Total_Demand = new providerArrayProperty(_pm, eModelParam.epTotal_Demand, get_Total_Demand, eProviderAggregateMode.agSum);
            GPCD_Used = new providerArrayProperty(_pm, eModelParam.epGPCD_Used, get_GPCD_Used, eProviderAggregateMode.agWeighted);
            Population_Used = new providerArrayProperty(_pm, eModelParam.epPopulation_Used, get_Population_Used, eProviderAggregateMode.agSum);
            PCT_WaterSupply_to_Industrial = new providerArrayProperty(_pm, eModelParam.epPCT_WaterSupply_to_Industrial, get_PCT_WaterSupply_to_Industrial, set_PCT_WaterSupply_to_Industrial, eProviderAggregateMode.agWeighted);
            PCT_Outdoor_WaterUseRes = new providerArrayProperty(_pm, eModelParam.epPCT_Outdoor_WaterUseRes, get_PCT_Outdoor_WaterUseRes, set_PCT_Outdoor_WaterUseRes, eProviderAggregateMode.agWeighted);
            PCT_Outdoor_WaterUseCom = new providerArrayProperty(_pm, eModelParam.epPCT_Outdoor_WaterUseCom, get_PCT_Outdoor_WaterUseCom, set_PCT_Outdoor_WaterUseCom, eProviderAggregateMode.agWeighted);
            PCT_Outdoor_WaterUseInd = new providerArrayProperty(_pm, eModelParam.epPCT_Outdoor_WaterUseInd, get_PCT_Outdoor_WaterUseInd, set_PCT_Outdoor_WaterUseInd, eProviderAggregateMode.agWeighted);
            Demand_On_Project = new providerArrayProperty(_pm, eModelParam.epOnProjectDemand, get_Demand_On_Project, eProviderAggregateMode.agSum);         
            Demand_Off_Project = new providerArrayProperty(_pm, eModelParam.epOffProjectDemand, get_Demand_Off_Project, eProviderAggregateMode.agSum);
            Population_On_Project = new providerArrayProperty(_pm, eModelParam.epOnProjectPopulation, get_Population_OnProject, eProviderAggregateMode.agSum);
            Population_Other = new providerArrayProperty(_pm, eModelParam.epOtherPopulation, get_Population_Other, eProviderAggregateMode.agSum);
       
            Incidental_Water_Credit = new providerArrayProperty(_pm, eModelParam.epAnnualIncidental, get_Incidental_Water_Credit, eProviderAggregateMode.agSum);
            Total_Vadose_To_Aquifer = new providerArrayProperty(_pm, eModelParam.epVadoseToAquifer, get_Total_Vadose_To_Aquifer_Flux, eProviderAggregateMode.agSum);

            Total_WWTP_Effluent = new providerArrayProperty(_pm, eModelParam.epTWWTPCreated_AF, get_TWWTP_Created, eProviderAggregateMode.agWeighted);
            GPCD_raw = new providerArrayProperty(_pm, eModelParam.epGPCDraw, get_GPCDraw, eProviderAggregateMode.agWeighted);
            Reclaimed_Water_Created = new providerArrayProperty(_pm, eModelParam.epTotalReclaimedCreated_AF, get_Reclaimed_Water_Created, eProviderAggregateMode.agWeighted);
            //
            SaltVerde_Annual_GW_Deliveries_SRP = new providerArrayProperty(_pm, eModelParam.epSaltVerde_Annual_GW_Deliveries_SRP, get_SaltVerde_Annual_GWDeliveries_SRP, eProviderAggregateMode.agSum);
            // 07.28.15
            Colorado_Max_Deliveries = new providerArrayProperty(_pm, eModelParam.epColorado_Max_Deliveries, get_Colorado_Max_Deliveries, eProviderAggregateMode.agSum);
            // 07.29.15
            Colorado_Unused_PriorityFour = new providerArrayProperty(_pm, eModelParam.epColorado_Unused_P4, get_Colorado_Unused_PriorityFour, eProviderAggregateMode.agSum);
            Colorado_Unused_PriorityFive = new providerArrayProperty(_pm, eModelParam.epColorado_Unused_P5, get_Colorado_Unused_PriorityFive, eProviderAggregateMode.agSum);
            //
            Default_Pumping_MnI_PCT = new providerArrayProperty(_pm, eModelParam.epPCT_Default_MnI_Pumping, get_Default_MandI_Pumping_PCT, set_Default_MandI_Pumping_PCT, eProviderAggregateMode.agWeighted);
            //
            CAP_LossPotential_PriorityFour = new providerArrayProperty(_pm, eModelParam.epCAP_LossPotential_Priority4, get_CAPshortages_PriorityFour, eProviderAggregateMode.agSum);
            CAP_LossPotential_PriorityFive = new providerArrayProperty(_pm, eModelParam.epCAP_LossPotential_Priority5, get_CAPshortages_PriorityFive, eProviderAggregateMode.agSum);
            //
            //CAP_DemandNotMet_PriorityFour = new providerArrayProperty(_pm, eModelParam.epCAP_DemandNotMet_Priority4, get_CAPdemandNotMet_PriorityFour, eProviderAggregateMode.agSum);
            //CAP_DemandNotMet_PriorityFive = new providerArrayProperty(_pm, eModelParam.epCAP_DemandNotMet_Priority5, get_CAPdemandNotMet_PriorityFive, eProviderAggregateMode.agSum);
            //
            Showers_MinPCD = new providerArrayProperty(_pm, eModelParam.epProvider_ShowersMinPCD, get_Showers, eProviderAggregateMode.agAverage);
            /*************************************************************************
            * Setup all Model Parameters in the Prameter Manager
            * 
            
            * 
            *************************************************************************/
            //
            #endregion
        // Field Definitions (DAS)
        //
        #region Inputs and Outputs
            // Base Inputs
            #region BI
            // Located in API_Base_ver_1.cs
            //_pm.AddParameter(new ModelParameterClass(eModelParam.epSimulation_Start_Year, "Simulation Start Year", "STARTYR",  rangeChecktype.rctCheckRange, 2000, 2006, geti_Simulation_Start_Year, seti_Simulation_Start_Year, RangeCheck.NoSpecialBase));
            //_pm.AddParameter(new ModelParameterClass(eModelParam.epSimulation_End_Year, "Simulation End Year", "STOPYR",  rangeChecktype.rctCheckRange, _defaultEndYear, 2085, geti_Simulation_End_Year, seti_Simulation_End_Year, RangeCheck.NoSpecialBase));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epColorado_Historical_Extraction_Start_Year, "Colorado Flow Trace Start Year", "COEXTSTYR",  rangeChecktype.rctCheckRangeSpecial, 762, 1979, geti_Colorado_Historical_Extraction_Start_Year, seti_Colorado_Historical_Extraction_Start_Year, ColoradoYearRangeCheck));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epColorado_Historical_Data_Source, "Colorado Historical Data Source", "COSRC",  rangeChecktype.rctCheckRange, 1, 3, geti_Colorado_Historical_Data_Source, seti_Colorado_Historical_Data_Source, RangeCheck.NoSpecialBase));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epColorado_Climate_Adjustment_Percent, "CO: Adjust Flows", "COCLMADJ",  rangeChecktype.rctCheckRange, 0, 130, geti_Colorado_Climate_Adjustment_Percent, seti_Colorado_Climate_Adjustment_Percent, RangeCheck.NoSpecialBase));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epColorado_User_Adjustment_Percent, "CO: Adjust Drought", "COUSRADJ",  rangeChecktype.rctCheckRange, 0, 100, geti_Colorado_User_Adjustment_Percent, seti_Colorado_User_Adjustment_Percent, RangeCheck.NoSpecialBase));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epColorado_User_Adjustment_StartYear, "Colorado Drought Start Year", "COUSRSTR",  rangeChecktype.rctCheckRange, 2006, 2081, geti_Colorado_User_Adjustment_StartYear, seti_Colorado_User_Adjustment_StartYear,RangeCheck.NoSpecialBase));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epColorado_User_Adjustment_Stop_Year, "Colorado Drought Stop Year", "COUSRSTP",  rangeChecktype.rctCheckRange, 2006, 2081, geti_Colorado_User_Adjustment_Stop_Year, seti_Colorado_User_Adjustment_Stop_Year, RangeCheck.NoSpecialBase));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSaltVerde_Historical_Extraction_Start_Year, "SaltVerde Flows Trace Start Year", "SVEXTSTYR",  rangeChecktype.rctCheckRangeSpecial, 1330, 1979, geti_SaltVerde_Historical_Extraction_Start_Year, seti_SaltVerde_Historical_Extraction_Start_Year, SaltVerdeYearRangeCheck));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSaltVerde_Historical_Data, "Salt-Verde Trace Data Source", "SVSRC",  rangeChecktype.rctCheckRange, 1, 3, geti_SaltVerde_Historical_Data, seti_SaltVerde_Historical_Data, RangeCheck.NoSpecialBase));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSaltVerde_Climate_Adjustment_Percent, "Salt-Verde: Adjust Flows", "SVCLMADJ",  rangeChecktype.rctCheckRange, 0, 130, geti_SaltVerde_Climate_Adjustment_Percent, seti_SaltVerde_Climate_Adjustment_Percent, RangeCheck.NoSpecialBase));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSaltVerde_User_Adjustment_Percent, "Salt-Verde: Adjust Drought", "SVUSRADJ",  rangeChecktype.rctCheckRange, 0, 100, geti_SaltVerde_User_Adjustment_Percent, seti_SaltVerde_User_Adjustment_Percent, RangeCheck.NoSpecialBase));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSaltVerde_User_Adjustment_Start_Year, "Salt-Verde Drought Start Year", "SVUSRSTR",  rangeChecktype.rctCheckRange, 2006, 2081, geti_SaltVerde_User_Adjustment_Start_Year, seti_SaltVerde_User_Adjustment_Start_Year, RangeCheck.NoSpecialBase));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSaltVerde_User_Adjustment_Stop_Year, "Salt-Verde Drought Stop Year", "SVUSRSTP",  rangeChecktype.rctCheckRange, 2006, 2081, geti_SaltVerde_User_Adjustment_Stop_Year, seti_SaltVerde_User_Adjustment_Stop_Year, RangeCheck.NoSpecialBase));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epProvider_Demand_Option, "Provider Demand Option", "DMOPT",  rangeChecktype.rctCheckRange, 1, 4, geti_Provider_Demand_Option, seti_Provider_Demand_Option, RangeCheck.NoSpecialBase));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Alter_GPCD, "Reduction in GPCD, by 2085", "PCRDGPCD",  rangeChecktype.rctCheckRange, 0, 70, geti_PCT_Reduce_GPCD, seti_PCT_Reduce_GPCD, RangeCheck.NoSpecialBase));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epAWSAnnualGWLimit, "Ignore Rule to Limit Pumping to Annual AWS GW Credit (0=False)", "AWSLIMIT",  rangeChecktype.rctCheckRange, 0, 1, geti_Assured_Water_Supply_Annual_Groundwater_Pumping_Limit, seti_Assured_Water_Supply_Annual_Groundwater_Pumping_Limit, RangeCheck.NoSpecialBase));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_REG_Growth_Rate_Adjustment, "Regional Pop Growth Rate", "REGPOPGR", rangeChecktype.rctCheckRange, 0, 300, geti_PCT_REG_Growth_Rate_Adjustment, seti_PCT_REG_Growth_Rate_Adjustment, RangeCheck.NoSpecialBase));
            // DAS
            _pm.AddParameter(new ModelParameterClass(eModelParam.epDefault_Status, "Anytime a parameter changes flag True", "DFAULT", rangeChecktype.rctCheckRange, 0, 1, geti_API_Default_Status, seti_API_Default_Status, RangeCheck.NoSpecialBase));
            // DAS 01.26.16
            _pm.AddParameter(new ModelParameterClass(eModelParam.epProvider_DroughtScenarios, "Severity: " + chosenDrought, "DROUSCEN", rangeChecktype.rctCheckRange, 0, 3, geti_API_Drought_Scenario, seti_API_Drought_Scenario, RangeCheck.NoSpecialBase));
            //
            // 06.02.16 DAS
            _pm.AddParameter(new ModelParameterClass(eModelParam.epCOdeltaBurden, "Burden for AZ for CO delta Water from M&I budget", "CODELTAB", rangeChecktype.rctCheckRange, 0, 1, geti_COdeltaBurdenForAZ, seti_COdeltaBurdenForAZ, RangeCheck.NoSpecialBase));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epCOdeltaBurden, "The proportion of environmental water for the CO Delta from AZ.", "Proportion", "Proportion of Total", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));

            _pm.AddParameter(new ModelParameterClass(eModelParam.epCOdeltaRatioOfBurden, "Ratio of Burden for AZ for CO delta Water from M&I budget", "CODELTAR", rangeChecktype.rctCheckRange, 0, 0, geti_COdeltaBurdenRatioForAZ, seti_COdeltaBurdenRatioForAZ, RangeCheck.NoSpecialBase));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epCOdeltaRatioOfBurden, "The sustainability Index for Environmental water.", "Percent", "Percent of Total Share", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
            //
            _pm.AddParameter(new ModelParameterClass(eModelParam.epBaseYear, "Base Year: current calendar", "BASEYR", rangeChecktype.rctCheckRange, 2000, 2020, geti_BaseYear, seti_BaseYear, RangeCheck.NoSpecialBase));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epBaseYear, "The year that determines when a policy is invoked", "Year", "Year", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
            //
            // 10.03.16 DAS
            _pm.AddParameter(new ModelParameterClass(eModelParam.epBase_Preset_Scenarios, "Preset Scenario: " + chosenScenario, "SCENARIO", rangeChecktype.rctCheckRange, 0, 4, geti_API_Scenario_Presets, seti_API_Scenario_Presets, RangeCheck.NoSpecialBase));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epBase_Preset_Scenarios, "Canned presets for the User Interface", "Unitless", "Unitless", "", new string[] { "None", "Tech Mgt", "Water Security", "Sustainability", "Demand Mgt" }, new int[] { 0, 1, 2, 3, 4 }, new ModelParameterGroupClass[] { }));

            // END 10.03.16 DAS


            #endregion
            // *****

            // Provider Inputs
            #region Provider Inputs

            _pm.AddParameter(new ModelParameterClass(eModelParam.epAlterGPCDpct, "Alter the provider trend in GPCD", "ALTRGPCD", modelParamtype.mptInputProvider, rangeChecktype.rctCheckRange, -95, 95 /* was 0 Quay changed to 95 12 13 2015 */, null, get_alterGPCDpct, null, set_alterGPCDpct, null, null, PCT_alter_GPCD));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epUse_GPCD, "Use GPCD", "USEGPCD", rangeChecktype.rctCheckRange, -1, 2000, RangeCheck.NoSpecialProvider,Use_GPCD));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_WasteWater_to_Reclaimed, "Effluent to Reclaimed Plant", "PCEFFREC", rangeChecktype.rctCheckRange, 0, 100, RangeCheck.NoSpecialProvider, PCT_Wastewater_Reclaimed));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Wastewater_to_Effluent, "Total Wastewater is Usable Effluent", "PCWWEFF", rangeChecktype.rctCheckRange, 0, 100, RangeCheck.NoSpecialProvider,PCT_Wastewater_to_Effluent));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Reclaimed_to_RO, "Reclaimed to RO", "PCRECRO", rangeChecktype.rctCheckRange, 0, 100, RangeCheck.NoSpecialProvider, PCT_Reclaimed_to_RO));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_RO_to_Water_Supply, "RO to Water Supply", "PCROWS", rangeChecktype.rctCheckRange, 0, 100, RangeCheck.NoSpecialProvider,PCT_RO_to_Water_Supply));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Reclaimed_to_DirectInject, "Reclaimed to DirectInject", "PCRECDI", rangeChecktype.rctCheckRangeSpecial, 0, 100, PCTReclaimedRangeCheck,PCT_Reclaimed_to_DirectInject));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Reclaimed_to_Water_Supply, "Reclaimed to Water Supply", "PCERECWS", rangeChecktype.rctCheckRangeSpecial, 0, 100, PCTReclaimedRangeCheck,PCT_Reclaimed_to_Water_Supply));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Reclaimed_to_Vadose, "Reclaimed to Vadose", "PCRECVAD", rangeChecktype.rctCheckRangeSpecial, 0, 100, PCTReclaimedRangeCheck,PCT_Reclaimed_to_Vadose));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Effluent_to_Vadose, "Effluent to Vadose", "PCEFFVAD", rangeChecktype.rctCheckRangeSpecial, 0, 100, PCTEffluentRangeCheck,PCT_Effluent_to_Vadose));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Effluent_to_PowerPlant, "Effluent to PowerPlant", "PCEFFPP", rangeChecktype.rctCheckRangeSpecial, 0, 100, PCTEffluentRangeCheck,PCT_Effluent_to_PowerPlant));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSurfaceWater__to_Vadose, "SurfaceWater to Vadose", "PCSWVAD", rangeChecktype.rctCheckRange, 0, 100000, RangeCheck.NoSpecialProvider,SurfaceWater__to_Vadose));
            // No longer used
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSurface_to_Vadose_Time_Lag, "Surface to Vadose Time Lag in Years", "VADLAG", rangeChecktype.rctCheckRange, 0, 50, RangeCheck.NoSpecialProvider,Surface_to_Vadose_Time_Lag));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epWaterBank_Source_Option, "WaterBank Source Option", "WBOPT", rangeChecktype.rctCheckRange, 1, 2, RangeCheck.NoSpecialProvider,WaterBank_Source_Option));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_SurfaceWater_to_WaterBank, "SurfaceWater to WaterBank PCT", "PCSWWB", rangeChecktype.rctCheckRange, 0, 100, RangeCheck.NoSpecialProvider,PCT_SurfaceWater_to_WaterBank));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epUse_SurfaceWater_to_WaterBank, "SurfaceWater to WaterBank Amt", "SWWB", rangeChecktype.rctCheckRange, 0, 100000, RangeCheck.NoSpecialProvider,Use_SurfaceWater_to_WaterBank));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_WaterSupply_to_Residential, "WaterSupply to Residential", "PCWSRES", rangeChecktype.rctCheckRangeSpecial, 0, 100, ResComPCTRangeCheck,PCT_WaterSupply_to_Residential));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_WaterSupply_to_Commercial, "WaterSupply to Commercial", "PCWSCOM", rangeChecktype.rctCheckRangeSpecial, 0, 100, ResComPCTRangeCheck,PCT_WaterSupply_to_Commercial));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epUse_WaterSupply_to_DirectInject, "Water Supply to DirectInject", "USEWSDI", rangeChecktype.rctCheckRange, 0, 100000, RangeCheck.NoSpecialProvider,Use_WaterSupply_to_DirectInject));
            // Modified 7 23 12 _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Outdoor_WaterUse, "% Outdoor Water Use", "PGOUTUSE", rangeChecktype.rctCheckRange, 0, 100, RangeCheck.NoSpecialProvider, get_PCT_Outdoor_WaterUse, RangeCheck.NoSpecialProvider, set_PCT_Outdoor_WaterUse,RangeCheck.NoSpecialProvider,RangeCheck.NoSpecialProvider,PCT_Outdoor_WaterUse));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Groundwater_Treated, "Groundwater Treated", "PCGWTRT", rangeChecktype.rctCheckRange, 0, 100, RangeCheck.NoSpecialProvider,PCT_Groundwater_Treated));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Reclaimed_Outdoor_Use, "Outdoor Can Use Reclaimed", "PCRECOUT", rangeChecktype.rctCheckRange, 0, 100, RangeCheck.NoSpecialProvider,PCT_Reclaimed_Outdoor_Use));
            // Modified 7 23 12 _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Growth_Rate_Adjustment, "% Growth Rate Adjustment", "PCGRWRTE", rangeChecktype.rctCheckRange, 0, 300, RangeCheck.NoSpecialProvider, get_PCT_Growth_Rate_Adjustment, RangeCheck.NoSpecialProvider, set_PCT_Growth_Rate_Adjustment,RangeCheck.NoSpecialProvider,RangeCheck.NoSpecialProvider,PCT_Growth_Rate_Adjustment));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Growth_Rate_Adjustment_OnProject, "Growth Rate Adj On Project", "PCGRTON", rangeChecktype.rctCheckRange, 0, 300, RangeCheck.NoSpecialProvider,PCT_Growth_Rate_Adjustment_OnProject));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Growth_Rate_Adjustment_Other, "Growth Rate Adj Other", "PCGRTOFF", rangeChecktype.rctCheckRange, 0, 300, RangeCheck.NoSpecialProvider,PCT_Growth_Rate_Adjustment_Other));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Max_Demand_Reclaim, "Max Demand Reclaimed", "PCDEMREC", rangeChecktype.rctCheckRange, 0, 70, RangeCheck.NoSpecialProvider, PCT_Max_Demand_Reclaim));
            // *** changed 8 13 12 _pm.AddParameter(new ModelParameterClass(eModelParam.epSetPopulations, "Provider Population Override", "POPOVRD", rangeChecktype.rctNoRangeCheck, 0, 0, RangeCheck.NoSpecialProvider, get_populations , RangeCheck.NoSpecialProvider, set_populations, RangeCheck.NoSpecialProvider, RangeCheck.NoSpecialProvider, Population_Override));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSetPopulationsOn, "Provider On-Project Pop Override", "POPOVON", rangeChecktype.rctNoRangeCheck, 0, 0, RangeCheck.NoSpecialProvider, Population_Override_On));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSetPopulationsOther, "Provider Off-Project Pop Override", "POPOVOFF", rangeChecktype.rctNoRangeCheck, 0, 0, RangeCheck.NoSpecialProvider, Population_Override_Other));
            //*************
            // normal

            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_WaterSupply_to_Industrial, "WaterSupply to Industrial", "PCWSIND", rangeChecktype.rctCheckRangeSpecial, 0, 100,  ResComPCTRangeCheck, PCT_WaterSupply_to_Industrial));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Outdoor_WaterUseRes, "Res Outdoor Water Use", "PROUTUSE", rangeChecktype.rctCheckRange, 0, 100, RangeCheck.NoSpecialProvider, PCT_Outdoor_WaterUseRes));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Outdoor_WaterUseCom, "Com Outdoor Water Use", "PCOUTUSE", rangeChecktype.rctCheckRange, 0, 100, RangeCheck.NoSpecialProvider, PCT_Outdoor_WaterUseCom));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Outdoor_WaterUseInd, "Ind Outdoor Water Use", "PIOUTUSE", rangeChecktype.rctCheckRange, 0, 100, RangeCheck.NoSpecialProvider, PCT_Outdoor_WaterUseInd));
            // QUAY Added 3/6/13
//            _pm.AddParameter(new ModelParameterClass(eModelParam.epWaterAugmentation, "Amount of Augmented Water", "WATAUG", rangeChecktype.rctCheckRange, 0, 100000, RangeCheck.NoSpecialProvider, WaterAugmentation));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epWaterAugmentation, "Augmented", "WATAUG", rangeChecktype.rctCheckRange, 0, 100000, RangeCheck.NoSpecialProvider, WaterAugmentation));

            //
            // DAS these appear redundant, but are not. "MAX" in a provider maximum in the event that every provider has a separate normal flow right
            // the Modify Normal flow is the adjustment of the normal flow. IN FACT, the FORTRAN model takes the minimum of the two
            // 08.12.16
            _pm.AddParameter(new ModelParameterClass(eModelParam.epProvider_Max_NormalFlow, "The maximum right usable", "NFLOWMAX", modelParamtype.mptInputProvider, rangeChecktype.rctCheckRange, 0, 554, null, get_normalFlow_rights_max, null, set_normalFlow_rights_max, null, null, Maximum_normalFlow_rights));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epModfyNormalFlow, "Modify Normal Flow", "MNFLOW", rangeChecktype.rctCheckRange, 0, 554, RangeCheck.NoSpecialProvider, PCT_modify_normalFlow));
            //

            // DAS added 01.07.15
            _pm.AddParameter(new ModelParameterClass(eModelParam.epProvider_GPCDmin, "The minimum GPCD obtainable", "GPCDMIN", modelParamtype.mptInputProvider, rangeChecktype.rctCheckRange, 0, 200, null, get_GPCDmin, null, set_GPCDmin, null, null, Provider_GPCD_Min));
            // DAS 07.30.15
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPCT_Default_MnI_Pumping, "The % of Demand as default M&I pumping", "PCPUMPDFLT", modelParamtype.mptInputProvider, rangeChecktype.rctCheckRange, 0, 5, null, get_Default_MandI_Pumping_PCT, null, set_Default_MandI_Pumping_PCT, null, null, Default_Pumping_MnI_PCT));
            //
            // DAS 06.02.16
            //     ----01.21.15 DAS
            GPCD_Res = new providerArrayProperty(_pm, eModelParam.epProvider_GPCDres, get_GPCD_Res, eProviderAggregateMode.agWeighted);
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_GPCDres, "Residential GPCD", "GPCDRES", GPCD_Res, 0, 300));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_GPCDres, "Residential GPCD", "GPCD", "Gallons per Capita per Day", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
            // 06.19.15
            GPCD_ComInd = new providerArrayProperty(_pm, eModelParam.epProvider_GPCDcomInd, get_GPCD_ComInd, eProviderAggregateMode.agWeighted);
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_GPCDcomInd, "Com and Ind GPCD", "GPCDCOMIND", GPCD_ComInd, 0, 500));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_GPCDcomInd, "Commercial and Industrial combined GPCD", "GPCD", "Gallons per Capita per Day", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));

            // 01.19.15
            Gross_Agriculture_WaterPumped_AF = new providerArrayProperty(_pm, eModelParam.epProvider_AgWaterPumped, get_Gross_Ag_pumped, eProviderAggregateMode.agSum);
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_AgWaterPumped, "Agricultural Water Pumped", "AGPUMPED", Gross_Agriculture_WaterPumped_AF, 0, 400000));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_AgWaterPumped, "Total Agriculture Pumped: ADWR estimate", "AF annum-1", "Acre-feet per annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
            // 11.20.16 I added it back to version 5.9.5
            PCT_AgWaterUsedToThresh = new providerArrayProperty(_pm, eModelParam.epProvider_AgWaterUsedvsThresh, get_AgWaterUsed_ratio, eProviderAggregateMode.agSum);
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_AgWaterUsedvsThresh, "Ag Water Used Relative to Threshold", "AGUSEDPCT", PCT_AgWaterUsedToThresh, 0, 500));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epProvider_AgWaterUsedvsThresh, "Agriculture Water Used relative to the 2015 estimate", "AF a-1", "Acre-feet per annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));
            //
            SaltVerde_Annual_SurfaceDeliveries_SRP = new providerArrayProperty(_pm, eModelParam.epSaltVerde_Annual_SurfaceDeliveries_SRP, get_SRP_SurfaceDeliveries, eProviderAggregateMode.agSum);
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epSaltVerde_Annual_SurfaceDeliveries_SRP, "SRP Surface", "SRPSURF", SaltVerde_Annual_SurfaceDeliveries_SRP, 0, 1000000));
            ExtendDoc.Add(new WaterSimDescripItem(eModelParam.epSaltVerde_Annual_SurfaceDeliveries_SRP, "Annual Surface Water Delivered from (rivers?) and Reservoirs on the SRP Project", "AF a-1", "Acre-feet per Annum-1", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));

            // end DAS 06.02.16
            #endregion
            // *****

            // Base Outputs
            #region BO
            _pm.AddParameter(new ModelParameterClass(eModelParam.epColorado_River_Flow, "Colorado River Flow", "CORFLOW", geti_Colorado_River_Flow, 5000000, 30000000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPowell_Storage, "Powell Storage", "POWSTORE", geti_Powell_Storage, 2000000, 30000000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epMead_Storage, "Mead Storage", "MEDSTORE", geti_Mead_Storage, 2000000, 30000000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSaltVerde_River_Flow, "Salt-Verde River Flow", "SVRFLOW", geti_SaltVerde_River_Flow, 0, 4000000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSaltTonto_AnnualFlow, "Salt/Tonto River Flow", "STRFLOW", geti_SaltTonto_River_Flow, 0, 4000000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epVerde_AnnualFlow, "Verde River Flow", "VRFLOW", geti_Verde_River_Flow, 0, 4000000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSaltVerde_Storage, "Salt-Verde Storage", "SVRSTORE", geti_SaltVerde_Storage, 500000, 2500000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epEffluent_To_Agriculture, "Effluent To Agriculture", "EFLAG", geti_Effluent_To_Agriculture, 0, 550000 ));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSaltVerde_Spillage, "Reservoir Spillage", "SVTSPILL", geti_SVTspillage, 0, 3500000 ));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epMeadLevel, "Mead Elevation", "MEADELEV", geti_ElevationMead, 800, 1300));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epRegionalNaturalRecharge, "Regional Aquifer Natural Recharge", "REGAQRCHG", geti_Regional_Natural_Recharge, 100000, 1200000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epRegionalCAGRDRecharge, "Regional CAGRD Recharge", "REGCAGRDR", geti_Regional_CAGRD_Recharge, 0, 250000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epRegionalInflow, "Regional Natural Aquifer Inflow", "REGAQIN", geti_Regional_Inflow, 20000, 50000 ));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epRegionalAgToVadose, "Regional Ag to Vadose Recharge", "REGAGVAD", geti_Regional_Ag_To_Vadose ));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epRegionalProviderRecharge, "Total Recharge All Providers", "REGPRRCHG", geti_Regional_Provider_Recharge, 0, 1500 ));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epRegionalAgOtherPumping, "Regional Ag and Other Pumping", "REGAOPMP", geti_Regional_Ag_Other_Pumping, 0, 700000 ));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epRegionalOutflow, "Regional Aquifer Natural Outflow", "REGNAOUT", geti_Regional_Outflow, 30000, 40000));
            // _pm.AddParameter(new ModelParameterClass(eModelParam.epRegionalGWBalance, "Regional Aquifer Balance", "REGAQBAL", geti_Regional_Groundwater_Balance, 50000000, 10000000 ));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epRegionalGWBalance, "Regional Aquifer Balance", "REGAQBAL", geti_Regional_Groundwater_Balance, 0, 10000000));

            _pm.AddParameter(new ModelParameterClass(eModelParam.epPowellLevel, "Powell Elevation (Feet-msl)", "POWELELEV", geti_ElevationPowell,3370, 3750));
            // DAS Added 06.16.14
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSaltOther_Storage, "Salt-Other Storage", "SOSTORE", geti_SaltOther_Storage, 58000, 1500000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epRoosevelt_Storage, "Roosevelt Storage", "ROOSSTORE", geti_Roosevelt_Storage, 100000, 2100000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epVerde_Storage, "Verde Storage", "VRSTORE", geti_Verde_Storage, 23000, 500000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSalt_Storage, "Salt River Storage", "SRSTORE", geti_Salt_Storage, 23000, 1200000));
            #endregion
            // *****

            // Provider Outputs
            #region PO
            _pm.AddParameter(new ModelParameterClass(eModelParam.epGroundwater_Pumped_Municipal, "Muni Groundwater Pumped", "MGWPUMP", Groundwater_Pumped_Municipal, 0, 150000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epGroundwater_Balance, "Available Groundwater", "GWAVAIL", Groundwater_Balance, 0, 7000000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSaltVerde_Annual_Deliveries_SRP, "SaltVerde Annual Deliveries SRP", "SRPDELIV", SaltVerde_Annual_Deliveries_SRP, 0, 300000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSaltVerde_Class_BC_Designations, "SaltVerde Class BC Designations", "SRPBCDES", SaltVerde_Class_BC_Designations, 0, 300000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epColorado_Annual_Deliveries, "Colorado Deliveries", "COLDELIV", Colorado_Annual_Deliveries, 0, 250000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epGroundwater_Bank_Used, "Banked", "BNKUSED", Groundwater_Bank_Used, 0, 50000));
//            _pm.AddParameter(new ModelParameterClass(eModelParam.epGroundwater_Bank_Used, "Groundwater Bank Used", "BNKUSED", Groundwater_Bank_Used, 0, 50000));

            _pm.AddParameter(new ModelParameterClass(eModelParam.epGroundwater_Bank_Balance, "Groundwater Bank Balance", "BNKAVAIL", Groundwater_Bank_Balance, 0, 700000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epReclaimed_Water_Used, "Reclaimed Water Used", "RECTOT", Reclaimed_Water_Used, 0, 50000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epReclaimed_Water_To_Vadose, "To Vadose", "RECVADOS", Reclaimed_Water_To_Vadose));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epReclaimed_Water_Discharged, "Discharged ", "RECDISC", Reclaimed_Water_Discharged, 0, 50000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epReclaimed_Water_to_DirectInject, "To Direct Injection", "RECINJEC", Reclaimed_Water_to_DirectInject));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epRO_Reclaimed_Water_Used, "RO Reclaimed Water Used", "ROTOT", RO_Reclaimed_Water_Used));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epRO_Reclaimed_Water_to_DirectInject, "RO Reclaimed DirectInject", "ROINJEC", RO_Reclaimed_Water_to_DirectInject));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epEffluent_Reused, "Total Effluent Reused", "EFLCRT", Total_Effluent_Reused, 0, 250000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epEffluent_To_Vadose, "To Vadose", "EFLVADOS", Effluent_To_Vadose, 0, 25000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epEffluent_To_PowerPlant, "To PowerPlant", "EFLPP", Effluent_To_PowerPlant, 0, 85000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epEffluent_Discharged, "Discharged", "EFLDISC", Effluent_Discharged, 0, 60000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epDemand_Deficit, "Demand Deficit", "DEMDEF", Demand_Deficit));
            //_pm.AddParameter(new ModelParameterClass(eModelParam.epGPCD_Used, "GPCD Used", "GPCDUSED", GPCD_Used, 0, 700));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epGPCD_Used, "GPCD Used", "GPCDUSED", GPCD_Used, 0, 400));
            // Population
            _pm.AddParameter(new ModelParameterClass(eModelParam.epPopulation_Used, "Population", "POPUSED", Population_Used,PopGroup, 1000, 4000000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epTotal_Demand, "Total Demand", "TOTDEM", Total_Demand, DemandGroup, 0, 500000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epOnProjectDemand, "On Project Demand (AF)", "ONDEM", Demand_On_Project, 0, 350000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epOffProjectDemand, "Off Project Demand (AF)", "OFFDEM", Demand_Off_Project, 0, 350000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epOnProjectPopulation, "On-project population (ppl)", "POPONPRJ", Population_On_Project, 0, 2000000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epOtherPopulation, "Other population (ppl)", "POPOTHER", Population_Other, 0, 2000000));
            //
            _pm.AddParameter(new ModelParameterClass(eModelParam.epAnnualIncidental, "Incidental Recharge Credit", "INCCREDIT", Incidental_Water_Credit, 0, 75000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epVadoseToAquifer, "Total Vadose Recharge-VtoAq", "VADRCHTOT", Total_Vadose_To_Aquifer, 0, 150000));

            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epTWWTPCreated_AF, "Total TWWTP Water Created", "TWWTP", Total_WWTP_Effluent, 0, 300000));
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epGPCDraw, "GPCD raw", "GPCDRAW", GPCD_raw, 0, 600));
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epTotalReclaimedCreated_AF, "Reclaimed Water Created", "TOTREC", Reclaimed_Water_Created, 0, 100000));

            // DAS Added 02.15.14
            _pm.AddParameter(new ModelParameterClass(eModelParam.epWaterAugmentationUsed, "Augmented", "WATAUGUSED", WaterAugmentationUsed, 0, 100000));
            //
            _pm.AddParameter(new ModelParameterClass(eModelParam.epSaltVerde_Annual_GW_Deliveries_SRP, "SRP Pumping", "SRPGW", SaltVerde_Annual_GW_Deliveries_SRP, 0, 400000));
            //
            // DAS 07.29.15,and 07.30.15
            _pm.AddParameter(new ModelParameterClass(eModelParam.epColorado_Max_Deliveries, "Colorado Maximum Deliveries", "CAPMAXP", Colorado_Max_Deliveries, 0, 300000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epColorado_Unused_P4, "Colorado Unused Priority Four", "CAPUNUSED4", Colorado_Unused_PriorityFour, 0, 200000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epColorado_Unused_P5, "Colorado Unused Priority Five", "CAPUNUSED5", Colorado_Unused_PriorityFive, 0, 200000));
            // DAS 07.31.15 
            _pm.AddParameter(new ModelParameterClass(eModelParam.epCAP_LossPotential_Priority4, "CAP Loss Potential Priority Four", "CAPLOSSP4", CAP_LossPotential_PriorityFour, 0, 300000));
            _pm.AddParameter(new ModelParameterClass(eModelParam.epCAP_LossPotential_Priority5, "CAP Loss Potential Priority Five", "CAPLOSSP5", CAP_LossPotential_PriorityFive, 0, 300000));
            // DAS 08.04.15 
            //_pm.AddParameter(new ModelParameterClass(eModelParam.epCAP_DemandNotMet_Priority4, "CAP Demand Not Met Priority Four", "CAPNOTMET4", CAP_DemandNotMet_PriorityFour, 0, 300000));
            //_pm.AddParameter(new ModelParameterClass(eModelParam.epCAP_DemandNotMet_Priority5, "CAP Demand Not Met Priority Five", "CAPNOTMET5", CAP_DemandNotMet_PriorityFive, 0, 300000));
            // 07.27.16 DAS
            _pm.AddParameter(new ModelParameterClass(eModelParam.epProvider_ShowersMinPCD, "Shower Minutes per capita per day", "SHOWERS", Showers_MinPCD, 0, 30));


            #endregion
            // *****
            #region Initialization
            // Make a Call to initialize the Groundwater Parameters if implementation exists (WaterSimDCDC_GWPaameters ver N.cs)
            initialize_GWModelParameters();
            // Make Call to initialize Derived model parameters, if implmentation exists 
            initialize_Derived_ModelParameters();
            // Make Call to initialize Sustainable model parameters, if implementation exists 
            initialize_Sustainable_ModelParameters();
            // Make Call to initialize user model parameters , implemented in WaterSimDCDC_User_Mods.cs
            initialize_Other_ModelParameters();
            // Call for Web Parameters
            initialize_WebInterface_DerivedParameters();
            //// Add extended documentation
            //initialize_ExtendedDocumentation();
        }
        //--------------------------------------------------------------     
        private void InitDefaultVariables()
        {
            //
            _ws.Startyear = _StartYear = _ws.SimulationStart = util.WaterSimDCDC_Default_Simulation_StartYear;
            _ws.Endyear = _EndYear = _ws.SimulationEnd = util.WaterSimDCDC_Default_Simulation_EndYear;
            // QUAY BEGIN EDIT 3 10 14
            Simulation_End_Year = _ws.Endyear;
            // QUAY end EDIT 3 10 14
            // Call API.PARMS to Set up Default array
            API.parms(_ws);

            // DAS 01.09.15
            API_Default_Status = 1;
            // das 02.01.19
            // das 02.21.19 changed to commented out
            //_ws.parmIwaniecScenariosYN = false;
            //_ws.parmWaterSim5YN = true;
            ////
            //TEMP FIX  CHANGE IN FUTURE IF FIXED
            Colorado_User_Adjustment_StartYear= util.WaterSimDCDC_Default_Colorado_User_Adjustment_StartYear;
            SaltVerde_Historical_Data_Source = util.WaterSimDCDC_Default_SaltVerde_Historical_Data;
            SaltVerde_User_Adjustment_Start_Year = util.WaterSimDCDC_Default_SaltVerde_User_Adjustment_Start_Year;
            Provider_Demand_Option = util.WaterSimDCDC_Provider_Demand_Option;
            Assured_Water_Supply_Annual_Groundwater_Pumping_Limit = 1;
            // GPCDFIX
            GPCDBuffer = GPCDDefaults;
            initialize_Provider_Default_ModelParameters();
            Web_Parms_setDefaultValues();
            // 06.02.16 DAS
                for (int p = 0; p < ProviderClass.NumberOfProviders; p++)
                {
                    this.COdeltaBurdenToAZ = false;
                this.WaterAugmentation[p] = 0;
                }     
            // end 06.02.16 DAS
            // start 06.09.17 DAS
            // zero out created memory locations
                for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { WaterAugmentationMemory[i] = 0; }

            // End 06.09.17
            // DAS 01.01.17,10.27.17 
            // set parameter values specific to the Scenarios Project
            // ==============================================================
            if (WaterSimManager.FortranDLL == "WaterSimDCDC_model_6.dll")
            {               
                ResetScenarioData(_ws);
            }
            // ==============================================================

        }
        internal void GPCDFIX()
        {
            // OK This is not exactly elegant but is in place to handle odd nature of Use_GPCD and GPCD_Used.
            // There is only on parameter in the model for provider GPCD.  It can be set to a GPCD for Demand Option 4, for other options
            //  it is calculated and can not be set.  The Get always reports the GPCD_Used in the last run, which for Option 4 is what it was set to, but for all other options 
            // is a calculated value changing each year.
            // To mimic all other values two parameters were created, one input one ouput, that use the same model function, the set for the input, and the get for output.
            // The get for the input paramter uses two dummy arrays, one for option 4 which is changed on a set, and one for all other options which returns 
            // the intitial default value used by the model in calculating GPCD.  These defaults can only be changed in the provider input file.
            // A special exception is added if USE_GPCD is set when option 4 is not selected since that does not work
            // This sets up these arrays.  In order to get the default values, the model has to be run for one year.  
            // fetch the defaults
            int OldStart = Simulation_Start_Year;
            int OldEnd = Simulation_End_Year;
            Simulation_Initialize();
            Simulation_Start_Year = 2000;
            Simulation_End_Year = 2010;
            // DAS 04.15.19 added these lines as default
            // ===========================
            _ws.parmIwaniecScenariosYN = false;
            _ws.parmWaterSim5YN = true;
            // ===========================
            Simulation_NextYear();
            //// OK now fetch the GCPD defaults
            GPCDBuffer = GPCDDefaults = _ws.ProviderGPCD;
            Simulation_Stop();
            // reset the defaults
            Simulation_Start_Year = OldStart;
            Simulation_End_Year = OldEnd;
            //
           // if (CreateModelOutputFiles) { _ws.CloseFiles(); }
            // OK will use these inthe provider property routines
            // NOTE, this is one of the exceptions where a get call and a run control is made into the model outside the ModelParamter interface.
       
        }
        #endregion
        ////------------------------------------------------------------------------
       
       
        /*************************************************************
        * Simulation Control Delegates, Fields, Properties and Methods
        * 
        * **************************************************************/
        #region Simulation_Control
       


        //----------------------------------------------------------------------------------------------------
        /// <summary>
        /// Must be called to setup a Simulation . Simulation can be run in two ways, 
        ///      calling SimulationNextYear() for each year to be run, call SimulationAllYears() which runs all the years
        ///   All simulations should be stopped with StopSimulation(), which will make sure all files are closed
        ///  Will reset SimulationLock to false;
        /// </summary>
        public override void Simulation_Initialize()
        /* -------------------------------------------
        * Initializes a Simulation.   
        * All simulations should be stopped with StopSimulation(), whihc will make sure all files are closed
        * --------------------------------------------*/
        {
            base.Simulation_Initialize();

            // call model reset
            _ws.ResetAll();
            // Initiallize defaults
            InitDefaultVariables();  // Sets Variables to Default Values
        }
        //----------------------------------------------------------------------------------------------------
        bool _inSimulationNextYear = false;
        /// <summary>
        /// Runs the next year in a series of years in a simulation, no pre or post process is evoked 
        /// </summary>
        /// <returns>The year of the simulation run</returns>
               
        public  override int Simulation_NextYear()
        /* --------------------------------------------------------
        * Returns the year run, if has already run last year, returns 0;
        * Calls close files afer running last year
        ----------------------------------------------------------- */
        {
           // bool check = _ws.parmIwaniecScenariosYN;
            int runyear = base.Simulation_NextYear();
            _ws.StartSimulation = false;
            return runyear;
        }
        //----------------------------------------------------------------

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Executes the model year operation. </summary>
        ///
        /// <remarks>   Mcquay, 1/20/2016. </remarks>
        ///
        /// <param name="year"> The year. </param>
        ///
        /// <returns>   An int. </returns>
        ///-------------------------------------------------------------------------------------------------

        protected override int RunModelYear(int year)
        {
            // call model for one year
            _ws.RunOneYear();
            // clean up
            _ws.GroundWater(year);

            return 0;
        }
        /// <summary>   Executes the year operation. </summary>
        /// <remarks>This has gone back and forth from protected to internal back again, might change in future</remarks>
        /// <param name="year"> The year. </param>
        //internal virtual void runYear(int year)
       
        //protected override bool runYear(int year)
        //// Iternal routine Runs one more year, call groundwater
        //// Fast as possible, no error checking, no reentry block
        //{

        //    bool Valid = _ws.get_ValidModelRun;

        //    // call model for one year
        //    _ws.RunOneYear();
        //    // clean up
        //    _ws.GroundWater(year);
        //    return Valid;
        //}
        
        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Simulation cleanup. </summary>
        /// <remarks> Override to finish out run of model if stopped before end year of simulation</remarks>
        ///-------------------------------------------------------------------------------------------------

        protected override void Simulation_Cleanup()
        {
            while (_CurrentYear < Sim_EndYear)
            {
                _ws.RunOneYear();
                _CurrentYear++;
            }
        }
        #endregion
        //----------------------------------------------------------------------------------
        // TESTING
        internal bool testParmeters(ref string Results, bool debug)
        {
            return _pm.testModelParameters(ref Results);//,debug);
        }
        //----------------------------------------------------------------------------------
        internal bool testParmeters()
        {
            string junk = " ";
            return _pm.testModelParameters(ref junk);//,false);
        }
       
        //
        //======================================================
        // Model Parameters
        // =====================================================
        //
        #region Model Parameters

        // OutPut Properties for Model Variables  OutPut after model year run
       

        //************************************
        // BASE OUTPUT PARAMETERS
        // ************************************

        #region Base output Parameters

        // Regional  Water Supply Data =========================
        //---------------------------------------
        //get_ColoradoRiverFlow
       
        /// <summary>   Gets the colorado river flow. </summary>
        ///<remarks>The total annual flow in the Colorado River above Lake Powell. Units AF</remarks>
        /// <value> The colorado river flow. </value>
       
        public int Colorado_River_Flow
        { get { return _ws.get_ColoradoRiverFlow; } }
       
        //---------------------------------------
        //get_PowellStorage
       
        /// <summary>   Gets the powell storage. </summary>
        /// <value> The powell storage. </value>
        ///<remarks>The total water storage in Lake Powell. Units maf</remarks>
       
        public int Powell_Storage
        { get { return _ws.get_PowellStorage; } }
       
        //---------------------------------------
        //get_MeadStorage
       
        /// <summary>   Gets the mead storage. </summary>
        /// <value> The mead storage. </value>
        ///<remarks>The total annual water storage in Lake Mead Units maf</remarks>
       
        public int Mead_Storage
        { get { return _ws.get_MeadStorage; } }
       
        //---------------------------------------
        //get_SaltVerdeRiverFlow
       
        /// <summary>   Gets the salt and verde river flow. </summary>
        /// <value> The salt verde river flow. </value>
        ///<remarks>The total annual flow of the Salt and Verde Rivers. Units AF</remarks>
       
        public int SaltVerde_River_Flow
        { get { return _ws.get_SaltVerdeRiverFlow; } }

        /// <summary>   Gets the Salt/Tonto river flow. </summary>
        /// <value> The salt tonto river flow. </value>
        ///<remarks>The total annual flow of the Salt/Tonto system. Units AF</remarks>

        public int SaltTonto_River_Flow
        { get { return _ws.get_SaltTontoAnnualFlow; } }

        /// <summary>   Gets the Verde river flow. </summary>
        /// <value> The Verde river flow. </value>
        ///<remarks>The total annual flow of the Verde River. Units AF</remarks>

        public int Verde_River_Flow
        { get { return _ws.get_VerdeAnnualFlow; } }

        //---------------------------------------
        // DAS 6/16/14
        /// <summary>
        /// Gets the Amount of Water in Lake Roosevelt
        /// </summary>
        public int Roosevelt_Storage
        { get { return _ws.get_RooseveltStorage; } }

        /// <summary>
        ///   Gets the amount of water in all other storage on the Salt side
        /// </summary>
        public int SaltOther_Storage
        { get { return _ws.get_OtherSaltStorage; } }

        /// <summary>
        ///  Gets the amount of water in storage on the Verde side
        /// </summary>
        public int Verde_Storage
        { get { return _ws.get_VerdeStorage; } }


        public int Salt_Storage
        { get { return _ws.get_RooseveltStorage + _ws.get_OtherSaltStorage; } }

        /// <summary>   Gets the salt verde storage. </summary>
        /// <value> The salt verde storage. </value>
        ///<remarks>The total annual storage in the Salt River Project reservoirs. Units maf</remarks>
       
        public int SaltVerde_Storage
        { get { return _ws.get_SaltVerdeStorage; } }
       
        //---------------------------------------
        //get_EffluentToAgriculture
       
        /// <summary>   Gets the effluent to agriculture. </summary>
        /// <value> Units AF. </value>
        ///<remarks>The total amount of wastewater effluent delivered to agriculural users. Units AF</remarks>
       
        public int Effluent_To_Agriculture
        { get { return _ws.get_EffluentToAgriculture; } }
       
       
        //-------------------------------------------------------
       
        ///-------------------------------------------------------------------------------------------------
        /// <summary> Gets the annual spillage over Granite Reef. </summary>
        /// <value> The svt spillage. Units Af</value>
        ///<remarks>The total amount of water that is spilled from Granite Reef in the year. Units AF</remarks>
        ///-------------------------------------------------------------------------------------------------
       
        public int SVT_Spillage
        { get { return _ws.get_SaltVerdeSpillage; } }
       
        //----------------------------------------------------------------------------
       
        ///-------------------------------------------------------------------------------------------------
        /// <summary> Gets the elevation ofLake Mead. </summary>
        /// <value> The elevation of mead in feet. </value>
        ///<remarks> The Elevation of Lake Mead at the end of the year.  Units Feet</remarks>
        ///-------------------------------------------------------------------------------------------------
       
        public int Elevation_of_Mead
        { get { return _ws.get_MeadElevation; } }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Gets the elevation ofLake Mead. </summary>
        /// <value> The elevation of mead in feet. </value>
        ///<remarks> The Elevation of Lake Mead at the end of the year.  Units Feet</remarks>
        ///-------------------------------------------------------------------------------------------------
        public int Elevation_of_Powell
        { get { return _ws.get_PowellElevation; } }
              
        ///-------------------------------------------------------------------------------------------------
        /// <summary>    Gets the regional natural recharge. </summary>
        ///
        /// <value>  The regional natural recharge. Units AF </value>
        ///-------------------------------------------------------------------------------------------------

        public int Regional_Natural_Recharge
        { get { return _ws.get_RegionalNaturalRecharge; } }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>    Gets the regional cagrd recharge. </summary>
        ///
        /// <value>  The regional cagrd recharge. Units AF </value>
        ///-------------------------------------------------------------------------------------------------

        public int Regional_CAGRD_Recharge
        { get { return _ws.CAGRD; } }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>    Gets the regional inflow. </summary>
        ///
        /// <value>  The regional inflow. Units AF </value>
        ///-------------------------------------------------------------------------------------------------

        public int Regional_Inflow
        { get { return _ws.get_RegionalInflow; } }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>    Gets the regional ag to vadose. </summary>
        ///
        /// <value>  The regional ag to vadose flux. Units AF </value>
        ///-------------------------------------------------------------------------------------------------

        public int Regional_Ag_To_Vadose
        { get { return _ws.get_RegionalAgToVadoseFlux; } }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>    Gets the regional provider recharge. </summary>
        ///
        /// <value>  The total amount of recharge from all providers.  Units AF</value>
        ///-------------------------------------------------------------------------------------------------

        public int Regional_Provider_Recharge
        { get { return _ws.get_RegionalProviderRecharge; } }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>    Gets the regional ag other pumping. </summary>
        ///
        /// <value>  The regional pumping from agriculture and other (non municiple ) uses. . Units AF </value>
        ///-------------------------------------------------------------------------------------------------

        public int Regional_Ag_Other_Pumping
        { get { return _ws.get_RegionalAgOtherPumping; } }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>    Gets the regional outflow. </summary>
        ///
        /// <value>  The regional natural aquifer outflow. . Units AF</value>
        ///-------------------------------------------------------------------------------------------------

        public int Regional_Outflow
        { get { return _ws.get_RegionalOutflow; } }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>    Gets the regional groundwater balance. </summary>
        ///
        /// <value>  The balance of the regional groundwater aquifer (estimate, assumes all recharge is in aquifer). Units AF </value>
        ///-------------------------------------------------------------------------------------------------

        public int Regional_Groundwater_Balance
        { get { return _ws.get_RegionalGroundWaterBalance; } }

        #endregion

        // 
        // =========================================================
        // Provider Outputs
        //========================================================== 
        // 

        #region Provider Outputs

 

        //---------------------------------------
        /// <summary>
        /// Gets the original Raw GPCD curves
        /// </summary>
        public providerArrayProperty GPCD_raw;
        internal int[] get_GPCDraw() { return _ws.get_ProviderGPCDraw; }

        //---------------------------------------
        private int[] get_TWWTP_Created()
        { return _ws.get_TotalTWWTP; }

        /// <summary> The reclaimed water created </summary>
        ///<remarks>The total annual amount of reclaimed water created, used and discharged. Units AF</remarks>

        public providerArrayProperty Total_WWTP_Effluent;

       
        //---------------------------------------
        // get_ProviderGWPumpedMunicipal
        private int[] get_Groundwater_Pumped_Municipal()
        { return _ws.get_ProviderGWPumpedMunicipal; }  
       
        /// <summary> The groundwater pumped municipal </summary>
        ///<remarks>The total amount of annual groundwater pumped. Units AF</remarks>
       
        public providerArrayProperty Groundwater_Pumped_Municipal;
       
        //---------------------------------------
        //ProviderGroundwater
        private int[] get_Groundwater_Balance()
        //{ return _ws.ModelGroundwater; }  Changed based on new credit model
        { return _ws.get_WaterCreditTotals; }
        /// <summary> The groundwater balance </summary>
        ///<remarks>The total groundwater supply available at end of year. Units AF.  
        ///         A call to this method before the first year has been run returns 0 values</remarks>
       
        public providerArrayProperty Groundwater_Balance;
               
        //---------------------------------------
        //get_SVTAnnualDeliveriesSRP
        private int[] get_SaltVerde_Annual_Deliveries_SRP()
        {
            return _ws.get_SVTAnnualDeliveriesSRP;
        }
             
       
        /// <summary> The salt verde annual deliveries srp </summary>
        ///<remarks>The total annual surface water and pumped groundwater delivered by SRP. Units AF</remarks>
        /// <seealso cref="Groundwater_Bank_Used"/>
        /// 
        public providerArrayProperty SaltVerde_Annual_Deliveries_SRP;
        //---------------------------------------
        // SRP pumped
        private int[] get_SaltVerde_Annual_GWDeliveries_SRP()
        {
            return  _ws.get_ClassBCpumpedSRP;}



        /// <summary> The salt verde annual deliveries srp </summary>
        ///<remarks>The total annual surface water and pumped groundwater delivered by SRP. Units AF</remarks>
        /// <seealso cref="Groundwater_Bank_Used"/>
        /// 
        public providerArrayProperty SaltVerde_Annual_GW_Deliveries_SRP;






        //---------------------------------------
        //get_SaltVerdeClassBCDesignations
        private int[] get_SaltVerde_Class_BC_Designations()
        { return _ws.get_SaltVerdeClassBCDesignationsUsed; }
        // Quay Changed to get_SaltVerdeClassBCDesignationsUsed from get_SaltVerdeClassBCDesignations 3/6/13
       
        /// <summary> The salt verde class bc designations </summary>
        ///<remarks>The total annual B / C designated surface water delivered by SRP. Units AF</remarks>
        
        public providerArrayProperty SaltVerde_Class_BC_Designations;




        //---------------------------------------
        //get_ColoradoAnnualDeliveries
        private int[] get_Colorado_Annual_Deliveries()
        { return _ws.get_ColoradoAnnualDeliveries; }
       
        /// <summary> The colorado annual deliveries </summary>
        ///<remarks>The total annual surface water deliveries by CAP, does not included banked water. Units AF</remarks>
        /// <seealso cref="Groundwater_Bank_Used"/>
        public providerArrayProperty Colorado_Annual_Deliveries;
        //---------------------------------------
 
        private int[] get_Colorado_Max_Deliveries()
        { return _ws.get_ColoradoMaxDeliveries; }

        /// <summary> The colorado annual deliveries </summary>
        ///<remarks>The total annual surface water deliveries by CAP, does not included banked water. Units AF</remarks>
        /// <seealso cref="Groundwater_Bank_Used"/>
        public providerArrayProperty Colorado_Max_Deliveries;
        //---------------------------------------
        private int[] get_Colorado_Unused_PriorityFour()
        { return _ws.get_ColoradoUnusedPriority4 ; }

        /// <summary>  </summary>
        ///<remarks> Units AF</remarks>
        /// <seealso cref="Groundwater_Bank_Used"/>
        public providerArrayProperty Colorado_Unused_PriorityFour;
        //---------------------------------------
        //---------------------------------------
        private int[] get_Colorado_Unused_PriorityFive()
        { return _ws.get_ColoradoUnusedPriority5; }

        /// <summary>  </summary>
        ///<remarks>. Units AF</remarks>
        /// <seealso cref="Groundwater_Bank_Used"/>
        public providerArrayProperty Colorado_Unused_PriorityFive;
        //---------------------------------------

        // 08.03.15 das
        //---------------------------------------
        private int[] get_CAPshortages_PriorityFour()
        { return _ws.get_CAPlossPotentialPriority4; }

        /// <summary>  </summary>
        ///<remarks> Units AF</remarks>
        /// <seealso cref="Groundwater_Bank_Used"/>
        public providerArrayProperty CAP_LossPotential_PriorityFour;
        //---------------------------------------
        //---------------------------------------
        private int[] get_CAPshortages_PriorityFive()
        { return _ws.get_CAPlossPotentialPriority5; }

        /// <summary>  </summary>
        ///<remarks> Units AF</remarks>
        /// <seealso cref="Groundwater_Bank_Used"/>
        public providerArrayProperty CAP_LossPotential_PriorityFive;
        //---------------------------------------
        // 08.04.15 das

        //private int[] get_CAPdemandNotMet_PriorityFour()
        //{ return _ws.get_CAPdemandNotMetPriority4; }

        ///// <summary>  </summary>
        /////<remarks> Units AF</remarks>
        ///// <seealso cref="Groundwater_Bank_Used"/>
        //public providerArrayProperty CAP_DemandNotMet_PriorityFour;
        ////---------------------------------------
        ////---------------------------------------
        //private int[] get_CAPdemandNotMet_PriorityFive()
        //{ return _ws.get_CAPdemandNotMetPriority5; }

        ///// <summary>  </summary>
        /////<remarks> Units AF</remarks>
        ///// <seealso cref="Groundwater_Bank_Used"/>
        //public providerArrayProperty CAP_DemandNotMet_PriorityFive;

        /// <summary>
        /// Showers in Minutes per Capita per Day
        /// for each water provider
        /// 07.27.16 DAS
        /// </summary>
        /// <returns></returns>
        private int[] get_Showers()
        { return _ws.ShowersBathsMinPCD; }

        public providerArrayProperty Showers_MinPCD;




        //---------------------------------------
        //get_GroundwaterBankUsed
        private int[] get_Groundwater_Bank_Used()
        { return _ws.get_GroundwaterBankUsed; }
       
        /// <summary> The groundwater bank used </summary>
        ///<remarks>The total annual amount of water delivered from water banking facilities.  These ground water facilities are assumed to be physically  outside the providers groundwater assets and would be delivered from these remote facilities to the provider.  Some of this may be delivered through the CAP or SRP canals but is not icnluded in SRP or CAP totals. Units AF</remarks>
        /// <seealso cref="SaltVerde_Annual_Deliveries_SRP"/>
        /// <seealso cref="Colorado_Annual_Deliveries"/>
        public providerArrayProperty Groundwater_Bank_Used;
       
        //---------------------------------------
        //get_GroundwaterBankBalance
        private int[] get_Groundwater_Bank_Balance()
        { return _ws.get_GroundwaterBankBalance; }
       
        /// <summary> The groundwater bank balance </summary>
        ///<remarks>The total banked water supply available at end of year. Units AF</remarks>
       
        public providerArrayProperty Groundwater_Bank_Balance;
       
        //---------------------------------------
        //get_ReclaimedWaterCreated
        private int[] get_Reclaimed_Water_Created()
        { return _ws.get_ReclaimedWaterCreated; }
       
                
        /// <summary> The reclaimed water created </summary>
        ///<remarks>The total annual amount of reclaimed water created, used and discharged. Units AF</remarks>
       
        public providerArrayProperty Reclaimed_Water_Created;
       
        //---------------------------------------
        //get_ReclaimedWaterUsed
        private int[] get_Reclaimed_Water_Used()
        { return _ws.get_ReclaimedWaterUsed; }
       
        /// <summary> The reclaimed water used </summary>
        ///<remarks>The total annual amount of reclaimed water used. Units AF</remarks>
        ///<see cref="Reclaimed_Water_Created"/>
       
        public providerArrayProperty Reclaimed_Water_Used;
       
        //---------------------------------------
        //get_ReclaimedWaterToVadose
        private int[] get_Reclaimed_Water_To_Vadose()
        { return _ws.get_ReclaimedWaterToVadose; }
       
        /// <summary> The reclaimed water to vadose </summary>
        ///<remarks>The annual amount of reclimed water used for vadose zone recharge. Units AF</remarks>
       
        public providerArrayProperty Reclaimed_Water_To_Vadose;
       
        //---------------------------------------
        //get_ReclaimedWaterDischarged
        private int[] get_Reclaimed_Water_Discharged()
        { return _ws.get_ReclaimedWaterDischarged; }
       
        /// <summary> The reclaimed water discharged </summary>
        public providerArrayProperty Reclaimed_Water_Discharged;
       
        //---------------------------------------
        //get_ReclaimedWaterDirectInject
        private int[] get_Reclaimed_Water_to_DirectInject()
        { return _ws.get_ReclaimedWaterDirectInject; }
       
        /// <summary> The reclaimed water to direct inject </summary>
        public providerArrayProperty Reclaimed_Water_to_DirectInject;
       
        //---------------------------------------
        //get_ROreclaimedWaterCreated
        private int[] get_RO_Reclaimed_Water_Created()
        { return _ws.get_ROreclaimedWaterCreated; }
       
        /// <summary> The ro reclaimed water created </summary>
        public providerArrayProperty RO_Reclaimed_Water_Created;
       
        //---------------------------------------
        //get_ROreclaimedWaterUsed
        private int[] get_RO_Reclaimed_Water_Used()
        { return _ws.get_ROreclaimedWaterUsed; }
       
        /// <summary> The ro reclaimed water used </summary>
        public providerArrayProperty RO_Reclaimed_Water_Used;
       
        //---------------------------------------
        //get_ROreclaimedWaterDirectInject
        private int[] get_RO_Reclaimed_Water_to_DirectInject()
        { return _ws.get_ROreclaimedWaterDirectInject; }
       
        /// <summary> The ro reclaimed water to direct inject </summary>
        public providerArrayProperty RO_Reclaimed_Water_to_DirectInject;
       
        //---------------------------------------
        //get_EffluentCreated
        private int[] get_Total_Effluent_Reused()
        { return _ws.get_TotalEffluentReused; }
       
        /// <summary> The effluent created </summary>
        ///<remarks>The total annual amount of wastewater effluent produced. Units </remarks>
       
        public providerArrayProperty Total_Effluent_Reused;
       
        //---------------------------------------
        //get_EffluentToVadose
        private int[] get_Effluent_To_Vadose()
        { return _ws.get_EffluentToVadose; }
       
        /// <summary> The effluent to vadose </summary>
        ///<remarks>The annual amount of reclaimed water used for vadose zone recharge. Units AF</remarks>
       
        public providerArrayProperty Effluent_To_Vadose;
       
        //---------------------------------------
        //get_EffluentToPowerPlant
        private int[] get_Effluent_To_PowerPlant()
        { return _ws.get_EffluentToPowerPlant; }
       
        /// <summary> The effluent to power plant </summary>
        ///<remarks>The annual amount of effluent delivered to power plants. Units AF</remarks>
       
        public providerArrayProperty Effluent_To_PowerPlant;
       
        //---------------------------------------
        //get_EffluentDischarged
        private int[] get_Effluent_Discharged()
        { return _ws.get_EffluentDischarged; }
       
        /// <summary> The effluent discharged </summary>
        ///<remarks>The annual amount of wastewater effluent discharged to a water course (envirionment). Units AF</remarks>
       
        public providerArrayProperty Effluent_Discharged;
       
 
   

        //  get_WaterCreditIncidental
        internal int[] get_Incidental_Water_Credit()
        { return _ws.get_WaterCreditIncidental; }

        /// <summary> The amount of annual incendental recharge credit by provider</summary>
        ///<remarks>The annual amount of incidental water use that is credited annually as recharge to the aquifer. Units AF</remarks>

        public providerArrayProperty Incidental_Water_Credit;

        //  get_VadoseToAquiferFlux
        internal int[] get_Total_Vadose_To_Aquifer_Flux()
        { return _ws.get_VadoseToAquiferFlux; }

        /// <summary> The amount of annual incendental recharge credit by provider</summary>
        ///<remarks>The annual amount of incidental water use that is credited annually as recharge to the aquifer. Units AF</remarks>

        public providerArrayProperty Total_Vadose_To_Aquifer;
               

        // On-Project Population - Provider level:  internal int[] get_PopulationOnProject
        private int[] get_Population_OnProject()
        { return _ws.get_PopulationOnProject; }
        /// <summary> The population project. </summary>
        public providerArrayProperty Population_On_Project;
       
        // On-Project Population - Provider level:  internal int[] get_PopulationOnProject
        private int[] get_Population_Other()
        { return _ws.get_PopulationOther; }
        /// <summary> The population other. </summary>
        public providerArrayProperty Population_Other;
       
        //---
        // ------------------------------------
        //get_DemandDeficit
        private int[] get_Demand_Deficit()
        { return _ws.get_DemandDeficit; }
       
        /// <summary> The demand deficit </summary>
        ///<remarks>The annual difference between demand and supply (demand - supply), 0 if supply is larger than demand. Units AF.  This is a good candidate as a indicator parameter.  
        ///		   There are a number of management actions that can be taken, and reflected in the model parameters to manage water supplies and demand.  
        ///		   A community's demand exceeding supply is an indicator of either a non-sustainable water resources budget or inadequate management of available water supplies and demand.    
        ///		   Both indicate an unresolved  water resource issue for the community.  Currently there is no feedback in the model to reduce growth (thus demand growth) for a community when a demand deficit occurs, nor a feedback to allocate this growth to other providers.</remarks>
          
        public providerArrayProperty Demand_Deficit;
       
        //---------------------------------------
        //ProviderGPCD
        private int[] get_GPCD_Used()
        { return _ws.ProviderGPCD; }
        // Sampson 02.07.2019 this will NOT stay as formulated here.....
         // { return _ws.get_ProviderGPCDraw; }

    /// <summary> The gpcd used </summary>
    ///<remarks>The GPCD used to estimate demand for the completed simulation year. When Provider_Demand_Option is 1,2, or 3, this is the calculated GPCD used to estimate demand.  
    ///		   if the Provider_Demand_Option =4, this is the GPCD specified by Use_GPCD parameter. Units: gpcd-gallons per capita per daya of water use</remarks>
    /// <seealso cref="Provider_Demand_Option"/>
    /// <seealso cref="Use_GPCD"/>
    public providerArrayProperty GPCD_Used;
      
        //---------------------------------------
        //TotalDemands
        private int[] get_Total_Demand()
        { return _ws.get_WaterDemand; }  //  { return _ws.TotalDemands; }
       
        /// <summary> The total demand </summary>
        ///<remarks>The total annual demand from all water customers. Units AF.  This calculated based on provider demand</remarks>
       
        public providerArrayProperty Total_Demand;
       
        //---------------------------------------
        //Population 
        private int[] get_Population_Used()
        { return _ws.get_Populations; }
       
        /// <summary> The population used </summary>
        ///<remarks>The population used to estimate demand for the completed simulation year. Units people</remarks>
       
        public providerArrayProperty Population_Used;
       
        //----------------------------------
        // Off Project Demand
        private int[] get_Demand_Off_Project()
        { return _ws.get_WaterDemandOther; }
       
        /// <summary> The demand off project. </summary>
        public providerArrayProperty Demand_Off_Project;
       
        //----------------------------------
        // Off Project Demand
        private int[] get_Demand_On_Project()
        { return _ws.get_WaterDemandOnProject; }
        /// <summary> The demand on project. </summary>
        public providerArrayProperty Demand_On_Project;

        #endregion

        /*****************************************************************
        * Base Input Parameters
        * 
        * ****************************************************************/

        #region Base Input Parameters
        //===============================================================
       
        //---------------------------------------
        //      Start year of simulation
        // Cannot be set while simulation in progress
        // Using shadow value _Simulation_Start_Year;  no get in WaterSimU
        /// <summary>
        /// Simulation_Start_Year
        /// </summary>
        /// <value>The first year of the Simulation.</value>
        /// <remarks> The first year of the Simulation. Range = 2000 to 2006  Cannot be set after Simulation starts.</remarks>
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
               
        public override int Simulation_Start_Year
        {
            set
            {
                if ((!_inRun)&(!FModelLocked))                
                {
                    base.Simulation_Start_Year = value;  
                //    _pm.CheckBaseValueRange(eModelParam.epSimulation_Start_Year, value);
                    _ws.SimulationStart = value;
                 //   _StartYear = value;
                    //_Simulation_Start_Year = value;
                }
            }
//            get { return Sim_StartYear; }// Sim_Simulation_Start_Year; }    // 
        }
       
        //---------------------------------------
        //End year of simulation	SimulationEnd
        // Cannot be set while simulation in progress
        // Using shadow value _Simulation_End_Year;  no get in WaterSimU
       
        /// <summary>   Gets or sets the simulation end year. </summary>
        /// <value> The simulation end year. </value>
        ///<remarks> The last year of the Simulation. Range = 2012 to 2085  Cannot be set after Simulation starts.</remarks>
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
               
        public override int Simulation_End_Year
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    base.Simulation_End_Year = value;
                    //    _pm.CheckBaseValueRange(eModelParam.epSimulation_End_Year, value);
                    //_ws.SimulationEnd = value + 1;  // Added this to force model to run last year;
                    _ws.SimulationEnd = value +1 ;  // Removed to correct outputs 08.26.16 das;
              
                    //_Simulation_End_Year = value;
                  //  _EndYear = value;
                    
                }
                // ELSE do we throw an exception? No Just document that it is ignored
            }
//            get { return Sim_EndYear;}// _Simulation_End_Year; }
        }
       
        //---------------------------------------
        //Index Year	ColoradoHistoricalExtractionStartYear
        // Cannot be set while simulation in progress
        /// <summary>   Gets or sets the colorado historical extraction start year. </summary>
        /// <value> The colorado historical extraction start year. </value>
        ///<remarks> The first year of the Colorado River flow record that will be used to create a 25 year trace to simulate river flow conditions. Special Range Check Applies See ColoradoYearRangeCheck  Cannot be set after Simulation starts.
        ///	Valid ranges are defined as follows <code>
        ///	       internal RiverRange ColoradoPaleo = new RiverRange(762, 1982);
        ///        internal RiverRange ColoradoBureau = new RiverRange(1906, 1982);</code>		</remarks>
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        /// <seealso cref="RiverRange"/>
        /// <seealso cref="Colorado_Historical_Data_Source"/>
                
        public int Colorado_Historical_Extraction_Start_Year
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epColorado_Historical_Extraction_Start_Year, value);
                    _ws.ColoradoHistoricalExtractionStartYear = value;
                }
                // ELSE do we throw an exception, A Message? No Just document that it is ignored
            }
            get { return _ws.ColoradoHistoricalExtractionStartYear; }
       
        }
        //---------------------------------------
        //Replace File	ColoradoHistoricalData    =1, paleo data; =2, Bireau of Rec data;=3, scenario data
        // Cannot be set while simulation in progress
        // using shadow value _Colorado_Historical_Data_Source no get in WaterSimU 
       
        /// <summary>   Gets or sets the colorado historical data source. </summary>
        /// <value> The colorado historical data source. </value>
        /// <remarks> The source of the Colorado River flow record: Value 1 uses the Bureau of Reclamation recorded record, Value 2 uses the tree ring reconstructed paleo record, Value 3 uses a user supplied river flow trace record Range = 1 to 3  Cannot be set after Simulation starts.</remarks>
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
              
        public int Colorado_Historical_Data_Source
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epColorado_Historical_Data_Source, value);
                    // ok setting but start and stop may not be set, change these and document
                    ColoradoRiverHistoricalReSetStart(value);
                    _ws.ColoradoHistoricalData = value;
                    // _Colorado_Historical_Data_Source = value; changed 7 24 11
                }
            }
            get { return _ws.ColoradoHistoricalData; } // _Colorado_Historical_Data_Source; } changed 7 24 11
        }
       
        //---------------------------------------
        //Climate Adjustment	ColoradoClimateAdjustmentPercent
       
        /// <summary>   Gets or sets the colorado climate adjustment percent. </summary>
        /// <value> The colorado climate adjustment percent. </value>
        /// <remarks> The percent (Value=50 is 50%) which is used to modify the Colorado river flow record, simulating impacts of climate change.  Change starts at beginning of Simulation. Range = 0 to 300  Cannot be set after Simulation starts.</remarks>
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
              
        public int Colorado_Climate_Adjustment_Percent
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epColorado_Climate_Adjustment_Percent, value);
                    _ws.ColoradoClimateAdjustmentPercent = value;
                }
            }
            get { return _ws.ColoradoClimateAdjustmentPercent; }
        }
        //---------------------------------------
        //Drought-Adjustment	ColoradoUserAdjustmentPercent
       
        /// <summary>   Gets or sets the colorado user adjustment percent. </summary>
        /// <value> The colorado user adjustment percent. </value>
        /// <remarks> The percent (Value=50 is 50%) which is used to modify the Colorado River flow record, starting and stopping in the years specified.  This is used to simulate a drought condition.  Range = 0 to 300  Cannot be set after Simulation starts.</remarks>
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        /// <seealso cref="Colorado_User_Adjustment_StartYear"/> 
        /// <seealso cref="Colorado_User_Adjustment_Stop_Year"/> 
       
        public int Colorado_User_Adjustment_Percent
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epColorado_User_Adjustment_Percent, value);
                    _ws.ColoradoUserAdjustmentPercent = value;
                }
            }
            get { return _ws.ColoradoUserAdjustmentPercent; }
        }
        //---------------------------------------
        //Drought-Start Year	ColoradoUserAdjustmentStartYear
       
        /// <summary>   Gets or sets the colorado user adjustment start year. </summary>
        /// <value> The colorado user adjustment start year. </value>
        /// <remarks> Determines the year the [Colorado User Adjustment %] will be start being applied.  Range = 2006 to 2081  Cannot be set after Simulation starts.</remarks>  
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        /// <seealso cref="Colorado_User_Adjustment_Percent"/>
        /// <seealso cref="Colorado_User_Adjustment_Stop_Year"/> 
       
        public int Colorado_User_Adjustment_StartYear
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epColorado_User_Adjustment_StartYear, value);
                    _ws.ColoradoUserAdjustmentStartYear = value;
                }
            }
            get { return _ws.ColoradoUserAdjustmentStartYear; }
        }
       
       
        //---------------------------------------
        //Drought_Stop year	ColoradoUserAdjustmentStopYear
       
        /// <summary>   Gets or sets the colorado user adjustment stop year. </summary>
        /// <value> The colorado user adjustment stop year. </value>
        /// <remarks> Determines the year the [Colorado User Adjustment %] will be stopped being applied. Range = 2006 to 2081  Cannot be set after Simulation starts.</remarks>  
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        /// <seealso cref="Colorado_User_Adjustment_Percent"/>
        /// <seealso cref="Colorado_User_Adjustment_StartYear"/> 
                
        public int Colorado_User_Adjustment_Stop_Year
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epColorado_User_Adjustment_Stop_Year, value);
                    _ws.ColoradoUserAdjustmentStopYear = value;
                }
            }
            get { return _ws.ColoradoUserAdjustmentStopYear; }
        }
        //---------------------------------------
        //Index Year	SaltVerdeHistoricalExtractionStartYear
       
        /// <summary>   Gets or sets the salt verde historical extraction start year. </summary>
        /// <value> The salt verde historical extraction start year. </value>
        /// <remarks> The first year of the Salt Verde River flow record that will be used to create a 25 year trace to simulate river flow conditions. Special Range Check Applies See SaltVerdeYearRangeCheck  Cannot be set after Simulation starts.  
        ///	Valid ranges are defined as follows <code>
        ///   internal RiverRange SaltVerdePaleo = new RiverRange(1330, 1982);
        ///   internal RiverRange SaltVerdeBureau = new RiverRange(1945, 1982);
        ///   </code></remarks>
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        /// <seealso cref="SaltVerde_Historical_Data_Source"/>
        /// <seealso cref="RiverRange"/>
       
        public int SaltVerde_Historical_Extraction_Start_Year
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epSaltVerde_Historical_Extraction_Start_Year, value);
                    _ws.SaltVerdeHistoricalExtractionStartYear = value;
                }
            }
            get { return _ws.SaltVerdeHistoricalExtractionStartYear; }
        }
        //---------------------------------------
        //Replace File	SaltVerdeTontoHistoricalData
       
        /// <summary>   Gets or sets the salt verde historical data source. </summary>
        /// <value> The salt verde historical data source. </value>
        /// <remarks> The source of the Salt Verde Rivers flow record: Value=1 uses the tree ring reconstructed paleo record, Value=2 uses the Bureau of Reclamation recorded record, Value=3 uses a user supplied river flow trace record. Range = 1 to 3  Cannot be set after Simulation starts.</remarks>  
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
       
        public int SaltVerde_Historical_Data_Source
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epSaltVerde_Historical_Data, value);
                    // ok setting but start and stop may not be set, change these and document
                    SaltVerdeRiverHistoricalReSetStart(value);
                    _ws.SaltVerdeTontoHistoricalData = value;
                    // _SaltVerde_Historical_Data = value;  // changed 7 24 11
                }
            }
            get { return _ws.SaltVerdeTontoHistoricalData; } // _SaltVerde_Historical_Data; } changed 7 24 11
        }
        //---------------------------------------
        //Climate Adjustment	SaltVerdeClimateAdjustmentPercent
       
        /// <summary>   Gets or sets the salt verde climate adjustment percent. </summary>
        ///
        /// <value> The salt verde climate adjustment percent. </value>
        /// <remarks> The percent (Value=50 is 50%) which is used to modify the Salt Verde River flow record, simulating impacts of climate change.  Change starts at beginning of Simulation. Range = 0 to 300  Cannot be set after Simulation starts.</remarks>  
            /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
       
        public int SaltVerde_Climate_Adjustment_Percent
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epSaltVerde_Climate_Adjustment_Percent, value);
                    _ws.SaltVerdeClimateAdjustmentPercent = value;
                }
            }
            get { return _ws.SaltVerdeClimateAdjustmentPercent; }
        }
        //---------------------------------------
        //Drought-Adjustment	SaltVerdeUserAdjustmentPercent
       
        /// <summary>   Gets or sets the salt verde user adjustment percent. </summary>
        /// <value> The salt verde user adjustment percent. </value>
        /// <remarks> The percent (Value=50 is 50%) which is used to modify the Salt Verde River flow record, starting and stopping in the years specified.  This is used to simulate a drought condition.  Range = 0 to 300  Cannot be set after Simulation starts.</remarks>  
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        /// <seealso cref="SaltVerde_User_Adjustment_Start_Year"/>
        /// <seealso cref="SaltVerde_User_Adjustment_Stop_Year"/>
        public int SaltVerde_User_Adjustment_Percent
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epSaltVerde_User_Adjustment_Percent, value);
                    _ws.SaltVerdeUserAdjustmentPercent = value;
                }
            }
            get { return _ws.SaltVerdeUserAdjustmentPercent; }
        }
        //---------------------------------------
        //Drought-Start Year	SaltVerdeUserAdjustmentStartYear
       
        /// <summary>   Gets or sets the salt verde user adjustment start year. </summary>
        /// <value> The salt verde user adjustment start year. </value>
        /// <remarks> Determines the year the [SaltVerde User Adjustment %] will be start being applied.  Range = 2006 to 2081  Cannot be set after Simulation starts.</remarks>  
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        /// <seealso cref="SaltVerde_User_Adjustment_Stop_Year"/>
        /// <seealso cref="SaltVerde_User_Adjustment_Percent"/>
       
        public int SaltVerde_User_Adjustment_Start_Year
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epSaltVerde_User_Adjustment_Start_Year, value);
                    _ws.SaltVerdeUserAdjustmentStartYear = value;
                }
            }
            get { return _ws.SaltVerdeUserAdjustmentStartYear; }
        }
        //---------------------------------------
        //Drought_Stop year	SaltVerdeUserAdjustmentStopYear
       
        /// <summary>   Gets or sets the salt verde user adjustment stop year. </summary>
        /// <value> The salt verde user adjustment stop year. </value>
        /// <remarks> Determines the year the [SaltVerde User Adjustment %] will be stopped being applied. Range = 2006 to 2081  Cannot be set after Simulation starts.</remarks>  
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>"
        /// <seealso cref="SaltVerde_User_Adjustment_Start_Year"/>
        /// <seealso cref="SaltVerde_User_Adjustment_Percent"/>
        public int SaltVerde_User_Adjustment_Stop_Year
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epSaltVerde_User_Adjustment_Stop_Year, value);
                    _ws.SaltVerdeUserAdjustmentStopYear = value;
                }
            }
            get { return _ws.SaltVerdeUserAdjustmentStopYear; }
        }
       
        //---------------------------------------
        //Water demand option	ProviderDemandOption
       
        /// <summary> Demand from file </summary>
        /// <remarks> Use with Provider_Demand_Option <see cref="Provider_Demand_Option"/></remarks> <seealso cref="Provider_Demand_Option"/>
        public const int pdoDemandFromFile = 1;
       
        /// <summary> Average gpcd and pop </summary>
        /// <remarks> Use with Provider_Demand_Option <see cref="Provider_Demand_Option"/></remarks> <seealso cref="Provider_Demand_Option"/>
        public const int pdoAverageGPCDandPOP = 2;
       
        /// <summary> Declining gpcd and pop </summary>
        /// <remarks> Use with Provider_Demand_Option <see cref="Provider_Demand_Option"/></remarks> <seealso cref="Provider_Demand_Option"/>
        public const int pdoDecliningGPCDandPOP = 3;
       
        /// <summary> User gpcd and pop </summary>
        /// <remarks> Use with Provider_Demand_Option <see cref="Provider_Demand_Option"/></remarks> <seealso cref="Provider_Demand_Option"/>
        public const int pdoUserGPCDandPOP = 4;
       
       
        /// <summary>   Gets or sets the provider demand option. </summary>
        ///
        /// <value> The provider demand option. </value>
        /// <remarks> The method that will be used to estimate annual demand  for all providers.  Value=1 reads demand values from an external file, Value=2 calculates demand based on a six year average GPCD and population read from a file, Value=3 estimates demand based on population estimates read from and external file and a smoothing function that slowly declines GPCD, Value=4 uses same method as 3, but allows the user to chnage the GPCD with Use_GPCD Paramtere <see cref="Use_GPCD"/>used for each provider at the beginning or during the simulation. Range = 1 to 4  Cannot be set after Simulation starts.</remarks>  
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        /// <seealso cref="Use_GPCD"/>
       
        public int Provider_Demand_Option
        {
            set
            {
                    _pm.CheckBaseValueRange(eModelParam.epProvider_Demand_Option, value);
                    _ws.ProviderDemandOption = value;
                }
       
            get { return _ws.ProviderDemandOption;} 
        }
        //===========================================================
       
        /// <summary>   Provider demand option label. </summary>
        /// <param name="value"> provider demand option value. </param>
        /// <returns>a string lable for the provider demand option values   </returns>
       
        public string Provider_Demand_Option_Label(int value)
        {
            _pm.CheckBaseValueRange(eModelParam.epProvider_Demand_Option, value);
            switch (value)
            {
                case pdoDemandFromFile:
                    return "Demand from file";
       
                case pdoAverageGPCDandPOP:
                    return "6 Year Average GPCD and Population";
       
                case pdoDecliningGPCDandPOP:
                    return "Declining GPCD and Population";
       
                case pdoUserGPCDandPOP:
                    return "User Assigned GPCD";
       
                default:
                    return "Bad Value";
       
            }
        }
        //---------------------------------------------------------------------------------------------------------------
        //set_ReduceGPCDpct
        // Fix for no get on GOCD
        int ReduceGPCDValue = util.DefaultReduceGPCDValue;
       
        /// <summary>   Gets or sets the Percent to Reduce GPCD. </summary>
        /// <value> The percent reduction in gpcd. </value>
        /// <remarks>The amount by which GPCD is expected to decrease by the end  of the simulation (i.e., 2085). 
        /// 		 Use this when Provider Demand Option is = 3 or Provider Demand Option=4.</remarks>
        /// <seealso cref="Provider_Demand_Option"/>
        /// 
        public int PCT_Reduce_GPCD
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epPCT_Alter_GPCD, value);
                    _ws.set_AlterGPCDpct = value;
                    // Store is this shadow value becuase model does not have one.
                    ReduceGPCDValue = value;
                }
            }
            get { return ReduceGPCDValue;}
        }
       
       
        
        //---------------------------------------
        // get_ProviderPopGrowthRateAdjustmentPct
        // provides for region wide adjusted growth rate
        // Must be 0 to use provider level pop growth rate adjustement
        //        public void seti_PCT_REG_Growth_Rate_Adjustment_Value(int value) { if (!FModelLocked) _ws.PopulationGrowthRateAdjustmentPercent = value; }
       
        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Gets or sets the regional growth rate adjustment factor. </summary>
        ///
        /// <value> The regional growth rate adjustment factor </value>
        /// <remarks>Adjuses the project growth rate for all providers by this factor.  100 means no change, 50 means 50% less than projection, 150 means 150% more than projected</remarks>
        ///-------------------------------------------------------------------------------------------------
       
        public int Regional_Growth_Rate_Adjust
        {
            set
            {
                /// DAVID Changed 2/24/14
                //if ((!_inRun) & (!FModelLocked))
                if (!FModelLocked)
                {
                    _pm.CheckBaseValueRange(eModelParam.epPCT_REG_Growth_Rate_Adjustment, value);
                    seti_PCT_REG_Growth_Rate_Adjustment(value);
                }
                // ELSE do we throw an exception? No Just document that it is ignored
            }
            get { return geti_PCT_REG_Growth_Rate_Adjustment(); }
       
        }
       
        //--------------------------------------------------
       
        internal bool _Assured_Water_Supply_Annual_Groundwater_Pumping_Limit = false;

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Gets or sets the assured water supply annual groundwater pumping limit.
        ///  </summary>
        ///
        /// <value> The assured water supply annual groundwater pumping limit. </value>
        ///-------------------------------------------------------------------------------------------------

        public int Assured_Water_Supply_Annual_Groundwater_Pumping_Limit
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _pm.CheckBaseValueRange(eModelParam.epAWSAnnualGWLimit, value);
                    seti_Assured_Water_Supply_Annual_Groundwater_Pumping_Limit(value);
                }
            }
            get
            {
                return geti_Assured_Water_Supply_Annual_Groundwater_Pumping_Limit();
            }
        }
        //
        // DAS 01.09.15
        /// <summary>
        /// Tells the fortran model if the simulation is for default conditions (zero; false) or not (one; true)
        /// Yes, I know, I am using reverse logic here....
        /// </summary>
        public int API_Default_Status
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    _ws.APIdefaultStatus = Convert.ToBoolean(value);
                }
            }
            get { return Convert.ToInt32(_ws.APIdefaultStatus); }    // 
        }
        // DAS 01.26.16
        // We WILL be implementing this in the final Version 5 release (and UI 23.9)
        private int Drought_Scenario;
        public int API_Drought_Scenario
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    Drought_Scenario = value;

                }
            }
            get { return Drought_Scenario; }    // 
        }
        //
        public bool COdeltaBurdenToAZ
        {
            set
            {
                _ws.set_COdeltaBurden = value;
                _COdeltaAZ = value;
            }
            get
            {
                return _COdeltaAZ;
            }
        }
        public int COdeltaBurdenRatioForAZ
        {
            set
            {
                Static = value;
            }
            get
            {
                return _ws.get_COdeltaRatio;
            }
        }
        public int BaseYear
        {
            set
            {
                _BaseYear = value;
            }
            get
            {
                return _BaseYear;
            }
        }

        //
        private int Scenario_Presets;
        public int API_Scenario_Presets
        {
            set
            {
                if ((!_inRun) & (!FModelLocked))
                {
                    Scenario_Presets = value;

                }
            }
            get { return Scenario_Presets; }    // 
        }

        #endregion

        //=====================================
        // Provider Inputs
        //=====================================

       // #region Provider Inputs

        //----------------------------------------------------------------
        /// <summary>
        /// 
        /// </summary>
        public providerArrayProperty Maximum_normalFlow_rights;
        internal int[] get_normalFlow_rights_max()
        { return _ws.NormalFlow_rights_max; }
        internal void set_normalFlow_rights_max(int[] value)
        {
            if (!FModelLocked) _ws.NormalFlow_rights_max = value;
        }
        //----------------------------------------------------------------

        // QUAY revised 3/16/13
        //
        // DAS revised 03.15.14
        /// <summary>
        /// An Amount of Augmented Water to add to Supply
        /// </summary>
        /// <remarks> Factor used to add additional water from some source external to the region to a providers water supply</remarks>
        public providerArrayProperty WaterAugmentation;
        //internal int[] get_NewWater() { return WaterAugmentationMemory; }
        internal int[] get_NewWater() { return _ws.NewWaterSupplies; }
        internal void set_NewWater(int[] value)
        {
            if (!FModelLocked)
            { 
                _ws.NewWaterSupplies = value;
                WaterAugmentationMemory = value;
            }
        }
    //
        public providerArrayProperty WaterAugmentationUsed;
        internal int[] get_NewWaterUsed() { return _ws.get_NewWaterSuppliesUsed ; }
          

    //
        public providerArrayProperty PCT_alter_GPCD;
        internal void set_alterGPCDpct(int[] value) {
            API_Default_Status = 0;
            if (!FModelLocked)  _ws.AlterProviderGPCDpct = value; }
        internal int[] get_alterGPCDpct() {return _ws.AlterProviderGPCDpct ; }
        //


        // das_April
        /// <summary>
        /// The minimum GPCD for each provider
        /// </summary>
        public int[] holdMinGPCD = new int[ProviderClass.NumberOfProviders];
        public providerArrayProperty Provider_GPCD_Min;
        // 07.20.16 DAS - this code was reading minimum GPCD out of the FORTRAN model. I changed
        // the call to read from the API
        //internal void set_GPCDmin(int[] value) { if (!FModelLocked)  _ws.ProviderGPCDmin = value; }
        internal void set_GPCDmin(int[] value) { if (!FModelLocked)
            
            holdMinGPCD = value;
        _ws.ProviderGPCDmin = value;
        }
        //internal int[] get_GPCDmin() {return _ws.ProviderGPCDmin ; }
        internal int[] get_GPCDmin() { return holdMinGPCD; }
        // end 07.20.16 DAS
              
               
        // Normal Flow
        internal int[] get_modifyNormalFlow() { return _ws.ModifyProviderNormalFlowPct; }
        internal void set_modifyNormalFlow(int[] value) { if (!FModelLocked)  _ws.ModifyProviderNormalFlowPct = value; }
               
       
        /// <summary> The pct modify normal flow </summary>
        /// <remarks> Factor used to adjust normal flow allocation per acre (SRP)</remarks>
        public providerArrayProperty PCT_modify_normalFlow;
       
       
        //WWtoRWWTP	parmWWtoRWWTPpct
        internal int[] get_PCT_Wastewater_Reclaimed() { return _ws.parmWWtoRWWTPpct; }
        internal void set_PCT_Wastewater_Reclaimed(int[] value)
        {  //pm.CheckValueRange(eModelParam.epPCT_Effluent_Reclaimed, value, true);    
            if (!FModelLocked) _ws.parmWWtoRWWTPpct = value;
        }
       
        /// <summary> The percent wastewater reclaimed </summary>
        /// <remarks> The percent of wasterwater that is sent to a Reclaim Plant, Range 0 to 100 </remarks>  
            /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
       
        public providerArrayProperty PCT_Wastewater_Reclaimed;
       
        //---------------------------------------
        //WWtoEffluent	parmWWtoEffluentPct
        internal int[] get_PCT_Wastewater_to_Effluent() { return _ws.parmWWtoEffluentPct; }
        internal void set_PCT_Wastewater_to_Effluent(int[] value) { if (!FModelLocked) _ws.parmWWtoEffluentPct = value; }
       
        /// <summary> The percent wastewater to effluent </summary>
        /// <remarks> The percent of wasterwater effluent that is used and not discharged into a water course. Range = 0 to 100</remarks>  
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
       
        public providerArrayProperty PCT_Wastewater_to_Effluent;
       
        //---------------------------------------
        //RWWtoRO	parmReclaimedWWtoROpct
        internal int[] get_PCT_Reclaimed_to_RO() { return _ws.parmReclaimedWWtoROpct; }
        internal void set_PCT_Reclaimed_to_RO(int[] value) { if (!FModelLocked)  _ws.parmReclaimedWWtoROpct = value; }
       
        /// <summary> The percent reclaimed to Reverse Osmosis </summary>
        /// <remarks> The percent of  reclaimed water that is sent to a Reverse Osmosis Plant (becomes potable). Special Range Check Applies See PCTReclaimedRangeCheck</remarks>  
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        /// <seealso cref="WaterSimDCDC.WaterSimManager.PCTReclaimedRangeCheck(int, WaterSimDCDC.eProvider, int)"/>
        /// <seealso cref="WaterSimDCDC.WaterSimManager.PCTReclaimedRangeCheck(int, WaterSimDCDC.eProvider, ref string, WaterSimDCDC.ModelParameterBaseClass)"/>
        public providerArrayProperty PCT_Reclaimed_to_RO;
       
        //---------------------------------------
        //ROtoOutput	parmROReclaimedToOutputPct
        internal int[] get_PCT_RO_to_Water_Supply() { return _ws.parmROReclaimedToOutputPct; }
        internal void set_PCT_RO_to_Water_Supply(int[] value) { if (!FModelLocked)  _ws.parmROReclaimedToOutputPct = value; }
       
        /// <summary> The percent reverse osmosis water to water supply </summary>
        /// <remarks> The percent of  water from Reverse Osmosis Plant that is used for potable water demand. Range = 0 to 100</remarks>  
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        public providerArrayProperty PCT_RO_to_Water_Supply;
       
        //---------------------------------------
        //RtoDInjection	parmReclaimedToDirectInjectPct
        internal int[] get_PCT_Reclaimed_to_DirectInject() { return _ws.parmReclaimedToDirectInjectPct; }
        internal void set_PCT_Reclaimed_to_DirectInject(int[] value) { if (!FModelLocked)  _ws.parmReclaimedToDirectInjectPct = value; }
               
       
        /// <summary> The percent reclaimed to direct inject </summary>
        /// <remarks> The percent of  reclaimed ater that is used to recharge an aquifer by direct injection into an aquifer. Special Range Check Applies See PCTReclaimedRangeCheck</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        /// <seealso cref="WaterSimDCDC.WaterSimManager.PCTReclaimedRangeCheck(int, WaterSimDCDC.eProvider, int)"/>
        /// <seealso cref="WaterSimDCDC.WaterSimManager.PCTReclaimedRangeCheck(int, WaterSimDCDC.eProvider, ref string, WaterSimDCDC.ModelParameterBaseClass)"/>
        /// <seealso cref="ParameterManagerClass.reclaimedchecklist"/>
        public providerArrayProperty PCT_Reclaimed_to_DirectInject;
       
        //---------------------------------------
        //RtoOutput	parmReclaimedToOutputPct
        internal int[] get_PCT_Reclaimed_to_Water_Supply() { return _ws.parmReclaimedToOutputPct; }
        internal void set_PCT_Reclaimed_to_Water_Supply(int[] value) { if (!FModelLocked)  _ws.parmReclaimedToOutputPct = value; }
       
        /// <summary> The percent reclaimed to water supply </summary>
        /// <remarks> The percent of  recliamed water that is used to meet qualified user demands (non-potable). Special Range Check Applies See PCTReclaimedRangeCheck</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        /// <seealso cref="WaterSimDCDC.WaterSimManager.PCTReclaimedRangeCheck(int, WaterSimDCDC.eProvider, int)"/>
        /// <seealso cref="WaterSimDCDC.WaterSimManager.PCTReclaimedRangeCheck(int, WaterSimDCDC.eProvider, ref string, WaterSimDCDC.ModelParameterBaseClass)"/>
        /// <seealso cref="ParameterManagerClass.reclaimedchecklist"/>
        public providerArrayProperty PCT_Reclaimed_to_Water_Supply;
       
        //---------------------------------------
        //RtoVadose	parmReclaimedToVadosePct
        internal int[] get_PCT_Reclaimed_to_Vadose() { return _ws.parmReclaimedToVadosePct; }
        internal void set_PCT_Reclaimed_to_Vadose(int[] value) { if (!FModelLocked)  _ws.parmReclaimedToVadosePct = value; }
       
        /// <summary> The percent reclaimed to vadose </summary>
        /// <remarks> The percent of  reclaimed water that is delivered to a vadoze zone recharge basin. Special Range Check Applies See PCTReclaimedRangeCheck</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        /// <seealso cref="WaterSimDCDC.WaterSimManager.PCTReclaimedRangeCheck(int, WaterSimDCDC.eProvider, int)"/>
        /// <seealso cref="WaterSimDCDC.WaterSimManager.PCTReclaimedRangeCheck(int, WaterSimDCDC.eProvider, ref string, WaterSimDCDC.ModelParameterBaseClass)"/>
        /// <seealso cref="ParameterManagerClass.reclaimedchecklist"/>
        public providerArrayProperty PCT_Reclaimed_to_Vadose;
       
        //---------------------------------------
        //get_EffluentToVadose	parmEffluentToVadosePct
        internal int[] get_PCT_Effluent_to_Vadose() { return _ws.parmEffluentToVadosePct; }
        internal void set_PCT_Effluent_to_Vadose(int[] value) { if (!FModelLocked)  _ws.parmEffluentToVadosePct = value; }
               
       
        /// <summary> The percent effluent to vadose </summary>
        /// <remarks> The percent of  wastewater effluent delivered to a vadose zone recharge basin. Special Range Check Applies See PCTEffluentRangeCheck</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        /// <seealso cref="WaterSimDCDC.WaterSimManager.PCTReclaimedRangeCheck(int, WaterSimDCDC.eProvider, int)"/>
        /// <seealso cref="WaterSimDCDC.WaterSimManager.PCTReclaimedRangeCheck(int, WaterSimDCDC.eProvider, ref string, WaterSimDCDC.ModelParameterBaseClass)"/>
        /// <seealso cref="WaterSimDCDC.ParameterManagerClass.effluentchecklist"/>
        public providerArrayProperty PCT_Effluent_to_Vadose;
       
        //---------------------------------------
        //EffluentToPP	parmEffluentToPowerPlantPct
        internal int[] get_PCT_Effluent_to_PowerPlant() { return _ws.parmEffluentToPowerPlantPct; }
        internal void set_PCT_Effluent_to_PowerPlant(int[] value) { if (!FModelLocked)  _ws.parmEffluentToPowerPlantPct = value; }
               
       
        /// <summary> The percent effluent to power plant </summary>
        /// <remarks> The percent of  wastewater effluent delivered to a power plants for cooling towers. Special Range Check Applies See PCTEffluentRangeCheck</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        /// <seealso cref="WaterSimDCDC.WaterSimManager.PCTEffluentRangeCheck(int, WaterSimDCDC.eProvider, int)"/>
        /// <seealso cref="WaterSimDCDC.WaterSimManager.PCTEffluentRangeCheck(int, WaterSimDCDC.eProvider, ref string, WaterSimDCDC.ModelParameterBaseClass)"/>
        /// <seealso cref="ParameterManagerClass.effluentchecklist"/>
        public providerArrayProperty PCT_Effluent_to_PowerPlant;
       
        //---------------------------------------
        //SWtoVadose	parmSurfaceWaterToVadosePct
        internal int[] get_SurfaceWater__to_Vadose() { return _ws.parmSurfaceWaterToVadoseAmt; } // changed 7 22 11 from get_parmSurfaceWaterToVadosePCT
        internal void set_SurfaceWater__to_Vadose(int[] value) 
		{ 
			if (!FModelLocked)
			{
				_ws.parmSurfaceWaterToVadoseAmt = value;
 			}
		} // changed 7 22 11 from get_parmSurfaceWaterToVadosePCT
       
        /// <summary> Surface water to vadose </summary>
        /// <remarks> The amount of surface water supply delivered to a vadose zone basin. Range = 0 to 100000</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        public providerArrayProperty SurfaceWater__to_Vadose;
       
        //---------------------------------------
        //SWtoWB	parmSurfaceWaterToWBankPct
        internal int[] get_PCT_SurfaceWater_to_WaterBank() { return _ws.parmSurfaceWaterToWbankPct; }  // _ws.parmSurfaceWaterToWbankPct chnaged in model 7 20 11
        internal void set_PCT_SurfaceWater_to_WaterBank(int[] value) { if (!FModelLocked)  _ws.parmSurfaceWaterToWbankPct = value; }  // _ws.parmSurfaceWaterToWbankPct chnaged in model 7 20 11
               
       
        /// <summary> The percent surface water to water bank </summary>
        /// <remarks> The percent of extra surface water that is sent to a water bank if [WaterBank Source Option] is set to a Value=1. Range = 0 to 100</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        public providerArrayProperty PCT_SurfaceWater_to_WaterBank;
       
        //---------------------------------------
        //    parmSurfaceWaterToWBankAmt
        internal int[] get_Use_SurfaceWater_to_WaterBank()
        { return _ws.parmSurfaceWaterToWBankAmt; }  // changed 7 26 11  API.set_parmSurfaceWaterToWbankAmt; } // Using array of myAPI as Dummy, have to think about this
        internal void set_Use_SurfaceWater_to_WaterBank(int[] value)
        {
            if (!FModelLocked)
            {
                _ws.parmSurfaceWaterToWBankAmt = value;
            }
        }
        /// <summary> The use surface water to water bank </summary>
        /// <remarks> The amount of water (source unknown) sent to a water bank if [WaterBank Source Option] is set to a Value=2. Range = 0 to 100000.  This parameter is essentially a way to create a new bucket of water that can be used when other supplies do not meet demand.  This could include fallowing of Colorado River farms, desalination of water in Mexico or California, sale of upper basic water rights etc.</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        public providerArrayProperty Use_SurfaceWater_to_WaterBank;
       
        //---------------------------------------
        //WStoRes	parmWaterSupplyToResidentialPct
        internal int[] get_PCT_WaterSupply_to_Residential() { return _ws.parmWaterSupplyToResPct; }
        internal void set_PCT_WaterSupply_to_Residential(int[] value) { if (!FModelLocked)  _ws.parmWaterSupplyToResPct = value; }
       
        /// <summary> The percent water supply to residential </summary>
        /// <remarks> The percent of total water supply used by residential customers. Special Range Check Applies See ResComPCTRangeCheck</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        /// <seealso cref="ResComPCTRangeCheck"/>
        /// <seealso cref="ParameterManagerClass.wateruserchecklist"/>
        public providerArrayProperty PCT_WaterSupply_to_Residential;
       
        //---------------------------------------
        //WStoCio	parmWaterSupplyToComPct
        internal int[] get_PCT_WaterSupply_to_Commercial() { return _ws.parmWaterSupplyToComPct; }
        internal void set_PCT_WaterSupply_to_Commercial(int[] value) { if (!FModelLocked)  _ws.parmWaterSupplyToComPct = value; }
       
        /// <summary> The percent water supply to commercial </summary>
        /// <remarks> The percent of  total water supply used by commercial customers. Special Range Check Applies See ResComPCTRangeCheck</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        /// <seealso cref="PCTResComRangeCheck"/>
        /// <seealso cref="ParameterManagerClass.wateruserchecklist"/>
        public providerArrayProperty PCT_WaterSupply_to_Commercial;
       
        //---------------------------------------
        internal int[] get_PCT_WaterSupply_to_Industrial() { return _ws.parmWaterSupplyToIndPct; }
        internal void set_PCT_WaterSupply_to_Industrial(int[] value) { if (!FModelLocked)  _ws.parmWaterSupplyToIndPct = value; }
       
        /// <summary> The percent water supply to industrial uses</summary>
        /// <remarks> The percent of  total water supply used by industiral vustomers. Special Range Check Applies See ResComPCTRangeCheck</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
        /// <seealso cref="PCTResComRangeCheck"/>
        /// <seealso cref="ParameterManagerClass.wateruserchecklist"/>
        public providerArrayProperty PCT_WaterSupply_to_Industrial;
        //
        //---------------------------------------
        ////OutDoorPct	parmOutdoorWaterUsePct
        //internal int[] get_PCT_Outdoor_WaterUse() { return _ws.get_parmOutdoorWaterUsePct; }
        //internal void set_PCT_Outdoor_WaterUse(int[] value) { if (!FModelLocked)  _ws.get_parmOutdoorWaterUsePct = value; }
       
        ///// <summary> The percent outdoor water use </summary>
        ///// <remarks> The percent of  potable water supply used for outdoor wate uses. Range = 0 to 100</remarks> 
        ///// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
       
        //public providerArrayProperty PCT_Outdoor_WaterUse;
       
        //---------------------------------------
        //GWtoGWTP	parmGroundwaterToGWTPlantPct
        internal int[] get_PCT_Groundwater_Treated() { return _ws.parmGroundwaterToGWTPlantPct; }
        internal void set_PCT_Groundwater_Treated(int[] value) { if (!FModelLocked)  _ws.parmGroundwaterToGWTPlantPct = value; }
       
        /// <summary> The percent groundwater treated </summary>
        /// <remarks> The percent of  groundwater that is treated before it is used for potable water supply. Range = 0 to 100</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
       
        public providerArrayProperty PCT_Groundwater_Treated;
       
        //---------------------------------------
        //ReclaimedOutdoor	parmReclaimedOutdoorUsePct
        internal int[] get_PCT_Reclaimed_Outdoor_Use() { return _ws.parmReclaimedOutdoorUsePct; }
        internal void set_PCT_Reclaimed_Outdoor_Use(int[] value) { if (!FModelLocked)  _ws.parmReclaimedOutdoorUsePct = value; }
       
        /// <summary> The percent reclaimed outdoor use </summary>
        /// <remarks> The percent of  outdoor water use that can be supplied by reclaimed water. Range = 0 to 100</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
         
        public providerArrayProperty PCT_Reclaimed_Outdoor_Use;
       
        // New Added to version 3
       
        //---------------------------------------
        // Use_WaterSupply_to_DirectInject 
        internal int[] get_Use_WaterSupply_to_DirectInject()
        { return _ws.parmWaterSupplyToDirectInjectAmt; } // chnaged 7 24 11  API.set_parmWaterSupplyToDirectInjectAmt; } // Using array of myAPI as Dummy, have to think about this
        internal void set_Use_WaterSupply_to_DirectInject(int[] value)
        {
            if (!FModelLocked)
            {
                _ws.parmWaterSupplyToDirectInjectAmt = value;
            }
        }
       
        /// <summary> The amount of water to direct inject </summary>
        /// <remarks> A fixed amount of potable water supply used for aquifer recharge by directly injecting into a potable aquifer. Range = 0 to 100000</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
       
        public providerArrayProperty Use_WaterSupply_to_DirectInject;
       
        //---------------------------------------
        // ***** Changed 8 13 12
        //// SetPopulation  Set_Population  // added 12 19 11 based on changes to model interface
       
        //internal int[] get_populations()
        //{ return SetPopultaionsValues; }
        //internal void set_populations(int [] value)
        //   {
        //    if (!FModelLocked)
        //    {
        //        _ws.SetPopulations = value;
        //        SetPopultaionsValues = value;
        //    }
        //}
       
            internal int[] get_populationsOn()
            { return SetPopultaionsValuesOn; }
            internal void set_populationsOn(int [] value)
            {
            if (!FModelLocked)
            {
                _ws.set_PopulationsOn = value;
                SetPopultaionsValuesOn = value;
            }
        }
        //*********
        /// <summary> Provider On Project Population Override</summary>
        /// <remarks> If set to a value other than 0, is used as the population of the provider, if zero then population calculated by model</remarks>
        // Changed 8 13 12 public providerArrayProperty Population_Override;
        public providerArrayProperty Population_Override_On;
       
        //******** Added 8 13 12
        internal int[] get_populationsOther()
        { return SetPopultaionsValuesOther; }
        internal void set_populationsOther(int[] value)
        {
            if (!FModelLocked)
            {
                _ws.set_PopulationsOther = value;
                SetPopultaionsValuesOther = value;
            }
        }
        /// <summary> Provider Off Project  Population Override</summary>
        /// <remarks> If set to a value other than 0, is used as the population of the provider, if zero then population calculated by model</remarks>
        public providerArrayProperty Population_Override_Other;
        //*********
       
       
        // 
        //---------------------------------------
        // TimeLagVadoseYears  Surface_to_Vadose_Time_Lag
        internal int[] get_Surface_to_Vadose_Time_Lag()
        { return _ws.TimeLagVadoseYears; } // changed 7 26 11  API.set_TimeLagVadoseYears; } // Using array of myAPI as Dummy, have to think about this
        internal void set_Surface_to_Vadose_Time_Lag(int[] value)
        {
            if (!FModelLocked)
            {
                _ws.TimeLagVadoseYears = value;
            }
        }
       
        /// <summary> The surface to vadose time lag </summary>
        /// <remarks> The time in years it takes water recharged to the vadose zone to reach the aquifer used for groundwater supply. Range = 0 to 50</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
       
        public providerArrayProperty Surface_to_Vadose_Time_Lag;
       
        //---------------------------------------
        //ProviderGPCD
        // This is a complicated property depends on how Provider_demand_option is set
       
        internal int[] get_Use_GPCD()
        {
            if (Provider_Demand_Option == 4)
                return GPCDBuffer ;
            else
                return GPCDDefaults;
        }
              
        internal void set_Use_GPCD(int[] value)
        {
            if (!FModelLocked)
            {
                if (Provider_Demand_Option == 4)
                {
                    _ws.ProviderGPCD = value;
                    GPCDBuffer = value;
                }
                else 
                {
                    // OK ignore for now -----  throw new WaterSim_Exception(WS_Strings.wsSetGPCDError);
                }
            }
        }
       
        /// <summary> The gpcd to use with User Supplied GPCD</summary>
        /// <remarks> The GPCD that will be used if [Provider Demand Option] is set to Value=4. Range = 30 to 500</remarks>  
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check, for now ignoring if Provider_Demand_Option is not set to 4 and values are set</exception>
        /// <seealso cref="Provider_Demand_Option"/>
        public providerArrayProperty Use_GPCD;
       
         
        ////---------------------------------------
        //// get_ProviderPopGrowthRateAdjustPct
        //// this is a bit compliated, in order for this to work, get_ProviderPopGrowthRateAdjustmentPct (see below) must be set to 0, Once set to 0, 
        //// it shold not be set back to 100 until a model run is over, otherwise growth curves will revert back to olld growth curves 
        //internal int[] get_PCT_Growth_Rate_Adjustment() { return _ws.get_ProviderPopGrowthRateAdjustPct; }
        //internal void set_PCT_Growth_Rate_Adjustment(int[] value)
        //{
        //    if (!FModelLocked)
        //    {
        //        // if regional growth factor is not 0 then check if value has non 100 values, if so set regional to 0
        //        //if (geti_PCT_REG_Growth_Rate_Adjustment() != 0)
        //        //{
        //        //    bool found = false;
        //        //    foreach(int pct in value)
        //        //        if (pct != 100)
        //        //        { 
        //        //            found = true;
        //        //            break;
        //        //        }
        //            if (found) seti_PCT_REG_Growth_Rate_Adjustment(0);
        //        //}
        //        _ws.get_ProviderPopGrowthRateAdjustPct = value;
        //    }
        //}
               
        ///// <summary> The percent growth rate adjustment </summary>
        ///// <remarks> A factor (percent) that is used to adjust the rate of population growth.  Used to create faster or slower growth rate scenarios.  100 (100%) means no factor is applied.  A zero means I pack my bags and move to one of the coasts. Range = 0 to 300</remarks> 
        ///// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
       
        //public providerArrayProperty PCT_Growth_Rate_Adjustment;
       
       
        //=============================================================
        // ProviderPopGrowthRateOnProjectPct 
        internal int[] get_PCT_Growth_Rate_Adjustment_OnProject() { return _ws.ProviderPopGrowthRateOnProjectPct; }
        internal void set_PCT_Growth_Rate_Adjustment_OnProject(int[] value)
        {
            if (!FModelLocked)
            {
                //if regional growth factor is not 0 then check if value has non 100 values, if so set regional to 0
                //if (geti_PCT_REG_Growth_Rate_Adjustment() != 0)
                //{
                //    bool found = false;
                //    foreach(int pct in value)
                //        if (pct != 100)
                //        { 
                //            found = true;
                //            break;
                //        }
                //if (found) seti_PCT_REG_Growth_Rate_Adjustment(0);
                //}
                _ws.ProviderPopGrowthRateOnProjectPct = value;
            }
        }
       
        /// <summary> The percent growth rate adjustment for OnProject Demand</summary>
        /// <remarks> A factor (percent) that is used to adjust the rate of population growth.  Used to create faster or slower growth rate scenarios.  100 (100%) means no factor is applied.  A zero means I pack my bags and move to one of the coasts. Range = 0 to 300</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
       
        public providerArrayProperty PCT_Growth_Rate_Adjustment_OnProject;
       
        //=============================================================
        // ProviderPopGrowthRateOtherPct 
        internal int[] get_PCT_Growth_Rate_Adjustment_Other() { return _ws.ProviderPopGrowthRateOtherPct; }
        internal void set_PCT_Growth_Rate_Adjustment_Other(int[] value)
        {
            if (!FModelLocked)
            {
                //if regional growth factor is not 0 then check if value has non 100 values, if so set regional to 0
                //if (geti_PCT_REG_Growth_Rate_Adjustment() != 0)
                //{
                //    bool found = false;
                //    foreach (int pct in value)
                //        if (pct != 100)
                //        {
                //            found = true;
                //            break;
                //        }
                //    if (found) seti_PCT_REG_Growth_Rate_Adjustment(0);
                //}
                _ws.ProviderPopGrowthRateOtherPct = value;
            }
        }
       
        /// <summary> The percent growth rate adjustment for Off Project Demand</summary>
        /// <remarks> A factor (percent) that is used to adjust the rate of population growth.  Used to create faster or slower growth rate scenarios.  100 (100%) means no factor is applied.  A zero means I pack my bags and move to one of the coasts. Range = 0 to 300</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
       
        public providerArrayProperty PCT_Growth_Rate_Adjustment_Other;
       
       
        //---------------------------------------
        //_ws.WaterBankingOption = set_WaterBankingOption;
        internal int[] get_WaterBank_Source_Option() { return _ws.WaterBankingOption; } // API.set_WaterBankingOption; } changed 7 24 11
        internal void set_WaterBank_Source_Option(int[] value)
        {
            if (!FModelLocked)
            {
                _ws.WaterBankingOption = value;
            }
        }
        
        /// <summary> The water bank source option </summary>
        /// <remarks> The source of water used for external water banking (outside provider groundwater): Value=1 a percent [% SurfaceWater to WaterBank] of ""unused"" surface water is sent to a water bank, Value= 2 a fixed amount[Amount of SurfaceWater to WaterBank] of an unknown extra source of water is sent to a water bank. Range = 1 to 2</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>
       
        public providerArrayProperty WaterBank_Source_Option;
       
        // Added to Model 8 10 2011
        //---------------------------------------
        //parmReclaimedToInputPct 
        internal int[] get_PCT_Max_Demand_Reclaim() { return _ws.parmReclaimedToInputPct; }
        internal void set_PCT_Max_Demand_Reclaim(int[] value) { if (!FModelLocked)  _ws.parmReclaimedToInputPct = value; }
       
        /// <summary> The percent maximum demand reclaim </summary>
        /// <remarks> The maximum percent of total demand that can be met by reclaimed water Range = 0 to 70</remarks> 
        /// <exception cref="WaterSim_Exception">if setting a value that does not pass the range check</exception>"
       
        public providerArrayProperty PCT_Max_Demand_Reclaim;

        //-------------------------------------------------------------------------- 
        //OutDoorPct	internal int[] get_parmWaterSupplyToResPct
        // Changed 8 13 12 internal int[] get_PCT_Outdoor_WaterUseRes() { return _ws.get_parmOutdoorWaterUseResPct; } 
        internal int[] get_PCT_Outdoor_WaterUseRes() { return _ws.parmOutdoorWaterUseResPct; }  // changed 8 13 12
        // Changed 8 13 12 internal void set_PCT_Outdoor_WaterUseRes(int[] value) { if (!FModelLocked)  _ws.get_parmOutdoorWaterUseResPct = value; }
        internal void set_PCT_Outdoor_WaterUseRes(int[] value) { if (!FModelLocked)  _ws.parmOutdoorWaterUseResPct = value; }

        /// <summary> The percent outdoor water use for residential </summary>
        /// <remarks> Indoor water use is 100 - PCT_Outdoor_WaterUseRes</remarks>
        public providerArrayProperty PCT_Outdoor_WaterUseRes;

        //-----------------------------------------------------
        //OutDoorPct	internal int[] get_parmWaterSupplyToComPct
        // Changed 8 13 12  internal int[] get_PCT_Outdoor_WaterUseCom() { return _ws.get_parmOutdoorWaterUseComPct; }
        // Changed 8 13 12  internal void set_PCT_Outdoor_WaterUseCom(int[] value) { if (!FModelLocked)  _ws.get_parmOutdoorWaterUseComPct = value; }
        internal int[] get_PCT_Outdoor_WaterUseCom() { return _ws.parmOutdoorWaterUseComPct; }
        internal void set_PCT_Outdoor_WaterUseCom(int[] value) { if (!FModelLocked)  _ws.parmOutdoorWaterUseComPct = value; }

        /// <summary> The pct outdoor water use for commercial </summary>
        /// <remarks> Indoor water use is 100 - PCT_Outdoor_WaterUseCom</remarks>
        public providerArrayProperty PCT_Outdoor_WaterUseCom;

        //OutDoorPct	 :internal int[] get_parmWaterSupplyToIndPct
        // Changed 8 13 12  internal int[] get_PCT_Outdoor_WaterUseInd() { return _ws.get_parmOutdoorWaterUseIndPct; }
        // Changed 8 13 12  internal void set_PCT_Outdoor_WaterUseInd(int[] value) { if (!FModelLocked)  _ws.get_parmOutdoorWaterUseIndPct = value; }
        internal int[] get_PCT_Outdoor_WaterUseInd() { return _ws.parmOutdoorWaterUseIndPct; }
        internal void set_PCT_Outdoor_WaterUseInd(int[] value) { if (!FModelLocked)  _ws.parmOutdoorWaterUseIndPct = value; }

        /// <summary> The pct outdoor water use for industiral </summary>
        /// <remarks> Indoor water use is 100 - PCT_Outdoor_WaterUseInd</remarks>
        public providerArrayProperty PCT_Outdoor_WaterUseInd;


        //---------------------------------------
        /// <summary>
        /// Providers pump a minimum amount of groundwater
        /// This first approach is a way to use a percentage of their
        /// water demand to estimate a default groundwater pumping 
        /// amount (AF yr-1). I think we are starting out with 1%, but
        /// there is a parameter (
        /// </summary>
        /// <returns></returns>
        internal int[] get_Default_MandI_Pumping_PCT() { return _ws.parmDefaultPumpingMandIPct; }
        internal void set_Default_MandI_Pumping_PCT(int[] value) { if (!FModelLocked)  _ws.parmDefaultPumpingMandIPct = value; }

        public providerArrayProperty Default_Pumping_MnI_PCT;
        //
        // DAS 06.02.16
            // Provider level Agriculture Water used divided by threshold (now, 2014 as basis)
            // or, cummulative credit transfer (100 minus) to municipal water users (100% is 100% ag use)
        private int[] get_AgWaterUsed_ratio()
        {
            return _ws.get_AgWaterUsedRelativeTo2014_PCT;
        }

            /// <summary> The gpcd used </summary>
            ///<remarks>The GPCD used to estimate demand for the completed simulation year. When Provider_Demand_Option is 1,2, or 3, this is the calculated GPCD used to estimate demand.  
            ///		   if the Provider_Demand_Option =4, this is the GPCD specified by Use_GPCD parameter. Units: gpcd-gallons per capita per daya of water use</remarks>
            /// <seealso cref="Provider_Demand_Option"/>
            /// <seealso cref="Use_GPCD"/>
            public providerArrayProperty PCT_AgWaterUsedToThresh;


            //---------------------------------------
            //ProviderGPCD residential
            // 01.21.15 DAS
            private int[] get_GPCD_Res()
            { return _ws.get_ProviderGPCDres; }

            /// <summary> The gpcd used </summary>
            ///<remarks>The GPCD used to estimate demand for the completed simulation year. When Provider_Demand_Option is 1,2, or 3, this is the calculated GPCD used to estimate demand.  
            ///		   if the Provider_Demand_Option =4, this is the GPCD specified by Use_GPCD parameter. Units: gpcd-gallons per capita per daya of water use</remarks>
            /// <seealso cref="Provider_Demand_Option"/>
            /// <seealso cref="Use_GPCD"/>
            public providerArrayProperty GPCD_Res;

            //---------------------------------------
            int[] GPCD_Difference = new int[ProviderClass.NumberOfProviders];

            //ProviderGPCD commercial & Industrial
            // 06.19.15 DAS
            private int[] get_GPCD_ComInd()
            {
                for (int p = 0; p < ProviderClass.NumberOfProviders; p++)
                {
                    GPCD_Difference[p] = _ws.ProviderGPCD[p] - _ws.get_ProviderGPCDres[p];
                }
                return GPCD_Difference;
            }

            /// <summary> The gpcd used </summary>
            ///<remarks>The GPCD used to estimate demand for the completed simulation year. When Provider_Demand_Option is 1,2, or 3, this is the calculated GPCD used to estimate demand.  
            ///		   if the Provider_Demand_Option =4, this is the GPCD specified by Use_GPCD parameter. Units: gpcd-gallons per capita per daya of water use</remarks>
            /// <seealso cref="Provider_Demand_Option"/>
            /// <seealso cref="Use_GPCD"/>
            public providerArrayProperty GPCD_ComInd;



            //---------------------------------------
            //Gross Ag water Pumped
            // 01.29.15 DAS
            private int[] get_Gross_Ag_pumped()
            { return _ws.get_GrossAgricultureWaterPumped_AF; }

            /// <summary> The pumped agriculture water </summary>
            ///<remarks>Depends on the  Web_AgricultureTransferToMuni_PCT variable which
            /// controls the  _ws.set_AgPumpingCurveIndex. The index sets the curve for Ag pumping
            /// See the index for more info
            /// </remarks>
            /// <seealso cref="Provider_Demand_Option"/>
            /// <seealso cref="Use_GPCD"/>
            public providerArrayProperty Gross_Agriculture_WaterPumped_AF;


            int[] SRP_pump = new int[ProviderClass.NumberOfProviders];
            int[] SRP_total = new int[ProviderClass.NumberOfProviders];
            int[] SRP_surf = new int[ProviderClass.NumberOfProviders];

            public providerArrayProperty SaltVerde_Annual_SurfaceDeliveries_SRP;
            private int[] get_SRP_SurfaceDeliveries()
            {
                for (int p = 0; p < ProviderClass.NumberOfProviders; p++)
                {
                    SRP_pump[p] = _ws.get_ClassBCpumpedSRP[p];
                    SRP_total[p] = _ws.get_SVTAnnualDeliveriesSRP[p];
                    SRP_surf[p] = Math.Max(0, SRP_total[p] - SRP_pump[p]);

                }

                // return SRP_surf;
                return SRP_surf;
            }  


        // end DAS 06.02.16

        #endregion

        //==========================================
        // Gets and Sets for Base input and out Properties
        //============================================
                
        #region Gets And Sets for Base Inputs/outputs

               
        // gets for Base outputs
              
        private int geti_Colorado_River_Flow() { return Colorado_River_Flow; }
        private int geti_Powell_Storage() { return Powell_Storage; }
        private int geti_Mead_Storage() { return Mead_Storage; }
        private int geti_SaltVerde_River_Flow() { return SaltVerde_River_Flow; }
        private int geti_SaltTonto_River_Flow() { return SaltTonto_River_Flow; }
        private int geti_Verde_River_Flow() { return Verde_River_Flow; }

        private int geti_SaltVerde_Storage() { return SaltVerde_Storage; }
        private int geti_Effluent_To_Agriculture() { return Effluent_To_Agriculture; }
        internal int geti_SVTspillage() { return SVT_Spillage; }
        internal int geti_ElevationMead() { return Elevation_of_Mead; }
        //DAS
        internal int geti_ElevationPowell() { return Elevation_of_Powell; }
        private int geti_SaltOther_Storage() { return SaltOther_Storage; }
        private int geti_Roosevelt_Storage() { return Roosevelt_Storage; }
        private int geti_Verde_Storage() { return Verde_Storage; }
        private int geti_Salt_Storage() { return Salt_Storage; }

        internal int geti_Regional_Natural_Recharge() { return Regional_Natural_Recharge; }
        internal int geti_Regional_CAGRD_Recharge() { return Regional_CAGRD_Recharge; }
        internal int geti_Regional_Inflow() { return Regional_Inflow; }
        internal int geti_Regional_Ag_To_Vadose() { return Regional_Ag_To_Vadose;  }
        internal int geti_Regional_Provider_Recharge(){ return Regional_Provider_Recharge; } 
        internal int geti_Regional_Ag_Other_Pumping(){ return Regional_Ag_Other_Pumping;  }
        internal int geti_Regional_Outflow() { return Regional_Outflow;  }
        internal int geti_Regional_Groundwater_Balance() { return Regional_Groundwater_Balance;  }


        // Gets for Base Inputs
        //private int geti_Simulation_Start_Year() { return Simulation_Start_Year; }
        //private int geti_Simulation_End_Year() { return Simulation_End_Year; }
        private int geti_Colorado_Historical_Extraction_Start_Year() { return Colorado_Historical_Extraction_Start_Year; }
        private int geti_Colorado_Historical_Data_Source() { return Colorado_Historical_Data_Source; }
        private int geti_Colorado_Climate_Adjustment_Percent() { return Colorado_Climate_Adjustment_Percent; }
        private int geti_Colorado_User_Adjustment_Percent() { return Colorado_User_Adjustment_Percent; }
        private int geti_Colorado_User_Adjustment_StartYear() { return Colorado_User_Adjustment_StartYear; }
        private int geti_Colorado_User_Adjustment_Stop_Year() { return Colorado_User_Adjustment_Stop_Year; }
        private int geti_SaltVerde_Historical_Extraction_Start_Year() { return SaltVerde_Historical_Extraction_Start_Year; }
        private int geti_SaltVerde_Historical_Data() { return SaltVerde_Historical_Data_Source; }
        private int geti_SaltVerde_Climate_Adjustment_Percent() { return SaltVerde_Climate_Adjustment_Percent; }
        private int geti_SaltVerde_User_Adjustment_Percent() { return SaltVerde_User_Adjustment_Percent; }
        private int geti_SaltVerde_User_Adjustment_Start_Year() { return SaltVerde_User_Adjustment_Start_Year; }
        private int geti_SaltVerde_User_Adjustment_Stop_Year() { return SaltVerde_User_Adjustment_Stop_Year; }
        private int geti_Provider_Demand_Option() { return Provider_Demand_Option; }
        private int geti_PCT_Reduce_GPCD() { return PCT_Reduce_GPCD; }
        private int geti_Assured_Water_Supply_Annual_Groundwater_Pumping_Limit()
        {
            if (_Assured_Water_Supply_Annual_Groundwater_Pumping_Limit) return 1;
            else return 0;
        }
        // QUAY Changed 3 7 13 removing Model_Interface PopulationGrowthRateAdjustmentPercent to use Provider Growth Rates
        private int geti_PCT_REG_Growth_Rate_Adjustment()
        {
            int Total = 0;
            // Get the On Project Growth Provider array
            int[] GrowthRates = _ws.ProviderPopGrowthRateOnProjectPct;
            // add these up
            foreach (int value in GrowthRates)
                Total += value;
            // get off project growth provider array
            GrowthRates = _ws.ProviderPopGrowthRateOtherPct;
            // continue adding
            foreach (int value in GrowthRates)
                Total += value;
            // now calculate an un weighted average (since we do not no population of each provider at this point)
            int AvgRate = Total / (GrowthRates.Length*2);
            return AvgRate;
        }
        // DAS
        private int geti_API_Default_Status() { return API_Default_Status; }
        // DAS 01.26.16
        private int geti_API_Drought_Scenario() { return API_Drought_Scenario; }
        //
        private int geti_COdeltaBurdenForAZ() { return Convert.ToInt32(COdeltaBurdenToAZ); }
        //
        private int geti_COdeltaBurdenRatioForAZ() { return COdeltaBurdenRatioForAZ; }
        //
        private int geti_BaseYear() { return _BaseYear; }
        // 10.03.16
        // Ray/Dave White want to have preset scenarios for the User Interface. This parameter
        // gets the numeric int associated with the pre-set (pre-defined) scenario
        private int geti_API_Scenario_Presets() { return API_Scenario_Presets; }

        //***************
      
        // Sets for Base Inputs 
        //private void seti_Simulation_Start_Year(int value) { Simulation_Start_Year = value; }
        //private void seti_Simulation_End_Year(int value) { Simulation_End_Year = value; }
        private void seti_Colorado_Historical_Extraction_Start_Year(int value) { Colorado_Historical_Extraction_Start_Year = value; }
        private void seti_Colorado_Historical_Data_Source(int value) { Colorado_Historical_Data_Source = value; }
        private void seti_Colorado_Climate_Adjustment_Percent(int value) { Colorado_Climate_Adjustment_Percent = value; }
        private void seti_Colorado_User_Adjustment_Percent(int value) { Colorado_User_Adjustment_Percent = value; }
        private void seti_Colorado_User_Adjustment_StartYear(int value) { Colorado_User_Adjustment_StartYear = value; }
        private void seti_Colorado_User_Adjustment_Stop_Year(int value) { Colorado_User_Adjustment_Stop_Year = value; }
        private void seti_SaltVerde_Historical_Extraction_Start_Year(int value) { SaltVerde_Historical_Extraction_Start_Year = value; }
        private void seti_SaltVerde_Historical_Data(int value) { SaltVerde_Historical_Data_Source = value; }
        private void seti_SaltVerde_Climate_Adjustment_Percent(int value) { SaltVerde_Climate_Adjustment_Percent = value; }
        private void seti_SaltVerde_User_Adjustment_Percent(int value) { SaltVerde_User_Adjustment_Percent = value; }
        private void seti_SaltVerde_User_Adjustment_Start_Year(int value) { SaltVerde_User_Adjustment_Start_Year = value; }
        private void seti_SaltVerde_User_Adjustment_Stop_Year(int value) { SaltVerde_User_Adjustment_Stop_Year = value; }
        private void seti_Provider_Demand_Option(int value) { Provider_Demand_Option = value; }
        private void seti_PCT_Reduce_GPCD(int value) { PCT_Reduce_GPCD = value; }
        // QUAY Modified 3 7 13 to Remove old model Regional Growth Adjust
        private void seti_PCT_REG_Growth_Rate_Adjustment(int value)
        {
            if (!FModelLocked)
            {
                int[] NewRates = new int[ProviderClass.NumberOfProviders];
                for (int i = 0; i < NewRates.Length; i++)
                    NewRates[i] = value;
                _ws.ProviderPopGrowthRateOnProjectPct = NewRates;
                _ws.ProviderPopGrowthRateOtherPct = NewRates;
            }
        }
        //************
        private void seti_Assured_Water_Supply_Annual_Groundwater_Pumping_Limit(int value)
        {
            bool LimitToggle = false;
            if (value > 0)
            {
                LimitToggle = true;
            }
            else
                LimitToggle = false;
            _ws.set_parmAllStrawsSucking = LimitToggle;
            _Assured_Water_Supply_Annual_Groundwater_Pumping_Limit = LimitToggle;
        }
        //
        // DAS drought
        private void seti_API_Default_Status(int value) { API_Default_Status = value; }

        // 01.19.16
        internal string chosenDrought = "";
        internal void seti_API_Drought_Scenario(int value)
        {
            //Drought_Scenario = value;
            API_Drought_Scenario = value;
            Colorado_User_Adjustment_StartYear = 2013;
            SaltVerde_User_Adjustment_Start_Year = 2013;

            switch (value)
            {

                case 0:
                    chosenDrought = "No Drought";
                    //Colorado_Climate_Adjustment_Percent = 100;
                    //SaltVerde_Climate_Adjustment_Percent = 100;
                    break;

                case 1:
                    chosenDrought = "Abnornally Dry";
                    Colorado_User_Adjustment_Stop_Year = 2020;
                    SaltVerde_User_Adjustment_Stop_Year = 2020;
                    Colorado_User_Adjustment_Percent = 95;
                    SaltVerde_User_Adjustment_Percent = 85;

                    //Colorado_Climate_Adjustment_Percent = 95;
                    //SaltVerde_Climate_Adjustment_Percent = 90;
                    break;
                case 2:
                    chosenDrought = "Moderate";
                    Colorado_User_Adjustment_Stop_Year = 2030;
                    SaltVerde_User_Adjustment_Stop_Year = 2030;
                    Colorado_User_Adjustment_Percent = 85;
                    SaltVerde_User_Adjustment_Percent = 70;

                    //Colorado_Climate_Adjustment_Percent = 93;
                    //SaltVerde_Climate_Adjustment_Percent = 80;


                    break;
                case 3:
                    chosenDrought = "Extreme";
                    Colorado_User_Adjustment_Stop_Year = 2040;
                    SaltVerde_User_Adjustment_Stop_Year = 2040;
                    Colorado_User_Adjustment_Percent = 60;
                    SaltVerde_User_Adjustment_Percent = 35;

                    //Colorado_Climate_Adjustment_Percent = 89;
                    //SaltVerde_Climate_Adjustment_Percent = 60;

                    break;

                case 4:
                    chosenDrought = "Exceptional";
                    Colorado_User_Adjustment_Stop_Year = 2050;
                    SaltVerde_User_Adjustment_Stop_Year = 2050;
                    Colorado_User_Adjustment_Percent = 50;
                    SaltVerde_User_Adjustment_Percent = 25;

                    //Colorado_Climate_Adjustment_Percent = 85;
                    //SaltVerde_Climate_Adjustment_Percent = 40;

                    break;

                default:
                    chosenDrought = "Moderate";
                    Colorado_User_Adjustment_Stop_Year = 2030;
                    SaltVerde_User_Adjustment_Stop_Year = 2030;

                    break;


            }
        }
        //
        private void seti_COdeltaBurdenForAZ(int value)
        {
            COdeltaBurdenToAZint = value;
            COdeltaBurdenToAZ = Convert.ToBoolean(COdeltaBurdenToAZint);
        }
        //
        private void seti_COdeltaBurdenRatioForAZ(int value)
        {
            COdeltaBurdenRatioForAZ = value;
        }
        private void seti_BaseYear(int value) { BaseYear = value; }


        //
        ModelParameterClass TM_AgTransferToMuni;
        ModelParameterClass TM_ReclaimedUse;
        ModelParameterClass TM_ROuse;
        ModelParameterClass TM_ROtoWaterSupply;
        ModelParameterClass TS_CODeltaWater;
        ModelParameterClass TS_Growth;
        ModelParameterClass TS_Conservation;

        // 10.03.16 DAS
        // Preset Scenarios for Dave White
        internal string chosenScenario = "";
        internal void seti_API_Scenario_Presets(int value)
        {
            TM_AgTransferToMuni = this.ParamManager.Model_Parameter(eModelParam.epWebUIAgriculture);
            TM_ReclaimedUse = this.ParamManager.Model_Parameter(eModelParam.epWebReclaimedWater_PCT);
            TM_ROuse = this.ParamManager.Model_Parameter(eModelParam.epPCT_Reclaimed_to_RO);
            TM_ROtoWaterSupply = this.ParamManager.Model_Parameter(eModelParam.epPCT_RO_to_Water_Supply);
            //
            TS_CODeltaWater = this.ParamManager.Model_Parameter(eModelParam.epEnvironmentalFlow_PCT);
            TS_Growth = this.ParamManager.Model_Parameter(eModelParam.epWebPop_GrowthRateAdj_PCT);
            TS_Conservation = this.ParamManager.Model_Parameter(eModelParam.epWebUIPersonal_PCT);
            //

            //
            API_Scenario_Presets = value;
 
            switch (value)
            {

                case 0:
                    chosenScenario = "No Preset";
                    break;

                case 1:
                    chosenScenario = "Techanical Management";
                    TM_AgTransferToMuni.Value = 50;
                    TM_ReclaimedUse.Value = 100;
                    TM_ROuse.Value = 100;
                    TM_ROtoWaterSupply.Value = 90;
                     break;
                case 2:
                     chosenScenario = "Water Security";

                    break;
                case 3:
                    chosenScenario = "New Sustainability";
                    TS_CODeltaWater.Value = 45;
                    TM_AgTransferToMuni.Value = 25;
                    TS_Growth.Value = 85;
                    TS_Conservation.Value = 65;
                    break;

                case 4:
                    chosenScenario = "Demand Management";
                    TS_Growth.Value = 75;
                    TS_Conservation.Value = 55;
                    break;

                default:
                    chosenScenario = "No preset- default";

                    break;


            }
        }









        #endregion

        // Special 
       
        /// <summary>   Fast get annual data.</summary>
        /// <param name="AS_Results">   [out] annual results all output parameters of Simulation. </param>
       
        public void FastGetAnnualData(ref AnnualSimulationResults AS_Results)
        {
            ModelParameterBaseClass MP;
            int ib_index = 0;
            int ob_index = 0;
            int ip_index = 0;
            int op_index = 0;
       
            foreach (int emp in _pm.eModelParameters())
            {
                MP = _pm.Model_ParameterBaseClass(emp);
                switch (MP.ParamType)
                {
                    case modelParamtype.mptInputBase :
                        AS_Results.Inputs.BaseInput[ib_index] = (MP as ModelParameterClass).Value;  // 7/29 (MP as BaseModelParameterClass).Value;
                        AS_Results.Inputs.BaseInputModelParam[ib_index] = MP.ModelParam;
                        //AS_Results.BaseInput[ib_index] = MP.Value;
                        ib_index++;
                        break;
                    case modelParamtype.mptInputProvider:;
                        AS_Results.Inputs.ProviderInput[ip_index] = MP.ProviderProperty.getvalues();
                        AS_Results.Inputs.ProviderInputModelParam[ip_index] = MP.ModelParam;
                        // AS_Results.ProviderInput.Values[ip_index] = MP.ProviderProperty.getvalues();;
                        ip_index++;
                        break;
                    case modelParamtype.mptOutputBase:
                        AS_Results.Outputs.BaseOutput[ib_index] = (MP as ModelParameterClass).Value;  // 7/29 (MP as BaseModelParameterClass).Value;
                        AS_Results.Outputs.BaseOutputModelParam[ib_index] = MP.ModelParam;
//                        AS_Results.BaseOutput[ob_index] = MP.Value;
                        ob_index++;
                        break;
                    case modelParamtype.mptOutputProvider:
                        AS_Results.Outputs.ProviderOutput[ip_index] = MP.ProviderProperty.getvalues();
                        AS_Results.Outputs.ProviderOutputModelParam[ip_index] = MP.ModelParam;
                        //AS_Results.ProviderOutput.Values[op_index] = MP.ProviderProperty.getvalues();
                        op_index++;
                        break;
       
                }
            }
        }
        // SPecial Range Checks
       
        /// <summary> River range.  </summary>
        /// <remarks> a struct used to define the range in years of a river's flow record</remarks>
        public  class RiverRange
        {
       
            /// <summary> The first </summary>
            protected int _First = 0;
       
            /// <summary> The last </summary>
            protected int _Last = 0;
        
            /// <summary> First year of record </summary>
            /// <value>a year</value>
            public int First 
            { get { return _First; } }
       
            /// <summary> Last year of record </summary>
            /// <value>a year</value>
            public int Last
            { get { return _Last;} }
       
            internal RiverRange(int first, int last)
            {
                _First = first;
                _Last = last;
            }
        }
       
        /// <summary>   User river range.  </summary>
        /// <remarks>   A struct used for defining the range of user supplied river flow records. </remarks>
       
        public class UserRiverRange : RiverRange
        {
               
            /// <summary> First year of record </summary>
            /// <value>a year</value>
            new public int First 
            { 
                get { return _First; }
                set { _First = value; }
            }
       
            /// <summary> Last year of record </summary>
            /// <value>a year</value>
            new public int Last
            { 
                get { return _Last;}
                set { _Last = value; }
            }
            internal UserRiverRange(int first, int last) : base(first,last) {}
        }
       
        /// <summary> The Colorado River Paleo River Range</summary>
        /// <seealso cref="RiverRange"/>
        public static RiverRange ColoradoPaleo = new RiverRange(762, 1982);
       
        /// <summary> The Colorado River Bureau/Historical River Range </summary>
        /// <seealso cref="RiverRange"/>
        public static RiverRange ColoradoBureau = new RiverRange(1906, 1982);
       
       
        /// <summary> The Salt Verde Rivers Paleo/Tree Ring River Range </summary>
        /// <seealso cref="RiverRange"/>
        public static RiverRange SaltVerdePaleo = new RiverRange(1330, 1982);
       
        /// <summary> The Salt Verde Bureau/Historical River Range </summary>
        /// <seealso cref="RiverRange"/>
        public static RiverRange SaltVerdeBureau = new RiverRange(1945, 1982);
       
       
        /// <summary> Colorado river range for user supplied data</summary>
        /// <seealso cref="UserRiverRange"/>
        public static UserRiverRange ColoradoUser = new UserRiverRange(1906, 2005);
       
        /// <summary> Salt Verde river range for user supplied data</summary>
        /// <seealso cref="UserRiverRange"/>
        public static UserRiverRange SaltVerdeUser = new UserRiverRange(1945, 2005);
       
       
        /// <summary> Constant for Tree Ring Paleo River Record source </summary>
        /// <value> Riversource </value>
        public const int rsPaleosource = 2;
        /// <summary> Constant for Bureau Historical River Record source </summary>
        /// <value> Riversource </value>
        public const int rsBureausource = 1;
        /// <summary> Constant for User Riversource </summary>
        /// <value> Riversource </value>
        public const int rsUsersource = 3;
        /// <summary> Source of River Trace Information. </summary>
        public static string[] SourceString = new string[] { "Source = 0", "Bureau Records", "Paleo Records", "User Records", "Source = 4", "Source = 5", "Source =6" };
       
        internal const int rcColorado = 1;
        internal const int rcSaltVerde = 2;
        // string[] RiverString = new string[] {"River = 0","Colorado River","Salt and Verde Rivers"};
        //---------------------------------------------------------------------
        internal void ColoradoRiverHistoricalReSetStart(int source)
        {
            int First = 0; int Last = 0;
            int start = ParamManager.Model_Parameter(eModelParam.epColorado_Historical_Extraction_Start_Year).Value; // 7/29 ParamManager.BaseModel_ParameterBaseClass(eModelParam.epColorado_Historical_Extraction_Start_Year).Value;
            if (!RiverHistoricalYearRangeCheck(start,source,rcColorado, ref First, ref Last))
            {
                ParamManager.Model_Parameter(eModelParam.epColorado_Historical_Extraction_Start_Year).Value = First; // 7/29 ParamManager.BaseModel_ParameterBaseClass(eModelParam.epColorado_Historical_Extraction_Start_Year).Value = First;
                ParamManager.Model_Parameter(eModelParam.epColorado_Historical_Extraction_Start_Year).EvokeReloadEvent(); // 7/29 ParamManager.BaseModel_ParameterBaseClass(eModelParam.epColorado_Historical_Extraction_Start_Year).EvokeReloadEvent();
            }
        }
        //---------------------------------------------------------------------
        internal void SaltVerdeRiverHistoricalReSetStart(int source)
        {
            int First = 0; int Last = 0;
            int start = ParamManager.Model_Parameter(eModelParam.epSaltVerde_Historical_Extraction_Start_Year).Value;  // 7/29 ParamManager.BaseModel_ParameterBaseClass(eModelParam.epSaltVerde_Historical_Extraction_Start_Year).Value;
            if (!RiverHistoricalYearRangeCheck(start, source, rcSaltVerde , ref First, ref Last))
            {
                ParamManager.Model_Parameter(eModelParam.epSaltVerde_Historical_Extraction_Start_Year).Value = First;  // 7/ 29 ParamManager.BaseModel_ParameterBaseClass(eModelParam.epSaltVerde_Historical_Extraction_Start_Year).Value = First;
                ParamManager.Model_Parameter(eModelParam.epSaltVerde_Historical_Extraction_Start_Year).EvokeReloadEvent();  // 7/29  ParamManager.BaseModel_ParameterBaseClass(eModelParam.epSaltVerde_Historical_Extraction_Start_Year).EvokeReloadEvent();
            }
        }
        //-----------------------------------------------
        internal static string RiverRangeString(int Source, int First, int Last)
        {
            string rrstr = "";
            if ((Source > 0) & (Source < SourceString.Length))
                rrstr = ": " + SourceString[Source] + " (value = " + Source.ToString() + ") start " + First.ToString() + "and end " + Last.ToString();
            return rrstr;
        }
        //-----------------------------------------------
        internal static bool RiverHistoricalYearRangeCheck(int Value, int source, int River, ref int FirstYear, ref int LastYear)
        {
            int First = 0;
            int Last = 0;
            switch (River)
            {
                //------------------------------------
                case 1:
                    switch (source)
                    {
                        case rsPaleosource:
                            First = ColoradoPaleo.First;
                            Last = ColoradoPaleo.Last;
                            break;
                        //------------------
                        case rsBureausource:
                            First = ColoradoBureau.First;
                            Last = ColoradoBureau.Last;
                            break;
                        //------------------
                        case rsUsersource:
                            First = ColoradoUser.First;
                            Last = ColoradoUser.Last;
                            break;
                        //------------------
                        default:
                            break;
                        //------------------
                    } // swithc case 1 source
                    break;
                //------------------------------------
                case 2:
                    switch (source)
                    {
                        case rsPaleosource:
                            First = SaltVerdePaleo.First;
                            Last = SaltVerdePaleo.Last;
                            break;
                        //------------------
                        case rsBureausource:
                            First = SaltVerdeBureau.First;
                            Last = SaltVerdeBureau.Last;
                            break;
                        //------------------
                        case rsUsersource:
                            First = SaltVerdeUser.First;
                            Last = SaltVerdeUser.Last;
                            break;
                        //------------------
                        default:  
                            break;
                        //------------------
                    } // switch case 2 source
                    break;
                //------------------------------------
                default:
                    break;
            } // switch source
            FirstYear = First;
            LastYear = Last;
            return  ((Value >= First) & (Value <= Last));
        }
        //-----------------------------------------------
       
        //-----------------------------------------------
        /// <summary>
        /// ColoradoYearRangeCheck
        /// </summary>
        /// <param name="Value">year</param>
        /// <param name="errMessage"></param>
        /// <param name="aModelParameter"></param>
        /// <returns></returns>
        internal bool ColoradoYearRangeCheck(int Value, ref string errMessage, ModelParameterBaseClass aModelParameter)
        {
            if ((Value == SpecialValues.MissingIntValue) && (errMessage.ToUpper() == "INFO"))
            {
                ModelParameterGroupClass GroupCodes = new ModelParameterGroupClass("SaltVerde Range", new  int[2] {eModelParam.epColorado_Historical_Data_Source, aModelParameter.ModelParam} );

                errMessage = BuildModelParameterGroupFieldList(aModelParameter.ParameterManager, GroupCodes, "Different Range for each source");
                return true;
            }
            else
            {
                bool valid = false;
                errMessage = "";
                int First = 0;
                int Last = 0; int source = aModelParameter.ParameterManager.Model_Parameter(eModelParam.epColorado_Historical_Data_Source).Value; // Colorado_Historical_Data_Source;  // 7/29 int Last = 0; int source = aModelParameter.ParameterManager.BaseModel_ParameterBaseClass(eModelParam.epColorado_Historical_Data_Source).Value;
                valid = RiverHistoricalYearRangeCheck(Value, source, rcColorado, ref First, ref Last);
                if (!valid) errMessage = "Invalid year of " + Value.ToString() + RiverRangeString(source, First, Last);
                return valid;
            }
        }
        //-----------------------------------------------
        /// <summary>
        /// Colorado_Historical_Extraction_Start_Year_RangeCheck
        /// </summary>
        /// <param name="Year">year to check</param>
        /// <returns>true if year is valid, false otherwise </returns>
        public bool Colorado_Historical_Extraction_Start_Year_RangeCheck(int Year)
        {
            string junk = "";
            return ColoradoYearRangeCheck(Year, ref junk, ParamManager.Model_ParameterBaseClass(eModelParam.epColorado_Historical_Extraction_Start_Year));
        }
        //-----------------------------------------------
        internal bool SaltVerdeYearRangeCheck(int Value, ref string errMessage, ModelParameterBaseClass aModelParameter)
        {
            if ((Value == SpecialValues.MissingIntValue) && (errMessage.ToUpper() == "INFO"))
            {
                ModelParameterGroupClass GroupCodes = new ModelParameterGroupClass("Salt Verde Range", new  int[2] {eModelParam.epSaltVerde_Historical_Data,aModelParameter.ModelParam});
                errMessage = BuildModelParameterGroupFieldList(aModelParameter.ParameterManager, GroupCodes, "Different Range for each source");
                return true;
            }
            else
            {
                bool valid = false;
                errMessage = "";
                int source = aModelParameter.ParameterManager.Model_Parameter(eModelParam.epSaltVerde_Historical_Data).Value;// SaltVerde_Historical_Data_Source;   // 7/29 aModelParameter.ParameterManager.BaseModel_ParameterBaseClass(eModelParam.epSaltVerde_Historical_Data).Value;
                int First = 0;
                int Last = 0;
                valid = RiverHistoricalYearRangeCheck(Value, source, rcSaltVerde, ref First, ref Last);
                if (!valid) errMessage = "Invalid year of " + Value.ToString() + RiverRangeString(source, First, Last);
                return valid;
            }
        }
        //-----------------------------------------------
        /// <summary>
        /// SaltVerde_Historical_Extraction_Start_Year_RangeCheck
        /// </summary>
        /// <param name="Value"></param>
        /// <returns></returns>
        public bool SaltVerde_Historical_Extraction_Start_Year_RangeCheck(int Value)
        {
            string junk = "";
            return SaltVerdeYearRangeCheck(Value, ref junk, ParamManager.Model_ParameterBaseClass(eModelParam.epSaltVerde_Historical_Extraction_Start_Year));
        }
        //-----------------------------------------------
        //------------------------------------------------
        static internal string BuildModelParameterGroupFieldList(ParameterManagerClass PM,  ModelParameterGroupClass Group, string Rule)
        {
            string temp = "{\"ParameterGroup\":[";
            int cnt = 0;
            foreach (int mpcode in Group.ModelParameters())
            {
                try
                {
                    ModelParameterClass MP = PM.Model_Parameter(mpcode);
                    if (MP != null)
                    {
                        string fldstr = MP.Fieldname;
                        if (cnt > 0)
                        {
                            temp += ",";
                        }
                        temp += '\"' + fldstr + '\"';
                        cnt++;
                    }
                }
                catch
                {
                }
            }
            temp += "], \"RULE\":\""+Rule+"\"}";
            return temp;
        }

        //-----------------------------------------------
        /// <summary>
        /// PCTEffluentRangeCheck
        /// </summary>
        /// <param name="Value"></param>
        /// <param name="provider"></param>
        /// <param name="emp"></param>
        /// <returns></returns>
        public bool PCTEffluentRangeCheck(int Value, eProvider provider, int emp)
        {
            bool test = false;
            string junk = "";
            if (ModelParamClass.valid(emp))
            {
            ModelParameterBaseClass MP = ParamManager.Model_ParameterBaseClass(emp);
            test = PCTEffluentRangeCheck(Value, provider, ref junk, MP);
            }
            else
                throw new WaterSim_Exception(WS_Strings.wsInvalid_EModelPAram);
            return test;
        }
        //-----------------------------------------------
       
        /// <summary>PCTEffluentRangeCheck. </summary>
        ///
        /// <remarks>   Ray, 8/20/2011. </remarks>
        ///
        /// <param name="Value">        . </param>
        /// <param name="provider">     . </param>
        /// <param name="errMessage">   [in,out]. </param>
        /// <param name="MP">           The. </param>
        ///
        /// <returns>   true if it succeeds, false if it fails. </returns>
        internal  bool PCTEffluentRangeCheck(int Value, eProvider provider, ref string errMessage, ModelParameterBaseClass MP)
        {
            if ((Value == SpecialValues.MissingIntValue) && (errMessage.ToUpper() == "INFO"))
            {
                errMessage = BuildModelParameterGroupFieldList(MP.ParameterManager,EffluentGroup,"Equal 100");
                return true;
            }
            else
            {
                bool valid = true;
                if (!FSuspendRangeCheck)
                {
                    ProviderIntArray currentvalues = new ProviderIntArray(0);
                    currentvalues = MP.ProviderProperty.getvalues();
                    ProviderIntArray CTotals = EffluentGroup.Totals;
                    int currenttotal = CTotals[provider];
                    int total = currenttotal + Value - currentvalues[(int)provider];
                    valid = ((Value <= 100) & (Value >= 0) & (total <= 100));
                    if (!valid) errMessage = "Total effluent allocated can not exceed 100 (%) " +
                        MP.ParameterManager.eParamGroupMessage(EffluentGroup, provider);
                }
                return valid;
            }
        }
        //-----------------------------------------------
        /// <summary>
        /// PCTReclaimedRangeCheck - Check to see of all the PCT reclaimed model paramters do nt eceeed 100% total
        /// </summary>
        /// <param name="Value">int: to be checked if violates over 100% rule</param>
        /// <param name="provider">eProvider: who will set the value</param>
        /// <param name="emp">eModelParam: This is the actual ModelParameter that is doing the check.</param>
        /// <returns>bool True if all values are less than or equal to 100, False of the do not</returns>
        public bool PCTReclaimedRangeCheck(int Value, eProvider provider, int emp)
        {
            bool test = false;
            string junk = "";
            if (ModelParamClass.valid(emp))
            {
                ModelParameterBaseClass MP = ParamManager.Model_ParameterBaseClass(emp);
                test = PCTReclaimedRangeCheck(Value, provider, ref junk, MP);
            }
            else
                throw new WaterSim_Exception(WS_Strings.wsInvalid_EModelPAram);
            return test;
        }
        //-----------------------------------------------------------------------
        internal bool PCTReclaimedRangeCheck(int Value, eProvider provider, ref string errMessage, ModelParameterBaseClass MP)
        {
            if ((Value == SpecialValues.MissingIntValue) && (errMessage.ToUpper() == "INFO"))
            {
                errMessage = BuildModelParameterGroupFieldList(MP.ParameterManager, ReclaimedGroup, "Equal 100");
                return true;
            }
            else
            {
                bool valid = true;
                if (!FSuspendRangeCheck)
                {
                    //int[] fuckingshit =  new int[10];
                    ProviderIntArray currentvalues = new ProviderIntArray(0);
                    currentvalues = MP.ProviderProperty.getvalues();
                    int currenttotal = MP.ParameterManager.eParamGroupTotal(ReclaimedGroup, provider);  //MP.ParameterManager.reclaimedchecklist, provider);
                    int total = currenttotal + Value - currentvalues[(int)provider];
                    valid = ((Value <= 100) & (Value >= 0) & (total <= 100));
                    if (!valid) errMessage = "Total reclaimed allocated can not exceed 100 (%) " +
                        MP.ParameterManager.eParamGroupMessage(ReclaimedGroup, provider);
                }
                return valid;
            }
        }
        //-----------------------------------------------
        /// <summary>
        /// PCTResComRangeCheck - Checks to see if Residential an Commerical water use PCT does not excede 100.
        /// </summary>
        /// <param name="Value">int: to be checked if violates over 100% rule</param>
        /// <param name="provider">eProvider: who will set the value</param>
        /// <param name="emp">eModelParam: This is the actual ModelParameter that is doing the check.</param>
        /// <returns>bool True if all values are less than or equal to 100, False of the do not</returns>
        public bool PCTResComRangeCheck(int Value, eProvider provider, int emp)
        {
            bool test = false;
            string junk = "";
            if (ModelParamClass.valid(emp))
            {
                ModelParameterBaseClass MP = ParamManager.Model_ParameterBaseClass(emp);
                test = ResComPCTRangeCheck(Value, provider, ref junk, MP);
            }
            else
                throw new WaterSim_Exception(WS_Strings.wsInvalid_EModelPAram);
            return test;
        }
               
        //-----------------------------------------------
              
        internal bool ResComPCTRangeCheck(int Value, eProvider provider, ref string errMessage, ModelParameterBaseClass MP)
        {
            if ((Value == SpecialValues.MissingIntValue) && (errMessage.ToUpper() == "INFO"))
            {
                errMessage = BuildModelParameterGroupFieldList(MP.ParameterManager, WaterUseGroup, "Equal 100");
                return true;
            }
            else
            {
                bool valid = true;
                if (!FSuspendRangeCheck)
                {
                    ProviderIntArray currentvalues = new ProviderIntArray(0);
                    currentvalues = MP.ProviderProperty.getvalues();
                    int currenttotal = MP.ParameterManager.eParamGroupTotal(WaterUseGroup, provider);
                    int total = currenttotal + Value - currentvalues[(int)provider];
                    valid = ((Value <= 100) & (Value >= 0) & (total <= 100));
                    if (!valid) errMessage = "Total water use can not exceed 100 (%) " +
                        MP.ParameterManager.eParamGroupMessage(WaterUseGroup, provider);
                }
                return valid;
            }
        }



        //------------------------------------------------------------------------
        // STATIC MEMBERS AND CONST



        //---------------------------------------------------------
        /// <summary>
        /// IEnumerable for simulationYears()
        /// </summary>
        /// <returns>a year from range of Simulation_Start_Year to Simulation_End_Year</returns>
        public IEnumerable<int> simulationYears()
        {
            for (int i = Simulation_Start_Year; i <= Simulation_End_Year; i++)
            {
                yield return i;
            }
        }
          


        #endregion
        // 
        //============================================================
    }  // end of WaterSimManager            
}  // End of WaterSimDCDC
