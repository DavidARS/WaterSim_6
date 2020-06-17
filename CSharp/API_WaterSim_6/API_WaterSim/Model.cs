using System;
using System.Collections.Generic;
//using System.Text;
using System.IO;                        // for BinaryReader

using WaterSimDCDC;                     // Model proper (constructor)
using API_WaterSim;
using StreamOutputs;
using SimpleFileManager;
// 10.08.14

namespace WaterSim
{
    public class Model : WaterSimU
    {
        #region class definitions
        /// <summary>  Tests model parameters, variables, inputs and outputs. </summary>
        /// <remarks>   David A Sampson 11/10/2015. </remarks>
        const string _APIVersion = "9.0";  // latest version of API
        string _ModelBuild = "";
        internal WaterSimManager_SIO ws;
        internal StreamWriter sw;
        internal StreamWriter swAdd;
        DateTime now = DateTime.Now;
        //
        bool stopHeader = false;

        // Variables to create scenarios
        // WaterSim 6 Scenarios
        ModelParameterClass SVTraceParm;
        ModelParameterClass COTraceParm;
        ModelParameterClass RainParm;
        public ModelParameterClass POPgrowth;
        public ModelParameterClass WatEfficiency;
        //
        ModelParameterClass AdoptionRate;
        ModelParameterClass YrsToInflection;
        // ----------------------------
        public int growth_ = 0;
        public int eff_ = 0;
        //
        internal FileOutputs FO;
        //internal FileOutputsBase FOB;
        //
        ProviderIntArray myout = new ProviderIntArray(0);
        internal int[] OneHundred = new int[ProviderClass.NumberOfProviders];
        internal int[] Misc = new int[ProviderClass.NumberOfProviders];
        // =======================================================================================================================
        // WaterSim 6 Scenarios
        // --------------------
        //bool many = true;


        //const int FCORTraceN = 3;
        //int[] COtrace = new int[FCORTraceN] { 1906, 1930, 1921 }; // 1906 = 16.638448, 1943 = 13.5908527; 1930- 1930, 1921
        ////  
        //const int FSVRTraceN = 3;
        //int[] SVtrace = new int[FSVRTraceN] { 1965, 1955, 1946 }; // 1965,1946,1955

        const int FCORTraceN = 1;
        int[] COtrace = new int[FCORTraceN] { 1938 };

        const int FSVRTraceN = 1;
        int[] SVtrace = new int[FCORTraceN] { 1946 };
        //

        // =======================================================================================================================
        const int FScenario = 7;
        int[] Scenario = new int[FScenario] { 1, 2, 3, 4, 5, 6, 7 };

        string[] sScenario = new string[FScenario] { "AD", "AF", "AH", "HHH", "EN", "ZW", "BAU" };
        //string[] sScenario = new string[FScenario] { "BAU" };
        //int RecCount = 1;

        // =======================================================================================================================
        // WaterSim 6 Scenarios

        // 04.12.19 updated
        //const int FGrowthN = 5;
        //int[] Growth = new int[FGrowthN] { 80, 90, 100, 110,120 }; // plus 130
        const int FGrowthN = 1;      //
        int[] Growth = new int[FGrowthN] { 100 };

        // =======================================================================================================================
        // WaterSim 6 Scenarios
        // roughly 1.5 % max reduction annually

        // So, 70 % is roughly the current default of watersim. So, we need to start at 70 and decrease
        // from there. 06.29.18
        // 04.12.19
        //const int FEfficiency = 5;
        //int[] Efficiency = new int[FEfficiency] {60,70, 80, 90, 100 };
        const int FEfficiency = 1;
        int[] Efficiency = new int[FEfficiency] { 100 };

        // ======================================================================================================================
        // WaterSim 6 Scenarios

        //const int FRainN = 4;
        //int[] RainFall = new int[FRainN] { 70, 85, 100, 115 };
        const int FRainN = 1;
        int[] RainFall = new int[FRainN] { 100 };
        //======================================================================================================================
        //
        //const int FAdoptN = 5;
        //int[] Adoption = new int[FAdoptN] {0,25,50,75,100};
        // Set this to zero to use default Iwaniec simulation values
        const int FAdoptN = 1;
        int[] Adoption = new int[FAdoptN] { 0};

        // ======================================================================================================================
        //const int FInflection = 5;
        //int[] Inflection = new int[FInflection] {7, 14, 21, 28, 35};
        // Set this to zero to use default Iwaniec simulation values
        
        const int FInflection = 1;
         int[] Inflection = new int[FInflection] { 0 };
        // ======================================================================================================================


        bool isSetError = false;
        string SetErrMessage = "No Error";
        //
        #endregion
        #region constructor
            /// <summary>   Constructor. </summary>
            /// <remarks>   11/10/2015. </remarks>
            /// <param name="DataDirectoryName">    Pathname of the data directory. </param>
            /// <param name="TempDirectoryName">    Pathname of the temporary directory. </param>
            public Model(string DataDirectoryName, string TempDirectoryName)
            {
                WaterSimManager.CreateModelOutputFiles = true;
                ws = new WaterSimManager_SIO(DataDirectoryName, TempDirectoryName);
                StreamW(TempDirectoryName);
                //runSimple();
                Run();
                //RunWS_5();

            }
        #endregion
        #region scenarios
            /// <summary>
            /// Run the model
            /// </summary>
            /// <param name="sw"></param>
            int[] GPCD = new int[33];
        internal int[] Check_Default_Wastewater_Reclaimed = null;
        public void Run()
            {
            //
            bool testing = false;
            bool maggie = false;
              //
               ScnGenForm();
            //
            myVariables my = new myVariables();
            //
            if (testing)
            {
                my.myComparison();
                FO = new FileOutputs(ws);

            }
            else
            {
                 my.myParameters();
                FO = new FileOutputs(ws);
            }
                 SimpleFileCopy SF = new SimpleFileCopy();
            //
            string ScnName = "SFS_BAU";
            String Filename = "SFS_BAUno.csv";
            int scen = 8 ;
            SF.copyScenarioFile(scen);
            //

            int ScnCnt = 1;
                double Count = 1;
                double traces = FCORTraceN * FSVRTraceN*FGrowthN*FRainN*FEfficiency*FAdoptN* FInflection;
                double total = traces;// *FScenario;
                sw = new System.IO.StreamWriter(Filename);
                //String File = "ByPass.txt";
                //swAdd = new System.IO.StreamWriter(File);
                ProviderIntArray New = new ProviderIntArray();
                ProviderIntArray Values = new ProviderIntArray();
                for (int i = 0; i < New.Length; i++) { New[i] = 0; }
                for (int i = 0; i < New.Length; i++) { Values[i] = 50; }

                List<string> ProcessList = new List<string>();
                ProcessList = ws.ProcessManager.ActiveProcesses;

            // testing this

            if (get_ValidModelRun == true)
            {
                // One at a time ------
                if (testing)
                {
                      //ScnName = "_ZW";
                    string ver = ws.Model_Version;
                    string buid = ws.ModelBuild;
                    ws.Simulation_Initialize(); // Base Initialization
                    //
                     Init(); // Clear API, set end year, include meteorology
                             // InitSpecial(New, Values);
                             // my.Initialize(scen, ws); // Sets the scenario parameters
                         //
                         //
                         // ==============================================
                         //
                        parmIwaniecScenariosYN = false;
                        parmWaterSim5YN = true;
                        set_parmCalculateRainWaterHarvesting = false;
                        set_parmCalculateGrayWater = false;
                        ws.Colorado_Historical_Extraction_Start_Year = 1939;
                        ws.SaltVerde_Historical_Extraction_Start_Year = 1946;
                        //
                    // ==============================================
                    Count = 1; total = 1;
                    //
                    if (maggie)
                    {
                        foreach (int rain in RainFall)
                        {
                            //if (SetParms(st, co, rain, growth, eff, adopt, yrs))
                            if (SetParmMaggie(rain))
                            {
                                string Cstring = DateTime.Now.ToString("h:mm:ss tt") + " In step: " + "--" + Count + " Of: " + total + " > " + (Count / total) * 100 + " %";
                                Console.WriteLine(Cstring);
                                Console.WriteLine("");
                                Console.WriteLine("Simulating " + scen + " " + ScnName);
                                runonly();

                            }
                            Count += 1;
                            runOutputs(scen, ScnCnt, my, sw);
                            ws.Simulation_Stop();
                        }

                        //
                        CloseFiles();

                    }
                    else
                    {
                        //
                        Check_Default_Wastewater_Reclaimed = ws.PCT_Wastewater_Reclaimed.getvalues().Values ;

                        //
                        string Cstring = DateTime.Now.ToString("h:mm:ss tt") + " In step: " + "--" + Count + " Of: " + total + " > " + (Count / total) * 100 + " %";
                        Console.WriteLine(Cstring);
                        Console.WriteLine("");
                        Console.WriteLine("Simulating " + scen + " " + ScnName);
                        runonly();
                        Count += 1;
                        runOutputs(scen, ScnCnt, my, sw);
                        ws.Simulation_Stop();

                        CloseFiles();
                    }

                }
                    else
                {
                    foreach (int st in SVtrace)
                    {
                        foreach (int co in COtrace)
                        {
                            foreach (int rain in RainFall)
                            {
                                foreach (int growth in Growth)
                                {
                                    foreach (int eff in Efficiency)
                                    {
                                        foreach (int adopt in Adoption)
                                        {
                                            foreach (int yrs in Inflection)
                                            {
                                                //string ScnName = "_ZW";
                                                RunningGrowth = growth;
                                                RunningEff = eff;
                                                string ver = ws.Model_Version;
                                                string buid = ws.ModelBuild;
                                                ws.Simulation_Initialize(); // Base Initialization
                                                //
                                                 parmWaterSim5YN = false;

                                                Init(); // Clear API, set end year, include meteorology
                                                        // InitSpecial(New, Values);
                                                if (parmWaterSim5YN)
                                                {
                                                    initWS5();
                                                }
                                                else
                                                {
                                                    my.Initialize(scen, ws); // Sets the scenario parameters
                                                }
                                                //
                                               if (SetParms(st, co, rain, growth, eff, adopt, yrs))
                                               // if (SetParmSFS(st, co, rain, growth, eff))
                                                {
                                                    string Cstring = DateTime.Now.ToString("h:mm:ss tt") + " In step: " + "--" + Count + " Of: " + total + " > " + (Count / total) * 100 + " %";
                                                    Console.WriteLine(Cstring);
                                                    Console.WriteLine("");
                                                    Console.WriteLine("Simulating " + scen + " " + ScnName);
                                                    runonly();
                                                }

                                                Count += 1;
                                                runOutputs(scen, ScnCnt, my, sw);
                                                ws.Simulation_Stop();
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                CloseFiles();
                sw.Flush();
                sw.Close();
            } // end of testing or not testing
            }
        #endregion
        #region properties
            public int RunningGrowth
            {
                get { return growth_; }
                set { growth_ = value; }
            }
            public int RunningEff
            {
                get { return eff_; }
                set {eff_ = value;}
            }
        #endregion properties
        #region simpleRun
        ProviderIntArray Out = new ProviderIntArray(0);
          ProviderIntArray CO = new ProviderIntArray(0);

          internal int[] Reduce = new int[ProviderClass.NumberOfProviders];
        void runonly()
        {
             for (int year = ws.Simulation_Start_Year; year < ws.Simulation_End_Year; ++year)
            {
                ws.Simulation_NextYear();
                StartSimulation = false;
            }
        }
        void runSimple()
        {
            myVariables my = new myVariables();
            my.myParameters();
            FO = new FileOutputs(ws);
            SimpleFileCopy SF = new SimpleFileCopy();
            int scen = 7;
            SF.copyScenarioFile(scen);
            ws.Simulation_Initialize();

            Init();
            my.Initialize(scen, ws);
            //
            // For watersim 5 simulations
            parmUseLCLU = true;
            parmIwaniecScenariosYN = true;
            parmWaterSim5YN = false;
            set_parmCalculateRainWaterHarvesting = false;
            set_parmCalculateGrayWater = false;
            ws.Colorado_Historical_Extraction_Start_Year = 1939;
            ws.SaltVerde_Historical_Extraction_Start_Year = 1946;


            for (int year = ws.Simulation_Start_Year; year < ws.Simulation_End_Year; ++year)
            {
                ws.Simulation_NextYear();
                StartSimulation = false;
            }
            CloseFiles();
        }
        void simpleRun()
        {
            myVariables my = new myVariables();
            my.myParameters();
            FO = new FileOutputs(ws);
            SimpleFileCopy SF = new SimpleFileCopy();
            int scen = 7;
            SF.copyScenarioFile(scen);
            ws.Simulation_Initialize();
            System.Threading.Thread.Sleep(3000);
            Init();
            my.Initialize(scen, ws);
            //
            // For watersim 5 simulations
            parmUseLCLU = true;
            parmIwaniecScenariosYN = true;
            //
            // 10.26.17
            ws.Simulation_End_Year = 2061;

            for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { parmWWtoRWWTPpct[i] = 100; }
            Out.Values = Reduce;
            //ws.PCT_alter_GPCD.setvalues(Out);
           ;

            ws.PCT_Wastewater_Reclaimed.setvalues(Out);
            //
            // 10.26.17
            ws.Simulation_End_Year = 2061;
            List<string> ProcessList = new List<string>();
            ProcessList = ws.ProcessManager.ActiveProcesses;
            for (int year = ws.Simulation_Start_Year; year < ws.Simulation_End_Year; ++year)
            {
                ws.Simulation_NextYear();
                StartSimulation = false;
                    sw.WriteLine(year
                       + ","               
                       + ws.PCT_Reclaimed_to_Water_Supply.RegionalValue(eProvider.Regional)
                       +","
                       + ws.Colorado_Annual_Deliveries.RegionalValue(eProvider.Regional)
                       + ","
                       + ws.SaltVerde_Annual_SurfaceDeliveries_SRP.RegionalValue(eProvider.Regional)
                       + ","
                       + ws.Groundwater_Pumped_Municipal.RegionalValue(eProvider.Regional)
                //       + ","
                //       + ws.ResidentialHighDensityOutdoorGPCD.RegionalValue(eProvider.Regional)
                //       + ","

                       );
                //}
             }
            sw.Flush();
            sw.Close();

        }
        void runCSV(int year)
        {
            swAdd.WriteLine(year
             + ","
             //+ RunningGrowth
             //+ ","
             //+ RunningEff
             //+ ","
             + ws.LowDensityDemand.RegionalValue(eProvider.Regional)
             + ","
             + ws.MediumDensityDemand.RegionalValue(eProvider.Regional)
             + ","
             + ws.HighDensityDemand.RegionalValue(eProvider.Regional)
             + ","
             + ws.TurfWaterDemand.RegionalValue(eProvider.Regional)
             + ","
             + ws.TreeWaterDemand.RegionalValue(eProvider.Regional)

                );


        }
        #endregion
        #region WaterSIm 5
        void initWS5()
        {
            parmIwaniecScenariosYN = false;
            // Climate
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
        void RunWS_5()
        {

            // 01.30.19
            ws.Simulation_End_Year = 2061;
            ws.Simulation_Initialize(); // Base Initialization
            bool test = parmIwaniecScenariosYN;
            parmIwaniecScenariosYN = false;
            parmWaterSim5YN = true;
            //
            set_parmCalculateRainWaterHarvesting = false;
            set_parmCalculateGrayWater = false;
            //
            List<string> ProcessList = new List<string>();
            ProcessList = ws.ProcessManager.ActiveProcesses;
            for (int year = ws.Simulation_Start_Year; year < ws.Simulation_End_Year; ++year)
            {
                ws.Simulation_NextYear();
                StartSimulation = false;
                sw.WriteLine(year
                   + ","
                   + ws.Regional_Groundwater_Balance
                   + ","
                   + ws. GPCD_Used.RegionalValue(eProvider.Regional)
                   + ","
                   + ws.Population_Used.RegionalValue(eProvider.Regional)
                   + ","
                   + ws.Total_Demand.RegionalValue(eProvider.Regional)
                   + ","
                   + ws.Groundwater_Pumped_Municipal.RegionalValue(eProvider.Regional)
                   + ","
                   + ws.AgriculturalDemand.RegionalValue(eProvider.Regional)
                   + ","
                   + ws.Gross_Agriculture_WaterPumped_AF.RegionalValue(eProvider.Regional)

                    );
                //}
            }
            sw.Flush();
            sw.Close();
            CloseAll();
        }
        #endregion WaterSim 5

        #region Init
        void Init()
            {
                // ========================================
                parmAPIcleared = true;
                //
                set_parmIncludeMeteorology = true;
                ws.Simulation_End_Year = 2061;


                //     getting
                 ws.API_Cleared = true;
            }
            // =====================================================================================
            void InitSpecial(ProviderIntArray one,ProviderIntArray two)
            {
                ws.PCT_Reclaimed_Outdoor_Use.setvalues(one);
                ws.PCT_Wastewater_Reclaimed.setvalues(one);
                ws.PCT_Reclaimed_to_Water_Supply.setvalues(two);
                ws.RainFallFactor = 100;
            }
            // =====================================================================================
            public bool SetParms(int SVTraceYr, int COTraceYr, int rain, int growth, int personal,int adoption, int yrs)
            //public bool SetParms(int SVTraceYr, int COTraceYr, int rain, int growth, int personal)
          {
            bool result = true;
                SVTraceParm.Value = SVTraceYr;
                COTraceParm.Value = COTraceYr;
                RainParm.Value = rain;
                POPgrowth.Value = growth;
                WatEfficiency.Value = personal;
                AdoptionRate.Value = adoption;
                YrsToInflection.Value = yrs;
                return result;
            }
        // =====================================================================================
        public bool SetParmSFS(int SVTraceYr, int COTraceYr, int rain, int growth, int personal)
        //public bool SetParms(int SVTraceYr, int COTraceYr, int rain, int growth, int personal)
        {
            bool result = true;
            SVTraceParm.Value = SVTraceYr;
            COTraceParm.Value = COTraceYr;
            RainParm.Value = rain;
            POPgrowth.Value = growth;
            WatEfficiency.Value = personal;
          
            return result;
        }
        public bool SetParmMaggie(int rain)
        //public bool SetParms(int SVTraceYr, int COTraceYr, int rain, int growth, int personal)
        {
            bool result = true;
            RainParm.Value = rain;
            return result;
        }

        // =====================================================================================
        public bool SetParmsOne(int SVTraceYr)
        {
            bool result = true;
            // testing this
            // set_parmRainwaterHarvestCompliance = 30;

            SVTraceParm.Value = SVTraceYr;
            return result;
        }
        public void ScnGenFormOne()
        {
            ws.IncludeAggregates = true;
           // AdoptionRate = ws.ParamManager.Model_Parameter(eModelParam.epRainHarvestCompliance);
         }

        // =====================================================================================
        public bool SetParm(int SVTraceYr, int COTraceYr)
            {
                bool result = true;
                SVTraceParm.Value = SVTraceYr;
                COTraceParm.Value = COTraceYr;
                return result;
            }
            // =====================================================================================

            // ========================================================================================================
            // WaterSim 6
            public void ScnGenForm()
            {
                ws.IncludeAggregates = true;
                SVTraceParm = ws.ParamManager.Model_Parameter(eModelParam.epSaltVerde_Historical_Extraction_Start_Year);
                COTraceParm = ws.ParamManager.Model_Parameter(eModelParam.epColorado_Historical_Extraction_Start_Year);
                RainParm = ws.ParamManager.Model_Parameter(eModelParam.epRainFallFactor);
                POPgrowth = ws.ParamManager.Model_Parameter(eModelParam.epWebPop_GrowthRateAdj_PCT);
                WatEfficiency = ws.ParamManager.Model_Parameter(eModelParam.epGPCDefficiencyLCLU);
                AdoptionRate = ws.ParamManager.Model_Parameter(eModelParam.epRainHarvestCompliance);
                YrsToInflection = ws.ParamManager.Model_Parameter(eModelParam.epRainHarvestInflection);
            }
        // =====================================================================================
        #endregion
        // =====================================================================================
        public void runOutputs(int i, int ScnCnt, myVariables mine, System.IO.StreamWriter SW)
        {
            string ScnName = "";
            ScnName = i + ",";
            //
            if (ScnCnt == 1)
            {
                if (stopHeader)
                {
                }
                else
                {
                    FO.WriteHeader(ws.SimulationRunResults,mine,  sw);
                }
                stopHeader = true;

            }
            if (FO.WriteResults(ws.SimulationRunResults, ScnName,mine, sw))
            {
                //ScnCnt++;

            }
            else
            {
            }
            ws.Simulation_Stop();
        }
        // =====================================================================================
        public void StreamW(string TempDirectoryName)
        {
            string filename = string.Concat(TempDirectoryName + "Output" + now.Month.ToString()
                + now.Day.ToString() + now.Minute.ToString() + now.Second.ToString()
                + "_" + ".csv");
            sw = File.AppendText(filename);
        }
        public string APiVersion { get { return _APIVersion; } }
        /// <summary>
        /// Verson of the Fortran Model
        /// </summary>
        public string ModelBuild { get { return _ModelBuild; } }
        // E.O.F.
    }
}
