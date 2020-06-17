using System;
using System.Collections.Generic;
//using System.Text;
using System.IO;                        // for BinaryReader

using WaterSimDCDC;                     // Model proper (constructor)
using API_WaterSim;
using StreamOutputs;
using SimpleFileManager;

namespace WaterSim
{
    public class Model_5 : WaterSimU
    {
        #region classdefinitions
        /// <summary>  Tests model parameters, variables, inputs and outputs. </summary>
        /// <remarks>   David A Sampson 11/10/2015. </remarks>
        const string _APIVersion = "9.0";  // latest version of API
        string _ModelBuild = "";
        internal WaterSimManager_SIO ws;
        internal StreamWriter sw;
       // internal StreamWriter swAdd;
        DateTime now = DateTime.Now;
        bool stopHeader = false;
        // WaterSim 5 Scenarios 
        ModelParameterClass SVTraceParm;
        ModelParameterClass COTraceParm;
        ModelParameterClass GPCDParm;
        ModelParameterClass PopParm;
        ModelParameterClass WBParm;
        ModelParameterClass WAugParm;
        ModelParameterClass WReuseParm;
        ModelParameterClass DroughtParmCO;
        ModelParameterClass DroughtParmSV;
        public int growth_ = 0;
        public int eff_ = 0;
        //
        internal FileOutputs FO;
        internal FileOutputsBase FOB;
        //
        ProviderIntArray myout = new ProviderIntArray(0);
        internal int[] OneHundred = new int[ProviderClass.NumberOfProviders];

        // WaterSim 5 revisited for Giuseppe Mascaro
        // -----------------------------------------------------
        const int FCORTraceN = 40;
        int[] COtrace = new int[FCORTraceN] { 1956, 1944, 1934, 1964, 1979, 1948, 1926, 1915, 1940, 1915, 1949, 1936, 1925, 1934, 1955, 1906, 1964, 1923, 1950, 1911, 1908, 1974, 1957, 1958, 1956, 1961, 1939, 1965, 1933, 1951, 1923, 1953, 1925, 1932, 1906, 1953, 1979, 1922, 1928, 1931 };
        //const int FCORTraceN = 1;
        //int[] COtrace = new int[FCORTraceN] { 1956};
        // ========================================= 
        //  
        const int FSVRTraceN = 40;
        int[] SVtrace = new int[FCORTraceN] { 1978, 1970, 1975, 1969, 1970, 1977, 1951, 1973, 1949, 1957, 1971, 1947, 1979, 1946, 1975, 1969, 1961, 1976, 1965, 1973, 1968, 1973, 1979, 1963, 1949, 1947, 1956, 1979, 1971, 1948, 1945, 1969, 1951, 1976, 1952, 1974, 1967, 1953, 1948, 1967 };
        //const int FSVRTraceN = 1;
        //int[] SVtrace = new int[FCORTraceN] { 1978};
        // =========================================
        //const int FGrowthN = 6;
        //int[] Growth = new int[FGrowthN] { 1, 25, 50, 75, 100, 125 };
        const int FGrowthN = 1;
        int[] Growth = new int[FGrowthN] {100};

        //const int FEfficiency = 4;
        //int[] Efficiency = new int[FEfficiency] {50,67,84,100};
        const int FEfficiency = 1;
        int[] Efficiency = new int[FEfficiency] {100 };


        int GPCD_pct = 0;

        //const int FWBN = 4;
        //int[] WaterBanking = new int[FWBN] { 0, 25, 50, 100 };
        const int FWBN = 1;
        int[] WaterBanking = new int[FWBN] { 0 };

        //const int FREUSEN = 4;
        //int[] WaterReuse = new int[FREUSEN] { 17, 50, 75, 100 };
        const int FREUSEN = 1;
        int[] WaterReuse = new int[FREUSEN] { 17 };

        //const int FDroughtN = 3;
        //int[] drought = new int[FDroughtN] { 2015, 2030, 2060 };
        const int FDroughtN = 1;
        //int[] drought = new int[FDroughtN] {2030 };

        //const int FAUGN = 4;
        //int[] WaterAug = new int[FAUGN] { 0, 7, 14, 21 };
        const int FAUGN = 1;
        int[] WaterAug = new int[FAUGN] { 0 };

        #endregion
        // ====================================================
        #region constructor
        /// <summary>   Constructor. </summary>
        /// <remarks>   07/16/2018. </remarks>
        /// <param name="DataDirectoryName">    Pathname of the data directory. </param>
        /// <param name="TempDirectoryName">    Pathname of the temporary directory. </param>
        public Model_5(string DataDirectoryName, string TempDirectoryName)
        {
            WaterSimManager.CreateModelOutputFiles = false;
            ws = new WaterSimManager_SIO(DataDirectoryName, TempDirectoryName);
            run();
        }
        #endregion
        #region WaterSim 5 Scenarios Revisited
        public void run()
        {
            scnGenForm();
            myVariables my = new myVariables();
            my.MyParameters();
            FO = new FileOutputs(ws);
            //
            String Filename = "Scenario.csv";
            int ScnCnt = 1;
            double Count = 1;
            double traces = FCORTraceN * FSVRTraceN * FEfficiency * FWBN * FAUGN * FREUSEN * FGrowthN * FDroughtN; ;
            double total = traces;
            sw = new System.IO.StreamWriter(Filename);
    
            List<string> ProcessList = new List<string>();
            ProcessList = ws.ProcessManager.ActiveProcesses;
            //
            if (get_ValidModelRun == true)
            {
               foreach (int st in SVtrace)
                {
                    foreach (int co in COtrace)
                    {
                        foreach (int wb in WaterBanking)
                        {
                            foreach (int reuse in WaterReuse)
                            {
                                foreach (int Aug in WaterAug)
                                {
                                    foreach (int pop in Growth)
                                    {
                                        foreach (int GPCD in Efficiency)
                                        {
                                            GPCD_pct = GPCD;
                                            string ScnName = "Base";
                                            string ver = ws.Model_Version;
                                            string buid = ws.ModelBuild;
                                            ws.Simulation_Initialize();
                                            my.init(ws);
                                            int scnName = 1;

                                            if (setParms(st, co, wb, reuse, GPCD, Aug, pop))
                                            {
                                                string Cstring = DateTime.Now.ToString("h:mm:ss tt") + " In step: " + "--" + Count + " Of: " + total + " > " + (Count / total) * 100 + " %";
                                                Console.WriteLine(Cstring);
                                                Console.WriteLine("");
                                                Console.WriteLine("Simulating " + ScnName);
                                                runonly();
                                            }
                                            Count += 1;
                                            runoutputs(scnName, ScnCnt, my, sw);
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
            string Myout = "Finished";
            Console.WriteLine(Myout);
            Console.WriteLine("");
            System.Threading.Thread.Sleep(2000); 
        }
        #endregion
        void runonly()
        {
            // ws.Simulation_AllYears();
            // 10.26.17
            for (int year = ws.Simulation_Start_Year; year < ws.Simulation_End_Year; ++year)
            {
                ws.Simulation_NextYear();
                StartSimulation = false;

            }
        }

        // =====================================================================================
        // WaterSim 5
        public bool setParms(int SVTraceYr, int COTraceYr, int wb, int reuse, int GPCD, int Aug, int Pop)
        {
            //(svTraceYr, coTraceYr, wb, reuse, GPCD, Aug, pop)
            bool result = true;

            SVTraceParm.Value = SVTraceYr;
            // ws.SaltVerde_Historical_Extraction_Start_Year = SVTraceYr;
            COTraceParm.Value = COTraceYr;
            //ws.Colorado_Historical_Extraction_Start_Year = COTraceYr;
            WBParm.Value = wb;
            //ws.Web_WaterBankPercent = wb;
            WReuseParm.Value = reuse;

            GPCDParm.Value = GPCD;
            //ws.Web_Personal_PCT = GPCD;
            WAugParm.Value = Aug;

            PopParm.Value = Pop;
            // ws.Web_PopulationGrowthRate_PCT = Pop;
            return result;
        }
        public void scnGenForm()
        {

            ws.IncludeAggregates = true;
            DroughtParmCO = ws.ParamManager.Model_Parameter(eModelParam.epColorado_User_Adjustment_Stop_Year);
            DroughtParmSV = ws.ParamManager.Model_Parameter(eModelParam.epSaltVerde_User_Adjustment_Stop_Year);

            SVTraceParm = ws.ParamManager.Model_Parameter(eModelParam.epSaltVerde_Historical_Extraction_Start_Year);
            COTraceParm = ws.ParamManager.Model_Parameter(eModelParam.epColorado_Historical_Extraction_Start_Year);
            WBParm = ws.ParamManager.Model_Parameter(eModelParam.epWebWaterBank_PCT);
            WReuseParm = ws.ParamManager.Model_Parameter(eModelParam.epWebReclaimedWater_PCT);
            GPCDParm = ws.ParamManager.Model_Parameter(eModelParam.epWebUIPersonal_PCT);
            WAugParm = ws.ParamManager.Model_Parameter(eModelParam.epWebAugmentation_PCT);
            PopParm = ws.ParamManager.Model_Parameter(eModelParam.epWebPop_GrowthRateAdj_PCT);
        }
        public void runoutputs(int i, int ScnCnt, myVariables mine, System.IO.StreamWriter SW)
        {
            //string ScnName = "Base";
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
                    FO.WriteHeader(ws.SimulationRunResults, mine, sw);
                }
                stopHeader = true;

            }
            if (FO.WriteResults(ws.SimulationRunResults, ScnName, mine, sw))
            {
                //ScnCnt++;

            }
            else
            {
            }
            ws.Simulation_Stop();
        }
        public string APiVersion { get { return _APIVersion; } }
        /// <summary>
        /// Verson of the Fortran Model
        /// </summary>
        public string ModelBuild { get { return _ModelBuild; } }
        // ========================================================================================================
        // E.O.F.
    }
}
