//      WaterSimDCDC Regional Water Demand and Supply Model Version 5.0

//       This is the C# interface to the WaterSim_DCDC FORTRAN dll.

//       Copyright (C) 2011 , The Arizona Board of Regents
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
using System.Runtime.InteropServices;       // for DllImport
using System.IO;       // for StreamWriter
using System.Diagnostics;
using System.ComponentModel;
using System.Threading;
using System.Data;
using System.Data.OleDb;

using System.Data.SqlClient;
using System.Data.Common;
using System.Windows.Forms;
using System.Web;


namespace WaterSimDCDC
{
    //
    /// <summary>
    /// The root class for the FORTRAN model of WaterSim. This class contains the
    /// constructor for the model.
    /// </summary>
    public class WaterSimU : IDisposable
    {
        //
        /// <summary>
        /// The FORTRAN dll name created by the WaterSim program (written in FORTRAN).
        /// </summary>
      
     //   const string FortranDLLFilename = "WaterSimDCDC_model_5.dll";
      //  const string FortranDLLFilename = "WaterSimDCDC_model.dll";
       // const string FortranDLLFilename = "WaterSimDCDC_model_6.dll";
   
        const string FortranDLLFilename = WaterSimDCDC.WaterSimManager.FortranDLL;
        //==========================================
        /// <summary>
        /// File build.  Updated when changes are made to the file.
        /// </summary>
        const string FileBuild =  "04.12.17_4:02:00";
        //
            #region Version-test and build-changed
            /// <summary>
            /// Base class for WaterSim. That is, the build not date.
            /// </summary>
             protected string mBuild = "base class";
            /// <summary>
            /// Returns the mBuild.
            /// </summary>
             protected internal virtual string get_Build
             { get { return mBuild; } }
            /// <summary>
            /// This date and time stamp of the FORTRAN model dll.
            /// </summary>
             protected string mTestVersion = "Base test";
            // properties found below Connection to model
            /// <summary>
            /// Returns the date and time stamp of the FORTRAN dll.
            /// </summary>
             protected internal virtual string get_TestVersion
             {
                get
                {
                    string TMonth = TestMonth_().ToString();
                    string TDay = TestDay_().ToString();
                    string TYear = TestYear_().ToString();
                    string THour = TestHour_().ToString();
                    string TMin = TestMin_().ToString();
                    mTestVersion = TMonth + "." + TDay + "." + TYear + "_" + THour + ":" + TMin;
                    return mTestVersion;
                }
             }
             static bool _isWaterSimU = false;  // used to keep track if a WAterSimManager object has been constructed

             //protected int mStatus = 0;
             //protected internal virtual int get_Status
             //{ get { return mStatus; } }

             //private string _programName;
             //private string _arguments;
            /// <summary>
            /// Where the data used to initialize and run the FORTRAN model reside.
            /// </summary>
             private string _dataDirectoryName;
            /// <summary>
            /// Where outputs are generated-written to disk- if the bool "Mywrite" is set 
            /// to true.
            /// </summary>
             private string _tempDirectoryName;
            //
            /// <summary>
            /// Used to create the build version and the date and time stamp (of the dll) 
            /// in the constructor (WaterSimU).
            /// </summary>
             public string myBuild;
            #endregion
            //-----------------------------------------------------------------------------------//      
            #region constants
             /// <summary>
             /// Number of modeled providers. i.e., those that we are focusing on. This excludes
             /// the two in the FORTRAN model ("no provider" and "other provider" and the three 
             /// in the MODFLOW extension to the model.
             /// </summary>
             internal const int cNumModeledProviders = 33;
            /// <summary>
            /// The number of water providers used in the current WaterSim-FORTRAN-model.
            /// </summary>
             internal const int cNumFortranProviders = 35;
            /// <summary>
            /// All providers in the SRV (at present- used or not used)
            /// </summary>
             internal const int cNumProviders = 38;

             private bool start_ = true;
             private int startyear = 2000; // This has no connectivity
             private int minStopYearDrought = 2085;
             private const int OutdoorWaterUsePct = 50;
             private const int OutdoorWaterUseResPct = 50;
             private const int OutdoorWaterUseComPct = 50;
             private const int OutdoorWaterUseIndPct = 50;

             internal const int ReclaimedToOutputPct = 10;
             private const int ReclaimedToInputPct = 10;
             private const int ReclaimedOutdoorUsePct = 100;
             private const int ROReclaimedToOutputPct = 80;
             private const int SurfaceWaterToWBankAmt = 1000;
             private const int SurfaceWaterToVadoseAmt = 0;
             private const int TimeLagForVadoseYears = 12;
             private const int WasteWaterToEffluentPct = 100;
             private const int WaterSupplyToDirectInjectAmt = 0;
             private const int WaterSupplyToRESpct = 70;
             private const int WaterSupplyToCOMpct = 20;
             private const int WaterSupplyToINDpct = 10;
             //
             public int RateResLeakagePct = 0; // i.e. 5.3%
             private const int RateComLeakageTenthsPct = 5;
             private const int RateIndustLeakageTenthsPct = 5;
             //
             private const int RateBlackWaterPct = 27;
             private const int AmountWaterCommercialTurf = 20;
             private const int OtherIndoorWaterPct = 30;

             //
             private const int SRPproviders = 10;
             protected internal int GroundWaterModel_ = 1; // default is simple=1
             private const int minUpperBasinValue_ = 2; // in the fortran array, one =year
             private const int maxUpperBasinValue_ = 4;
            #endregion
             //-----------------------------------------------------------------------------------//
            #region Initialize Model - Unused
            // [layer,row,column]
     
             #endregion
             //-----------------------------------------------------------------------------------//
            #region Connection to model (constructor): summary
            //
 
            // 
             public WaterSimU()
             {
             }
             /// <summary>
             ///  Constructor to instantiate the WaterSim model (in Extended).
             /// </summary>
             /// <param name="Data">data directory needed to run the model</param>
             /// <param name="Outputs">temporary directory path for text outputs</param>
             /// <param name="Write">bool determining whether text files get written or not</param>
             /// 
             public WaterSimU(string Data, string Outputs, bool Write)
             {
                 // Some Basic Tests
                 if (_isWaterSimU) throw new Exception("Already a WaterSimU object");
                 _isWaterSimU = true;
                 //
                 _dataDirectoryName = Data;
                 _tempDirectoryName = Outputs;
                 //
                 // writeLog Sets the FORTRAN model swith to enable writing  to the disk- the log file or
                 // other logical units depending on what's set in Global.f90
                 //==============================================================================
                 //
                  writeLog = Write;// 03.12.16 das
                 //
                  string preFix = " WSP.";
                 // Load the DLL by making a call to it- 
                 string start = BuildNum_().ToString() + "." + BuildVer_() + "." + BuildSubVer_() + "-";
                 // DAS 05.18.12
                 // Build date and time. i.e., month.day.year_hour:minute
                  myBuild = get_TestVersion;
                 // Combned build info (version and build date)
                  mBuild = preFix + start;  //+ ": " + myBuild;
                 //
                 // send the data path and the output path (if applicable) to the FORTRAN model
                 // ============================================================================
                 const int cBufferLengthSpecification = 200;
                 int sel = 1;
                 string stop = "*";
                 SetStringData_(ref sel, StringToFixedBytes(cBufferLengthSpecification,
                       Data + stop));
 
                 SetStringOutput_(ref sel, StringToFixedBytes(cBufferLengthSpecification,
                       Outputs));
                 //  
                  try
                 {
                     // ------------------------------------
                     // 07.26.16 DAS
                     // Check if the data files are found
                      // ===================================
                     ReadFileCheck();
                     bool found = get_FilesFound;
                     if (found)
                     {
                         // -----------------------------------------------
                         // If writing output files, open the logical units
                        // OpenFiles_();
                         // ------------------------------
                         // Read the data files- initialize the FORTRAN model
                         ResetAll();
                     }
                     else
                     {
                         throw new Exception("FORTRAN MODEL did not successfully find the data files: Sampson catch");
                     }

                 }
                 catch (IOException e)
                 {
                     throw new Exception("FORTRAN MODEL did not successfully find the data files: try/catch" + e);
                 }
             }
                      ~WaterSimU()
               {
                   Dispose(false);
               }
               //------------------------------------------------------------------------------------------
               /// <summary>
               /// Called by Dispose() and ~WaterSimManager() Finalizer  Prevents Finalizer from closing model files.
               /// </summary>
               /// <param name="disposing">Finalizer will call with a false</param>
               protected virtual void Dispose(bool disposing)
               {
                   if (disposing)
                   {
                       _isWaterSimU = false;
                       CloseFiles();
                       
       
                   }
                   // Get rid of unmanged stuff, like dbConnections
               }
               //------------------------------------------------------------------------------------------
              /// <summary>
              /// Must be called before WaterSim object loses scope or is lost (reassigned).
              /// </summary>
               public void Dispose()
               {
                   Dispose(true);
                   GC.SuppressFinalize(this);
               }
               //*****************************************************

            private static byte[] StringToFixedBytes(int bufferLengthSpecification, string str)
            {
                byte[] bytes = new byte[bufferLengthSpecification];
                for (int i = 0; i < bufferLengthSpecification; ++i) bytes[i] = (byte)32;
                if (0 < str.Length)
                {
                    if (bufferLengthSpecification < str.Length)

                        str = str.Substring(0, bufferLengthSpecification);

                    ASCIIEncoding.ASCII.GetBytes(str).CopyTo(bytes, 0);
                }
                return bytes;
            }

            //[DllImport(@FortranDLLFilename,CallingConvention=CallingConvention.Cdecl )]
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int BuildNum_();

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int BuildVer_();

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int BuildSubVer_();

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int TestMonth_();
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int TestDay_();
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int TestYear_();

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int TestHour_();
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int TestMin_();

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void SetStringData_(ref int selector, byte[] newValue);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void SetStringOutput_(ref int selector, byte[] newValue);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getStatus_();

            #endregion
            //-----------------------------------------------------------------------------------//
            #region Initialize Model Inputs From DB- Unused: summary
            /// <summary>
            /// Place holder for Database inputs into the FORTRAN dll for initializing the FORTRAN
            /// model
            /// 
            protected static class InterfaceInputsGPCD
            {
                internal const int Behavior = 7, Efficiency = 7, GPCDarray=10;
            }
            private int[,] GPCDbehavior = new int[35, InterfaceInputsGPCD.Behavior];
 
            protected internal int[,] GPCDResInputs
            {
                get
                {
                    int num = cNumFortranProviders;
                    int use = InterfaceInputsGPCD.Behavior;
                     int[,] GPCDResInputs = new int[num, use];
                     getGPCDResBehavior_(ref num, ref use, GPCDResInputs);
                    return GPCDResInputs;
                 }
                set
                {
                    int num = cNumFortranProviders;
                    int use = InterfaceInputsGPCD.Behavior;
                    GPCDbehavior = value;
                    setGPCDResBehavior_(ref num, ref use, (GPCDbehavior));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getGPCDResBehavior_(ref int count, ref int use, int[,] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setGPCDResBehavior_(ref int count, ref int use, int[,] values);
            /// 
            /// 
            /// </summary>
            protected static class InterfaceInputs
            {
             internal const int CAParray=2, PopulationArray=85;
            }
            /// <summary>
            /// Place holder
            /// </summary>
            private int[,] modelINputs = new int[35,InterfaceInputs.CAParray];
            /// <summary>
            /// Place Holder for database inputs from the interface into the FORTRAN model.
            /// </summary>
            //protected internal int[,] CAPinputs
            //{
            //    get
            //    {
            //        int num = cNumFortranProviders;
            //        int col=InterfaceInputs.CAParray;
            //        int[,] CAPinputs = new int[num,col];
            //        getCAPinputs_(ref num, ref col, CAPinputs);
            //        return CAPinputs;
            //        //return WaterSimU.TrimProvNameS(CAPinputs);
            //    }
            //    set
            //    {
            //        int num = cNumFortranProviders;
            //        int col = InterfaceInputs.CAParray;
            //        modelINputs = value;
            //        setCAPinputs_(ref num, ref col, (modelINputs));
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getCAPinputs_(ref int count, ref int col, int[,] values);

            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern void setCAPinputs_(ref int count, ref int CAParray, int[,] values);

            #endregion
            //-----------------------------------------------------------------------------------//
            #region Simulation Duration- Start and End: summary
            /// <summary>
            /// Year to start simulation: at present this is static at the year 2000
            /// </summary>
            protected internal int SimulationStart
            {
                get
                {
                    int gvalue = 0;
                    return getStartSimulation_(ref gvalue);
                }
                set
                {
                    int start = RangeCheckStartYear(value);
                    setStartSimulation_(ref start);
                }
            }
            //[DllImport(@"WaterSimDCDC_model.dll")]
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getStartSimulation_(ref int value);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setStartSimulation_(ref int value);
            //
            /// <summary>
            /// Year to stop simulation.  In my vernacular this is NOT inclusive. Ray runs through
            /// the end of the year for Simulation End.
            /// </summary>
            protected internal int SimulationEnd
            {
                get
                {
                    int gvalue = 0;
                    return getEndSimulation_(ref gvalue);
                }
                set
                {
                    int end = RangeCheckEndYear(value);
                    setEndSimulation_(ref end);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getEndSimulation_(ref int value);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setEndSimulation_(ref int value);
            //
            /// <summary>
            ///  Write standard, fixed, outputs to disk.  Found in the Outputs directory
            /// </summary>
            protected internal bool writeToDisk
            {
                get
                {
                    bool gvalue = false;
                    return getWriteStatus_(ref gvalue);
                }
                set
                {
                    setWriteStatus_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern bool getWriteStatus_(ref bool value);
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setWriteStatus_(ref bool value);
            //
            protected internal bool writeLog
            {
                get
                {
                    bool gvalue = false;
                    return getWriteLog_(ref gvalue);
                }
                set
                {
                    setWriteLog_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern bool getWriteLog_(ref bool value);
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setWriteLog_(ref bool value);

            #region Control Run
            protected internal bool StartSimulation
            {
                get { return start_; }
                set { start_ = value; }
            }
            protected internal int Startyear
            {
                get { return SimulationStart; }
                set { SimulationStart = value; }
            }

            protected internal int Endyear
            {
                get { return SimulationEnd; }
                set { SimulationEnd = value; }
            }
            #endregion

            protected internal void ReadFileCheck()
            {
                ReadFilesErr_();
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void ReadFilesErr_();



            /// <summary>
            /// Exactly that.  Opens and closes text files in the model proper.
            /// </summary>
            protected internal void OpenFiles()
            {
                OpenFiles_();
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void OpenFiles_();

            protected internal void CloseFiles()
            {
                CloseFiles_();
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void CloseFiles_();
            /// <summary>
            /// NOT presently used.
            /// </summary>
            protected internal void CloseSpecial()
            {
                CloseFilesS_();
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void CloseFilesS_();

            #endregion
            //-----------------------------------------------------------------------------------//
            #region Run Variables: summary
            // Absent: internal  int CurrentTime
            //
            /// <summary>
            /// Place holder for resetting states. NOT currently implemented.
            /// </summary>
            protected internal void ResetYear()
            {
                Init_();
                //InitStates_();
            }
            // Not currently implemented
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void InitYear_();

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void SetStartYear_(ref int value);
            // Not currently implemented
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void InitStates_();
            // Critical to the model
            /// <summary>
            /// This is the main property to reset the FORTRAN model to initial state conditions.
            /// It is used at the start of every simulation. It MUST be called before each
            /// successful simulation.
            /// </summary>
            protected internal void ResetAll()
            {
                Init_();
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void Init_();
            //
            protected internal void ResetAll_path(string Data,String Outputs)
            {
                const int cBufferLengthSpecification = 200;
                int sel = 1;

                SetStringData_(ref sel, StringToFixedBytes(cBufferLengthSpecification,
                Data));
    
                SetStringOutput_(ref sel, StringToFixedBytes(cBufferLengthSpecification,
                      Outputs));

                Init_();
            }
            protected internal void CloseAll()
            {   
                CloseFortran_();
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void CloseFortran_();
        /// <summary>
        /// Run the FORTRAN model for one year.  Keep all variables and parameters active
        /// in memory using the common block.
        /// </summary>
        protected internal void RunOneYear()
            {
                RunOneYear_();
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void RunOneYear_();
            //
            protected internal void RunManyYears()
            {
                RunManyYears_();
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void RunManyYears_();

             //
            //internal string ProgramName
            //{
            //    get { return _programName; }
            //    set { _programName = value; }
            //}

            //internal string Arguments
            //{
            //    get { return _arguments; }
            //    set { _arguments = value; }
            //}

            //internal string DataDirectoryName
            //{
            //    get { return _dataDirectoryName; }
            //    set { _dataDirectoryName = value; }
            //}

            //internal string TempDirectoryName
            //{
            //    get { return _tempDirectoryName; }
            //    set { _tempDirectoryName = value; }
            //}
            #endregion
            //-----------------------------------------------------------------------------------//
            #region Internally Defined I/O: summary
            /// <summary>
            /// Struct not presently used.  Used to modify demand in the MODFLOW model
            /// based on temperature data read as input into the FORTRAN model.
            /// </summary>
            internal struct outputs
            {

                internal float get_TemperatureMultiplier
                {
                    get
                    {
                        int gvalue = 0;
                        float ClimateDemand = 0;
                        getDemandMultiplier_(ref gvalue);
                        return ClimateDemand = Convert.ToSingle(gvalue * 0.01);
                    }
                }
                [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
                static extern int getDemandMultiplier_(ref int value);

            }
            #endregion
            //-----------------------------------------------------------------------------------//
            #region FORTRAN Error/Status Codes: summary
            protected internal bool get_FilesFound
            {
                get
                {
                    bool checkedValue = false;
                    return getReadFileError_(ref checkedValue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern bool getReadFileError_(ref bool value);

            /// <summary>
            /// This property checks if the FORTRAN model ran through each call without an error 
            /// code being generated.  It informs the interface if the results are likely feasible
            /// or not.  Returns true if the model read files correctly, and executed.
            /// </summary>
            protected internal bool  get_ValidModelRun
            {
                get
                {
                    bool checkedValue = false;
                    return getErrorCode_(ref checkedValue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern bool getErrorCode_(ref bool value);
          //
           protected internal bool APIdefaultStatus
            {
                get
                {
                    bool checkedValue = false;
                    return get_APIinterFaceStatus_(ref checkedValue);
                }
                 set
                {
                     set_APIinterFaceStatus_(ref value);
 
                 }
            }
           [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
           protected static extern bool get_APIinterFaceStatus_(ref bool value);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
           protected static extern bool set_APIinterFaceStatus_(ref bool value);
 


            #endregion
            //-----------------------------------------------------------------------------------//
            #region Groundwater: summary
            // From the API
            // WaterSimDCDC_APIver15.cs- DAS 10.27.11 
            // Now WaterSimDCDC_APIver20.cs
                //
                /// <summary>
                ///  Simple groundwater: 1) Value=0, MODFLOW groundwater: 2) Value=1
                /// </summary>
               protected internal int GroundWaterModel
                {
                    get{ return GroundWaterModel_; }
                    set{ GroundWaterModel_ = value;}
                }
            //
            // Set as an output for the API on 12.07.11
            /// <summary>
            /// This is get_Available_Groundwater property for
            ///     epAvailableGroundwater, or 
            ///     Available_Groundwater parameter (WaterSimManager)
            ///         using ModflowGroundwater[33]
            /// </summary>
            protected internal int[] ModflowGroundwater = new int[33]; // 12.07.11 DAS
            /// <summary>
            /// A constructor needed to run the MODFLOW code in the console
            /// application.  Not needed, or used, here.  But, retained so that
            /// this same interface may be used for all versions of the model.
            /// </summary>
            protected internal void GroundWater()
            {
            }
            /// <summary>
            ///  This is the method used in the console application of the model
            ///  When MODFLOW is used. Out must uncomment the lines to use the 
            ///  code. 
            /// </summary>
            /// <param name="year">The current year simulated</param>
            protected internal void GroundWater(int year)
            {
                bool SimpleGroundwater = Convert.ToBoolean(GroundWaterModel);
                if (!SimpleGroundwater)
                {
                    //GroundwaterInterface.sGroundwater(year, StartSimulation,
                    //    DataDirectoryName, TempDirectoryName, get_ProviderGWPumpedMunicipal,
                    //    get_SaltVerdeClassBCDesignations, get_ProviderGWRecharge, ModflowGroundwater);
                }
            }
            // =================================================================
            /// <summary>
            /// This is get_Groundwater_Balance property (WaterSimManager) for
            ///     epGroundwater_Balance, or
            ///     Groundwater_Balance parameter (WaterSimManager)
            ///         using ModelGroundwater
            /// </summary>
            protected internal int[] ModelGroundwater
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] ModelGroundwater = new int[numProviders];
                    getAnnualProviderGroundW_(ref numProviders, ModelGroundwater);
                    return TrimProvNames(ModelGroundwater);
                }
                set
                {
                    int numProviders = cNumFortranProviders;
                    int[] myout = new int[cNumModeledProviders];
                    myout = value;
                    setProviderGroundWater_(ref numProviders, ExpandProvNames35(myout));
                }
              }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getAnnualProviderGroundW_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setProviderGroundWater_(ref int count, int[] values);
            // =================================================================================
            // Regional Groundwater variables from City_Model
            // ---------------------------------------------------------------------------------
            //
            //
            //
            //
            //
            #region Regional Groundwater: summary
            /// <summary>
            /// Newest of code where we evaluate all water providers as the aggregate. This is the
            /// variable for setting or getting the current CAGRD(Central Arizona Groundwater 
            /// Replenishment District) estimate.
            /// </summary>
            protected internal int CAGRD
            {
                set
                {
                    int checkedValue = RangeCheckCAGRD(value);
                    setCAGRD_(ref checkedValue);
                }
                get
                {
                    int gvalue = 0;
                    return getCAGRD_(ref gvalue);
                }

            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setCAGRD_(ref int value);
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getCAGRD_(ref int value);
            //
            /// <summary>
            ///  The provider aggregated estimate of inflow in the SRV basin.  This is the
            ///  mountain front estimates from the SRV model report number 19, Figure 3.2
            ///  (ADWR April 2009)
            /// </summary>
            protected internal int get_RegionalInflow
            {
                get
                {
                    int gvalue = 0;
                    return getRegionalInflow_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getRegionalInflow_(ref int value);
            //
            /// <summary>
            ///  See directly above: outflow from Figure 3.2 (Version 5.7 Parameters ver 5.xlsx) which
            ///  is the current name of the parameter/inputs/outputs/codes for the model.
            /// </summary>
            protected internal int get_RegionalOutflow
            {
                get
                {
                    int gvalue = 0;
                    return getRegionalOutflow_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getRegionalOutflow_(ref int value);
            //
            /// <summary>
            /// Agriculture and other pumping for the SRV basin.  I received pumping estimates
            /// from Dale Mason (ADWR) in text file format. The other pumping comes from the
            /// report number 22, page 15. (July 2010)
            /// </summary>
            protected internal int get_RegionalAgOtherPumping
            {
                get
                {
                    int gvalue = 0;
                    return getAgAndOtherPumping_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getAgAndOtherPumping_(ref int value);
            //
            /// <summary>
            ///  At present the only recharge we do at the SRV basin scale is
            ///  direct injection estimates simulated.
            /// </summary>
            protected internal int get_RegionalProviderRecharge
            {
                get
                {
                    int gvalue = 0;
                    return getRegionalRecharge_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getRegionalRecharge_(ref int value);
            //
            /// <summary>
            /// At present this is only the portion of groundwater pumped; no surface water,
            /// other than effluent, is simulated.
            /// </summary>
            protected internal int get_RegionalAgToVadoseFlux
            {
                get
                {
                    int gvalue = 0;
                    return getAgToVadoseFlux_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getAgToVadoseFlux_(ref int value);
            //
            /// <summary>
            /// This is the Phoenix AMA, or more closely the SRV basis regional groundwater
            /// budget. Annual balance from the 33 Water providers... two- no provider and other
            /// provider - are not considered.
            /// </summary>
            protected internal int get_RegionalGroundWaterBalance
            {
                get
                {
                    int gvalue = 0;
                    return getRegionalGroundwater_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getRegionalGroundwater_(ref int value);
            //
            /// <summary>
            /// Natural recharge includes SRP and CAP canal leakage into the vadose, mountain front
            /// not accounted for by Figure 3.2 (see above), leakage into the stream from flood events
            /// .
            /// </summary>
            protected internal int get_RegionalNaturalRecharge
            {
                get
                {
                    int gvalue = 0;
                    return getNaturalRecharge_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getNaturalRecharge_(ref int value);
            //
            #endregion
            #region Provider-level
            /// <summary>
            /// Provider-specific estimates of recharge from City Model.  This includes
            /// all fluxes from the vadose to the aquifer.  Consult with the City-model
            /// Figure to ascertain the specific fluxes.
            /// </summary>
            //
            protected internal int[] get_VadoseToAquiferFlux  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] VadoseToAquiferFlux = new int[num];
                    getfluxVadoseToAquifer_(ref num, VadoseToAquiferFlux);
                    return TrimProvNames(VadoseToAquiferFlux);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getfluxVadoseToAquifer_(ref int count, int[] values);
            //
            /// <summary>
            /// The annual credit for vadose water from residential, commercial, and industrial
            /// water users. This is the outdoor water not evapotranspired.
            /// </summary>
            protected internal int[] get_WaterCreditIncidental
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_WaterCreditIncidental = new int[num];
                    getIncidentalCredits_(ref num, get_WaterCreditIncidental);
                    return WaterSimU.TrimProvNameS(get_WaterCreditIncidental);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getIncidentalCredits_(ref int count, int[] values);
            /// <summary>
            ///  March 1, 2014
            /// </summary>
            /// NOT in API
            protected internal int[] get_WaterCreditsAgAdded
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_WaterCreditsAgAdded = new int[num];
                    getAgToMuniAddedCredits_(ref num, get_WaterCreditsAgAdded);
                    return WaterSimU.TrimProvNameS(get_WaterCreditsAgAdded);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getAgToMuniAddedCredits_(ref int count, int[] values);
            /// <summary>
            /// March 1, 2014
            /// </summary>
            /// NOT in API
            protected internal int[] get_WaterCreditsTotalAdded
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_WaterCreditsTotalAdded = new int[num];
                    getTotalAddedCredits_(ref num, get_WaterCreditsTotalAdded);
                    return WaterSimU.TrimProvNameS(get_WaterCreditsTotalAdded);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getTotalAddedCredits_(ref int count, int[] values);

            /// <summary>
            /// The total credit balance for groundwater credits. Please refer to the credit model
            /// Figure for additional information. Labeled Figure two in the xls documentation.
            /// i.e., Version 5.7 Parameters ver 5.xlsx file.
            /// </summary>
            protected internal int[] get_WaterCreditTotals
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_WaterCreditTotals = new int[num];
                    getTotalCredits_(ref num, get_WaterCreditTotals);
                    return WaterSimU.TrimProvNameS(get_WaterCreditTotals);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getTotalCredits_(ref int count, int[] values);
            //

            //

            /// <summary>
            /// The annual difference between the annual pumping credits and the demand for water
            /// at the time-step. i.e., the demand for groundwater not satisfied by the annual
            /// credits allowed.
            /// 09.05.12
            /// </summary>
            protected internal int[] get_WaterCreditDeficits
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_WaterCreditDeficits = new int[num];
                    getCreditDeficits_(ref num, get_WaterCreditDeficits);
                    return WaterSimU.TrimProvNameS(get_WaterCreditDeficits);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getCreditDeficits_(ref int count, int[] values);

            #endregion
            //

            #endregion
            //-----------------------------------------------------------------------------------//
            #region Colorado River Variables: summary
            //
            /// <summary>
            /// Total CAP delivered along the aqueduct. Colorado River Arizona share minus 
            /// on-river uses and losses to evaporation.
            /// </summary>
            protected internal int get_CAP_BO
            {
                get
                {
                    int gvalue = 0;
                    return getCAP_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getCAP_(ref int value);
            // 
            /// <summary>
            /// Elevation of Lake Mead in feet above msl.
            /// </summary>
            protected internal int get_MeadElevation
            {
                get
                {
                    int gvalue = 0;
                    return getMeadElevation_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getMeadElevation_(ref int value);
            //
            protected internal int get_PowellElevation
            {
                get
                {
                    int gvalue = 0;
                    return getPowellElevation_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getPowellElevation_(ref int value);
            //


            /// <summary>
            /// The length of the river trace to use, in years, for the Colorado River historical
            /// or paleo flows. The default, now, is 30 years.  It was 25 years in previous versions
            /// of WaterSim.
            /// </summary>
            protected internal int set_ColoradoTrace
            {
                set
                {
                    int year =value;
                    setCOtrace_(ref year);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setCOtrace_(ref int value);
            /// <summary>
            /// The river data to use.  See the WaterSim xlsx documentation. i.e.,
            /// Version 5.7 Parameters ver 5.xlsx. 1=Bureau of Reclamation, 2=Paleo,
            /// 3=Scenario data (user defined- 85-year record).
            /// </summary>
            protected internal int ColoradoHistoricalData
            {
                get
                {
                    int gvalue = 0;
                    return getCOHistData_(ref gvalue);
                }
                set
                {
                    int year = RangeCheckColoradoData(value);
                    setCOHistData_(ref year);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getCOHistData_(ref int value);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setCOHistData_(ref int value);

            // Index year for runoff estimates (from historical runoff data)
            //         
            /// <summary>
            /// The "index" year to use.  I.e., the start of the (default) 30-year trace from the
            /// historical or paleo data for river runoff. 
            /// </summary>
            protected internal int ColoradoHistoricalExtractionStartYear
            {

                get
                {
                    int gvalue = 0;
                    return getCOHistExtractionStartYear_(ref gvalue);
                }
                set
                {
                    int year = RangeCheckColoradoHistoricalExtractionStartYear(value);
                    setCOHistExtractionStartYear_(ref year);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getCOHistExtractionStartYear_(ref int value);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setCOHistExtractionStartYear_(ref int value);
            //

            // Drought year start
            //
            //
            /// <summary>
            /// The year that a user imposed drought on the system begins. This, of course,
            /// for the Colorado River System. This must be greater than the start year and
            /// less than the end year.
            /// </summary>
 
            protected internal int ColoradoUserAdjustmentStartYear
            {
                get
                {
                    return getCOUserAdjustmentStartYear_();
                }
                set
                {
                    int year = RangeCheckColoradoUserAdjustmentStartYear(value);
                    setCOUserAdjustmentStartYear_(ref year);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getCOUserAdjustmentStartYear_();
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setCOUserAdjustmentStartYear_(ref int value);
            //

            // Drought year end
            //
            //        
            /// <summary>
            /// The year that a user imposed drought on the Colorado River System stops. This
            /// must be greater than the start year and greater than the simulation start
            /// but less than the end year.
            /// </summary>
            protected internal int ColoradoUserAdjustmentStopYear
            {
                get
                {
                    return getCOUserAdjustmentStopYear_();
                }
                set
                {
                    int year = RangeCheckColoradoUserAdjustmentStopYear(value);
                    setCOUserAdjustmentStopYear_(ref year);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getCOUserAdjustmentStopYear_();
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setCOUserAdjustmentStopYear_(ref int value);
            //

            // Drought factor (%) to adjust CO River runoff estimates (i.e., increase or reduce)
            //
            //              
            /// <summary>
            /// The change in the Colorado River flow (positive or negative) during a drought. This
            /// is evaluated as a percent of the historical (or paleo) record.
            /// </summary>
            protected internal int ColoradoUserAdjustmentPercent
            {
                get
                {
                    return getCOUserAdjustmentPercent_();
                }
                set
                {
                    int percent = RangeCheckColoradoUserAdjustmentPercent(value);
                    setCOUserAdjustmentPercent_(ref percent);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getCOUserAdjustmentPercent_();
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setCOUserAdjustmentPercent_(ref int value);
            //
            //
            // Climate factor (%) to adjust CO River runoff estimates (i.e., increase or reduce)
            //  
            /// <summary>
            /// The change in the Colorado River Flow (positive or negative) over the course
            /// of a simulation. This now operates as a variable change that reaches the estimated
            /// amount by a year set using the property "ClimateFactorEndYear." That is, now it is
            /// evaluated as a gradual reduction (or increase) achieved by the "EndYear."
            /// </summary>
            protected internal int ColoradoClimateAdjustmentPercent
            {
                get
                {
                    int gvalue = 0;
                    return getCOClimateAdjustmentPercent_(ref gvalue);
                }
                set
                {
                    int percent = RangeCheckColoradoClimateAdjustmentPercent(value);
                    setCOClimateAdjustmentPercent_(ref percent);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getCOClimateAdjustmentPercent_(ref int value);
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setCOClimateAdjustmentPercent_(ref int value);

            /// <summary>   Gets the get upper basin deliveries. </summary>
            /// <value> The get upper basin deliveries. Units are acre-feet</value>
            //setUpperBasinDeliveriesPCT(value)
            protected internal int get_UpperBasinDeliveries
            {
                get
                {
                    int gvalue = 0;
                    return getUpperBasinDeliveries_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getUpperBasinDeliveries_(ref int value);
            //

            /// <summary>   Sets the information describing the data for the upper basin estimate. </summary>
            /// <value> Value=2 is the 2007 Upper Colorado River Comission schedule,
            ///              value=3 is the ADWR Arizona Upper Basin Depletion schedule,
            ///              value=4 is the model estimate (Tim Lant original estimate). </value>

            //protected internal int set_upperBasinData
            public int set_upperBasinData
            {
                set
                {
                    int myvalue = RangeCheckUpperBasinIndex(value);
                    setUpperBasinDeliveriesIndex_(ref myvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setUpperBasinDeliveriesIndex_(ref int value);

            // 
            // absent: internal  int ColoradoActualEstimateOrRandom (NOT used)
            //
            #endregion CO river variables
            //-----------------------------------------------------------------------------------//
            #region Salt/Verde River Variables: summary
            //
            // 
            /// <summary>
            /// The length of the river trace to use, in years, for the Salt-Verde-Tonto River historical
            // or paleo flows. The default, now, is 30 years.  It was 25 years in previous versions
            // of WaterSim. Updated on 07.26.13 das
            /// </summary>
            protected internal int set_SaltVerdeTrace
            {
                set
                {
                    int gvalue = RangeCheckSTVtrace(value);
                    setSVTtrace_(ref gvalue);
                }
                get
                {
                    int gvalue = 0;
                    return getSTVtrace_(ref gvalue);
                 
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setSVTtrace_(ref int value);
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSTVtrace_(ref int value);

            /// <summary>   Gets the get salt-tonto verde trace. 
            ///             Separate from the set because this is a testing variable</summary>
            /// <value> The get salt verde trace. 07.26.13 das</value>

            protected internal int get_SaltVerdeTrace {
                get { int gvalue = 0; return getSTVtrace_(ref gvalue); } }
       
            //
            /// <summary>
            /// The river data to use.  See the WaterSim xlsx documentation. i.e.,
            /// Version 5.7 Parameters ver 5.xlsx. 1=Bureau of Reclamation, 2=Paleo,
            /// 3=Scenario data (user defined- 85-year record).
            /// </summary>
            protected internal int SaltVerdeTontoHistoricalData
            {
                get
                {
                    int gvalue = 0;
                    return getSVTHistData_(ref gvalue);
                }
                set
                {
                    int gvalue = RangeCheckSVTData(value);
                    setSVTHistData_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSVTHistData_(ref int value);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setSVTHistData_(ref int value);

            //   Index year for the Salt-Verde-Tonto rivers.  The year to start simulations 
            // from historical data estimates.
            //
            //         
            /// <summary>
            /// The "index" year to use.  I.e., the start of the (default) 30-year trace from the
            /// historical or paleo data for river runoff.
            /// </summary>
            protected internal int SaltVerdeHistoricalExtractionStartYear
            {
                get
                {
                    int gvalue = 0;
                    return getSVHistExtractionStartYear_(ref gvalue);
                }
                set
                {
                    int year = RangeCheckSaltVerdeHistoricalExtractionStartYear(value);
                    setSVHistExtractionStartYear_(ref year);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSVHistExtractionStartYear_(ref int value);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setSVHistExtractionStartYear_(ref int value);
            //

            // Drought year - year to implement a drought on the Salt-Verde-Tonto River system
            //
            //   
            /// <summary>
            /// The year that a user imposed drought on the system begins. This, of course,
            /// for the Salt-Verde-Tonto River System. This must be greater than the start year and
            /// less than the end year.
            /// </summary>
            protected internal int SaltVerdeUserAdjustmentStartYear
            {
                get
                {
                    return getSVUserAdjustmentStartYear_();
                }
                set
                {
                    int year = RangeCheckSaltVerdeUserAdjustmentStartYear(value);
                    setSVUserAdjustmentStartYear_(ref year);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSVUserAdjustmentStartYear_();
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setSVUserAdjustmentStartYear_(ref int value);
            //

            // Year to stop the imposed drought on the Salt-Verde-Tonto river system
            //
            //        
            /// <summary>
            /// The year that a user imposed drought on the  Salt-Verde-Tonto River System stops. This
            /// must be greater than the start year and greater than the simulation start
            /// but less than the end year.
            /// </summary>
            protected internal int SaltVerdeUserAdjustmentStopYear
            {
                get
                {
                    return getSVUserAdjustmentStopYear_();
                }
                set
                {
                    int year = RangeCheckSaltVerdeUserAdjustmentStopYear(value);
                    setSVUserAdjustmentStopYear_(ref year);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSVUserAdjustmentStopYear_();
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setSVUserAdjustmentStopYear_(ref int value);
            //

            // Reductions in base flow of the Salt-Verde-Tonto based on the drought imposed above
            //
            //     
            /// <summary>
            /// The change in the Salt-Verde-Tonto River flow (positive or negative) during a drought. This
            /// is evaluated as a percent of the historical (or paleo) record. i.e., 90 would create a 10%
            /// reduction in flows on the system during a drought period.
            /// </summary>
      
            protected internal int SaltVerdeUserAdjustmentPercent
            {
                get
                {
                    return getSVUserAdjustmentPercent_();
                }
                set
                {
                    int percent = RangeCheckSaltVerdeUserAdjustmentPercent(value);
                    setSVUserAdjustmentPercent_(ref percent);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSVUserAdjustmentPercent_();
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setSVUserAdjustmentPercent_(ref int value);
            //

            //   Climate factor for the Salt-Verde-Tonto river system (proportional increase in flow
            // as influenced by the adjustment imposed).  Used as a percent in the Interface.
            //
            //                 
            /// <summary>
            /// The change in the Salt-Verde-Tonto River Flow (positive or negative) over the course
            /// of a simulation. This now operates as a variable change that reaches the estimated
            /// amount by a year set using the property "ClimateFactorEndYear." That is, now it is
            /// evaluated as a gradual reduction (or increase) achieved by the "EndYear."
            /// </summary>
            protected internal int SaltVerdeClimateAdjustmentPercent
            {
                get
                {
                    int gvalue = 0;
                    return getSVClimateAdjustmentPercent_(ref gvalue);
                }
                set
                {
                    int percent = RangeCheckSaltVerdeClimateAdjustmentPercent(value);
                    setSVClimateAdjustmentPercent_(ref percent);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSVClimateAdjustmentPercent_(ref int value);
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setSVClimateAdjustmentPercent_(ref int value);
            //
            // absent:  internal  SRPReleaseEstimateMethodType SRPReleaseEstimateMethod
            //
            //   Three approaches to estimate SRP release from the Salt-Verde-Tonto River reservoir. The
            // historical estimates are not verified (original code).  The Tim Lant approach, and the
            // David Sampson Approach.  See draft manusctip for details.  Defult is now the DAS approach.

            #endregion
            //-----------------------------------------------------------------------------------//
            #region Groundwater Variables:County scale-Not used & not in new dll
            // ===================================
            //

            // Current state of the groundwater-county level simulations (acft)
            //
            //           [CERTIFIED 02.03.2010 das]
            /// <summary>
            /// County-scale. Not presently used. NO recent evaluation of the variable.
            /// </summary>
            //protected internal int get_GroundwaterState
            //{
            //    get
            //    {
            //        int gvalue = 0;
            //        return getGroundwaterState_(ref gvalue);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getGroundwaterState_(ref int value);
            //

            // absent:   internal  int get_NaturalRecharge
            //
  
            //

            //  Annual difference in groundwater state variable - county scale: acft a-1
            //
            //          [CERTIFIED 02.03.2010 das]
            /// <summary>
            ///  County-scale. Not presently used. NO recent evaluation of the variable.
            /// </summary>
            /// 
            /// 05.16.17 commented out
            //protected internal int get_GroundwaterDifference
            //{
            //    get
            //    {
            //        int gvalue = 0;
            //        return getGroundwaterDiff_(ref gvalue);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getGroundwaterDiff_(ref int value);
            //

            // Annual groundwater pumped, county scale (acft a-1)
            //
            //          [CERTIFIED 02.03.2010 das]
            /// <summary>
            ///  County-scale. Not presently used. NO recent evaluation of the variable.
            /// </summary>
            /// 05.16.17 commented Out
            //protected internal int get_GroundwaterPumpage
            //{
            //    get
            //    {
            //        int gvalue = 0;
            //        return getGroundwaterPumpage_(ref gvalue);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getGroundwaterPumpage_(ref int value);
            //

            // Annual groundwater recharged, county scale (acft a-1)
            //
            //          [CERTIFIED 02.03.2010 das]
            /// <summary>
            ///  County-scale. Not presently used. NO recent evaluation of the variable.
            /// </summary>
            /// 05.16.17 commented out
            //protected internal int get_GroundwaterRecharge
            //{
            //    get
            //    {
            //        int gvalue = 0;
            //        return getGroundwaterRecharge_(ref gvalue);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getGroundwaterRecharge_(ref int value);
            //
            #endregion
            //-----------------------------------------------------------------------------------//
            #region Energy: summary

            //          [CERTIFIED 02.11.2010 das] - Giga Watt hours a-1
            /// <summary>
            /// The energy (electrical) needed to pump CAP water from Lake Havasu to Granite Reef. 
            /// The units are Giga Watt hours per annum.
            /// </summary>
            //protected internal int get_CAPEnergyUsedInPumping
            //{
            //    get
            //    {
            //        int gvalue = 0;
            //        return getCAPEnergyUsedInPumping_(ref gvalue);
            //        //return gvalue;
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getCAPEnergyUsedInPumping_(ref int value);

            //

            //
            // absent: internal  int GroundwaterPumpingEnergyUse

            #endregion
            //-----------------------------------------------------------------------------------//
            #region County Scale: Population variables-changed
            //
            //  Maricopa County population from MAG projections 
            //
            //      [CERTIFIED- 01.29.10 das]
            //protected internal int get_Population
            //{
            //    get
            //    {
            //        int gvalue = 0;
            //        return getPopMaricopa_(ref gvalue);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getPopMaricopa_(ref int value);
            //
            #endregion
            //-----------------------------------------------------------------------------------//
            #region Provider Populations-Changed
            //   Factor to increase or decrease the county population.  This variable uses the
            // annual differences in base projection to add to the previous year estimate to obtain
            // a new estimate (i.e., pop in year -1 + (pop in year - pop in year-1)* PopulationGrowthRateAdjustment).
            //
            //          [CERTIFIED- 01.29.10 das]
            /// <summary>
            ///  The parameter used to chage the population growth rate for all providers at once.
            ///  This variable is not currently used.
            /// </summary>
            //
            // 11.03.17 das commented out this property
            //
            //protected internal int PopulationGrowthRateAdjustmentPercent
            //{
            //    // QUAY MOD 3 6 13
            //    // Sampson 01.09.14
            //    get
            //    {
            //        int gvalue = 0;
            //        return getPopGrowthRateAdjustmentPct_(ref gvalue);
            //    }
            //    set
            //    {
            //        int percent = RangeCheckPopulationGrowthRateAdjustmentPercent(value);
            //        setPopGrowthRateAdjustmentPct_(ref percent);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern void setPopGrowthRateAdjustmentPct_(ref int value);

            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getPopGrowthRateAdjustmentPct_(ref int value);
            //
            // absent:  internal  int PopulationIn2030
            //
            // -------------------------------------------------------
            // Provider-level variables
            //
            // Provider-specific population growth rate adjustments.
            //

            /// <summary>   Gets or sets the provider Population growth rate adjust pct. </summary>
            /// 
            /// <value> The provider POP growth rate adjust pct for each water provider. </value>
            protected internal int[] ProviderPopGrowthRateAdjustPct
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] PopulationGrowthRateAdjustPct = new int[numProviders];
                    getPopGrowthRateAdjustPct_(ref numProviders, PopulationGrowthRateAdjustPct);
                    return TrimProvNames(PopulationGrowthRateAdjustPct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setPopGrowthRateAdjustPct_(ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getPopGrowthRateAdjustPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setPopGrowthRateAdjustPct_(ref int count, int[] values);
            //

            // Provider-specific population growth rate adjustments for on-project (SRP) members.
            // NOTE: this variable is overritten (in the FORTRAN model) by
            //  set_ProviderPopGrowthRateAdjustPct[i]- must be equal to zero to be invoked
            protected internal int[] ProviderPopGrowthRateOnProjectPct
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] ProviderPopGrowthRateOnProjectPct = new int[numProviders];
                    getPopGrowthRateOnProjectPct_(ref numProviders, ProviderPopGrowthRateOnProjectPct);
                    return TrimProvNames(ProviderPopGrowthRateOnProjectPct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setPopGrowthRateOnProjectPct_(ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getPopGrowthRateOnProjectPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setPopGrowthRateOnProjectPct_(ref int count, int[] values);
            //
            // Provider-specific population growth rate adjustments for off-project (SRP) members.
            //
            protected internal int[] ProviderPopGrowthRateOtherPct
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] ProviderPopGrowthRateOtherPct = new int[numProviders];
                    getPopGrowthRateOtherPct_(ref numProviders, ProviderPopGrowthRateOtherPct);
                    return TrimProvNames(ProviderPopGrowthRateOtherPct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setPopGrowthRateOtherPct_(ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getPopGrowthRateOtherPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setPopGrowthRateOtherPct_(ref int count, int[] values);
            //

            //  Water Provider get_Populations - on project
            //
            //      [CERTIFIED-06.22.12
            protected internal int[] get_PopulationOnProject
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_PopulationOnProject = new int[num];
                    PopulationOnProject_(ref num, get_PopulationOnProject);
                    return TrimProvNames(get_PopulationOnProject);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int PopulationOnProject_(ref int count, int[] values);
            //
            // Population Other (i.e., not on-project)
            //      [CERTIFIED-06.22.12
            protected internal int[] get_PopulationOther
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_PopulationOther = new int[num];
                    PopulationOther_(ref num, get_PopulationOther);
                    return TrimProvNames(get_PopulationOther);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int PopulationOther_(ref int count, int[] values);

            //  Water Provider get_Populations     
            //
            //      [CERTIFIED- 01.29.10 das][RE-CERTIFIED- 02.04.10 das]
            protected internal int[] get_Populations
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_Populations = new int[num];
                    getPopulations_(ref num, get_Populations);
                    return TrimProvNames(get_Populations);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getPopulations_(ref int count, int[] values);
            //
            // DAS 08.06.12
            protected internal int[] set_PopulationsOn
            {
                set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setPopulationsOn_(ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int setPopulationsOn_(ref int count, int[] values);
            //
            protected internal int[] set_PopulationsOther
            {
                set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setPopulationsOther_(ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int setPopulationsOther_(ref int count, int[] values);

            #endregion
            //-----------------------------------------------------------------------------------//
            #region Policy Variables: summary
            //

    
            /// <summary>
            /// This is the year at which the imposed climate change factor for either the Colorado
            /// River System, or the Salt-Verde-Tonto River System achieves the value set by 
            /// ColoradoClimateAdjustmentPercent or SaltVerdeClimateAdjustmentPercent. Thus, a gradual
            /// change in the runoff modifiers reaches the set value by the ClimateFactorEndYear (at
            /// present, one value for both river systems).
            /// </summary>
            protected internal int ClimateFactorEndYear
            {
                get
                {
                    int gvalue = 0;
                    int myInt=getClimateFactorEndYear_(ref gvalue);
                    return myInt + 2000;
                }
                set
                {
                    int year = RangeCheckClimateFactorEndYear(value);
                    int pass = value - 2000;
                    setClimateFactorEndYear_(ref pass);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getClimateFactorEndYear_(ref int value);
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setClimateFactorEndYear_(ref int value);
            // 01.18.12 DAS
            /// <summary>
            ///  Property to modify the Trott table- bypassing the 1910 statute whereby
            /// the user tells the model the acre-feet per acre that class A member
            /// lands will receive as normal flow (from the daily flow estimate and the
            /// daily table that uses cfs by provider by flow threshold Use values ranging 
            /// from 1 (0.1) to 55 (5.5) acre-feet per acre-1. Note that 55 reverts to 5.4288 
            /// within the model [maximum- equilivent to 0.3 Miners inches per acre-1 as set in the 1910 decree]
            /// </summary>
            /// // There are two of these variables MNFLOW das
            protected internal int[] ModifyProviderNormalFlowPct
            {
                get
                {
                    int numProviders = cNumModeledProviders;
                    int numSRPmembers = SRPproviders;
                    int[] ModifyProviderNormalFlow10 = new int[SRPproviders];
                    getModifyNormalFlow_(ref numSRPmembers, ModifyProviderNormalFlow10);
                    //
                    int[] ModifyProviderNormalFlowPct = new int[numProviders];
                    ModifyProviderNormalFlowPct = ExpandProvNamesSRPto33(ModifyProviderNormalFlow10);
                    return ModifyProviderNormalFlowPct;
                }
                set
                {
                    int numProviders = cNumModeledProviders;
                    int numSRPmembers = SRPproviders;
                    int[] myout = new int[numProviders];
                    myout = value;
                    setModifyNormalFlow_(ref numSRPmembers, TrimProvNamesToSRP(myout));
                }

            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setModifyNormalFlow_(ref int count, int[] values);
            //
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getModifyNormalFlow_(ref int count, int[] values);



            //
            /// <summary>
            /// The percent reduction in GPCD realized by 2085. Verified in July, 2013
            /// with new code.
            /// Modified on 02.06.14 to include the method "scale"
            /// </summary>
            protected internal int set_AlterGPCDpct
            {
                set
                {
                    // Scale to unit range
                    int checkedValue = RangeCheckGPCDpct(value);
                    int second = scale(value);
                    setAlterGPCDPct_(ref second);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setAlterGPCDPct_(ref int value);

            protected int scale(int value)
            {
                int response = 0;
                if (value != 0)
                {
                    response = -value;
                }
                else
                {

                }
                return response;
            }
            // --------------------------------------------------------------------------------------
            /// <summary>
            /// This is used by the parameter_default file
            /// </summary>
            protected internal int[] AlterProviderGPCDpct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] AlterProviderGPCDpct = new int[num];
                    getProviderAlterGPCDPct_(ref num, AlterProviderGPCDpct);
                    return WaterSimU.TrimProvNameS(AlterProviderGPCDpct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    int numProviders = cNumModeledProviders;
                    int[] myout = new int[numProviders];
                    myout = value;
                    setProviderAlterGPCDPct_(ref num, ExpandProvNames35(myout));
                }

            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setProviderAlterGPCDPct_(ref int count, int[] values);
            //
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getProviderAlterGPCDPct_(ref int count, int[] values);

            protected internal int[] ProviderGPCDmin
            {
                set
                {
                  
                    int num = cNumFortranProviders;
                    int numProviders = cNumModeledProviders;
                    int[] myout = new int[numProviders];
                    myout = value;
                    setProviderMinGPCD_(ref num, ExpandProvNames35(myout));
                }
                get
                {
                    int num = cNumFortranProviders;
                    int[] MinProviderGPCD = new int[num];
                    getProviderAlterGPCDPct_(ref num, MinProviderGPCD);
                    return WaterSimU.TrimProvNameS(MinProviderGPCD);
                    
                }

            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setProviderMinGPCD_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getProviderMinGPCD_(ref int count, int[] values);

            // absent:  internal  RunoffDataTypeCO RunoffDataCO

            // absent:  internal  RunoffDataTypeSVT RunoffDataSVT

            // ------------------------------------------------------------
            // County Scale

            //
            // County-scale water policies
            //
            /// <summary>
            /// County-scale. Not fully used.  That is, this county scale variable has not been used in
            /// years.
            /// </summary>
            protected internal enum WaterPolicyType { NoOverdraft = 1, SatisfyDemand, FiveYearSustainable, FixedResidentialGPCD };
            //           [CERTIFIED- 02.03.10 das]
            /// <summary>
            /// County-scale. See above:
            /// </summary>
            //internal WaterPolicyType WaterPolicy
            //{
            //    get
            //    {
            //        int iWaterPolicy = getWaterPolicy_();
            //        switch (iWaterPolicy)
            //        {
            //            case 1:
            //                return WaterPolicyType.NoOverdraft;
            //            case 2:
            //                return WaterPolicyType.SatisfyDemand;
            //            case 3:
            //                return WaterPolicyType.FiveYearSustainable;
            //            case 4:
            //                return WaterPolicyType.FixedResidentialGPCD;
            //        }
            //        throw new Exception("Unknown water policy type: " + iWaterPolicy.ToString());
            //    }
            //    set
            //    {
            //        int checkedValue = RangeCheckWaterPolicy(value);
            //        setWaterPolicy_(ref checkedValue);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getWaterPolicy_();

            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern void setWaterPolicy_(ref int value);


            // Year to implement policy
            //
            //       
            /// <summary>
            /// County-scale. Not used in years. This is the year that implements the policy selected in the
            /// Water Policy Type (above)
            /// </summary>
            //protected internal int WaterPolicyStartYear
            //{
            //    get
            //    {
            //        return getWaterPolicyStartYear_();
            //    }
            //    set
            //    {
            //        int year = RangeCheckWaterPolicyStartYear(value);
            //        setWaterPolicyStartYear_(ref year);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getWaterPolicyStartYear_();

            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern void setWaterPolicyStartYear_(ref int value);
            //

            //  Shortage sharing of water as imposed by policy
            //
            //        
            /// <summary>
            /// County-scale. A variable that is a hold over from previous versions of
            /// the model.  We have not used this in the current version of the model.
            /// </summary>
            //internal ShortageSharingPolicyType ShortageSharingPolicy
            //{
            //    get
            //    {
            //        int gvalue = 0;
            //        int iShortageSharingPolicy = getShortagePolicy_(ref gvalue);
            //        return 1 == iShortageSharingPolicy ?
            //            ShortageSharingPolicyType.ProportionalSharing :
            //            ShortageSharingPolicyType.AgLosesWaterFirst;
            //    }
            //    set
            //    {
            //        int checkedValue = RangeCheckShortageSharingPolicy(value);
            //        setShortagePolicy_(ref checkedValue);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getShortagePolicy_(ref int value);

            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern void setShortagePolicy_(ref int value);
            //

            // Residential 
            // GPCD 
            //
            //     
            /// <summary>
            /// County-scale. Not used in years.  As the name suggests, residental GPCD to use
            /// in the simulations.
            /// </summary>
            //protected internal int ResidentialGPCD
            //{
            //    get
            //    {
            //        //int something = getResidentialGPCD_();
            //        //Console.WriteLine(something.ToString());
            //        //return something;
            //        return getResidentialGPCD_();
            //    }
            //    set
            //    {
            //        int checkedValue = RangeCheckResidentialGPCD(value);
            //        setResidentialGPCD_(ref checkedValue);
            //        checkedValue = Convert.ToInt32(checkedValue * 1.33);
            //        setTotalGPCD_(ref checkedValue);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getResidentialGPCD_();
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern void setResidentialGPCD_(ref int value);
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setTotalGPCD_(ref int value);
            //

            // Three residential density paths (1-3)
            //
            //           
            /// <summary>
            /// County-scale. Not used in years.
            /// </summary>
            //protected internal int ResidDensityPath
            //{
            //    get
            //    {
            //        int gvalue = 0;
            //        return getResidDensityPath_(ref gvalue);
            //    }
            //    set
            //    {
            //        int checkedValue = RangeCheckResidDensityPath(value);
            //        setResidDensityPath_(ref checkedValue);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getResidDensityPath_(ref int value);
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern void setResidDensityPath_(ref int value);
            //

            //   Household percent pools.  In the model used to calculate the difference
            // between current and the year 2000 estimate (default for 2000 is 20%)
            //
            //
            /// <summary>
            /// County-scale. Not used in years.
            /// </summary>
            protected internal int set_HHoldPctPools
            {
                set
                {
                    int checkedValue = RangeCheckHHoldPct(value);
                    setPropPools_(ref checkedValue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setPropPools_(ref int value);
            //

            //   Household percent mesic.  In the model used to calculate the difference
            // between current and the year 2000 estimate (default for 2000 is 20%)
            //
            //       
            /// <summary>
            /// County-scale. Not used in years.
            /// </summary>
            protected internal int set_HHoldPctMesic
            {
                set
                {
                    int checkedValue = RangeCheckHHoldPct(value);
                    setPropMesic_(ref checkedValue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setPropMesic_(ref int value);
            //

            //   Agricultural retirement year, when all but Indian agriculture stops. Default
            // is 2070.
            //
            //              
            /// <summary>
            /// County-scale. Not used in years. 11.05.13 DAS
            /// </summary>
            //protected internal int AgricultureRetirementYear
            //{
            //    get
            //    {
            //        int gvalue = 0;
            //        return getAgricultureRetirementYear_(ref gvalue);
            //    }
            //    set
            //    {
            //        int year = RangeCheckAgricultureRetirementYear(value);
            //        setAgricultureRetirementYear_(ref value);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getAgricultureRetirementYear_(ref int value);
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern void setAgricultureRetirementYear_(ref int value);
            //
            // absent: internal  int GWCutoffSwitch
            //

            //
            // absent:  internal  int MirrorWaterPolicy
            //
            #region Ground Water Policy Variables
            //
            // absent: internal  int ArizonaWaterBankingAuthoritySwitch 
            //
            // absent:  internal  int CAGRDSwitch
            //
            //
            /// <summary>
            /// County-scale. Not used in years.
            /// </summary>
            protected internal bool NaturalRechargeSwitchF
            {
                get
                {
                    bool gvalue = false;
                    return getNaturalRechargeSwitch_(ref gvalue);
                }
                set
                {
                    setNaturalRechargeSwitch_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern bool getNaturalRechargeSwitch_(ref bool value);
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setNaturalRechargeSwitch_(ref bool value);

            //
            // absent:   internal  int GroundWaterCutoffThreshold
            //
            #endregion
            #region Recycled Water Policy Variables
            //
            // absent:  internal  int RecycledWaterRatePercent 
            //
            // absent:  internal  int ResidentialIndoorUsePercent
            //
            // absent:   internal  int CommercialAndIndustrialIndoorUsePercent
            //
            //
            #endregion


            #endregion
            //-----------------------------------------------------------------------------------//
            #region Agriculture
            /// <summary>
            /// Grab a portion of the county surface water used for Agriculture and divert it to
            /// Municipal and Commercial/ Industrial water use.
            ///  NOT USED
            // ----------------------------------------------------------------------------------------------------
            // 05.16.17 commented this out
            //protected internal int WaterFromAgSurfaceTotal
            //{
            //    get
            //    {
            //        int gvalue = 0;
            //        return getWaterFromAgSurfaceTotal_(ref gvalue);
            //    }
            //    set
            //    {
            //        int MyInt = value;
            //        setWaterFromAgSurfaceTotal_(ref MyInt);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern void setWaterFromAgSurfaceTotal_(ref int value);

            //  [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getWaterFromAgSurfaceTotal_(ref int value);

            /// <summary>
            /// What is this? 
            /// 08.11.16
            /// </summary>
            protected internal int[] WaterFromAgSurface
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] WaterFromAgSurface = new int[num];
                    getWaterFromAgSurface_(ref num, WaterFromAgSurface);
                    return WaterSimU.TrimProvNameS(WaterFromAgSurface);
                }
                set
                {
                    int numbers = cNumModeledProviders;
                    int[] myout = new int[numbers];
                    myout = value;
                    setWaterFromAgSurface_(ref numbers, ExpandProvNames35(myout));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getWaterFromAgSurface_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setWaterFromAgSurface_(ref int count, int[] values);
            //
            protected internal int[] get_WaterFromAgSurfaceMax
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] WaterFromAgSurfaceMax = new int[num];
                    getWaterFromAgSurfaceMax_(ref num, WaterFromAgSurfaceMax);
                    return WaterSimU.TrimProvNameS(WaterFromAgSurfaceMax);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getWaterFromAgSurfaceMax_(ref int count, int[] values);
            //
            protected internal int[] get_WaterFromAgPumpingMax
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] WaterFromAgPumpingMax = new int[num];
                    getWaterFromAgPumpingMax_(ref num, WaterFromAgPumpingMax);
                    return WaterSimU.TrimProvNameS(WaterFromAgPumpingMax);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getWaterFromAgPumpingMax_(ref int count, int[] values);

            /// <summary>
            /// Revisit- 01.29.15 DAS
            /// This is the central control to move water from Ag to Urban (groundwater credits)
            /// 
            /// Grab a portion of the AMA groundwater used for Agriculture and divert it to
            /// Municipal and Commercial/ Industrial water use. The units are Acre-feet per year.
            /// pumping based on credits and the ADWR groundwater curve
            /// 06.17.16
            /// </summary>
            protected internal int [] WaterFromAgPumping
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] WaterFromAgPumping = new int[num];
                    getWaterFromAgPumping_(ref num, WaterFromAgPumping);
                    return WaterSimU.TrimProvNameS(WaterFromAgPumping);
                 }
                set
                {
                    int numbers = cNumFortranProviders;
                    int[] myout = new int[numbers];
                    myout = value;
                    setWaterFromAgPumping_(ref numbers,ExpandProvNames35(myout));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getWaterFromAgPumping_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setWaterFromAgPumping_(ref int count, int[] values);
    
            //
            /// <summary>
            ///  Added as a provider basis on 05.11.14 to meet Vinze_Johnston 
            /// </summary>
            protected internal int[] WaterToAgriculture_AF
            {

                set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setWaterToAgricultureAF_(ref num, ExpandProvNames35(citiModel));
                }
                get
                {
                    int num = cNumFortranProviders;
                    int[] waterToAgriculture_AF = new int[num];
                    getWaterToAgricultureAF_(ref num, waterToAgriculture_AF);
                    return TrimProvNameS(waterToAgriculture_AF);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setWaterToAgricultureAF_(ref int count, int[] values);
            //
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getWaterToAgricultureAF_(ref int count, int[] values);
            //
            /// <summary>
            /// 05.16.14 DAS - net ag water annually
            /// </summary>
            protected internal int[] get_NetAgricultureWater_AF
            {

                get
                {
                    int num = cNumFortranProviders;
                    int[] waterForAgriculture_AF = new int[num];
                    getNetAgricultureWaterAF_(ref num, waterForAgriculture_AF);
                    return TrimProvNameS(waterForAgriculture_AF);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getNetAgricultureWaterAF_(ref int count, int[] values);
          //
            /// <summary>
            /// 05.16.14,06.19.15 DAS - gross ag water pumped annually 
            ///  - okay 06.17.16 - 03.14.17 checked.....
            /// </summary>
            protected internal int[] get_GrossAgricultureWaterPumped_AF
            {

                get
                {
                    int num = cNumFortranProviders;
                    int[] AgricultureWater_AF = new int[num];
                    getGrossAgricultureWaterAF_(ref num, AgricultureWater_AF);
                    return TrimProvNameS(AgricultureWater_AF);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getWaterFromAgPumpingTotal_(ref int count, int[] values);
            protected static extern int getGrossAgricultureWaterAF_(ref int count, int[] values);
            /// <summary>
            /// 01.27.15 DAS
            ///  Scaled from 1 to 17, this parameter sets the Agriculture Pumping curve index
            ///  for the model. A value of 9 is ~ the ADWR Ag pumping curve estimate. A value
            ///  of 1 is a ~ 5% reduction in Ag pumping from 2014 levels out to 2085 (flat line). A value
            ///  of 17 is a ~ 40% in the ADWR estimate out to 2085.
            /// </summary>
            public int set_AgPumpingCurveIndex
            {

                set
                {
                    int checkedValue = RangeCheckAgIndex(value);
                    setAgPumpingCurve_(ref checkedValue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setAgPumpingCurve_(ref int value);
            //
            /// <summary>
            ///  Provider level control of the property above ( set_AgPumpingCurveIndex)
            /// </summary>
            // Need a range check for this property
             protected internal int[] set_ProviderAgCAPandPumpingCurveIndex
            {

                set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setProviderAgCreditCurvePCT_(ref num, ExpandProvNames35(citiModel));                  
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setProviderAgCreditCurvePCT_(ref int count, int[] values);

        /// <summary>
        /// 01.28.15
        ///  The percentage of credits that can be transfered from Ag to Muni - default is 80%
        /// </summary>
            public int set_AgCreditTransferPCT
            {

                set
                {
                    int checkedValue = RangeCheckAgCreditTransferPCT(value);
                    setAgCreditTransferPCT_(ref checkedValue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setAgCreditTransferPCT_(ref int value);
            //
            /// <summary>
            /// 01.30.15 (NO!)
            ///  An estimate of actual water used by Agriculture based on land cover land use (now)
            ///  NOW USED 06.17.16 but NOT in API
            /// </summary>
            /// 05.16.17 Commented Out
            /// 
            //protected internal int[] get_AgPumpingTotal_AF
            //{

            //    get
            //    {
            //        int num = cNumFortranProviders;
            //        int[] AgriculturePumped = new int[num];
            //        getWaterFromAgPumpingTotal_(ref num, AgriculturePumped);
            //        return TrimProvNameS(AgriculturePumped);

            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected  static extern int getWaterFromAgPumpingTotal_(ref int count, int[] values);
            //
           /// <summary>
           ///  Our current Sustainability Indicator for agriculture water. This reflects the amount
           ///  of Ag water used relative to (2014) pumping or surface water deliveries. This includes
           ///  CAP, SRP, and Groundwater. NOTE: WE DO NOT modify SRP ("other") water in the model
           ///  due to the uncertainty, at this time. 
           ///  02.17.15 DAS7
           /// </summary>
            protected internal int[] get_AgWaterUsedRelativeTo2014_PCT
            {

                get
                {
                    int num = cNumFortranProviders;
                    int[] AgricultureWaterRatio_PCT = new int[num];
                    int[] temp = new int[num];
                    getAgCreditTransPCT_(ref num, AgricultureWaterRatio_PCT);

                    return TrimProvNameS(AgricultureWaterRatio_PCT);
                    // return TrimProvNameS(temp);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
          //  protected static extern int getAgWaterToPotential_PCT_(ref int count, int[] values);
            protected static extern int getAgCreditTransPCT_(ref int count, int[] values);
        
            // New SI 11.20.16
            protected internal int get_AgProductionPCT
            {

                get
                {
                    int gvalue = 0;
                    int myInt = getAgProductionPCT_(ref gvalue);
                    return myInt;
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getAgProductionPCT_(ref int value);
            //
            public int set_AgEfficiency
            {

                set
                {
                    int checkedValue = RangeCheckAgEfficiency(value);
                    setAgEfficiency_(ref checkedValue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setAgEfficiency_(ref int value);


            //
            #endregion
            //-----------------------------------------------------------------------------------//
            #region Environmental Water
            //
            /// <summary>
            ///  January 2015 code- set in Web Interface file
            /// </summary>
            protected internal int set_FlowToCOdelta
            {
                set
                {
                    int checkedValue = RangeCheckEnvCo(value);
                    setWaterToCOdelta_(ref checkedValue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setWaterToCOdelta_(ref int value);
            //
            /// <summary>
            /// Provider level equilivant to set_FlowToCOdelta, designed for the DecisionGame code
            /// 02.10.15 DAS
            /// </summary>
            protected internal int[] set_ProviderFlowToCOdelta
            {
                set
                {
                     int num = cNumFortranProviders;
                    citiModel = value;
                    setProviderFlowToCOdelta_AF_(ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setProviderFlowToCOdelta_AF_(ref int count, int[] values);
            //
        /// <summary>
        /// THis was the VinceJohnston bool that I renamed today. 02.10.15 DAS
        /// It triggers code in the model that corresponds to the Decision Game model that
        /// we are integrating herein
        /// </summary>
            protected internal bool set_modelDecisionGame
            {
                set
                {
                    set_parmVinze_Johnston_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void set_parmVinze_Johnston_(ref bool value);


            //
            ///<summary> This bool determines whether AZ (Muni M&I only) is burdened with all the water cost
            /// for the CO Delta project (158088 AF year-1)=true or if the costs are shared proportionally
            /// </summary>
            // Default is true, Set to false to enforce proportional sharing among the three states
            // plus Mexico
            protected internal bool set_COdeltaBurden
            {
                set
                {
                    setCODeltaBurden_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setCODeltaBurden_(ref bool value);
            //
            /// <summary>
            ///  The environmental Sustainability Indicator
            ///  02.17.15 DAS
            /// </summary>
            protected internal int get_COdeltaRatio
            {
                get
                {
                    int gvalue = 0;
                    int myInt = getRatioCOdeltaBurden_(ref gvalue);
                    return myInt;
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getRatioCOdeltaBurden_(ref int value);


            //
            /// <summary>
            /// A beta variable created for the Ajay and Johnston study, this variable removes 
            /// a portion of the flow on the Colorado River from use by the Res/Com/Industrial 
            /// users. Currently line 519 in WaterShed_CO.f90. At present, cannot exceed 15% of
            /// the total flow. Units are Acre-feet per annum.
            /// </summary>
            protected internal int set_FlowToEnvironment_CO
            {
                set
                {
                    int checkedValue = RangeCheckEnvCo(value);
                    setWaterToEvironmentCO_(ref checkedValue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setWaterToEvironmentCO_(ref int value);
            //
            // 
            /// <summary>
            ///  A beta variable created for the Ajay and Johnston study, this variable removes 
            /// a portion of the flow on the Verde River from use by the Res/Com/Industrial 
            /// users. Currently line 361 in DailyDesignations.f90. At present, cannot exceed 15% of
            /// the total flow. Units are Acre-feet per annum.
            /// </summary>
            protected internal int set_FlowToEnvironment_Verde
            {
                set
                {
                    int checkedValue = RangeCheckEnvVerde(value);
                    setWaterToEvironmentVerde_(ref checkedValue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setWaterToEvironmentVerde_(ref int value);
            //
            // 
            /// <summary>
            /// A beta variable created for the Ajay and Johnston study, this variable removes 
            /// a portion of the flow on the Salt River from use by the Res/Com/Industrial 
            /// users. Currently line 361 in DailyDesignations.f90. At present, cannot exceed 15% of
            /// the total flow. Units are Acre-feet per annum.
            /// </summary>
            protected internal int set_FlowToEnvironment_Salt
            {
                set
                {
                    int checkedValue = RangeCheckEnvSalt(value);
                    setWaterToEvironmentSalt_(ref checkedValue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setWaterToEvironmentSalt_(ref int value);
            //
        /// <summary>
        /// Arizona's contribution (af a-1) to the CO River Delta
        /// </summary>
            protected internal int get_AZshareCOdeltaWater
            {
                get
                {

                    int gvalue = 0;
                    int myInt = getAZshareCOdeltaWater_(ref gvalue);
                    return myInt;

                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getAZshareCOdeltaWater_(ref int value);

            //
            protected internal int get_CAshareCOdeltaWater
            {
                get
                {

                    int gvalue = 0;
                    int myInt = getCAshareCOdeltaWater_(ref gvalue);
                    return myInt;

                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getCAshareCOdeltaWater_(ref int value);

            //
            protected internal int get_NVshareCOdeltaWater
            {
                get
                {

                    int gvalue = 0;
                    int myInt = getNVshareCOdeltaWater_(ref gvalue);
                    return myInt;

                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getNVshareCOdeltaWater_(ref int value);

            //
            protected internal int get_MXshareCOdeltaWater
            {
                get
                {

                    int gvalue = 0;
                    int myInt = getMXshareCOdeltaWater_(ref gvalue);
                    return myInt;

                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getMXshareCOdeltaWater_(ref int value);
            //
            //
            /// <summary>
            /// Provider level estimates of the ratio of water allocated to the CO river delta
            /// and the total AZ share
            /// 04.07.15 DAS
            /// </summary>
            protected internal int[] get_ProviderCOdeltaRatio
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] ProviderCOdeltaRatio = new int[num];
                    getProviderCOdelataRatioAZ_(ref num, ProviderCOdeltaRatio);
                    return WaterSimU.TrimProvNameS(ProviderCOdeltaRatio);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getProviderCOdelataRatioAZ_(ref int count, int[] values);

            #endregion
            //-----------------------------------------------------------------------------------//
            #region Policy-Provider Inputs-Changed: summary 
            // 08.31.12 - validated on 09.04.12
            /// <summary>
            /// When surface water availability reaches 80% of standard then this switch
            /// is invoked, allowing providers to pump as much groundwater as they need to 
            /// meet demand.
            /// </summary>
            protected internal bool[] get_EightyPercentRule
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_intEightyPercentRule = new int[num];
                    bool[] temp_Eighty = new bool [num];
                    geteightyPctRule_(ref num, get_intEightyPercentRule);

                    for (int i = 0; i < num; i++)
                    {
                    temp_Eighty[i] = Convert.ToBoolean(get_intEightyPercentRule[i]);
                    }
                    return WaterSimU.TrimProvNameB(temp_Eighty);
                }
            }

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void geteightyPctRule_(ref int count, int[] values);
            //
            /// <summary>
            ///  The lag, in years, for water deposited on the surface to reach the aquifer.
            /// </summary>
            protected internal int[] TimeLagVadoseYears
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] TimeLagVadoseYears = new int[num];
                    getTimeLagVadoseYears_(ref num, TimeLagVadoseYears);
                    return WaterSimU.TrimProvNameS(TimeLagVadoseYears);
                }
                set
                {
                    int pdefault = TimeLagForVadoseYears;
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setTimeLagVadoseYears_(ref pdefault, ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getTimeLagVadoseYears_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setTimeLagVadoseYears_(ref int pdefault, ref int count, int[] values);

            // 1 = read from file (on disk)
            // 2 = six-year average GPCD used to adjust new provider demand based on 2006 GPCD by provider (silly at this point)
            // 3 = SES estimate of GPCD with population
            // 4 = SES and interface changes to GPCD
            /// <summary>
            /// Property that informs which demand option to use. 1) Read from file, 2=five-year average GPCD
            /// and population, 3=SES estimate (first five years from Water Provider data), 4=same but GPCD may
            /// be input into the model from the interface.
            /// </summary>
            protected internal int ProviderDemandOption
            {
                get
                {
                    int gvalue = 0;
                    return getDemandOption_(ref gvalue);
                }
                set
                {
                    int checkedValue = RangeCheckDemandOption(value);
                    setDemandOption_(ref checkedValue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getDemandOption_(ref int value);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setDemandOption_(ref int value);
            //
            /// <summary>
            /// NOT currrently being used as of 09.04.12
            /// </summary>
            protected internal int DemandPercent
            {
                get
                {
                    int gvalue = 0;
                    return getDemandPercent_(ref gvalue);
                }
                set
                {
                    int gvalue = RangeCheckDemandPercent(value);
                    setDemandPercent_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getDemandPercent_(ref int value);
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setDemandPercent_(ref int value);
            //
            /// <summary>
            /// Water Banking Option: =1) then use "unused" surface water to bank at a rate
            /// gvf_parm_SWtoWB(i) [used as a proportion in the model but as a percent in the
            /// interface. =2) Bank a set amount as gvf_parm_SWtoWBamount(i) in the model but as
            /// public int[] get_parmSurfaceWaterToWBankAmt in this interface (units=AF).
            /// </summary>
            protected internal int[] WaterBankingOption
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] WaterBankingOption = new int[num];
                    getWaterBankingOption_(ref num, WaterBankingOption);
                    return WaterSimU.TrimProvNameS(WaterBankingOption);
                }
                set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setWaterBankingOption_(ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getWaterBankingOption_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setWaterBankingOption_(ref int count, int[] values);

            // AF annum-1
            // 02.21.13 DAS added

            /// <summary>   Sets the set new water supplies. </summary>
            /// <value> The set new water supplies. AF annum-1</value>
            /// <summary>
            ///  As new supplies. "Magical" water
            /// </summary>
            protected internal int[] NewWaterSupplies
            {
                  set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setNewWaterSupplies_(ref num, ExpandProvNames35(citiModel));
                }
                get
                {
                    int num = cNumFortranProviders;
                    int[] NewWaterSupplies = new int[num];
                    getNewWaterSupplies_(ref num, NewWaterSupplies);
                    return WaterSimU.TrimProvNameS(NewWaterSupplies);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setNewWaterSupplies_(ref int count, int[] values);
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getNewWaterSupplies_(ref int count, int[] values);
            //
            // 01.31.19 david arthur sampson
            int[] zero = new int[0];
            /// <summary>
            ///  So, this is water augmentation, but water used in the demand structure
            ///  i.e., not all water requests are granted- water demand and supply drive
            ///  whether augmented water is used. And, now, we are adding Agriculture
            ///  surface water diversions to Municipal water use. So, this gets counted as 
            ///  augmented water
            ///  03.15.14 DAS
            /// </summary>
            
            protected internal int[] get_NewWaterSuppliesUsed
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] NewWaterSuppliesUsed = new int[num];
                    getNewWaterSuppliesUsed_(ref num, NewWaterSuppliesUsed);
                    return WaterSimU.TrimProvNameS(NewWaterSuppliesUsed);
 
                 }
                set
                {
                    int num = cNumFortranProviders;
                    citiModel = zero;
                    setNewWaterSupplies_(ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getNewWaterSuppliesUsed_(ref int count, int[] values);
 

            //
            #endregion
            //-----------------------------------------------------------------------------------//
            #region CityModel: summary
             #region Parameters: summary

              //



            //
            /// <summary>   The citi model. </summary>
            private int[] citiModel = new int[33];

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter waste water to the reclaimed waste water treatment plant. </summary>
            ///
            /// <value> The parameter waste water to the reclaimed waste water treatment plant. Units are 
            ///         percent (0 to 100). If the value is less than 100 the remaining water goes to the
            ///         traditional waste water treatment plant (as effluent). </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////
            protected internal int[] parmWWtoRWWTPpct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmWWtoRWWTPpct = new int[num];
                    getParmWWtoRWWTPPct_(ref num, parmWWtoRWWTPpct);
                    return WaterSimU.TrimProvNameS(parmWWtoRWWTPpct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setParmWWtoRWWTPPct_(ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmWWtoRWWTPPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmWWtoRWWTPPct_(ref int count, int[] values);

            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter reclaimed waste water to the RO plant. </summary>
            ///
            /// <value> The parameter reclaimed waste water. Units are percent (0 to 100). If less than
            ///         100 the remaining water goes to the reclaimed water state variable. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmReclaimedWWtoROpct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmReclaimedWWtoROpct = new int[num];
                    getParmRWWToROPct_(ref num, parmReclaimedWWtoROpct);
                    return TrimProvNameS(parmReclaimedWWtoROpct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setParmRWWToROPct_(ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmRWWToROPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmRWWToROPct_(ref int count, int[] values);
            ////
            /// <summary>
            /// Waste water to Effluent Percent. Units percent (0 to 100). Whatever is not utilized
            /// here defaults to the regional waste water surface discharge (the environment).
            /// </summary>
            protected internal int[] parmWWtoEffluentPct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmWWtoEffluentPct = new int[num];
                    getParmWWtoEffluentPct_(ref num, parmWWtoEffluentPct);
                    return TrimProvNameS(parmWWtoEffluentPct);

                }
                set
                {
                    int pdefault = WasteWaterToEffluentPct;
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setParmWWtoEffluentPct_(ref pdefault, ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmWWtoEffluentPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmWWtoEffluentPct_(ref int mydefault, ref int count, int[] values);
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter effluent to vadose pct. </summary>
            ///
            /// <value> The parameter effluent to vadose pct. Units are percent (0 to 100).</value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmEffluentToVadosePct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmEffluentToVadosePct = new int[num];
                    getParmEffluenttoVadosePct_(ref num, parmEffluentToVadosePct);
                    return TrimProvNameS(parmEffluentToVadosePct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setParmEffluenttoVadosePct_(ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmEffluenttoVadosePct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmEffluenttoVadosePct_(ref int count, int[] values);
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter effluent to power plant pct. </summary>
            ///
            /// <value> The parameter effluent to power plant pct. Units are percent (0 to 100).
            ///         Together, with effluent to vadose, the remaining goes to Agriculture. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmEffluentToPowerPlantPct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmEffluentToPowerPlantPct = new int[num];
                    getParmEffluentToPPlantPct_(ref num, parmEffluentToPowerPlantPct);
                    return TrimProvNames(parmEffluentToPowerPlantPct);

                }
                set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setParmEffluentToPPlantPct_(ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmEffluentToPPlantPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmEffluentToPPlantPct_(ref int count, int[] values);

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter reclaimed to vadose pct. This is the reclaimed water
            ///             sent to the vadose. Four paths for Reclaimed water: 1) to output (to be used in the
            ///             next simulation year, 2) direct injection (into the aquifer, 3) to the vadose (time
            ///             lag before it reaches the aquifer, or 4) the difference (to regional waste water
            ///             surfaces discharge- to the environment. </summary>
            ///
            /// <value> The parameter reclaimed to vadose pct. In apposition to reclained to direct injection,
            ///         and to output, all three may add to a value of 100.  If not, the remaining goes to the
            ///         surface discharge (units are percent- 0 to 100).</value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmReclaimedToVadosePct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmReclaimedToVadosePct = new int[num];
                    getParmReclaimedToVadosePct_(ref num, parmReclaimedToVadosePct);
                    return TrimProvNameS(parmReclaimedToVadosePct);
                }
                set
                {
                    int numProviders = cNumFortranProviders;
                    citiModel = value;
                    setParmReclaimedToVadosePct_(ref numProviders, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmReclaimedToVadosePct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmReclaimedToVadosePct_(ref int count, int[] values);
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter reclaimed to direct inject pct. Four paths for Reclaimed
            ///              water: 1) to output (to be used in the
            ///             next simulation year, 2) direct injection (into the aquifer, 3) to the vadose (time
            ///             lag before it reaches the aquifer, or 4) the difference (to regional waste water
            ///             surfaces discharge- to the environment.</summary>
            ///
            /// <value> The parameter reclaimed to direct inject pct. Reclaimed water can go to
            ///         output, or to vadose, or to direct injection. The remaining (if less than 100)
            ///         goes to Regional Waste Water Surfaces Discharge.</value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmReclaimedToDirectInjectPct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmReclaimedToDirectInjectPct = new int[num];
                    getParmReclaimedToDirectInjPct_(ref num, parmReclaimedToDirectInjectPct);
                    return TrimProvNameS(parmReclaimedToDirectInjectPct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setParmReclaimedToDirectInjPct_(ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmReclaimedToDirectInjPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmReclaimedToDirectInjPct_(ref int count, int[] values);
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>
            /// Gets a value indicating whether or not to get the parameter override shortage.
            /// </summary>
            ///
            /// <value> true if get parameter override shortage, false if not. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal bool get_parmOverrideShortage
            {
                get
                {
                    bool gvalue = false;
                    return gvalue;
                }
             }
  

            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter reclaimed to output pct. Four paths for reclaimed water:
            ///             1) to output (to be used in the
            ///             next simulation year, 2) direct injection (into the aquifer, 3) to the vadose (time
            ///             lag before it reaches the aquifer, or 4) the difference (to regional waste water
            ///             surfaces discharge- to the environmen.t</summary>
            ///
            /// <value> The parameter reclaimed to output pct.  </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmReclaimedToOutputPct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmReclaimedToOutputPct = new int[num];
                    getParmReclaimedToOutputPct_(ref num, parmReclaimedToOutputPct);
                    return TrimProvNameS(parmReclaimedToOutputPct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    int pdefault = ReclaimedToOutputPct;
                    citiModel = value;
                    setParmReclaimedToOutputPct_(ref pdefault, ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmReclaimedToOutputPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmReclaimedToOutputPct_(ref int mydefault, ref int count, int[] values);
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter reclaimed to input pct. </summary>
            ///
            /// <value> The parameter reclaimed to input pct. This parameter tells the model how much of the
            ///         reclaimed to output may be used. Units are percent (0 to 100)</value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmReclaimedToInputPct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmReclaimedToInputPct = new int[num];
                    getParmReclaimedToInputPct_(ref num, parmReclaimedToInputPct);
                    return TrimProvNameS(parmReclaimedToInputPct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    int pdefault = ReclaimedToInputPct;
                    citiModel = value;
                    setParmReclaimedToInputPct_(ref pdefault, ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmReclaimedToInputPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmReclaimedToInputPct_(ref int mydefault, ref int count, int[] values);
            //

            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter ro reclaimed to output pct. i.e., reverse osmosis
            ///             water that can be used in the next simulation time step.  In this case, at present,
            ///             the following year. Two paths for RO water: 1) to output, or 2) to potable water
            ///             which goes into direct injection (straight into the aquifer). </summary>
            ///
            /// <value> The parameter ro reclaimed to output pct. Units are percent (0 to 100). </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmROReclaimedToOutputPct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmROReclaimedToOutputPct = new int[num];
                    getParmROToOutputPct_(ref num, parmROReclaimedToOutputPct);
                    return TrimProvNameS(parmROReclaimedToOutputPct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    int pdefault = ROReclaimedToOutputPct;
                    citiModel = value;
                    setParmROToOutputPct_(ref pdefault, ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmROToOutputPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmROToOutputPct_(ref int mydefault, ref int count, int[] values);
            //
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter surface water to wbank pct. This is water banking water.
            ///             It sets the percent of unused surface water to be sent to a water bank. </summary>
            ///
            /// <value> The parameter surface water to wbank pct. Units are percent (0 to 100). </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmSurfaceWaterToWbankPct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmSurfaceWaterToWbankPct = new int[num];
                    getParmSWtoWBankPct_(ref num, parmSurfaceWaterToWbankPct);
                    return TrimProvNameS(parmSurfaceWaterToWbankPct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setParmSWtoWBankPct_(ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmSWtoWBankPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmSWtoWBankPct_(ref int count, int[] values);
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter surface water to water bank amount. </summary>
            ///
            /// <value> The parameter surface water to w bank amount. Units are acre-feet per year. This
            ///         parameter tells the model how much surface water to move to a water bank account.</value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmSurfaceWaterToWBankAmt
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmSurfaceWaterToWBankAmt = new int[num];
                    getParmSWtoWBankAmt_(ref num, parmSurfaceWaterToWBankAmt);
                    return TrimProvNameS(parmSurfaceWaterToWBankAmt);
                }
                set
                {
                    int num = cNumFortranProviders;
                    int pdefault = SurfaceWaterToWBankAmt; // Acre-feet to begin with default banking
                    citiModel = value;
                    setParmSWtoWBankAmt_(ref pdefault, ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmSWtoWBankAmt_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmSWtoWBankAmt_(ref int mydefault, ref int count, int[] values);
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter surface water to vadose amount. </summary>
            ///
            /// <value> The parameter surface water to vadose amount. Units are acre-feet per year.</value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmSurfaceWaterToVadoseAmt
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmSurfaceWaterToVadoseAmt = new int[num];
                    getParmSWtoVadoseAmt_(ref num, parmSurfaceWaterToVadoseAmt);
                    return TrimProvNameS(parmSurfaceWaterToVadoseAmt);
                }

                set
                {
                    int num = cNumFortranProviders;
                    int pdefault = SurfaceWaterToVadoseAmt; // Acre-feet to befin with default banking
                    citiModel = value;
                    setParmSWtoVadoseAmt_(ref pdefault, ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmSWtoVadoseAmt_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmSWtoVadoseAmt_(ref int mydefault, ref int count, int[] values);
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter water supply to direct inject amount. </summary>
            ///
            /// <value> The parameter water supply to direct inject amount. Units are acre-feet. This parameter
            ///         tells the model how much of the water supply should be sent to the aquifer via
            ///         direct injection. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmWaterSupplyToDirectInjectAmt
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmWaterSupplyToDirectInjectAmt = new int[num];
                    getParmWStoDIAmt_(ref num, parmWaterSupplyToDirectInjectAmt);
                    return TrimProvNameS(parmWaterSupplyToDirectInjectAmt);
                }
                set
                {
                    int num = cNumFortranProviders;
                    int pdefault = WaterSupplyToDirectInjectAmt; // Acre-feet to befin with default banking
                    citiModel = value;
                    setParmWStoDIAmt_(ref pdefault, ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmWStoDIAmt_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmWStoDIAmt_(ref int mydefault, ref int count, int[] values);
            //
            // 06.29.12 DAS

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter outdoor water use residential users pct. </summary>
            ///
            /// <value> The parameter outdoor water use for residential users pct. Units are percent (0 to 100).
            ///        The remaining water is used indoors.</value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmOutdoorWaterUseResPct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmOutdoorWaterUseResPct = new int[num];
                    getParmOutDoorResPct_(ref num, parmOutdoorWaterUseResPct);
                    return TrimProvNameS(parmOutdoorWaterUseResPct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    int pdefault = OutdoorWaterUseResPct;
                    citiModel = value;
                    setParmOutDoorResPct_(ref pdefault, ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmOutDoorResPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmOutDoorResPct_(ref int mydefault, ref int count, int[] values);
            //
            // 06.29.12 DAS

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter outdoor water use for commercial users pct. </summary>
            ///
            /// <value> The parameter outdoor water use for commercial water users as a percent. Units are percent
            ///         (0 to 100) the remaining water is used indoor for the commercial state variable. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmOutdoorWaterUseComPct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmOutdoorWaterUseComPct = new int[num];
                    getParmOutDoorComPct_(ref num, parmOutdoorWaterUseComPct);
                    return TrimProvNameS(parmOutdoorWaterUseComPct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    int pdefault = OutdoorWaterUseComPct;
                    citiModel = value;
                    setParmOutDoorComPct_(ref pdefault, ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmOutDoorComPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmOutDoorComPct_(ref int mydefault, ref int count, int[] values);
            //
            // 06.29.12 DAS

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter outdoor water use industrial users as a pct. </summary>
            ///
            /// <value> The parameter outdoor water use for industrial water users. Units are percent (0 to 100)
            ///         with the remaining water used indoors.</value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmOutdoorWaterUseIndPct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmOutdoorWaterUseIndPct = new int[num];
                    getParmOutDoorIndPct_(ref num, parmOutdoorWaterUseIndPct);
                    return TrimProvNameS(parmOutdoorWaterUseIndPct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    int pdefault = OutdoorWaterUseIndPct;
                    citiModel = value;
                    setParmOutDoorIndPct_(ref pdefault, ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmOutDoorIndPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmOutDoorIndPct_(ref int mydefault, ref int count, int[] values);
            //
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter reclaimed outdoor use pct. </summary>
            ///
            /// <value> The parameter reclaimed outdoor use pct. This is reclaimed water from a previous 
            ///         time-step. It can be used indoor for black water or outdoor. Units are percent 
            ///         (0 to 100). What is not used outdoor is used indoor for the toilet.  </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmReclaimedOutdoorUsePct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmReclaimedOutdoorUsePct = new int[num];
                    getReclaimedOutdoorUse_(ref num, parmReclaimedOutdoorUsePct);
                    return TrimProvNameS(parmReclaimedOutdoorUsePct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    int pdefault = ReclaimedOutdoorUsePct;
                    citiModel = value;
                    setReclaimedOutdoorUse_(ref pdefault, ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getReclaimedOutdoorUse_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setReclaimedOutdoorUse_(ref int mydefault, ref int count, int[] values);
            //
            //
            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter water supply to residential pct. </summary>
            ///
            /// <value> The parameter water supply to residential water users pct.  Units are percent (0 to 100)
            ///         with the remaining potentially going to commercial or industrial water users. 
            ///         Three paths for this water (residential, commercial, or industrial water users). 
            ///          All three MUST add up to 100.</value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmWaterSupplyToResPct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmWaterSupplyToResPct = new int[num];
                    getParmWStoResPct_(ref num, parmWaterSupplyToResPct);
                    return TrimProvNameS(parmWaterSupplyToResPct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    int pdefault = WaterSupplyToRESpct;
                    citiModel = value;
                    setParmWStoResPct_(ref pdefault, ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmWStoResPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmWStoResPct_(ref int mydefault, ref int count, int[] values);
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter water supply to commercial water users pct. </summary>
            ///
            /// <value> The parameter water supply to commercial water state variable as a percent. So, values
            ///         may be zero to 100. Three paths for this water (residential, commercial, or industrial
            ///         water users).  All three MUST add up to 100.</value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmWaterSupplyToComPct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmWaterSupplyToComPct = new int[num];
                    getParmWStoComPct_(ref num, parmWaterSupplyToComPct);
                    return TrimProvNameS(parmWaterSupplyToComPct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    int pdefault = WaterSupplyToCOMpct;
                    citiModel = value;
                    setParmWStoComPct_(ref pdefault, ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmWStoComPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmWStoComPct_(ref int mydefault, ref int count, int[] values);
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter water supply to industrial water users pct. </summary>
            ///
            /// <value> The parameter water supply to the state variable for industrial water as a percent.
            ///         Three paths for this water (residential, commercial, or industrial
            ///         water users).  All three MUST add up to 100. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmWaterSupplyToIndPct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmWaterSupplyToIndPct = new int[num];
                    getParmWStoIndPct_(ref num, parmWaterSupplyToIndPct);
                    return TrimProvNameS(parmWaterSupplyToIndPct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    int pdefault = WaterSupplyToINDpct;
                    citiModel = value;
                    setParmWStoIndPct_(ref pdefault, ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmWStoIndPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmWStoIndPct_(ref int mydefault, ref int count, int[] values);

            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets or sets the parameter groundwater to the groundwater treatment plant pct. </summary>
            ///
            /// <value> The parameter groundwater to gwt plant pct. Units are percent (0 to 100). Two paths for
            ///         this water: 1) either to the groundwater treatment plant, or 2) straight to the water
            ///         supply. Whatever water is not sent to the GWTP defaults to the water supply. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int[] parmGroundwaterToGWTPlantPct
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmGroundwaterToGWTPlantPct = new int[num];
                    getParmGWtoGWTPPct_(ref num, parmGroundwaterToGWTPlantPct);
                    return TrimProvNameS(parmGroundwaterToGWTPlantPct);
                }
                set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setParmGWtoGWTPPct_(ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmGWtoGWTPPct_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmGWtoGWTPPct_(ref int count, int[] values);

            // Certified 07.05.11 das

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>
            /// Sets a value indicating whether the parameter include meteorology should be set.
            /// </summary>
            ///
            /// <value> true if set parameter include meteorology, false if not. This is not used at the 
            ///         moment. Can be used to read meteorological data such as temperature and precipitation
            ///         for the Valley.</value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal bool set_parmIncludeMeteorology
            {
                set
                {
                    bool Meteo = value;
                    setParmIncludeMeteo_(ref Meteo);
                }
            }

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmIncludeMeteo_(ref bool value);
            // ---------------------------------------------------------------------------
            // 

  
            //
            #endregion
             #region Ajay_Johnston: summary
            // Largely for the Johnson and Vince Project
            // new on 07.02.12
            // ================================================================================

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Sets the set parameter rate residential water users leakage pct. </summary>
            ///
            /// <value> The set parameter rate for residential water users. The percent of water received
            ///         that cannot be used because it is leakage. Units are percent (default is about 5.3 percent).
            ///         But, as an integer, must be a whole number. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////
            //protected internal bool set_parmVinze_Johnston
            //{
            //    set
            //    {
            //        bool VJ = value;
            //        set_parmVinze_Johnston_(ref VJ);
            //    }
            //}

            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern void set_parmVinze_Johnston_(ref bool value);
        /// <summary>
        /// NOT IN API
        /// </summary>
            //protected internal int[] parmRateResLeakagePct
            //{
            //    set
            //    {
            //        int num = cNumFortranProviders;
            //        int pdefault = RateResLeakagePct;
            //        citiModel = value;
            //        setRateResLeakage_(ref pdefault, ref num, ExpandProvNames35(citiModel));
            //    }
            //    get
            //    {
            //        int num = cNumFortranProviders;
            //        int[] parmRateResLeakagePct = new int[num];
            //        getRateResLeakage_(ref num, parmRateResLeakagePct);
            //        return TrimProvNameS(parmRateResLeakagePct);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern void setRateResLeakage_(ref int mydefault, ref int count, int[] values);
            ////
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getRateResLeakage_(ref int count, int[] values);
 
            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Sets the set parameter black water for residential water users as a pct. </summary>
            ///
            /// <value> The set parameter black water percent (0 to 100). HOWEVER, this is a percentage of
            ///         indoor water use. Default is about 25 %.</value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////
            //
            // NOT in API
            protected internal int[] parmBlackWaterResPct
            {
                set
                {
                    int num = cNumFortranProviders;
                    int pdefault = RateBlackWaterPct;
                    citiModel = value;
                    setParmBlackWaterPct_(ref pdefault, ref num, ExpandProvNames35(citiModel));
                }
                get
                {
                    int num = cNumFortranProviders;
                    int[] parmBlackWaterResPct = new int[num];
                    getParmBlackWaterPct_(ref num, parmBlackWaterResPct);
                    return TrimProvNameS(parmBlackWaterResPct);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmBlackWaterPct_(ref int mydefault, ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getParmBlackWaterPct_(ref int count, int[] values);
 
            // Commercial water for golf and public parks

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Sets the set commercial turf pct. </summary>
            ///
            /// <value> The set commercial turf pct. I created this parameter so that we can estimate the 
            ///         amount of commercial water (outdoor) that will be used for turf irrigation.  </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////
            //
            // NOT in API
            //
            protected internal int set_CommercialTurfPct
            {

                set
                {
                    int checkedValue = RangeCheckAg(value);
                    setWaterCommercialTurf_(ref checkedValue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setWaterCommercialTurf_(ref int value);
            //
             // Commercial water for Agriculture

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Sets the set water to agriculture in units af (acre-feet per annum). </summary>
            ///
            /// <value> The set water to agriculture af. This is water that becomes a demand source such that
            ///         it removes water from the source stream.  i.e., it adds to the total demand for a 
            ///         water provider. Thus, water, from whatever source, must account for agricultural water
            ///         (it has the highest priority in the demand scheme). </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////
  

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the get showers baths minutes pcd (minutes per capita per day). </summary>
            ///
            /// <value> The get showers and baths (lumped in this case) units minutes per capital per day
            ///         allowed based on the amount of water allocated to showers and baths. See directly
            ///         below (i.e., set_ShowersBathsPCT).</value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////
            //
            // NOW in API
            // 07.27.16 
            protected internal int [] ShowersBathsMinPCD
            {
                get
                {
                  
                    int num = cNumFortranProviders;
                    int[] showers = new int[num];
                    getShowerBathPCD_(ref num, showers);
                    return TrimProvNameS(showers);

                }
  
             }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getShowerBathPCD_(ref int count, int[] values);
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Sets the set showers baths as a percentage of indoor water use. </summary>
            ///
            /// <value> The set showers baths pct.  Units are percent (0 to 100 possible). But, realistically,
            ///         this should be about 16 to 20 %.</value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////
            //
            // NOT in API
            //
            protected internal int set_ShowersBathsPCT
            {
                set
                {
                    int checkedValue = RangeCheckShowerBath(value);
                    setShowerBathPCT_(ref checkedValue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setShowerBathPCT_(ref int value);
            //

             ////////////////////////////////////////////////////////////////////////////////////////////////////
             /// <summary>  Gets the get flushes pcd (per capita per day). </summary>
             ///
             /// <value>    Returns the number of toilet flushes per capita per day based on the amount of black
            ///            water allocated. Or, set as "parmBlackWaterResPct", a percentage of indoor water
            ///            use for residential water users.</value>
             ////////////////////////////////////////////////////////////////////////////////////////////////////
            // NOT in API
             internal double[] get_FlushesPCD
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] myGet = new int[num];
                    getFlushesPCD_(ref num, myGet);
                    myGet = TrimProvNameS(myGet);
                    double [] myConvert = new double[cNumModeledProviders];
                    for (int i = 0; i < cNumModeledProviders; i++)
                    {
                         myConvert[i] = Convert.ToInt32(myGet[i]);
                         myConvert[i] = myConvert[i] * 0.01;
                    }
                    return myConvert;
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getFlushesPCD_(ref int count, int[] values);

             #endregion
            #endregion
            //-----------------------------------------------------------------------------------//
            #region Groundwater Inputs-changed
            /// <summary>
            /// This sets whether providers pump unlimited groundwater (value=true) or
            /// they are restricted to their credits (value-false)
            /// </summary>
                protected internal bool set_parmAllStrawsSucking
                {
                    set
                    {
                        bool Straws = value;
                        setParmAllStraws_(ref Straws);
                    }
                }

                [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
                protected static extern void setParmAllStraws_(ref bool value);
            /// <summary>
            /// Default pumping by the providers as a percentage of their total demand
            /// 07.30.15 
            /// </summary>
                protected internal int[] parmDefaultPumpingMandIPct
                {
                    get
                    {
                        int num = cNumFortranProviders;
                        int[] DefaultPumpingMandIPCT = new int[num];
                        getDefaultPumpingMandIPct_(ref num, DefaultPumpingMandIPCT);
                        return WaterSimU.TrimProvNameS(DefaultPumpingMandIPCT);
                    }
                    set
                    {
                        int numbers = cNumModeledProviders;
                        int[] myout = new int[numbers];
                        myout = value;
                        setDefaultPumpingMandIPct_(ref numbers, ExpandProvNames35(myout));
                    }
                }
                [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
                protected static extern int getDefaultPumpingMandIPct_(ref int count, int[] values);

                [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
                protected static extern void setDefaultPumpingMandIPct_(ref int count, int[] values);


            #endregion
            //-----------------------------------------------------------------------------------//
            #region Sensitivity
            //protected internal int set_Years
            //{
            //    set
            //    {
            //        int check = value;
            //        setYears_(ref check);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern void setYears_(ref int value);
            ////

            //    protected internal void RunManyYears()
            //    {
            //        RunManyYears_();
            //    }
            //    [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //    protected static extern void RunManyYears_();

            //    /// <summary>
            //    /// Sets the number of iterations of possible values in the parameter distribution(s)
            //    /// </summary>
            //    protected internal int set_NparameterValues
            //    {
            //        set
            //        {
            //            int check = value;
            //            setNparameterValues_(ref check);
            //        }
            //    }
            //    [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //    protected static extern void setNparameterValues_(ref int value);
            //    //
            //    protected internal int set_Nparameters
            //    {
            //        set
            //        {
            //            int check = value;
            //            setNparameters_(ref check);
            //        }
            //    }
            //    [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //    protected static extern void setNparameters_(ref int value);
            //    //

            //    protected internal bool set_parmPassRunManyYears
            //    {
            //        set
            //        {
            //            bool Pass = value;
            //            setPassRunMayYear_(ref Pass);
            //        }
            //    }

            //    [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //    protected static extern void setPassRunMayYear_(ref bool value);
            #endregion
            //-----------------------------------------------------------------------------------//
            #region City Model Outputs-Changed

 

            /// <summary>
            /// Output "Properties" for the City Model
            /// </summary>
            //

            protected internal int[] get_GroundwaterBankUsed  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_GroundwaterBankUsed = new int[num];
                    getGWbankedUsed_(ref num, get_GroundwaterBankUsed);
                    return TrimProvNames(get_GroundwaterBankUsed);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getGWbankedUsed_(ref int count, int[] values);
            //
            protected internal int[] get_GroundwaterBankBalance  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_GroundwaterBankBalance = new int[num];
                    getGWbanked_(ref num, get_GroundwaterBankBalance);
                    return TrimProvNames(get_GroundwaterBankBalance);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getGWbanked_(ref int count, int[] values);
            //

            /// <summary>
            /// Total Reclaimed water produced for the year
            /// </summary>
            //
            //protected internal int[] get_ReclaimedWaterTotal // 
            //{
            //    get
            //    {
            //        int num = cNumFortranProviders;
            //        int[] get_ReclaimedWaterTotal = new int[num];
            //        getReclaimedWaterTotal_(ref num, get_ReclaimedWaterTotal);
            //        return TrimProvNames(get_ReclaimedWaterTotal);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getReclaimedWaterTotal_(ref int count, int[] values);
            //
            // changed from  protected internal int[] ReclaimedWaterOutput
            protected internal int[] get_ReclaimedWaterCreated  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] ReclaimedWaterCreated = new int[num];
                    getReclaimedWaterTotal_(ref num, ReclaimedWaterCreated);
                    return TrimProvNames(ReclaimedWaterCreated);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getReclaimedWaterTotal_(ref int count, int[] values);

            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getReclaimedWaterOut_(ref int count, int[] values);
            //
            protected internal int[] get_ReclaimedWaterUsed  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_ReclaimedWaterUsed = new int[num];
                    getReclaimedWaterUsed_(ref num, get_ReclaimedWaterUsed);
                    return TrimProvNames(get_ReclaimedWaterUsed);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getReclaimedWaterUsed_(ref int count, int[] values);
            //
    
            //
            protected internal int[] get_ReclaimedWaterToVadose  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_ReclaimedWaterToVadose = new int[num];
                    getReclaimedWaterToVadose_(ref num, get_ReclaimedWaterToVadose);
                    return TrimProvNames(get_ReclaimedWaterToVadose);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getReclaimedWaterToVadose_(ref int count, int[] values);
            //
            protected internal int[] get_ReclaimedWaterDischarged  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_ReclaimedWaterDischarged = new int[num];
                    getReclaimedWaterDischarged_(ref num, get_ReclaimedWaterDischarged);
                    return TrimProvNames(get_ReclaimedWaterDischarged);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getReclaimedWaterDischarged_(ref int count, int[] values);
            //
            protected internal int[] get_ReclaimedWaterDirectInject  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_ReclaimedWaterDirectInject = new int[num];
                    getReclaimedWaterDInject_(ref num, get_ReclaimedWaterDirectInject);
                    return TrimProvNames(get_ReclaimedWaterDirectInject);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getReclaimedWaterDInject_(ref int count, int[] values);
            //
            // Changed from  protected internal int[] ROreclaimedWaterOutput
            protected internal int[] get_ROreclaimedWaterCreated  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] ROreclaimedWaterOutput = new int[num];
                    getROReclaimedWaterOut_(ref num, ROreclaimedWaterOutput);
                    return TrimProvNames(ROreclaimedWaterOutput);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getROReclaimedWaterOut_(ref int count, int[] values);
            //
            protected internal int[] get_ROreclaimedWaterUsed  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_ROreclaimedWaterUsed = new int[num];
                    getROReclaimedWaterUsed_(ref num, get_ROreclaimedWaterUsed);
                    return TrimProvNames(get_ROreclaimedWaterUsed);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getROReclaimedWaterUsed_(ref int count, int[] values);
            //
            protected internal int[] get_ROreclaimedWaterDirectInject  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_ROreclaimedWaterDirectInject = new int[num];
                    getROReclaimedWaterDI_(ref num, get_ROreclaimedWaterDirectInject);
                    return TrimProvNames(get_ROreclaimedWaterDirectInject);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getROReclaimedWaterDI_(ref int count, int[] values);
            //
            protected internal int[] get_TotalTWWTP  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_TWWTPCreated = new int[num];
                    getTWWTPCreated_(ref num, get_TWWTPCreated);
                    return TrimProvNames(get_TWWTPCreated);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getTWWTPCreated_(ref int count, int[] values);

            protected internal int[] get_TotalEffluentReused  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_EffluentCreated = new int[num];
                    getEffluentCreated_(ref num, get_EffluentCreated);
                    return TrimProvNames(get_EffluentCreated);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getEffluentCreated_(ref int count, int[] values);
            //
            protected internal int[] get_EffluentToPowerPlant  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_EffluentToPowerPlant = new int[num];
                    getEffluentToPowerPlant_(ref num, get_EffluentToPowerPlant);
                    return TrimProvNames(get_EffluentToPowerPlant);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getEffluentToPowerPlant_(ref int count, int[] values);
            //
            protected internal int[] get_EffluentToVadose  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_EffluentToVadose = new int[num];
                    getEffluentToVadose_(ref num, get_EffluentToVadose);
                    return TrimProvNames(get_EffluentToVadose);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getEffluentToVadose_(ref int count, int[] values);
            //
            protected internal int[] get_EffluentDischarged  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_EffluentDischarged = new int[num];
                    getEffluentToDischarge_(ref num, get_EffluentDischarged);
                    return TrimProvNames(get_EffluentDischarged);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getEffluentToDischarge_(ref int count, int[] values);
            //
            protected internal int get_EffluentToAgriculture
            {
                get
                {
                    int gvalue = 0;
                    return getEffluentToAg_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getEffluentToAg_(ref int value);
            //

            /// <summary>   Gets the get demand deficit: This Is unmet Water Demand </summary>
            /// <value> Found in Provider: subroutine aDemandSupply(T,vGP,vLP)
            ///         Units are AF annum-1 </value>
            
            protected internal int[] get_DemandDeficit  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_DemandDeficit = new int[num];
                    getDemandDeficit_(ref num, get_DemandDeficit);
                    return TrimProvNames(get_DemandDeficit);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getDemandDeficit_(ref int count, int[] values);


            #endregion       
            //-----------------------------------------------------------------------------------//
            #region Provider Outputs-Changed
            //
              #region surface water-changed
            // CO River
            //
            // ---------------------------------------------------------------------------
            //  Annual deliveries of CO river water via get_CAP_BO designations (acft a-1)
            //
            //              [CERTIFIED- 02.09.10 das] 
            protected internal int[] get_ColoradoAnnualDeliveries  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] get_ColoradoAnnualDeliveries = new int[numProviders];
                    getCOAnnualDeliveries_(ref numProviders, get_ColoradoAnnualDeliveries);

                    return TrimProvNames(get_ColoradoAnnualDeliveries); // Contract 35 to 33
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getCOAnnualDeliveries_(ref int count, int[] values);
            //
            //  NOTE: NOW used in the API as of 07.28.15 DAS
            // -------------------------------------------------------
            protected internal int[] get_ColoradoMaxDeliveries  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] get_ColoradoMaxDeliveries = new int[numProviders];
                    getCOMaxDeliveries_(ref numProviders, get_ColoradoMaxDeliveries);

                    return TrimProvNames(get_ColoradoMaxDeliveries); // Contract 35 to 33
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getCOMaxDeliveries_(ref int count, int[] values);
            //
            // -------------------------------------------------------
            protected internal int[] get_ColoradoUnusedPriority4  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] get_ColoradoUnusedPriority4 = new int[numProviders];
                    getCAPpriority4NotUsed_AF_(ref numProviders, get_ColoradoUnusedPriority4);

                    return TrimProvNames(get_ColoradoUnusedPriority4); // Contract 35 to 33
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getCAPpriority4NotUsed_AF_(ref int count, int[] values);
            //
              // -------------------------------------------------------
            protected internal int[] get_ColoradoUnusedPriority5  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] get_ColoradoUnusedPriority5 = new int[numProviders];
                    getCAPpriority5NotUsed_AF_(ref numProviders, get_ColoradoUnusedPriority5);

                    return TrimProvNames(get_ColoradoUnusedPriority5); // Contract 35 to 33
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getCAPpriority5NotUsed_AF_(ref int count, int[] values);
            //
            // -------------------------------------------------------
            protected internal int[] get_CAPlossPotentialPriority4  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] get_CAPlossPotentialPriority4 = new int[numProviders];
                    getCAPlossPotentialPriority4_AF_(ref numProviders, get_CAPlossPotentialPriority4);

                    return TrimProvNames(get_CAPlossPotentialPriority4); // Contract 35 to 33
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getCAPlossPotentialPriority4_AF_(ref int count, int[] values);
            //
            // -------------------------------------------------------
            protected internal int[] get_CAPlossPotentialPriority5  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] get_CAPlossPotentialPriority5 = new int[numProviders];
                    getCAPlossPotentialPiority5_AF_(ref numProviders, get_CAPlossPotentialPriority5);

                    return TrimProvNames(get_CAPlossPotentialPriority5); // Contract 35 to 33
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getCAPlossPotentialPiority5_AF_(ref int count, int[] values);
            //
            // -------------------------------------------------------
           // 08.04.15 das
            //protected internal int[] get_CAPdemandNotMetPriority4  // af for the year by provider
            //{
            //    get
            //    {
            //        int numProviders = cNumFortranProviders;
            //        int[] get_CAPdemandNotMetP4 = new int[numProviders];
            //        getCAPdemandNotMetPriority4_AF_(ref numProviders, get_CAPdemandNotMetP4);

            //        return TrimProvNames(get_CAPdemandNotMetP4); // Contract 35 to 33
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern void getCAPdemandNotMetPriority4_AF_(ref int count, int[] values);
            ////
            //// -------------------------------------------------------
            //protected internal int[] get_CAPdemandNotMetPriority5  // af for the year by provider
            //{
            //    get
            //    {
            //        int numProviders = cNumFortranProviders;
            //        int[] get_CAPdemandNotMetP5 = new int[numProviders];
            //        getCAPdemandNotMetPriority5_AF_(ref numProviders, get_CAPdemandNotMetP5);

            //        return TrimProvNames(get_CAPdemandNotMetP5); // Contract 35 to 33
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern void getCAPdemandNotMetPriority5_AF_(ref int count, int[] values);
            //
            // -------------------------------------------------------
           // protected internal int[] get_CAPp5Available  // af for the year by provider
           // {
           //     get
           //     {
           //         int numProviders = cNumFortranProviders;
           //         int[] get_CAPp5Available = new int[numProviders];
           //         getCAPpriority5Available_AF_(ref numProviders, get_CAPp5Available);

           //         return TrimProvNames(get_CAPp5Available); // Contract 35 to 33
           //     }
           // }
           // [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
           // protected static extern void getCAPpriority5Available_AF_(ref int count, int[] values);
           // //
           // // -------------------------------------------------------
           // protected internal int[] get_CAPp4Available  // af for the year by provider
           // {
           //     get
           //     {
           //         int numProviders = cNumFortranProviders;
           //         int[] get_CAPp4Available = new int[numProviders];
           //         getCAPpriority4Available_AF_(ref numProviders, get_CAPp4Available);

           //         return TrimProvNames(get_CAPp4Available); // Contract 35 to 33
           //     }
           // }
           // [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
           //// protected static extern void getCOunusedPriority4_(ref int count, int[] values);
           // protected static extern void getCAPpriority4Available_AF_(ref int count, int[] values);
            //



            // -------------------------------------------------------
 

            // SVT Rivers
            // ---------------------------------------------------------------------------
            // 
            // 
            // New CODE in the MODEL- Salt-Tonto Rivers separated from the Verde River
            //
            // Verde
            protected internal int get_VerdeAnnualFlow
            {
                get
                {
                    int gvalue = 0;
                    return getVerdeFlow_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getVerdeFlow_(ref int value);

    
            //

            /// <summary>   Gets the verde class A water used. </summary>
            /// <value> The verde class A water (AF year-1). </value>
            /// NOT in API
            protected internal int get_VerdeClassAwaterUsed
            {
                get
                {
                    int gvalue = 0;
                    return getVerdeClassAwaterUsed_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getVerdeClassAwaterUsed_(ref int value);

            /// <summary>   Gets the verde class B and C water used. </summary>
            /// <value> The verde class B and C water used (AF annum-1). </value>
            /// NOT in API
            protected internal int get_VerdeClassBCwaterUsed
            {
                get
                {
                    int gvalue = 0;
                    return getVerdeClassBCwaterUsed_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getVerdeClassBCwaterUsed_(ref int value);

            //
            //                  Salt-Tonto
            //
            protected internal int get_SaltTontoAnnualFlow
            {
                get
                {
                    int gvalue = 0;
                    return getSaltTontoFlow_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSaltTontoFlow_(ref int value);

            protected internal int get_SaltTontoStorageAnnual
            {
                get
                {
                    int gvalue = 0;
                    return getSaltTontoStorage_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSaltTontoStorage_(ref int value);
            //
            // NOT in API
            //
            protected internal int get_SaltTontoClassAwaterUsed
            {
                get
                {
                    int gvalue = 0;
                    return getSTClassAwaterUsed_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSTClassAwaterUsed_(ref int value);
            //
            // NOT in API
            //
            protected internal int get_SaltTontoClassBCwaterUsed
            {
                get
                {
                    int gvalue = 0;
                    return getSaltTontoClassBCwaterUsed_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSaltTontoClassBCwaterUsed_(ref int value);


            //  Annual deliveries of SVT river water via Class A,B,and C designations (acft a-1)
            // 
            // 02.09.12,02.20.12
            protected internal int[] get_SVTAnnualDeliveriesSRP  // af for the year by provider
            {
                get
                {
                    int num = 10;
                    int[] SVAnnualDeliveries = new int[num];
                    getSRPAnnualDeliveries_(ref num, SVAnnualDeliveries);
                    int[] get_SVTAnnualDeliveriesSRP = ExpandToAllProviders(SVAnnualDeliveries); // Expand 10 to 33
                    return get_SVTAnnualDeliveriesSRP;
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getSRPAnnualDeliveries_(ref int count, int[] values);
            //
  
            //
            //  Maximum, class A,B,C, and NCS
            // Not used in the API as of 02.09.12 DAS
            // NOT certified
            // =======================================
            protected internal int[] get_SaltVerdeMaxDeliveries   // af for the year by provider
            {
                get
                {
                    // 
                    int numProviders = 10;
                    int[] SaltVerdeMaxDeliveries10 = new int[numProviders];
                    getSVMaxDeliveriesSRP_(ref numProviders, SaltVerdeMaxDeliveries10);
                    int[] get_SaltVerdeMaxDeliveries = ExpandToAllProviders(SaltVerdeMaxDeliveries10); // Expand 10 to 33

                    return get_SaltVerdeMaxDeliveries;
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSVMaxDeliveriesSRP_(ref int count, int[] values);
            //

            // Class A
            //
            //   NOTE: not currently used in the API as of 02.09.12 DAS
            //   
            //   These are class A designations "used"-01.29.13 das
            // -------------------------------------------------------------------------  
            protected internal int[] get_SaltVerdeClassADesignationsUsed   // af for the year by provider
            {
                get
                {
                    int numProviders = 10;
                    int[] SVClassADesignations = new int[numProviders];
                    getSVClassADesignations_(ref numProviders, SVClassADesignations);

                    int[] SaltVerdeClassADesignations = ExpandToAllProviders(SVClassADesignations); // Expand 10 to 33

                    return SaltVerdeClassADesignations;
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSVClassADesignations_(ref int count, int[] values);
            //

             // Class B and C - surface water and pumping
            // 02.09.12 DAS changed
            protected internal int[] get_SaltVerdeClassBCDesignationsUsed   // af for the year by provider
            {
                get
                {
                    // 
                    int numProviders = 10;
                    int[] SVClassBC = new int[numProviders];
                    getSVClassBCDesignations_(ref numProviders, SVClassBC);

                    int[] get_SaltVerdeClassBCDesignations = ExpandToAllProviders(SVClassBC); // Expand 10 to 33

                    return get_SaltVerdeClassBCDesignations;
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSVClassBCDesignations_(ref int count, int[] values);
            //
            // Class B and C maximum designations
            // NOT in API
            protected internal int[] get_SaltVerdeClassBCmax   // af for the year by provider
            {
                get
                {
                    // 
                    int numProviders = 10;
                    int[] SVClassBCmax = new int[numProviders];
                    getSVClassBCmax_(ref numProviders, SVClassBCmax);

                    int[] get_SaltVerdeClassBCmax = ExpandToAllProviders(SVClassBCmax); // Expand 10 to 33

                    return get_SaltVerdeClassBCmax;
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSVClassBCmax_(ref int count, int[] values);
            //

            // Not in the API
            // Class B and C from reservoir
            // 02.20.12
            protected internal int[] get_SaltVerdeClassBCreservoirUsed   // af for the year by provider
            {
                get
                {
                    // 
                    int numProviders = 10;
                    int[] get_SaltVerdeClassBCreservoirUsed = new int[numProviders];
                    getSVTclassBCstorage_(ref numProviders, get_SaltVerdeClassBCreservoirUsed);

                    int[] SaltVerdeClassBCres = ExpandToAllProviders(get_SaltVerdeClassBCreservoirUsed); // Expand 10 to 33

                    return SaltVerdeClassBCres;
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSVTclassBCstorage_(ref int count, int[] values);




            // NCS
            // Not used in the API as of 02.09.12 DAS
            // --------------------------------------
            //protected internal int[] get_SaltVerdeNCSDesignationsUsed   // af for the year by provider
            //{
            //    get
            //    {
            //        // 
            //        int num = 10;
            //        int[] SVTNCSDesignations = new int[num];

            //        getSVNCS_(ref num, SVTNCSDesignations);
            //        int[] get_SaltVerdeNCSDesignations = ExpandToAllProviders(SVTNCSDesignations); // expand 10 to 33
            //        return get_SaltVerdeNCSDesignations;
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getSVNCS_(ref int count, int[] values);
            //

            // Not used in the API as of 02.09.12 DAS
            // ======================================
            //protected internal int[] get_SaltVerdeNCSmax   // af for the year by provider
            //{
            //    get
            //    {
            //        // This was brought to 35 in the FORTRAN dll
            //        int numProviders = 10;
            //        int[] SVTNCSmax = new int[numProviders];

            //        getSVNCSmax_(ref numProviders, SVTNCSmax);
            //        int[] get_SaltVerdeNCSmax = ExpandToAllProviders(SVTNCSmax); // expand 10 to 33
            //        return get_SaltVerdeNCSmax;
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getSVNCSmax_(ref int count, int[] values);
            //

            //   Provider surface water designations.  This includes get_CAP_BO water but also surface water
            // delivered to SRP customers via the Class A and B (and NCS) designations for the 10 cities that use
            // SVT river water.
            //
            //  02.09.12, 02.10.12 DAS not currently used in the API
            // Includes reservoir water
            // NOT in API
            // ------------------------------------------------------------------
            protected internal int[] get_ProviderSWdesignations  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] get_ProviderSWdesignations = new int[numProviders];
                    getProviderSWDesignations_(ref numProviders, get_ProviderSWdesignations);

                    return TrimProvNames(get_ProviderSWdesignations);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getProviderSWDesignations_(ref int count, int[] values);
            //

            #endregion
            #region Groundwater-changed

                // Provider gtoundwater designations.  They my pump up to but not exceeding this value.
                //  Originally this was addded as a check of the provider water balance
                //
                //              [CERTIFIED- 02.09.10 das] 
                // NOT in API
                protected internal int[] get_ProviderGWdesignations  // af for the year by provider
                {
                    get
                    {
                        int numProviders = cNumFortranProviders;
                        int[] get_ProviderGWdesignations = new int[numProviders];
                        getProviderGWDesignations_(ref numProviders, get_ProviderGWdesignations);

                        return TrimProvNames(get_ProviderGWdesignations);
                    }
                }
                [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
                protected static extern void getProviderGWDesignations_(ref int count, int[] values);
            //
            // 

                /// <summary>   Sets the provider annual gw credits. </summary>
                /// <value> The provider annual groundwater pumping credits- AF annum-1. </value>
                // 08.16.2013
                // NOT in API
                protected internal int[] set_ProviderAllowanceGWcredits
                {
                    set
                    {
                        int num = cNumFortranProviders;
                        citiModel = value;
                        setProviderAnnualCredits_(ref num, ExpandProvNames35(citiModel));
                    }
                }
                [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
                protected static extern void setProviderAnnualCredits_(ref int count, int[] values);

                /// <summary>   Gets the get provider groundwater stored. </summary>
                /// <value> This is combined surface water, effluent, and direct injection estimates. AF annum-1
                ///             These data add to the storage credits</value>
                // NOT in API
                protected internal int[] get_ProviderGWstored  // af for the year by provider
                {
                    get
                    {
                        int numProviders = cNumFortranProviders;
                        int[] get_ProviderGWstored = new int[numProviders];
                        getProviderGWstored_(ref numProviders, get_ProviderGWstored);

                        return TrimProvNames(get_ProviderGWstored);
                    }
                }
                [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
                protected static extern void getProviderGWstored_(ref int count, int[] values);


            //
            //  The amount of groundwater pumped by each provider NOT including the SRP
            // pumping for those providers that have such designations.
            //
            // 
            //
            //  02.09.12 DAS
            protected internal int[] get_ProviderGWPumpedMunicipal  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] get_ProviderGWPumpedMunicipal = new int[numProviders];
                    getProviderGWPumpageMuni_(ref numProviders, get_ProviderGWPumpedMunicipal);

                    return TrimProvNames(get_ProviderGWPumpedMunicipal);

                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getProviderGWPumpageMuni_(ref int count, int[] values);
            //
            //
            // trimmed to 33 -
            // 02.09.12 DAS
            protected internal int[] get_ClassBCpumpedSRP
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] get_ClassBCpumpedSRP = new int[numProviders];
                    getProviderGWPumpageSRP_(ref numProviders, get_ClassBCpumpedSRP);

                    return TrimProvNames(get_ClassBCpumpedSRP);
                }

            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getProviderGWPumpageSRP_(ref int count, int[] values);

            //
            // The provider specific amount of water recharged. When supply > demand.
            //
            //          [CERTIFIED- 02.10.10 das] 
            //
           // NOT in API
            protected internal int[] get_ProviderGWRecharge  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] get_ProviderGWRecharge = new int[numProviders];
                    getProviderGWRecharge_(ref numProviders, get_ProviderGWRecharge);

                    return TrimProvNames(get_ProviderGWRecharge);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getProviderGWRecharge_(ref int count, int[] values);
            //

            // ===================================

                     #endregion
            //       
            #endregion
            //-----------------------------------------------------------------------------------//
            #region Annual Results Variables:
            //
              #region Water Demand: summary
            //
            //   Annual water demands for each provider.  At present, no provider water policies
            // are available, so this represents the raw empirical demand adjusted for using
            // current GPCD estimates for each provider.
            //
            //      [CERTIFIED- 02.05.10 das]
            /// <summary>
            /// Total water demand for each provider.  Estimated as the product of population and
            /// and GPCD, or read in as input. See Version 5.7 Parameters ver 4.xlsx for details.
            /// </summary>
            protected internal int[] get_WaterDemand  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] totalDemands = new int[numProviders];
                    getTotalDemands_(ref numProviders, totalDemands);

                    return TrimProvNames(totalDemands);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getTotalDemands_(ref int count, int[] values);
            //
            /// <summary>
            /// On-project (SRP members) water demand. This represents only the 10 water
            /// providers that are members.
            /// </summary>
            protected internal int[] get_WaterDemandOnProject  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] onDemands = new int[numProviders];
                    getOnProjectDemand_(ref numProviders, onDemands);

                    return TrimProvNames(onDemands);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getOnProjectDemand_(ref int count, int[] values);
            //
            /// <summary>
            /// Other water demand.  This represents the off-project water demand for those
            /// providers that receive SRP water but "WaterDemand" for those that do not.
            /// </summary>
            protected internal int[] get_WaterDemandOther  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] offDemands = new int[numProviders];
                    getOffProjectDemand_(ref numProviders, offDemands);

                    return TrimProvNames(offDemands);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getOffProjectDemand_(ref int count, int[] values);
            //
            /// <summary>
            /// I am not, at present, sure that this and the following properties return.
            /// 10.08.13 DAS
            /// NOT in API
            /// </summary>
            //protected internal int[] get_NetWaterDemandOnProject  // af for the year by provider
            //{
            //    get
            //    {
            //        int numProviders = cNumFortranProviders;
            //        int[] onDemands = new int[numProviders];
            //        getOnProjectNetDemand_(ref numProviders, onDemands);

            //        return TrimProvNames(onDemands);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern void getOnProjectNetDemand_(ref int count, int[] values);
            //
            /// <summary>
            /// Other water demand.  This represents the off-project water demand for those
            /// providers that receive SRP water but "WaterDemand" for those that do not.
            /// </summary>
            //protected internal int[] get_NetWaterDemandOther  // af for the year by provider
            //{
            //    get
            //    {
            //        int numProviders = cNumFortranProviders;
            //        int[] offDemands = new int[numProviders];
            //        getOffProjectNetDemand_(ref numProviders, offDemands);

            //        return TrimProvNames(offDemands);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern void getOffProjectNetDemand_(ref int count, int[] values);
            //






            //
            //    As the name implies.
            //      lvf_GPCDses=gv_AdjustedGPCD
            //
            //      lv_GrowthRateAdjDemand(T%year,j) = &
            //       ((lvf_GPCDses*365)*(1./gpd_galperacft))*lv_GrowthRateAdjPop(T%year,j)

            //  [CERTIFIED- 08.07.12 das]
            //        
            //
            /// <summary>
            /// This is the GPCD used in the model to estimate water demand.  It is either
            /// read out of a text file (providerGPCD_2008.txt) and then decreased as per the 
            /// Percent Reduction in GPCD (set_AlterGPCDpct[]) or it is given as an input.  
            /// If used as an input (setter used) Provider Demand Option 4 (ProviderDemandOption) needs
            /// to be used.
            /// </summary>
            protected internal int[] ProviderGPCD
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] ProviderGPCD = new int[numProviders];
                    getProviderGPCD_(ref numProviders, ProviderGPCD);
                    return TrimProvNames(ProviderGPCD);
                }
                set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setProviderGPCD_(ref num, ExpandProvNames35(citiModel));
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getProviderGPCD_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setProviderGPCD_(ref int count, int[] values);

            /// <summary>
            ///  get the model estimates of GPCD for option 4 when user changes GPCD this
            ///  returns what the GPCD would have been if it had not been altered
            /// </summary>
            protected internal int[] get_ProviderGPCDraw
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] ProviderGPCD = new int[numProviders];
                    getProviderGPCDraw_(ref numProviders, ProviderGPCD);
                    return TrimProvNames(ProviderGPCD);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getProviderGPCDraw_(ref int count, int[] values);
            //
            /// <summary>
            ///  Provider estimate of Residential GPCD
            ///  01.21.15 DAS
            /// </summary>
            protected internal int[] get_ProviderGPCDres
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] ProviderGPCDres = new int[numProviders];
                    getProviderGPCDres_(ref numProviders, ProviderGPCDres);
                    return TrimProvNames(ProviderGPCDres);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getProviderGPCDres_(ref int count, int[] values);



            // There are two of these 08.12.16
            // NFLOWMAX das
            protected internal int[] NormalFlow_rights_max
            {
                set
                {
                    int num = cNumModeledProviders;
                    citiModel = value;
                    setNormalFlowEmpiricalMax_(ref num,TrimProvNamesToSRP(citiModel));
                    //
                }
                get
                {
                    int numProviders = 10;
                    int[] NormalFlow_rights_max = new int[numProviders];
                    getNormalFlowEmpiricalMax_(ref numProviders, NormalFlow_rights_max);
                    return ExpandProvNamesSRPto33(NormalFlow_rights_max);
                }
            }

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setNormalFlowEmpiricalMax_(ref int count, int[] values);

           [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getNormalFlowEmpiricalMax_(ref int count, int[] values);



            #endregion
              #region CO River variables: summary
            //

            //
            // CO river flow.  We need to add a variable to select the data set that we want to read.
            //

            // [CERTIFIED 02.02.2010 das]
            /// <summary>
            /// The flow on the Colorado River as manipulated (after manipulation). Units are Acre-feet per annum.
            /// </summary>
            protected internal int get_ColoradoRiverFlow
            {
                get
                {
                    int gvalue = 0;
                    gvalue = getCORiverFlow_(ref gvalue);
                    return gvalue;
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getCORiverFlow_(ref int values);
            //

            // Storage in Mead and in Powell (acft)
            //
            //       [CERTIFIED- 02.04.10 das]
            /// <summary>
            /// The storage in the Colorado System in Acre-feet. This is the sum of Lake Mead
            /// and Lake Powell.
            /// </summary>
            protected internal int get_ColoradoStorage
            {
                get
                {
                    int gvalue = 0;
                    return getCOStorage_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getCOStorage_(ref int values);
            //

            // Annual storage in Lake Powell (acft)
            //
            //      [CERTIFIED- 02.04.10 das]
            /// <summary>
            /// Storage in Powell (acre-feet) Total (includes the dead pool)
            /// </summary>
            protected internal int get_PowellStorage
            {
                get
                {
                    int gvalue = 0;
                    return getPowellStorage_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getPowellStorage_(ref int values);
            //

            // Annual storage in Lake Mead (acft)
            //
            //  [CERTIFIED- 02.04.10 das]
            /// <summary>
            /// Storage in Lake Mead (acre-feet) Total (includes dead pool)
            /// </summary>
            protected internal int get_MeadStorage
            {
                get
                {
                    int gvalue = 0;
                    return getMeadStorage_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getMeadStorage_(ref int values);
            //

            // get_CAP_BO delivery (county scale) Deliveries into Maricopa County.
            //
            //   [CERTIFIED- 02.04.10 das]
            /// <summary>
            ///  Deliveries of Colorado River water via the CAP along the aqueduct. This is what
            ///  reaches Granite Reef for Municipal and Industrial (i.e., CAP tiers I,II,III, and IV)
            /// </summary>
            protected internal int get_ColoradoDeliveryMandI
            {
                get
                {
                    int gvalue = 0;
                    return getCODelivery_(ref gvalue);

                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getCODelivery_(ref int values);
            //
            //
            // absent:  internal  int CAP2MandIexcess

            //    
            // Used in the API. 02.09.12- this is class I and class II water
            /// <summary>
            /// Not used.
            /// </summary>
            protected internal int get_CAP2MandI
            {
                get
                {
                    int gvalue = 0;
                    return getCAPMandI_(ref gvalue);

                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getCAPMandI_(ref int values);
            //
            /// <summary>
            /// Arizona Share of Colorado River water. Non-drought conditions, this is 2.8 million acre-feet.
            /// </summary>
            protected internal int get_AZshareCO
            {
                get
                {
                    int gvalue = 0;
                    return getAZshareCO_(ref gvalue);

                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getAZshareCO_(ref int values);
            //
            // absent: 
            //

            #endregion
              #region Salt-Verde-Tonto River Variables: summary
            //

            // SRP release from the reservoir system.  This is a function of storage and flow. Annual
            // estimates are, just that, estimates.  Monthly will be available soon, using actual release
            // estimates.
            //
            //   [CERTIFIED- 02.04.10 das]

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the get salt verde tonto release. </summary>
            ///
            /// <value> The get salt verde tonto release. Units are acre-feet per annum. This is the total
            ///         release from the reservoir system.</value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int get_SaltVerdeTontoRelease
            {
                get
                {
                    int gvalue = 0;
                    return getSVDelivery_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSVDelivery_(ref int values);
            //

            //   Salt-Verde-Tonto River flow.  I added a variable to select the data set that we
            // want to use.  At present it defaults to the Bureau of Reclamation data.
            //
            //              [CERTIFIED 02.02.2010 das]

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the get salt verde river flow. </summary>
            ///
            /// <value> The get salt verde river flow. Units are acre-feet per annum. This is the flow 
            ///         estimate used by the model (after any climate or drought manipulations). </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int get_SaltVerdeRiverFlow
            {
                get
                {
                    int gvalue = 0;
                    return getSVRiverFlow_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSVRiverFlow_(ref int values);
            //

            // Storage on the Salt, Verde, and Tonto Rivers. We model only one reservoir (acft)
            //
            //    [CERTIFIED 02.04.2010 das]

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the get salt verde storage./ </summary>
            ///
            /// <value> The get salt verde storage. Total storage in acre-feet in the salt and verde river
            ///         system (all reservoirs).</value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int get_SaltVerdeStorage
            {
                get
                {
                    int gvalue = 0;
                    return getSVStorage_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSVStorage_(ref int values);
            //
            protected internal int get_VerdeStorage
            {
                get
                {
                    int gvalue = 0;
                    return getVerdeStorage_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getVerdeStorage_(ref int values);
            //
            protected internal int get_RooseveltStorage
            {
                get
                {
                    int gvalue = 0;
                    return getRooseveltStorage_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getRooseveltStorage_(ref int values);
            //
            protected internal int get_OtherSaltStorage
            {
                get
                {
                    int gvalue = 0;
                    return getOtherSaltStorage_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getOtherSaltStorage_(ref int values);
            //



            // Spillage (acft) - spill water
            //
            //    [CERTIFIED 03.19.2012 das]

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the get salt verde spillage. </summary>
            ///
            /// <value> The get salt verde spillage. Just that: spillage over the dam from when flows and
            ///         storage exceed maximum capacity. NOTE: we model the SRP system using one reservoir
            ///         but we account for storage from all six.</value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int get_SaltVerdeSpillage
            {
                get
                {
                    int gvalue = 0;
                    return getSVTspillage_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getSVTspillage_(ref int values);
            //

            //
            // --------------------------------------------------------------

            //
            // absent:  internal  int SaltVerdeAnnualGroundwaterDemand 
            //

            //
            // absent: internal  int SaltVerdeRechargeSRVReach 
            //

            #endregion
            //
            #endregion
            //-----------------------------------------------------------------------------------//
            #region MODFLOW annual outputs: summary
            //myMODFLOWoutputs my = new myMODFLOWoutputs();
            /// <summary>
            /// When connected, this is the basin estimate of pumpage for the SRV as evaluated
            /// using MODFLOW. Units are Acre-feet per annum. 
            /// </summary>
            protected internal int get_BasinWidePumpage
            {
                get
                {

                    return 0;
                }
            }
            /// <summary>
            ///  When connected, this is the basin estimate of net change for the SRV as evaluated
            /// using MODFLOW. Units are Acre-feet per annum. Annual estimates.
            /// </summary>
            protected internal int get_BasinWideDeltaAnnual
            {
                get
                {

                    return 0;
                }
            }
            #endregion
            // ----------------------------------------------------------------------------------//
            #region WaterSim_6 Variables
            /// <summary>
            /// 
            protected internal int set_ppH_M
            {
                set
                {
                    set_ppH_M_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void set_ppH_M_(ref int value);
        //
            protected internal int set_ppH_L
            {
                set
                {
                    set_ppH_L_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void set_ppH_L_(ref int value);
        //
            protected internal int set_Provider
            {
                set
                {
                    set_provider_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void set_provider_(ref int value);

            /// 
            /// Determine if a base simulation, or a parameterized one - i.e., not the first 13 years
            /// is in action. When parmAPIcleared=true, its a full set run (i.e., not the 13 year
            /// initial GPCDFIX simulation setparmIwaniecScenarios
            /// </summary>
            protected internal bool parmAPIcleared
            {
                set
                {
                    setAPIcode_(ref value);
                }
                get
                {
                      bool gvalue = false;
                      return getAPIcode_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setAPIcode_(ref bool value);
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern bool getAPIcode_(ref bool value);
            //
            //protected internal bool parmIwaniecScenariosYN
             public bool parmIwaniecScenariosYN
             {
                set
                {
                    setparmIwaniecScenarios_(ref value);
                }
                get
                {
                    bool gvalue = false;
                    return getparmIwaniecScenarios_(ref gvalue);
                }
             }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setparmIwaniecScenarios_(ref bool value);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern bool getparmIwaniecScenarios_(ref bool value);
            //
            // 02.01.19 das
            public bool parmWaterSim5YN
            {
                set
                {
                    setparmWaterSim5_(ref value);
                }
                get
                {
                    bool gvalue = false;
                    return getparmWaterSim5_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setparmWaterSim5_(ref bool value);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern bool getparmWaterSim5_(ref bool value);

        /// <summary>
        ///  Tells the Fortran model to use the Land cover land use data, or not, when
        ///  gvl_IncludeMeteorology is true. This gives us the ability to use GPCD and
        ///  population-based water demand along with rainwater harvesting, stormwater
        ///  capture. NOT IN API
        ///  08.19.16
        /// </summary>
        protected internal bool parmUseLCLU
            {
                set
                {
                    setUseLCLU_(ref value);
                }
                get
                {
                    bool gvalue = false;
                    return getUseLCLU_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setUseLCLU_(ref bool value);
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern bool getUseLCLU_(ref bool value);
            //
            // For now, one scenario at a time
            // This variable tells the FORTRAN model which of othe 7 scenarios to run;
            // -------------------------------------------------------------------------
            /// David Iwaniec Scenario
            ///  1=AD; 2 = AF; 3=AH; 4=HHH; 5=EC; 6=ZW; 7=BAU
            ///  AD=adaptive drought, AF=adaptive flood, AH=adaptive heat, HHH=healthy harvest hubs
            ///  EC=emerald city, ZW=zero waste, BAU = strategic (business as usual modified)
            ///  
            /// Summer 2016
            /// </summary>
            protected internal int set_scenario
            {
                set
                {
                    setScenario_(ref value);
                    parmIwaniecScenariosYN = true;

                }
                get
                {
                    int gvalue = 0;
                    int myInt = getScenario_(ref gvalue);
                    return myInt;
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getScenario_(ref int value);
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setScenario_(ref int value);
            //
            /// <summary>
            /// 
            /// </summary>
            protected internal int set_GPCDEfficiency
        {
                set
                {
                setGPCDEfficiency_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setGPCDEfficiency_(ref int value);
            //
        protected internal bool set_parmIwaniecPPtoAgEffluent
            {
                set
                {
                    setParmIwaniecPPtoAgEffluent_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmIwaniecPPtoAgEffluent_(ref bool value);
            //
            /// <summary>
            /// If true, then it sets the "water leak" parameters to zero
            /// NOT IN API
            /// </summary>
            protected internal bool set_parmIwaniecNoLeaks
            {
                set
                {
                    setParmIwaniecLeaks_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmIwaniecLeaks_(ref bool value);


            /// <summary>
            /// Sets a value indicating whether the parameter calculate gray water should be set.
            /// </summary>
            /// <value> true if set parameter calculate gray water, false if not. </value>
            // NOT in API DAS 11.23.15
            protected internal bool set_parmCalculateGrayWater
            {
                set
                {
                    setParmGrayWater_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmGrayWater_(ref bool value);
            //
            protected internal int set_parmGrayWaterCompliancePCT
            {
                set
                {
                    setParmGrayWaterCompliance_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmGrayWaterCompliance_(ref int value);
            //
            /// <summary>
            /// Actually, years to the inflection point of a logistic equation
            /// </summary>
            protected internal int set_parmYearsToAdopt
            {
                set
                {
                    setParmYearsToAdopt_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmYearsToAdopt_(ref int value);
            //
            protected internal int set_parmYearsToAdoptRainWater
            {
                set
                {
                    setParmYearsToAdoptRainWater_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmYearsToAdoptRainWater_(ref int value);

            protected internal int set_parmYearsToAdoptGrayWater
            {
                set
                {
                    setParmYearsToAdoptGrayWater_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmYearsToAdoptGrayWater_(ref int value);
          

            //
            protected internal bool set_parmCalculateRainWaterHarvesting
            {
                set
                {
                    bool Rain = value;
                    setParmRainWaterHarvesting_(ref Rain);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmRainWaterHarvesting_(ref bool value);
            //
            protected internal bool set_parmCalculateRainWaterResidentialOnly
            {
                set
                {
                    setParmRainWaterHarvestResOnly_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmRainWaterHarvestResOnly_(ref bool value);
            //
            protected internal bool set_parmCalculateRainWaterCommercialOnly
            {
                set
                {
                    setParmRainWaterHarvestComOnly_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmRainWaterHarvestComOnly_(ref bool value);


            //
            protected internal bool set_parmCalculateStormWaterHarvesting
            {
                set
                {
                    setParmStormWaterHarvesting_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setParmStormWaterHarvesting_(ref bool value);

            int[] stormWaterCapacity = new int[33];
            protected internal int[] set_StormWaterCapacity  // 
            {
                set
                {
                    int num = cNumFortranProviders;
                    citiModel = value;
                    setStormWaterCapacity_m3_(ref num, ExpandProvNames35(citiModel));
                    stormWaterCapacity = citiModel;
                }
                get
                {
                    int num = cNumFortranProviders;
                    int[] Myreturn = new int[num];
                    getStormWaterCapacity_m3_(ref num, Myreturn);
                    return TrimProvNames(Myreturn);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setStormWaterCapacity_m3_(ref int count, int[] values);

            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getStormWaterCapacity_m3_(ref int count, int[] values);
            //
            protected internal int[] get_StormWaterHarvested  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] Myreturn = new int[num];
                    get_harvestedStormWater_(ref num, Myreturn);
                    return TrimProvNames(Myreturn);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int get_harvestedStormWater_(ref int count, int[] values);




            // 
            protected internal int[] get_GrayWaterResidential  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] Myreturn = new int[num];
                    getGrayWaterResidential_(ref num, Myreturn);
                    return TrimProvNames(Myreturn);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getGrayWaterResidential_(ref int count, int[] values);
            // 
            protected internal int[] get_GrayWaterCommercial  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_GrayWaterCommercial = new int[num];
                    getGrayWaterCommercial_(ref num, get_GrayWaterCommercial);
                    return TrimProvNames(get_GrayWaterCommercial);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getGrayWaterCommercial_(ref int count, int[] values);
            // NOT in API
            protected internal int[] get_GrayWaterIndustrial  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] get_GrayWaterIndustrial = new int[num];
                    getGrayWaterIndustrial_(ref num, get_GrayWaterIndustrial);
                    return TrimProvNames(get_GrayWaterIndustrial);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getGrayWaterIndustrial_(ref int count, int[] values);
            /// <summary>
            /// 
            /// </summary>
            protected internal int[] get_GPCD_resGrayWater  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] Myreturn = new int[num];
                    getGPCD_resGrayWater_(ref num, Myreturn);
                    return TrimProvNames(Myreturn);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getGPCD_resGrayWater_(ref int count, int[] values);
            //
            protected internal int[] get_GPCD_comGrayWater  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] Myreturn = new int[num];
                    getGPCD_comGrayWater_(ref num, Myreturn);
                    return TrimProvNames(Myreturn);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getGPCD_comGrayWater_(ref int count, int[] values);
            //
            protected internal int[] get_GPCD_indGrayWater  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] Myreturn = new int[num];
                    getGPCD_indGrayWater_(ref num, Myreturn);
                    return TrimProvNames(Myreturn);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getGPCD_indGrayWater_(ref int count, int[] values);
            //
            // 06.16.16
            protected internal int[] get_EffluentUsed_Agriculture  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] Myreturn = new int[num];
                    get_effluentUsedAg_(ref num, Myreturn);
                    return TrimProvNames(Myreturn);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int get_effluentUsedAg_(ref int count, int[] values);
            //
            // 08.11.16
            /// <summary>
            /// Ag Surface water used after LCLU determination of use in Agriculture.f90
            /// </summary>
            protected internal int[] get_SurfaceUsed_Agriculture  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] Myreturn = new int[num];
                    getAgSurfaceTotal_(ref num, Myreturn);
                    return TrimProvNames(Myreturn);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getAgSurfaceTotal_(ref int count, int[] values);
            //
            /// <summary>
            /// 08.11.16
            /// Groundwater used Ag after LCLU determiniations in Agriculture.f90
            /// </summary>
            protected internal int[] get_GroundwaterUsed_Agriculture  // 
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] Myreturn = new int[num];
                    getAgPumpingTotal_(ref num, Myreturn);
                    return TrimProvNames(Myreturn);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getAgPumpingTotal_(ref int count, int[] values);
            //
         



            /// <summary>
             //
            // ------------------- OUTPUTS ---------------------------
            /// <summary>
            /// The ratio of total non-potable outdoor water use
            /// from rainwater, stormwater, and gray water
            /// capture systems to total water used out-of-doors. 
            /// This is system total water (residential, commercial, industrial).
            /// It was created from the new data structure in Provider.f90
            /// 
            //
            protected internal int [] get_RainHarvestedToTotalOutdoor
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] Myreturn = new int[num];
                    getRainHarvestToTotalOutdoor_(ref num, Myreturn);
                    return TrimProvNames(Myreturn);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getRainHarvestToTotalOutdoor_(ref int count, int[] values);
            //
            //
            protected internal int[] get_GrayWaterToTotalOutdoor
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] Myreturn = new int[num];
                    getGrayWaterToTotalOutdoor_(ref num, Myreturn);
                    return TrimProvNames(Myreturn);               
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getGrayWaterToTotalOutdoor_(ref int count, int[] values);
 
        protected internal int [] get_ReclaimedToTotalOutdoor
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] Myreturn = new int[num];
                    getReclaimedToTotalOutdoor_(ref num, Myreturn);
                    return TrimProvNames(Myreturn);

                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getReclaimedToTotalOutdoor_(ref int count, int[] values);
        //
        //12.10.18
        protected internal int[] get_GrossStormwaterCreated
        {
            get
            {
                int num = cNumFortranProviders;
                int[] Myreturn = new int[num];
                getGrossStormWater_AF_(ref num, Myreturn);
                return TrimProvNames(Myreturn);

            }
        }
        [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
        protected static extern int getGrossStormWater_AF_(ref int count, int[] values);
        /// <summary>
        /// This may include non-potable and potable, although
        /// not much work has been done to-date looking at using
        /// non-potable indoors
        /// 
        /// 08.10.16
        /// </summary>
        protected internal int get_ResIndoorWaterUse
            {
                get
                {
                    int gvalue = 0;
                    return getResIndoorUse_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getResIndoorUse_(ref int values);
            //
            /// <summary>
            /// see above
            /// 08.10.16
            /// </summary>
            protected internal int get_ResOutdoorWaterUse
            {
                get
                {
                    int gvalue = 0;
                    return getResOutdoorUse_(ref gvalue);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getResOutdoorUse_(ref int values);
            //
            /// <summary>
            /// Indoor and outdoor water use by commercial entities
            /// 
            /// 08.10.16
            /// </summary>
            /// 05.16.17 commented Out
            /// 
            //protected internal int get_ComWaterUsed
            //{
            //    get
            //    {
            //        int gvalue = 0;
            //        return getComUse_(ref gvalue);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getComUse_(ref int values);
            //
            /// <summary>
            /// Industrial water use indoor and outdoors
            /// 
            /// 08.10.16
            /// </summary>
            /// 05.16.17 commented out
            //protected internal int get_IndWaterUsed
            //{
            //    get
            //    {
            //        int gvalue = 0;
            //        return getIndUse_(ref gvalue);
            //    }
            //}
            //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            //protected static extern int getIndUse_(ref int values);
            //
            /// <summary>
            /// LCLU efficiency for residential water use
            /// </summary>
            protected internal int set_LCLUefficiencyResidential
            {
                set
                {
                    set_LCLUefficiencyRes_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void set_LCLUefficiencyRes_(ref int value);
            // faith
            protected internal int[] get_RainWaterHarvested
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parm = new int[num];
                    getRainWaterHarvested_(ref num, parm);
                    return TrimProvNameS(parm);
                }
             }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getRainWaterHarvested_(ref int count, int[] values);
            //
            protected internal int[] get_RainWaterHarvested_SF
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parm = new int[num];
                    getRainWaterHarvested_SF_(ref num, parm);
                    return TrimProvNameS(parm);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getRainWaterHarvested_SF_(ref int count, int[] values);
            //
            protected internal int[] get_RainWaterHarvested_MF
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parm = new int[num];
                    getRainWaterHarvested_MF_(ref num, parm);
                    return TrimProvNameS(parm);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getRainWaterHarvested_MF_(ref int count, int[] values);
            //
            protected internal int[] get_RainWaterHarvested_PU
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parm = new int[num];
                    getRainWaterHarvested_PU_(ref num, parm);
                    return TrimProvNameS(parm);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getRainWaterHarvested_PU_(ref int count, int[] values);
        //
        /// <summary>
        /// Ag LCLU demand
        /// 01.17.17 
        /// </summary>
        protected internal int[] get_WaterDemand_Agriculture
        {
            get
            {
                int num = cNumFortranProviders;
                int[] parm = new int[num];
                getAnnualDemand_AG_AF_(ref num, parm);
                return TrimProvNameS(parm);
            }
        }
        [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
        protected static extern int getAnnualDemand_AG_AF_(ref int count, int[] values);

        /// <summary>
        /// Medium Density LCLU demand
        /// 01.09.17 
        /// </summary>
        protected internal int[] get_WaterDemand_MediumDensity
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parm = new int[num];
                    getAnnualDemand_MD_AF_(ref num, parm);
                    return TrimProvNameS(parm);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getAnnualDemand_MD_AF_(ref int count, int[] values);
            //
            /// <summary>
            ///  High Density LCLU water demand (AF annum-1)
            ///  01.09.17
            /// </summary>
            protected internal int[] get_WaterDemand_HighDensity
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parm = new int[num];
                    getAnnualDemand_HD_AF_(ref num, parm);
                    return TrimProvNameS(parm);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getAnnualDemand_HD_AF_(ref int count, int[] values);
            //
            /// <summary>
            /// Low Density (Peri Urban) water demand from LCLU data (AF annum-1)
            /// 01.09.17
            /// </summary>
            protected internal int[] get_WaterDemand_LowDensity
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parm = new int[num];
                    getAnnualDemand_LD_AF_(ref num, parm);
                    return TrimProvNameS(parm);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getAnnualDemand_LD_AF_(ref int count, int[] values);
            //
            protected internal int[] get_WaterDemand_Turf
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parm = new int[num];
                    getAnnualDemand_Turf_AF_(ref num, parm);
                    return TrimProvNameS(parm);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getAnnualDemand_Turf_AF_(ref int count, int[] values);
            //
            protected internal int[] get_WaterDemand_Greenway
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parm = new int[num];
                    getAnnualDemand_GWay_AF_(ref num, parm);
                    return TrimProvNameS(parm);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getAnnualDemand_GWay_AF_(ref int count, int[] values);
            //
            protected internal int[] get_WaterDemand_Tree
            {
                get
                {
                    int num = cNumFortranProviders;
                    int[] parm = new int[num];
                    getAnnualDemand_Tree_AF_(ref num, parm);
                    return TrimProvNameS(parm);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern int getAnnualDemand_Tree_AF_(ref int count, int[] values);
            //
            // Nonpotable
            //
            // ---------------------------------------------------------------------------
            //  Total nonpotable water used (acft a-1)
            //
            //             
            protected internal int[] get_NonpotableWaterTotal  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] param = new int[numProviders];
                    getNonpotableTotalUsed_AF_(ref numProviders, param);

                    return TrimProvNames(param); // Contract 35 to 33
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getNonpotableTotalUsed_AF_(ref int count, int[] values);
            //
            //
            //
            //
            //  Low Density Residential
            //             
            protected internal int[] get_ResidentialLowDenIndoorGPCD  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] param = new int[numProviders];
                    getResIndoorGPCD_PU_(ref numProviders, param);

                    return TrimProvNames(param); // Contract 35 to 33
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getResIndoorGPCD_PU_(ref int count, int[] values);
            //
            //             
            protected internal int[] get_ResidentialLowDenOutdoorGPCD  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] param = new int[numProviders];
                    getResOutdoorGPCD_PU_(ref numProviders, param);

                    return TrimProvNames(param); // Contract 35 to 33
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getResOutdoorGPCD_PU_(ref int count, int[] values);
            //
            // Medium Density Residential
            //
            protected internal int[] get_ResidentialMediumDenIndoorGPCD  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] param = new int[numProviders];
                    getResIndoorGPCD_MD_(ref numProviders, param);

                    return TrimProvNames(param); // Contract 35 to 33
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getResIndoorGPCD_MD_(ref int count, int[] values);
            //
            //             
            protected internal int[] get_ResidentialMediumDenOutdoorGPCD  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] param = new int[numProviders];
                    getResOutdoorGPCD_MD_(ref numProviders, param);

                    return TrimProvNames(param); // Contract 35 to 33
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getResOutdoorGPCD_MD_(ref int count, int[] values);
            //
            // High Density Residential GPCD
            protected internal int[] get_ResidentialHighDenIndoorGPCD  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] param = new int[numProviders];
                    getResIndoorGPCD_HD_(ref numProviders, param);

                    return TrimProvNames(param); // Contract 35 to 33
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getResIndoorGPCD_HD_(ref int count, int[] values);
            //
            //             
            protected internal int[] get_ResidentialHighDenOutdoorGPCD  // af for the year by provider
            {
                get
                {
                    int numProviders = cNumFortranProviders;
                    int[] param = new int[numProviders];
                    getResOutdoorGPCD_HD_(ref numProviders, param);

                    return TrimProvNames(param); // Contract 35 to 33
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void getResOutdoorGPCD_HD_(ref int count, int[] values);
            //
            /// <summary>
            /// This variable sets the change in the rainfall for the model. Increasing or decreasing (within bounds set by the API parameter)
            /// 04.12.2017
            /// </summary>
            internal int _rainFallFactor = 100;
            internal int rainFallFactor
            {
                get { return _rainFallFactor; }
                set { _rainFallFactor = value; }
            }
            protected internal int set_RainFallFactor
            {
                set
                {
                    setRainFallFactor_(ref value);
                    rainFallFactor = value;
                }
                get
                {
                    return rainFallFactor;

                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setRainFallFactor_(ref int value);
        //

            // 08.25.18
            protected internal int set_parmRainwaterHarvestCompliance
            {
                set
                {
                setRainHarvestCompliance_(ref value);
                }
            }
            [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
            protected static extern void setRainHarvestCompliance_(ref int value);
        // 08.25.18
        protected internal int set_parmRainwaterInflection
        {
            set
            {
                setInflectionPoint_(ref value);
            }
        }
        [DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void setInflectionPoint_(ref int value);



        // 05.16.17 Commented Out

        //protected internal int get_TestOne
        //{
        //    get
        //    {
        //        int gvalue = 0;
        //        return getTestOne_(ref gvalue);
        //    }
        //}
        //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
        //protected static extern int getTestOne_(ref int values);

        #endregion
        //-----------------------------------------------------------------------------------//
        // 
        #region ErrorControl
        /// <summary>   Gets the readstatus. If other than zero, lists the unit that the read file failed on </summary>
        /// <value> Default=Zero. If other than zero there was a problem finding a text file for input: FORTRAN unit # returned</value>
        protected internal int Readstatus
                {
                    get { return getStatus_(); }
                }

                // 01.18.12 DAS
                /// <summary>
                /// The case construct in the model kernel which tells the model what order to use
                /// when satisfying demand from the various sources.  At present we have to options:
                /// CASE 1
                ///  call sSRP_classA(T,gvi,Aout)              
                ///  call sSRP_classBandCstorage(T,gvi,Aout)   
                ///  call sCAP_water(T,gvi,Sout%sCAP_maf,vPV)  
                ///  call sSRP_classBandC(T,gvi,Aout,vPV)      
                ///  call sSRP_NCS(T,gvi,Aout)                 
                ///  call sGWater(T,gvi)                      
                ///  call sBanking_k(T,gvi,lvl_Interrupt)     
                /// CASE 2
                ///  call sSRP_classA(T,gvi,Aout)
                ///  call sCAP_water(T,gvi,Sout%sCAP_maf,vPV)
                ///  call sSRP_classBandCstorage(T,gvi,Aout)
                ///  call sSRP_classBandC(T,gvi,Aout,vPV)
                ///  call sSRP_NCS(T,gvi,Aout)
                ///  call sGWater(T,gvi)
                ///  call sBanking_k(T,gvi,lvl_Interrupt)
                /// </summary>
                //protected internal int KernelCase
                //{
                //    set
                //    {
                //        int checkedValue = RangeCheckKernelCase(value);
                //        setKernelCase_(ref checkedValue);
                //    }
                //    get
                //    {
                //        int gvalue = 0;
                //        return getKernelCase_(ref gvalue);
                //    }
                //}
                //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
                //protected static extern void setKernelCase_(ref int value);
                //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
                //protected static extern int getKernelCase_(ref int value);
                ///
                ///
                ///
                //protected internal int[] errorCheckDataOnMetBandC
                //{
                //    get
                //    {
                //        int num = cNumFortranProviders;
                //        int[] errorCheck = new int[num];
                //        getDataCheckOnMetBC_AF_(ref num, errorCheck);
                //        return TrimProvNameS(errorCheck);
                //    }
                // }
                //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
                //protected static extern int getDataCheckOnMetBC_AF_(ref int count, int[] values);
                /// <summary>
                /// Data checking of the model
                /// </summary>
                //protected internal int[] errorCheckDataOffMetGW
                //{
                //    get
                //    {
                //        int num = cNumFortranProviders;
                //        int[] errorCheck = new int[num];
                //        getDataCheckOffMetGW_AF_(ref num, errorCheck);
                //        return TrimProvNameS(errorCheck);
                //    }
                //}
                //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
                //protected static extern int getDataCheckOffMetGW_AF_(ref int count, int[] values);

 
            #endregion
            //===========================
            #region Utilities
            // ------------------------------------------------------------------------------
 
                ////////////////////////////////////////////////////////////////////////////////////////////////////
                /// <summary>   Executes the callback handler operation. </summary>
                ///
                /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
                ///
                /// <param name="year"> The year. Not currently used.</param>
                ////////////////////////////////////////////////////////////////////////////////////////////////////

                internal delegate void RunCallbackHandler(int year);
                protected internal enum ShortageSharingPolicyType { ProportionalSharing = 1, AgLosesWaterFirst };

                protected internal virtual int RangeCheckShortageSharingPolicy(ShortageSharingPolicyType value)
                {
                    return RangeCheck((int)ShortageSharingPolicyType.ProportionalSharing, (int)value,
                        (int)ShortageSharingPolicyType.AgLosesWaterFirst,
                        "ShortageSharingPolicy");
                }
                //
                //
                /// <summary>   The adjusted number modeled water providers. </summary>
                const int AdjNumProviders = 33;

                ////////////////////////////////////////////////////////////////////////////////////////////////////
                /// <summary>   Trim prov names. Integer array.</summary>
                ///
                /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
                ///
                /// <exception cref="Exception">    Thrown when an exception error condition occurs. </exception>
                ///
                /// <param name="OrigArray">    The array passed in (size) </param>
                ///
                /// <returns>  The adjusted array. Typically used to trim from 35 to 33. </returns>
                ////////////////////////////////////////////////////////////////////////////////////////////////////

                protected int[] TrimProvNames(int[] OrigArray)
                {
                    int[] AdjArray = new int[AdjNumProviders];
                    int iAdjArray = 0;
                    for (int iOrigArray = 0; iOrigArray < OrigArray.Length; ++iOrigArray)
                    {
                        if (0 < ProvNameIndicies[iOrigArray]) AdjArray[iAdjArray++] = OrigArray[iOrigArray];
                    }
                    if (AdjNumProviders != iAdjArray) throw new Exception("Provider Array Lengths Not Copacetic-537 in UG.cs");
                    return AdjArray;
                }

                ////////////////////////////////////////////////////////////////////////////////////////////////////
                /// <summary>   Trim prov name s. Integer array. </summary>
                ///
                /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
                ///
                /// <exception cref="Exception">    Thrown when an exception error condition occurs. </exception>
                ///
                /// <param name="OrigArray">    The array passed in (size) </param>
                ///
                /// <returns>  The static equivalent of the above method. </returns>
                ////////////////////////////////////////////////////////////////////////////////////////////////////

                protected static int[] TrimProvNameS(int[] OrigArray)
                {
                    int[] AdjArray = new int[AdjNumProviders];
                    int iAdjArray = 0;
                    for (int iOrigArray = 0; iOrigArray < OrigArray.Length; ++iOrigArray)
                    {
                        if (0 < ProvNameIndicies[iOrigArray]) AdjArray[iAdjArray++] = OrigArray[iOrigArray];
                    }
                    if (AdjNumProviders != iAdjArray) throw new Exception("Provider Array Lengths Not Copacetic-537 in UG.cs");
                    return AdjArray;
                }
                //
                protected static int[] TrimProvNameAg(int[] OrigArray)
                {
                    int[] AdjArray = new int[AdjNumProviders];
                    int iAdjArray = 0;
                    for (int iOrigArray = 0; iOrigArray < OrigArray.Length; ++iOrigArray)
                    {
                        if (0 < ProvNameIndiciesAg[iOrigArray])
                        {
                            AdjArray[iAdjArray++] = OrigArray[iOrigArray];
                        }
                        else
                        {
                            AdjArray[iAdjArray++] = -1;
                        }
                    }
                    if (AdjNumProviders != iAdjArray) throw new Exception("Provider Array Lengths Not Copacetic for Agriculture SI in User Interface.cs");
                    return AdjArray;
                }

                ////////////////////////////////////////////////////////////////////////////////////////////////////
                /// <summary>   Trim prov name bool. Bool array. </summary>
                ///
                /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
                ///
                /// <exception cref="Exception">    Thrown when an exception error condition occurs. </exception>
                ///
                /// <param name="OrigArray">    The array passed in (size) </param>
                ///
                /// <returns> The boolian equivalent of the two above. </returns>
                ////////////////////////////////////////////////////////////////////////////////////////////////////

                protected static bool[] TrimProvNameB(bool[] OrigArray)
                {
                    bool[] AdjArray = new bool[AdjNumProviders];
                    int iAdjArray = 0;
                    for (int iOrigArray = 0; iOrigArray < OrigArray.Length; ++iOrigArray)
                    {
                        if (0 < ProvNameIndicies[iOrigArray]) AdjArray[iAdjArray++] = OrigArray[iOrigArray];
                    }
                    if (AdjNumProviders != iAdjArray) throw new Exception("Provider Array Lengths Not Copacetic-537 in UG.cs");
                    return AdjArray;
                }

                ////////////////////////////////////////////////////////////////////////////////////////////////////
                /// <summary>   Trim prov names to srp. Trims to the 10 SRP memers. </summary>
                ///
                /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
                ///
                /// <exception cref="Exception">    Thrown when an exception error condition occurs. </exception>
                ///
                /// <param name="OrigArray">    The array passed in (size) </param>
                ///
                /// <returns> Only SRP members. </returns>
                ////////////////////////////////////////////////////////////////////////////////////////////////////

                protected static int[] TrimProvNamesToSRP(int[] OrigArray)
                {
                    int[] AdjArray = new int[10];
                    int iAdjArray = 0;
                    for (int iOrigArray = 0; iOrigArray < OrigArray.Length; ++iOrigArray)
                    {
                        if (0 < ProvNameIndiciesSRP[iOrigArray]) AdjArray[iAdjArray++] = OrigArray[iOrigArray];
                    }
                    if (SRPproviders != iAdjArray) throw new Exception("Provider Array Lengths Not Copacetic-537 in UG.cs");
                    return AdjArray;
                }

                const int AdjNumProvidersAg = 19;
                internal static int[] ProvNameIndiciesAg = new int[]
                {
                 1,  // Adaman Mutual
                 1,  // Arizona Water Company - White Tanks
                -1,  // Arizona-American - Paradise Valley
                -1,  // Arizona-American - Sun City
                -1,  // Arizona-American - Sun City West
                 1,  // Avondale
                -1,  // Berneil
                 1,  // Buckeye
                -1,  // Carefree
                -1,  // Cave Creek
                 1,  // Chandler
                -1,  // Chaparral City
                 1,  // City of Surprise
                 1,  // Clearwater Utilities
                -1,  // Desert Hills
                 1,  // El Mirage
                 1,  // Gilbert
                 1,  // Glendale
                 1,  // Goodyear
                 1,  // Litchfield Park
                 1,  // Mesa
                -1,  // no provider
                -1,  // other provider
                 1,  // Peoria
                 1,  // Phoenix
                 1,  // Queen Creek
                 1,  // Rigby
                 1,  // Rio Verde
                -1,  // Rose Valley
                -1,  // Scottsdale
                -1,  // Sunrise
                -1,  // Tempe
                 1,  // Tolleson
                 1,  // Valley Utilities
                 1  // West End
             };

            /// <summary>
            /// The 33 Modeled water providers. The list of the 33.
            /// </summary>
            internal static int[] ProvNameIndicies = new int[]
            {
                 1,  // Adaman Mutual
                 1,  // Arizona Water Company - White Tanks
                 1,  // Arizona-American - Paradise Valley
                 1,  // Arizona-American - Sun City
                 1,  // Arizona-American - Sun City West
                 1,  // Avondale
                 1,  // Berneil
                 1,  // Buckeye
                 1,  // Carefree
                 1,  // Cave Creek
                 1,  // Chandler
                 1,  // Chaparral City
                 1,  // City of Surprise
                 1,  // Clearwater Utilities
                 1,  // Desert Hills
                 1,  // El Mirage
                 1,  // Gilbert
                 1,  // Glendale
                 1,  // Goodyear
                 1,  // Litchfield Park
                 1,  // Mesa
                -1,  // no provider
                -1,  // other provider
                 1,  // Peoria
                 1,  // Phoenix
                 1,  // Queen Creek
                 1,  // Rigby
                 1,  // Rio Verde
                 1,  // Rose Valley
                 1,  // Scottsdale
                 1,  // Sunrise
                 1,  // Tempe
                 1,  // Tolleson
                 1,  // Valley Utilities
                 1  // West End
             };
            /// <summary>
            /// The 35 FORTRAN providers with one off-set. THe list of the 35.
            /// </summary>
                internal static int[] ProvNameIndicies35 = new int[]
            {
                 1,  // Adaman Mutual
                 1,  // Arizona Water Company - White Tanks
                 1,  // Arizona-American - Paradise Valley
                 1,  // Arizona-American - Sun City
                 1,  // Arizona-American - Sun City West
                 1,  // Avondale
                 1,  // Berneil
                 1,  // Buckeye
                 1,  // Carefree
                 1,  // Cave Creek
                 1,  // Chandler
                 1,  // Chaparral City
                 1,  // City of Surprise
                 1,  // Clearwater Utilities
                 1,  // Desert Hills
                 1,  // El Mirage
                 1,  // Gilbert
                 1,  // Glendale
                 1,  // Goodyear
                 1,  // Litchfield Park
                 1,  // Mesa
                 1,  // no provider
                 1,  // other provider 
                 1,  // Peoria
                 1,  // Phoenix
                 1,  // Queen Creek
                 1,  // Rigby
                 1,  // Rio Verde
                 1,  // Rose Valley
                 1,  // Scottsdale
                 1,  // Sunrise
                 1,  // Tempe
                 1,  // Tolleson
                 1,  // Valley Utilities
                 1,  // West End
                -1   // off-set
             };
                /// <summary>
                /// THe 35 FORTRAN water providers with the 3 MODFLOW off-sets. A list.
                /// </summary>
                internal static int[] ProvNameIndicies35all = new int[]
            {
                 1,  // Adaman Mutual
                 1,  // Arizona Water Company - White Tanks
                 1,  // Arizona-American - Paradise Valley
                 1,  // Arizona-American - Sun City
                 1,  // Arizona-American - Sun City West
                 1,  // Avondale
                 1,  // Berneil
                 1,  // Buckeye
                 1,  // Carefree
                 1,  // Cave Creek
                 1,  // Chandler
                 1,  // Chaparral City
                 1,  // City of Surprise
                 1,  // Clearwater Utilities
                 1,  // Desert Hills
                 1,  // El Mirage
                 1,  // Gilbert
                 1,  // Glendale
                 1,  // Goodyear
                 1,  // Litchfield Park
                 1,  // Mesa
                 1,  // no provider
                 1,  // other provider 
                 1,  // Peoria
                 1,  // Phoenix
                 1,  // Queen Creek
                 1,  // Rigby
                 1,  // Rio Verde
                 1,  // Rose Valley
                 1,  // Scottsdale
                 1,  // Sunrise
                 1,  // Tempe
                 1,  // Tolleson
                 1,  // Valley Utilities
                 1,  // West End
                -1, //
                -1,
                -1
             };
                /// <summary>
                /// The SRP water providers modeled. A static list.
                /// </summary>
                internal static int[] ProvNameIndiciesSRP = new int[]
            {
                 -1,  // Adaman Mutual
                 -1,  // Arizona Water Company - White Tanks
                 -1,  // Arizona-American - Paradise Valley
                 -1,  // Arizona-American - Sun City
                 -1,  // Arizona-American - Sun City West
                  1,  // Avondale
                 -1,  // Berneil
                 -1,  // Buckeye
                 -1,  // Carefree
                 -1,  // Cave Creek
                  1,  // Chandler
                 -1,  // Chaparral City
                 -1,  // City of Surprise
                 -1,  // Clearwater Utilities
                 -1,  // Desert Hills
                 -1,  // El Mirage
                  1,  // Gilbert
                  1,  // Glendale
                 -1,  // Goodyear
                 -1,  // Litchfield Park
                  1,  // Mesa
                  1,  // Peoria
                  1,  // Phoenix
                 -1,  // Queen Creek
                 -1,  // Rigby
                 -1,  // Rio Verde
                 -1,  // Rose Valley
                  1,  // Scottsdale
                 -1,  // Sunrise
                  1,  // Tempe
                  1,  // Tolleson
                 -1,  // Valley Utilities
                 -1  // West End
             };
                /// <summary>
                /// The groundwater reliance providers in group "A" as evaluated
                /// in the Sampson et al. (2011) WaterSim paper.
                /// </summary>
                internal static int[] ProvNameGWR_A = new int[]
                {
                 1,  // Adaman Mutual
                -1,  // Arizona Water Company - White Tanks
                -1,  // Arizona-American - Paradise Valley
                -1,  // Arizona-American - Sun City
                -1,  // Arizona-American - Sun City West
                -1,  // Avondale
                 1,  // Berneil
                 1,  // Buckeye
                -1,  // Carefree
                -1,  // Cave Creek
                -1,  // Chandler
                -1,  // Chaparral City
                 1,  // City of Surprise
                 1,  // Clearwater Utilities
                 1,  // Desert Hills
                 1,  // El Mirage
                -1,  // Gilbert
                -1,  // Glendale
                 1,  // Goodyear
                 1,  // Litchfield Park
                -1,  // Mesa
                -1,  // no provider
                -1,  // other provider
                -1,  // Peoria
                -1,  // Phoenix
                -1,  // Queen Creek
                 1,  // Rigby
                -1,  // Rio Verde
                 1,  // Rose Valley
                -1,  // Scottsdale
                 1,  // Sunrise
                -1,  // Tempe
                -1,  // Tolleson
                 1,  // Valley Utilities
                 1  // West End
                };
                /// <summary>
                ///  The groundwater reliance providers in group "B" as evaluated
                /// in the Sampson et al. (2011) WaterSim paper.
                /// </summary>
                internal static int[] ProvNameGWR_B = new int[]
                {
                -1,  // Adaman Mutual
                -1,  // Arizona Water Company - White Tanks
                 1,  // Arizona-American - Paradise Valley
                 1,  // Arizona-American - Sun City
                 1,  // Arizona-American - Sun City West
                 1,  // Avondale
                -1,  // Berneil
                -1,  // Buckeye
                -1,  // Carefree
                -1,  // Cave Creek
                -1,  // Chandler
                -1,  // Chaparral City
                -1,  // City of Surprise
                -1,  // Clearwater Utilities
                -1,  // Desert Hills
                -1,  // El Mirage
                -1,  // Gilbert
                -1,  // Glendale
                -1,  // Goodyear
                -1,  // Litchfield Park
                -1,  // Mesa
                -1,  // no provider
                -1,  // other provider
                -1,  // Peoria
                -1,  // Phoenix
                 1,  // Queen Creek
                -1,  // Rigby
                 1,  // Rio Verde
                -1,  // Rose Valley
                -1,  // Scottsdale
                -1,  // Sunrise
                -1,  // Tempe
                -1,  // Tolleson
                -1,  // Valley Utilities
                -1  // West End
                };
                /// <summary>
                ///  The groundwater reliance providers in group "C" as evaluated
                /// in the Sampson et al. (2011) WaterSim paper.
                /// </summary>
                internal static int[] ProvNameGWR_C = new int[]
                {
                -1,  // Adaman Mutual
                 1,  // Arizona Water Company - White Tanks
                -1,  // Arizona-American - Paradise Valley
                -1,  // Arizona-American - Sun City
                -1,  // Arizona-American - Sun City West
                -1,  // Avondale
                -1,  // Berneil
                -1,  // Buckeye
                 1,  // Carefree
                -1,  // Cave Creek
                -1,  // Chandler
                -1,  // Chaparral City
                -1,  // City of Surprise
                -1,  // Clearwater Utilities
                -1,  // Desert Hills
                -1,  // El Mirage
                 1,  // Gilbert
                -1,  // Glendale
                -1,  // Goodyear
                -1,  // Litchfield Park
                -1,  // Mesa
                -1,  // no provider
                -1,  // other provider
                 1,  // Peoria
                -1,  // Phoenix
                -1,  // Queen Creek
                -1,  // Rigby
                -1,  // Rio Verde
                -1,  // Rose Valley
                -1,  // Scottsdale
                -1,  // Sunrise
                -1,  // Tempe
                 1,  // Tolleson
                -1,  // Valley Utilities
                -1  // West End
                };
                /// <summary>
                ///  The groundwater reliance providers in group "D" as evaluated
                /// in the Sampson et al. (2011) WaterSim paper.
                /// </summary>
                internal static int[] ProvNameGWR_D = new int[]
                {
                -1,  // Adaman Mutual
                -1,  // Arizona Water Company - White Tanks
                -1,  // Arizona-American - Paradise Valley
                -1,  // Arizona-American - Sun City
                -1,  // Arizona-American - Sun City West
                -1,  // Avondale
                -1,  // Berneil
                -1,  // Buckeye
                -1,  // Carefree
                 1,  // Cave Creek
                 1,  // Chandler
                 1,  // Chaparral City
                -1,  // City of Surprise
                -1,  // Clearwater Utilities
                -1,  // Desert Hills
                -1,  // El Mirage
                -1,  // Gilbert
                 1,  // Glendale
                -1,  // Goodyear
                -1,  // Litchfield Park
                 1,  // Mesa
                -1,  // no provider
                -1,  // other provider
                -1,  // Peoria
                 1,  // Phoenix
                -1,  // Queen Creek
                -1,  // Rigby
                -1,  // Rio Verde
                -1,  // Rose Valley
                 1,  // Scottsdale
                -1,  // Sunrise
                 1,  // Tempe
                -1,  // Tolleson
                -1,  // Valley Utilities
                -1  // West End
                };

                /// <summary>
                /// Trim providers to the FORTRAN 35 for a double array
                /// </summary>
                /// <param name="OrigArray">The array passed in</param>
                /// <returns>The resulting array of the trim using ProvNameIndicies</returns>
                protected double[] TrimProvNamesD35(double[] OrigArray)
                {
                    double[] AdjArray = new double[AdjNumProviders];
                    int iAdjArray = 0;
                    for (int iOrigArray = 0; iOrigArray < OrigArray.Length; ++iOrigArray)
                    {
                        if (0 < ProvNameIndicies[iOrigArray]) AdjArray[iAdjArray++] = OrigArray[iOrigArray];
                    }
                    if (AdjNumProviders != iAdjArray) throw new Exception("Provider Array Lengths Not Copacetic-537 in UG.cs");
                    return AdjArray;
                }
                /// <summary>
                /// Trim water providers to the 38 total (MODFLOW analysis) for a double array
                /// </summary>
                /// <param name="OrigArray">Original Array</param>
                /// <returns>The resulting array using ProvNameIndiciesAll</returns>
                internal static double[] TrimProvNamesD38(double[] OrigArray)
                {
                    double[] AdjArray = new double[33];
                    int iAdjArray = 0;
                    for (int iOrigArray = 0; iOrigArray < OrigArray.Length; ++iOrigArray)
                    {
                        if (0 < ProvNameIndiciesAll[iOrigArray]) AdjArray[iAdjArray++] = OrigArray[iOrigArray];
                    }
                    if (AdjNumProviders != iAdjArray) throw new Exception("Provider Array Lengths Not Copacetic-537 in UG.cs");
                    return AdjArray;
                }

                ////////////////////////////////////////////////////////////////////////////////////////////////////
                /// <summary>   Trim prov names to d 35. A double array of the FORTRAN 35 water providers. </summary>
                ///
                /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
                ///
                /// <exception cref="Exception">    Thrown when an exception error condition occurs. </exception>
                ///
                /// <param name="OrigArray">    The array passed in. </param>
                ///
                /// <returns> The adjusted array. </returns>
                ////////////////////////////////////////////////////////////////////////////////////////////////////

                internal static double[] TrimProvNamesToD35(double[] OrigArray)
                {
                    double[] AdjArray = new double[35];
                    int iAdjArray = 0;
                    for (int iOrigArray = 0; iOrigArray < OrigArray.Length; ++iOrigArray)
                    {
                        if (0 < ProvNameIndicies35all[iOrigArray]) AdjArray[iAdjArray++] = OrigArray[iOrigArray];
                    }
                    if (cNumFortranProviders != iAdjArray) throw new Exception("Provider Array Lengths Not Copacetic-537 in UG.cs");
                    return AdjArray;
                }
                //
                /// <summary>
                /// A silly method that extracts the water providers in a particular group as defined as A,B,C, or D
                /// as created in teh Sampson et al. (2011) WaterSim paper. Not used, presently.
                /// </summary>
                /// <param name="OrigArray">The array passed in (size)</param>
                /// <param name="myArray">No idea</param>
                /// <param name="grouping">The groundwater reliance group desired</param>
                /// <returns>Returns the array seeking.  i.e., that which houses the grouping based on the
                /// standard amount of groundwater that the water provider uses in their portfolio</returns>
                internal static int [] TrimProvNamesGWR(int[] OrigArray, int myArray, int grouping)
                {
                    int[] AdjArray = new int[myArray];
                    int iAdjArray = 0;
                    for (int iOrigArray = 0; iOrigArray < OrigArray.Length; ++iOrigArray)
                    {
                        if (grouping == 1){
                            if (0 < ProvNameGWR_A[iOrigArray]) AdjArray[iAdjArray++] = OrigArray[iOrigArray];
                        }
                        else if (grouping == 2)
                        {
                            if (0 < ProvNameGWR_B[iOrigArray]) AdjArray[iAdjArray++] = OrigArray[iOrigArray];
                        }
                        else if (grouping == 3)
                        {
                            if (0 < ProvNameGWR_C[iOrigArray]) AdjArray[iAdjArray++] = OrigArray[iOrigArray];
                        }
                        else
                        {
                            if (0 < ProvNameGWR_D[iOrigArray]) AdjArray[iAdjArray++] = OrigArray[iOrigArray];
                        }
                    }
                    //if (cNumFortranProviders != iAdjArray) throw new Exception("Provider Array Lengths Not Copacetic-537 in UG.cs");
                    return AdjArray;
                }

                ////////////////////////////////////////////////////////////////////////////////////////////////////
                /// <summary>   Expand prov names 35. This is a static integer array. </summary>
                ///
                /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
                ///
                /// <exception cref="Exception">  Thrown when an exception error condition occurs. </exception>
                ///
                /// <param name="OrigArray"> The array passed in (size) </param>
                ///
                /// <returns>Expanded array from the 35 water providers modeled to the 38 used in the MODFLOW
                ///          model. i.e., uses ProvNameIndiciesAll.</returns>
                ////////////////////////////////////////////////////////////////////////////////////////////////////

                internal static int[] ExpandProvNames35(int[] OrigArray)
                {
                    int[] AdjArray = new int[36];
                    int iArray = 33;
                    int icount = 0;
                    for (int iAdjArray = 0; iAdjArray < AdjArray.Length; ++iAdjArray)
                    {
                        if (0 < ProvNameIndiciesAll[iAdjArray] )
                        {

                            AdjArray[iAdjArray] = OrigArray[icount];
                            ++icount;
                        }
                    }
                    if (icount != iArray) throw new Exception("Provider Array Lengths Not Copacetic-1530 in UG.cs");
                    return AdjArray;
                }
                /// <summary>
                /// Expand the 33 water providers modeled to the 38 as used in the MODFLOW runs for a double array.
                /// </summary>
                /// <param name="OrigArray">The orrginal array of 33 water providers found in ProvNameIndiciesAll.</param>
                /// <returns></returns>
                internal static double[] ExpandProvNames(double[] OrigArray)
                {
                    double[] AdjArray = new double[38];
                    int iArray = 33;
                    int icount = 0;
                    for (int iAdjArray = 0; iAdjArray < AdjArray.Length; ++iAdjArray)
                    {
                        if (0 < ProvNameIndiciesAll[iAdjArray])
                        {
                            AdjArray[iAdjArray] = OrigArray[icount];
                            ++icount;
                        }
                    }
                    if (icount != iArray) throw new Exception("Provider Array Lengths Not Copacetic-1530 in UG.cs");
                    return AdjArray;
                }
                /// <summary>
                /// Expand the 33 water providers modeled to the 38 as used in the MODFLOW runs for an integer array.
                /// </summary>
                /// <param name="OrigArray">The orrginal array of 33 water providers found in ProvNameIndiciesAll.</param>
                /// <returns>The array with 38 water providers.</returns>
                internal static int[] ExpandProvNamesInt(int[] OrigArray)
                {
                    int[] AdjArray = new int[38];
                    int iArray = 33;
                    int icount = 0;
                    for (int iAdjArray = 0; iAdjArray < AdjArray.Length; ++iAdjArray)
                    {
                        if (0 < ProvNameIndiciesAll[iAdjArray])
                        {
                            AdjArray[iAdjArray] = OrigArray[icount];
                            ++icount;
                        }
                    }
                    if (icount != iArray) throw new Exception("Provider Array Lengths Not Copacetic-1546 in UG.cs");
                    return AdjArray;
                }
                /// <summary>
                /// Expand the SRP water providers to the 33 modeled using an integer.
                /// </summary>
                /// <param name="OrigArray">This is a value of Ten</param>
                /// <returns>The array of 33 water providers</returns>
                internal static int[] ExpandProvNamesSRPto33(int[] OrigArray)
                {
                    int[] AdjArray = new int[33];
                    int iArray = 10;
                    int icount = 0;
                    for (int iAdjArray = 0; iAdjArray < AdjArray.Length; ++iAdjArray)
                    {
                        if (0 < ProvNameIndiciesSRP[iAdjArray])
                        {
                            AdjArray[iAdjArray] = OrigArray[icount];
                            ++icount;
                        }
                  
                    }
                    if (icount != iArray) throw new Exception("Provider Array Lengths Not Copacetic-5265 in Model_Interface.cs");
                    return AdjArray;
                }
                
                //
                /// <summary>
                /// Expand the SRP water providers to the 33 modeled using a double.
                /// </summary>
                /// <param name="OrigArray">This is 10</param>
                /// <returns>The array of 33 water providers</returns>
                internal static double[] ExpandProvNamesSRPto33D(double[] OrigArray)
                {
                    double[] AdjArray = new double[33];
                    int iArray = 10;
                    int icount = 0;
                    for (int iAdjArray = 0; iAdjArray < AdjArray.Length; ++iAdjArray)
                    {
                        if (0 < ProvNameIndiciesSRP[iAdjArray])
                        {
                            AdjArray[iAdjArray] = OrigArray[icount];
                            ++icount;
                        }

                    }
                    if (icount != iArray) throw new Exception("Provider Array Lengths Not Copacetic-1530 in UG.cs");
                    return AdjArray;
                }

            // =======================================================================================================
            //
                /// <summary>
                /// All water providers at present for the SRV.
                /// </summary>
                internal static int[] ProvNameIndiciesAll = new int[]
            {
                 1,  // Adaman Mutual
                 1,  // Arizona Water Company - White Tanks
                 1,  // Arizona-American - Paradise Valley
                 1,  // Arizona-American - Sun City
                 1,  // Arizona-American - Sun City West
                 1,  // Avondale
                 1,  // Berneil
                 1,  // Buckeye
                 1,  // Carefree
                 1,  // Cave Creek
                 1,  // Chandler
                 1,  // Chaparral City
                 1,  // City of Surprise
                 1,  // Clearwater Utilities
                 1,  // Desert Hills
                 1,  // El Mirage
                 1,  // Gilbert
                 1,  // Glendale
                 1,  // Goodyear
                 1,  // Litchfield Park
                 1,  // Mesa
                -1,  // no provider
                -1,  // other provider
                 1,  // Peoria
                 1,  // Phoenix
                 1,  // Queen Creek
                 1,  // Rigby
                 1,  // Rio Verde
                 1,  // Rose Valley
                 1,  // Scottsdale
                 1,  // Sunrise
                 1,  // Tempe
                 1,  // Tolleson
                 1,  // Valley Utilities
                 1,  // West End
                -1,  // "Gila River Reservation",
                -1,  // "Salt River Reservation",
                -1   // "Salt River Project"
             };
           
            #endregion
            //
            #region Shared Utility Routines
            /// <summary>
            /// Expand water providers from the 10 to the modeled set.
            /// </summary>
            /// <param name="srpValuesOnly">SRP data only</param>
            /// <returns>The modeled set of whatever for 33 (currently)</returns>
            protected internal virtual int[] ExpandToAllProviders(int[] srpValuesOnly)
            {
                if (null == srpValuesOnly) return null;
                int[] fullList = new int[SRPProviderNameIndices.Length];
                for (int iProv = 1; iProv < SRPProviderNameIndices.Length; ++iProv)
                {
                    fullList[iProv] = 0 > SRPProviderNameIndices[iProv] ? 0 :
                        srpValuesOnly[SRPProviderNameIndices[iProv]];
                }
                return fullList;
            }
            /// <summary>   The srp provider name indices. Offsets that return just the SRP members</summary>
            protected static int[] SRPProviderNameIndices = new int[]
        #region Offsets into SRP-only providers list
        {
            -1,  // ad   0 Adaman Mutual
            -1,  // wt   4 Arizona Water Company - White Tanks
            -1,  // pv   1 Arizona-American - Paradise Valley
            -1,  // su   2 Arizona-American - Sun City
            -1,  // sw   3 Arizona-American - Sun City West
             0,  // av   5 Avondale
            -1,  // be   6 Berneil
            -1,  // bu   7 Buckeye
            -1,  // cf   8 Carefree
            -1,  // cc   9 Cave Creek
             1,  // ch  10 Chandler
            -1,  // cp  11 Chaparral City
            -1,  // sp  12 City of Surprise
            -1,  // cu  13 Clearwater Utilities
            -1,  // dh  14 Desert Hills
            -1,  // em  15 El Mirage
             2,  // gi  16 Gilbert
             3,  // gl  17 Glendale
            -1,  // go  18 Goodyear
            -1,  // lp  19 Litchfield Park
             4,  // me  20 Mesa
             5,  // pe  21 Peoria
             6,  // ph  22 Phoenix
            -1,  // qk  23 Queen Creek
            -1,  // rg  24 Rigby
            -1,  // rv  25 Rio Verde
            -1,  // ry  26 Rose Valley
             7,  // sc  27 Scottsdale
            -1,  // sr  28 Sunrise
             8,  // te  29 Tempe
             9,  // to  30 Tolleson
            -1,  // vu  31 Valley Utilities
            -1   // we  32 West End
        };
        #endregion
            /// <summary>   The enum providers. The 35 used (old code)</summary>
            protected string[] enumProviders =
        #region Provider names in the order that WaterSim maintains them for use in enumerated arrays
        {
            "Adaman Mutual",
            "Arizona Water Company - White Tanks",
            "Arizona-American - Paradise Valley",
            "Arizona-American - Sun City",
            "Arizona-American - Sun City West",
            "Avondale_",
            "Berneil_",
            "Buckeye_",
            "Carefree_",
            "Cave Creek_",
            "Chandler_",
            "Chaparral City_",
            "City of Surprise",
            "Clearwater Utilities",
            "Desert Hills_",
            "El Mirage_",
            "Gilbert_",
            "Glendale_",
            "Goodyear_",
            "Litchfield Park_",
            "Mesa_",
            "no Provider",
            "other Mini Providers",
            "Peoria_",
            "Phoenix_",
            "Queen Creek_",
            "Rigby_",
            "Rio Verde_",
            "Rose Valley_",
            "Scottsdale_",
            "Sunrise_",
            "Tempe_",
            "Tolleson_",
            "Valley Utilities",
            "West End"
        };
        #endregion

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets a list of names of the providers. </summary>
            ///
            /// <value> A list of names of the providers. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            internal string[] ProviderNames
            {
                get
                {
                    /// <summary>   List of names of the providers. </summary>
                    string[] providerNames = new string[]
                #region Provider names in the order that WaterSim maintains them
                {
                    "Adaman Mutual",
                    "Arizona Water Company - White Tanks",
                    "Arizona-American - Paradise Valley",
                    "Arizona-American - Sun City",
                    "Arizona-American - Sun City West",
                    "Avondale",
                    "Berneil",
                    "Buckeye",
                    "Carefree",
                    "Cave Creek",
                    "Chandler",
                    "Chaparral City",
                    "City of Surprise",
                    "Clearwater Utilities",
                    "Desert Hills",
                    "El Mirage",
                    "Gilbert",
                    "Glendale",
                    "Goodyear",
                    "Litchfield Park",
                    "Mesa",
                    "no provider",
                    "other providers",
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
                    "West End"
                };
                #endregion
                    return providerNames;
                }
            }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the provider abbrev modeled. </summary>
            ///
            /// <value> The provider abbreviations of the water providers modeled. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            internal string[] ProviderAbbrevModeled
            {
                get
                {
                 string[] ProviderAbbrevModeled = new string[]
            
                {
                    "am",
                    "wt",
                    "pv",
                    "su",
                    "sw",
                    "av",
                    "be",
                    "bu",
                    "cf",
                    "cc",
                    "ch",
                    "cp",
                    "sp",
                    "cu",
                    "dh",
                    "em",
                    "gi",
                    "gl",
                    "go",
                    "lp",
                    "me",
                    "pe",
                    "ph",
                    "qk",
                    "rg",
                    "rv",
                    "ry",
                    "sc",
                    "sr",
                    "te",
                    "to",
                    "vu",
                    "we"
                };
                    return ProviderAbbrevModeled;
                }
            }
         /// <summary>  The enum srp providers. Just the 10.</summary>
         protected string[] enumSRPProviders =
        #region SRP Provider names in the order that WaterSim maintains them for use in enumerated arrays
        {
            "Avondale_SRP",
            "Chandler_SRP",
            "Gilbert_SRP",
            "Glendale_SRP",
            "Mesa_SRP",
            "Peoria_SRP",
            "Phoenix_SRP",
            "Scottsdale_SRP",
            "Tempe_SRP",
            "Tolleson_SRP"
        };
        #endregion

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets a list of names of the srp providers. </summary>
            ///
            /// <value> A list of names of the srp providers. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            internal string[] SRPProviderNames
            {
                get
                {
                    /// <summary>   List of names of the srp providers. </summary>
                    string[] srpProviderNames = new string[]
                #region Provider names in the order that WaterSim maintains them
                {
                    "Avondale",
                    "Chandler",
                    "Gilbert",
                    "Glendale",
                    "Mesa",
                    "Peoria",
                    "Phoenix",
                    "Scottsdale",
                    "Tempe",
                    "Tolleson",
                };
                #endregion
                    return srpProviderNames;
                }
            }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Converts a value to an int. </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    THe value to check. </param>
            ///
            /// <returns>   value as an int. </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal int ToInt(double value)
            {
                return Convert.ToInt32(Math.Round(value));
            }


            #region RangeChecks
            #region Simulation Duration

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the minimum start year. </summary>
            ///
            /// <value> The minimum start year. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinStartYear
            { get { return 2000; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the maximum start year. </summary>
            ///
            /// <value> The maximum start year. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxStartYear
            { get { return 2006; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check start year. </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns>   . </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckStartYear(int value)
            {
                return RangeCheck(MinStartYear, value,
                    MaxStartYear,
                    "Simulation start year out-of-bounds");
            }

            //protected internal virtual int SimulationStart
            //{ get { return 2006; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the duration of the simulation. </summary>
            ///
            /// <value> The simulation duration. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int SimulationDuration
            { get { return 75; } }

            //protected internal virtual int SimulationEnd
            //{ get { return SimulationStart + SimulationDuration; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the minimum end year. </summary>
            ///
            /// <value> The minimum end year. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinEndYear
            { get { return 2001; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the maximum end year. </summary>
            ///
            /// <value> The maximum end year. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxEndYear
            { get { return 2086; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check end year. </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns>   . </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckEndYear(int value)
            {
                return RangeCheck(MinEndYear, value,
                    MaxEndYear,
                    "Simulation end year out-of-bounds");
            }

            #endregion

            #region Colorado River Variables

            // Runoff data to use
            // -----------------------------------------------------

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the information describing the minimum colorado historical. </summary>
            ///
            /// <value> Information describing the minimum colorado historical. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinColoradoHistoricalData
            { get { return 1; } } // Paleo Data
            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the information describing the maximum colorado historical. </summary>
            ///
            /// <value> Information describing the maximum colorado historical. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxColoradoHistoricalData
            { get { return 3; } } // Other data (Bureau of Rec)
            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check colorado data. This is the historical river flow data. </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns>   . </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckColoradoData(int value)
            {
                return RangeCheck(MinColoradoHistoricalData, value,
                    MaxColoradoHistoricalData,
                    "ColoradoHistoricalDataUse");
            }
            //----------------------


            // Index year of historical runoff to use
            //---------------------------------------------------------

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the minimum colorado historical extraction start year. </summary>
            ///
            /// <value> The minimum colorado historical extraction start year. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinColoradoHistoricalExtractionStartYear
            { get { return 762; } } //1906 paleo is 762;

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the maximum colorado historical extraction start year. </summary>
            ///
            /// <value> The maximum colorado historical extraction start year. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxColoradoHistoricalExtractionStartYear
            { get { return 1980; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check colorado historical extraction start year. </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns>   . </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckColoradoHistoricalExtractionStartYear(int value)
            {
                return RangeCheck(MinColoradoHistoricalExtractionStartYear, value,
                    MaxColoradoHistoricalExtractionStartYear,
                    "ColoradoHistoricalExtractionStartYear");
            }
            //----------------------


            // Drought year on the CO to start
            // --------------------------------------------------
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the minimum colorado user adjustment start year. </summary>
            ///
            /// <value> The minimum colorado user adjustment start year. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinColoradoUserAdjustmentStartYear
            { get { return startyear; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the maximum colorado user adjustment start year. </summary>
            ///
            /// <value> The maximum colorado user adjustment start year. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxColoradoUserAdjustmentStartYear
            { get { return minStopYearDrought; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check colorado user adjustment start year. </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns>   . </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckColoradoUserAdjustmentStartYear(int value)
            {
                return RangeCheck(MinColoradoUserAdjustmentStartYear, value,
                    MaxColoradoUserAdjustmentStartYear,
                    "ColoradoUserAdjustmentStartYear");
            }
            protected internal virtual int MinUpperBasinValue
            { get { return minUpperBasinValue_; } }
            protected internal virtual int MaxUpperBasinValue
            { get { return maxUpperBasinValue_; } }

            //----------------------
            protected internal virtual int RangeCheckUpperBasinIndex(int value)
            {
                return RangeCheck(MinUpperBasinValue, value,
                    MaxUpperBasinValue,
                    "setUpperBasinIndex exception");
            }


            // Drought year on the CO River to end
            // --------------------------------------------------------
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the minimum colorado user adjustment stop year. </summary>
            ///
            /// <value> The minimum colorado user adjustment stop year. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinColoradoUserAdjustmentStopYear
            { get { return startyear; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the maximum colorado user adjustment stop year. </summary>
            ///
            /// <value> The maximum colorado user adjustment stop year. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxColoradoUserAdjustmentStopYear
            { get { return minStopYearDrought; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check colorado user adjustment stop year. </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns>   . </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckColoradoUserAdjustmentStopYear(int value)
            {
                return RangeCheck(MinColoradoUserAdjustmentStopYear, value,
                    MaxColoradoUserAdjustmentStopYear,
                    "ColoradoUserAdjustmentStopYear");
            }
            // --------------------


            // Severity of drought (or surplus)on the CO River
            // ----------------------------------------------------------
            //
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the minimum colorado user adjustment percent. </summary>
            ///
            /// <value> The minimum colorado user adjustment percent. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinColoradoUserAdjustmentPercent
            { get { return 0; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the maximum colorado user adjustment percent. </summary>
            ///
            /// <value> The maximum colorado user adjustment percent. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxColoradoUserAdjustmentPercent
            { get { return 300; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check colorado user adjustment percent. </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns>   . </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckColoradoUserAdjustmentPercent(int value)
            {
                return RangeCheck(MinColoradoUserAdjustmentPercent, value,
                    MaxColoradoUserAdjustmentPercent,
                    "ColoradoUserAdjustmentPercent");
            }
            // -------------------

            // Climate impacts on the CO River (delta runoff)
            // internal override int ColoradoClimateAdjustmentPercent
            // ----------------------------------------------------
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the minimum colorado climate adjustment percent. </summary>
            ///
            /// <value> The minimum colorado climate adjustment percent. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinColoradoClimateAdjustmentPercent
            { get { return 0; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the maximum colorado climate adjustment percent. </summary>
            ///
            /// <value> The maximum colorado climate adjustment percent. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxColoradoClimateAdjustmentPercent
            { get { return 300; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check colorado climate adjustment percent. </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns>   . </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckColoradoClimateAdjustmentPercent(int value)
            {
                return RangeCheck(MinColoradoClimateAdjustmentPercent, value,
                    MaxColoradoClimateAdjustmentPercent,
                    "ColoradoClimateAdjustmentPercent");
            }
            // -------------------


            // NOT USED
            // hold-over from the original WaterSim
            // --------------------------------------------------
            //protected internal virtual int MinColoradoActualEstimateOrRandom
            //{ get { return 1; } }
            //protected internal virtual int MaxColoradoActualEstimateOrRandom
            //{ get { return 3; } }
            //protected internal virtual int RangeCheckColoradoActualEstimateOrRandom(int value)
            //{
            //    return RangeCheck(MinColoradoActualEstimateOrRandom, value,
            //        MaxColoradoActualEstimateOrRandom,
            //        "ColoradoActualEstimateOrRandom");
            //}
            // --------------------

            #endregion

            #region Salt/Verde River Variables
            protected internal virtual int MinSTVtrace
            { get { return 20; } } // Bureau of Reclamation
            protected internal virtual int MaxSTVtrace
            { get { return 30; } } // Other data - scenarios
             protected internal virtual int RangeCheckSTVtrace(int value)
            {
                return RangeCheck(MinSTVtrace, value,
                    MaxSTVtrace,
                    "Salt-Tonto-Verde trace length to use");
            }
            //----------------------

            // Runoff data to use
            // -----------------------------------------------------
            /// <summary>
            /// The minimum value for the runoff data selection. 
            /// </summary>
            protected internal virtual int MinSVTHistoricalData
            { get { return 1; } } // Bureau of Reclamation
            /// <summary>
            /// The maximum value for the runoff data to select.
            /// </summary>
            protected internal virtual int MaxSVTHistoricalData
            { get { return 3; } } // Other data - scenarios
            /// <summary>
            /// The range check for the SVT runoff data
            /// </summary>
            /// <param name="value">THe value to check</param>
            /// <returns>Returns the value passed, or the exception</returns>
            protected internal virtual int RangeCheckSVTData(int value)
            {
                return RangeCheck(MinSVTHistoricalData, value,
                    MaxSVTHistoricalData,
                    "Salt-Verde-TontoHistoricalDataUse");
            }
            //----------------------


            // Index year of historical runoff to use
            //------------------------------------------------------------
            // internal override int SaltVerdeHistoricalExtractionStartYear

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the minimum salt verde historical extraction start year. </summary>
            ///
            /// <value> The minimum salt verde historical extraction start year. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinSaltVerdeHistoricalExtractionStartYear
            { get { return 1330; } } //1946 - 1330 AD is paleo;

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the maximum salt verde historical extraction start year. </summary>
            ///
            /// <value> The maximum salt verde historical extraction start year. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxSaltVerdeHistoricalExtractionStartYear
            { get { return 1980; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check salt verde historical extraction start year. </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns>   . </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckSaltVerdeHistoricalExtractionStartYear(int value)
            {
                return RangeCheck(MinSaltVerdeHistoricalExtractionStartYear, value,
                    MaxSaltVerdeHistoricalExtractionStartYear,
                    "SaltVerdeHistoricalExtractionStartYear");
            }
            //----------------------

            // Drought year on the Salt-Verde to start
            // ----------------------------------------------------
            // internal override int SaltVerdeUserAdjustmentStartYear
            // //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the minimum salt verde user adjustment start year. </summary>
            ///
            /// <value> The minimum salt verde user adjustment start year. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinSaltVerdeUserAdjustmentStartYear
            { get { return startyear; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the maximum salt verde user adjustment start year. </summary>
            ///
            /// <value> The maximum salt verde user adjustment start year. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxSaltVerdeUserAdjustmentStartYear
            { get { return minStopYearDrought; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check salt verde user adjustment start year. </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns>   . </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckSaltVerdeUserAdjustmentStartYear(int value)
            {
                return RangeCheck(MinSaltVerdeUserAdjustmentStartYear, value,
                    MaxSaltVerdeUserAdjustmentStartYear,
                    "SaltVerdeUserAdjustmentStartYear");
            }
            //----------------------

            // Drought year on the Salt-Verde to stop
            // ----------------------------------------------------
            // internal override int SaltVerdeUserAdjustmentStopYear
            // //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the minimum salt verde user adjustment stop year. </summary>
            ///
            /// <value> The minimum salt verde user adjustment stop year. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinSaltVerdeUserAdjustmentStopYear
            { get { return startyear; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the maximum salt verde user adjustment stop year. </summary>
            ///
            /// <value> The maximum salt verde user adjustment stop year. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxSaltVerdeUserAdjustmentStopYear
            { get { return minStopYearDrought; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check salt verde user adjustment stop year. </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns>   . </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckSaltVerdeUserAdjustmentStopYear(int value)
            {
                return RangeCheck(MinSaltVerdeUserAdjustmentStopYear, value,
                    MaxSaltVerdeUserAdjustmentStopYear,
                    "SaltVerdeUserAdjustmentStopYear");
            }
            //----------------------

            // Drought effect on runoff for the Salt-Verde 
            // ----------------------------------------------------
            // internal override int SaltVerdeUserAdjustmentPercent
            // //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the minimum salt verde user adjustment percent. </summary>
            ///
            /// <value> The minimum salt verde user adjustment percent. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinSaltVerdeUserAdjustmentPercent
            { get { return 0; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the maximum salt verde user adjustment percent. </summary>
            ///
            /// <value> The maximum salt verde user adjustment percent. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxSaltVerdeUserAdjustmentPercent
            { get { return 400; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check salt verde user adjustment percent. </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns>   . </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckSaltVerdeUserAdjustmentPercent(int value)
            {
                return RangeCheck(MinSaltVerdeUserAdjustmentPercent, value,
                    MaxSaltVerdeUserAdjustmentPercent,
                    "SaltVerdeUserAdjustmentPercent");
            }
            //----------------------

            // Climate effects on runoff for the Salt-Verde 
            // ---------------------------------------------------------
            //   internal override int SaltVerdeClimateAdjustmentPercent
            //   //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the minimum salt verde climate adjustment percent. </summary>
            ///
            /// <value> The minimum salt verde climate adjustment percent. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinSaltVerdeClimateAdjustmentPercent
            { get { return 0; } }

                ////////////////////////////////////////////////////////////////////////////////////////////////////
                /// <summary>   Gets the maximum salt verde climate adjustment percent. </summary>
                ///
                /// <value> The maximum salt verde climate adjustment percent. </value>
                ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxSaltVerdeClimateAdjustmentPercent
            { get { return 400; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check salt verde climate adjustment percent. </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns>   . </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckSaltVerdeClimateAdjustmentPercent(int value)
            {
                return RangeCheck(MinSaltVerdeClimateAdjustmentPercent, value,
                    MaxSaltVerdeClimateAdjustmentPercent,
                    "SaltVerdeClimateAdjustmentPercent");
            }
            //----------------------


            // 
            // NOT USED
            // --------------------------------------------------------------------------------
            //protected internal enum SRPReleaseEstimateMethodType { Historical = 1, TJLApproach, DASApproach };

            //protected internal virtual int RangeCheckSRPReleaseEstimateMethod(SRPReleaseEstimateMethodType value)
            //{
            //    return RangeCheck((int)SRPReleaseEstimateMethodType.Historical, (int)value,
            //        (int)SRPReleaseEstimateMethodType.DASApproach,
            //        "SRPReleaseEstimateMethod");
            //}

            //----------------------


            // This variable now controls use of the Salt-Verde-Tonto Paleo climate data - as of 26 June 2009
            // das (i.e. can no longer use a value of two if a longer stream record is desired)
            // --------------------------------------------------------------------------------
            //protected internal virtual int MinSaltVerdeActualEstimateOrRandom
            //{ get { return 1; } }

            //protected internal virtual int MaxSaltVerdeActualEstimateOrRandom
            //{ get { return 3; } }

            //protected internal virtual int RangeCheckSaltVerdeActualEstimateOrRandom(int value)
            //{
            //    return RangeCheck(MinSaltVerdeActualEstimateOrRandom, value,
            //        MaxSaltVerdeActualEstimateOrRandom,
            //        "SaltVerdeActualEstimateOrRandom");
            //}
            // --------------------------

            #endregion

            #region Population Variables: summary

            // ----------------------------------------------------------
            /// <summary>
            /// The minimum value for decreasing the population growth rate.
            /// </summary>
            protected internal virtual int MinPopulationGrowthRateAdjustmentPercent
            { get { return 0; } }
            /// <summary>
            /// The maximum value for increasing the population growth rate.
            /// </summary>
            protected internal virtual int MaxPopulationGrowthRateAdjustmentPercent
            { get { return 700; } }
            /// <summary>
            /// The range check method for population growth rate.
            /// </summary>
            /// <param name="value">The value passed in to this method</param>
            /// <returns>Returns the value passed in or the exception</returns>
            protected internal virtual int RangeCheckPopulationGrowthRateAdjustmentPercent(int value)
            {
                return RangeCheck(MinPopulationGrowthRateAdjustmentPercent, value,
                    MaxPopulationGrowthRateAdjustmentPercent,
                    "PopulationGrowthRateAdjustmentPercent");
            }

            // --------------------------

            // -----------------------------------------------------------------
            /// <summary>
            /// The range check for the county-scale water policy variable that has
            /// not been recently used.
            /// </summary>
            /// <param name="value">The water policy</param>
            /// <returns>The value back, or the exception</returns>
            protected internal virtual int RangeCheckWaterPolicy(WaterPolicyType value)
            {
                return RangeCheck((int)WaterPolicyType.NoOverdraft, (int)value,
                    (int)WaterPolicyType.FixedResidentialGPCD,
                    "WaterPolicy");
            }



            #endregion

            #region GenericProvider

            // Modify Normal flow- regression to estimate acre-feet acre-1 for
            // the trott table for daily estimates
            // 01.18.12 DAS
            // ------------------------------------------------------------------

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the minimum trott normal flow. </summary>
            ///
            /// <value> The minimum trott normal flow. This is 0.1 acre-feet per acre </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinTrottNormalFlow
            { get { return 1; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the maximum trott normal flow. </summary>
            ///
            /// <value> The maximum trott normal flow. This is 5.5 acre-feet per acre. The model
            ///         actually reduces it to the 5.4xxxxx actual maximum. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxTrottNormalFlow
            { get { return 55; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check normal flow. </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns>The value or an exception. </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckNormalFlow(int value)
            {
                return RangeCheck(MinTrottNormalFlow, value, MaxTrottNormalFlow, "Modify Normal flow estimates from Trott Table");
            }

            // Kernel Case- order that we use for water sources
            // 01.18.12 DAS
            // ------------------------------------------------------------------

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the minimum kernel case.  Not really used at the moment.</summary>
            ///
            /// <value> The minimum kernel case. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinKernelCase
            { get { return 1; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the maximum kernel case. </summary>
            ///
            /// <value> The maximum kernel case. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxKernelCase
            { get { return 2; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check kernel case. </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns>The value or an exception. </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckKernelCase(int value)
            {
                return RangeCheck(MinKernelCase, value, MaxKernelCase, "Kernel Case [order of water sources]");
            }




            // Flow to environment for Colorado flows
            // ------------------------------------------------------------------

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the minimum environment flow co. </summary>
            ///
            /// <value> The minimum environment flow co. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinEnvFlowCO
            { get { return 0; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the maximum environment flow co. </summary>
            ///
            /// <value> The maximum environment flow co. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxEnvFlowCO
            { get { return 5000000; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check environment co. </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns> The value or an exception. </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckEnvCo(int value)
            {
                return RangeCheck(MinEnvFlowCO, value, MaxEnvFlowCO, "Environment Flow Diversion Range Check Error");
            }
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the minimum environment flow verde. </summary>
            ///
            /// <value> The minimum environment flow verde. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinEnvFlowVerde
            { get { return 0; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the maximum environment flow verde. </summary>
            ///
            /// <value> The maximum environment flow verde. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxEnvFlowVerde
            { get { return 500000; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check environment verde. Acre-feet used of verde river water to be
            ///             set aside for the environment. Maximum of 15% of flow (implicit).</summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns> The value or an exception. </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckEnvVerde(int value)
            {
              
                  return RangeCheck(MinEnvFlowVerde, value, MaxEnvFlowVerde, "Environment Flow Diversion Range Check Error");
            }
            //

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the minimum environment flow salt. </summary>
            ///
            /// <value> The minimum environment flow salt. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MinEnvFlowSalt
            { get { return 0; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Gets the maximum environment flow salt. </summary>
            ///
            /// <value> The maximum environment flow salt. </value>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int MaxEnvFlowSalt
            { get { return 500000; } }

            ////////////////////////////////////////////////////////////////////////////////////////////////////
            /// <summary>   Range check environment salt. Acre-feet of water to be set aside for the
            ///             environment from the Salt River.  Maximum of 15% of flow (implicit). </summary>
            ///
            /// <remarks>   Dasamps 1, 9/12/2012. </remarks>
            ///
            /// <param name="value">    The value to check. </param>
            ///
            /// <returns>   . </returns>
            ////////////////////////////////////////////////////////////////////////////////////////////////////

            protected internal virtual int RangeCheckEnvSalt(int value)
            {
                return RangeCheck(MinEnvFlowSalt, value, MaxEnvFlowSalt, "Environment Flow Diversion Range Check Error");
            }

            protected internal virtual int MinAg
            { get { return 0; } }

            protected internal virtual int MaxAg
            { get { return 1000000; } }

            protected internal virtual int RangeCheckAg(int value)
            {
                return RangeCheck(MinAg, value, MaxAg, "Water for Ag range check error");
            }
           // 01.27.15 DAs
            protected internal virtual int MinAgIndex
            { get { return 0; } }

            protected internal virtual int MaxAgIndex
           // { get { return 20; } }
           { get { return 31; } }
            protected internal virtual int RangeCheckAgIndex(int value)
            {
                return RangeCheck(MinAgIndex, value, MaxAgIndex, "Water Index for Ag pumping curve range check error");
            }
            //
            // 01.28.15 DAs
            protected internal virtual int MinAgCreditPCT
            { get { return 0; } }

            protected internal virtual int MaxAgCreditPCT
            { get { return 100; } }

            protected internal virtual int RangeCheckAgCreditTransferPCT(int value)
            {
                return RangeCheck(MinAgCreditPCT, value, MaxAgCreditPCT, "Percnt of credits that can be transfered from Muni to Ag pumping curve range check error");
            }
            // 11.15.16 DAS
            protected internal virtual int MinAgEfficiency
            { get { return 0; } }

            protected internal virtual int MaxAgEfficiency
            { get { return 100; } }
            protected internal virtual int RangeCheckAgEfficiency(int value)
            {
                return RangeCheck(MinAgEfficiency, value,MaxAgEfficiency, "Agricultural Efficiency Variable- Range check error");
            }
            // End 11.15.16 DAS

        //
            protected internal virtual int MinShowerBathPCT
            { get { return 0; } }

            protected internal virtual int MaxShowerBathPCT
            { get { return 50; } }

            protected internal virtual int RangeCheckShowerBath(int value)
            {
                return RangeCheck(MinShowerBathPCT, value, MaxShowerBathPCT, "Pct indoor residential for showers and baths");
            }

            // 

            // ------------------------------------------------------------------
            protected internal virtual int MinGPCDmethod
            { get { return 0; } }

            protected internal virtual int MaxGPCDmethod
            { get { return 3; } }

            protected internal virtual int RangeCheckGPCDmethod(int value)
            {
                return RangeCheck(MinGPCDmethod, value, MaxGPCDmethod, "GPCD method");
            }


            // Provider level
            // Reduction in GPCD (%) by thte year 2100; used in the SES algorithm
            // ------------------------------------------------------------------
            protected internal virtual int MinGPCDpct
            { get { return -75; } }

            protected internal virtual int MaxGPCDpct
            { get { return 75; } }

            protected internal virtual int RangeCheckGPCDpct(int value)
            {
                return RangeCheck(MinGPCDpct, value, MaxGPCDpct, "GPCD pct reduction");
            }
            // -------------------------
            // Old code- County scale
            // ------------------------------------------------------------------
            protected internal virtual int MinWaterPolicyStartYear
            { get { return SimulationStart; } }

            protected internal virtual int MaxWaterPolicyStartYear
            { get { return SimulationEnd; } }

            protected internal virtual int RangeCheckWaterPolicyStartYear(int value)
            {
                return RangeCheck(MinWaterPolicyStartYear, value,
                    MaxWaterPolicyStartYear,
                    "WaterPolicyStartYear");
            }

            // Old code- County scale
            // internal override int ResidentialGPCD
            // ------------------------------------------------------
            protected internal virtual int MinResidentialGPCD
            { get { return 0; } }

            protected internal virtual int MaxResidentialGPCD
            { get { return 1200; } }

            protected internal virtual int RangeCheckResidentialGPCD(int value)
            {
                return RangeCheck(MinResidentialGPCD, value,
                    MaxResidentialGPCD,
                    "ResidentialGPCD");
            }
            // Old code- County scale
            //  internal override int ResidDensityPath
            // ------------------------------------------------------------------
            protected internal virtual int MinResidDensityPath
            { get { return 1; } }

            protected internal virtual int MaxResidDensityPath
            { get { return 3; } }

            protected internal virtual int RangeCheckResidDensityPath(int value)
            {
                return RangeCheck(MinResidDensityPath, value,
                    MaxResidDensityPath,
                    "ResidDensityPath");
            }
            protected internal virtual int MinHHoldPcts
            { get { return 0; } }

            protected internal virtual int MaxHHoldPcts
            { get { return 100; } }

            protected internal virtual int RangeCheckHHoldPct(int value)
            {
                return RangeCheck(MinHHoldPcts, value,
                    MaxHHoldPcts,
                    "Household percents");
            }
            protected internal virtual int MinCAGRD
            { get { return 0; } }

            protected internal virtual int MaxCAGRD
            { get { return 100; } }

            protected internal virtual int RangeCheckCAGRD(int value)
            {
                return RangeCheck(MinCAGRD, value,
                    MaxCAGRD,
                    "CAGRD water");
            }

            // -------------------------


            // Water demand options
            // internal override int ProviderDemandOption
            // ------------------------------------------------------------------
            protected internal virtual int MinDemandOption
            { get { return 1; } }

            protected internal virtual int MaxDemandOption
            { get { return 4; } }

            protected internal virtual int RangeCheckDemandOption(int value)
            {
                return RangeCheck(MinDemandOption, value,
                    MaxDemandOption,
                    "Demand Option");
            }
            // Old code- County scale
            //  internal override int AgricultureRetirementYear
            // ------------------------------------------------------------------
            protected internal virtual int MinAgricultureRetirementYear
            { get { return 2020; } }

            protected internal virtual int MaxAgricultureRetirementYear
            { get { return 2099; } }

            protected internal virtual int RangeCheckAgricultureRetirementYear(int value)
            {
                return RangeCheck(MinAgricultureRetirementYear, value,
                    MaxAgricultureRetirementYear,
                    "AgricultureRetirementYear");
            }
            //  internal override int   
            // ------------------------------------------------------------------
            /// <summary>
            /// The minimum year that can be chosen for the Climate Factor End Year. This
            /// is the year that the Climate Factor Adjustment Percent reaches the desired value.
            /// </summary>
            protected internal virtual int MinClimateFactorEndYear
            { get { return 2010; } }
            /// <summary>
            /// The maximum year that can be chosen for the Climate Factor End Year. This
            /// is the year that the Climate Factor Adjustment Percent reaches the desired value.
            /// </summary>
            protected internal virtual int MaxClimateFactorEndYear
            { get { return 2085; } }
            /// <summary>
            /// The method that returns the value or exception for the Climate Factor End Year.
            /// </summary>
            /// <param name="value">The value passed (or returned)</param>
            /// <returns>The value, or the exception depending on what was sent</returns>
            protected internal virtual int RangeCheckClimateFactorEndYear(int value)
            {
                return RangeCheck(MinClimateFactorEndYear, value,
                    MaxClimateFactorEndYear,
                    "Climate Factor End Year");
            }

            // ------------------------------------------------------------------
 

            // Change water demand as a percent of read input
            // protected internal int DemandPercent
            // ------------------------------------------------------------------
            /// <summary>
            /// Place holder for the minimum reduction in water demand for manipulating
            /// the water demand values read in as input from the Model.
            /// </summary>
            protected internal virtual int MinDemandPercent
            { get { return 1; } }
            /// <summary>
            /// Place holder for the maximum increase in water demand for manipulating
            /// the water demand values read in as input from the Model.
            /// </summary>
            protected internal virtual int MaxDemandPercent
            { get { return 300; } }
            /// <summary>
            /// The range check for manipulating water demand estimates that are read in from 
            /// a text file in the model.  No longer connected.
            /// </summary>
            /// <param name="value">The minimum or maximum value</param>
            /// <returns>The value if acceptable, an exception if not.</returns>
            protected internal virtual int RangeCheckDemandPercent(int value)
            {
                return RangeCheck(MinDemandPercent, value,
                    MaxDemandPercent,
                    "Demand Percent");
            }
            #endregion

            #region Generic
            protected internal int RangeCheck(int min, int value, int max, string tag)
            {
                if (min > value || max < value)
                {
                    throw new ArgumentException(tag + " value is out of range");
                }
                return value;
            }
            int cNumFortran = 35;
            protected internal int[] RangeChecks(int min, int[] values, int max, string tag)
            {
                for (int num = 0; num < cNumFortran; ++num)
                {
                    if (min > values[num] || max < values[num])
                    {
                        throw new ArgumentException(tag + " Value is out of range");
                    }
                }
                return values;
            }

            internal double RangeCheck(double min, double value, double max, string tag)
            {
                if (min > value || max < value)
                {
                    throw new ArgumentException(tag + " value is out of range");
                }
                return value;
            }
            #endregion

            #endregion
            /// <summary>
            /// Default exception for properties/methods/operations that
            /// are not implemented.
            /// </summary>
            protected internal void NotImplementedException()
            {
                throw new Exception("The method or operation is not implemented.");
            }
            #endregion
        //===========================      
        #region Generic Templates

                // Provider parameter
                //
                //protected internal int[] parmX
                //{
                //    get
                //    {
                //        int num = cNumFortranProviders;
                //        int[] parmX = new int[num];
                //        getCall_(ref num, parmX);
                //        return WaterSimU.TrimProvNameS(parmX);
                //    }
                //    set
                //    {
                //        int numbers = cNumModeledProviders;
                //        int[] myout = new int[numbers];
                //        myout = value;
                //        setCall_(ref numbers, ExpandProvNames35(myout));
                //    }
                //}
                //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
                //protected static extern int getCall_(ref int count, int[] values);

                //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
                //protected static extern void setCall_(ref int count, int[] values);


                // Base Parameter setter
                //
                //protected internal int set_Parm
                //{
                //    set
                //    {
                //        int checkedValue = RangeCheckX(value);
                //        setCall_(ref checkedValue);
                //    }
                //}
                //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
                //protected static extern void setCall_(ref int value);

                // Base Parameter getter
                //
                //protected internal int get_Parm
                //{
                //    get
                //    {

                //        int gvalue = 0;
                //        int myInt = getCall_(ref gvalue);
                //        return myInt;

                //    }
                //}
                //[DllImport(@FortranDLLFilename, CallingConvention = CallingConvention.Cdecl)]
                //protected static extern int getCall_(ref int value);

        #endregion
    }
    #region Exceptions
    public class Interface_Exception : Exception
    {

        /// <summary> Identifier string for this exception</summary>
        protected const string Pre = "WaterSim Interface Error-no connection to dll: ";

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Constructor. </summary>
        ///
        /// <remarks> Ray, 1/24/2013. </remarks>
        ///
        /// <param name="message"> The message. </param>
        ///-------------------------------------------------------------------------------------------------

        public Interface_Exception(string message) : base(Pre + message) { }

        public Interface_Exception(int index) : base(Pre + Interface_strings.Get(index)) { }
            
    }
    public static class Interface_strings
    {
        public const int IFreadData = 0;
        public const int IFrunModel = 1;
        public const int IFconnectModel = 2;
        public const int NumberOfStrings = 3;

        internal static string[] values = new string[NumberOfStrings]
        {
            "Error on Read data",
            "Error on Run Model",
            "No Connection to dll"
        };
            public static bool Valid(int index)
        {
            return ((index>0)&(index<NumberOfStrings));
        }
        //-----------------------------------------------------------------

        /// <summary>   Get a WaterSimDCDC string. </summary>
        ///
        /// <remarks>Returns a WaterSimDCDC for the index value   </remarks>
        ///
        /// <param name="index">    Zero-based index of the array of strings. </param>
        ///
        /// <returns>   . </returns>

        public static string Get(int index)
        {
            if (Valid(index)) return values[index];
            else return "";
        }
    }
 
    #endregion
}
  
    