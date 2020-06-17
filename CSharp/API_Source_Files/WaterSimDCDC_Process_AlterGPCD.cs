using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.Drawing;
using WaterSimDCDC;
using WaterSimDCDC.Documentation;
using System.IO;    
namespace WaterSimDCDC.Processes
{
    // Form used to set initial values
    /// <summary>   AlterGPCDFeedbackProcessForm- Form for viewing the alter gpcd feedback process. </summary>
    #region Form
    public partial class AlterGPCDFeedbackProcessForm : Form
    {
        const int DEFAULTMINDEFICIT = 0;
        const int DEFAULTMAXDEFICIT = 100;
        const int DEFAULTMINGPCD = 70;
        const int DEFAULTMAXGPCD = 1000;

        internal string FProcessName = "";
        internal int FMaxDifference = 50;
        internal int FMinGPCD = 70;

        public AlterGPCDFeedbackProcessForm()
        {
            InitializeComponent();
            textBox_ProcessName.Text = FProcessName;
            textBoxMaxDeficit.Text = FMaxDifference.ToString();
            textBoxMinGPCD.Text = FMinGPCD.ToString();
        }

        private bool TestMaxDeficit(ref int value)
        {
            bool testresult = false;
            string textmax = textBoxMaxDeficit.Text.Trim();
            try
            {
                int Test = Convert.ToInt32(textmax);
                if ((Test < DEFAULTMINDEFICIT) || (Test > DEFAULTMAXDEFICIT))
                    testresult = false;
                else
                {
                    testresult = true;
                    value = Test;
                }
            }
            catch
            {
                testresult = false;
            }
            return testresult;
        }

        private bool TestMinGPCD(ref int value)
        {
            bool testresult = false;
            string textmin = textBoxMinGPCD.Text.Trim();
            try
            {
                int Test = Convert.ToInt32(textmin);
                if ((Test < DEFAULTMINGPCD) || (Test > DEFAULTMAXGPCD))
                    testresult = false;
                else
                {
                    testresult = true;
                    value = Test;
                }
            }
            catch
            {
                testresult = false;
            }
            return testresult;
        }

        private void buttonOK_Click(object sender, EventArgs e)
        {
            string ExitMessage = "";
            bool OKtoClose = true;
            // Do field checks
            if (textBox_ProcessName.Text == "")
            {
                ExitMessage = "Process name can not be blank.  ";
                textBox_ProcessName.Text = FProcessName;
                OKtoClose = false;
            }
            int dummy = 0;
            if (!TestMaxDeficit(ref dummy))
            {
                ExitMessage += "Deficit trigger must be in range " + DEFAULTMINDEFICIT.ToString() + " to " + DEFAULTMAXDEFICIT.ToString() + ".  ";
                textBoxMaxDeficit.Text = FMaxDifference.ToString();
                OKtoClose = false;
            }
            if (!TestMinGPCD(ref dummy))
            {
                ExitMessage += "Minimum GPCD must be in range " + DEFAULTMINDEFICIT.ToString() + " to " + DEFAULTMAXDEFICIT.ToString() + ".  ";
                textBoxMinGPCD.Text = FMinGPCD.ToString();
                OKtoClose = false;
            }

            if (OKtoClose)
            {
                this.DialogResult = System.Windows.Forms.DialogResult.OK;
                this.Close();
            }
            else
            {
                MessageBox.Show(ExitMessage);
            }

        }

        private void textBox_ProcessName_TextChanged(object sender, EventArgs e)
        {
            if (textBox_ProcessName.Text.Trim() != "")
            {
                FProcessName = textBox_ProcessName.Text.Trim();
                StatusLabel.Text = "";
            }
            else
                StatusLabel.Text = "Name cannot be blank.";
        }

        public string ProcessName
        {
            get { return FProcessName; }
            set { textBox_ProcessName.Text = value; } // Should set FProcessname when TextChange event triggered
        }

        public int MaxDifference
        {
            get { return FMaxDifference; }
            set { textBoxMaxDeficit.Text = value.ToString(); }  // should evoke text change 
        }

        public int MinGPCD
        {
            get { return FMinGPCD; }
            set { textBoxMinGPCD.Text = value.ToString(); }  // should evoke text change 
        }

        private void textBoxMaxDeficit_TextChanged(object sender, EventArgs e)
        {

            int value = 0;
            if (!TestMaxDeficit(ref value))
                StatusLabel.Text = "Deficit must be in range of " + DEFAULTMINDEFICIT.ToString() + " to " + DEFAULTMAXDEFICIT.ToString();
            else
            {
                FMaxDifference = value;
                StatusLabel.Text = "";
            }
        }

        private void textBoxMinGPCD_TextChanged(object sender, EventArgs e)
        {
            int value = 0;
            if (!TestMinGPCD(ref value))
                StatusLabel.Text = "Deficit must be in range of " + DEFAULTMINGPCD.ToString() + " to " + DEFAULTMAXGPCD.ToString();
            else
            {
                FMinGPCD = value;
                StatusLabel.Text = "";
            }
        }
    }
    

#endregion
   
    /// <summary>   Alter gpcd feedback process. </summary>

    #region Feedback Process
    public class AlterGPCDFeedbackProcess : WaterSimDCDC.AnnualFeedbackProcess
    {
        const int MINGPCDALLOWED = 90;
        const int MAXDifferenceAllowed = 10;
        int FMaxDifference = 0;
        int FMinGPCD = 0;
        int FGPCDDeclineFactor = 0;
        double AdjustFactor_Double = 0.0;
        ProviderDoubleArray GPCD_old = new ProviderDoubleArray(0);
        ProviderDoubleArray GPCD_raw = new ProviderDoubleArray(0);
        ProviderDoubleArray GPCD_previous = new ProviderDoubleArray(0);
        double[] GPCD_original = new double[ProviderClass.NumberOfProviders];
        double YrCount = 0.0;
        double Diff = 0;
        double Slope = 0;
        //protected internal int[] InitialCredits = new int[ProviderClass.NumberOfProviders];
       // double[] differenceCredits = new double[ProviderClass.NumberOfProviders];
        //double[] differenceOut = new double[ProviderClass.NumberOfProviders];
        double[] PBalance = new double[ProviderClass.NumberOfProviders];
        double[] Bcurrent = new double[ProviderClass.NumberOfProviders];
        double[] PreviousBal = new double[ProviderClass.NumberOfProviders];
        double ModResponse = 0.5;
        double[] yearPlusplus = new double[ProviderClass.NumberOfProviders] ;
       // ProviderIntArray defaultTargetGPCD = new ProviderIntArray(0);
        double[] dNewTarget = new double[ProviderClass.NumberOfProviders];
        double[] Result = new double[ProviderClass.NumberOfProviders];
        int[] addDefault = new int[ProviderClass.NumberOfProviders];
        double[] originalModified = new double[ProviderClass.NumberOfProviders];
        double[] newModified = new double[ProviderClass.NumberOfProviders];
        /// <summary>
        ///  Sampson 10.07.15
        /// </summary>
        private void seti_BaseYear(int value) { BaseYear = value; }
        int _BaseYear = 2015;


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

        protected bool Ftemporal = false;

         ///-------------------------------------------------------------------------------------------------
        /// <summary>   Constructor. </summary>
        /// <param name="aName">    Name of Process. </param>
        ///-------------------------------------------------------------------------------------------------

        public AlterGPCDFeedbackProcess(string aName)
            : base(aName)
        {
            EvokeDialogAndFetchValues();
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Default constructor. </summary>
        ///-------------------------------------------------------------------------------------------------

        public AlterGPCDFeedbackProcess() : base("Alter GPCD Based on Deficit")
        {
            EvokeDialogAndFetchValues();
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Constructor. </summary>
        /// <param name="aName"> Name of Process. </param>
        /// <param name="WSim">  The WatewrSimManager who will register process</param>
        ///-------------------------------------------------------------------------------------------------

        public AlterGPCDFeedbackProcess(string aName, WaterSimManager WSim)  : base(aName)
        {
            EvokeDialogAndFetchValues();
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Constructor. </summary>
        /// <param name="WSim"> The WatewrSimManager who will register process. </param>
        ///-------------------------------------------------------------------------------------------------

        public AlterGPCDFeedbackProcess(WaterSimManager WSim)
            : base("Alter GPCD Based on Groundwater Credit Balance")
        {
            EvokeDialogAndFetchValues();
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Constructor. </summary>
        /// <remarks>Does not evoke form for initial State Values.  Values set at defaults</remarks>
        /// <param name="WSim">     The WatewrSimManager who will register process. </param>
        /// <param name="Quiet">    true to quiet. </param>
        ///-------------------------------------------------------------------------------------------------

        public AlterGPCDFeedbackProcess(WaterSimManager WSim, bool Quiet)
            : base("Alter GPCD Based on Groundwater Credit Balance")
        {
            if (Quiet)
            {
                Fname = "DEFAULT PROCESS";
                FMaxDifference = MAXDifferenceAllowed;
                FMinGPCD = MINGPCDALLOWED;
            }
            else
            {
                EvokeDialogAndFetchValues();
            }
        }
        /// <summary>
        ///  Sampson 10.07.15
        /// </summary>
        public bool Step
        {
            get { return Ftemporal; }
            set { Ftemporal = value; }
        }
        
        /// <summary>
        ///  Sampson 10.07.15
        /// </summary>
        /// <param name="WSim"></param>
        /// <param name="Quiet"></param>
        /// 
        public AlterGPCDFeedbackProcess(WaterSimManager WSim,bool Quiet,string Astep)
            : base("Alter GPCD Based annually")
        {
            if (Quiet)
            {
                Ftemporal = Quiet;
                Fname = "Annual PROCESS";
                //FMaxDifference = MAXDifferenceAllowed;
                //FMinGPCD = MINGPCDALLOWED;
                 int year = 2000;
               
            }
            else
            {
                EvokeDialogAndFetchValues();
            }
        }


        ///-------------------------------------------------------------------------------------------------
        /// <summary> Evokes dialog and fetches values. </summary>
        ///-------------------------------------------------------------------------------------------------

        protected void EvokeDialogAndFetchValues()
        {
            AlterGPCDFeedbackProcessForm FPF = new AlterGPCDFeedbackProcessForm();
            if (Fname!="") FPF.ProcessName = Fname;
            if (FPF.ShowDialog() == DialogResult.OK)
            {
                Fname = FPF.ProcessName;
                // ===================================
                //Get data from form and set process values here
                // 
                FMaxDifference = FPF.MaxDifference;
                FMinGPCD = FPF.MinGPCD;
                // 
                // ====================================
            }
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Builds the description strings. </summary>
        ///-------------------------------------------------------------------------------------------------

        protected override void BuildDescStrings()
        {
            FProcessDescription = "Alter's GPCD if credits < " + FMaxDifference.ToString() + "%";
            FProcessLongDescription = "When a provider's prior year credit balance exceeds MaxDifference (" + FMaxDifference.ToString() + "%" +
                "(, lowers GPCD to create a positive credit trend, but no lower than MinGPCD (" + FMinGPCD.ToString() + ").";
            FProcessCode = "GPCD_DEF_"+FMaxDifference.ToString()+"_MIN_"+FMinGPCD.ToString();
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Gets the class description. </summary>
        /// <returns> Class Description. </returns>
        ///-------------------------------------------------------------------------------------------------

        new static public string ClassDescription()
        {
            return "Alter GPCD based on MaxDifference trigger and Minimum GPCD set with Dialog.";

        }

        internal int AdjustGPCD(int OriginalGPCD)
        {
            int newGPCD;
            double YearAdjustFactor = AdjustFactor_Double - (AdjustFactor_Double * (System.Math.Pow(1 + AdjustFactor_Double, YrCount) - 1));           // convert factors to double
            double NewGPCD_Double = Convert.ToDouble(OriginalGPCD);
            // calculate newgpcd by decline by adjustfactor
            NewGPCD_Double = NewGPCD_Double - (NewGPCD_Double * YearAdjustFactor);
            newGPCD = Convert.ToInt32(NewGPCD_Double);
            // check if below Min
            if (newGPCD < FMinGPCD)
                newGPCD = FMinGPCD;
            return newGPCD;
        }

        internal double AdjustGPCD(double OriginalGPCD)
        {
            // calculate newgpcd by decline by adjustfactor
            double YearAdjustFactor = AdjustFactor_Double - (AdjustFactor_Double * (System.Math.Pow(1 + AdjustFactor_Double, YrCount) - 1));
            double NewGPCD = OriginalGPCD - (OriginalGPCD * YearAdjustFactor);
            // check if below Min
            double MinGPCD = Convert.ToDouble(FMinGPCD);
            if (NewGPCD < MinGPCD)
                NewGPCD = MinGPCD;
            return NewGPCD;
        }

        public override DocTreeNode Documentation()
        {
            DocTreeNode node = base.Documentation();
            node.AddChildField("FMaxDifference", FMaxDifference.ToString());
            node.AddChildField("FMinGPCD", FMinGPCD.ToString());
            return node;
        }
        //
     
    
        ///-------------------------------------------------------------------------------------------------
        /// <summary> method that is called when a Simulation is initialized. </summary>
        /// <param name="WSim"> The WaterSimManager that is making call. </param>
        /// <returns> true if it succeeds, false if it fails. </returns>
        /// ### <seealso cref="ProcessStarted"/>
        ///-------------------------------------------------------------------------------------------------

        public override bool ProcessInitialized(WaterSimManagerClass WSim)
        {
            return base.ProcessInitialized(WSim);
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> method that is called right before the first year of a simulation is called.
        ///     </summary>
        /// <param name="year"> The year about to be run. </param>
        /// <param name="WSim"> The WaterSimManager that is making call. </param>
        /// <returns> true if it succeeds, false if it fails. </returns>
        ///-------------------------------------------------------------------------------------------------
        ProviderIntArray Initial_Credits = new ProviderIntArray(0); 
 
        public override bool ProcessStarted(int year, WaterSimManagerClass WSimClass)
        {
            WaterSimManager WSim = (WSimClass as WaterSimManager);
              
            // OK Simulation Starting, Parameters have been set, fetch the ReduceGOCD factor and use it to calculate rate
            YrCount = 1;
            FGPCDDeclineFactor = WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epPCT_Alter_GPCD).Value; // 7/29  BaseModel_ParameterBaseClass(eModelParam.epPCT_Reduce_GPCD).Value;
            double TotalYears = Convert.ToDouble((WSim.Simulation_End_Year - WSim.Simulation_Start_Year) + 1);
            AdjustFactor_Double = (Convert.ToDouble(FGPCDDeclineFactor) / 100) / TotalYears;
            //
            return base.ProcessStarted(year, WSim);
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Method that is called before each annual run. </summary>
        /// <param name="year"> The year about to be run. </param>
        /// <param name="WSim"> The WaterSimManager that is making call. </param>
        /// <returns> true if it succeeds, false if it fails. Error should be placed in FErrorMessage.
        ///     </returns>
        ///-------------------------------------------------------------------------------------------------


        public override bool PreProcess(int year, WaterSimManagerClass WSimClass)
        {

            WaterSimManager WSim = (WSimClass as WaterSimManager);
            double GPCD_Decline_Double = 0;
            double MyGPCD = 0;
            // I  yet do not understand "Step"
            Step = true;
            // // Check if first run
            ProviderIntArray FUSED_GPCD = WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epGPCD_Used).ProviderProperty.getvalues();
            ProviderIntArray RAW_GPCD = WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epGPCDraw).ProviderProperty.getvalues();
     
                // OK Not First year
                // Check if model is locked, if so unlock it.
                bool TempLock = WSim.isLocked();
                if (TempLock) WSim.UnLockSimulation();
                double[] previousBalance = new double[ProviderClass.NumberOfProviders];
                // Get data we need to evaluate GPCD
                ModelParameterBaseClass MP;
                MP = WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epGroundwater_Balance);

                ProviderIntArray FBalance = MP.ProviderProperty.getvalues();
                ProviderIntArray FPCT_BAL = WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epGroundwater_Balance).ProviderProperty.getvalues();
                ProviderIntArray New_slope = new ProviderIntArray(0);
                // ---------------------------------------------------------------------------------------------------------------------------------------------------
                if (Step)
                {
                    MyGPCDProcess(year, (WSim as WaterSimManager));
                }
                else
                {

                    for (int i = 0; i < Initial_Credits.Length; i++)
                    {
                        if (year == 2001)
                        {
                            PBalance[i] = Initial_Credits[i];
                        }
                        else
                        {
                            PBalance[i] = Bcurrent[i];
                        }
                        PreviousBal[i] = PBalance[i];
                        Bcurrent[i] = Convert.ToDouble(FBalance[i]);
                    }
                    if (2011 < year)
                    {
                        //
                        for (int i = 0; i < ProviderClass.NumberOfProviders; i++)
                        {
                            New_slope[i] = Convert.ToInt32(WSim.API_defaultSlope_GPCD[i]);
                        }
                        WSim.PCT_alter_GPCD.setvalues(New_slope);
                        //
                        // create and array to set GPCD
                        // ProviderIntArray FSet_GPCD = new ProviderIntArray(0);
                        double Islope = 0;
                        double maxResponse = -10;
                        // Loop through providers and check for PCT_Deficit that exceed FMaxDifference
                        bool SetGPCDS = false;
                        for (int i = 0; i < FPCT_BAL.Length; i++)
                        {
                            //if (i == provider)
                            //{
                            Diff = 0;
                            Islope = 0;
                            // this is strange code! how does it work?
                            eProvider ep = (eProvider)i;
                            Islope = utilities.Slope(Convert.ToDouble(year - 1), Convert.ToDouble(year), PreviousBal[i], Bcurrent[i]);
                            Diff = utilities.Difference(Bcurrent[i], PreviousBal[i]);

                            MyGPCD = GPCD_original[i];

                            GPCD_Decline_Double = 0;
                            int myDiff = 0;
                            double DiffUse = 0;

                            if (0 <= Diff)
                            {
                                myDiff = 1;
                                MyGPCD = GPCD_raw[i];
                            }
                            if (Diff < 0)
                            {
                                myDiff = 2;

                            }

                            double divide = 0;
                            double alterGPCD = 0;
                            Slope = Islope;

                            switch (myDiff)
                            {
                                case 1:
                                    GPCD_Decline_Double = MyGPCD;
                                    break;
                                case 2:
                                    DiffUse = Math.Abs(Math.Max(-0.999999, Diff));
                                    divide = 2000 * (1 - DiffUse);
                                    //GPCD_Decline_Double = MyGPCD + (Math.Min(maxResponse, (Islope / divide) * (1 + (DiffUse / 1.5))) * ModResponse);
                                    alterGPCD = Math.Max(maxResponse, ((Islope * ModResponse) / divide) * (1 + DiffUse) / 2);
                                    GPCD_Decline_Double = MyGPCD + alterGPCD;

                                    // Force flat responses to decrease by one GPCD per year
                                    // for those water providers with a decreasing trend in GPCD
                                    // 09.22.15 DAS
                                    if (GPCD_Decline_Double == MyGPCD)
                                    {
                                        if (WSim.PCT_alter_GPCD.getvalues().Values[i] < 0)
                                        {
                                            GPCD_Decline_Double = MyGPCD - 1;
                                        }
                                    }
                                    if (MyGPCD < GPCD_Decline_Double)
                                    {
                                        if (WSim.PCT_alter_GPCD.getvalues().Values[i] == 0)
                                        {
                                            GPCD_Decline_Double = MyGPCD - alterGPCD;
                                        }
                                    }

                                    break;

                                default:
                                    GPCD_Decline_Double = MyGPCD;

                                    break;
                            }

                            int New_GPCD = Convert.ToInt32(GPCD_Decline_Double);
                            if ((New_GPCD > FMinGPCD))
                            {
                                FUSED_GPCD[i] = New_GPCD;
                            }
                            else
                            {
                                FUSED_GPCD[i] = FMinGPCD;
                            }
                            GPCD_old[i] = FUSED_GPCD[i];
                            SetGPCDS = true;

                            if (Islope == 0)
                            {
                                GPCD_old[i] = AdjustGPCD(GPCD_old[i]);
                                FUSED_GPCD[i] = Convert.ToInt32(GPCD_old[i]);
                            }

                            //} // provider
                            //
                        }
                        // All Done
                        if (SetGPCDS)
                            WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epUse_GPCD).ProviderProperty.setvalues(FUSED_GPCD);

                        // if model was locked comming in, set lock going out
                        if (TempLock) WSim.LockSimulation();


                    } // 2012 < year
                }
                return base.PreProcess(year, WSim);
            
        }
     
        public void MyPreProcess(int year, WaterSimManager WS)
        {
            if (year == 2000)
            {
                for (int p = 0; p < ProviderClass.NumberOfProviders; p++)
                {
                    Initial_Credits[p] = WS.Groundwater_Balance.getvalues().Values[p];
                }

            }
        }
        public void MyGPCDProcess(int year, WaterSimManager WSim)
        {
          
            int[] final = new int[ProviderClass.NumberOfProviders];
            double[] increment = new double[ProviderClass.NumberOfProviders];
            double[] gpcd2013 = new double[ProviderClass.NumberOfProviders];

            double mod = 0;
            //double smoothCurve = 2;
            // 07.19.16 DAS was testing this....
            double smoothCurve = 3.25;
            // ===========================================

            ModResponse = Convert.ToDouble(FMaxDifference) * 0.01;
            // Check if first run
            ProviderIntArray FUSED_GPCD = WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epGPCD_Used).ProviderProperty.getvalues();
            ProviderIntArray RAW_GPCD = WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epGPCDraw).ProviderProperty.getvalues();
            ProviderIntArray New_Int = new ProviderIntArray(0);
            ProviderIntArray New_slope = new ProviderIntArray(0);
            YrCount++;
            // Set oldGOCD to these values.
            for (int i = 0; i < FUSED_GPCD.Values.Length; i++)
            {
                GPCD_old[i] = Convert.ToDouble(FUSED_GPCD[i]);
                GPCD_original[i] = Convert.ToDouble(FUSED_GPCD[i]);
                GPCD_raw[i] = Convert.ToDouble(RAW_GPCD[i]);
            }
            //}
            // not first year, do it (NEVER being set....) 07.19.16 DAS
            if (Step)
            {
                if (year < 2002)
                {
                    for (int p = 0; p < RAW_GPCD.Values.Length; p++) { yearPlusplus[p] = 1; }
                    //
                }
                else
                {
                    if (year == 2013) { for (int p = 0; p < RAW_GPCD.Values.Length; p++) { gpcd2013[p] = GPCD_raw[p]; } }
                }
                // --------------------------------------------------------------------------------------------
                int minGPCD = 0;
                //if (WSim.BaseYear <= year)
                if (BaseYear <= year)
                {
                    if (WSim.Provider_Demand_Option == 3)
                    {
                        for (int i = 0; i < RAW_GPCD.Values.Length; i++)
                        {
                            originalModified[i] = Convert.ToInt32(WSim.GPCD_raw[i]);
                            // originalModified[i] = Convert.ToInt32(WSim.Use_GPCD[i]);
                            minGPCD = WSim.Provider_GPCD_Min.getvalues().Values[i];
                            final[i] = Math.Max(minGPCD, Convert.ToInt32(originalModified[i]));
                            New_slope[i] = Convert.ToInt32(WSim.API_defaultSlope_GPCD[i]);
                        }
                        New_Int.Values = final;
                        WSim.Use_GPCD.setvalues(New_Int);
                        WSim.PCT_alter_GPCD.setvalues(New_slope);
                    }
                    else
                    {


                        for (int i = 0; i < RAW_GPCD.Values.Length; i++)
                        {
                            if (WSim.API_Reduce_GPCD[i] >= 1)
                            {
                                minGPCD = WSim.Provider_GPCD_Min.getvalues().Values[i];
                                originalModified[i] = WSim.GPCD[year - 2000, i];
                                final[i] = Math.Max(minGPCD, Convert.ToInt32(originalModified[i]));
                                New_Int[i] = final[i];
                            }
                            else
                            {

                                if (WSim.API_Slope_GPCD[i] < 0)
                                {
                                    increment[i] = ((1 - WSim.API_Reduce_GPCD[i]) / 10) / smoothCurve;
                                    yearPlusplus[i] = yearPlusplus[i] - increment[i];

                                    mod = Math.Max(yearPlusplus[i], WSim.API_Reduce_GPCD[i]);
                                    minGPCD = WSim.Provider_GPCD_Min.getvalues().Values[i];
                                    originalModified[i] = WSim.GPCD_raw[i] * mod;
                                    final[i] = Math.Max(minGPCD, Convert.ToInt32(originalModified[i]));
                                    New_Int[i] = final[i];
                                }
                                else
                                {
                                    increment[i] = ((1 - WSim.API_Reduce_GPCD[i]) / 10) / smoothCurve;
                                    yearPlusplus[i] = yearPlusplus[i] - increment[i];
                                    GPCD_previous[i] = GPCD_raw[i];
                                    mod = Math.Max(yearPlusplus[i], WSim.API_Reduce_GPCD[i]);
                                    minGPCD = WSim.Provider_GPCD_Min.getvalues().Values[i];
                                    //
                                    originalModified[i] = GPCD_raw[i] * mod;
                                    WSim.GPCD[year - 2000, i] = originalModified[i];
                                    WSim.GPCDraw[year - 2000, i] = GPCD_raw[i];
                                    //
                                    // must first be initialized for this code to work (2015 < t)
                                    if (0 < WSim.GPCD[year - 1 - 2000, i])
                                    {
                                        if (WSim.GPCD[year - 1 - 2000, i] <= originalModified[i])
                                        {
                                            // WSim.GPCDraw Must be initialized (2015 < t)
                                            if (0 < WSim.GPCDraw[year - 1 - 2000, i])
                                            {
                                                if (WSim.GPCDraw[year - 2 - 2000, i] < GPCD_raw[i] - 1)
                                                {
                                                    newModified[i] = 0;
                                                }
                                                else
                                                {
                                                    newModified[i] = originalModified[i] * 0.999;
                                                    // newModified[i]= (WSim.GPCD[year - 1 - 2000, i]*0.999;
                                                    originalModified[i] = newModified[i];
                                                    WSim.GPCD[year - 2000, i] = originalModified[i];
                                                }
                                            }
                                        }
                                    }
                                    //
                                    final[i] = Math.Max(minGPCD, Convert.ToInt32(originalModified[i]));
                                    New_Int[i] = final[i];


                                    // Linear model
                                    // -------------------------------------------
                                    //yr = Convert.ToDouble(year);
                                    //slope_ =   WSim.API_Slope_GPCD[i] ;
                                    //originalModified[i] = (slope_ * yr + WSim.API_Intercept_GPCD[i]) * WSim.API_Reduce_GPCD[i];
                                    //final[i] = Math.Max(minGPCD, Convert.ToInt32(originalModified[i]));
                                    //New_Int[i] = final[i];
                                    //                          
                                }

                                New_slope[i] = Convert.ToInt32(WSim.API_defaultSlope_GPCD[i]);

                            }

                        }
                        New_Int.Values = final;
                        WSim.Use_GPCD.setvalues(New_Int);
                        WSim.PCT_alter_GPCD.setvalues(New_slope)    ;
                        //

                    }
                } // base year block
                else
                {
                    for (int i = 0; i < RAW_GPCD.Values.Length; i++)
                    {
                        originalModified[i] = Convert.ToInt32(WSim.GPCD_raw[i]);
                       // originalModified[i] = Convert.ToInt32(WSim.Use_GPCD[i]);
                        minGPCD = WSim.Provider_GPCD_Min.getvalues().Values[i];
                        final[i] = Math.Max(minGPCD, Convert.ToInt32(originalModified[i]));
                        New_slope[i] = Convert.ToInt32(WSim.API_defaultSlope_GPCD[i]);
                    }
                    New_Int.Values = final;
                    WSim.Use_GPCD.setvalues(New_Int);
                    WSim.PCT_alter_GPCD.setvalues(New_slope);
                }

            }
     
 

        }

        // temp in Process_AlterGPCD.cs
        // 09.11.15 das
        public double Difference = 0;
       
        public double LocalSlope = 0;
        public int AlterGPCD_Difference
        { get { return Convert.ToInt32(Difference); } }

        public int AlterGPCD_Slope
        { get { return Convert.ToInt32(Slope); } }
        ///-------------------------------------------------------------------------------------------------
        /// <summary> Method that is called before after each annual run. </summary>
        /// <param name="year"> The year just run. </param>
        /// <param name="WSim"> The WaterSimManager that is making call. </param>
        ///
        /// <returns> true if it succeeds, false if it fails. Error should be placed in FErrorMessage.
        ///     </returns>
        ///-------------------------------------------------------------------------------------------------

        public override bool PostProcess(int year, WaterSimManagerClass WSim)
        {
           
            Difference = Diff;
            Slope = LocalSlope;
            
             return base.PostProcess(year, WSim);
        }
    }
#endregion
          
}
