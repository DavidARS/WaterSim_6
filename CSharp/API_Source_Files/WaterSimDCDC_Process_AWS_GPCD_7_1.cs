using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;

using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Drawing;
using WaterSimDCDC;
using WaterSimDCDC.Documentation;

namespace WaterSimDCDC.Processes
{
    /// <summary>   Form for viewing the alter gpcdaws feedback process. </summary>
    #region Form
    public partial class AlterGPCDAWSFeedbackProcessForm : Form
    {
        const int DEFAULTMINAWS = 1;
        const int DEFAULTMAXAWS = 100;
        const int DEFAULTMINGPCD = 30;
        const int DEFAULTMAXGPCD = 1000;
        const int DEFAULTMAXDECLINE = 50;
        const int DEFAULTMINDECLINE = 1;

        internal string FProcessName = "";
        internal int FAWSYearTrigger = 10;
        internal int FMinGPCD = 30;
        internal int FAnnualDecline = 10;

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Default constructor. </summary>
        ///-------------------------------------------------------------------------------------------------

        public AlterGPCDAWSFeedbackProcessForm()
        {
            InitializeComponent();
            textBox_ProcessName.Text = FProcessName;
            textBoxAWSYears.Text = FAWSYearTrigger.ToString();
            textBoxMinGPCD.Text = FMinGPCD.ToString();
            textBoxPercentDecline.Text = FAnnualDecline.ToString();
        }
        //-------------------------------------
        private bool TestPercentDecline(ref int value)
        {
            bool testresult = false;
            string textmax = textBoxPercentDecline.Text.Trim();
            try
            {
                int Test = Convert.ToInt32(textmax);
                if ((Test < DEFAULTMINDECLINE) || (Test > DEFAULTMAXDECLINE))
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
        //=======================================
        private bool TestMaxAWS(ref int value)
        {
            bool testresult = false;
            string textmax = textBoxAWSYears.Text.Trim();
            try
            {
                int Test = Convert.ToInt32(textmax);
                if ((Test < DEFAULTMINAWS) || (Test > DEFAULTMAXAWS))
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
        //-------------------------------------

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
        //-------------------------------------
        
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
            if (!TestMaxAWS(ref dummy))
                {
                    ExitMessage += "AWS trigger must be in range " + DEFAULTMINAWS.ToString() + " to " + DEFAULTMAXAWS.ToString()+".  ";
                    textBoxAWSYears.Text = FAWSYearTrigger.ToString();
                    OKtoClose = false;
                }
            if (!TestMinGPCD(ref dummy))
                    {
                        ExitMessage += "Minimum GPCD must be in range " + DEFAULTMINGPCD.ToString() + " to " + DEFAULTMAXGPCD.ToString()+".  ";
                        textBoxMinGPCD.Text = FMinGPCD.ToString();
                        OKtoClose = false;
                    }
            if (!TestPercentDecline(ref dummy))
                {
                    ExitMessage += "Annual Percent Decline must be in range " + DEFAULTMINDECLINE.ToString() + " to " + DEFAULTMAXDECLINE.ToString()+".  ";
                    textBoxPercentDecline.Text = FAWSYearTrigger.ToString();
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
        //-------------------------------------

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

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Gets or sets the name of the process. </summary>
        ///
        /// <value> The name of the process. </value>
        ///-------------------------------------------------------------------------------------------------

        public string ProcessName
        {
            get {  return FProcessName; }
            set { textBox_ProcessName.Text = value; } // Should set FProcessname when TextChange event triggered
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Gets or sets the maximum the ws trigger. </summary>
        ///
        /// <value> The maximum the ws trigger. </value>
        ///-------------------------------------------------------------------------------------------------

        public int MaxAWSTrigger
        {
            get { return FAWSYearTrigger; }
            set { textBoxAWSYears.Text = value.ToString(); }  // should evoke text change 
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Gets or sets the minimum gpcd. </summary>
        ///
        /// <value> The minimum gpcd. </value>
        ///-------------------------------------------------------------------------------------------------

        public int MinGPCD
        {
            get { return FMinGPCD; }
            set { textBoxMinGPCD.Text = value.ToString(); }  // should evoke text change 
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Gets or sets the annual decline. </summary>
        ///
        /// <value> The annual decline. </value>
        ///-------------------------------------------------------------------------------------------------

        public int AnnualDecline
        {
            get { return FAnnualDecline; }
            set { textBoxPercentDecline.Text = value.ToString(); }  // should evoke text change 
        }

        //-------------------------------------

        private void textBoxMaxDeficit_TextChanged(object sender, EventArgs e)
        {
         
            int value = 0;
            if (! TestMaxAWS(ref value))
                    StatusLabel.Text = "AWS Year Trigger must be in range of "+DEFAULTMINAWS.ToString()+" to "+ DEFAULTMAXAWS.ToString();
            else
                {
                    FAWSYearTrigger = value;
                    StatusLabel.Text = "";
                }
        }
        //-------------------------------------

        private void textBoxMinGPCD_TextChanged(object sender, EventArgs e)
        {
            int value = 0;
            if (! TestMinGPCD(ref value))
                    StatusLabel.Text = "AWS Rule YEars must be in range of "+DEFAULTMINAWS.ToString()+" to "+ DEFAULTMAXAWS.ToString();
            else
            {
                FMinGPCD = value;
                StatusLabel.Text = "";
            }
        }
        //-------------------------------------

        private void textBoxPercentDecline_TextChanged(object sender, EventArgs e)
        {
            {
                int value = 0;
                if (!TestPercentDecline(ref value))
                    StatusLabel.Text = "Annual Percent Decline must be in range of " + DEFAULTMINDECLINE.ToString() + " to " + DEFAULTMAXDECLINE.ToString();
                else
                {
                    FAnnualDecline = value;
                    StatusLabel.Text = "";
                }
            }

        }
    }

    #endregion

    //*********************************************************************
    //   
    //   AlterGPCDAWSFeedbackProcess
    //    AnnualFeedbackProcess   
    //         
    //
    // *********************************************************

    #region AlterGPCDAWSFeedbackProcessClass

    public class AlterGPCDAWSFeedbackProcess : WaterSimDCDC.AnnualFeedbackProcess
    {
        const int MINGPCDALLOWED = 70;
        const int MAXAWS = 30;
        const int MAXGPCDDECLINE = 10;
        int FMinGPCD = 0;
        int FMaxAWS = 0;
        int FGPCDDeclineFactor = 0;
        double AdjustFactor_Double = 0.0;
        ProviderDoubleArray GPCD_old = new ProviderDoubleArray(0);
        double YrCount = 0.0;
         WaterSimManager FWSim;
         AnnualFeedbackProcess FAFPTracker; // for GW tracking
         int FAnnualPercentDecline = 10;

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Constructor. </summary>
        /// <remarks> This constructor will evoke a dialog that allows interactive setting of process variables but does not 
        ///           create any Model Parameters or attach add the process to a WaterSimManager.ProcessManager.</remarks>
         /// <see cref="AlterGPCDAWSFeedbackProcessForm"/>
        /// <param name="aName">    Name of Process. </param>
        ///-------------------------------------------------------------------------------------------------

        public AlterGPCDAWSFeedbackProcess(string aName)
            : base(aName)
        {
            EvokeDialogAndFetchValues();
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Default constructor. </summary>
        ///-------------------------------------------------------------------------------------------------

        public AlterGPCDAWSFeedbackProcess() : base("Alter GPCD Based on AWS Rule")
        {
            EvokeDialogAndFetchValues();
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Constructor. </summary>
        /// <remarks> A form will ber displayed to set initial values, and then the process is added to the WSim.ProcessManager</remarks>
        /// <see cref="AlterGPCDAWSFeedbackProcessForm"/>
        /// <param name="aName"> Name of Process. </param>
        /// <param name="WSim">  The WatewrSimManager who will register process</param>
        ///-------------------------------------------------------------------------------------------------

        public AlterGPCDAWSFeedbackProcess(string aName, WaterSimManager WSim)  : base(aName)
        {
            EvokeDialogAndFetchValues();
            SetUpProcessAndParameters(WSim);
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Constructor. </summary>
        /// <remarks> A form will ber displayed to set initial values, and then the process is added to the WSim.ProcessManager</remarks>
        /// <see cref="AlterGPCDAWSFeedbackProcessForm"/>
        /// <param name="WSim"> The WatewrSimManager who will register process. </param>
        ///-------------------------------------------------------------------------------------------------

        public AlterGPCDAWSFeedbackProcess(WaterSimManager WSim)
            : base("Alter GPCD Based on AWS Rule")
        {
            EvokeDialogAndFetchValues();
            SetUpProcessAndParameters(WSim);
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Constructor. </summary>
        ///<remarks>If Quiet does not call Form to set initial values, uses defaults, else Form is opened</remarks>
        /// <param name="WSim">     The WatewrSimManager who will register process. </param>
        /// <param name="Quiet">    true to quiet. </param>
        ///-------------------------------------------------------------------------------------------------

        public AlterGPCDAWSFeedbackProcess(WaterSimManager WSim, bool Quiet)
            : base("Alter GPCD Based on AWS Rule")
        {
            if (Quiet)
            {
                Fname = "DEFUALT AWS ALTER GPCD PROCESS";
                FMaxAWS = MAXAWS;
                FMinGPCD = MINGPCDALLOWED;
                FAnnualPercentDecline = MAXGPCDDECLINE;
            }
            else
            {
                EvokeDialogAndFetchValues();
            }
            SetUpProcessAndParameters(WSim);
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Evokes dialog and fetches values. </summary>
        ///-------------------------------------------------------------------------------------------------

        protected void EvokeDialogAndFetchValues()
        {
            AlterGPCDAWSFeedbackProcessForm FPF = new AlterGPCDAWSFeedbackProcessForm();
            if (Fname!="") FPF.ProcessName = Fname;
            if (FPF.ShowDialog() == DialogResult.OK)
            {
                Fname = FPF.ProcessName;
                // ===================================
                //Get data from form and set process values here
                // 
                FMaxAWS = FPF.MaxAWSTrigger;
                FMinGPCD = FPF.MinGPCD;
                FAnnualPercentDecline = FPF.FAnnualDecline;
                // 
                // ====================================
            }
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Documentation. </summary>
        /// <returns> Documentation in a DocTreeNode. </returns>
        ///-------------------------------------------------------------------------------------------------

        public override DocTreeNode Documentation()
        {
            DocTreeNode node = base.Documentation();
            node.AddChildField("FMaxAWS", FMaxAWS.ToString());
            node.AddChildField("FMinGPCD", FMinGPCD.ToString());
            node.AddChildField("FAnnualPercentDecline", FMinGPCD.ToString());
            return node;
        }

        internal void SetUpProcessAndParameters(WaterSimManager WSim)
        {
            ModelParameterBaseClass MP = WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epYearsNotAssured);
            FWSim = WSim;
            if (MP == null)
            {
                FAFPTracker = new TrackAvailableGroundwater("Tracking Support for AWS Limit", WSim);
                WSim.ProcessManager.AddProcess(FAFPTracker);
            }
            WSim.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epYearsOfNonAWSGPCDTrigger, "Trigger for GPCD NonAWS Years", "AWSGPCD", modelParamtype.mptInputBase, rangeChecktype.rctNoRangeCheck, 0, 0, geti_Max_Years_NonAWS_Trigger, null, seti_Max_Years_NonAWS_Trigger, null, null, null, null));
            WSim.ProcessManager.ProcessManagerChangeEvent += OnProcessManagerProcessEventHandler;
        }

        // OK need to have an event handler to precent anyone from delete the tracking process while this process is still active.
        internal void OnProcessManagerProcessEventHandler(object sender, ProcessManagerEventArgs e)
        {
            AnnualFeedbackProcess AFP = e.TheAnnualFeedbackProcess;
            e.TheResult = true;
            if (e.TheProcessEventType == ProcessManagerEventType.peDeleteProcess)
            {
                if (AFP.Name == "TrackAvailableGroundwater")
 
                    e.TheResult = false;
            }

        }
        ///-------------------------------------------------------------------------------------------------
        /// <summary> Deletes the ModelParameter to avoid calls to a disposed class method
        ///     </summary>
        ///-------------------------------------------------------------------------------------------------

        public override void CleanUp()
        {
            if (FWSim != null)
            {
                FWSim.ParamManager.DeleteParameter(eModelParam.epYearsOfNonAWSGPCDTrigger);
                FWSim.ProcessManager.Delete(FAFPTracker);
                FWSim.ProcessManager.ProcessManagerChangeEvent -= OnProcessManagerProcessEventHandler;
            }
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Builds the description strings. </summary>
        ///-------------------------------------------------------------------------------------------------

        protected override void BuildDescStrings()
        {
            FProcessDescription = "Alters GPCD when GW unsustainable > " + FMaxAWS.ToString() + "Yrs";
            FProcessLongDescription = "Lowers GPCD when a provider's GW pumping does not meet AWS rule for " + FMaxAWS.ToString() + "yrs" +
                "by "+FAnnualPercentDecline.ToString() + "% per year, but no lower than MinGPCD (" + FMinGPCD.ToString() + ").";
            FProcessCode = "GPCD_AWS_"+FMaxAWS.ToString()+"MN"+FMinGPCD.ToString()+"PC"+FAnnualPercentDecline.ToString();
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Gets the class description. </summary>
        /// <returns> Class Description. </returns>
        ///-------------------------------------------------------------------------------------------------

        new static public string ClassDescription()
        {
            return "Alter GPCD based on MaxDeficit trigger and Minimum GPCD set with Dialog.";

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

        public override bool ProcessStarted(int year, WaterSimManagerClass WSimClass)
        {

            WaterSimManager WSim = (WSimClass as WaterSimManager);
            // OK Simulation Starting, Parameters have been set, fetch the ReduceGOCD factor and use it to calculate rate
            YrCount = 1;
            FGPCDDeclineFactor = WSim.ParamManager.Model_Parameter(eModelParam.epPCT_Alter_GPCD).Value;  // 7/29  WSim.ParamManager.BaseModel_ParameterBaseClass(eModelParam.epPCT_Reduce_GPCD).Value;
            double TotalYears = Convert.ToDouble((WSim.Simulation_End_Year - WSim.Simulation_Start_Year) + 1);
            AdjustFactor_Double = (Convert.ToDouble(FGPCDDeclineFactor) / 100) / TotalYears;
            return base.ProcessStarted(year, WSim);
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Method that is called before each annual run. </summary>
        /// <param name="year"> The year about to be run. </param>
        /// <param name="WSim"> The WaterSimManager that is making call. </param>
        /// <returns> true if it succeeds, false if it fails. Error should be placed in FErrorMessage.
        ///     </returns>
        ///-------------------------------------------------------------------------------------------------

        public override bool PreProcess(int year, WaterSimManagerClass WSim)
        {
            // Check if first run
            ProviderIntArray FUSED_GPCD = WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epGPCD_Used).ProviderProperty.getvalues();
             YrCount++;
             // Set oldGOCD to these values.
             for (int i=0;i<FUSED_GPCD.Values.Length;i++)
                {
                    GPCD_old[i] = Convert.ToDouble(FUSED_GPCD[i]);
                }
            //}
            // not first year, do it
            WSim.ParamManager.Model_Parameter(eModelParam.epProvider_Demand_Option).Value = 4;  // 7/29 WSim.ParamManager.BaseModel_ParameterBaseClass(eModelParam.epProvider_Demand_Option).Value = 4;
                // OK Not First year
                // Check if model is locked, if so unlock it.
                bool TempLock = WSim.isLocked();
                if (TempLock) WSim.UnLockSimulation();

                // Get data we need to evaluate AWS
                
                ProviderIntArray AWS_Years = WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epYearsNotAssured).ProviderProperty.getvalues();
                ProviderIntArray FPOP = WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epPopulation_Used).ProviderProperty.getvalues();
                // create and array to set GPCD
                ProviderIntArray FSet_GPCD = new ProviderIntArray(0);
                // Loop through providers and check for AWS_Years FMaxAWS
                bool SetGPCDS = false;
                for (int i = 0; i < AWS_Years.Length; i++)
                {
                    if (AWS_Years[i] > FMaxAWS)
                    {
                        eProvider ep = (eProvider)i;
                        // OK Calculated how much lower GPCD needs to go to accomodate 
                        ////int MyDeficit_AF = FDeficit[i];
                        //double MyDeficit_G = util.ConvertAFtoGallons(MyDeficit_AF);
                        //int MyPCTDeficit = FPCT_Deficit[i];
                        //int MyPop = FPOP[i];
                        //double GPCD_Decline_Double = 0.0;
                        int GPCD_Decline_Int = 0;
                        int MyGPCD = FUSED_GPCD[i];
                        //// if (MyPop > MyDeficit) then it will be 1
                        //GPCD_Decline_Double = util.CalcGPCD(MyDeficit_G, MyPop);
                        //GPCD_Decline_Int = Convert.ToInt32(GPCD_Decline_Double);
                        GPCD_Decline_Int = (MyGPCD * FAnnualPercentDecline) / 100;
                        int New_GPCD = MyGPCD - GPCD_Decline_Int;
                        // Checl if New GPCD not to low
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
                    }
                    else
                    {
                        GPCD_old[i] = AdjustGPCD(GPCD_old[i]);
                        FUSED_GPCD[i] = Convert.ToInt32(GPCD_old[i]); 
                    }
                }
                //
                 // All Done
                 if (SetGPCDS)
                        WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epUse_GPCD).ProviderProperty.setvalues(FUSED_GPCD);
                

                // if model was locked comming in, set lock going out
                if (TempLock) WSim.LockSimulation();
            
             return base.PreProcess(year, WSim);
        }

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
            return base.PostProcess(year, WSim);
        }

        
            //=========================================================
            // Unsustainble Years Trigger
            //---------------------------------------     

            public int Max_Years_NonAWS_Trigger

            { 
                get { return FMaxAWS; }
                set { FMaxAWS = value; }
             }
              
             internal int geti_Max_Years_NonAWS_Trigger() { return Max_Years_NonAWS_Trigger; }
             internal void seti_Max_Years_NonAWS_Trigger(int value) { Max_Years_NonAWS_Trigger = value; }
        }
    #endregion

}

