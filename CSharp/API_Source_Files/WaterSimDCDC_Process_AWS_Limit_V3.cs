using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Text;
using System.Windows.Forms;
using WaterSimDCDC;
using WaterSimDCDC.Data;
using WaterSimDCDC.Documentation;


namespace WaterSimDCDC.Processes
{
    /// <summary>   Form for viewing the aws feedback process. </summary>
    #region
    public partial class AWSFeedbackProcessForm : Form
    {
        const int MINTRIGGER = 5;
        const int MAXTRIGGER = 30;
        int FTrigger = 10;
        internal string FProcessName = "";
        public AWSFeedbackProcessForm()
        {
            InitializeComponent();
            AssuredTriggertextBox.Text = FTrigger.ToString();
        }
        private bool TestTrigger(ref int value)
        {
            bool testresult = false;
            string textmax = AssuredTriggertextBox.Text.Trim();
            try
            {
                int Test = Convert.ToInt32(textmax);
                if ((Test < MINTRIGGER) || (Test > MAXTRIGGER))
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
            if (!TestTrigger(ref dummy))
            {
                ExitMessage += "Deficit trigger must be in range " + MINTRIGGER.ToString() + " to " + MAXTRIGGER.ToString() + ".  ";
                AssuredTriggertextBox.Text = FTrigger.ToString();
                OKtoClose = false;
            }
            if (OKtoClose)
            {
                this.DialogResult = System.Windows.Forms.DialogResult.OK;
                this.Close();
            }
            else
                MessageBox.Show(ExitMessage);

        }

        private void textBox_ProcessName_TextChanged(object sender, EventArgs e)
        {
            FProcessName = textBox_ProcessName.Text.Trim();
        }

        public string ProcessName
        {
            get { return FProcessName; }
            set { textBox_ProcessName.Text = value; } // Should set FProcessname when TextChange event triggered
        }

        public int YearsForAWSTrigger
        {
            get { return FTrigger; }
            set
            {
                int dummy = 0;
                AssuredTriggertextBox.Text = value.ToString();
                if (TestTrigger(ref dummy))
                    FTrigger = value;
                else
                    AssuredTriggertextBox.Text = FTrigger.ToString();
            }
        }

    }
    #endregion

    //***********************************************************************
    //
    //     AWS Limit Feedback Process
    //    Limit growth when GW uses does not meet 100 year assured supply rule
    //
    // **********************************************************************
    #region
    public class AWSLimitFeedbackProcess : WaterSimDCDC.AnnualFeedbackProcess
        {
            ProviderIntArray BeingManaged = new ProviderIntArray(0);  // 0 means not managed
            ProviderIntArray ConsecutiveUnassuredYears = new ProviderIntArray(0);

            ProviderIntArray FAddOnPop = new ProviderIntArray(0);
            ProviderIntArray FAddOffPop = new ProviderIntArray(0);

            WaterSimManager FWSim;
            AnnualFeedbackProcess FAFPTracker; // for GW tracking
            int FMaxUnassuredYears = 10;
            const int NOGROWTH = 0;

            ///-------------------------------------------------------------------------------------------------
            /// <summary>   Constructor. </summary>
            /// <param name="aName">    Name of Process. </param>
            ///-------------------------------------------------------------------------------------------------

            public AWSLimitFeedbackProcess(string aName)
                : base(aName)
            {
                EvokeDialogAndFetchValues();
            }

            ///-------------------------------------------------------------------------------------------------
            /// <summary>   Default constructor. </summary>
            ///-------------------------------------------------------------------------------------------------

            public AWSLimitFeedbackProcess()
                : base("AWS Limit Feedback")
            {
                EvokeDialogAndFetchValues();
            }

            ///-------------------------------------------------------------------------------------------------
            /// <summary> Constructor. </summary>
            /// <param name="aName"> Name of Process. </param>
            /// <param name="WSim">  The WatewrSimManager who will register process</param>
            ///-------------------------------------------------------------------------------------------------

            public AWSLimitFeedbackProcess(string aName, WaterSimManager WSim)
                : base(aName)
            {
                EvokeDialogAndFetchValues();
                SetUpProcessAndParameters(WSim);


            }

            ///-------------------------------------------------------------------------------------------------
            /// <summary> Constructor. </summary>
            ///<remarks> Evokes Form to set initial values.</remarks>
            /// <param name="WSim"> The WatewrSimManager who will register process. </param>
            ///-------------------------------------------------------------------------------------------------

            public AWSLimitFeedbackProcess(WaterSimManager WSim)
                : base("AWS Limit Feedback")
            {
                EvokeDialogAndFetchValues();
                SetUpProcessAndParameters(WSim);
            }

            ///-------------------------------------------------------------------------------------------------
            /// <summary>   Constructor. </summary>
            ///<remarks> Does not evoke Form to set initial values, sets them to default values.</remarks>
            /// <param name="WSim">     The WatewrSimManager who will register process. </param>
            /// <param name="Quiet">    true to quiet. </param>
            ///-------------------------------------------------------------------------------------------------

            public AWSLimitFeedbackProcess(WaterSimManager WSim, bool Quiet)
                : base("AWS Limit Feedback")
            {
                if (Quiet)
                {
                    Fname = "DEFAULT AWS LIMIT PROCESS";
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
                AWSFeedbackProcessForm FPF = new AWSFeedbackProcessForm();
                if (Fname != "") FPF.ProcessName = Fname;
                if (FPF.ShowDialog() == DialogResult.OK)
                {
                    Fname = FPF.ProcessName;
                    // ===================================
                    //Get data from form and set process values here
                    // 
                    // 
                    // ====================================
                    FMaxUnassuredYears = FPF.YearsForAWSTrigger;

                }
            }

            ///-------------------------------------------------------------------------------------------------
            /// <summary> Documentation. </summary>
            /// <returns> Documentation in a DocTreeNode. </returns>
            ///-------------------------------------------------------------------------------------------------

            public override DocTreeNode Documentation()
            {
                DocTreeNode node = base.Documentation();
                node.AddChildField("FMaxUnassuredYears", FMaxUnassuredYears.ToString());
                return node;
            }

            //============================================
            internal void SetUpProcessAndParameters(WaterSimManager WSim)
            {
                ModelParameterBaseClass MP = WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epYearsNotAssured);
                FWSim = WSim;
                if (MP == null)
                {
                    FAFPTracker = new TrackAvailableGroundwater("Tracking Support for AWS Limit", WSim);
                    WSim.ProcessManager.AddProcess(FAFPTracker);
                }
                WSim.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epYearsOfNonAWSTrigger, "Trigger for NonAWS Years", "NONAWSTRG", modelParamtype.mptInputBase, rangeChecktype.rctNoRangeCheck, 0, 0, geti_Max_Years_NonAWS_Trigger, null, seti_Max_Years_NonAWS_Trigger, null, null, null, null));
                WSim.ProcessManager.ProcessManagerChangeEvent += OnProcessManagerProcessEventHandler;
            }

            // OK need to have an event handler to precent anyone from delete the tracking process while this process is still active.
            internal void OnProcessManagerProcessEventHandler(object sender, ProcessManagerEventArgs e)
            {
                AnnualFeedbackProcess AFP = e.TheAnnualFeedbackProcess;
                e.TheResult = true;
                if (e.TheProcessEventType == ProcessManagerEventType.peDeleteProcess)
                {
                    		
                    if (AFP.Name == "TrackAvailableGroundwater"	)
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
                    FWSim.ParamManager.DeleteParameter(eModelParam.epYearsOfNonAWSTrigger);
                    FWSim.ProcessManager.Delete(FAFPTracker);
                    FWSim.ProcessManager.ProcessManagerChangeEvent -= OnProcessManagerProcessEventHandler;
                }
              }

            ///-------------------------------------------------------------------------------------------------
            /// <summary> Builds the description strings. </summary>
            ///-------------------------------------------------------------------------------------------------

            protected override void BuildDescStrings()
            {
                FProcessDescription = "Limits growth based on AWS GW Rules";
                FProcessLongDescription = "Limits growth when rates of groundwater pumping can no longer assure adequate water for 100 years";
                FProcessCode = "AWS_YR=" + FMaxUnassuredYears.ToString();
            }

            ///-------------------------------------------------------------------------------------------------
            /// <summary> Gets the class description. </summary>
            /// <returns> Class Description. </returns>
            ///-------------------------------------------------------------------------------------------------

            new static public string ClassDescription()
            {
                return "Limits Growth Based on AWS Rules";
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


            public override bool ProcessStarted(int year, WaterSimManagerClass WSim)
            {
                // Initialize these add arrays
                for (int i = 0; i < FAddOnPop.Length; i++) FAddOnPop[i] = 0;
                for (int i = 0; i < FAddOffPop.Length; i++) FAddOffPop[i] = 0;

                return base.ProcessStarted(year, WSim);
            }


            //============================================
            private bool TestUnassured(int year, WaterSimManager WSim)
            {
                return ((year > WSim.Simulation_Start_Year) && (year < (WSim.Simulation_Start_Year + 100)));
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
                ProviderIntArray PulledPopOn = WSim.Projected_OnProject_Pop.getvalues();
                ProviderIntArray PulledPopOff = WSim.Projected_Other_Pop.getvalues();
                ProviderIntArray NewPopOn = new ProviderIntArray(0);
                ProviderIntArray NewPopOff = new ProviderIntArray(0);

                bool TempLock = WSim.isLocked();

                WSim.UnLockSimulation();


                //bool ManagingPop = false;
                // for 2000 to 2010 just grab the projections
                if (year < 2011)
                {
                    for (int i = 0; i < NewPopOn.Length; i++)
                    {
                        NewPopOn[i] = PulledPopOn[i];
                    }

                    for (int i = 0; i < NewPopOff.Length; i++)
                    {
                        NewPopOff[i] = PulledPopOff[i];
                    }

                }//WSim.Simulation_Start_Year + 1))
                else
                {
                    ProviderIntArray LastYearOnPop = WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epOnProjectPopulation).ProviderProperty.getvalues();
                    ProviderIntArray LastYearOffPop = WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epOtherPopulation).ProviderProperty.getvalues();
                    ProviderIntArray YearForZero = WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epYearGWGoesZero).ProviderProperty.getvalues();
                    ProviderIntArray IsTriggered = new ProviderIntArray(0);
                    // Check which are in unassured year range
                    for (int i = 0; i < YearForZero.Length; i++)
                        if (TestUnassured(YearForZero[i], (WSim as WaterSimManager)))
                        {
                            ConsecutiveUnassuredYears[i]++;
                            if (ConsecutiveUnassuredYears[i] > FMaxUnassuredYears)
                            {
                                IsTriggered[i] = 1;
                                // Once triggered always being managed
                                BeingManaged[i] = 1;
                            }
                        }
                        else
                        {
                            ConsecutiveUnassuredYears[i] = 0;
                        }

                    PopulationClass.LimitAndReallocateGrowth(WSim, year, BeingManaged, IsTriggered, FMaxUnassuredYears, ref NewPopOn, ref NewPopOff);
   
                }
                // Ok Set PopOverride Values
                for (int i = 0; i < NewPopOn.Length; i++)
                {
                    WSim.Population_Override_On[i] = NewPopOn[i];
                }
                for (int i = 0; i < NewPopOff.Length; i++)
                {
                    WSim.Population_Override_Other[i] = NewPopOff[i];
                }
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

            ///-------------------------------------------------------------------------------------------------
            /// <summary> Method called when Sumulation is stopped. </summary>
            /// <param name="WSim"> The simulation. </param>
            /// <returns> true if it succeeds, false if it fails. </returns>
            ///-------------------------------------------------------------------------------------------------

            public override bool ProcessStop(WaterSimManagerClass WSimClass)
            {

                WaterSimManager WSim = (WSimClass as WaterSimManager);

                // Reset Pop Overide
                ProviderIntArray Reset = new ProviderIntArray(-1);

                WSim.Population_Override_On.setvalues(Reset);
                WSim.Population_Override_Other.setvalues(Reset);

                return base.ProcessStop(WSim);
            }

            //=========================================================
            // Unsustainble Years Trigger
            //---------------------------------------     

            public int Max_Years_NonAWS_Trigger

            { 
                get { return FMaxUnassuredYears; }
                set { FMaxUnassuredYears = value; }
             }
              
             internal int geti_Max_Years_NonAWS_Trigger() { return Max_Years_NonAWS_Trigger; }
             internal void seti_Max_Years_NonAWS_Trigger(int value) { Max_Years_NonAWS_Trigger = value; }
        }

    #endregion

       
}
