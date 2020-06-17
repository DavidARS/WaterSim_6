using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.Drawing;
using WaterSimDCDC;
using WaterSimDCDC.Data;
using WaterSimDCDC.Documentation;


namespace WaterSimDCDC.Processes
{
    /// <summary>   Form for viewing the alter growth feedback process. </summary>
    #region
    public partial class AlterGrowthFeedbackProcessForm : Form
    {
        const int DEFAULTMINDEFICIT = 0;
        const int DEFAULTMAXDEFICIT = 100;
        int FMaxDeficit = 15;
        internal string FProcessName = "";
        public AlterGrowthFeedbackProcessForm()
        {
            InitializeComponent();
            textBoxMaxDeficit.Text = FMaxDeficit.ToString();
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
                textBoxMaxDeficit.Text = FMaxDeficit.ToString();
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
            string temp = textBox_ProcessName.Text.Trim();
            if (temp != "")
            {
                FProcessName = temp;
                StatusLabel.Text = "";
            }
            else
                StatusLabel.Text = "Process name can not be blank.";
        }

        public string ProcessName
        {
            get {  return FProcessName; }
            set { textBox_ProcessName.Text = value; } // Should set FProcessname when TextChange event triggered
        }

        public int MaxDeficit
        {
            get { return FMaxDeficit; }
            set { textBoxMaxDeficit.Text = value.ToString(); }  // should evoke text change 
        }
 
        private void textBoxMaxDeficit_TextChanged(object sender, EventArgs e)
        {
            int value = 0;
            if (!TestMaxDeficit(ref value))
                StatusLabel.Text = "Deficit trigger must be in range " + DEFAULTMINDEFICIT.ToString() + " to " + DEFAULTMAXDEFICIT.ToString() + ".";
            else
            {
               FMaxDeficit = value;
               StatusLabel.Text = "";
            }
        }


        private void AlterGrowthFeedbackProcessForm_Shown(object sender, EventArgs e)
        {
        }
    }
    #endregion

    //======================================================
    // AlterGrowthFeedbackProcess
    // Limits growth of Deficit occurs
    // =============================
    /// <summary>   Alter growth feedback process. </summary>
    #region
    public class AlterGrowthFeedbackProcess : WaterSimDCDC.AnnualFeedbackProcess
    {
        ProviderIntArray BeingManaged = new ProviderIntArray(0);  // 0 means not managed
  
        int FMaxDefict = 15;
        const int NOGROWTH = 0;
        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Constructor. </summary>
        /// <param name="aName">    Name of Process. </param>
        ///-------------------------------------------------------------------------------------------------

        public AlterGrowthFeedbackProcess(string aName)
            : base(aName)
        {
            EvokeDialogAndFetchValues(null);
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Default constructor. </summary>
        ///-------------------------------------------------------------------------------------------------

        public AlterGrowthFeedbackProcess()  : base("AlterGrowthOnDeficit")
        {
            EvokeDialogAndFetchValues(null);
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Constructor. </summary>
        /// <remarks>Form is evoked to set initial values.</remarks>
        /// <param name="aName"> Name of Process. </param>
        /// <param name="WSim">  The WatewrSimManager who will register process</param>
        ///-------------------------------------------------------------------------------------------------

        public AlterGrowthFeedbackProcess(string aName, WaterSimManager WSim)  : base(aName)
        {
            EvokeDialogAndFetchValues(WSim);
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Constructor. </summary>
        /// <remarks>Form is evoked to set initial values.</remarks>
        /// <param name="WSim"> The WatewrSimManager who will register process. </param>
        ///-------------------------------------------------------------------------------------------------

        public AlterGrowthFeedbackProcess(WaterSimManager WSim)
            : base("AlterGrowthOnDeficit")
        {
            EvokeDialogAndFetchValues(WSim);
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Constructor. </summary>
        /// <remarks>If Quiet then Form is not evoked to set initial values, defaults used. Otherwise form is used.</remarks>
        /// <param name="WSim">     The WatewrSimManager who will register process. </param>
        /// <param name="Quiet">    true to quiet. </param>
        ///-------------------------------------------------------------------------------------------------

        public AlterGrowthFeedbackProcess(WaterSimManager WSim, bool Quiet)
            : base("AlterGrowthOnDeficit")
        {
            if (Quiet)
            {
                Fname = "DEFAULY ALTER GROWTH On DEFICIT PROCESS";
            }
            else
            {
                EvokeDialogAndFetchValues(WSim);
            }
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Method called when Sumulation is stopped. </summary>
        /// <param name="WSim"> The simulation. </param>
        /// <returns> true if it succeeds, false if it fails. </returns>
        ///-------------------------------------------------------------------------------------------------

        public override bool  ProcessStop(WaterSimManagerClass WSimClass)
        {
            WaterSimManager WSim = (WSimClass as WaterSimManager);

            // Reset Pop Overide
            ProviderIntArray Reset = new ProviderIntArray(-1);

             WSim.Population_Override_On.setvalues(Reset);
             WSim.Population_Override_Other.setvalues(Reset);
    
            return base.ProcessStop(WSim);
}
        ///-------------------------------------------------------------------------------------------------
        /// <summary> Evokes dialog and fetches values. </summary>
        ///-------------------------------------------------------------------------------------------------
        protected void EvokeDialogAndFetchValues(WaterSimManager WSim)
        {
            AlterGrowthFeedbackProcessForm FPF;
            FPF = new AlterGrowthFeedbackProcessForm();
            
            if (Fname!="") FPF.ProcessName = Fname;
            if (FPF.ShowDialog() == DialogResult.OK)
            {
                Fname = FPF.ProcessName;
                FMaxDefict = FPF.MaxDeficit;
                // ===================================
                //Get data from form and set process values here
                //if (WSim != null)
                //{
                //    LoadPopData(WSim.DataDirectory);              // 
                //}
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
            node.AddChildField("FMaxDefict", FMaxDefict.ToString());
            return node;
        }
        ///-------------------------------------------------------------------------------------------------
        /// <summary> Builds the description strings. </summary>
        ///-------------------------------------------------------------------------------------------------

        protected override void BuildDescStrings()
        {
            FProcessDescription = "Limits growth if % Deficit > "+FMaxDefict.ToString();
            FProcessLongDescription = "Freezes growth when Deficit is over  "+FMaxDefict.ToString()+"% and reallocates providers growth to other providers not in deficit";
            FProcessCode = "LIMIT_DEF_" + FMaxDefict.ToString();
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Gets the class description. </summary>
        /// <returns> Class Description. </returns>
        ///-------------------------------------------------------------------------------------------------

        new static public string ClassDescription()
        {
            return "Limits and reallocated provider growth based on MaxDeficit set with Dialog";
        }

        //internal void LoadPopData(string Pathname)
        //{
        //    PopData = new PopulationClass(Pathname, 2000);

        //}
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
           // for (int i = 0; i < FAddOnPop.Length; i++) FAddOnPop[i] = 0;
           // for (int i = 0; i < FAddOffPop.Length; i++) FAddOffPop[i] = 0;

            return base.ProcessStarted(year, WSim);
        }

        //----------------------------------------------------
        int PercentDif(int newvalue, int oldvalue)
        {
            if (oldvalue != 0)
            {
                
                double pct = Convert.ToDouble(newvalue - oldvalue) / Convert.ToDouble(oldvalue);
                int diff = Convert.ToInt32(pct *100);
                return diff;
            }
                else
                return 0;
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
   
                ProviderIntArray FPCT_Deficit = WSim.Percent_Deficit.getvalues();
                ProviderIntArray InDeficit = new ProviderIntArray(0);

    
                // Check which are in deficit this year, inc defricit count, must be out of deficit for as long as in deficit to clear
                for (int i = 0; i < FPCT_Deficit.Length; i++)
                    if (FPCT_Deficit[i] > FMaxDefict)
                    {
                        InDeficit[i]++;
                        BeingManaged[i] = 1;
                        
                    }
                    else
                    {
                        if (InDeficit[i] > 0)
                        {
                            InDeficit[i]--;
                        }
                    }
               // WaterSimManager WSim, int year, ProviderIntArray BeingManaged, ProviderIntArray TriggerCnt, int TriggerLimit, ref ProviderIntArray NewPopOn, ref ProviderIntArray NewPopOff
                PopulationClass.LimitAndReallocateGrowth(WSim, year, BeingManaged, InDeficit, FMaxDefict, ref NewPopOn, ref NewPopOff);
  
            } // past 2010
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
    }
    #endregion
          
}
