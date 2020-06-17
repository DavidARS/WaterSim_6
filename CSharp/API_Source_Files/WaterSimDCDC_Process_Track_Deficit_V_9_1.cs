using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.Drawing;
using WaterSimDCDC;
using WaterSimDCDC.Documentation;

namespace WaterSimDCDC.Processes
{
    /// <summary>   Form for viewing the track deficit feedback process. </summary>
    #region
    public partial class TrackDeficitFeedbackProcessForm : Form
    {
        const int DEFAULTMINDEFICIT = 0;
        const int DEFAULTMAXDEFICIT = 100;
        int FMaxDeficit = 15;
        internal string FProcessName = "";
        public TrackDeficitFeedbackProcessForm()
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

    //============================================
    // TrackProviderDeficitsParameter AnnualFeedbackProcess
    //
    // ============================================
    #region
    ///-------------------------------------------------------------------------------------------------
    /// <summary> Track provider deficits parameter. </summary>
    ///
    /// <remarks> Ray, 8/21/2012. </remarks>
    ///-------------------------------------------------------------------------------------------------

    public class TrackProviderDeficitsParameter: AnnualFeedbackProcess// WaterSimDCDC.TrackProviderDeficits
    {
        // Max used to trigger count
        //int FMaxDificit = 5;
        /// <summary>
        /// A provider array with number of years of Deficit for each provider
        /// </summary>
        public ProviderIntArray CountList = new ProviderIntArray(0);
        public ProviderIntArray CountList_2 = new ProviderIntArray(0);
        /// <summary>
        /// a provider array with the total AF of deficit for each provide across all years
        /// </summary>
        public ProviderDoubleArray TotalList = new ProviderDoubleArray(0.0);
        public ProviderDoubleArray TotalList_2 = new ProviderDoubleArray(0.0);
        /// <summary> A provider array how many continuous years in deficit, 0 if not in deficit. </summary>
        public ProviderIntArray ContinuousList = new ProviderIntArray(0);
        public ProviderIntArray ContinuousList_2 = new ProviderIntArray(0);
        /// <summary> A provider array of longest period of continuous deficit </summary>
        public ProviderIntArray LongestContinuous = new ProviderIntArray(0);
        public ProviderIntArray LongestContinuous_2 = new ProviderIntArray(0);

        public const int DEFAULTDEFICITTRIGGERFORTRACKING = 10;
        providerArrayProperty Deficit_Years;
        //
        providerArrayProperty CreditDeficit_Years;

       // used to prevent more than on parameter being added to list
        static int ClassCount = 0;
        static int ClassCount_2 = 0;

        // Need this for disposal
        WaterSimManager FWSim;

        int FMaxDeficit = DEFAULTDEFICITTRIGGERFORTRACKING;

        private int[] get_Deficit_Years()
        {
            ProviderIntArray temp = new ProviderIntArray(0);
            for (int i = 0; i < temp.Length; i++)
                temp.Values[i] = CountList.Values[i];
            return temp.Values;
        }
        private int[] get_CreditDeficit_Years()
        {
            ProviderIntArray temp = new ProviderIntArray(0);
            for (int i = 0; i < temp.Length; i++)
                temp.Values[i] = CountList_2.Values[i];
            return temp.Values;
        }
        int FMaxCreditDeficit = 10;
        public int MaxCreditDeficit
        {
            get { return FMaxCreditDeficit; }
            set { MaxCreditDeficit = value;}  // should evoke text change 
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Default constructor. </summary>
        /// <remarks> Does not add a parameter but does evoke the input dialog</remarks>
        ///-------------------------------------------------------------------------------------------------

        public TrackProviderDeficitsParameter()
            : base("Count Years of Provider Deficits")
        {
           
            EvokeDialogAndFetchValues(null);
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Constructor. </summary>
        /// <param name="aName"> Name of Process. </param>
        /// <param name="WSim">  The WatewrSimManager who will register process</param>
        /// <remarks> Adds tracking parameter and evokes input dialog</remarks>
        ///-------------------------------------------------------------------------------------------------

        public TrackProviderDeficitsParameter(string aName, WaterSimManager WSim)  : base(aName, WSim)
        {
            AddTrackingParameter(WSim);
            EvokeDialogAndFetchValues(WSim);
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Constructor. </summary>
        ///<remarks>Form to set initial values is evoked.</remarks>
        /// <param name="WSim"> The WatewrSimManager who will register process. </param>
        /// <remarks> Adds tracking parameter and evokes input dialog</remarks>
        ///-------------------------------------------------------------------------------------------------

        public TrackProviderDeficitsParameter(WaterSimManager WSim)
            : base("Count Years of Provider Deficits")
        {
            AddTrackingParameter(WSim);
            EvokeDialogAndFetchValues(WSim);
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Constructor. </summary>
        ///<remarks>If Quiet Form to set initial values is not evoked and defualts used, otherwise form is evoked.</remarks>
        /// <param name="WSim">     The WatewrSimManager who will register process. </param>
        /// <param name="Quiet">    true to quiet. </param>
        /// <remarks> Adds tracking parameter </remarks>
        ///-------------------------------------------------------------------------------------------------

        public TrackProviderDeficitsParameter(WaterSimManager WSim, bool Quiet)
            : base("Count Years of Provider Deficits")
        {

            AddTrackingParameter(WSim);
            if (Quiet)
            {
                Fname = "DEFAULT DEFICIT TRACKING PROCESS";
            }
            else
            {
                EvokeDialogAndFetchValues(WSim);
            }
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary>   Adds a tracking model parameters. </summary>
        ///
        /// <param name="WSim"> The WatewrSimManager who will mabage tracking aprameters. </param>
        ///-------------------------------------------------------------------------------------------------

        private void AddTrackingParameter(WaterSimManager WSim)
        {
            Deficit_Years = new providerArrayProperty(WSim.ParamManager, eModelParam.epDeficit_Years, this.get_Deficit_Years, eProviderAggregateMode.agAverage);
            // Added on 08.23.16 das
            CreditDeficit_Years = new providerArrayProperty(WSim.ParamManager, eModelParam.epCreditDeficit_Years, this.get_CreditDeficit_Years, eProviderAggregateMode.agAverage);

            // Only add one parameter  Checking just in case more than one is added, Process manager should not allow more than one but that will
            // not stop someone from constructing two objects and not add to ProcessManager
            if (ClassCount == 0)
            {
                WSim.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epDeficit_Years, "Cumulative Years of Demand Deficit", "CUMDEF", modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck, 0, 0, null, this.get_Deficit_Years, null, null, null, null, Deficit_Years));
                FWSim = WSim;
            }

            ClassCount++;
            if (ClassCount_2 == 0)
            {
                WSim.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epCreditDeficit_Years, "Cumulative Years of Credit Deficit", "CREDDEF", modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck, 0, 0, null, this.get_CreditDeficit_Years, null, null, null, null, Deficit_Years));

                FWSim = WSim;
            }
            ClassCount_2++;
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Evokes dialog and fetches values. </summary>
        ///-------------------------------------------------------------------------------------------------
        protected void EvokeDialogAndFetchValues(WaterSimManager WSim)
        {
            TrackDeficitFeedbackProcessForm TPF = new TrackDeficitFeedbackProcessForm();
            
            if (Fname!="") TPF.ProcessName = Fname;
            if (TPF.ShowDialog() == DialogResult.OK)
            {
                Fname = TPF.ProcessName;
                FMaxDeficit = TPF.MaxDeficit;
            }
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Documentation. </summary>
        /// <returns> Documentation in a DocTreeNode. </returns>
        ///-------------------------------------------------------------------------------------------------

        public override DocTreeNode Documentation()
        {
            DocTreeNode node = base.Documentation();
            node.AddChildField("FMaxDeficit", FMaxDeficit.ToString());
            node.AddChildField("FMaxCreditDeficit", FMaxCreditDeficit.ToString());
            return node;
        }
        ///-------------------------------------------------------------------------------------------------
        /// <summary> Builds the description strings. </summary>
        ///-------------------------------------------------------------------------------------------------

        protected override void BuildDescStrings()
        {
            string temp = "";
            if (FWSim!=null) temp = "Parameter for tracking ";
            else temp = "Running ";
            FProcessDescription = temp+"total years with % deficit  > "+FMaxDeficit.ToString();
            FProcessLongDescription = "For each provider a "+temp+"total of years when percent deficit is over  "+FMaxDeficit.ToString();
            FProcessCode = "CUMDEF" + FMaxDeficit.ToString()+"%";
            //

        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Deletes the ModelParameter to avoid calls to a disposed class method
        ///     </summary>
        ///-------------------------------------------------------------------------------------------------

        public override void CleanUp()
        {
            if (FWSim != null)
                FWSim.ParamManager.DeleteParameter(eModelParam.epDeficit_Years);
                FWSim.ParamManager.DeleteParameter(eModelParam.epCreditDeficit_Years);
            if (ClassCount > 0)
                ClassCount--;
            if (ClassCount_2 > 0)
                ClassCount_2--;
        }

        new static public string ClassDescription()
        {
            return "Tracks Years of Provider Deficit and adds Cumulative Years of Deficit Parameter.";
        }


        //----------------------------------------------------
        /// <summary>
        /// This is the ProcessMethod that keeps track of Deficits by provider
        /// </summary>
        /// <param name="year">Simulation Year (not used)</param>
        /// <param name="WSim">The WaterSimManager object (used to get data)</param>
        /// <returns></returns>
        override public bool PostProcess(int year, WaterSimManagerClass WSimClass)
        {
            WaterSimManager WSim = (WSimClass as WaterSimManager);

            ProviderIntArray Deficits = new ProviderIntArray();
            ProviderIntArray CreditDeficits = new ProviderIntArray();
            // get the deficit data
            Deficits = WSim.Demand_Deficit.getvalues();
            CreditDeficits = WSim.AF_water_CreditDeficits.getvalues();

            foreach (eProvider ep in ProviderClass.providers())
            {
                if (Deficits[ep] > FMaxDeficit)
                {

                    CountList[ep]++;
                    TotalList[ep] += Deficits[ep];
                    ContinuousList[ep]++;
                    if (LongestContinuous[ep] < ContinuousList[ep]) LongestContinuous[ep] = ContinuousList[ep];
                }
                else
                {
                    ContinuousList[ep] = 0;
                }
                //
                if (CreditDeficits[ep] > FMaxCreditDeficit)
                {
                    CountList_2[ep]++;
                    TotalList_2[ep] += CreditDeficits[ep];
                    ContinuousList_2[ep]++;
                    if (LongestContinuous_2[ep] < ContinuousList_2[ep]) LongestContinuous_2[ep] = ContinuousList_2[ep];

                }
            }
            return true;
        }
        //----------------------------------------------------
    }
    #endregion
          
}
