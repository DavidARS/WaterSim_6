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
   
    
    //***********************************************************************
    //
    //     Policy Limit Feedback Process
    //     Invoke Policies when triggered 
    //
    // **********************************************************************
    #region
    public class PolicyLimitFeedbackProcess : WaterSimDCDC.AnnualFeedbackProcess
        {
            ProviderIntArray BeingManaged = new ProviderIntArray(0);  // 0 means not managed
            ProviderIntArray ConsecutiveDeficitYears = new ProviderIntArray(0);

            ProviderIntArray FTriggerGWdeficits = new ProviderIntArray(0);

            WaterSimManager FWSim;
            AnnualFeedbackProcess FAFPTracker; // for GW tracking
            //
            int FmaxDeficitYears = 5;
            const int NOGROWTH = 0;

            ///-------------------------------------------------------------------------------------------------
            /// <summary>   Constructor. </summary>
            /// <param name="aName">    Name of Process. </param>
            ///-------------------------------------------------------------------------------------------------

            public PolicyLimitFeedbackProcess(string aName)
                : base(aName)
            {
                //EvokeDialogAndFetchValues();
            }

            ///-------------------------------------------------------------------------------------------------
            /// <summary>   Default constructor. </summary>
            ///-------------------------------------------------------------------------------------------------

            public PolicyLimitFeedbackProcess()
                : base("Policy Limit Feedback(s)")
            {
               // EvokeDialogAndFetchValues();
            }

            ///-------------------------------------------------------------------------------------------------
            /// <summary> Constructor. </summary>
            /// <param name="aName"> Name of Process. </param>
            /// <param name="WSim">  The WatewrSimManager who will register process</param>
            ///-------------------------------------------------------------------------------------------------

            public PolicyLimitFeedbackProcess(string aName, WaterSimManager WSim)
                : base(aName)
            {
               // EvokeDialogAndFetchValues();
                SetUpPolicyProcessAndParameters(WSim);


            }

            ///-------------------------------------------------------------------------------------------------
            /// <summary> Constructor. </summary>
            ///<remarks> Evokes Form to set initial values.</remarks>
            /// <param name="WSim"> The WatewrSimManager who will register process. </param>
            ///-------------------------------------------------------------------------------------------------

            public PolicyLimitFeedbackProcess(WaterSimManager WSim)
                : base("AWS Limit Feedback")
            {
              //  EvokeDialogAndFetchValues();
                SetUpPolicyProcessAndParameters(WSim);
            }

            ///-------------------------------------------------------------------------------------------------
            /// <summary>   Constructor. </summary>
            ///<remarks> Does not evoke Form to set initial values, sets them to default values.</remarks>
            /// <param name="WSim">     The WatewrSimManager who will register process. </param>
            /// <param name="Quiet">    true to quiet. </param>
            ///-------------------------------------------------------------------------------------------------

            public PolicyLimitFeedbackProcess(WaterSimManager WSim, bool Quiet)
                : base("AWS Limit Feedback")
            {
                if (Quiet)
                {
                    Fname = "DEFAULT AWS LIMIT PROCESS";
                }
                else
                {
                    //EvokeDialogAndFetchValues();
                }
                SetUpPolicyProcessAndParameters(WSim);
            }

            ///-------------------------------------------------------------------------------------------------
            /// <summary> Evokes dialog and fetches values. </summary>
            ///-------------------------------------------------------------------------------------------------

             ///-------------------------------------------------------------------------------------------------
            /// <summary> Documentation. </summary>
            /// <returns> Documentation in a DocTreeNode. </returns>
            ///-------------------------------------------------------------------------------------------------

            public override DocTreeNode Documentation()
            {
                DocTreeNode node = base.Documentation();
                node.AddChildField("FMaxDeficitYears", FmaxDeficitYears.ToString());
                return node;
            }

            //============================================
            internal void SetUpPolicyProcessAndParameters(WaterSimManager WSim)
            {
                ModelParameterBaseClass MP = WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epDeficit_Years);
                FWSim = WSim;
                if (MP == null)
                {
                    FAFPTracker = new TrackGroundwaterCreditsProcess("Tracking Support for Credits Limit", WSim);
                    //FAFPTracker = new TrackAvailableGroundwater("Tracking Support for AWS Limit", WSim);
                    WSim.ProcessManager.AddProcess(FAFPTracker);
                }
                WSim.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epYearsOfCreditsTrigger, "Trigger for Negative Credit Balance", "NEGCREDITS", modelParamtype.mptInputBase, rangeChecktype.rctNoRangeCheck, 0, 0, geti_Max_Years_Deficit_Trigger, null, seti_Max_Years_Deficit_Trigger, null, null, null, null));
                WSim.ProcessManager.ProcessManagerChangeEvent += OnProcessManagerProcessEventHandler;
            }

            // OK need to have an event handler to prevent anyone from deleting the tracking process while this process is still active.
            // das
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
            //    FProcessCode = "AWS_YR=" + FMaxUnassuredYears.ToString();
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
                //for (int i = 0; i < FAddOnPop.Length; i++) FAddOnPop[i] = 0;
                //for (int i = 0; i < FAddOffPop.Length; i++) FAddOffPop[i] = 0;

                return base.ProcessStarted(year, WSim);
            }


            //============================================
            private bool TestPolicy(int year, WaterSimManager WSim)
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
 //               ProviderIntArray NewPopOff = new ProviderIntArray(0);

                bool TempLock = WSim.isLocked();

                WSim.UnLockSimulation();


                //bool ManagingPop = false;
                // for 2000 to 2010 just grab the projections
                if (year < 2015)
                {
                    }//WSim.Simulation_Start_Year + 1))
                else
                {
                    ProviderIntArray YearForZero = WSim.ParamManager.Model_ParameterBaseClass(eModelParam.epYearCreditsGoesNeg).ProviderProperty.getvalues();
                    ProviderIntArray IsTriggered = new ProviderIntArray(0);
                    // Check which are in unassured year range
                    for (int i = 0; i < YearForZero.Length; i++)
                        if (TestPolicy(YearForZero[i], (WSim as WaterSimManager)))
                        {
                            ConsecutiveDeficitYears[i]++;
                            if (ConsecutiveDeficitYears[i] > Max_Years_Deficit_Trigger)
                            {
                                IsTriggered[i] = 1;
                                // Once triggered always being managed
                                BeingManaged[i] = 1;
                            }
                        }
                        else
                        {
                            ConsecutiveDeficitYears[i] = 0;
                        }
                    PolicyClass.ManagePolicies( WSim, year, BeingManaged, IsTriggered);
                    //PopulationClass.LimitAndReallocateGrowth(WSim, year, BeingManaged, IsTriggered, FMaxUnassuredYears, ref NewPopOn, ref NewPopOff);
   
                }
                // Ok Set PopOverride Values
            //    for (int i = 0; i < NewPopOn.Length; i++)
            //    {
            //        WSim.Population_Override_On[i] = NewPopOn[i];
            //    }
            //    for (int i = 0; i < NewPopOff.Length; i++)
            //    {
            //        WSim.Population_Override_Other[i] = NewPopOff[i];
            //    }
            //    if (TempLock) WSim.LockSimulation();

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

            public int Max_Years_Deficit_Trigger
            {
                get { return FmaxDeficitYears; }
                set { FmaxDeficitYears = value; }
            }

            internal int geti_Max_Years_Deficit_Trigger() { return Max_Years_Deficit_Trigger; }
            internal void seti_Max_Years_Deficit_Trigger(int value) { Max_Years_Deficit_Trigger = value; }
        }

    #endregion

       
}
