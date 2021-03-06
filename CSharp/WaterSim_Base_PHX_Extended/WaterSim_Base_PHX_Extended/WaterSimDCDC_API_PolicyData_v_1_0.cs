﻿// ===========================================================
//     WaterSimDCDC Regional Water Demand and Supply Model Version 5.0

//       A Class the adds Support for Data Not Derived from the WaterSim Model

//       WaterSimDCDC_API_Process 
//       Version 5.0
//       Keeper Ray Quay  ray.quay@asu.edu
//       Copyright (C) 2011,2012 , The Arizona Board of Regents
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
using UniDB;
namespace WaterSimDCDC
{
    public class PolicyClass
    {
        public PolicyClass()
        {
        }
        static public void ManagePolicies(WaterSimManager WSim, int year, ProviderIntArray BeingManaged, ProviderIntArray TriggerCnt)
        {
            for (int i = 0; i < 2; i++)
            {
                if (0 < BeingManaged[i])
                {

                }
                else
                {
                }
            }
        }
    }
}    
namespace WaterSimDCDC.Processes
{

    public class TrackGroundwaterCreditsProcess : WaterSimDCDC.AnnualFeedbackProcess
    {
        public ProviderIntArray FYearsOfZeroOrBelow = new ProviderIntArray(0);
        public ProviderDoubleArray FAvailSlope = new ProviderDoubleArray(0.0);
        public ProviderDoubleArray FSlopeIntercept = new ProviderDoubleArray(0.0);

        public ProviderIntArray FYearOfZero = new ProviderIntArray(0);
        public ProviderIntArray FYearsNegative = new ProviderIntArray(0);
        public ProviderIntArray FInitialLevel = new ProviderIntArray(0);
        public ProviderIntArray FPctCreditsAvail = new ProviderIntArray(0);
        public ProviderIntArray FYearCreditsGoNeg = new ProviderIntArray(0);
        // used to prevent more than on parameter being added to list
        static int ClassCount = 0;
        const int SlopeYears = 5;

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Default constructor. </summary>
        ///-------------------------------------------------------------------------------------------------

        public TrackGroundwaterCreditsProcess()
            : base()
        {

        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Constructor. </summary>
        /// <param name="WSim"> The WaterSimManager that is making call. </param>
        ///-------------------------------------------------------------------------------------------------

        public TrackGroundwaterCreditsProcess(WaterSimManager WSim)
            : base(WSim)
        {
            SetupParameters(WSim);
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Constructor. </summary>
        /// <param name="aName"> The name. </param>
        /// <param name="WSim">  The WaterSimManager that is making call. </param>
        ///-------------------------------------------------------------------------------------------------

        public TrackGroundwaterCreditsProcess(string aName, WaterSimManager WSim)
            : base(aName, WSim)
        {
            SetupParameters(WSim);
        }

        internal void SetupParameters(WaterSimManager WSim)
        {
            // Only add one parameter  Checking just in case more than one is added, Process manager should not allow more than one but that will
            // not stop someone from constructing two objects and not add to ProcessManager
            if (ClassCount == 0)
            {
                ClassCount++;
                // Add % Groundwater
                Percent_Credits_Available = new providerArrayProperty(WSim.ParamManager, eModelParam.epPCT_CreditsAvailable, get_PCT_CreditsAvail, eProviderAggregateMode.agWeighted);
                WSim.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epPCT_CreditsAvailable, "Percent of Initial Groundwater Credits Available", "PCTCRED", modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck, 0, 0, null, get_PCT_CreditsAvail, null, null, null, null, Percent_Credits_Available));
                // epYearCreditsGoesNeg
                Year_Credits_Go_Negative = new providerArrayProperty(WSim.ParamManager, eModelParam.epYearCreditsGoesNeg, get_PCT_CreditsAvail, eProviderAggregateMode.agWeighted);
                WSim.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epYearCreditsGoesNeg, "Year that Credts go negative", "YRCREDNEG", modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck, 0, 0, null, get_Year_CreditsGoNegative, null, null, null, null, Year_Credits_Go_Negative));


             }
            this.FWsim = WSim;
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Clean up. </summary>
        ///-------------------------------------------------------------------------------------------------

        public override void CleanUp()
        {
            if (this.FWsim != null)
            {
                // OK clean up by delete this parameter
                this.FWsim.ParamManager.DeleteParameter(eModelParam.epPCT_CreditsAvailable);
                this.FWsim.ParamManager.DeleteParameter(eModelParam.epYearCreditsGoesNeg);
            }
        }
        new static public string ClassDescription()
        {
            return "Tracks groundwater credits to calculate the slope of the credit balance";
        }
        ///-------------------------------------------------------------------------------------------------
        /// <summary> Builds the description strings. </summary>
        ///-------------------------------------------------------------------------------------------------

        protected override void BuildDescStrings()
        {
            FProcessDescription = "Creates parameters to track groundwater credits ";
            FProcessLongDescription = "Creates groundwater parameters to track the groundwater credit score b";
            FProcessCode = "TRKGW";
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> method that is called right before the first year of a simulation is called.
        ///     </summary>
        /// <param name="year"> The year about to be run. </param>
        /// <param name="WSim"> The WaterSimManager that is making call. </param>
        ///
        /// <returns> true if it succeeds, false if it fails. </returns>
        ///-------------------------------------------------------------------------------------------------

        public override bool ProcessStarted(int year, WaterSimManagerClass WSim)
        {
            // zero out accumulating arrays
            for (int i = 0; i < FYearsOfZeroOrBelow.Length; i++)
            {
                FPctCreditsAvail[i] = 0;
                FYearsNegative[i] = 0;
                FInitialLevel[i] = 0;
                FAvailSlope[i] = 0.0;
                FSlopeIntercept[i] = 0.0;
                FYearCreditsGoNeg[i] = 0;

                // Get Initial Levels
            }
            return base.ProcessStarted(year, WSim);
        }

        const int ASSURREDYEARS = 100;

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Method that is called after each annual run. </summary>
        /// <param name="year"> The year about to be run. </param>
        /// <param name="WSim"> The WaterSimManager that is making call. </param>
        /// <returns> true if it succeeds, false if it fails. Error should be placed in FErrorMessage.
        ///     </returns>
        ///-------------------------------------------------------------------------------------------------

        public override bool PostProcess(int year, WaterSimManagerClass WSimClass)
        {
            WaterSimManager WSim = (WSimClass as WaterSimManager);

            // check if GW is zero or below
            ProviderIntArray CreditBal = new ProviderIntArray(0);
            CreditBal = WSim.AF_water_CreditDeficits.getvalues();
            if (year == WSim.Simulation_Start_Year)
            {
                for (int i = 0; i < CreditBal.Length; i++)
                {
                    FInitialLevel[i] = CreditBal[i];
                    if (CreditBal[i] > 0)
                    {
                        FPctCreditsAvail[i] = 100;
                        FYearCreditsGoNeg[i] = 1;
                    }
                    else
                    {
                        FPctCreditsAvail[i] = 0;
                        FYearCreditsGoNeg[i] = 0;
                    }
                }
            }
            else
            {

                for (int i = 0; i < CreditBal.Length; i++)
                {
                    if (CreditBal[i] < 1)
                    // If zero or below, start counting
                    {
                        FYearsOfZeroOrBelow[i]++;
                        if (FYearOfZero[i] == 0)
                        {
                            FYearOfZero[i] = year;
                        }
                    }
                    // calculate slope and X intercept (ie projected year 0)
                    double Y1 = Convert.ToDouble(CreditBal[i]);
                    double Rise = Y1 - Convert.ToDouble(FInitialLevel[i]);
                    double X1 = Convert.ToDouble(year);
                    double Run = Convert.ToDouble(year - WSim.Simulation_Start_Year);
                    if ((Rise < 0) && (Run > 0))
                    {
                        FAvailSlope[i] = Rise / Run;
                        double eqconstant = Y1 - (FAvailSlope[i] * X1);
                        if ((FAvailSlope[i] != 0) && (eqconstant != 0))
                            FSlopeIntercept[i] = (-1 * eqconstant) / FAvailSlope[i];  
                        else
                            FSlopeIntercept[i] = 0;
                    }
                    else
                    {
                        FAvailSlope[i] = 0;
                        FSlopeIntercept[i] = 0;
                    }
                    // check if first year
                    if ((FSlopeIntercept[i] > WSim.Simulation_Start_Year) && (FSlopeIntercept[i] < year + SlopeYears))
                    {
                        FYearsNegative[i]++;
                    }
                    // This should only happen if the utility has no groundwater
                    if (FInitialLevel[i] > 0)
                    {
                        // calculate PCT GW Avail
                        double InitLevel = Convert.ToDouble(FInitialLevel[i]);
                        FPctCreditsAvail[i] = Convert.ToInt32((Convert.ToDouble((CreditBal[i]) / InitLevel) * 100));
                    }
                    else
                    {  // no groundwater 
                        // OK here is a wierd rule.  It is possible for a utility to start with no ground water and acquire ground water,
                        // If that is the case, then the first time groundwater is acquired, then balance is set to that.
                        if (CreditBal[i] > 0)
                        {
                            FInitialLevel[i] = CreditBal[i];
                            FPctCreditsAvail[i] = 100;
                            FYearCreditsGoNeg[i] = 1;
                        }
                        else
                        {
                            FPctCreditsAvail[i] = 0; 
                            FYearCreditsGoNeg[i] = 0;
                        }
                    }
                }
            }

            return base.PostProcess(year, WSim);
        }


        //=========================================================
        //Percent Credits water Available
        //---------------------------------------     

        private int[] get_PCT_CreditsAvail()
        {
            ProviderIntArray FPCT_GW = new ProviderIntArray(0);
            // get deficit and demand

            for (int i = 0; i < FPCT_GW.Length; i++)
            {
                FPCT_GW[i] = FPctCreditsAvail[i];
            }
            return FPCT_GW.Values;
        }
        private int[] get_Year_CreditsGoNegative()
        {
            ProviderIntArray FYEAR_Credits = new ProviderIntArray(0);
            // get deficit and demand

            for (int i = 0; i < FYEAR_Credits.Length; i++)
            {
                FYEAR_Credits[i] = FYearCreditsGoNeg[i];
            }
            return FYEAR_Credits.Values;
        }

        /// <summary> Groundwater as a percent of Initial Groundwater Available (100 = 100%) </summary>
        ///<remarks>0 if Deficit is 0 </remarks>
        /// <seealso cref="Demand_Deficit"/>
        /// 
        public providerArrayProperty Percent_Credits_Available;
        public providerArrayProperty Year_Credits_Go_Negative;
        //=========================================================
        //Years of GW zero or Below
        //---------------------------------------     

        private int[] get_YearsOfZero()
        {
            ProviderIntArray YearsOfZero = new ProviderIntArray(0);
            // get deficit and demand

            for (int i = 0; i < YearsOfZero.Length; i++)
            {
                YearsOfZero[i] = FYearsOfZeroOrBelow[i];
            }
            return YearsOfZero.Values;
        }
        /// <summary> The years groundwater Balance at or below zero. </summary>
        /// <seealso cref="Groundwater_Balance"/>
        public providerArrayProperty Years_GW_At_or_Below_Zero;

        //=========================================================
        //Year Credits will go to zero
        //---------------------------------------     

        private int[] get_YearCreditWillBeZero()
        {
            ProviderIntArray YearWillBeZero = new ProviderIntArray(0);
            // get deficit and demand
            for (int i = 0; i < YearWillBeZero.Length; i++)
            {
                if (FSlopeIntercept[i] > 3000)
                    YearWillBeZero[i] = 3000;
                else
                    if (FSlopeIntercept[i] < 0)
                        YearWillBeZero[i] = 0;
                    else
                        YearWillBeZero[i] = Convert.ToInt32(FSlopeIntercept[i]);
            }
            return YearWillBeZero.Values;
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> The years grounwater will be zero. </summary>
        /// <seealso cref="Groundwater_Balance"/>
        ///<seealso cref="Years_Groundwater_Not_Assured"/>
        ///-------------------------------------------------------------------------------------------------

        public providerArrayProperty Year_Groundwater_Will_Be_Zero;
        //=========================================================
        //Count of Years during which Groundwater can not neet Assured Supply rule.
        //---------------------------------------     

        private int[] get_Years_Negative()
        {
            ProviderIntArray YearsNegative = new ProviderIntArray(0);
            // get deficit and demand

            for (int i = 0; i < YearsNegative.Length; i++)
            {
                YearsNegative[i] = FYearsNegative[i];
            }
            return YearsNegative.Values;
        }

        /// <summary> The Number of years groundwater can not be assured. </summary>
        ///<seealso cref="Groundwater_Balance"/>
        ///<seealso cref="Year_Groundwater_Will_Be_Zero"/>

        public providerArrayProperty Years_Groundwater_Not_Assured;
    }
}
