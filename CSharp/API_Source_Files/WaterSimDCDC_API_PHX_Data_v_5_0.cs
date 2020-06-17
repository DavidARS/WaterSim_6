// ===========================================================
//     WaterSimDCDC Regional Water Demand and Supply Model Version 5.0

//       A Class the adds access to WaterSim Data files and related parameter management
//       
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
using WaterSimDCDC;

namespace WaterSimDCDC.Data
{
    ///-------------------------------------------------------------------------------------------------
    /// <summary> Population class. </summary>
    ///
    /// <remarks> A Class that loads Population Data from WaterSim data files and provides support for managing population Parameters </remarks>
    ///-------------------------------------------------------------------------------------------------

    public class PopulationClass
    {
        private const int DefaultStartYear = 2000;
        // 06.29.16 DAS shit...
        //string FortranPopOnFilename = "App_Data\\Data\\OnProjectPopulation_2085.txt";
        //string FortranPopOffFilename = "App_Data\\Data\\OtherPopulation_2085.txt";
        //
        string FortranPopOnFilename = "App_Data\\Data\\OnProjectApril_2085.txt";
        string FortranPopOffFilename = "App_Data\\Data\\OtherProjectApril_2085.txt";


        List<ProviderIntArray> PopOnByYearList = new List<ProviderIntArray>();
        List<ProviderIntArray> PopOffByYearList = new List<ProviderIntArray>();
        List<ProviderIntArray> PopTotalByYearList = new List<ProviderIntArray>();
        List<ProviderDoubleArray> PopOnRateByYearList = new List<ProviderDoubleArray>();
        List<ProviderDoubleArray> PopOffRateByYearList = new List<ProviderDoubleArray>();

        
        int FStartYear = 0;
        
        
        string FPopPathName = "";

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Default constructor. </summary>
        ///-------------------------------------------------------------------------------------------------

        public PopulationClass()
        {

        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Constructor. </summary>
        ///
        /// <remarks> Loads the OnProject and OtherProject population files from the AppData directory. Since these tables do not indicate the start year of the 
        ///                     population data, that must be passed.  Default is 2000.</remarks>
        ///
        /// <exception cref="Exception"> Thrown when an exception error condition occurs. </exception>
        ///
        /// <param name="pathname">  Full pathname to where WaterSim Data is located, same as WaterSimManager.DataDirectory. </param>
        /// <param name="StartYear"> The start year of the population table, if zero Set to PopulationClass.DefaultStartYear. </param>
        /// <seealso cref="WaterSimManager.DataDirectory"/>
        /// <seealso cref="PopulationClass.DefaultStartYear"/>
        ///-------------------------------------------------------------------------------------------------

        public PopulationClass(string pathname, int StartYear)
        {
            string myerr = "";
            if (!LoadAllPopData(pathname, ref myerr))
            {
                throw new Exception(WS_Strings.Get(WS_Strings.wsPopDataLoadError) + myerr);
            }
            if (StartYear == 0)
                FStartYear = DefaultStartYear;
            else
                FStartYear = StartYear;
            }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Gets Total Projected Population for a given year. </summary>
        ///
        /// <exception cref="Exception"> Thrown when an exception error condition occurs. </exception>
        ///
        /// <param name="year"> The year. </param>
        ///
        /// <returns> The POP data. </returns>
        ///-------------------------------------------------------------------------------------------------

        public ProviderIntArray GetYearPopData(int year)
        {
            int index = year - FStartYear;
            if ((index<0)||(index>(PopTotalByYearList.Count-1)))
            {
                throw new Exception(WS_Strings.Get(WS_Strings.wsInvalidPopIndex));
            }
            return PopTotalByYearList[index];
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Gets OnProject Projected Population for a given year. </summary>
        /// <exception cref="Exception"> Thrown when an exception error condition occurs. </exception>
        ///
        /// <param name="year"> The year. </param>
        ///
        /// <returns> The POP data. </returns>
        ///-------------------------------------------------------------------------------------------------

        public ProviderIntArray GetYearOnPopData(int year)
        {
            int index = year - FStartYear;
            if ((index < 0) || (index > (PopOnByYearList.Count - 1)))
            {
                throw new Exception(WS_Strings.Get(WS_Strings.wsInvalidPopIndex));
            }
            return PopOnByYearList[index];
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Gets Other (off Project) Projected Population for a given year. </summary>
        /// <exception cref="Exception"> Thrown when an exception error condition occurs. </exception>
        ///
        /// <param name="year"> The year. </param>
        ///
        /// <returns> The POP data. </returns>
        ///-------------------------------------------------------------------------------------------------

        public ProviderIntArray GetYearOffPopData(int year)
        {
            int index = year - FStartYear;
            if ((index < 0) || (index > (PopOffByYearList.Count - 1)))
            {
                throw new Exception(WS_Strings.Get(WS_Strings.wsInvalidPopIndex));
            }
            return PopOffByYearList[index];
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Gets the OnProject projected growth rate for a specified year. </summary>
        /// <exception cref="Exception"> Thrown when an exception error condition occurs. </exception>
        ///
        /// <param name="year"> The year. </param>
        ///
        /// <returns> The growth rate. </returns>
        ///-------------------------------------------------------------------------------------------------

        public ProviderDoubleArray GetYearRatePopOnData(int year)
        {
            int index = year - FStartYear;
            if ((index < 0) || (index > (PopOnRateByYearList.Count - 1)))
            {
                throw new Exception(WS_Strings.Get(WS_Strings.wsInvalidPopIndex));
            }
            return PopOnRateByYearList[index];
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Gets the Other (OffProject) projected growth rate for a specified year. </summary>
        /// <exception cref="Exception"> Thrown when an exception error condition occurs. </exception>
        ///
        /// <param name="year"> The year. </param>
        ///
        /// <returns> The growth rate. </returns>
        ///-------------------------------------------------------------------------------------------------

        public ProviderDoubleArray GetYearRatePopOffData(int year)
        {
            int index = year - FStartYear;
            if ((index < 0) || (index > (PopOffRateByYearList.Count - 1)))
            {
                throw new Exception(WS_Strings.Get(WS_Strings.wsInvalidPopIndex));
            }
            return PopOffRateByYearList[index];
        }

        internal string AddFileName(string Path, string Filename)
        {
            if (Path.Length > 2)
            {
                string temp = Path.Substring(Path.Length - 2);

                if (temp != "\\")
                {
                    if (Path[Path.Length - 1] == '\\')
                    { Path = Path + "\\"; }
                    else { Path = Path + "\\"; }
                }
            }

            return Path + Filename;
                
        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Gets or sets the full pathname of the POP data file. </summary>
        ///
        /// <value> The full pathname of the POP data file. </value>
        ///-------------------------------------------------------------------------------------------------

        public string PopDataPathName
        {
            get { return FPopPathName; }
            set
            {
                string errString = "";
                bool test = (LoadAllPopData(value, ref errString));
                if (test)
                { 
                    FPopPathName = value; }
                else
                { 
                    throw new WaterSim_Exception(WS_Strings.Get(WS_Strings.wsPopDataLoadError)+" "+errString);
                }
            }

        }

        bool LoadAllPopData(string DataPathName, ref string errString)
        {
            bool test = false;
            // Clear the OnPop list
            PopOnByYearList.Clear();
            // Build Pathname
            string filename = AddFileName(DataPathName, FortranPopOnFilename);
            // Load the OnPop data
            test = LoadPopDataFile(filename, ref errString, ref PopOnByYearList);
            // Continue if no error
            if (test)
            {
                // Clear the off pop list
                PopOffByYearList.Clear();
                // Build Filename
                filename = AddFileName(DataPathName, FortranPopOffFilename);
                // load OffPop data
                test = LoadPopDataFile(filename, ref errString, ref PopOffByYearList);
                // continue if no error
                if (test)
                {
                    // Calculate Total Pop
                    PopTotalByYearList.Clear();
                    // Cycle through each year's provider array for pop list
                    for (int i = 0; i < PopOnByYearList.Count; i++)
                    {
                        ProviderIntArray Tot = new ProviderIntArray(0);
                        // Add on and off for each provider
                        for (int j = 0; j < Tot.Length; j++)
                        {
                            Tot.Values[j] = PopOnByYearList[i].Values[j] + PopOffByYearList[i].Values[j];
                        }
                        // add total to the pop year list
                        PopTotalByYearList.Add(Tot);
                    }
                    // Calculate Growth Rate
                    // Calculate Total Pop
                    PopOnRateByYearList.Clear();
                    PopOffRateByYearList.Clear();
                    // Cycle through each year's provider array for pop list
                    for (int i = 1; i < PopTotalByYearList.Count; i++)
                    {
                        ProviderDoubleArray Rate = new ProviderDoubleArray(0.0);
                        // Add on for each provider
                        for (int j = 0; j < Rate.Length; j++)
                        {
                            double LastYearOn = Convert.ToDouble(PopOnByYearList[i-1].Values[j]);
                            double ThisYearOn = Convert.ToDouble(PopOnByYearList[i].Values[j]);
                            if (LastYearOn > 0.0)
                            {
                                double pctchange = (ThisYearOn - LastYearOn) / LastYearOn;
                                Rate[j] = 1+ pctchange;
                                //if (pctchange > .05)
                                //    pctchange = .05;
                            }
                            else
                                Rate[j] = 1.0;
                            
                        }
                        // add total to the pop On year list
                        PopOnRateByYearList.Add(Rate);
                        
                        // Add off and off for each provider
                        for (int j = 0; j < Rate.Length; j++)
                        {
                            double LastYearOff = Convert.ToDouble(PopOffByYearList[i - 1].Values[j]);
                            double ThisYearOff = Convert.ToDouble(PopOffByYearList[i].Values[j]);
                            if (LastYearOff > 0.0)
                            {
                                double pctchange = (ThisYearOff - LastYearOff) / LastYearOff;
                                Rate[j] = 1+pctchange;
                                //if (pctchange > .05)
                                //    pctchange = .05;
                            }
                            else
                                Rate[j] = 1.0;

                        }
                        // add total to the pop Off year list
                        PopOffRateByYearList.Add(Rate);
                    }

                }
            }
            if (test == false)
            {
                PopOffByYearList.Clear();
                PopOnByYearList.Clear();
                PopTotalByYearList.Clear();
                PopOnRateByYearList.Clear();
                PopOffRateByYearList.Clear();
            }
            return test;
        }

        bool LoadPopDataFile(string DataPathName, ref string errString, ref List<ProviderIntArray> PopByYearList)
        {
            bool test = true;
            string MyErrString = "";
            List<UniDB.Tools.DynamicTextData> DataList = new List<UniDB.Tools.DynamicTextData>();
            List<string> DataLines = new List<string>();
            DataLines = UniDB.Tools.ReadLinesFromTextFile(DataPathName,ref MyErrString);
            if (MyErrString != "")
            { 
                test = false;
                errString = MyErrString;
            }

            int linecnt = 0;
            foreach(string line in DataLines)
            {
                if (line.Length > 0)
                {
                    int cnt = 0;
                    int index = 0;
                    ProviderIntArray DataYear = new ProviderIntArray(0);
                    DataList.Clear();
                    DataList = UniDB.Tools.FetchDataFromTextLine(line, UniDB.Tools.DataFormat.SpaceDelimited);

                    foreach (UniDB.Tools.DynamicTextData dtd in DataList)
                    {
                        if (WaterSimU.ProvNameIndicies[cnt] > -1)
                        {
                            if (dtd.CanBeInt())
                            {
                                DataYear.Values[index] = dtd.ValueInt;
                            }

                            index++;
                        }
                        cnt++;
                    }
                    if (index != ProviderClass.NumberOfProviders)
                    {
                        test = false;
                        errString += " Line " + linecnt.ToString() + " Field Cnt = " + cnt.ToString();

                    }
                    PopByYearList.Add(DataYear);
                }
                linecnt++;
            }        
            
            
            return test;

        }

        ///-------------------------------------------------------------------------------------------------
        /// <summary> Limit and reallocate growth. </summary>
        ///
        /// <remarks> This is a standard routine to limit growth for some providers and reallocate lost growth potential to other providers. 
        ///           Tries to keep total regional reallocated numbers the same as the total regional projected regional population numbers.
        ///           The Being managed array 
        ///           and the trigger array need to be set before call to this routine.  After the call the users code must set the population parameters.  The following code provides and example.  
        ///           <code>  
        ///                     ProviderIntArray IsTriggered = new ProviderIntArray(0);
        ///                     for (int i = 0; i < YearForZero.Length; i++)
        ///                        if (TestUnassured(YearForZero[i], WSim))
        ///                        {
        ///                            ConsecutiveUnassuredYears[i]++;
        ///                            if (ConsecutiveUnassuredYears[i]>FMaxUnassuredYears)
        ///                            {  
        ///                                // mark as triggered
        ///                                 IsTriggered[i]=0;
        ///                                // Once triggered always being managed
        ///                                BeingManaged[i] = 1;
        ///                        }
        ///                        else
        ///                        {
        ///                            ConsecutiveUnassuredYears[i] = 0;
        ///                        }
        ///
        ///                    PopulationClass.LimitAndReallocateGrowth(WSim, year, BeingManaged, IsTriggered, ref NewPopOn, ref NewPopOff);
        ///
        ///                }
        ///                // Ok Set PopOverride Values
        ///                for (int i = 0; i < NewPopOn.Length; i++)
        ///                {
        ///                    WSim.Population_Override_On[i] = NewPopOn[i];
        ///                }
        ///                for (int i = 0; i < NewPopOff.Length; i++)
        ///                {
        ///                    WSim.Population_Override_Other[i] = NewPopOff[i];
        ///                }
        ///   </code>
        ///                    </remarks>
        ///
        /// <param name="WSim">         The WaterSimManager for simulation. </param>
        /// <param name="year">         The year of simulation. </param>
        /// <param name="BeingManaged"> A ProviderIntArray indicating providers being managed, 1 = use projected growth rate, 0= use projected pop numbers . </param>
        /// <param name="TriggerCnt">  A ProviderIntArray indicating providers being limited (ie no growth) >0=Limit,  0 = Do not limit. </param>
     
        /// <param name="NewPopOn">     [in,out] The new Onproject POP numbers. </param>
        /// <param name="NewPopOff">    [in,out] The new Other (off project) POP numbers. </param>
        /// 
        ///-------------------------------------------------------------------------------------------------

        static public void LimitAndReallocateGrowth(WaterSimManager WSim, int year, ProviderIntArray BeingManaged, ProviderIntArray Triggered,  ref ProviderIntArray NewPopOn, ref ProviderIntArray NewPopOff)
        {
            ProviderIntArray LastYearOnPop = WSim.Population_On_Project.getvalues();
            ProviderIntArray LastYearOffPop = WSim.Population_Other.getvalues();
   
            ProviderIntArray PulledPopOn = WSim.Projected_OnProject_Pop.getvalues();
            ProviderIntArray PulledPopOff = WSim.Projected_Other_Pop.getvalues();
            bool ReallocatePop = false;
            
    
            for (int i = 0; i < NewPopOn.Length; i++)
            {
                if (Triggered[i]>0)
                {
                    ReallocatePop = true;
                    NewPopOn[i] = LastYearOnPop[i];
                    NewPopOff[i] = LastYearOffPop[i];
                }
                else
                if (BeingManaged[i] > 0)
                {
                    ReallocatePop = true;
                    double LastOn = Convert.ToDouble(LastYearOnPop[i]);
                    double RateOn = WSim.PopData.GetYearRatePopOnData(year).Values[i];
                    double LastOff = Convert.ToDouble(LastYearOffPop[i]);
                    double RateOff = WSim.PopData.GetYearRatePopOffData(year).Values[i];
                    int Addon = Convert.ToInt32(LastOn * RateOn);
                    int Addoff = Convert.ToInt32(LastOff * RateOff);
                    NewPopOff[i] = Addoff ;
                    NewPopOn[i] = Addon ;
                }
                else
                {
                    // Check and see if on trajectory for higher
                    if (PulledPopOn[i] > LastYearOnPop[i])
                        NewPopOn[i] = PulledPopOn[i];
                    else
                        NewPopOn[i] = LastYearOnPop[i];
                    // Check and see if on trajectory for higher
                    if (PulledPopOff[i] > LastYearOffPop[i])
                        NewPopOff[i] = PulledPopOff[i];
                    else
                        NewPopOff[i] = LastYearOffPop[i];
                }
            }  // For newPop
                        // OK Now reallocate unused growth
                
            if (ReallocatePop)
                // Only reason not doing this is if no one has been ever been triggered to stop growth
            {
                // Calc Total Potential growth and Modified Growth
                // On Possible Total
                int TotalPossibleOnGrowth = 0;
                    
                foreach (int value in PulledPopOn.Values)
                {
                    TotalPossibleOnGrowth += value;
                }
                // off Possible Total
                int TotalPossibleOffGrowth = 0;
                foreach (int value in PulledPopOff.Values)
                {
                    TotalPossibleOffGrowth += value;
                }
                // on Actual Modified
                int TotalModifiedOnGrowth = 0;
                foreach (int value in NewPopOn.Values)
                {
                    TotalModifiedOnGrowth += value;
                }
                // off Possible Total
                int TotalModifiedOffGrowth = 0;
                foreach (int value in NewPopOff.Values)
                {
                    TotalModifiedOffGrowth += value;
                }
                // Calculate Difference

                double  PopDifferenceOn = Convert.ToDouble(TotalPossibleOnGrowth - TotalModifiedOnGrowth);
                double PopDifferenceOff = Convert.ToDouble(TotalPossibleOffGrowth - TotalModifiedOffGrowth);

                // Calculate Totals of NonDeficit People
                double TotalNotLimitedOn = 0;
                double TotalNotLimitedOff = 0;
                for (int i=0;i<NewPopOff.Length;i++)
                {
                    if ( Triggered[i]>0)
                    {
                    TotalNotLimitedOff += NewPopOff[i];
                    }
                }
                for (int i = 0; i < NewPopOn.Length; i++)
                {
                    if (Triggered[i] >0)
                    {
                        TotalNotLimitedOn += NewPopOn[i];
                    }
                }
                // Calculate Ratios for those not in deficit
                ProviderDoubleArray ShareOn= new ProviderDoubleArray(0); 
                ProviderDoubleArray ShareOff = new ProviderDoubleArray(0);
                for (int i = 0; i < NewPopOn.Length; i++ )
                {
                    if ((TotalNotLimitedOn > 0) && (Triggered[i]==0))
                    {
                        ShareOn[i] = Convert.ToDouble(NewPopOn[i]) / TotalNotLimitedOn;
                    }
                }
                for (int i = 0; i < NewPopOff.Length; i++)
                {
                    if ((TotalNotLimitedOff > 0)&&(Triggered[i]==0))
                    {
                        ShareOff[i] = Convert.ToDouble(NewPopOff[i]) / TotalNotLimitedOff;
                    }
                }
                // Now allocate Extra growth based on ratio
                for (int i = 0; i < NewPopOff.Length; i++)
                {
                    if (Triggered[i]==0)
                    {
                        // Calculate how much to add this year
                        int addmore = Convert.ToInt32(PopDifferenceOff * ShareOff[i]);
                        // Re add this added amount.
                        NewPopOff[i] += addmore;
                        }
                }
                for (int i = 0; i < NewPopOn.Length; i++)
                {
                    if (Triggered[i]==0)
                    {
                        // Calculate how much to add this year
                        int addmore = Convert.ToInt32(PopDifferenceOn * ShareOn[i]);
                        // Re add this added amount.
                        NewPopOn[i] += addmore;
                    }
                }
            } 
        }
    }
}
