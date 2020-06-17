using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using WaterSimDCDC;
using API_WaterSim;

namespace StreamOutputs
{
    public class FileOutputsBase
    {
        public List<int> myParms = new List<int>();
        public List<int> myControls = new List<int>();
        public int[] eModelParametersForOutput;
        public FileOutputsBase()
        {
            

        }
    }
     public class FileOutputs 
    {
        internal WaterSimManager_SIO wsim;
        int RecCount = 1;
       // public List<int> myParms = new List<int>();
       // public int[] eModelParametersForOutput;
        internal FileOutputsBase FOB;
        //public List<int> myParms = new List<int>();
        //const int OutPutParamN = 3;
        //int[] eModelParametersForOutput = new int[OutPutParamN];

        const int OutPutParamN = 2;
        //int[] eModelParametersForOutput = new int[OutPutParamN]
        //  {
        //    eModelParam.epSaltVerde_Annual_Deliveries_SRP,
        //    eModelParam.epColorado_Annual_Deliveries
   

        //  };

        public FileOutputs(WaterSimManager_SIO ws)
        {
            this.wsim = ws;
           // this.FOB = fob;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="SR"></param>
        /// <param name="SW"></param>
        /// <returns></returns>
        public bool WriteHeader(SimulationResults SR, myVariables mine, System.IO.StreamWriter SW)
        {
            bool result = false;
            int FirstYear = SR.StartYear;
            AnnualSimulationResults ASR = SR.ByYear(FirstYear);
            string BaseStr = "ID,BUILD,SCN_NAME,SIMYEAR";
            //FileOutputsBase fob = this.FOB;

            // now loop through base outputs and set those
            try
            {
                int index = 0;
                foreach (int emp in ASR.Outputs.BaseOutputModelParam)
                {
                    // check if this is one of the output fields
                    if (isOutPutParam(mine,emp))
                    {
                        ModelParameterClass MP = wsim.ParamManager.Model_Parameter(emp);

                        BaseStr += "," + MP.Fieldname;
                    }
                    index++;
                }
                // lopp through base inputs
                index = 0;
                foreach (int emp in ASR.Inputs.BaseInputModelParam)
                {
                    if (isOutPutParam(mine,emp))
                    {
                        ModelParameterClass MP = wsim.ParamManager.Model_Parameter(emp);

                        BaseStr += "," + MP.Fieldname;
                    }
                    index++;
                }
                BaseStr += ",PRVDCODE";
                // loop through provider outputs
                index = 0;
                foreach (int emp in ASR.Outputs.ProviderOutputModelParam)
                {
                    // check if for output
                    if (isOutPutParam(mine,emp))
                    {
                        ModelParameterClass MP = wsim.ParamManager.Model_Parameter(emp);

                        BaseStr += "," + MP.Fieldname;
                    }
                    index++;
                }
                // loop through provider inputs
                index = 0;
                foreach (int emp in ASR.Inputs.ProviderInputModelParam)
                {
                    // check if for output
                    if (isOutPutParam(mine,emp))
                    {
                        ModelParameterClass MP = wsim.ParamManager.Model_Parameter(emp);

                        BaseStr += "," + MP.Fieldname;
                    }
                    index++;
                }

                SW.WriteLine(BaseStr);
                result = true;
            }
            finally
            {
                SW.Flush();
            }
            return result;
        }

        public bool WriteResults(SimulationResults SR, string ScenarioName,  myVariables mine,System.IO.StreamWriter SW)
        {

            bool result = false;
            string IDS = "";
            string build = wsim.ModelBuild;
            // get the start year            
            int FirstYear = SR.StartYear;

            // loop through all years in SR
            for (int yeari = 0; yeari < SR.Length; yeari++)
            {
                // Set the calender year
                int ThisYear = FirstYear + yeari;
                if(ThisYear < 2060){
                // get results for this year
                AnnualSimulationResults ASR = SR.ByYear(ThisYear);

                // set the key, the scenario name and the year
                string BaseStr = RecCount.ToString() + "," + '"'+ build + '"'+ ","  + ScenarioName +  ThisYear.ToString();
                // now loop through base outputs and set those
                int index = 0;
                foreach (int emp in ASR.Outputs.BaseOutputModelParam)
                {
                    // check if this is one of the output fields
                    if (isOutPutParam(mine,emp))
                    {
                        BaseStr += "," + ASR.Outputs.BaseOutput.Values[index].ToString();
                    }
                    index++;
                }
                // lopp through base inputs
                index = 0;
                foreach (int emp in ASR.Inputs.BaseInputModelParam)
                {
                    if (isOutPutParam(mine,emp))
                    {
                        BaseStr += "," + ASR.Inputs.BaseInput.Values[index].ToString();
                    }
                    index++;
                }
                // OK, have all base stuff, now loop through providers
                eProvider ep = eProvider.Regional;
                //foreach (eProvider ep in ProviderClass.providersAll())
                {
                    // Increment the rec count
                    RecCount++;
                    // set the base string
                    IDS = BaseStr + "," + '"' + ProviderClass.FieldName(ep) + '"';
                    // loop through provider outputs
                    index = 0;
                    foreach (int emp in ASR.Outputs.ProviderOutputModelParam)
                    {
                        // check if for output
                        if (isOutPutParam(mine,emp))
                        {
                            if ((ep < eProvider.Regional) || (ASR.Outputs.ProviderOutput[index].IncludesAggregates))
                            {
                                IDS += "," + ASR.Outputs.ProviderOutput[index].Values[ProviderClass.index(ep, true)].ToString();
                            }
                            else
                            {
                                IDS += "," + SpecialValues.MissingIntValue.ToString();
                            }
                        }
                        index++;
                    }
                    // loop through provider inputs
                    index = 0;
                    foreach (int emp in ASR.Inputs.ProviderInputModelParam)
                    {
                        // check if for output
                        if (isOutPutParam(mine,emp))
                        {
                            if ((ep < eProvider.Regional) || (ASR.Inputs.ProviderInput[index].IncludesAggregates))
                            {
                                IDS += "," + ASR.Inputs.ProviderInput[index].Values[ProviderClass.index(ep, true)].ToString();
                            }
                            else
                            {
                                IDS += "," + SpecialValues.MissingIntValue.ToString();
                            }
                        }
                        index++;
                    }
                    // ok write it out
                    SW.WriteLine(IDS);
                } // provider
            }
            } // year
            SW.Flush();
            result = true;
            return result;
        }
        public bool isOutPutParam(myVariables mine,int theEmp)
        {
            bool found = false;
            //const int OutPutParamN = 2;
            //int[] eModelParametersForOutput = new int[OutPutParamN]
            //  {
            //    eModelParam.epGroundwater_Pumped_Municipal,
            //    eModelParam.epRegGWpctInitial
            //  };
            //foreach (int emp in eModelParametersForOutput)
           // FO.ParametersForOutput();
            foreach (int emp in mine.FOB.eModelParametersForOutput)
            {
                if (emp == theEmp)
                {
                    found = true;
                    break;
                }
            }
            return found;
        }

        #region Website directory faking
        private static string DataDirectoryName
        {
            get
            {
                return @"App_Data\WaterSim_6_0\";
            }
        }

        private static string TempDirectoryName
        {
            set
            {
                string dir = value;
                string.Concat(@"WaterSim_Output\", dir);
            }
            get
            {
                // Make a common for testing
                return @"WaterSim_Output\";
                // Make the temp directory name unique for each access to avoid client clashes
                //return +System.Guid.NewGuid().ToString() + @"\";
            }
        }
        private static void CreateDirectory(string directoryName)
        {
            System.IO.DirectoryInfo dir = new System.IO.DirectoryInfo(directoryName);
            if (!dir.Exists)
            {
                dir.Create();
            }
        }
        #endregion

    }
}
