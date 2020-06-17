using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using WaterSimDCDC.Documentation;


namespace WaterSimDCDC
{
    public partial class WaterSimManager
    {
       // -------------------------------------------------------------------------------------------------------------------------------------------
       //-------------------------------------------------------------
        //
        public const int mpAPIcleared = 147;
        /// <summary>
        /// Creates a Directory of directoryName if it does not exist.  Relative references is from the directory the program is executing from.
        /// This is primarily here as a demonstration of how the WaterSimDECDC_User_Mods file can be used to add user methods and parameters to\
        /// the WaterSimManager class.  The User can add their own methods and parameters using this file. 
        /// </summary>
        /// <param name="directoryName">Name of new or existing directory</param>
        public void CreateDirectory(string directoryName)
        {
            try
            {
                System.IO.DirectoryInfo dir = new System.IO.DirectoryInfo(directoryName);
                if (!dir.Exists)
                {
                    dir.Create();
                }
            }
            catch
            {
            }
        }
        //---------------------------------------------------------------------------
        //
        const string checkModel = "";
        // This routine is called by initialize_ModelParameters
        // This is how User Defined Model Parameters are added to WaterSimManager Class Parameter Manager
        //   ExtendDoc.Add(new WaterSimDescripItem(eModelParam., "Description", "Short Unit", "Long Unit", "", new string[] { }, new int[] { }, new ModelParameterGroupClass[] { }));

         partial void initialize_Other_ModelParameters()
        {
             // Use these to add documentation to any parameters you add
            //ParameterManagerClass FPM = ParamManager;
            //Extended_Parameter_Documentation ExtendDoc = FPM.Extended;
            //
            if (FortranDLL == "WaterSimDCDC_model_6.dll")
            {
                initialize_WaterSim_6_ModelParameters();
            }
        //    int drought = 2;
         //   this.ParamManager.Model_ParameterBaseClass(eModelParam.epProvider_DroughtScenarios).Value =drought;
           //----------------------------------------------------------------------------------------------------
        }
        /// <summary>
        /// Not yet used- but perhaps soon
        /// </summary>
         public bool API_Cleared
         {
             set { _ws.parmAPIcleared = value;}
             get { return _ws.parmAPIcleared;}
         }
         // stop
        // ---------------------------------------------------------------------------------------------------------
        //
    }
   
}
