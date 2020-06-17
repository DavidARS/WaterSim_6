using System;
using System.Collections.Generic;



namespace WaterSimDCDC
{

    public partial class WaterSimManager
    {
        //---------------------------------------------------------------------------
        // Define Constants for new parameters
        //public const int epCreditDeficits = 102;
        //public const int epWaterFromAgPumping = 103;
        //---------------------------------------------------------------------
        // This routine is called by initialize Groundwater Model Parameters
         partial void initialize_GWModelParameters()
        {
            for (int p = 0; p < ProviderClass.NumberOfProviders; p++)
            {
                AgGWtoMuniAccumulate[p] = 0;
            }
             // Begin Added 2/24/14 das rq
            Use_WaterFromAgPumping = new providerArrayProperty(_pm, eModelParam.epProvider_WaterFromAgPumping, get_WaterFromAgPumping, set_WaterFromAgPumping, eProviderAggregateMode.agSum);
             // QUAY EDIT 6/17/14 Changed max value to 100,000 from 50,000.  This was causing an error in the web interface routines
            //this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_WaterFromAgPumping, "Water to Muni from Ag Pumping", "AGTOMUN1", modelParamtype.mptInputProvider, rangeChecktype.rctCheckRange, 0, 300000, null, get_WaterFromAgPumping, null, set_WaterFromAgPumping, null, null, Use_WaterFromAgPumping));
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_WaterFromAgPumping, "Agricultural Pumping to Municipal", "AGTOMUN1", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 300000, null, get_WaterFromAgPumping, null, set_WaterFromAgPumping, null, null, Use_WaterFromAgPumping));

            //
            Use_WaterFromAgSurface = new providerArrayProperty(_pm, eModelParam.epProvider_WaterFromAgSurface, get_WaterFromAgSurface, set_WaterFromAgSurface, eProviderAggregateMode.agSum);
            // QUAY EDIT 6/17/14 Changed max value to 100,000 from 50,000.  This was causing an error in the web interface routines
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_WaterFromAgSurface, "Water to Muni from Ag Surface CAP", "AGTOMUN2", modelParamtype.mptInputProvider, rangeChecktype.rctCheckRange, 0, 10000, null, get_WaterFromAgSurface, null, set_WaterFromAgSurface, null, null, Use_WaterFromAgSurface));
            //
            WaterFromAgPumpingMax = new providerArrayProperty(_pm, eModelParam.epProvider_WaterFromAgPumpingMax, geti_WaterFromAgPumpingMax, null, eProviderAggregateMode.agSum);
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_WaterFromAgPumpingMax, "Water to Muni from Ag Pumping Max", "AGTOMPX", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 50000, null, geti_WaterFromAgPumpingMax, null, null, null, null, WaterFromAgPumpingMax));
            ////
            WaterFromAgSurfaceMax = new providerArrayProperty(_pm, eModelParam.epProvider_WaterFromAgSurfaceMax, geti_WaterFromAgSurfaceMax, null, eProviderAggregateMode.agSum);
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_WaterFromAgSurfaceMax, "Water to Muni from Ag Surface CAP Max", "AGTOMSX", modelParamtype.mptOutputProvider, rangeChecktype.rctCheckRange, 0, 50000, null, geti_WaterFromAgSurfaceMax, null, null, null, null, WaterFromAgSurfaceMax));
            // End Added 2/24/14 das rq

            // deleted das rq 2/24/14 AF_water_FromAgPumping = new providerArrayProperty(this.ParamManager, eModelParam.epProvider_WaterFromAgPumping, get_waterFromAgPumping, set_waterFromAgPumping, eProviderAggregateMode.agNone);
            // Modified QUAY 1/20/14 - Added this.ParamManager,  to call
            AF_water_CreditDeficits = new providerArrayProperty(this.ParamManager, eModelParam.epCreditDeficits, get_waterCreditDeficits, eProviderAggregateMode.agSum);
 
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epGroundwater_Model, "Groundwater Model", "GWMODEL", modelParamtype.mptInputBase, rangeChecktype.rctCheckRange, 0, 1, geti_Groundwater_Model, null, seti_Groundwater_Model, null, null, null, null));
            // deleted das rq 2/24/14  this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epProvider_WaterFromAgPumping, "Water From Ag Pumping(AF)", "WAGPUMP", modelParamtype.mptInputProvider, rangeChecktype.rctCheckRange, 0, 55, null, get_waterFromAgPumping, null, set_waterFromAgPumping, null, null, AF_water_FromAgPumping));
            //
 
            //
            // Outputs at the Provider level (array)
            //=================================
            this.ParamManager.AddParameter(new ModelParameterClass(eModelParam.epCreditDeficits, "Water Credit Deficits(AF)", "WDEFICIT", modelParamtype.mptOutputProvider, rangeChecktype.rctNoRangeCheck, -300000, 0, null, get_waterCreditDeficits, null, null, null, null, AF_water_CreditDeficits));

        }
        // =================================================
        public providerArrayProperty Available_Groundwater;
        private int[] get_Available_Groundwater() { return _ws.ModflowGroundwater; }

        private int geti_Basin_Pumpage(){ return _ws.get_BasinWidePumpage; }
        private int geti_Basin_NetAnnual(){ return _ws.get_BasinWideDeltaAnnual; }

        
        internal int geti_Groundwater_Model() { return Groundwater_Model; }
        internal void seti_Groundwater_Model(int value) { Groundwater_Model = value; }
       /// <summary>
       /// Property for getting and setting the groundwater model used; i.e., 0 = MODFLOW
       /// and 1 = simple bucket
       /// </summary>
        public int Groundwater_Model{ set {if ((!_inRun) & (!FModelLocked))
                {
                    this.ParamManager.CheckBaseValueRange(eModelParam.epGroundwater_Model, value);
                    _ws.GroundWaterModel = value;
                }
            }
            get { return _ws.GroundWaterModel; }    // 
        }
        //-----------------------------------------------------
 
        // Inputs
        int[] AgGWtoMuniAccumulate = new int[ProviderClass.NumberOfProviders];
        // Begin Added  01.08.14 das rq
        // Use_WaterFromAgPumping 
        ProviderIntArray Aggregate = new ProviderIntArray(0);
        internal int[] get_WaterFromAgPumping()
        {
            return _ws.WaterFromAgPumping;
            //for (int p = 0; p < ProviderClass.NumberOfProviders; p++)
            //{
            //    Aggregate.FValues[p]+= _ws.WaterFromAgPumping[p];
            //    //AgGWtoMuniAccumulate[p] += _ws.WaterFromAgPumping[p];
            //}
            //Aggregate.Values = AgGWtoMuniAccumulate;

           // return Aggregate.Values;
            } //  
        internal void set_WaterFromAgPumping(int[] value)
        {   if (!FModelLocked)   {  _ws.WaterFromAgPumping = value;
            }
        }
        /// <summary> The amount of water to divert from Ag groundwater pumping  </summary>
        /// <remarks> Acre-feet annum-1. Range = 0 to 100000</remarks> 
        /// <exception cref="WaterSim_Exception">NOT SET</exception>
        public providerArrayProperty Use_WaterFromAgPumping;
        //
        //-----------------------------------------------------
         internal int[] get_WaterFromAgSurface()
        { return _ws.WaterFromAgSurface; } //  
        internal void set_WaterFromAgSurface(int[] value)
        {
            if (!FModelLocked)
            {
                _ws.WaterFromAgSurface = value;
            }
        }
        public providerArrayProperty Use_WaterFromAgSurface;
        /// <summary> The amount of water to divert from Ag groundwater pumping  </summary>
        /// <remarks> Acre-feet annum-1. Range = 0 to 100000</remarks> 
        /// <exception cref="WaterSim_Exception">NOT SET</exception>

        public providerArrayProperty WaterFromAgSurfaceMax;

        internal int[] geti_WaterFromAgSurfaceMax()
        { return _ws.get_WaterFromAgSurfaceMax; } //  

        public providerArrayProperty WaterFromAgPumpingMax;

        internal int[] geti_WaterFromAgPumpingMax()
        { return _ws.get_WaterFromAgPumpingMax; } //  

        // end Added  01.08.14 das  rq
        //-----------------------------------------------
        // Begin Deleted 01.08.14 das  rq
        //public providerArrayProperty AF_water_FromAgPumping;
        //internal int[] get_waterFromAgPumping() { return _ws.WaterFromAgPumping; }
        //internal void set_waterFromAgPumping(int[] value) { if (!FModelLocked)  _ws.WaterFromAgPumping = value; }
        // Outputs
        // End Deleted 01.08.14 das  rq
        //-----------------------------------------------
  
        // Credit model total credit balance deficits
        public providerArrayProperty AF_water_CreditDeficits;
        internal int[] get_waterCreditDeficits() { return _ws.get_WaterCreditDeficits; }

    }
   
}
