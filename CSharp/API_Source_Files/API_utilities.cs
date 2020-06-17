//WaterSimDCDC Regional Water Demand and Supply Model Version 5.0

//       This is the C# Utilities file for the C# interface to the WaterSim_DCDC FORTRAN dll.

//       Copyright (C) 2011 , The Arizona Board of Regents
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
// =====================================================================================
using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;       // for DllImport
//using WaterSim;
//CHANGED QUAY was WaterSim
namespace WaterSimDCDC
{
    public static class API
    {
        public const int numModeledProviders = 33;
        const string FileBuildAPIutilities = "07.30.15_11:21:00";
        //
        #region providerNamedConstants
      public const int ad = (int)providers.Adaman_Mutual;
      public const int wt = (int)providers.White_Tanks;
      public const int pv = (int)providers.Paradise_Valley;
      public const int su = (int)providers.Sun_City;
      public const int sw = (int)providers.Sun_City_West;
      public const int av = (int)providers.Avondale;
      public const int be = (int)providers.Berneil;
      public const int bu = (int)providers.Buckeye;
      public const int cf = (int)providers.Carefree;
      public const int cc = (int)providers.Cave_Creek;
      public const int ch = (int)providers.Chandler;
      public const int cp = (int)providers.Chaparral_City;
      public const int sp = (int)providers.Surprise;
      public const int cu = (int)providers.Clearwater_Utilities;
      public const int dh = (int)providers.Desert_Hills;
      public const int em = (int)providers.El_Mirage;
      public const int gi = (int)providers.Gilbert;
      public const int gl = (int)providers.Glendale;
      public const int go = (int)providers.Goodyear;
      public const int lp = (int)providers.Litchfield_Park;
      public const int me = (int)providers.Mesa;
      public const int pe = (int)providers.Peoria;
      public const int ph = (int)providers.Phoenix;
      public const int qk = (int)providers.Queen_Creek;
      public const int rg = (int)providers.Rigby;
      public const int rv = (int)providers.Rio_Verde;
      public const int ry = (int)providers.Rose_Valley;
      public const int sc = (int)providers.Scottsdale;
      public const int sr = (int)providers.Sunrise;
      public const int te = (int)providers.Tempe;
      public const int to = (int)providers.Tolleson;
      public const int vu = (int)providers.Valley_Utilities;
      public const int we = (int)providers.West_End;
        //
        #endregion
        //
        #region parmsInit

        public static int[] set_parmWWtoRWWTPpct    = new int[numModeledProviders] {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
        public static int[] set_parmReclaimedWWtoROpct      = new int[numModeledProviders] {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
        public static int[] set_parmWWtoEffluentPct = new int[numModeledProviders] {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};

        public static int[] set_parmEffluentToVadosePct= new int[numModeledProviders] {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
        public static int[] set_parmEffluentToPowerPlantPct = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};

        public static int[] set_parmReclaimedToVadosePct = new int[numModeledProviders] {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
        public static int[] set_parmReclaimedToDirectInjectPct =  new int[numModeledProviders] {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
        public static int[] set_parmReclaimedToOutputPct    = new int[numModeledProviders] {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
        public static int[] set_parmReclaimedToInputPct = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };

        public static int[] set_parmROReclaimedToOutputPct  = new int[numModeledProviders] {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};

        public static int[] set_parmSurfaceWaterToWbankPct = new int[numModeledProviders] {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
        public static int[] set_parmSurfaceWaterToWbankAmt = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
        public static int[] set_parmSurfaceWaterToVadoseAmt = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
        public static int[] set_parmWaterSupplyToDirectInjectAmt = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};

        // 01.24.13
        //public static int[] set_parmOutdoorWaterUsePct = new int[numModeledProviders] {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
        public static int[] set_parmReclaimedOutdoorUsePct = new int[numModeledProviders] {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
        public static int[] set_parmOutdoorWaterUseResPct = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
        public static int[] set_parmOutdoorWaterUseComPct = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
        public static int[] set_parmOutdoorWaterUseIndPct = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };

        //
        public static int[] set_parmWaterSupplyToResPct = new int[numModeledProviders] {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
        public static int[] set_parmWaterSupplyToComPct = new int[numModeledProviders] {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
        public static int[] set_parmWaterSupplyToIndPct = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };

        public static int[] set_parmGroundwaterToGWTPlantPct = new int[numModeledProviders] {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
        // New since passing to Ray - 07.07.11 das
        public static bool set_parmIncludeMeteo = new bool();
        public static int[] set_TimeLagVadoseYears = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,-1};
        public static int[] set_ProviderGPCD = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
        public static int[] set_ProviderPopGrowthRateAdjustPct = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
        // 05.25.12 DAS
        public static int[] set_ProviderPopGrowthRateOnProjectPct = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
        public static int[] set_ProviderPopGrowthRateOtherPct = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
  

        public static int[] set_WaterBankingOption = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
        // DAS
        public static int[] set_PopulationsOn = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
        public static int[] set_PopulationsOther = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };

        public static int[] modflowTomodel = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
        public static int[] set_ModelGroundwater = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };

        public static int[] modifyNormalFlow = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
        //
        public static int[] set_WaterFromAgPumping = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
        public static int[] set_WaterFromAgSurface = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
         //
        public static int[] set_parmRateResLeakagePct = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
        public static int[] set_parmBlackWaterPct = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
        // 02.21.13
        public static int[] set_newWaterSupplies = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
        // 08.16.13
        public static int[] set_annualGWCredits = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };

        public static int[] ProviderAlterGPCDPCT = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };

        public static int[] ProviderMaxNormalFlow = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };

        public static int[] set_waterToAgriculture_AF = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };

        public static int[] set_ProviderAgCreditCurveIndex = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };

        public static int[] set_providerFlowToCOdelta = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };

        public static int[] set_parmDefaultPumpingMandIPct = new int[numModeledProviders] { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };

        #endregion
        //
        #region outputs

        #endregion
        //
        // CHANGED QUAY was WaterSim.WaterSimU _ws
        public static void parms(WaterSimU _ws)
        {
            //
            _ws.parmWWtoRWWTPpct = set_parmWWtoRWWTPpct;
            _ws.parmReclaimedWWtoROpct = set_parmReclaimedWWtoROpct;
            _ws.parmWWtoEffluentPct = set_parmWWtoEffluentPct;
            _ws.parmReclaimedToInputPct = set_parmReclaimedToInputPct;

            _ws.parmROReclaimedToOutputPct = set_parmROReclaimedToOutputPct;
            _ws.parmSurfaceWaterToWbankPct = set_parmSurfaceWaterToWbankPct;
            _ws.parmSurfaceWaterToWBankAmt = set_parmSurfaceWaterToWbankAmt;
            //
            _ws.parmSurfaceWaterToVadoseAmt = set_parmSurfaceWaterToVadoseAmt;
            _ws.parmWaterSupplyToDirectInjectAmt = set_parmWaterSupplyToDirectInjectAmt;
            //
            //_ws.get_parmOutdoorWaterUsePct      = set_parmOutdoorWaterUsePct;

            _ws.parmOutdoorWaterUseResPct = set_parmOutdoorWaterUseResPct;
            _ws.parmOutdoorWaterUseComPct = set_parmOutdoorWaterUseComPct;
            _ws.parmOutdoorWaterUseIndPct = set_parmOutdoorWaterUseIndPct;
            //
            _ws.parmReclaimedOutdoorUsePct  = set_parmReclaimedOutdoorUsePct;
            _ws.parmGroundwaterToGWTPlantPct = set_parmGroundwaterToGWTPlantPct;
            //
            // Other Provider variables
            _ws.set_parmIncludeMeteorology = set_parmIncludeMeteo;
            _ws.TimeLagVadoseYears = set_TimeLagVadoseYears;
            //_ws.ProviderGPCD = set_ProviderGPCD;
            _ws.ProviderGPCD = set_ProviderGPCD;
            // County-scale
            _ws.ProviderPopGrowthRateAdjustPct = set_ProviderPopGrowthRateAdjustPct;
            // 05.25.12 DAS
            _ws.ProviderPopGrowthRateOnProjectPct = set_ProviderPopGrowthRateOnProjectPct;
            _ws.ProviderPopGrowthRateOtherPct = set_ProviderPopGrowthRateOtherPct;

            _ws.WaterBankingOption = set_WaterBankingOption;
            // DAS
            _ws.set_PopulationsOn = set_PopulationsOn;
            _ws.set_PopulationsOther = set_PopulationsOther;

            _ws.ModifyProviderNormalFlowPct = modifyNormalFlow ;
            _ws.WaterFromAgPumping = set_WaterFromAgPumping;
            _ws.WaterFromAgSurface = set_WaterFromAgSurface;
            //
            _ws.set_ProviderAllowanceGWcredits = set_annualGWCredits;
                // Ajay and Johnston Project
            _ws.parmRateResLeakagePct = set_parmRateResLeakagePct;
            _ws.parmBlackWaterResPct = set_parmBlackWaterPct;
            //
             // Other CityModel or Legacy variables
            _ws.DemandPercent = 100;
            //
            _ws.NewWaterSupplies = set_newWaterSupplies;
            //
            _ws.AlterProviderGPCDpct = ProviderAlterGPCDPCT;
            // 12.09.13
            _ws.NormalFlow_rights_max = ProviderMaxNormalFlow;

            _ws.WaterToAgriculture_AF = set_waterToAgriculture_AF;
            //
            _ws.set_ProviderAgCAPandPumpingCurveIndex = set_ProviderAgCreditCurveIndex;
            //
            _ws.set_ProviderFlowToCOdelta = set_providerFlowToCOdelta;
            //
            _ws.parmDefaultPumpingMandIPct = set_parmDefaultPumpingMandIPct;
            //
            RangeCheckEffluent(_ws);
            RangeCheckReclaimed(_ws);
            RangeCheckWaterSupply(_ws);
            //
            // 02.02.12 DAS
            // Commented out on 04.26.12- no longer using MODFLOW estimates in the model (at this time)
            //double[] Groundwater = new double[numModeledProviders];
            //Groundwater = WaterSim.initialGW.initMODFLOWGroundwater(_ws.Startyear, numModeledProviders, _ws.DataDirectoryName, _ws.TempDirectoryName);
            // Pass Initial groundwater estimates into the fortran model (done 
            // in the API call using the property "ModelGroundwater"
            for (int i = 0; i < numModeledProviders; ++i)
            {
                //API.modflowTomodel[i] = 0; // Convert.ToInt32(Groundwater[i]);
            }
            //_ws.ModelGroundwater = modflowTomodel;
            _ws.ModelGroundwater = set_ModelGroundwater;
 
 
        }
        //
    
        #region RangeCheck
        // CHANGED QUAY was WaterSim.WaterSimU _ws

        public static void RangeCheckReclaimed(WaterSimU _ws)
        {
            string tag = "Reclaimed parameters-";
            // CHANGED QUAY was WaterSim.WaterSimU _ws

            int mydefault=WaterSimU.ReclaimedToOutputPct;
            int ReclaimedToOutput;
            for (int num = 0; num < numModeledProviders; ++num)
            {
                ReclaimedToOutput=set_parmReclaimedToOutputPct[num];
                RangeCheckThree_oneDefault(num, set_parmReclaimedToOutputPct,set_parmReclaimedToDirectInjectPct,
                   set_parmReclaimedToVadosePct, ReclaimedToOutput,mydefault, tag);
                //
            }
                _ws.parmReclaimedToOutputPct = set_parmReclaimedToOutputPct;
                _ws.parmReclaimedToDirectInjectPct = set_parmReclaimedToDirectInjectPct;
                _ws.parmReclaimedToVadosePct = set_parmReclaimedToVadosePct;
        }
        //
        // CHANGED QUAY was WaterSim.WaterSimU _ws
         public static void RangeCheckEffluent(WaterSimU _ws)
        {
            // default variable must be placed in the first position
            string tag = "Effluent parameters-";
            int pdefault = 0;
            for (int num = 0; num < numModeledProviders; ++num)
            {
                RangeCheckTwo(num,set_parmEffluentToVadosePct,set_parmEffluentToPowerPlantPct, pdefault, tag);
                //
            }
                _ws.parmEffluentToVadosePct = set_parmEffluentToVadosePct;
                _ws.parmEffluentToPowerPlantPct = set_parmEffluentToPowerPlantPct;
        }
        //
         // CHANGED QUAY was WaterSim.WaterSimU _ws

        public static void RangeCheckWaterSupply(WaterSimU _ws)
        {
                ProviderIntArray One = new ProviderIntArray(0);
            // default variable must be placed in the first and second position
            string tag = "Water Supply to various: parameters-";
            int pdefault1 = 70;
            int pdefault2 = 20;
            int[] Res = new int[numModeledProviders];
            int[] Com = new int [numModeledProviders];
            int[] Ind = new int[numModeledProviders];
            for (int num = 0; num < numModeledProviders; ++num)
            {
                RangeCheckTwo_twoDefaults(num, set_parmWaterSupplyToResPct,
                    set_parmWaterSupplyToComPct , pdefault1, pdefault2, tag);
                //
            }
            for (int i = 0; i < numModeledProviders; i++)
            {
                Res[i]=set_parmWaterSupplyToResPct[i] ;
                Com[i]=set_parmWaterSupplyToComPct[i];
                Ind[i]=set_parmWaterSupplyToIndPct[i];
            }
           for(int j = 0; j < numModeledProviders; j++)
           {
               _ws.parmWaterSupplyToResPct[j]=Res[j];
               _ws.parmWaterSupplyToComPct[j] = Com[j];
               _ws.parmWaterSupplyToIndPct[j] = Ind[j];
           }
          
            
        }

        //
        public static void RangeCheckTwo(int iProv, int[] one, int[] two, int mydefault, string tag)
        {
            // default has been changed
            if (-1 < one[iProv])
            {
                if (-1 < two[iProv])
                {
                    if (100 < one[iProv] + two[iProv])
                    {
                        throw new ArgumentException(tag + "Values out of range");
                    }

                }
                else
                {
                    if (100 < ((one[iProv] + two[iProv]) + 1))
                    {
                        throw new ArgumentException(tag + "Values out of range");
                    }
                }

            }
            // using default 
            {
                if (100 - mydefault < two[iProv] )
                {
                    throw new ArgumentException(tag + "  Values out of range for: " + iProv);
                }
            }
        }
        public static void RangeCheckThree_oneDefault(int iProv, int[] one, int[] two, int[] three, int parmSetInRunInit, int WaterSimUdefault,string tag)
        {
            // output=-1, direct inject=2, vadose=3
            // default variable has been changed

                if (-1 != one[iProv])
                {
                    if (0 == one[iProv])
                    {
                        if (-1 < two[iProv])
                        {
                            if (-1 < three[iProv])
                            {
                                if (100 < two[iProv] + three[iProv])
                                {
                                    throw new ArgumentException("A " + tag + "  Values out of range for: " + iProv);
                                }
                            }
                            else
                            {
                                if (100 < two[iProv])
                                {
                                    throw new ArgumentException("A1 " + tag + "  Values out of range for: " + iProv);
                                }

                            }
                        }
                        else
                        {
                            if (100 < three[iProv])
                            {
                                throw new ArgumentException("A2 " + tag + "  Values out of range for: " + iProv);
                            }

                        }

                    }
                    else
                    {
                        if (-1 < two[iProv])
                        {
                            if (-1 < three[iProv])
                            {
                                if (100 < (parmSetInRunInit + two[iProv] + three[iProv]))
                                {
                                    throw new ArgumentException("B " + tag + "  Values out of range for: " + iProv);
                                }
                            }
                        }
                        else
                        {
                            if (-1 < three[iProv])
                            {
                                if (100 < (parmSetInRunInit + three[iProv]))
                                {
                                    throw new ArgumentException("B1 " + tag + "  Values out of range for: " + iProv);
                                }
                            }
                            else
                            {
                                if (100 < one[iProv]) //
                                {
                                    throw new ArgumentException("B2 " + tag + "  Values out of range for: " + iProv);
                                }

                            }
                        }
                    }
                }
                // one is equal to default
                else
                {
                    if (-1 < two[iProv])
                    {
                        if (-1 < three[iProv])
                        {
                            if (100 < (WaterSimUdefault + two[iProv] + three[iProv]))
                            {
                                throw new ArgumentException("C1 " + tag + "  Values out of range for: " + iProv);
                            }
                        }
                        else
                        {
                            if (100 < (WaterSimUdefault + two[iProv]))
                            {
                                throw new ArgumentException("C2 " + tag + "  Values out of range for: " + iProv);
                            }

                        }
                    }
                    else
                    {
                        if (-1 < three[iProv])
                        {
                            if (100 < (WaterSimUdefault + three[iProv]))
                            {
                                throw new ArgumentException("C3 " + tag + "  Values out of range for: " + iProv);
                            }
                        }
                    }


                }
                // 
                if (-1 != three[iProv])
                {
                    if (one[iProv] == 0 && two[iProv] == 0)
                    {
                        if (100 < three[iProv])
                        {
                            throw new ArgumentException("E " + tag + "  Values out of range for: " + iProv);
                        }
                    }
                }
                if (-1 < one[iProv] && -1 < two[iProv])
                {
                    if (100 < (one[iProv] + two[iProv]))
                    {
                        throw new ArgumentException("G " + tag + "  Values out of range for: " + iProv);
                    }
                }

                //
            //}
            //else
            //{

            //}
        }        //
        public static void RangeCheckTwo_twoDefaults(int iProv, int[] one, int[] two, int mydefault1,int mydefault2, string tag)
        {
            // default variable has been changed
            if (-1 < one[iProv] || -1 < two[iProv])
            {
                if (-1 < one[iProv] && -1 < two[iProv])
                {
                    if (100 < (one[iProv] + two[iProv] ) )
                    {
                        throw new ArgumentException(tag + "Values out of range");
                    }

                }
                if(-1 < one[iProv] )
                {
                    if (-1 == two[iProv])
                    {
                        if (100 < (one[iProv] + mydefault2 ) )
                        {
                            throw new ArgumentException(tag + "Values out of range");
                        }
                    }
                }
                if (-1 < two[iProv])
                {
                    if (100 < (mydefault1 + two[iProv] ) )
                    {
                        throw new ArgumentException(tag + "Values out of range");
                    }
                }
            }
            // using default variable
            else
            {
                if (100 < ((mydefault1+mydefault2)+0) )
                {
                    throw new ArgumentException(tag + "  Values out of range for: " + iProv);
                }
            }
        }

        #endregion
        //
        public enum providers
        {
            Adaman_Mutual,
            White_Tanks,
            Paradise_Valley,
            Sun_City,
            Sun_City_West,
            Avondale,
            Berneil,
            Buckeye,
            Carefree,
            Cave_Creek,
            Chandler,
            Chaparral_City,
            Surprise,
            Clearwater_Utilities,
            Desert_Hills,
            El_Mirage,
            Gilbert,
            Glendale,
            Goodyear,
            Litchfield_Park,
            Mesa,
            Peoria,
            Phoenix,
            Queen_Creek,
            Rigby,
            Rio_Verde,
            Rose_Valley,
            Scottsdale,
            Sunrise,
            Tempe,
            Tolleson,
            Valley_Utilities,
            West_End
        };

    }
}