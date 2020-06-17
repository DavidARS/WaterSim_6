 // ===========================================================
//     WaterSimDCDC Regional Water Demand and Supply Model Version 5.0

//       A method that initializes the initial state of Provider Parameters

//       WaterSimDCDC_API_Provider_Default_parms 
//       Version 1
//       Keeper Ray Quay  ray.quay@asu.edu
//       
//       NOTE This will be replaced eventually with a database.
//       
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
//      Current Date: 07.28.15, 10.09.15
//====================================================================================

using System;

namespace WaterSimDCDC
{
     public partial class WaterSimManager
    {
         // Added RQ 12/29/14 to save defaults for later use
         internal int[] Default_Wastewater_Reclaimed = null;
         // Added DAS 10.04.15 to save defaults for later use (webInterface.cs)
         internal int[] Default_Banking = null;
          //
         ProviderDoubleArray SlopeGPCD = new ProviderDoubleArray(0);
         ProviderDoubleArray InterceptGPCD = new ProviderDoubleArray(0);
         ProviderIntArray defaultTargetGPCD = new ProviderIntArray(0);
         ProviderIntArray defaultMinGPCD = new ProviderIntArray(0);
         ProviderIntArray responseGPCD = new ProviderIntArray(0);
         ProviderIntArray modSlope = new ProviderIntArray(0);          
         //
         public ProviderDoubleDoubleArray GPCD = new ProviderDoubleDoubleArray(0, 0);
         public ProviderDoubleDoubleArray GPCDraw = new ProviderDoubleDoubleArray(0, 0);
         //
         const string FileBuildDefault = "09.30.14_22:21:00";
         //
     partial void initialize_Provider_Default_ModelParameters()   
        {

            ProviderIntArray temp = new ProviderIntArray(0);
        
         // Where did these data come from ?
            double [] DepthToGroundwater = new double[ProviderClass.NumberOfProviders] {
                300             	, //0 ad :       Adaman_Mutual,
                200             	, //1 wt :        White_Tanks,
                350             	, //2 pv :        Paradise_Valley,
                400             	, //3 su :        Sun_City,
                450             	, //4 sw :        Sun_City_West,
                200             	, //5 av :        Avondale,
                200             	, //6 be :        Berneil,
                150             	, //7 bu :        Buckeye,
                500             	, //8 cf :        Carefree,
                500             	, //9 cc :        Cave_Creek,
                200             	, //10 ch :        Chandler,
                500             	, //11 cp :        Chaparral_City,
                400             	, //12 sp :        Surprise,
                100             	, //13 cu :        Clearwater_Utilities,
                500             	, //14 dh :        Desert_Hills,
                350             	, //15 em :        El_Mirage,
                200             	, //16 gi :        Gilbert,
                200             	, //17 gl :        Glendale,
                100             	, //18 go :        Goodyear,
                200             	, //19 lp :        Litchfield_Park,
                250             	, //20 me :        Mesa,
                200   	, //23 pe :        Peoria,
                300   	, //24 ph :        Phoenix,
                400   	, //25 qk :        Queen_Creek,
                100   	, //26 rg :        Rigby,
                500   	, //27 rv :        Rio_Verde,
                400   	, //28 ry :        Rose_Valley,
                300   	, //29 sc :        Scottsdale,
                400   	, //30 sr :        Sunrise,
                125   	, //31 te :        Tempe,
                200   	, //32 to :        Tolleson,
                300   	, //33 vu :        Valley_Utilities,
                450   	  //34 we :        West_End,
                   };
             //
         // Converting depth to years
            double[] years = new double[ProviderClass.NumberOfProviders];
            int[] year = new int[ProviderClass.NumberOfProviders];
            for (int i = 0; i < ProviderClass.NumberOfProviders; i++)
            {
                years[i] = DepthToGroundwater[i] * 0.075;
                year[i] = Convert.ToInt32(years[i]);
            }

            temp.Values = year;
            Surface_to_Vadose_Time_Lag.setvalues(temp);
            // Wastewater to Reclaimed

            int[] WWtoReclaim = new int[ProviderClass.NumberOfProviders] {
                    0	, //0 ad :       Adaman_Mutual,
                    0	, //1 wt :        White_Tanks,
                    0	, //2 pv :        Paradise_Valley,
                    0	, //3 su :        Sun_City,
                    0	, //4 sw :        Sun_City_West,
                    0	, //5 av :        Avondale,
                    0	, //6 be :        Berneil,
                    33	, //7 bu :        Buckeye,
                    100	, //8 cf :        Carefree,
                    0	, //9 cc :        Cave_Creek,
                    0	, //10 ch :        Chandler,
                    100	, //11 cp :        Chaparral_City,
                    83	, //12 sp :        Surprise,
                    0	, //13 cu :        Clearwater_Utilities,
                    0	, //14 dh :        Desert_Hills,
                    0	, //15 em :        El_Mirage,
                    50	, //16 gi :        Gilbert,
                    0	, //17 gl :        Glendale,
                    0	, //18 go :        Goodyear,
                    100	, //19 lp :        Litchfield_Park,
                    2	, //20 me :        Mesa,
                    3	, //23 pe :        Peoria,
                    2	, //24 ph :        Phoenix,
                    0	, //25 qk :        Queen_Creek,
                    0	, //26 rg :        Rigby,
                    100	, //27 rv :        Rio_Verde,
                    0	, //28 ry :        Rose_Valley,
                    16	, //29 sc :        Scottsdale,
                    0	, //30 sr :        Sunrise,
                    6	, //31 te :        Tempe,
                    0	, //32 to :        Tolleson,
                    0	, //33 vu :        Valley_Utilities,
                    0	  //34 we :        West_End,
            
                   };

            temp.Values = WWtoReclaim;
            PCT_Wastewater_Reclaimed.setvalues(temp);
            Default_Wastewater_Reclaimed = WWtoReclaim;


            // Reclaimed to Water Supply
            // Modified Quay 12/29/14 changed 0 to 100
            int[] ReclaimtoSupply = new int[ProviderClass.NumberOfProviders] {
                100, //0	, //0 ad :       Adaman_Mutual,
                100, //0	, //1 wt :        White_Tanks,
                100, //0	, //2 pv :        Paradise_Valley,
                100, //0	, //3 su :        Sun_City,
                100, //0	, //4 sw :        Sun_City_West,
                100, //0	, //5 av :        Avondale,
                100, //0	, //6 be :        Berneil,
                100	, //7 bu :        Buckeye,
                100	, //8 cf :        Carefree,
                100, //0	, //9 cc :        Cave_Creek,
                100, //0	, //10 ch :        Chandler,
                100	, //11 cp :        Chaparral_City,
                100	, //12 sp :        Surprise,
                100, //0	, //13 cu :        Clearwater_Utilities,
                100, //0	, //14 dh :        Desert_Hills,
                100, //0	, //15 em :        El_Mirage,
                100, //0	, //16 gi :        Gilbert,
                100, //0	, //17 gl :        Glendale,
                100, //0	, //18 go :        Goodyear,
                100	, //19 lp :        Litchfield_Park,
                100	, //20 me :        Mesa,
                //0	, //21 no :       No provider
                //0	, //22 op :       Other Provider
                100	, //23 pe :        Peoria,
                100	, //24 ph :        Phoenix,
                100, //0	, //25 qk :        Queen_Creek,
                100, //0	, //26 rg :        Rigby,
                100	, //27 rv :        Rio_Verde,
                100, //0	, //28 ry :        Rose_Valley,
                100, //0	, //29 sc :        Scottsdale,
                100, //0	, //30 sr :        Sunrise,
                100	, //31 te :        Tempe,
                100, //0	, //32 to :        Tolleson,
                100, //0	, //33 vu :        Valley_Utilities,
                100 //0	 //34 we :        West_End,

            };

            temp.Values = ReclaimtoSupply;
            PCT_Reclaimed_to_Water_Supply.setvalues(temp);
          
            //
            // Reclaimed Water to Vadose

            int[] ReclaimtoVadose = new int[ProviderClass.NumberOfProviders] {
                0	, //0 ad :       Adaman_Mutual,
                0	, //1 wt :        White_Tanks,
                0	, //2 pv :        Paradise_Valley,
                0	, //3 su :        Sun_City,
                0	, //4 sw :        Sun_City_West,
                0	, //5 av :        Avondale,
                0	, //6 be :        Berneil,
                0	, //7 bu :        Buckeye,
                0	, //8 cf :        Carefree,
                0	, //9 cc :        Cave_Creek,
                0	, //10 ch :        Chandler,
                0	, //11 cp :        Chaparral_City,
                0	, //12 sp :        Surprise,
                0	, //13 cu :        Clearwater_Utilities,
                0	, //14 dh :        Desert_Hills,
                0	, //15 em :        El_Mirage,
                0	, //16 gi :        Gilbert,
                0	, //17 gl :        Glendale,
                0	, //18 go :        Goodyear,
                0	, //19 lp :        Litchfield_Park,
                0	, //20 me :        Mesa,
                //0	, //21 no :       No provider
                //0	, //22 op :       Other Provider
                0	, //23 pe :        Peoria,
                0	, //24 ph :        Phoenix,
                0	, //25 qk :        Queen_Creek,
                0	, //26 rg :        Rigby,
                0	, //27 rv :        Rio_Verde,
                0	, //28 ry :        Rose_Valley,
                0	, //29 sc :        Scottsdale,
                0	, //30 sr :        Sunrise,
                0	, //31 te :        Tempe,
                0	, //32 to :        Tolleson,
                0	, //33 vu :        Valley_Utilities,
                0	 //34 we :        West_End,

            };

            temp.Values = ReclaimtoVadose;
            PCT_Reclaimed_to_Vadose.setvalues(temp);
            //
            // Reclaimed Water to Vadose

            int[] ReclaimtoDI = new int[ProviderClass.NumberOfProviders] {
                0	, //0 ad :       Adaman_Mutual,
                0	, //1 wt :        White_Tanks,
                0	, //2 pv :        Paradise_Valley,
                0	, //3 su :        Sun_City,
                0	, //4 sw :        Sun_City_West,
                0	, //5 av :        Avondale,
                0	, //6 be :        Berneil,
                0	, //7 bu :        Buckeye,
                0	, //8 cf :        Carefree,
                0	, //9 cc :        Cave_Creek,
                0	, //10 ch :        Chandler,
                0	, //11 cp :        Chaparral_City,
                0	, //12 sp :        Surprise,
                0	, //13 cu :        Clearwater_Utilities,
                0	, //14 dh :        Desert_Hills,
                0	, //15 em :        El_Mirage,
                0	, //16 gi :        Gilbert,
                0	, //17 gl :        Glendale,
                0	, //18 go :        Goodyear,
                0	, //19 lp :        Litchfield_Park,
                0	, //20 me :        Mesa,
                //0	, //21 no :       No provider
                //0	, //22 op :       Other Provider
                0	, //23 pe :        Peoria,
                0	, //24 ph :        Phoenix,
                0	, //25 qk :        Queen_Creek,
                0	, //26 rg :        Rigby,
                0	, //27 rv :        Rio_Verde,
                0	, //28 ry :        Rose_Valley,
                0	, //29 sc :        Scottsdale,
                0	, //30 sr :        Sunrise,
                0	, //31 te :        Tempe,
                0	, //32 to :        Tolleson,
                0	, //33 vu :        Valley_Utilities,
                0	 //34 we :        West_End,

            };

            temp.Values = ReclaimtoDI;
            PCT_Reclaimed_to_DirectInject.setvalues(temp);
            //

            // Reclaimed to Reverse Osmosis

            int[] ReclaimtoRO = new int[ProviderClass.NumberOfProviders] {

                        0	, //0 ad :       Adaman_Mutual,
                        0	, //1 wt :        White_Tanks,
                        0	, //2 pv :        Paradise_Valley,
                        0	, //3 su :        Sun_City,
                        0	, //4 sw :        Sun_City_West,
                        0	, //5 av :        Avondale,
                        0	, //6 be :        Berneil,
                        0	, //7 bu :        Buckeye,
                        0	, //8 cf :        Carefree,
                        0	, //9 cc :        Cave_Creek,
                        0	, //10 ch :        Chandler,
                        0	, //11 cp :        Chaparral_City,
                        0	, //12 sp :        Surprise,
                        0	, //13 cu :        Clearwater_Utilities,
                        0	, //14 dh :        Desert_Hills,
                        0	, //15 em :        El_Mirage,
                        0	, //16 gi :        Gilbert,
                        0	, //17 gl :        Glendale,
                        0	, //18 go :        Goodyear,
                        0	, //19 lp :        Litchfield_Park,
                        0	, //20 me :        Mesa,
                        //0	, //21 no :       No provider
                        //0	, //22 op :       Other Provider
                        0	, //23 pe :        Peoria,
                        0	, //24 ph :        Phoenix,
                        0	, //25 qk :        Queen_Creek,
                        0	, //26 rg :        Rigby,
                        0	, //27 rv :        Rio_Verde,
                        0	, //28 ry :        Rose_Valley,
                        100	, //29 sc :        Scottsdale,
                        0	, //30 sr :        Sunrise,
                        0	, //31 te :        Tempe,
                        0	, //32 to :        Tolleson,
                        0	, //33 vu :        Valley_Utilities,
                        0	  //34 we :        West_End,
                };

            temp.Values = ReclaimtoRO;
            PCT_Reclaimed_to_RO.setvalues(temp);

            // Reverse Osmosis to Water Supply

            int[] ROtoSupply = new int[ProviderClass.NumberOfProviders] {

                        0	, //0 ad :       Adaman_Mutual,
                        0	, //1 wt :        White_Tanks,
                        0	, //2 pv :        Paradise_Valley,
                        0	, //3 su :        Sun_City,
                        0	, //4 sw :        Sun_City_West,
                        0	, //5 av :        Avondale,
                        0	, //6 be :        Berneil,
                        0	, //7 bu :        Buckeye,
                        0	, //8 cf :        Carefree,
                        0	, //9 cc :        Cave_Creek,
                        0	, //10 ch :        Chandler,
                        0	, //11 cp :        Chaparral_City,
                        0	, //12 sp :        Surprise,
                        0	, //13 cu :        Clearwater_Utilities,
                        0	, //14 dh :        Desert_Hills,
                        0	, //15 em :        El_Mirage,
                        0	, //16 gi :        Gilbert,
                        0	, //17 gl :        Glendale,
                        0	, //18 go :        Goodyear,
                        0	, //19 lp :        Litchfield_Park,
                        0	, //20 me :        Mesa,
                        //0	, //21 no :       No provider
                        //0	, //22 op :       Other Provider
                        0	, //23 pe :        Peoria,
                        0	, //24 ph :        Phoenix,
                        0	, //25 qk :        Queen_Creek,
                        0	, //26 rg :        Rigby,
                        0	, //27 rv :        Rio_Verde,
                        0	, //28 ry :        Rose_Valley,
                        100	, //29 sc :        Scottsdale,
                        0	, //30 sr :        Sunrise,
                        0	, //31 te :        Tempe,
                        0	, //32 to :        Tolleson,
                        0	, //33 vu :        Valley_Utilities,
                        0	  //34 we :        West_End,
                };

            temp.Values = ROtoSupply;
            PCT_RO_to_Water_Supply.setvalues(temp);

            // Max Reclaimed to demand
            // QUAY EDIT 1/4/15 changed 25 to 45 for AMS Demo
            // 
            int[] ReclaimedDemand = new int[ProviderClass.NumberOfProviders] {

                        45	, //0 ad :       Adaman_Mutual,
                        45	, //1 wt :        White_Tanks,
                        45	, //2 pv :        Paradise_Valley,
                        45	, //3 su :        Sun_City,
                        45	, //4 sw :        Sun_City_West,
                        45	, //5 av :        Avondale,
                        45	, //6 be :        Berneil,
                        45	, //7 bu :        Buckeye,
                        45	, //8 cf :        Carefree,
                        45	, //9 cc :        Cave_Creek,
                        45	, //10 ch :        Chandler,
                        45	, //11 cp :        Chaparral_City,
                        45	, //12 sp :        Surprise,
                        45	, //13 cu :        Clearwater_Utilities,
                        45	, //14 dh :        Desert_Hills,
                        45	, //15 em :        El_Mirage,
                        45	, //16 gi :        Gilbert,
                        45	, //17 gl :        Glendale,
                        45	, //18 go :        Goodyear,
                        45	, //19 lp :        Litchfield_Park,
                        45	, //20 me :        Mesa,
                        //45	, //21 no :       No provider
                        //45	, //22 op :       Other Provider
                        45	, //23 pe :        Peoria,
                        45	, //24 ph :        Phoenix,
                        45	, //25 qk :        Queen_Creek,
                        45	, //26 rg :        Rigby,
                        45	, //27 rv :        Rio_Verde,
                        45	, //28 ry :        Rose_Valley,
                        45	, //29 sc :        Scottsdale,
                        45	, //30 sr :        Sunrise,
                        45	, //31 te :        Tempe,
                        45	, //32 to :        Tolleson,
                        45	, //33 vu :        Valley_Utilities,
                        45	 //34 we :        West_End,
                    };
            temp.Values = ReclaimedDemand;
            PCT_Max_Demand_Reclaim.setvalues(temp);


            // Wastewater to Effluent

            int[] WWtoEffluent = new int[ProviderClass.NumberOfProviders] {
              
                        0	, //0 ad :       Adaman_Mutual,
                        0	, //1 wt :        White_Tanks,
                        0	, //2 pv :        Paradise_Valley,
                        0	, //3 su :        Sun_City,
                        0	, //4 sw :        Sun_City_West,
                        100	, //5 av :        Avondale,
                        0	, //6 be :        Berneil,
                        52	, //7 bu :        Buckeye,
                        0	, //8 cf :        Carefree,
                        0	, //9 cc :        Cave_Creek,
                        100	, //10 ch :        Chandler,
                        100	, //11 cp :        Chaparral_City,
                        100	, //12 sp :        Surprise,
                        0	, //13 cu :        Clearwater_Utilities,
                        0	, //14 dh :        Desert_Hills,
                        96	, //15 em :        El_Mirage,
                        100	, //16 gi :        Gilbert,
                        91	, //17 gl :        Glendale,
                        45	, //18 go :        Goodyear,
                        100	, //19 lp :        Litchfield_Park,
                        71	, //20 me :        Mesa,
                        //0	, //21 no :       No provider
                       // 0	, //22 op :       Other Provider
                        56	, //23 pe :        Peoria,
                        81	, //24 ph :        Phoenix,
                        0	, //25 qk :        Queen_Creek,
                        0	, //26 rg :        Rigby,
                        100	, //27 rv :        Rio_Verde,
                        0	, //28 ry :        Rose_Valley,
                        61	, //29 sc :        Scottsdale,
                        0	, //30 sr :        Sunrise,
                        100	, //31 te :        Tempe,
                        0	, //32 to :        Tolleson,
                        0	, //33 vu :        Valley_Utilities,
                        0	 //34 we :        West_End,
                 };

            temp.Values = WWtoEffluent;
            PCT_Wastewater_to_Effluent.setvalues(temp);


            // Effluent to Vadose

            int[] EffluenttoVadose = new int[ProviderClass.NumberOfProviders] {


                    0	, //0 ad :       Adaman_Mutual,
                    0	, //1 wt :        White_Tanks,
                    0	, //2 pv :        Paradise_Valley,
                    0	, //3 su :        Sun_City,
                    0	, //4 sw :        Sun_City_West,
                    100	, //5 av :        Avondale,
                    0	, //6 be :        Berneil,
                    5	, //7 bu :        Buckeye,
                    0	, //8 cf :        Carefree,
                    0	, //9 cc :        Cave_Creek,
                    54	, //10 ch :        Chandler,
                    0	, //11 cp :        Chaparral_City,
                    16	, //12 sp :        Surprise,
                    0	, //13 cu :        Clearwater_Utilities,
                    0	, //14 dh :        Desert_Hills,
                    96	, //15 em :        El_Mirage,
                    100	, //16 gi :        Gilbert, 0
                    49	, //17 gl :        Glendale,
                    45	, //18 go :        Goodyear,
                    0	, //19 lp :        Litchfield_Park,
                    0	, //20 me :        Mesa,
                    //0	, //21 no :       No provider
                   // 0	, //22 op :       Other Provider
                    38	, //23 pe :        Peoria,
                    4	, //24 ph :        Phoenix,
                    0	, //25 qk :        Queen_Creek,
                    0	, //26 rg :        Rigby,
                    0	, //27 rv :        Rio_Verde,
                    0	, //28 ry :        Rose_Valley,
                    18	, //29 sc :        Scottsdale,
                    0	, //30 sr :        Sunrise,
                    16	, //31 te :        Tempe,
                    0	, //32 to :        Tolleson,
                    0	, //33 vu :        Valley_Utilities,
                    0	 //34 we :        West_End,
               };

            temp.Values = EffluenttoVadose;
            PCT_Effluent_to_Vadose.setvalues(temp);

            // Effluent to POWER

            int[] EffluenttoPower = new int[ProviderClass.NumberOfProviders] {

                    0	, //0 ad :       Adaman_Mutual,
                    0	, //1 wt :        White_Tanks,
                    0	, //2 pv :        Paradise_Valley,
                    0	, //3 su :        Sun_City,
                    0	, //4 sw :        Sun_City_West,
                    0	, //5 av :        Avondale,
                    0	, //6 be :        Berneil,
                    0	, //7 bu :        Buckeye,
                    0	, //8 cf :        Carefree,
                    0	, //9 cc :        Cave_Creek,
                    0	, //10 ch :        Chandler,
                    0	, //11 cp :        Chaparral_City,
                    0	, //12 sp :        Surprise,
                    0	, //13 cu :        Clearwater_Utilities,
                    0	, //14 dh :        Desert_Hills,
                    0	, //15 em :        El_Mirage,
                    0	, //16 gi :        Gilbert,
                    17	, //17 gl :        Glendale,
                    0	, //18 go :        Goodyear,
                    0	, //19 lp :        Litchfield_Park,
                    14	, //20 me :        Mesa,
                   // 0	, //21 no :       No provider
                   // 0	, //22 op :       Other Provider
                    0	, //23 pe :        Peoria,
                    36	, //24 ph :        Phoenix,
                    0	, //25 qk :        Queen_Creek,
                    0	, //26 rg :        Rigby,
                    0	, //27 rv :        Rio_Verde,
                    0	, //28 ry :        Rose_Valley,
                    19	, //29 sc :        Scottsdale,
                    0	, //30 sr :        Sunrise,
                    55	, //31 te :        Tempe,
                    0	, //32 to :        Tolleson,
                    0	, //33 vu :        Valley_Utilities,
                    0	 //34 we :        West_End,

               };

            temp.Values = EffluenttoPower;
            PCT_Effluent_to_PowerPlant.setvalues(temp);

            // Water Banking

            int[] AmntToBank = new int[ProviderClass.NumberOfProviders] {

                    0	, //0 ad :       Adaman_Mutual,
                    0	, //1 wt :        White_Tanks,
                    0	, //2 pv :        Paradise_Valley,
                    0	, //3 su :        Sun_City,
                    0	, //4 sw :        Sun_City_West,
                    18814	, //5 av :        Avondale,
                    0	, //6 be :        Berneil,
                    25	, //7 bu :        Buckeye,
                    0	, //8 cf :        Carefree,
                    0	, //9 cc :        Cave_Creek,
                    0	, //10 ch :        Chandler,
                    0	, //11 cp :        Chaparral_City,
                    10249	, //12 sp :        Surprise,
                    0	, //13 cu :        Clearwater_Utilities,
                    0	, //14 dh :        Desert_Hills,
                    508	, //15 em :        El_Mirage,
                    0	, //16 gi :        Gilbert,
                    145	, //17 gl :        Glendale,
                    3531	, //18 go :        Goodyear,
                    0	, //19 lp :        Litchfield_Park,
                    376	, //20 me :        Mesa,
                   // 0	, //21 no :       No provider
                   // 0	, //22 op :       Other Provider
                    0	, //23 pe :        Peoria,
                    2017	, //24 ph :        Phoenix,
                    0	, //25 qk :        Queen_Creek,
                    0	, //26 rg :        Rigby,
                    0	, //27 rv :        Rio_Verde,
                    0	, //28 ry :        Rose_Valley,
                    3963	, //29 sc :        Scottsdale,
                    0	, //30 sr :        Sunrise,
                    5	, //31 te :        Tempe,
                    0	, //32 to :        Tolleson,
                    0	, //33 vu :        Valley_Utilities,
                    0	 //34 we :        West_End,

               };

            temp.Values = AmntToBank;
            Use_SurfaceWater_to_WaterBank.setvalues(temp);
            // Initialize the banking method to 2
          // 10.04.15
            Default_Banking = AmntToBank;
          //
            for (int i = 0; i < AmntToBank.Length; i++)
            {
                if (AmntToBank[i] > 0) WaterBank_Source_Option[i] = 2;
            }
         //
         //
            // DAS
            // Based on toilet use-
            /// <summary>   The pct outdoor use for residential water users. </summary>
            int[] PCT_outdoor_use_residential = new int[ProviderClass.NumberOfProviders] {
                     72  , //0 ad :       Adaman_Mutual,
                     42  , //1 wt :        White_Tanks,
                     91  , //2 pv :        Paradise_Valley,
                     65  , //3 su :        Sun_City,
                     52  , //4 sw :        Sun_City_West,
                     39  , //5 av :        Avondale,
                     88  , //6 be :        Berneil,
                     61  , //7 bu :        Buckeye,
                     76  , //8 cf :        Carefree,
                     69  , //9 cc :        Cave_Creek,
                     60  , //10 ch :        Chandler,
                     64  , //11 cp :        Chaparral_City,
                     43  , //12 sp :        Surprise,
                     38  , //13 cu :        Clearwater_Utilities,
                     40  , //14 dh :        Desert_Hills,
                     43  , //15 em :        El_Mirage,
                     58  , //16 gi :        Gilbert,
                     50  , //17 gl :        Glendale,
                     49  , //18 go :        Goodyear,
                     71  , //19 lp :        Litchfield_Park,
                     52  , //20 me :        Mesa,
                   // 0	, //21 no :       No provider
                   // 0	, //22 op :       Other Provider
                     47  , //23 pe :        Peoria,
                     55  , //24 ph :        Phoenix,
                     65  , //25 qk :        Queen_Creek,
                     51  , //26 rg :        Rigby,
                     93  , //27 rv :        Rio_Verde,
                     52  , //28 ry :        Rose_Valley,
                     68  , //29 sc :        Scottsdale, 0.71 - changed on 12.12.16 das to match Scenarios's project estimates
                     59  , //30 sr :        Sunrise,
                     70  , //31 te :        Tempe,
                     73  , //32 to :        Tolleson,
                     34  , //33 vu :        Valley_Utilities,
                     51   //34 we :        West_End,                  

                };
            temp.Values = PCT_outdoor_use_residential;
            PCT_Outdoor_WaterUseRes.setvalues(temp);
            /// <summary>   The pct watersupply to residential water users. </summary>
            int[] PCT_Watersupply_to_Res = new int[ProviderClass.NumberOfProviders] {

                    31     , //0 ad :        Adaman_Mutual, (the remaining data come from the excell data sheet for the model)
                    70     , //1 wt :        White_Tanks, - used average from all data
                    65     , //2 pv :        Paradise_Valley,
                    79     , //3 su :        Sun_City,
                    75     , //4 sw :        Sun_City_West,
                    66     , //5 av :        Avondale,
                    98     , //6 be :        Berneil,
                    63     , //7 bu :        Buckeye,
                    75     , //8 cf :        Carefree,
                    65     , //9 cc :        Cave_Creek,
                    60     , //10 ch :       Chandler,
                    78     , //11 cp :       Chaparral_City,
                    65     , //12 sp :       Surprise,
                    99     , //13 cu :       Clearwater_Utiliti
                    96     , //14 dh :       Desert_Hills,
                    77     , //15 em :       El_Mirage,
                    72     , //16 gi :       Gilbert,
                    72     , //17 gl :       Glendale,
                    58     , //18 go :       Goodyear,
                    61     , //19 lp :       Litchfield_Park,
                    64     , //20 me :       Mesa,
                   // 0	   , //21 no :       No provider
                   // 0	   , //22 op :       Other Provider
                    72     , //23 pe :       Peoria,
                    61     , //24 ph :       Phoenix, - used data from a 4/23/12 document- GPCD city of phoenix 1990 to 2011
                    73     , //25 qk :       Queen_Creek,
                    95     , //26 rg :       Rigby,
                    25     , //27 rv :       Rio_Verde,
                    79     , //28 ry :       Rose_Valley,
                    70     , //29 sc :       Scottsdale,
                    96     , //30 sr :       Sunrise,
                    51     , //31 te :       Tempe,
                    17     , //32 to :       Tolleson,
                    76     , //33 vu :       Valley_Utilities,
                    91      //34 we :       West_End,   
                };
            temp.Values = PCT_Watersupply_to_Res;
            // set all values to zero first
            for (int i = 0; i < ProviderClass.NumberOfProviders; i++)
            {
                PCT_WaterSupply_to_Residential[i] = 0;
            }
            for (int i = 0; i < ProviderClass.NumberOfProviders; i++)
            {
                PCT_WaterSupply_to_Commercial[i] = 0;
            }
            for (int i = 0; i < ProviderClass.NumberOfProviders; i++)
            {
                PCT_WaterSupply_to_Industrial[i] = 0;
            }


            PCT_WaterSupply_to_Residential.setvalues(temp);
            /// <summary>   The pct watersupply to commercial water users. </summary>
            int[] PCT_Watersupply_to_Com = new int[ProviderClass.NumberOfProviders] {

                  34       , //0 ad :       Adaman_Mutual, (the remaining data come from the excell data sheet for the model)
                  21      , //1 wt :        White_Tanks, - used average from all data
                  20      , //2 pv :        Paradise_Valley,
                  16      , //3 su :        Sun_City,
                  23      , //4 sw :        Sun_City_West,
                  20      , //5 av :        Avondale,
                  2       , //6 be :        Berneil,
                  19      , //7 bu :        Buckeye,
                  23      , //8 cf :        Carefree,
                  20      , //9 cc :        Cave_Creek,
                  18      , //10 ch :        Chandler,
                  16      , //11 cp :        Chaparral_City,
                  20      , //12 sp :        Surprise,
                  1       , //13 cu :        Clearwater_Utiliti
                  2       , //14 dh :        Desert_Hills,
                  23      , //15 em :        El_Mirage,
                  22      , //16 gi :        Gilbert,
                  22      , //17 gl :        Glendale,
                  17      , //18 go :        Goodyear,
                  18      , //19 lp :        Litchfield_Park,
                  19      , //20 me :        Mesa,
                // 0	 , //21 no :       No provider
                // 0	 , //22 op :       Other Provider
                  22      , //23 pe :        Peoria,
                  18      , //24 ph :        Phoenix, - used data from a 4/23/12 document- GPCD city of phoenix 1990 to 2011
                  22      , //25 qk :        Queen_Creek,
                  5       , //26 rg :        Rigby,
                  8       , //27 rv :        Rio_Verde,
                  16      , //28 ry :        Rose_Valley,
                  21      , //29 sc :        Scottsdale,
                  2       , //30 sr :        Sunrise,
                  15      , //31 te :        Tempe,
                  5       , //32 to :        Tolleson,
                  23      , //33 vu :        Valley_Utilities,
                  9        //34 we :        West_End,   
                };
            temp.Values = PCT_Watersupply_to_Com;
            PCT_WaterSupply_to_Commercial.setvalues(temp);

            /// <summary>   The pct watersupply to industrial water users. </summary>
            int[] PCT_Watersupply_to_Ind = new int[ProviderClass.NumberOfProviders] {

                    35   , //0 ad :       Adaman_Mutual, (the remaining data come from the excell data sheet for the model)
                     9   , //1 wt :        White_Tanks, - used average from all data
                    15   , //2 pv :        Paradise_Valley,
                     5   , //3 su :        Sun_City,
                     2   , //4 sw :        Sun_City_West,
                    14   , //5 av :        Avondale,
                     0   , //6 be :        Berneil,
                    18   , //7 bu :        Buckeye,
                     2   , //8 cf :        Carefree,
                    15   , //9 cc :        Cave_Creek,
                    22   , //10 ch :        Chandler,
                     6   , //11 cp :        Chaparral_City,
                    15   , //12 sp :        Surprise,
                     0   , //13 cu :        Clearwater_Utiliti
                     2   , //14 dh :        Desert_Hills,
                     0   , //15 em :        El_Mirage,
                     6   , //16 gi :        Gilbert,
                     6   , //17 gl :        Glendale,
                    25   , //18 go :        Goodyear,
                    21   , //19 lp :        Litchfield_Park,
                    17   , //20 me :        Mesa,
                  // 0	 , //21 no :       No provider
                  // 0	 , //22 op :       Other Provider
                     6   , //23 pe :        Peoria,
                    21   , //24 ph :        Phoenix, - used data from a 4/23/12 document- GPCD city of phoenix 1990 to 2011
                     5   , //25 qk :        Queen_Creek,
                     0   , //26 rg :        Rigby,
                    67   , //27 rv :        Rio_Verde,
                     5   , //28 ry :        Rose_Valley,
                     9   , //29 sc :        Scottsdale,
                     2   , //30 sr :        Sunrise,
                    34   , //31 te :        Tempe,
                    78   , //32 to :        Tolleson,
                     1   , //33 vu :        Valley_Utilities,
                     0    //34 we :        West_End,   
                };
            temp.Values = PCT_Watersupply_to_Ind;
            PCT_WaterSupply_to_Industrial.setvalues(temp);
         //
            int[] min_alter_GPCD = new int[ProviderClass.NumberOfProviders] {

                    30    , //0 ad :       Adaman_Mutual, (the remaining data come from the excell data sheet for the model)
                    30   , //1 wt :        White_Tanks, - used average from all data
                    30    , //2 pv :        Paradise_Valley,
                    30   , //3 su :        Sun_City,
                    30   , //4 sw :        Sun_City_West,
                    30   , //5 av :        Avondale,
                    30   , //6 be :        Berneil,
                    30   , //7 bu :        Buckeye,
                    30   , //8 cf :        Carefree,
                    30   , //9 cc :        Cave_Creek,
                    30   , //10 ch :        Chandler,
                    30   , //11 cp :        Chaparral_City,
                    30   , //12 sp :        Surprise,
                    30   , //13 cu :        Clearwater_Utiliti
                    30   , //14 dh :        Desert_Hills,
                    30   , //15 em :        El_Mirage,
                    30   , //16 gi :        Gilbert,
                    30   , //17 gl :        Glendale,
                    30   , //18 go :        Goodyear,
                    30   , //19 lp :        Litchfield_Park,
                    30   , //20 me :        Mesa,
                  // 50	 , //21 no :       No provider
                  // 50	 , //22 op :       Other Provider
                    30   , //23 pe :        Peoria,
                    30   , //24 ph :        Phoenix, - used data from a 4/23/12 document- GPCD city of phoenix 1990 to 2011
                    30   , //25 qk :        Queen_Creek,
                    30   , //26 rg :        Rigby,
                    30   , //27 rv :        Rio_Verde,
                    30   , //28 ry :        Rose_Valley,
                    30   , //29 sc :        Scottsdale,
                    30   , //30 sr :        Sunrise,
                    30   , //31 te :        Tempe,
                    30   , //32 to :        Tolleson,
                    30   , //33 vu :        Valley_Utilities,
                    30    //34 we :        West_End,   
                };
            temp.Values = min_alter_GPCD;
            Provider_GPCD_Min.setvalues(temp);

            /// <summary>   The pct watersupply to industrial water users. </summary>
            double[] intercept_GPCD = new double[ProviderClass.NumberOfProviders] {
                1033,      //ad  -3264.8 ===1033.04
                3853.58,     // wt
                27068,       //pv
                9930,        //su
                5678.58,    //4 sw :        Sun_City_West,
                4440.25,    // av
                4536,        //be
                967.5,          // bu
                1840,        // cf
                844.79,         //cc
                2529,       //ch
                11889,      // cp
                1459.36,     // sp
                5542.67,    //13 cu :       Clearwater_Utilities 1
                1391.54,       // dh
                4996.81,
                5596.82,  // gi
                10210,    // gl
                6248.48, // go
                3404.78,          // lp
                1851.48,    //me
                5385.93,      // pe
                4855.85,  // ph
                10771,   // qk
                4163.47,  //rg
                6830.66,           //rv
                2212.75,           //ry
                2977,           //sc
                427,   //sr -2986 ===427
                7787.63, //te
                2428,          //to
                874.93,          //vu
                4732.2   //we
            };
              
            InterceptGPCD.Values = intercept_GPCD;
            /// <summary>   The pct watersupply to industrial water users. </summary>
            double[] pct_alter_GPCD = new double[ProviderClass.NumberOfProviders] {

                     -0.28979,    //0 ad :        Adaman_Mutual, 1.855 (the remaining data come from the excell data sheet for the model)-0.28979
                     -1.833,   //1 wt :         White_Tanks, 
                     -13,       //2 pv :        Paradise_Valley, 1
                    -4.785,    //3 su :         Sun_City,
                    -2.738,    //4 sw :        Sun_City_West, 2
                    -2.130,    //5 av :         Avondale,
                     -1.7,       //6 be :        Berneil, 2
                     -0.429,    //7 bu :         Buckeye,
                     -0.7857,   //8 cf :         Carefree,
                     -0.25,     //9 cc :         Cave_Creek,
                     -1.147,   //10 ch :       Chandler,
                    -5.8,       //11 cp :       Chaparral_City,
                     -0.65,    //12 sp :       Surprise,
                    -2.688,    //13 cu :       Clearwater_Utilities 1
                     -0.6,      //14 dh :       Desert_Hills, 4
                    -2.420,    //15 em :      El_Mirage,
                    -2.687,    //16 gi :        Gilbert,
                    -5.004,    //17 gl :        Glendale,
                    -3.002,    //18 go :       Goodyear,
                     -1.6,      //19 lp :        Litchfield_Park,
                     -0.835,    //20 me :      Mesa,
                     //-3,      //21 no :        No provider
                    //-3,      //22 op :        Other Provider
                     -2.599,    //23 pe :      Peoria,
                    -2.325,    //24 ph :       Phoenix, - used data from a 4/23/12 document- GPCD city of phoenix 1990 to 2011
                    -5.235,    //25 qk :      Queen_Creek,
                    -2.02,     //26 rg :       Rigby,
                     -3.04,    //27 rv :       Rio_Verde,
                     -1.00,    //28 ry :       Rose_Valley,
                      -1.327,    //29 sc :     Scottsdale,
                     -0.05714,    //30 sr :       Sunrise, 1.643 -      -0.05714
                    -3.742,    //31 te :       Tempe, 1
                     -0.916,    //32 to :        Tolleson,
                     -0.364,    //33 vu :        Valley_Utilities,
                    -2.3     //34 we :       West_End,   
                };
             SlopeGPCD.Values = pct_alter_GPCD;
          
             //
             //ProviderIntArray modSlope = new ProviderIntArray(0);          
             modSlope.Values= createGPCD(pct_alter_GPCD, intercept_GPCD, min_alter_GPCD);
             PCT_alter_GPCD.setvalues(modSlope);
             API_Default_Status = 1;

            /// <summary>   The pct watersupply to industrial water users. </summary>
 
            /// <summary>   The pct watersupply to industrial water users. </summary>
            //int[] Add_SWtoAG_AF = new int[ProviderClass.NumberOfProviders] {

            //        0   , //0 ad :       Adaman_Mutual, (the remaining data come from the excell data sheet for the model)
            //        0   , //1 wt :        White_Tanks, - used average from all data
            //        0   , //2 pv :        Paradise_Valley,
            //        0   , //3 su :        Sun_City,
            //        0   , //4 sw :        Sun_City_West,
            //        0   , //5 av :        Avondale,
            //        0   , //6 be :        Berneil,
            //        0   , //7 bu :        Buckeye,
            //        0   , //8 cf :        Carefree,
            //        0   , //9 cc :        Cave_Creek,
            //        0   , //10 ch :        Chandler,
            //        0   , //11 cp :        Chaparral_City,
            //        0   , //12 sp :        Surprise,
            //        0   , //13 cu :        Clearwater_Utiliti
            //        0   , //14 dh :        Desert_Hills,
            //        0   , //15 em :        El_Mirage,
            //        0   , //16 gi :        Gilbert,
            //        0   , //17 gl :        Glendale,
            //        0   , //18 go :        Goodyear,
            //        0   , //19 lp :        Litchfield_Park,
            //        0   , //20 me :        Mesa,
            //      // 0	 , //21 no :       No provider
            //      // 0	 , //22 op :       Other Provider
            //        0   , //23 pe :        Peoria,
            //        0   , //24 ph :        Phoenix, - used data from a 4/23/12 document- GPCD city of phoenix 1990 to 2011
            //        0   , //25 qk :        Queen_Creek,
            //        0   , //26 rg :        Rigby,
            //        0   , //27 rv :        Rio_Verde,
            //        0   , //28 ry :        Rose_Valley,
            //        0   , //29 sc :        Scottsdale,
            //        0   , //30 sr :        Sunrise,
            //        0   , //31 te :        Tempe,
            //        0   , //32 to :        Tolleson,
            //        0   , //33 vu :        Valley_Utilities,
            //        0    //34 we :        West_End,   
            //    };
            //temp.Values = Add_SWtoAG_AF;
            //SurfaceWater_to_Agriculture.setvalues(temp);
            //
            int[] Default_MnIPumping_PCT = new int[ProviderClass.NumberOfProviders] {
                // Used 1 for all cities who lowest ammual avg GW/PRoduction ration for AWDR Annual Reports 2003 to 2013 was more than 5%.
                // for cities who lowest avg was less than 5%.  Lowest avg was used
                    5   , //0 ad :       Adaman_Mutual, 
                    5   , //1 wt :        White_Tanks, -
                    5   , //2 pv :        Paradise_Valley,
                    5   , //3 su :        Sun_City,
                    5   , //4 sw :        Sun_City_West,
                    5   , //5 av :        Avondale,
                    5   , //6 be :        Berneil,
                    5   , //7 bu :        Buckeye,
                    5   , //8 cf :        Carefree,
                    1   , //9 cc :        Cave_Creek,
                    5   , //10 ch :        Chandler,
                    5   , //11 cp :        Chaparral_City,
                    1  , //12 sp :        Surprise,
                    5   , //13 cu :        Clearwater_Utiliti
                    5   , //14 dh :        Desert_Hills,
                    5   , //15 em :        El_Mirage,
                    5   , //16 gi :        Gilbert, 5
                    2   , //17 gl :        Glendale,
                    5   , //18 go :        Goodyear,
                    5   , //19 lp :        Litchfield_Park,
                    4   , //20 me :        Mesa,
                  // 0	 , //21 no :       No provider
                  // 0	 , //22 op :       Other Provider
                    5   , //23 pe :        Peoria,
                    4   , //24 ph :        Phoenix, -
                    5   , //25 qk :        Queen_Creek,
                    5  , //26 rg :        Rigby,
                    5   , //27 rv :        Rio_Verde,
                    5   , //28 ry :        Rose_Valley,
                    5   , //29 sc :        Scottsdale,
                    5   , //30 sr :        Sunrise,
                    5   , //31 te :        Tempe,
                    1   , //32 to :        Tolleson,
                    5   , //33 vu :        Valley_Utilities,
                    5    //34 we :        West_End,   
                };
            temp.Values = Default_MnIPumping_PCT;
            Default_Pumping_MnI_PCT.setvalues(temp);

        }
         // create methods here
       internal int[] createGPCD(double[] pct_alter_GPCD, double[] intercept_GPCD, int[] min_alter_GPCD)
         {
             int up = 0;
             double gpcd = 0;
             double start = 0;
             double temp_gpcd = 0;
             double alter_PCT = 0;
             double yr = 0;

             double maxGPCD_pct = 95;
             double minGPCD_pct = -95;
             double correctBias = 0.76;
             int[] start_gpcd = new int[ProviderClass.NumberOfProviders];
             int[] response_GPCD = new int[ProviderClass.NumberOfProviders];
             int[] myOut = new int[ProviderClass.NumberOfProviders];
            // ============================================
             for (int i = 0; i < ProviderClass.NumberOfProviders; i++)
             {
                 double alter = 0;
                 alter = pct_alter_GPCD[i];
                 if (0 < alter) up = 1;
                 if (alter == 0) up = 0;
                 if (alter < 0) up = -1;
                 switch (up)
                 {
                     case (1):
                         for (int j = 2000; j < 2086; j++)
                         {
                             yr = Convert.ToDouble(j);
                             gpcd = pct_alter_GPCD[i] * yr + intercept_GPCD[i];
                             response_GPCD[i] = 0;
                             if (j == 2013)
                             {
                                 start_gpcd[i] = Convert.ToInt32(gpcd);
                             }
                             if (2084 < j)
                             {
                                 temp_gpcd = gpcd;
                                 start = Convert.ToDouble(start_gpcd[i]);
                                 alter_PCT = ((temp_gpcd - start) / start) * 100;
                                 defaultTargetGPCD[i] = Convert.ToInt32(gpcd);
                             }
                         }
                         // added the following line on 10.05.15 das
                         response_GPCD[i] = Convert.ToInt32(Math.Min(maxGPCD_pct, alter_PCT));
                         break;
                     case (0):

                         break;
                     case (-1):
                         for (int j = 2013; j < 2086; j++)
                         {
                             yr = Convert.ToDouble(j);
                             // send to WebInterface.cs file
                             gpcd = pct_alter_GPCD[i] * yr + intercept_GPCD[i];
                             //gpcd = Math.Max(min_alter_GPCD[i], pct_alter_GPCD[i] * yr + intercept_GPCD[i]);
                             defaultTargetGPCD[i] = Convert.ToInt32(gpcd);
                             // use here
                             gpcd = Math.Max(0, pct_alter_GPCD[i] * yr + intercept_GPCD[i]);
                             //
                             response_GPCD[i] = 0;
                             if (j == 2013)
                             {
                                 start_gpcd[i] = Convert.ToInt32(gpcd);
                             }
                             if (2084 < j)
                             {
                                 //temp_gpcd = Math.Max(gpcd,min_alter_GPCD[i]) ;
                                 temp_gpcd = gpcd;
                                 start = Convert.ToDouble(start_gpcd[i]);
                                 // 10.06.15 das
                                 alter_PCT = Math.Max(minGPCD_pct, ((temp_gpcd - start) / start) * 100);
                             }
                         }
                         response_GPCD[i] = Convert.ToInt32(Math.Max(minGPCD_pct, alter_PCT * correctBias));
                         defaultMinGPCD[i] = min_alter_GPCD[i];
                         responseGPCD[i] = response_GPCD[i];

                         break;
                     default:
                         break;
                 }
                 myOut[i] = response_GPCD[i];
             }
             return myOut;
             //myOut.Values = response_GPCD;
         }



         //
    }
}