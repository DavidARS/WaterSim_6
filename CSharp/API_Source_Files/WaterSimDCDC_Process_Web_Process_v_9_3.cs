using System.Text;
using System;
using System.Windows.Forms;
using WaterSimDCDC;
using WaterSimDCDC.Documentation;

namespace WaterSimDCDC.Processes
{
    /// <summary>   Form for viewing the track deficit feedback process. </summary>
    //

    internal class IndicatorWeighting
    {
        double calc = 0;
        internal int Pout = 0;
        int One = 0;
        int Two = 0;
        int Three = 0;
        //
        double _w1 = 0.4;
        double _w2 = 0.6;
        double _w3 = 0;
        //
        string weight = "Weighting factors do not add to 1";
        string sum = "The number of passing weighted is not correct";
        //
         internal IndicatorWeighting() : base()
        {

        }
        internal IndicatorWeighting(int First, int Second, double W1,double W2) 
        {
            One = First;
            Two = Second;
            
            _w1 = W1;
            _w2 = W2;
        }
        internal IndicatorWeighting(int First, int Second, int Third,double W1, double W2, double W3)
        {
            One = First;
            Two = Second;
            Three = Third;
            _w1 = W1;
            _w2 = W2;
            _w3 = W3;
        }

        internal int Weight()
        {
             try
            {
                 if(0 <= One && 0 <= Two)
                {
                    if( Pcheck(w1, w2, w3))
                    { calc =( Convert.ToDouble(One) * w1) + (Convert.ToDouble(Two) * w2);}
                     
                }
                 else if (0 <= One && 0 <= Two && 0 <= Three)
                {
                    if (Pcheck(w1, w2, w3)) { calc = (Convert.ToDouble(One) * w1) + (Convert.ToDouble(Two) * w2) + (Convert.ToDouble(Three) * w3); }
                }
                 Pout = Convert.ToInt32(calc);

            }
            catch (Exception e)
             {
               sum = e.Message;
             }

            return Pout;
        }
        //-----------------------------------------------------------
        private bool Pcheck(double one, double two, double three)
        {
            bool check = false;
            double Total = one + two + three;
            if (Total.Equals(1.0))
            {
                try
                {
                    check = true;
                }
                catch(Exception e)
                {
                    weight = e.Message;
                }
            }
            return check;
        }
        internal double w1
        {
            get { return _w1; }
            set { _w1 = value; }
        }
        internal double w2
        {
            get { return _w2; }
            set { _w2 = value; }
        }
        internal double w3
        {
            get { return _w3; }
            set { _w3 = value; }
        }


    }
    /// <summary>
    /// Personal_GPCD_WebFeedbackProcess
    /// </summary>
    #region
    public class Personal_GPCD_WebFeedbackProcess : WaterSimDCDC.AnnualFeedbackProcess
    {
        public Personal_GPCD_WebFeedbackProcess()
            : base()
        {
            Fname = "Personal Use of Water";
        }
        public Personal_GPCD_WebFeedbackProcess(string aName)
            : base(aName)
        {
        }
        public Personal_GPCD_WebFeedbackProcess(WaterSimManager WSim)
            : base("Personal Use of Water")
        {
            FetchValuesForPersonal(WSim);
        }
        public Personal_GPCD_WebFeedbackProcess(string aName, WaterSimManager WSim)
            : base(aName, WSim)
        {
            FetchValuesForPersonal(WSim);
        }
        protected void FetchValuesForPersonal(WaterSimManager WSim)
        {
            WSim.Web_Personal_PCT = WSim.PCT_Personal;
        }

        protected override void BuildDescStrings()
        {
            FProcessDescription = "Creates parameters to track Personal Water Use Parameter";
            FProcessLongDescription = "Creates personal water use parameter to set and get GPCD values based on a Personal Use input control.";
            FProcessCode = "TRKPU";
        }

        public override bool PreProcess(int year, WaterSimManagerClass WSimClass)
        {
            WaterSimManager WSim = (WSimClass as WaterSimManager);

            bool TempLock = WSim.isLocked();

            WSim.UnLockSimulation();
            FetchValuesForPersonal(WSim);
            if (TempLock) WSim.LockSimulation();

            return base.PreProcess(year, WSim);
        }

    }

    #endregion
}
