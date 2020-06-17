!
!  File is Functions.f90
!
!    This file contains the functions for the model. Can be called from anywhere in
! the program.
!
! ---------------------------------------------------------------------------------------
!
!      WaterSimDCDC Regional Water Demand and Supply Model Version 5.0

!       This is the Fortran code for the WaterSim_DCDC FORTRAN dll.

!       Copyright (C) 2014 , The Arizona Board of Regents
!              on behalf of Arizona State University

!       All rights reserved.

!       Developed by the Decision Center for a Desert City
!       Lead Model Development - David A. Sampson <david.a.sampson@asu.edu>

!       This program is free software: you can redistribute it and/or modify
!       it under the terms of the GNU General Public License version 3 as published by
!       the Free Software Foundation.

!       This program is distributed in the hope that it will be useful,
!       but WITHOUT ANY WARRANTY; without even the implied warranty of
!       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!       GNU General Public License for more details.

!       You should have received a copy of the GNU General Public License
!       along with this program.  If not, please see <http:!www.gnu.org/licenses/>.
!
!====================================================================================
!
!
! Module:       Module lms_ClimateFactor
! Functions:    function fClimateFactorCO(i,year,lvi_CF)
!               function fClimateFactorSVT(i,lvi_year,lvi_CF)
!               Function fLinear(m,x,b)
!               subroutine ClimateParms(reduction,lvi_year,m,b)
!               Function slope(y2,x2)
!               Function intercept(y,m,x)
!
! No Module:    subroutine ClimateFactorCO(x,ClimateFactor)
!               subroutine ClimateFactorSVT(x,ClimateFactor)
!               function fEffectiveGPCD(days,population,use)
!               function fRelease(tier,low,mid,high,target,balance)
!               function fStorageRelease(state)
!               function fModifyNormalFlow(x)
!               function fModifyTrottNormalFlow(j,lvf_normal)
!               function fstates(standing,a1,a2,a3,a4,a5,s1,s2,s3,s4)
!               function fLogicalToInteger (truthValue)
!               function fDConvert(variable,DP)
!               function fRoundToAF(variable)
!               subroutine lvf_temp(alpha,inLast)
!               function fSESgpcd_alpha(gvf_AlterGPCD_pct)
!               function fSESgpcd_discount(alpha,gpcd,gpcd_1,gpcd_2,gpcd_3)
!               function  fSESgpcd_equalWeight(N,gpcd)
!               function fGamma(beta)
!               function fSESgpcd_increase(year,gamma,gpcd,lvf_modAlpha)

!                       Reservoir evaporation
!               function fEvaporation(state)
!               function fEvaporationMonthly(state)
!               function fEvaporation_channel(x,flux)
!               function fEvaporation_reach(pan)
!               function fBankStorageReach(lvf_diffStorage)
!               function fEvaporation_Powell(volumeTotal,pan)
!               function fEvaporation_Mead(volumeTotal,pan)

!                           Energy
!               function fHorsePowerCAP(hHt,e,pumped)
!               function fKilowatthrsFromHP(hp)
!               integer function fCftsToacftperannum(days,cft)
!               function fAcftperannumTocfts(days,acft)
!               function fLeapYear(yr)

! OUTPUTS: 
!
! created on 10.08.09
!
! david arthur sampson

! last write was: 09.11.13,07.21.14
! ---------------------------------
!

! =================================================================================================
! 
Module lms_ClimateFactor
    use gm_ModelControl
    use gm_GlobalData

    !    implicit implied
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
        integer, private :: x
        real, private :: m,b
    !
 Contains
    !

    ! -------------------------------------------
    ! Called by  ClimateFactorCO
    ! Used to reduce the climate factor such that
    ! The change set is reached by the threshold
    ! Date set by the interface.
    ! ================================================
    function fClimateFactorCO(i,gvi_StartF,year,lvi_CF)
        ! -------------- Types ------------------
        integer :: i,gvi_StartF
        integer :: lvi_CF,year
        real :: fVariableFactorX
        real :: lvf_equationBase,fClimateFactorCO
        ! =======================================
            !
            fClimateFactorCO=1.
            !
            if(gvl_start)then
             call ClimateParms(lvi_CF,year,m,b)
               gvf_LinearClimateFactorCO(1)=m
               gvf_LinearClimateFactorCO(2)=b
               gvf_LinearClimateFactorCO(3)=lvi_CF*0.01
            endif
                x=i
                x=fVariableFactorX(i,gvi_StartF)
                m = gvf_LinearClimateFactorCO(1)
                b = gvf_LinearClimateFactorCO(2)
                !
                lvf_equationBase=fLinear(m,x,b)
                !
                if(x < 1)then
                else
                  fClimateFactorCO= lvf_equationBase
                endif
            !
      return
    end function fClimateFactorCO
    ! ---------------------------

    ! ---------------------------------------
    ! See above
    ! ===================================================
    function fClimateFactorSVT(i,gvi_StartF,lvi_year,lvi_CF)
        !
        ! -------------- Types ---------------------
        integer :: i,gvi_StartF
        integer :: lvi_CF,lvi_year
        real:: fVariableFactorX
        real :: lvf_equationBase ,fClimateFactorSVT
        ! ==========================================
        !
            !
            fClimateFactorSVT=1
            !
            if(gvl_start)then
             call ClimateParms(lvi_CF,lvi_year,m,b)
               gvf_LinearClimateFactorSVT(1)=m
               gvf_LinearClimateFactorSVT(2)=b
               gvf_LinearClimateFactorSVT(3)=lvi_CF*0.01
            endif
            !
            x=i
            x=fVariableFactorX(i,gvi_StartF)
            m = gvf_LinearClimateFactorSVT(1)
            b = gvf_LinearClimateFactorSVT(2)
            !
            lvf_equationBase=fLinear(m,x,b)
            !
            if(x < 1)then
            else
                fClimateFactorSVT= lvf_equationBase
            endif
            !
        !
      return
    end function fClimateFactorSVT
    ! ----------------------------

    ! ---------------------
    Function fLinear(m,x,b)
        !
        ! --- Types -------
        integer :: x

        real :: m,b
        real :: fLinear
        ! =================
        ! 
        fLinear=m*x+b
        !
    end Function fLinear
    ! ------------------

    ! ---------------------------------------------
    subroutine ClimateParms(reduction,lvi_year,m,b)
        !
        ! --- Types ------------------
        integer :: reduction,lvi_year
        real :: m,b
        ! ============================
        !
            !
             m=0.
            m = slope(reduction*0.01,lvi_year)
             b=0.
            b = intercept(reduction*0.01,m,lvi_year)
            !
        !
      return
    end subroutine ClimateParms
    ! -------------------------

    ! -------------------
    Function slope(y2,x2)
        !
        ! ----- Types ---------
        integer :: x2

        real :: y2
        real :: slope,temp
        ! =====================
        !
            !
             temp=0
             slope=0
            if(y2 < 1)then
              temp = (1-y2)/(1-x2)
            else
             temp =  (-(1-y2))/(x2-1)
            endif
            !       
            slope=temp
            !
         !
    End function slope
    ! ----------------

    ! -----------------------
    Function intercept(y,m,x)
        !
        ! -------- Types -------
        integer :: x

        real :: y,m
        real :: stage,intercept
        ! ======================
        !
            !
             stage=0.
            stage=m*x
            !
             intercept=0.
            if(stage < 0)then
             intercept = y+abs(stage)
            else
             intercept = y-stage
            endif
            !
        !
    End function Intercept
    ! --------------------
!
End Module lms_ClimateFactor
! --------------------------
Module lms_AgricultureRetirement
 use gm_ModelControl
    use gm_GlobalData

    !    implicit implied
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    !
        integer, private :: x
        real, private :: m,b
    !
 contains
    !
   function fAgRetirementFactor(i,lvi_Start,lvi_year,lvi_AgF)
        !
        ! -------------- Types ---------------------
        integer :: i 
        integer :: lvi_Start,lvi_AgF,lvi_year

        real :: lvf_equationBase,fFactor
        real :: fAgRetirementFactor    
        real:: fVariableFactorX   
        ! ==========================================
        !
            ! 
            if(gvl_start)then
             call AgricParms(lvi_AgF,lvi_year,m,b)
               gvf_LinearAgFactor(1)=m
               gvf_LinearAgFactor(2)=b
               gvf_LinearAgFactor(3)=lvi_AgF*0.01
            endif
            !  
                x=fVariableFactorX(i,lvi_Start)
                m = gvf_LinearAgFactor(1)
                b = gvf_LinearAgFactor(2)
                !
                lvf_equationBase=fLinear(m,x,b)
                !
                if(gvf_LinearAgFactor(3) < lvf_equationBase)then
                    fFactor= lvf_equationBase
                else
                    fFactor=gvf_LinearAgFactor(3)
                endif
                 !  
            fAgRetirementFactor=fFactor
         !
      return
    end function fAgRetirementFactor
    ! ----------------------------


    ! ---------------------------------------------
    subroutine AgricParms(reduction,lvi_year,m,b)
        !
        ! --- Types ------------------
        integer :: reduction,lvi_year
        real :: m,b
        ! ============================
        !
            !
             m=0.
            m = slope(reduction*0.01,lvi_year)
             b=0.
            b = intercept(reduction*0.01,m,lvi_year)
            !
        !
      return
    end subroutine AgricParms
    ! -------------------------

    ! -------------------
    Function slope(y2,x2)
        !
        ! --- Types ----
        integer :: x2

        real :: y2
        real :: slope,temp
        ! ==============
        !
            !
             temp=0
             slope=0
            if(y2 < 1)then
              temp = (1-y2)/(1-x2)
            else
             temp =  (-(1-y2))/(x2-1)
            endif
            !       
            slope=temp
            !
        !
    End function slope
    ! ----------------
    ! -----------------------
    Function intercept(y,m,x)
        !
        ! -------- Types -------
        integer :: x

        real :: y,m
        real :: stage,intercept
        ! ======================
        !
            !
             stage=0.
            stage=m*x
            !
             intercept=0.
            if(stage < 0)then
             intercept = y+abs(stage)
            else
             intercept = y-stage
            endif
            !
        !
    End function Intercept
    ! --------------------
    ! ---------------------
    Function fLinear(m,x,b)
        !
        ! --- Types -------
        integer :: x

        real :: m,b
        real :: fLinear
        ! =================
        ! 
        fLinear=m*x+b
        !
    end Function fLinear
    ! ------------------

End Module lms_AgricultureRetirement

    ! ------------------------------------
    function fVariableFactorX(i,lvi_Start)
        integer :: i,lvi_Start
        real :: fVariableFactorX
        !
         fVariableFactorX=1.0
        fVariableFactorX=max(0,i-lvi_Start)
        !
     return
    end function fVariableFactorX
    ! --------------------------

   ! ========================================
    subroutine agRetirement(x,AgFactor)
     use lms_AgricultureRetirement
        !
        ! ---------- Types -----------
        integer :: x,lvi_Start,lvi_End,lvi_AgF
        real :: AgFactor
        ! ============================
        ! 
            ! 
             AgFactor=1.0
              lvi_AgF=nint(gvf_AgRetirementPct*100)
              lvi_Start=gvi_StartFactors
              lvi_End=gvi_EndAgRetire
             AgFactor=fAgRetirementFactor(x,lvi_Start,lvi_End,lvi_AgF)
            ! Cannot have a zero climate factor-
            if(AgFactor < 0.000001)AgFactor=0.01
            if(1.0 < AgFactor)AgFactor=1.0
        !
      return
    end subroutine agRetirement
   ! --------------------------

    ! -------------------------------------------
    ! Reduces the climate factor for the CO River
    ! percent change in runoff in a linear trend
    ! Reaches the desired reduction by the date
    ! indicated by the user.
    ! ===============================================

    ! -----------------------------------------
    subroutine ClimateFactorCO(x,ClimateFactor)
     use lms_ClimateFactor
        !
        ! ----------- Types ---------
        integer :: x,lvi_End,lvi_CF

        real :: ClimateFactor
        ! ===========================
        !
            ! Assumes that the CO and the SVT rivers have the same Stop mvi_year
            lvi_CF=nint(gv_climatefactorCO*100)
             lvi_End=gi_ClimateAdjustmentYearEnd
            ClimateFactor=fClimateFactorCO(x,gvi_StartFactors,lvi_End,lvi_CF)
            if(ClimateFactor < 0.000001)ClimateFactor=0.01

!             lvi_CF=nint(gv_climatefactorCO*100)
!             lvi_End=gi_ClimateAdjustmentYearEnd
!            ClimateFactor=fClimateFactorCO(x,lvi_End,lvi_CF)
!            if(ClimateFactor < 0.000001)ClimateFactor=0.01
            !
        !
      return
    end subroutine ClimateFactorCO
    ! -----------------------------

    ! -----------------------------------------------
    ! For SRP River system. See above.
    ! ===============================================
    subroutine ClimateFactorSVT(x,ClimateFactor)
     use lms_ClimateFactor
        !
        ! ---------- Types -----------
        integer :: x,lvi_End,lvi_CF

        real :: ClimateFactor
        ! ============================
        ! 
            ! Assumes that the CO and the SVT rivers have the same Stop Year
             lvi_CF=nint(gv_climatefactorSVT*100)
              lvi_End=gi_ClimateAdjustmentYearEnd
             ClimateFactor=fClimateFactorSVT(x,gvi_StartFactors,lvi_End,lvi_CF)
            ! Cannot have a zero climate factor- assumes 0.2 is lowest
            if(ClimateFactor < 0.000001)ClimateFactor=0.01
            !

!              lvi_CF=nint(gv_climatefactorSVT*100)
!              lvi_End=gi_ClimateAdjustmentYearEnd
!             ClimateFactor=fClimateFactorSVT(x,lvi_End,lvi_CF)
!            ! Cannot have a zero climate factor- assumes 0.2 is lowest
!            if(ClimateFactor < 0.000001)ClimateFactor=0.01
        !
      return
    end subroutine ClimateFactorSVT
   ! -------------------------------


    ! =============================================================================================
    ! Start here for functions
    ! ------------------------

    ! --------------------
    function fRound(aReal)
      implicit none
        ! ---- Types ------
        real :: fRound
        real :: aReal,frac
        integer :: value
        ! =================
        !
            !
             frac=fraction(aReal)
             value=nint(aReal)
            if(0 < aReal)then
              if(0.5 < frac)value=value+1
            else if(aReal < 0)then
              if(frac < -0.5)value=value-1
            else
            endif
            !
            fRound=value
            !
        !
    end function fRound
    ! -----------------



    !  subroutine daysintheyearK(year,T)
    ! ---------------------------------------------------------
    ! NOT now used. May, in the future. Was used for gray water
    ! recycling stuff in Water_CityModel.f90
    ! =========================================================

    ! ------------------------------------------
    function fEffectiveGPCD(days,population,use)
        !
        ! -------------- Types --------------------------
        integer :: days
        real :: population,use
        real :: fEffectiveGPCD
        real(8), parameter :: gpd_galperacft=325851.43326
        ! ===============================================
        !
            !
            fEffectiveGPCD=(use*gpd_galperacft) / (population * days)
            !
        !
      return
    end function fEffectiveGPCD
    ! --------------------------

        ! -------------------------------------------------------------------------------------
        !  This is a series of equations to estimate what 
        ! release would be based on current storage.
        ! ==========================================================
!           function fRelease(tier,balance)
!                ! --------- Types -----------
!                integer :: tier
!                real(8) :: balance
!                real :: fRelease
!                real, parameter :: target=8.23
!                real, parameter :: low=7.0
!                real, parameter :: mid=9.0
!                real, parameter :: high=9.5
!                ! ================================
!                    !
!                    if(tier < 2)then
!                        ! Tier One
!                        if(balance < 0)then
!                            fRelease=min(high,max(low,abs(balance)))
!                            fRelease=min(high,max(low,mid+balance/2))
!
!                        else 
!                            fRelease=max(low,min(high,balance))
!                        endif
!                        !
!                    else 
!                        ! Tier Three
!                        if(balance < 0)then
!                            fRelease=min(mid,max(low,abs(balance)))
!                        else 
!                            fRelease=max(low,min(mid,balance))
!                        endif
!                        !
!                    endif
!                    !
!
!              return
!            end function fRelease
            ! -------------------
   

    ! Regression conducted on:
    ! 09.01.11 das
    !
    !NOTE: An intercept was not specified for this model.
    !
    !                                              Sum of        Mean               Approx
    !            Source                    DF     Squares      Square    F Value    Pr > F
    !
    !            Model                      2      219622      109811     870.76    <.0001
    !            Error                      3       378.3       126.1
    !            Uncorrected Total          5      220000
    !
    !
    !The NLIN Procedure
    !
    !                                                 Approx
    !                   Parameter      Estimate    Std Error    Approximate 95% Confidence Limits
    !
    !                   a                1249.8        135.7       817.8      1681.9
    !                   b                0.9983     0.000108      0.9980      0.9987
    !
    !
    !                                 Approximate Correlation Matrix
    !                                                a               b
    !
    !                                a       1.0000000      -0.9753574
    !                                b      -0.9753574       1.000000
    ! Type IV exponential
    !
    !  Y = a * b^x
    !
    ! ------------------------------------------------------------------------------------
    !
    ! -------------------------------
    real function fIncrementFloat(In)
        ! -- Types ---
        real :: In
        ! ============
            !
            fIncrementFloat=In + In
            !
    end function fIncrementFloat
    ! --------------------------

   ! ---------------------------------
    real function fIncrementDouble(In)
        ! --- Types ---
        real(8) :: In
        ! =============
            !
            fIncrementDouble=In + In
            !
    end function fIncrementDouble
    ! ---------------------------

  ! -----------------------------
  function fStorageRelease(acfta,state)
    !
    ! Type IV exponential function
    ! Based on a meeting that I had with Tim Skarupa, yvonne reinink, mark hubble
    ! on 09.01.11 (1 pm at ARP headquarters) There seems to be some secrecy in max and min pumping
    ! and at what volume it occurs... based on the Phillips et al. (2009) paper
    !
    ! Note: I modified the abscissa and thus the ordinate to create a positive slope
    ! because it is easier to fit. And, I added an intercept
    !
    ! 06.21.12 DAS NEW algorithms
    !
    ! 800 (kAF) <= storage < 1600
    ! pumping (kAF) = f(intercept + scaling*state**shape)
    ! pumping = a*b**x + c;
    ! 
!                                        The NLIN Procedure
!
!                 NOTE: Convergence criterion met but a WARNING message was
!                 issued.
!
!                                       Estimation Summary
!
!                              Method                   Gauss-Newton
!                              Iterations                          6
!                              Subiterations                       2
!                              Average Subiterations        0.333333
!                              R                            2.366E-7
!                              PPC(c)                        1.95E-8
!                              RPC(c)                       3.145E-6
!                              Object                       2.355E-8
!                              Objective                    37.49556
!                              Observations Read                   5
!                              Observations Used                   5
!                              Observations Missing                0
!
!                                              Sum of        Mean               Approx
!            Source                    DF     Squares      Square    F Value    Pr > F
!
!            Model                      2     39462.5     19731.3    1052.46    0.0009
!            Error                      2     37.4956     18.7478
!            Corrected Total            4     39500.0
!
!
!                                                 Approx
!                   Parameter      Estimate    Std Error    Approximate 95% Confidence Limits
!
!                   a                1003.0      22.1827       907.6      1098.5
!                   b                0.9181       0.0173      0.8438      0.9923
!                   c                -180.2      80.6491      -527.2       166.8
!
!
!                                 Approximate Correlation Matrix
!                                        a               b               c
!
!                        a       1.0000000       0.1693487      -0.2670448
!                        b       0.1693487       1.0000000      -0.9946937
!                        c      -0.2670448      -0.9946937       1.0000000
!
    ! NOTE: c not signficantly different from zero, but without it, the fit is poor
    !     I made this call (to use it) for that reason alone. 09.11.14 DAS- checked on 09.25.14
    !
    ! --------------- Types and parameters --------
    real :: lvf_kAF
    real, parameter :: lpf_maxPumping=325
    real, parameter :: lpf_minPumping=65
    real, parameter :: lpf_intercept=-180.2
    real, parameter :: lpf_scaling=1003.0
    real, parameter :: lpf_shape=0.9181
    real :: fStorageRelease ! kAF
    real(8) :: state,acfta
        ! 04.06.15 DAS
    real:: lvf_pumpingRatio

    logical :: newAlgorithm
    ! =============================================
    ! DAS
        newAlgorithm=.true.
        !
        fStorageRelease=0
        lvf_kAF=state*1000 ! kAF
        !
        lvf_pumpingRatio=acfta/3.0
        !
        if(newAlgorithm)then
          if(lvf_kAF <= 800)then
              fStorageRelease=lpf_maxPumping
                ! 01.29.13 DAS- 
                ! 04.24.13 DAS again! New data on pumping from silvo 
                ! and re-evaluation of Phillips et al.
                ! 09.09.14 Charlie Ester talk at PERA club 800 kAF is now max pumping threshold
                ! Maximum changed to 325 kAF (here: from slide I saw charlie present)
          else
            !
            fStorageRelease=(lpf_scaling*lpf_shape**(lvf_kAF/100) + lpf_intercept)*lvf_pumpingRatio
            !
            ! This was 1600 before 04.24.13
            if(1580 <= lvf_kAF)fStorageRelease=lpf_minPumping
            !
          endif

        else
          if(lvf_kAF <= 600)then
              fStorageRelease=lpf_maxPumping
                ! 01.29.13 DAS- 
                ! 04.24.13 DAS again! New data on pumping from silvo 
                ! and re-evaluation of Phillips et al.
                ! 09.09.14 Charlie Ester talk at PERA club 800 kAF is now max pumping threshold
                ! Maximum changed to 325 kAF (here: from slide I saw charlie present)
          else
            !
            fStorageRelease=(lpf_scaling*lpf_shape**(lvf_kAF/100) + lpf_intercept)
            !
            ! This was 1600 before 04.24.13
            if(1580 <= lvf_kAF)fStorageRelease=lpf_minPumping
            !
          endif

        endif
        !
    return
  end function fStorageRelease
  ! --------------------------

    ! 02.18.12 DAS
    ! -----------------------------------------------
    ! Providers do not use all of their normal flow
    ! rights. Accordingly, these functions help
    ! generate an estimate based on inputs from the
    ! interface to adjust normal flow "used" based
    ! on their estimate of acre feet per acre 
    ! ===============================================

    ! ---------------------------
    function fModifyNormalFlow(x)
        !
        ! ---------- Types and parameters ---------
        real, parameter :: lpf_dependent=0.18420
        real, parameter :: lvf_max=5.4288
        real :: fModifyNormalFlow,x
        ! =========================================
            !
            fModifyNormalFlow=1.0
            if(0 < x)then
              fModifyNormalFlow=min(x * lpf_dependent,lvf_max)
            endif
            !
        !
      return
    end function fModifyNormalFlow
    ! ----------------------------

    ! -------------------------------------------
    function fModifyTrottNormalFlow(j,lvf_normal)
        !
        ! ----------- Types ---------------------
        integer :: j

        real :: lvf_normal(10),fModifyNormalFlow
        real :: fModifyTrottNormalFlow
        ! =======================================
            !
            fModifyTrottNormalFlow=fModifyNormalFlow(lvf_normal(j))
            !
      return
    end function fModifyTrottNormalFlow
    ! ----------------------------------- 


        ! -------------------------------------------------------
        ! Simple function to clear estimates of state variables
        ! in the TDS (total disolved solids) module
        ! =======================================================

        ! ----------------------------------------------------
         function fstates(standing,a1,a2,a3,a4,a5,s1,s2,s3,s4)
            !
            ! ---------------- Types -------------
            real :: fstates
            real :: a1,a2,a3,a4,a5,s1,s2,s3,s4
            real :: standing
            ! ====================================
            !
              !
              fstates=(standing+a1+a2+a3+a4+a5)-(s1+s2+s3+s4)
              !
              standing=0;a1=0;a2=0;a3=0;a4=0;a5=0;s1=0;s2=0;s3=0;s4=0
            !
          return
         end function fstates
        ! --------------------
 
    ! -------------------------------------
    ! Self explanatory
    ! =====================================

    ! -------------------------------------
    function fLogicalToInteger (truthValue)
        !
        ! ---------- Types ----------------
        integer :: fLogicalToInteger

        logical, intent (in) :: truthValue
        ! ======================================
        !
            fLogicalToInteger=0
            !
            if (truthValue) then
              fLogicalToInteger = 1
            else
              fLogicalToInteger = 0
            endif
        !
      return
    end function
    ! ----------

    ! ----------------------------
    ! Decimal conversion to target
    ! decimal places (i.e.,DP).
    ! ============================

    ! -----------------------------
    function fDConvert(variable,DP)
        !
        ! ------ Types --------
        integer DP,out

        real :: variable
        real :: fDConvert
        real :: new,v
        ! =====================
        !
            !
            v=10**DP
            new=variable*v
            out=NINT(new)
            fDConvert=out/v
            !
        !
     return   
    end function
    ! ----------------------------

    ! ----------------------------
    ! Decimal conversion to target
    ! decimal places (i.e.,DP).
    ! ============================

    ! ---------------------------
    function fRoundToAF(variable)   
        !
        ! ------ Types -------
        real :: variable
        real :: fRoundToAF
        real :: new,v
        ! ====================
        !
            !
            v=variable
            new=float(int(v * 1 + 0.5)) / 1
            fRoundToAF = new
            !
        !
     return
    end function
    ! -----------

 ! Estimate alpha used in the SES (simple exponential smoothing) algorithm for reducing
 ! GPCD over time for each water provider
 ! ==================================================

    ! --------------------------------------------------------------------
    function fSESgpcd_alpha(yearIN,lvf_AlterGPCD,lvf_target,gpcd,lvl_error)
        !
        ! ------------------ Types ------------------------
        integer :: count
        integer :: i,j,lvi_DoStop

        real :: fSESgpcd_alpha
        real:: fMinMaXalpha

        real:: lvf_target,alpha_a,alpha_t
        real,parameter:: lpf_minAlpha=0.01

        real :: lvf_AlterGPCD,fSESgpcd_discount
        real :: lvf_gpcdEstimate
        real :: lvf_GPCD(75),gpcd(7),lvf_error,yearIN
       
        real :: lvf_originalGPCD(7),lpf_minimumGPCD=5
        real :: lvf_simpleSolution(3)


        logical :: lvl_error
        logical :: lvl_top,lvl_bottom
        ! =================================================
        !
        !
        !   %LET model = a*b**x * x**-c;
        ! 1 to 70 %
        ! using Phoenix as a base estimate, 178 GPCD (i.e., ~ 2009 estimate)
        ! 08.19.13   SAS analysis

        ! y = alpha in percent (must divide input by 100)
        ! x = proportional reduction
        ! ------------------------------------------------------------------------------------
        real, parameter :: a=90.5916
        real, parameter :: b= 0.9969
        real, parameter :: c=-0.0422
!        
!                                             Sum of        Mean               Approx
!            Source                    DF     Squares      Square    F Value    Pr > F
!
!            Model                      3     37398.9     12466.3     162409    <.0001
!            Error                      4      0.3070      0.0768
!            Uncorrected Total          7     37399.3
!
!
!                                                 Approx
!                   Parameter      Estimate    Std Error    Approximate 95% Confidence Limits
!
!                   a               90.5916       0.2824     89.8075     91.3756
!                   b                0.9969     0.000132      0.9965      0.9972
!                   c                0.0422      0.00204      0.0365      0.0479
        !
        alpha_a=0.9
        alpha_t=0
        !
        lvf_error=1 ! GPCD
        ! lvi_DoStop; 85 years of GPCD estimates (2085-2010 = 75)
        lvi_DoStop=75
        lvi_DoStop=72 ! From 2013 NOT 2010 so should be 72
        lvi_DoStop=87  ! Experimental - to run to 2100 - more asymptotic at 50 GPCD
        if(lvf_target < lpf_minimumGPCD)lvf_target=lpf_minimumGPCD

        count=0
        lvf_originalGPCD=gpcd
        lvl_top=.false. ;lvl_bottom=.false.
        if(0 < yearIN)lvi_DoStop=nint(2085-yearIN)
        
        ! reduction of 1 < 95 %
         alpha_t=90
        if(0 < abs(lvf_AlterGPCD))then
         alpha_t=(a*b**abs(lvf_AlterGPCD) * abs(lvf_AlterGPCD)**c )
        else

        endif
          !
          fSESgpcd_alpha=alpha_t
          !
          if(0 < alpha_t)then
           alpha_a= fMinMaXalpha(alpha_t*1/100)
          else
           alpha_a= 0.01
          endif
         !
     do j = 1,1,1
5     continue
        count=count+1
        !
        if(lvl_top)then
         lvf_simpleSolution(1)=alpha_a
        endif
        if(lvl_top .and. lvl_bottom)then
          lvf_simpleSolution(2)=alpha_a
            !
          if(75 < count)then
            lvf_simpleSolution(3) =  (lvf_simpleSolution(1) +  lvf_simpleSolution(2))*0.5
            alpha_a=fMinMaXalpha(lvf_simpleSolution(3))
          else
            !
            if(lvf_gpcdEstimate < lvf_target)then
              alpha_a=fMinMaXalpha(alpha_a+0.00025)
            else
              alpha_a = fMinMaXalpha(alpha_a-0.00025)
            endif
            !
          endif
           !
           lvl_top=.false.
           lvl_bottom=.false.
           !
        endif
        !
        if(alpha_a <= lpf_minAlpha)goto 10
        !      
        if(100 < count)goto 100
         gpcd = lvf_originalGPCD
             do i = 1,lvi_DoStop,1
              lvf_GPCD(i)=0
             end do
             do i = 1,lvi_DoStop,1
                !
                if(0 < alpha_a)then
                 lvf_gpcdEstimate=fSESgpcd_discount(alpha_a,gpcd)
                else
                  goto 10
                endif
                !
                lvf_GPCD(i)=lvf_gpcdEstimate
                gpcd(4) = gpcd(3); gpcd(3) = gpcd(2); gpcd(2)= gpcd(1);  gpcd(1)=lvf_GPCD(i)
                !
             end do
                !
               if(lvf_gpcdEstimate < lvf_target)then
                 lvl_top=.true.
                !
                if(lvf_gpcdEstimate+lvf_error > lvf_target)goto 10
                alpha_a=fMinMaXalpha(alpha_a+0.005)
                goto 5
               else if(lvf_gpcdEstimate > lvf_target)then
                 lvl_bottom=.true.
                !
                if(lvf_gpcdEstimate-lvf_error < lvf_target)goto 10
                alpha_a = fMinMaXalpha(alpha_a-0.005)
                goto 5
               endif
            !
     end do
1000        continue
            goto 10
100         continue
            lvl_error=.true.
10        continue
        !
        fSESgpcd_alpha=alpha_a
        !
    return
  end function
  ! -----------
  function fMinMaXalpha(alpha_a) result(alpha_b)
        real:: alpha_a,alpha_b
        real,parameter:: lpf_minAlpha=0.01
        real,parameter:: lpf_maxAlpha=0.99
        !
        alpha_b=max(lpf_minAlpha,min(alpha_a,lpf_maxAlpha))
        !
    return
  end function fMinMaxalpha

    ! --------------------------------------------------------------
    function fSESgpcd_gamma(lvf_AlterGPCD,gpcd,lvf_target,lvl_error)
        !
        ! ----------------------- Types ------------------
        integer :: count
        integer :: i,j,k

        real :: fSESgpcd_gamma,lvf_target,beta_temp
        real :: lvf_AlterGPCD,fSESgpcd_increase,lvf_error
        real :: lvf_gpcdEstimate
        real :: lvf_GPCD(75),gpcd(7),lvf_originalGPCD(7)
        real :: lvf_scaler,lvf_diff,lvf_modAlpha
        real :: gamma,fGamma

        logical :: lvl_top,lvl_bottom,lvl_error
        ! =================================================
        !       
            lvf_error=1
            count=0
            lvf_modAlpha=0
            lvf_scaler= 0.01
            lvf_diff=0.001
            lvl_top=.false. ;lvl_bottom=.false.
            lvl_error=.false.
            !
             do k = 2,7,1
                gpcd(k) = 0.9 * gpcd(k-1)
             end do
            lvf_originalGPCD=gpcd

            do j = 1,1,1
    5        continue
            if(lvl_top .and. lvl_bottom)then
                if(lvf_gpcdEstimate < lvf_target)then
                     lvf_modAlpha=lvf_modAlpha-0.0005
                else 
                     lvf_modAlpha=lvf_modAlpha+0.0005
                endif
              lvl_top=.false.
              lvl_bottom=.false.
            endif
                count=count+1
                beta_temp=lvf_AlterGPCD * lvf_scaler !0.25 
                gamma=fGamma(beta_temp)
                if(250 > count)lvf_error=2
                !
            if(5000 < count)goto 100
             gpcd = lvf_originalGPCD
                 do i = 1,75,1
                  lvf_GPCD(i)=0
                 end do
                 do i = 1,75,1
                    lvf_gpcdEstimate=fSESgpcd_increase(i+2009,gamma,gpcd,lvf_modAlpha)
                    lvf_GPCD(i)=lvf_gpcdEstimate
                    gpcd(4) = gpcd(3); gpcd(3) = gpcd(2); gpcd(2)= gpcd(1);  gpcd(1)=lvf_GPCD(i)
                !
             end do
                !
               if(lvf_gpcdEstimate < lvf_target)then
                 lvl_top=.true.
                 if(lvf_gpcdEstimate+lvf_error > lvf_target)goto 10
                 lvf_scaler = lvf_scaler+lvf_diff
                 lvf_modAlpha=lvf_modAlpha-0.0025
                goto 5
               else if(lvf_gpcdEstimate > lvf_target)then
                 lvl_bottom=.true.
                 if(lvf_gpcdEstimate-lvf_error < lvf_target)goto 10
                 lvf_scaler = max(0.00001,lvf_scaler-lvf_diff)
                 lvf_modAlpha=lvf_modAlpha+0.0025
                goto 5
               endif
            !
        end do
1000    continue
            goto 10
100    continue
             lvl_error=.true.
            ! write(7,*)"Beta Loop",provider
            !
10        continue
            !
        fSESgpcd_gamma=fGamma(beta_temp)
        !
   return
 end function
! -----------


!       Browns simple exponential smoothing alghorithm used in the model
! 
!     ! http://www.duke.edu/~rnau/411avg.htm
!     ! Brown's simple Exponential Smoothing (exponentially weighted moving average)
!     ! alpha determines relative weight among observations. This is a discounted 
!       moving average with the discount factor of 1 - alpha

  ! ------------------------------------
  function fSESgpcd_discount(alpha_n,gpcd)
    !
    ! ------------- Types ------------------
    real :: fSESgpcd_discount
    real :: gpcdEstimate
    real :: alpha_n
    real :: gpcd(7)
    ! ======================================
        !
         gpcdEstimate =0
         fSESgpcd_discount=0
        if(0 < alpha_n)then
          gpcdEstimate = &
            alpha_n*(gpcd(1)+(1.-alpha_n)*gpcd(2)+((1.-alpha_n)**2)*gpcd(3)+((1.-alpha_n)**3)*gpcd(4) )
          fSESgpcd_discount=gpcdEstimate
        endif
        !
    !
    return
  end function
  ! ----------

    ! -----------------------------------
    function fSESgpcd_equalWeight(N,gpcd)
        !
    ! ------------- Types ------------------
    integer :: i,N

    real :: lvf_add,fSESgpcd_equalWeight
    real :: gpcd(10)
    ! ======================================
    !
         lvf_add=0
        do i = 1,N,1
         lvf_add=lvf_add+gpcd(i)
        end do
        !
        fSESgpcd_equalWeight = lvf_add * (1./ N)
        !
    !
    return
  end function
  ! ----------

  ! --------------------
  function fGamma(beta)
    !
    ! ---------------- Types ---------------
    real :: fGamma
    real :: beta
    real, parameter :: alpha_b=0.91872
    real, parameter :: omega= 0.09335 ! minus sign
    ! ======================================
    !
        !
        fGamma = alpha_b - omega*beta
        if(1 <= beta)then
          fGamma = 0.82537
        else
         if(beta <= 0.01)then
           fGamma = 0.91779
         endif
        endif
        !
    !
   return
  end function
  ! ----------

  ! ------------------------------------------------------
  function fSESgpcd_increase(year,gamma,gpcd,lvf_modAlpha)
    !
    ! ------------------ Types ------------------
    integer :: year

    real :: fSESgpcd_increase
    real :: alpha_z,gamma ,lvf_modAlpha
    real :: gpcd(7)
    real :: alphaMax=0.99
    real :: lvf_a,a,lpf_base=0.8 ! 0.8 0.65
    ! ===========================================
    !
        !
         alpha_z=min(alphaMax,lpf_base + lvf_modAlpha)
         a = gpcd(1)    
         !
         lvf_a=year/(a + gamma*year)
        !
         fSESgpcd_increase =0
        fSESgpcd_increase = &
          (alpha_z*lvf_a*gpcd(1)+(1.-alpha_z)*gpcd(2)+((1.-alpha_z)**2)*gpcd(3)+((1.-alpha_z)**3)*gpcd(4) )
        !
    return
  end function
  !-----------


    ! -----------------------------
    ! Estimate evaporation from the
    ! SRP reservoir system
    ! ==================================

    ! --------------------------
    function fEvaporation(state)
        use gm_DataAndSensitivity
        !
        ! ------------ Types -----------------------
        real(8) :: state
        real(8) :: fEvaporation,first,second
 !       real(8), parameter :: a=0.01 ! maf was 0.83
        ! As of 06.21.12 DAS
       ! real, parameter :: lvp_newRate=0.05
       ! real(8), parameter :: b=0.3 ! maf 
       ! real(8), parameter :: m=0.028
        ! ==========================================
        ! 
            ! Used for SVT River reservoirs
            ! -------------
             fEvaporation=0.
!            first=max(a,lvp_newRate*state)
            first=gpf_newRate*state
            !
            !second=m * state + b
            second=gpf_srpSlope * state + gpf_srpIntercept
            !
            fEvaporation=max(first,second)
            !
       !
      return
    end function fEvaporation
    ! -----------------------

    ! -----------------------------
    ! Estimate evaporation from the
    ! SRP reservoir system
    ! =============================

    ! ---------------------------------
    function fEvaporationMonthly(state)
        !
        ! ------------ Types -------------------
        real(8) :: state
        real(8) :: fEvaporation
        real :: evapMonthly,fEvaporationMonthly
        ! ======================================
        ! 
            ! Used for SVT River reservoirs
            ! -------------
             evapMonthly=0.
            evapMonthly=fEvaporation(state)

             fEvaporationMonthly=0
            fEvaporationMonthly=evapMonthly/12
       !
      return
    end function fEvaporationMonthly
    ! ------------------------------
  
   ! -----------------------------------------------
!    function fEvaporation_channel(x,flux)
!       ! -------------- Types and parameters -------
!       real(8) :: x
!       real :: flux
!       real(8) :: fEvaporation_channel
!       real(8), parameter :: b=0.25 ! maf 0.15
!       real(8), parameter :: m=0.07 ! last: 0.015
!       ! ===============================================
!            !
!            ! Used for Colorado River Reservoirs
!            ! ---------------
!             fEvaporation_channel=0.
!            !
!            fEvaporation_channel= m * x + b 
!            !
!      return
!     end function fEvaporation_channel
    ! -----------------------------------------------

    ! ------------------------------
    function fEvaporation_reach(pan)
        !
        ! --------------- Types and parameters ----------
        real :: fEvaporation_reach,pan
        real, parameter :: area=38970000 ! m2
        real, parameter :: unitConvert=1e6
        real, parameter :: cubicMtoAF=0.00081071318212
        ! ===============================================
            !
            ! Used for Colorado River Reach - Powell to Mead
            ! ---------------
             fEvaporation_reach=0.
            fEvaporation_reach= area * pan *cubicMtoAF * (1/ unitConvert)
            !
        !
      return
     end function fEvaporation_reach
    ! ------------------------------

       ! ------------------------------------------
        function fBankStorageReach(lvf_diffStorage)
            !
            ! ------------------ Types -------------------
            real :: lvf_diffStorage
            real :: fBankStorageReach
            real, parameter :: lpf_bankStorageChannel=0.08
            ! ============================================
                !
                fBankStorageReach=lvf_diffStorage*lpf_bankStorageChannel
                !
            !
         return
        end function fBankStorageReach
        ! ----------------------------

    ! ------------------------------------------
    function fEvaporation_Powell(volumeTotal,pan)
        !
        ! ----------------- Types ---------------------
        real :: fEvaporation_Powell
        real :: surfaceArea,pan ! Assume, for now, surface area in acres
        real(8) :: volumeTotal
        real,parameter :: b0=1422.6
        real, parameter :: b1=11421.8
        real, parameter :: b2=0.8066
        real, parameter :: unitConvert=1e6
        real, parameter :: acreToSqM=4046.8564224
        real, parameter :: cubicMtoAF=0.00081071318212
        ! ============================================
            !
             surfaceArea=0
            if(1.895000 < volumeTotal)then
! Wrong number here....! fuc
                if(volumeTotal < 27.865918)then  
                  surfaceArea=b0 + b1 * volumeTotal**b2
                else
                  surfaceArea=168927
                endif
            else
              surfaceArea=20303
            endif
            !
            fEvaporation_Powell=(surfaceArea* acreToSqM * pan) * cubicMtoAF * (1/unitConvert )
            !
        !
     return
    end function fEvaporation_Powell
    ! ------------------------------

    ! -----------------------------------------
    function fEvaporation_Mead(volumeTotal,pan)
        !
        ! ------------------ Types --------------------
        real :: fEvaporation_Mead
        real :: surfaceArea,pan
        real(8) :: volumeTotal
        real,parameter :: b0=17136.2
        real, parameter :: b1=7041.6
        real, parameter :: b2=0.8978
        real, parameter :: unitConvert=1e6
        real, parameter :: acreToSqM=4046.8564224
        real, parameter :: cubicMtoAF=0.00081071318212
        ! =============================================
        !
            !
             surfaceArea=0
            if(2.035000 < volumeTotal)then
                if(volumeTotal < 29.418237)then
                  surfaceArea=b0 + b1 * volumeTotal**b2
                else
                  surfaceArea=144893
                endif
            else
              surfaceArea=28911
            endif
            !                        acre ->   m2      * m  = m3 -> AF -> maf
            fEvaporation_Mead=(surfaceArea * acreToSqM * pan) * cubicMtoAF * (1/unitConvert)
            !
        !
     return
    end function fEvaporation_Mead
    ! ----------------------------

    ! --------------------------------------------------------------------
    ! Series of functions to estimate electrical use for pumping CAP water
    ! to the valley


    ! -----------------------------------------
    ! Horse power for Central Arizona Project
    ! estimates of electrical use for
    ! pumping water up the aqueduct
    ! =========================================

    ! -----------------------------------
    function fHorsePowerCAP(hHt,e,pumped)
        !
        ! ---------------- Types -------------
        real(8):: hHt   ! head height
        real(8):: e
        real(8) :: pumped
        real(8) :: lbsperft3 = 62.4
        real(8) :: ftlbspers = 550
        real(8) :: fHorsePowerCAP
        ! ====================================
        !
            !
            fHorsePowerCAP=0
            fHorsePowerCAP=lbsperft3*hHt*pumped * (1/(ftlbspers*e))
            !
        !
      return
    end function 
    ! ----------

    ! -----------------------------
    function fKilowatthrsFromHP(hp)
        !
        ! ----------- Types -------------
        real(8):: hp
        real(8) :: kWperHP = 0.745699872
        real(8) :: fKilowatthrsFromHP
        !================================
        !
            !
            fKilowatthrsFromHP=0
            fKilowatthrsFromHP=NINT(hp*kWperHP)
            !
        !
      return
    end function 
    ! ----------

    ! --------------------------------------------
    integer function fCftsToacftperannum(days,cft)
        !
        ! ----- Types ----
        integer :: days
        real(8):: cft
        !=================
        !
            !
            fCftsToacftperannum=NINT((cft*86400*days)* (1/43560.000627))
            !
        !
      return
    end function 
    ! ----------

    ! -------------------------------------
    function fAcftperannumTocfts(days,acft)
        !
        ! ------- Types --------------------
        integer :: days

        real(8) :: acft,fAcftperannumTocfts
        !====================================
        !
            !
            fAcftperannumTocfts=0
            fAcftperannumTocfts=(acft*43560.000627)/(86400*days)
            !
        !
      return
    end function
    ! -----------



    ! See subroutine  daysintheyearK(year,T)
    ! found in Kernel.f90
    ! --------------------
    function fLeapYear(yr)
        ! ----- Types ----
        integer :: year,yr

        real :: fLeapYear
        ! ================
        !
            !
            year = yr
            fLeapYear=365
	        if(year == 1948 .OR. year == 1952 .OR. year == 1956 .OR. year == 1960 .OR. year == 1964 & 
                .OR. year == 1968 .OR. year == 1972 .OR. year == 1976 .OR. year == 1980 .OR. year == 1984 &
                .OR. year == 1988 .OR. year == 1992 .OR. year == 1996 .OR. year == 2000 .OR. year == 2004 &
	            .OR. year == 2008 .OR. year == 2012)then;
                fLeapYear=366
            endif
            !
        !
    end function fLeapYear
    ! --------------------

    ! -----------------------------
    function fmonthsFromDOY(yr,doy)
        ! 
        ! ------------------------------ Types ---------------------------
        integer :: yr,year,doy
        integer :: month
        integer :: one,two,three,four,five,six,seven,eight,nine,ten,eleven
        integer :: fmonthsFromDOY

        real :: leap,fLeapYear

        logical :: leapYear
        ! ================================================================
        !
            !
            one=32; two=61; three=92; four=122; five=153 ; six=183; seven=214 ; eight=245
            nine = 275; ten=306; eleven = 336
            !
             year = yr
             leap= fLeapYear(year)
             leapYear=.false.
            !
            if(nint(leap) == 366)LeapYear = .true.
            !
             month=1
            if(LeapYear)then
              if(doy < one) month=1
              if(one <= doy .and. doy < two) month=2
              if(two <= doy .and. doy < three) month=3
              if(three <= doy .and. doy < four) month=4
              if(four <= doy .and. doy < five) month=5
              if(five <= doy .and. doy < six) month=6
              if(six <= doy .and. doy < seven) month=7
              if(seven <= doy .and. doy < eight) month=8
              if(eight <= doy .and. doy < nine) month=9
              if(nine <= doy .and. doy < ten) month=10
              if(ten <= doy .and. doy < eleven) month=11
              if(eleven <= doy) month=12
            else
             if(doy <= one) month=1
              if(one <= doy .and. doy < two-1) month=2
              if(two-1 <= doy .and. doy < three-1) month=3
              if(three-1 <= doy .and. doy < four-1) month=4
              if(four-1 <= doy .and. doy < five-1) month=5
              if(five-1 <= doy .and. doy < six-1) month=6
              if(six-1 <= doy .and. doy < seven-1) month=7
              if(seven-1 <= doy .and. doy < eight-1) month=8
              if(eight-1 <= doy .and. doy < nine-1) month=9
              if(nine-1 <= doy .and. doy < ten-1) month=10
              if(ten-1 <= doy .and. doy < eleven-1) month=11
              if(eleven-1 <= doy) month=12
            endif
            !
            fmonthsFromDOY=month
            !
        !
    end function fmonthsFromDOY
!
    function fSTVerdeReservoirF(x)
        ! ---- Types -----
        real :: x
        real :: fSTVerdeReservoirF
        !real, parameter :: lpf_m=0.00125
        real, parameter :: lpf_m=0.000625
        !real, parameter :: lpf_b=-1
        real, parameter :: lpf_b=0
            !
            if(x < 1600)then
             fSTVerdeReservoirF = lpf_m*x+lpf_b
!                if (x < 800)then
!                 fSTVerdeReservoirF=0.01
!                endif
            else
             fSTVerdeReservoirF = 1.0
            endif
            !          
        !
     return
    end function fSTVerdeReservoirF


   ! --------------------------------------------------------------------
    function fSES_alpha(yearIN,lvf_AlterPumping,lvf_target,lvf_originalPumping,lvl_error)
        !
        ! ------------------ Types ------------------------
        integer :: count
        integer :: i,j,lvi_DoStop

        real :: fSES_alpha,fMinMaXalpha
        real:: lvf_target,alpha_f,alpha_temp
        real :: lvf_AlterPumping,fSESgpcd_discount
        real :: lvf_pumpingEstimate
        real :: lvf_pumping(75),pumping(7),lvf_error,yearIN
        
        real :: lvf_originalPumping(7)
        real :: lvf_simpleSolution(3)
        real,parameter:: lpf_minAlpha=0.01
        !real,parameter:: lpf_maxAlpha=0.99

        logical :: lvl_error
        logical :: lvl_top,lvl_bottom
        ! =================================================
        !
        !
        !   %LET model = a*b**x * x**-c;
        !   1 to 70 %
        !   Using Phoenix as a base estimate, 178 GPCD (i.e., ~ 2009 estimate)
        !   01.22.15   SAS analysis

        ! Borrowing from GPCD for agriculture pumping

        ! y = alpha in percent (must divide input by 100)
        ! x = proportional reduction
        ! ------------------------------------------------------------------------------------
        real, parameter :: a=90.5916
        real, parameter :: b= 0.9969
        real, parameter :: c=-0.0422
!        
!                                             Sum of        Mean               Approx
!            Source                    DF     Squares      Square    F Value    Pr > F
!
!            Model                      3     37398.9     12466.3     162409    <.0001
!            Error                      4      0.3070      0.0768
!            Uncorrected Total          7     37399.3
!
!
!                                                 Approx
!                   Parameter      Estimate    Std Error    Approximate 95% Confidence Limits
!
!                   a               90.5916       0.2824     89.8075     91.3756
!                   b                0.9969     0.000132      0.9965      0.9972
!                   c                0.0422      0.00204      0.0365      0.0479
        !
        lvf_error=10 ! acft
        ! lvi_DoStop; 85 years of GPCD estimates (2085-2010 = 75)
        lvi_DoStop=75
        
        count=0
!        lvf_originalPumping=pumping
        lvl_top=.false. ;lvl_bottom=.false.
        if(0 < yearIN)lvi_DoStop=nint(2085-yearIN)
        
        ! reduction of 1 < 70 %
         alpha_temp=90
        if(0 < abs(lvf_AlterPumping))then
         alpha_temp=(a*b**abs(lvf_AlterPumping) * abs(lvf_AlterPumping)**c )
        else

        endif

         fSES_alpha=alpha_temp
          alpha_f=fMinMaXalpha(alpha_temp*1/100)
         !
     do j = 1,1,1
5     continue
        count=count+1
        !
        if(lvl_top)then
         lvf_simpleSolution(1)=alpha_f
        endif
        if(lvl_top .and. lvl_bottom)then
          lvf_simpleSolution(2)=alpha_f
            !
          if(500 < count)then
            lvf_simpleSolution(3) =  (lvf_simpleSolution(1) +  lvf_simpleSolution(2))*0.5
            alpha_f=fMinMaXalpha(lvf_simpleSolution(3))
          else
            !
            if(lvf_pumpingEstimate < lvf_target)then
              alpha_f=fMinMaXalpha(alpha_f+0.000002)
            else
              alpha_f = fMinMaXalpha(alpha_f-0.000002)
            endif
            !
          endif
          lvl_top=.false.
          lvl_bottom=.false.
        endif
        !
        if(alpha_f <= lpf_minAlpha)goto 10
        !      
        if(1000 < count)goto 100
         pumping = lvf_originalpumping
             do i = 1,lvi_DoStop,1
              lvf_pumping(i)=0
             end do
             do i = 1,lvi_DoStop,1
                !
                lvf_pumpingEstimate=fSESgpcd_discount(alpha_f,pumping)
                lvf_pumping(i)=lvf_pumpingEstimate
                pumping(4) = pumping(3); pumping(3) = pumping(2); pumping(2)= pumping(1);  pumping(1)=lvf_pumping(i)
                !
             end do
                !
               if(lvf_pumpingEstimate < lvf_target)then
                 lvl_top=.true.
                !
                if(lvf_pumpingEstimate+lvf_error > lvf_target)goto 10
                alpha_f=fMinMaXalpha(alpha_f+0.00025)
                goto 5
               else if(lvf_pumpingEstimate > lvf_target)then
                 lvl_bottom=.true.
                !
                if(lvf_pumpingEstimate-lvf_error < lvf_target)goto 10
                alpha_f = fMinMaXalpha(alpha_f-0.00025)
                goto 5
               endif
            !
     end do
1000        continue
            goto 10
100         continue
            lvl_error=.true.
10        continue
        !
        fSES_alpha=alpha_f   
        !
        if(lvl_error)write(7,*)'fSES for pumping error'
    return
  end function
  ! -----------

  ! ------------------------------------
  function fSES_discount(alpha_in,array)
    !
    ! 01.27.15 DAS
    ! Generic SES discount algorithm
    !
    ! ------------- Types ------------------
    real :: fSES_discount
    real :: Estimate
    real :: alpha_in
    real :: array(7)
    ! ======================================
        !
         Estimate =0
        if(0 < alpha_in)then
          Estimate = &
            alpha_in*(array(1)+(1.-alpha_in)*array(2)+((1.-alpha_in)**2)*array(3)+((1.-alpha_in)**3)*array(4) )
          fSES_discount=Estimate
        endif
        !
    !
    return
   end function
  ! ----------

   function fDemandFromPopGPCD(gpcd,pop) result(demand_acft_a)

        ! ------ Types -------------
        real :: demand_acft_a,gpcd,pop
        real(8), parameter :: galperacft=325851.43326
        ! ===============================================
            !
            demand_acft_a = ((gpcd*365)*(1./galperacft))*pop
            !
        !
     return
    end function fDemandFromPopGPCD


   ! -------------------------------------------------------
    function fCreateTarget(baseline,pctChange) result(target)
        !
        ! ---------------- Types ----------------
        real:: pctChange,response
        real:: baseline,target
        real, parameter:: TheMin=50
        real, parameter:: TheMax=1200
        ! =======================================
            ! Both give the same result
            response = (baseline*(1+(pctChange/100)))
            !response = ((pctChange/100)*baseline)+baseline
            target=max(TheMin,min(response,TheMax))
            !
        ! ----
     return
    end function fCreateTarget
!
    ! -------------------------------------------------------
    function fHyperbola(Time,years) result(temp)
        ! --------- types ----------
        integer :: Time
        real :: alpha,beta  ! horizontal asymptote
        real :: slope=0.22609
        real :: intercept = 0.1
        real :: years,temp
        ! ==========================
            ! data window 0.5 to 6 for alpha
            ! R2=0.991; DF=5; MSE=0.0043; F=5794
            ! alpha = 0.22609 * years + 0.1
            ! To reach 90% of asymptote
            !
            ! Time is policy year NOT running calendar year
            ! 04.21.16 das
            ! ------------
            temp =1.0
            beta = 0.1
            beta = 0.94
            if(0 < years)then
             alpha = slope * years + intercept
               if(years < 2)then
               else
              
                temp = Time / (alpha + beta *Time)
               endif
            else
              alpha = 5
              temp = Time / (alpha + beta *Time)
            endif
             !
      return
    end function fHyperbola
    ! -------------------------------------------------------

   ! -------------------------------------------------------
    function fInverseHyperbola(Time) result(temp)
        ! --------- types ----------
        integer :: Time
        real :: alpha,beta  ! horizontal asymptote
        real :: temp
        ! ==========================
            ! data window 0.5 to 6 for alpha
            ! R2=0.991; DF=5; MSE=0.0043; F=5794
            ! alpha = 0.22609 * years + 0.1
            ! To reach 90% of asymptote
            ! 04.21.16 das
            ! ------------
             temp =1.0
             beta = 0.94
             alpha=3.5
            temp = Time / (alpha + beta *Time)
            !
      return
    end function fInverseHyperbola
    ! -------------------------------------------------------

! -------------------------------------------------------
    function fHyperbolaRunoff(Precip) result(temp)
        ! --------- types ---------
        real :: Precip,temp ! horizontal asymptote
        real :: a,b
        ! ==========================
            ! data window 
            ! 
            !
            ! 
            ! 04.28.16 das
            ! ------------
             a = 25
             b = 1
             temp =1.0
            if(0 < Precip)then
             temp = 1 - (Precip / (a + b *Precip))
            endif
            !
      return
    end function fHyperbolaRunoff
    ! -------------------------------------------------------

! -------------------------------------------------------
    function fHyperbolLCLUstatic(Time) result(temp)
        ! --------- types ----------
        integer :: Time
        real :: alpha,beta  ! horizontal asymptote
        real :: temp
        ! ==========================
            ! 
            ! 05.19.16 das
            ! ------------
              alpha = 5
              beta = 0.9
              temp = Time / (alpha + beta *Time)
            !
      return
    end function fHyperbolLCLUstatic
    ! -------------------------------------------------------

    ! -------------------------------------------------------
    subroutine sSpanAndInflection(policy,scenario,LCLU,span,compliance,years,model)
        ! --------------- Types -----------
        integer :: policy,scenario,LCLU,model
        real :: inflection,span,years
        real :: compliance
        real :: strategicCompliance
        !real :: lpf_inputInflection=0
        ! ==================================
            !
            ! Generic Default Values
            ! ===============
            inflection=2040
            model=0
            strategicCompliance=0.30
            !
            select case(LCLU)
            ! Rainwater harvesting, storm water capture, and gray water use
              case(0)
                  select case(scenario)
                    case(0)
                        ! Basic WaterSim Six NOT in the Iwaniec series- for general use
                        select case(policy)
                            ! Rainwater Harvesting Residential
                            case(1)
                              span=15
                              inflection=2040
                            ! 90% of possible homes
                              compliance = 0.9

                            ! Rainwater Harvesting Commercial/Industrial
                            case(2)
                              span=15
                              inflection=2035
                              compliance = 0.9

                            ! Gray water use Residential
                            case(3)
                              span=20
                              inflection=2040
                              compliance = 0.7

                            ! Storm water capture Residential
                            case(5)
                              span=10
                              inflection=2030
                              compliance=0.70
                            ! Storm water capture Commercial
                            case(6)
                              span=15
                              inflection=2035
                              compliance=0.70
                            case default
                              span=15
                              inflection=2035
                              compliance=1.0

                        end select
                    case(1)
                              span=0
                              inflection=2040
                              compliance = 1.0

                        ! Adpative Drought
                        select case(policy)
                            ! Rainwater Harvesting Residential
                            case(1)
                              span=15
                              inflection=2040
                            ! 50% of possible homes, but 50% single family homes, so 25%
                              compliance = 0.5

                            ! Rainwater Harvesting Commercial/Industrial
                            case(2)
                              span=15
                              inflection=2035
                              compliance = 0.5

                            ! Gray water use Residential
                            case(3)
                              span=20
                              inflection=2040
                              compliance = 0.9

                           ! Gray water use Commercial/Industrial
                            case(4)
                              span=20
                              inflection=2040
                              compliance = 0.9

                            ! Storm water capture Residential
                            case(5)
                              span=10
                              inflection=2030
                              compliance=0.90
                            ! Storm water capture Commercial
                            case(6)
                              span=10
                              inflection=2030
                              compliance=0.90
                            case default
                              span=15
                              inflection=2035
                              compliance=1.0
                        end select
                    case(2)
                              span=0
                              inflection=2040
                              compliance = 1.0

                       ! Adaptive Flood
                        select case(policy)
                            ! Rainwater Harvesting Residential
                            case(1)
                              span=15
                              inflection=2035
                            ! 50% of possible homes, but 50% single family homes, so 25%
                              compliance = 0.77

                            ! Rainwater Harvesting Commercial/Industrial
                            case(2)
                              span=15
                              inflection=2035
                              compliance = 0.77

                            ! Gray water use Residential
                            case(3)
                              span=20
                              inflection=2035
                              compliance = 0.77

                           ! Gray water use Commercial/Industrial
                            case(4)
 
                            ! Storm water capture Residential
                            case(5)
                              span=20
                              inflection=2040
                              compliance=1.00

                            ! Storm water capture Commercial
                            case(6)
                              span=20
                              inflection=2040
                              compliance=1.00
                            case default
                              span=15
                              inflection=2035
                              compliance=1.0
                        end select

                    case(3)
                              span=0
                              inflection=2040
                              compliance = 1.0

                       ! Adpative Heat
                        select case(policy)
                            ! Rainwater Harvesting Residential
                            case(1)
                              span=15
                              inflection=2035
                              compliance = 0.5

                            ! Rainwater Harvesting Commercial/Industrial
                            case(2)
                              span=15
                              inflection=2035
                              compliance = 0.5

                            ! Gray water use Residential
                            case(3)
                              span=7
                              inflection=2047
                              compliance = 0.95

                            ! Gray water use Commercial/Industrial
                            case(4)
                              span=10
                              inflection=2055
                              compliance = 0.95

                            ! Storm water capture Residential
                            case(5)

                            ! Storm water capture Commercial
                            case(6)
                            case default
                        end select
                    case(4)
                              span=0
                              inflection=2040
                              compliance = 1.0

                        ! Healthy Harvest Hubs scenario
                        select case(policy)
                            ! Rainwater Harvesting Residential
                            case(1)

                            ! Rainwater Harvesting Commercial/Industrial
                            case(2)
 
                            ! Gray water use Residential
                            case(3)
                              span=15
                              inflection=2035
                              compliance = 1.0

                            ! Gray water use Commercial/Industrial
                            case(4)
                              compliance=0.0
                            ! Storm water capture Residential
                            case(5)
                              span=10
                              inflection=2050
                              compliance=1.0

                            ! Storm water capture Commercial
                            case(6)
                              span=10
                              inflection=2050
                              compliance=1.0

                            case default
                        
                        end select
                    case(5)
                              span=0
                              inflection=2040
                              compliance = 1.0

                        ! Emerald City scenario
                        select case(policy)
                            ! Rainwater Harvesting Residential
                            case(1)
                              span=15
                              inflection=2035
                            ! 50% of possible homes, but 50% single family homes, so 25%
                              compliance = 0.77

                            ! Rainwater Harvesting Commercial/Industrial
                            case(2)
                               span=15
                              inflection=2035
                            ! 50% of possible homes, but 50% single family homes, so 25%
                              compliance = 0.77

                            ! Gray water use Residential
                            case(3)
                              span=20
                              inflection=2040
                              compliance = 0.77

                            ! Gray water use Commercial/Industrial
                            case(4)
                              span=20
                              inflection=2040
                              compliance = 0.77

                            ! Storm water capture Residential
                            case(5)
                              span=10
                              inflection=2050
                              compliance=1.0

                            ! Storm water capture Commercial
                            case(6)
                              span=10
                              inflection=2050
                              compliance=1.0

                            case default
                        
                        end select

                    case(6)
                              span=0
                              inflection=2040
                              compliance = 1.0

                        ! Zero Waste scenario
                        select case(policy)
                            ! Rainwater Harvesting Residential
                            case(1)
                              span=10
                              inflection=2030
                              compliance = 1.0

                            ! Rainwater Harvesting Commercial/Industrial
                            case(2)
                              span=10
                              inflection=2030
                              compliance = 1.0

                            ! Gray water use Residential
                            case(3)
                              span=10
                              inflection=2030
                              compliance = 1.0

                           ! Gray water use Commercial/Industrial
                            case(4)          
                              span=10
                              inflection=2030
                              compliance = 0.8

                            ! Storm water capture Residential
                            case(5)
                              span=20
                              inflection=2040
                              compliance=1.0

                            ! Storm water capture Commercial
                            case(6)
                              span=20
                              inflection=2040
                              compliance=1.0

                            case default
                              span=15
                              inflection=2035
                              compliance=1.0
                        end select

                    case(7)
                      ! Strategic scenario
                       
                        select case(policy)
                            ! Rainwater Harvesting Residential
                            case(1)
                              span=20
                              inflection=2040
                              compliance = strategicCompliance

                            ! Rainwater Harvesting Commercial/Industrial
                            case(2)
                              span=20
                              inflection=2040
                              compliance = strategicCompliance

                            ! Gray water use Residential
                            case(3)
                              span=20
                              inflection=2040
                              compliance = strategicCompliance

                           ! Gray water use Commercial/Industrial
                            case(4)          
                              span=20
                              inflection=2040
                              compliance = strategicCompliance

                            ! Storm water capture Residential
                            case(5)
                              span=20
                              inflection=2040
                              compliance=strategicCompliance

                            ! Storm water capture Commercial
                            case(6)
                              span=20
                              inflection=2040
                              compliance=strategicCompliance



                            case default
                              span=15
                              inflection=2035
                              compliance=1.0
                        end select

                    case(8)
                      ! Base for Strategic scenario (07.20.17 das)
                       
                        select case(policy)
                            ! Rainwater Harvesting Residential
                            case(1)
                              span=20
                              inflection=2040
                              compliance = 0.30

                            ! Rainwater Harvesting Commercial/Industrial
                            case(2)
                              span=20
                              inflection=2040
                              compliance = 0.30

                            ! Gray water use Residential
                            case(3)
                              span=20
                              inflection=2040
                              compliance = 0.30

                           ! Gray water use Commercial/Industrial
                            case(4)          
                              span=20
                              inflection=2040
                              compliance = 0.30

                            ! Storm water capture Residential
                            case(5)
                              span=20
                              inflection=2040
                              compliance=0.30

                            ! Storm water capture Commercial
                            case(6)
                              span=20
                              inflection=2040
                              compliance=0.30



                            case default
                              span=15
                              inflection=2035
                              compliance=1.0
                        end select

                    case default
                    
                  end select

                ! LCLU = 1 = Agriculture
                ! -------------------------------
              case(1)
                  select case(scenario)
   
                     case default
                      span=0
                      inflection=2015 ! zero outs years
                      compliance = 1.0
                      ! Linear
                      model=1
                  end select
              case(2)
                ! High Density Residential, Commercial, and Industrial
                ! -----------------------------------------------------
                select case(scenario)
                    ! HHH
                    case(4)
                      span=10
                      inflection=2030
                      compliance = 1.0
                      ! Logistic
                      model=2

                    case default
                      span=20
                      inflection=2040
                      compliance = 1.0
                      ! Linear
                      model=1
                  end select

              case(3)
                     span=0
                      inflection=2015 ! zero outs years
                      compliance = 1.0
                      ! Linear
                      model=1

              case(4)
                     span=0
                      inflection=2015 ! zero outs years
                      compliance = 1.0
                      ! Linear
                      model=1

              case(5)
                ! Greenway
                ! -------------------------------
                select case(scenario)
                    ! AF
                    case(2)
                      span=10
                      inflection=2050
                      compliance = 1.0
                      ! Logistic
                      model=2

                    case default
                      span=20
                      inflection=2040
                      compliance = 1.0
                      ! Linear
                      model=1
                  end select
              case(6)
                ! Impervious
                ! ------------------------
                  select case(scenario)
                   case(4) ! HHH
                      span=5
                      inflection=2055
                      compliance = 1.0
                      model=2
                    case(5) ! EC
                      span=5
                      inflection=2055
                      compliance = 1.0
                      model=2
                    case default
                      ! Linear
                      span=20
                      inflection=2040
                      compliance = 1.0
                      model=1
                  end select
              case(7) ! Mountain vegetation
                     span=0
                      inflection=2015 ! zero outs years
                      compliance = 1.0
                      ! Linear
                      model=1

              case(8) 
                ! Medium density Residential
                ! -------------------------------
                  select case(scenario)
                    case(4) ! HHH
                      span=20
                      inflection=2040
                      compliance = 1.0
                      model=2
                    case default
                      ! Linear
                      span=20
                      inflection=2040
                      compliance = 1.0
                      model=1
                  end select

              case(9) ! Soil
                     span=0
                      inflection=2015 ! zero outs years
                      compliance = 1.0
                      ! Linear
                      model=1

              case(10) ! Tree
                select case(scenario)
                    case(2) ! AF
                      span=15
                      inflection=2035
                      compliance = 1.0
                      model=2
                    case(4) ! HHH
                      span=20
                      inflection=2040
                      compliance = 1.0
                      model=2
                    case(5) ! EN
                      span=20
                      inflection=2040
                      compliance = 1.0
                      model=2
                    case default
                      ! Linear
                      span=20
                      inflection=2040
                      compliance = 1.0
                      model=1
                endselect
            case(11) ! Wash
                     span=0
                      inflection=2015 ! zero outs years
                      compliance = 1.0
                      ! Linear
                      model=1

            case(12) ! Water
                     span=0
                      inflection=2015 ! zero outs years
                      compliance = 1.0
                      ! Linear
                      model=1

            case(13) ! Low density residential
                ! --------------------------------
              select case(scenario)
                case(1)
                  span=20
                  inflection=2040
                  compliance = 1.0
                  model=2
                case default
                  span=20
                  inflection=2040
                  compliance = 1.0
                  model=2
              end select
              case default


            end select
           ! 
           years=inflection-2015
        !
!write(106,*)policy,scenario,LCLU,span,compliance,years,model
10 format(4(I2,1x),4(F6.1,1x))
      return
    end subroutine sSpanAndInflection
    ! -------------------------------------------------------

       ! -----------------------------------------------------------
        subroutine sRampParameters(PolicyYear,years,InGlobal,InLocal,Out)
            ! ------------- Types -----------
            integer :: PolicyYear
            real :: test,InGlobal,InLocal,Out !,temp
            real :: lvf_localYears,years

            real ::fInverseHyperbola,fHyperbola
            real,parameter :: minOut=0.001
            ! ===============================
                !
                  lvf_localYears=years
                if(InGlobal <= InLocal)then
                  test = max(InGlobal, InLocal* fHyperbola(PolicyYear,lvf_localYears))
                else
                  test = max(InLocal, InGlobal * (1-fInverseHyperbola(PolicyYear)))
                endif
                if(test < minOut)test=0
                ! -----
                !
                Out=min(1.0,test)
                !
          return
        end subroutine sRampParameters
        ! ------------------------------

    ! -------------------------------------------------------
    function fLogistic(Time,a,span,lvf_years) result(tempOut)
        ! --------- types ----------
        integer :: Time
        real :: a,b,c  
        real :: temp,span,lvf_time
        real :: tempOut,lvf_years
        real, parameter :: min=0.01
        real, parameter :: divisor=20
        ! ==========================
            ! 
            ! To reach inflection point
            ! 04.21.16 das
            ! ------------
             c=0.5
             if(divisor < span)then
              c= 1. - (span/divisor)
             else if(span < divisor)then
              c= 1. - (span/divisor)
             else
             endif 
            !
             temp = 1.0
            lvf_time=Time
            if(0 < lvf_years)then
             b = c * lvf_years          
             ! inflecion point = b / c
              if(lvf_years < 2)then
              else
               temp = a / (1 + exp(b - c*lvf_time))
              endif
            else
              b=5
              temp = a / (1 + exp(b - c*Time))
            endif
            ! = max(min,temp)
            ! Crude, but the previous code returned rounding values
            ! with no zero value available
             tempOut=0
            if(min < temp)tempOut = temp
            !
            if(span < 1)tempOut = 0
      return
    end function fLogistic
    ! -------------------------------------------------------

    ! --------------------------------------------------
    function fInverseLogistic(Time,a,years) result(temp)
        ! --------- types ----------
        integer :: Time
        real :: a,b,c  
        real :: years,temp
        real, parameter :: min=0.001
        real, parameter :: thresh=0.995
        ! ==========================
            ! 
            ! To reach inflection point
            ! 04.21.16 das
            ! ------------
             c=0.5
             !temp = 1.0
             temp = 0.0
            if(0 < years)then
             b = c * years           
             ! inflecion point = b / c
              if(years < 2)then
              else
               temp = 1 - (a / (1 + exp(b - c*Time)))
              endif
            else
              b=5
              temp = 1 - (a / (1 + exp(b - c*Time)))
            endif
            !
            temp = max(min,temp)
            if(thresh < temp)temp=1.0
            !
      return
    end function fInverseLogistic
    ! ---------------------------

    ! -------------------------------------------------------
    function fLogisticLCLUstatic(Time) result(temp)
        ! --------- types ----------
        integer :: Time
        real :: b  
        real :: temp
        real, parameter :: a=1
        real,parameter :: c=0.2
        real, parameter::years=25
        ! ==========================
            ! 
            ! To reach inflection point
            ! 04.21.16 das
            ! ------------
              temp = 1.0
             b = c * years           
             ! inflecion point = b / c
             temp = a / (1 + exp(b - c*Time))
            !
      return
    end function fLogisticLCLUstatic
    ! -------------------------------------------------------

    ! -------------------------------------------------------
    function fLogisticLCLUspan(Time,a,span,lvf_years) result(tempOut)
        ! --------- types ----------
        integer :: Time
        real :: a,b,c,span  
        real :: temp,lvf_years
        real :: tempOut
      !  real, parameter :: a=1
       ! real,parameter :: c=0.2
       ! real, parameter::years=25
        real, parameter :: divisor=20
        ! ==========================
            ! 
            ! To reach inflection point
            ! 04.21.16 das
            ! ------------
             c=0.5
             if(divisor < span)then
              c= 1. - (span/divisor)
             else if(span < divisor)then
              c= 1. - (span/divisor)
             else
             endif 

              temp = 1.0
            if(0 < lvf_years)then
             b = c * lvf_years           
             ! inflecion point = b / c
             temp = a / (1 + exp(b - c*Time))
            else
              b=5
              temp = a / (1 + exp(b - c*Time))
            endif
            !
            ! = max(min,temp)
            ! Crude, but the previous code returned rounding values
            ! with no zero value available
             tempOut=0
            if(0.01 < temp)tempOut = temp
            !
            if(span < 1)tempOut = 0
            !
      return
    end function fLogisticLCLUspan
    ! -------------------------------------------------------

            ! --------------------------------
            function fsumLCLUdemand(Array,j) result(temp)
               use gm_GlobalData
              ! ----------- types -----------
              integer :: i,j
              real :: temp
              real :: array(gpi_LULC,gvi_maxProV)
              ! =============================
                !
                 temp=0
                do i = 2,gpi_LULC,1
                 temp=temp+Array(i,j)
                end do
                !
              return
            end function fsumLCLUdemand
            ! -------------------------

           ! =====================================
            function fLCLUefficiencies(Time,amount,lvf_modifier) result(result)
                integer :: Time,amount
                real :: lvf_modifier
                real :: b  
                real :: temp,result
                real, parameter :: a=1
                real,parameter :: c=0.2
                real, parameter:: years=25
                real :: lvf_y2
                real,parameter :: lvf_Y1=1
                real, parameter :: lpf_defaultEff=30
                ! ==========================
                    ! 
                    ! To reach inflection point
                    ! 04.21.16 das
                    ! ------------
                      temp = 1.0
                     b = c * years           
                     ! inflecion point = b / c
                     temp = a / (1 + exp(b - c*Time))
                    !
                    ! Amount in percent as input
                    lvf_y2=amount*0.01
                    if(amount .ne. 1)lvf_y2=max(0,(amount- lvf_modifier-lpf_defaultEff)*0.01)
                    ! lpf_modfier added on 06.20.18 for multi-family units
                    result= (lvf_Y1 + (temp * (lvf_y2-lvf_Y1)))
                    !
              return
            end function fLCLUefficiencies
            ! =====================================

        Module lms_RainfallFactor
            use gm_ModelControl
            use gm_GlobalData

            !    implicit implied
            !
            ! -----
            include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
            !
                integer, private :: x
                real, private :: m,b
            !
         Contains
            !

            ! -------------------------------------------
            ! Called by 
            ! 
            ! ================================================
            function fRainfallFactor(i,gvi_StartF,year,lvi_CF)
                ! -------------- Types ------------------
                integer :: i,gvi_StartF
                integer :: lvi_CF,year
                real :: fVariableFactorX
                real :: lvf_equationBase,fRainfallFactor
                ! =======================================
                    !
                    fRainfallFactor=1.
                    !
                    if(gvl_start)then
                     call RainfallParms(lvi_CF,year-gvi_StartF,m,b)
                       gvf_LinearRainfallFactor(1)=m
                       gvf_LinearRainfallFactor(2)=b
                       gvf_LinearRainfallFactor(3)=lvi_CF*0.01
                    endif
                        x=i
                        x=fVariableFactorX(i,gvi_StartF-1)
                      if(x < 1)then
                      else
                        m = gvf_LinearRainfallFactor(1)
                        b = gvf_LinearRainfallFactor(2)
                        !
                        lvf_equationBase=fLinear(m,x,b)
                        !
                          fRainfallFactor= lvf_equationBase
                        !
                      endif
                    !
              return
            end function fRainfallFactor
            ! ---------------------------

            ! ---------------------
            Function fLinear(m,x,b)
                !
                ! --- Types -------
                integer :: x

                real :: m,b
                real :: fLinear
                ! =================
                ! 
                fLinear=m*x+b
                !
            end Function fLinear
            ! ------------------

            ! ---------------------------------------------
            subroutine RainfallParms(reduction,lvi_year,m,b)
                !
                ! --- Types ------------------
                integer :: reduction,lvi_year
                real :: m,b
                ! ============================
                !
                    !
                     m=0.
                    m = slope(reduction*0.01,lvi_year)
                     b=0.
                    b = intercept(reduction*0.01,m,lvi_year)
                    !
                !
              return
            end subroutine RainfallParms
            ! -------------------------

            ! -------------------
            Function slope(y2,x2)
                !
                ! ----- Types ---------
                integer :: x2

                real :: y2
                real :: slope,temp
                ! =====================
                !
                    !
                     temp=0
                     slope=0
                    if(y2 < 1)then
                      temp = (1-y2)/(1-x2)
                    else
                     temp =  (-(1-y2))/(x2-1)
                    endif
                    !       
                    slope=temp
                    !
                 !
            End function slope
            ! ----------------

            ! -----------------------
            Function intercept(y,m,x)
                !
                ! -------- Types -------
                integer :: x

                real :: y,m
                real :: stage,intercept
                ! ======================
                !
                    !
                     stage=0.
                    stage=m*x
                    !
                     intercept=0.
                    if(stage < 0)then
                     intercept = y+abs(stage)
                    else
                     intercept = y-stage
                    endif
                    !
                !
            End function Intercept
            ! --------------------
        !
        End Module lms_RainfallFactor
        ! --------------------------
        !
    ! -----------------------------------------
    subroutine RainfallFactor(x,RainfallFactorOut)
     use lms_RainfallFactor
        !
        ! ----------- Types ---------
        integer :: x,lvi_End,lvi_CF
        integer:: gvi_StartFactor
        real :: RainfallFactorOut
        ! ===========================
        !
            ! Assumes that
            lvi_End=2060
            gvi_StartFactor=2010
            !
            lvi_CF=nint(gvf_rainfallfactor)
            RainfallFactorOut=fRainfallFactor(x,gvi_StartFactor,lvi_End,lvi_CF)
            if(RainfallFactorOut < 0.000001)RainfallFactorOut=0.01
        !
      return
    end subroutine RainfallFactor
    ! -----------------------------

!
!       Code Parking
!
    ! -------------------
!       function fLotSize(lvf_areaSquareFeet)
!            real :: fLotSize
!            real :: lvf_areaSquareFeet
!            real :: lvf_areaSqMeters
!            real, parameter :: lpf_SqFeetToSqMeters = 0.09290304
!                !
!                 fLotSize=0
!                lvf_areaSqMeters=lvf_areaSquareFeet*lpf_SqFeetToSqMeters
!                fLotSize=lvf_areaSqMeters
!                !
!          return
!        end function fLotSize
!        function fETm3(lvf_ETmeters,lvf_areaSqFeet,lvf_max)
!            ! --------------------- Types --------------------
!            real :: fETm3
!            real :: fLotSize
!            real :: lvf_ETmeters,lvf_max
!            real :: lpf_simple=0.793
!            real :: lvf_m3
!            real :: lvf_flux,lvf_flux_In
!            real, parameter :: lpf_slopeCoefficient=2.0675
!            real, parameter :: lpd_acfttokgal=325.85143326
!            real,parameter :: lpd_m3Toacft=0.00081071318212
!                ! --------------------------------------------
!                ! 
!                 lvf_fluxIn=0
!                lvf_fluxIn=nint((lpd_acfttokgal*lvf_max) * (1/lpf_slopeCoefficient))
!
!                fETm3 = lvf_ETmeters * fLotSize(lvf_areaSqFeet)
!                lvf_m3=lvf_max*(1/lpd_m3Toacft)
!                !
!                 fETm3 = min(lpf_simple * lvf_m3,fETm3)
!                !
!          return
!        end functionfETm3

!
! =================================================================================================
! E.O.F. Functions.f90