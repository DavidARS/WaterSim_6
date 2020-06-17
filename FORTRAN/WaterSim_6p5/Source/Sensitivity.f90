Module lms_sensitivity
  use gm_ModelControl
   use gm_GlobalData
    use gm_DataAndSensitivity
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_6\WaterSimDCDC.txt"
    ! 
  contains
     !
     ! --------------------------
      subroutine Sensitivity(T)
            ! - Type Constructs -
            type(runTime)T
            ! ===================
            !
            call openFiles_sen(T)

            !
      return
      end subroutine Sensitivity
      ! ----------------------------
        ! ----------------------
        subroutine openFiles_sen(T)
            !
            ! ------------- Types --------------
             character(len=200) :: lvc_DPath=' '
            ! ==================================
            ! - Type Constructs -
            type(runTime)T
            ! ===================
            !
                !
                if(gpl_release)then
                  lvc_DPath=trim(gvc_DPath)
                else
                  lvc_DPath=gvc_Path
                endif
                !
                module="lm_Sensitivity"
                !
                Infile='App_Data\Data\Sensitivity.txt'; LU=9
                if(T%year.EQ.T%startyear)then
                    ! keep the unit open until I have completed the analyses
                 if(T%policy%inSensitivity)then
                 else
                  if(T%policy%openFile)then
                   call openFiles(module,lvc_DPath,Infile,LU)
                   call readFiles_sen(T,module,Infile,LU)
                    T%policy%openFile=.false.
                  endif
                 endif
                endif
                !
              return
        end subroutine openFiles_sen
       ! ---------------------------

        ! -----------------------------------------
        subroutine readFiles_sen(T,module_,file_,LU_)
            !
            ! ----------------- Types -----------------
            integer :: LU_,ios
            character(len=25) :: module_
            character(len=50) :: file_
            ! =========================================
            ! - Type Constructs -
            type(runTime)T
            ! ===================

              ! 
!              LU=9
              read(LU_,*,err=1,iostat=ios)T%Policy%variable,T%Policy%increment,T%Policy%lowValue,T%Policy%highValue
            !  close(LU_)
            !
1           continue
            if(ios >0)then
             goto 1000
            endif
            !
         return
          1000 continue
               if(gvl_writeLog)then
                    string=35
                    LU=0
                    call sStrings(string,errorString)
                    call eWrite(errorString,LU)
                endif
              gvl_errorFlag=.false.
             !
            call sErrorCodes(module_,file_,LU_)
            !
        end subroutine readFiles_sen
        ! ------------------------

end module lms_sensitivity

    ! ---------------------------------------------
    Subroutine sensitivity_(T)
       use lms_sensitivity
        !
        ! - Type Constructs -
        type(runTime)T
        ! ===================
        !
        call Sensitivity(T)
        !
    end subroutine sensitivity_
    ! ---------------------------

    ! -----------------------------
    subroutine readOpenFile_Sen(T)
       use lms_sensitivity
        ! ---------- types ---------
        integer :: LU_,ios
        ! ==========================

        ! - Type Constructs -
        type(runTime)T
        ! ===================
            !
            module="lm_Sensitivity"
            Infile='App_Data\Data\Sensitivity.txt'; LU_=9
            !
              read(LU_,*,err=1,iostat=ios)T%Policy%variable,T%Policy%increment,T%Policy%lowValue,T%Policy%highValue
            if(gvl_APIcleared)then
             gvf_increment=T%Policy%increment
            endif
            if(T%Policy%variable == "done")then
             T%policy%finished=.true.
             goto 1000
            endif
1           continue
            if(ios >0)then
             goto 1000
            endif
            !
      return
1000   continue
           if(gvl_writeLog)then
                    string=35
                    LU=0
                    call sStrings(string,errorString)
                    call eWrite(errorString,LU_)
                endif
              gvl_errorFlag=.false.
             !
            call sErrorCodes(module,Infile,LU_)
    end subroutine readOpenFile_Sen
    ! --------------------------------

    ! --------------------------------
    subroutine updateValue(T,NewValue)
       use lms_sensitivity
        ! ----------- types ---------
        real :: NewValue,temp
        ! ===========================
        ! - Type Constructs -
        type(runTime)T
        ! ===================
           !
               NewValue=0
               temp=0
            !
              NewValue=T%Policy%lowValue+gvf_increment
              gvf_increment=gvf_increment+T%Policy%increment
              temp=T%Policy%lowValue+gvf_increment
            !
            if(T%Policy%highValue < temp)then
             T%policy%finished=.true.
            endif
        !
      return
    end subroutine updateValue
    ! -------------------------

    ! -------------------------
    subroutine embeddedCall(T)
        !use gm_GlobalData
        use gm_ModelControl
        use gm_DataAndSensitivity
        !
        ! ------------ Types -------------
        real :: NewValue
        ! ================================
        ! - Type Constructs -
        type(runTime)T
        ! ===================
            !
            if(gpl_runSensitivity)then
              if(T%year.EQ.T%startyear)then
                if(T%atStartOfProviderLoop)then
                     ! set defaults
                     call setDefaults()
                     !
                    if(T%Policy%variable == "gpf_AgEvap")then
                     !gpf_increment=T%Policy%increment
                      call updateValue(T,NewValue)
                      gpf_AgEvap=NewValue
                    endif
                    if(T%Policy%variable == "gpf_AgToVadose")then
                      call updateValue(T,NewValue)
                       gpf_AgToVadose=NewValue
                    endif
                    if(T%Policy%variable == "gpf_newRate")then
                      call updateValue(T,NewValue)
                       gpf_newRate=NewValue
                    endif
                    if(T%Policy%variable == "gpf_srpSlope")then
                       call updateValue(T,NewValue)
                       gpf_srpSlope=NewValue
                    endif
                    if(T%Policy%variable == "gpf_srpIntercept")then
                      call updateValue(T,NewValue)
                       gpf_srpIntercept=NewValue
                    endif
                    if(T%Policy%variable == "gpf_panMead")then
                      call updateValue(T,NewValue)
                       gpf_panMead=NewValue
                    endif
                    if(T%Policy%variable == "gpf_panReach")then
                      call updateValue(T,NewValue)
                       gpf_panReach=NewValue
                    endif
                    if(T%Policy%variable == "gpf_panPowell")then
                      call updateValue(T,NewValue)
                       gpf_panPowell=NewValue
                    endif
                    if(T%Policy%variable == "gpf_bankStorage")then
                      call updateValue(T,NewValue)
                       gpf_bankStorage=NewValue
                    endif
                    if(T%Policy%variable == "gpf_gallonsPerMinute")then
                      call updateValue(T,NewValue)
                       gpf_gallonsPerMinute=NewValue
                    endif
                    if(T%Policy%variable == "gpf_SWTPefficiency")then
                      call updateValue(T,NewValue)
                       gpf_SWTPefficiency=NewValue
                    endif
                    if(T%Policy%variable == "gpf_WSefficiency")then
                      call updateValue(T,NewValue)
                       gpf_WSefficiency=NewValue
                    endif
                    if(T%Policy%variable == "gpf_WWsourceEfficiency")then
                      call updateValue(T,NewValue)
                       gpf_WWsourceEfficiency=NewValue
                    endif
                    if(T%Policy%variable == "gpf_WWTPefficiency")then
                      call updateValue(T,NewValue)
                       gpf_WWTPefficiency=NewValue
                    endif
                    if(T%Policy%variable == "gpf_RWWTPefficiency")then
                      call updateValue(T,NewValue)
                       gpf_RWWTPefficiency=NewValue
                    endif
                    if(T%Policy%variable == "gpf_ROefficiency")then
                      call updateValue(T,NewValue)
                       gpf_ROefficiency=NewValue
                    endif
                    if(T%Policy%variable == "gpf_RecEfficiency")then
                      call updateValue(T,NewValue)
                       gpf_RecEfficiency=NewValue
                    endif
                    if(T%Policy%variable == "gpf_RateComLeak")then
                      call updateValue(T,NewValue)
                       gpf_RateComLeak=NewValue
                    endif
                    if(T%Policy%variable == "gpf_RateIndLeak")then
                      call updateValue(T,NewValue)
                       gpf_RateIndLeak=NewValue
                    endif
                    if(T%Policy%variable == "gpf_RateResLeak")then
                      call updateValue(T,NewValue)
                       gpf_RateResLeak=NewValue
                    endif
                    if(T%Policy%variable == "gpf_showersBathPCT")then
                      call updateValue(T,NewValue)
                       gpf_showersBathPCT=NewValue
                    endif
                    if(T%Policy%variable == "gpf_gallonsPerFlush")then
                      call updateValue(T,NewValue)
                       gpf_gallonsPerFlush=NewValue
                    endif
                   if(T%Policy%variable == "gpf_flushesPerDay")then
                      call updateValue(T,NewValue)
                       gpf_flushesPerDay=NewValue
                    endif
                    if(T%Policy%variable == "gpf_evapotrans")then
                      call updateValue(T,NewValue)
                       gpf_evapotrans=NewValue
                    endif
                    if(T%Policy%variable == "gpf_BWpercent")then
                      call updateValue(T,NewValue)
                       gpf_BWpercent=NewValue
                    endif
                    if(T%Policy%variable == "gpf_modLowerThreshold")then
                      call updateValue(T,NewValue)
                       gpf_modLowerThreshold=NewValue
                    endif
                    if(T%Policy%variable == "gpf_upperThreshold")then
                      call updateValue(T,NewValue)
                       gpf_upperThreshold=NewValue
                    endif
                    if(T%Policy%variable == "gpf_AgWaterUse")then
                      call updateValue(T,NewValue)
                       gpf_AgWaterUse=NewValue
                    endif
                    if(T%Policy%variable == "gpf_propSaveVerdeStorage")then
                      call updateValue(T,NewValue)
                       gpf_propSaveVerdeStorage=NewValue
                    endif
                    if(T%Policy%variable == "gpf_threshStorage")then
                      call updateValue(T,NewValue)
                       gpf_threshStorage=NewValue
                    endif
                    if(T%Policy%variable == "gpf_ratioVerde")then
                      call updateValue(T,NewValue)
                       gpf_ratioVerde=NewValue
                    endif
                    if(T%Policy%variable == "gpf_addFlowThreshold")then
                      call updateValue(T,NewValue)
                       gpf_addFlowThreshold=NewValue
                    endif
                    if(T%Policy%variable == "gpf_bankStoragePowell")then
                      call updateValue(T,NewValue)
                       gpf_bankStoragePowell=NewValue
                    endif
                    if(T%Policy%variable == "gpf_bankStorageMead")then
                      call updateValue(T,NewValue)
                       gpf_bankStorageMead=NewValue
                    endif
                endif
              endif
            endif
            !
      return
    end subroutine embeddedCall
    ! ----------------------------
    
    ! ----------------------------
    subroutine setDefaults()
      use gm_DataAndSensitivity
        !
        gpf_AgEvap=gdf_AgEvap
        gpf_AgToVadose=gdf_AgToVadose
        gpf_newRate   =gdf_newRate
        gpf_srpSlope  = gdf_srpSlope
        gpf_srpIntercept=gdf_srpIntercept
        gpf_panMead=gdf_panMead
        gpf_panReach=gdf_panReach
        gpf_panPowell=gdf_panPowell
        gpf_bankStorage=gdf_bankStorage
        gpf_gallonsPerMinute=gdf_gallonsPerMinute
        gpf_SWTPefficiency=gdf_SWTPefficiency
        gpf_WSefficiency=gdf_WSefficiency
        gpf_WWsourceEfficiency=gdf_WWsourceEfficiency
        gpf_WWTPefficiency=gdf_WWTPefficiency
        gpf_RWWTPefficiency=gdf_RWWTPefficiency
        gpf_ROefficiency=gdf_ROefficiency
        gpf_RecEfficiency=gdf_RecEfficiency
        !
        gpf_RateComLeak=gdf_RateComLeak
        gpf_RateIndLeak=gdf_RateIndLeak
        gpf_RateResLeak=gdf_RateResLeak
        gpf_showersBathPCT=gdf_showersBathPCT
        gpf_gallonsPerFlush=gdf_gallonsPerFlush
        gpf_flushesPerDay=gdf_flushesPerDay
        gpf_evapotrans=gdf_evapotrans

        gpf_BWpercent=gdf_BWpercent
        !
        gpf_modLowerThreshold=gdf_modLowerThreshold
        gpf_upperThreshold=gdf_upperThreshold
        gpf_AgWaterUse=gdf_AgWaterUse
        gpf_propSaveVerdeStorage=gdf_propSaveVerdeStorage
        gpf_ratioVerde=gdf_ratioVerde
        gpf_addFlowThreshold=gdf_addFlowThreshold
        !
        gpf_bankStoragePowell=gdf_bankStoragePowell
        gpf_bankStorageMead=gdf_bankStorageMead
      return
    end subroutine setDefaults
    ! ----------------------------

    ! ----------------------------
! E.O.F.  