!
! File is Agriculture.f90
!
!   This Module models water for the environment pool and fluxes.  And, it controls
! some of the provider water balance for municipal water providers
!
! Global OUTPUTS:  
!
! Local OUTPUTS:
!               
!   
! Local INPUTS:
!
! created on 06.25.12
!
!
! david arthur sampson
! last write was: 06.25.12
! =================================================================================================
!
   ! ---------------------------------------------------
     subroutine sWaterForEnv(T)
      use lms_CitiWaterBudgets
       use gm_ModelControl
         integer :: i
 
         type(runTime)T
         ! ============
          !
          ! ======================
           do i = 1,gvi_Providers,1
            !
                
           end do
            !
      return
    end subroutine sWaterForEnv
    ! -----------------------

!
! =================================================================================================
! E.O.F.