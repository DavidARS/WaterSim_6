module Module1

    ! place global type definitions, generic interfaces, 
    ! variable declarations and initializations here

contains

    subroutine MySub (Ivar, Rvar)
        implicit none
        !
        ! variable declarations
        !
        integer, intent (in out) :: Ivar
        real, intent (in out) :: Rvar
        !
        ! executable statements
        !
    end subroutine MySub

    function MyFun (Ivar, Rvar) result (Ires)
        implicit none
        !
        ! variable declarations
        !
        integer :: Ires
        integer, intent (in out) :: Ivar
        real, intent (in out) :: Rvar
        !
        ! executable statements
        !
        Ires = 0   ! Assign function result 
    end function

end module Module1

