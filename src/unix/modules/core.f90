!   
!   Probability Core Solver
!

module core

contains

subroutine calculate_probability(vars, n, probs, p)
    implicit none 

!   
!   Inclusion and Exclusion Principle
!   
!                       X(n)                n = 1
!   F(n) = 
!           F(n-1) * [(1 - X(n)] + X(n)     n > 1
!

    ! Arguments
    integer, intent(in)             :: vars     ! ±äÁ¿¸öÊý 
    integer, intent(in)             :: n        ! ³é¿¨´ÎÊý
    real, dimension(*), intent(in)  :: probs    ! ´æ·Å¿¨Æ¬¸ÅÂÊµÄÊý×é(Ð¡ÊýÐÎÊ½)
    real, intent(inout)             :: p        ! Î´³éÆëµÄ¸ÅÂÊ(Ð¡ÊýÐÎÊ½)

    integer                         :: i        ! Ñ­»·±äÁ¿
    real                            :: Xn       
    real,allocatable, dimension(:) :: results  ! ´æ·ÅµÝÍÆ¹«Ê½½á¹û


    allocate(results(vars)) 
        
    Xn = (1 - probs(1)) ** n                    ! n = 1
    results(1) = Xn

    do i = 2, vars
        results(i) = results(i-1) * (1 - (1 - probs(i)) ** n) + (1 - probs(i)) ** n
    end do

    p = results(vars)

end subroutine calculate_probability

end module
