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
    integer, intent(in)             :: vars     ! 变量个数 
    integer, intent(in)             :: n        ! 抽卡次数
    real, dimension(*), intent(in)  :: probs    ! 存放卡片概率的数组(小数形式)
    real, intent(inout)             :: p        ! 未抽齐的概率(小数形式)

    integer                         :: i        ! 循环变量
    real                            :: Xn       
    real,allocatable, dimension(:) :: results  ! 存放递推公式结果


    allocate(results(vars)) 
        
    Xn = (1 - probs(1)) ** n                    ! n = 1
    results(1) = Xn

    do i = 2, vars
        results(i) = results(i-1) * (1 - (1 - probs(i)) ** n) + (1 - probs(i)) ** n
    end do

    p = results(vars)

end subroutine calculate_probability

end module
