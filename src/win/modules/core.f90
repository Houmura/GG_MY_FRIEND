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
    integer, intent(in)             :: vars     ! �������� 
    integer, intent(in)             :: n        ! �鿨����
    real, dimension(*), intent(in)  :: probs    ! ��ſ�Ƭ���ʵ�����(С����ʽ)
    real, intent(inout)             :: p        ! δ����ĸ���(С����ʽ)

    integer                         :: i        ! ѭ������
    real                            :: Xn       
    real,allocatable, dimension(:) :: results  ! ��ŵ��ƹ�ʽ���


    allocate(results(vars)) 
        
    Xn = (1 - probs(1)) ** n                    ! n = 1
    results(1) = Xn

    do i = 2, vars
        results(i) = results(i-1) * (1 - (1 - probs(i)) ** n) + (1 - probs(i)) ** n
    end do

    p = results(vars)

end subroutine calculate_probability

end module
