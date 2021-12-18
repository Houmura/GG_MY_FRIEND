
use core, only: calculate_probability
use option
!use inverse

implicit none

! 公共变量声明区
integer,dimension(1) :: times                       !抽卡次数
integer,dimension(1) :: vars                        !变量数
real,dimension(1) :: given_prob                     !给定概率
real,allocatable,dimension(:) :: probabilities      !各卡片的概率数组(小数形式)




subroutine inversed_calculation(vars)

    implicit none

    integer,dimension(:),intent(in) :: vars
    integer :: max_times = 9999
    integer :: i, times, status
    logical :: found_solution = .FALSE.
    real :: current_prob
    character(100) :: arg


    call show_inv_config(vars(1),probabilities)


    if (vars(1) >= 5) call percentage2decimal(probabilities(5),pe)
    if (vars(1) >= 4) call percentage2decimal(probabilities(4),pd)
    if (vars(1) >= 3) call percentage2decimal(probabilities(3),pc)
    if (vars(1) >= 2) call percentage2decimal(probabilities(2),pb)
    if (vars(1) >= 1) call percentage2decimal(probabilities(1),pa)

    do i = 0, max_times
        if (vars(1)==1) then 
            call calculate_probability(i,pa,current_prob)
        else if ((vars(1)==2)) then
            call calculate_probability(i,pa,pb,current_prob)
        else if ((vars(1)==3)) then
            call calculate_probability(i,pa,pb,pc,current_prob)
        else if ((vars(1)==4)) then
            call calculate_probability(i,pa,pb,pc,pd,current_prob)
        else if ((vars(1)==5)) then
            call calculate_probability(i,pa,pb,pc,pd,pe,current_prob)
        else
            error stop "Case that variables are more then 5 currently is not supported!"
        end if

        

        if (current_prob < given_prob(1)) then
            write(*,*)" SOLUTION FOUND, TIMES= ",i
            exit
        end if
    end do

        !call show_probability(vars(1),given_prob(1))
    
end subroutine inversed_calculation




subroutine show_inv_config(given_prob,probabilities)
    implicit none
    integer, intent(in) :: given_prob
    real, dimension(:),intent(in) :: probabilities
    integer :: k
    character(5),allocatable,dimension(:) :: probs_chars

    allocate(probs_chars(given_prob))
    do k = 1,vars
        write(probs_chars(k),'(f5.2)') probabilities(k)
    end do

    write(*,*) ""
    write(*,*) "┌------------------------------------------------┐"
    write(*,*) "                                                    "
    write(*,*) "                    输入的配置                      "
    write(*,*) "                                                    "
    write(*,*) "                                                    "
    write(*,*) "    抽卡概率期望：",given_prob,"							    " 
    write(*,'(1x,A)',advance='no') "    设定的概率："
    write(*,'(*(A))',advance='no') probs_chars//"%  "
    write(*,*) ""
    write(*,*) "└------------------------------------------------┘"
    write(*,*) ""

    deallocate(probs_chars)

end subroutine show_inv_config



subroutine print_given_prob_setting()

    write(*,*) ""
    write(*,*) "┌------------------------------------------------┐"
    write(*,*) "                                                    "
    write(*,*) "                 确定抽卡期望概率                      "
    write(*,*) "                                                    "
    write(*,*) "                                                    "
    write(*,*) "    请输入期望的概率：                              "
    write(*,*) "                                                    "
    write(*,*) "    按[回车键]确认输入      任意时刻可输入Q退出程序 "
    write(*,*) "└------------------------------------------------┘"
    write(*,*) ""

end subroutine print_given_prob_setting 