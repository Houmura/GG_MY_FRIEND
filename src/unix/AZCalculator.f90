program azcalculator

! --------------------------------------------------
!   碧蓝航线活动毕业概率计算器  [version 2.0]
!    
!
!   2021.12.17
!
!   Nothing. All rights reserved.'
! --------------------------------------------------

    use ui
    use option
    use cl

    implicit none
    logical :: is_forward, is_inversed
    
    
    if (command_argument_count()==0) then
        call print_tool_info()
        do while (.TRUE.)
            call main_loop()
        end do
    else
        call print_tool_info()
    
        is_forward = .FALSE.
        is_inversed = .FALSE.

        is_forward = have_option('-f')
        is_inversed = have_option('-i')

        if ((is_forward .eqv. .TRUE.) .AND. (is_inversed .eqv. .TRUE.)) &
write(*,*)" 参数 -f 和 -i 有且只能存在一个！ 请检查命令行输入参数"
        if ((is_forward .eqv. .FALSE.) .AND. (is_inversed .eqv. .FALSE.)) &
write(*,*)" 参数 -f 或 -i 必须给出！ 请检查命令行输入参数"
        if (is_forward .eqv. .TRUE.) call forward_calculation_cl()
        if (is_inversed .eqv. .TRUE.) call inversed_calculation_cl()

        
    end if
        

end program azcalculator


