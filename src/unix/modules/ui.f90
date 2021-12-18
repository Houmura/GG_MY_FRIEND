!
!   User Interface (Command-Line)
!

module ui

    use core, only: calculate_probability
    use option
    !use inverse

    implicit none
    
    ! 公共变量声明区
    integer,dimension(1) :: times                       !抽卡次数
    integer,dimension(1) :: vars                        !变量数
    integer,dimension(1) :: mode                  !计算模式
    real,dimension(1) :: given_prob                     !给定概率
    real,allocatable,dimension(:) :: probabilities      !各卡片的概率数组(小数形式)
    
    real :: pa, pb, pc, pd, pe              !出现各卡的概率(小数形式)
    !real :: p_a, p_b, p_c, p_d, p_e         !出现各卡的概率(百分比形式)

    !integer :: arg_pos = 0               ! Number of arguments
    !logical :: is_forward, is_inversed   ! Function flags
    !character(len=64) :: arg             ! Hold the argument


    contains
    
    subroutine print_tool_info()

        write(*,*) "┌------------------------------------------------┐"
        write(*,*) "   碧蓝航线活动毕业概率计算器  [version 2.0]   "
        write(*,*) "                                                    "
        write(*,*) "                                                    "
        write(*,*) "                                      2021.12.17    "
        write(*,*) "                                                    "
        write(*,*) "                  Nothing. All rights reserved.     "
        write(*,*) "└------------------------------------------------┘"

    end subroutine print_tool_info



    subroutine print_vars_setting()

        write(*,*) ""
        write(*,*) "┌------------------------------------------------┐"
        write(*,*) "                                                    "
        write(*,*) "                  确定卡池概率                      "
        write(*,*) "                                                    "
        write(*,*) "                                                    "
        write(*,*) "    请输入限定卡的个数：                            "
        write(*,*) "                                                    "
        write(*,*) "    按[回车键]确认输入      任意时刻可输入Q退出程序 "
        write(*,*) "└------------------------------------------------┘"
        write(*,*) ""

    end subroutine print_vars_setting



    subroutine print_times_setting()

        write(*,*) ""
        write(*,*) "┌------------------------------------------------┐"
        write(*,*) "                                                    "
        write(*,*) "                  确定抽卡次数                      "
        write(*,*) "                                                    "
        write(*,*) "                                                    "
        write(*,*) "    请输入抽卡的次数：                              "
        write(*,*) "                                                    "
        write(*,*) "    按[回车键]确认输入      任意时刻可输入Q退出程序 "
        write(*,*) "└------------------------------------------------┘"
        write(*,*) ""

    end subroutine print_times_setting
    


    subroutine print_probs_setting(i)
        implicit none
        integer, intent(in) :: i

        write(*,*)       ""  
        write(*,*)       "┌------------------------------------------------┐"
        write(*,'(1x,A,I1,A)')"   请输入第 ",i," 张卡的出现概率(单位:%)："
        write(*,*)       "└------------------------------------------------┘"
        write(*,*)       ""    

    end subroutine print_probs_setting



    subroutine convert_input(input_arg,output_arg,status_code)
        implicit none

        character(*),intent(in) :: input_arg
        class(*),intent(inout) :: output_arg
        integer,intent(inout) :: status_code
        character(100) :: io_msg
        
        status_code = 1
        select type (output_arg)
        type is(integer)
            read(input_arg,*,iostat=status_code,iomsg=io_msg) output_arg
            if (status_code > 0) then
                write(*,*) "   非法输入，请检查输入后再试,错误信息: "
                write(*,*) "   ",io_msg
                write(*,*) "   "
            end if
        type is(real)
            read(input_arg,*,iostat=status_code,iomsg=io_msg) output_arg
            if (status_code > 0) then
                write(*,*) "   非法输入，请检查输入后再试,错误信息: "
                write(*,*) "   ",io_msg
                write(*,*) "   "
            end if
        type is(logical)
            read(input_arg,*,iostat=status_code,iomsg=io_msg) output_arg
            if (status_code > 0) then
                write(*,*) "   非法输入，请检查输入后再试,错误信息: "
                write(*,*) "   ",io_msg
                write(*,*) "   "
            end if
        type is(character(*))
            read(input_arg,*,iostat=status_code,iomsg=io_msg) output_arg
            if (status_code > 0) then
                write(*,*) "   非法输入，请检查输入后再试,错误信息: "
                write(*,*) "   ",io_msg
                write(*,*) "   "
            end if
        class default
            error stop "未知输入类型"
        end select

        return
    end subroutine



    ! 主界面UI
    subroutine main_loop()

        implicit none
        
        integer :: i, status
        character(100) :: msg
        character(32) :: arg

        ! 选择计算模式
        do while (.TRUE.)
            write(*,*) ""
            write(*,*) "┌------------------------------------------------┐"
            write(*,*) "                                                    "
            write(*,*) "                  确定计算模式                      "
            write(*,*) "                                                    "
            write(*,*) "    请输入计算模式的序号：                          "
            write(*,*) "                                                    "
            write(*,*) "    1 给定抽卡次数，计算毕业概率                  "
            write(*,*) "    2 给定毕业概率，计算抽卡次数的期望          "
            write(*,*) "                                                    "
            write(*,*) "    按[回车键]确认输入      任意时刻可输入Q退出程序 "
            write(*,*) "└------------------------------------------------┘"
            write(*,*) ""
            read(*,*) arg

            if (arg == 'Q' .OR. arg == 'q') then
                stop "程序已退出"
            else
                ! 为了统一命令行下的变量输入，将vars用长度1的数组表示
                call convert_input(arg,mode(1),status)
                if (status <= 0) then 
                    if (mode(1) <= 2 .AND. mode(1)>=1) then
                        exit
                    else
                        write(*,*)"非法输入，请重新输入序号"
                    end if
                end if
            end if
        end do


        ! 读取变量的个数
        do while (.TRUE.)
            call print_vars_setting()
            read(*,*) arg
            if (arg == 'Q' .OR. arg == 'q') then
                stop "程序已退出"
            else
                ! 为了统一命令行下的变量输入，将vars用长度1的数组表示
                call convert_input(arg,vars(1),status)
                if (status <= 0) then 
                    if (vars(1) > 0) then
                        exit
                    else
                        write(*,*)"请输入合法的变量数(1-5)"
                    end if
                end if
            end if
        end do

        allocate(probabilities(vars(1)))    


        ! 读取限定卡的概率
        do i = 1,vars(1)
            do while (.TRUE.)
                call print_probs_setting(i) 
                read(*,*,iostat=status) arg
                if (arg == 'Q' .OR. arg == 'q') then
                    stop "程序已退出"
                else
                    call convert_input(arg,probabilities(i),status)
                    if (status <= 0) then 
                        if (probabilities(i) > 0) then
                            exit
                        else
                            write(*,*)"请输入合法的概率(单位: %)"
                        end if
                    end if
                    write(*,'(1x,A,I1,A,F5.2,A)')"   第",i,"张卡的出现概率为",probabilities(i),"%. "
                end if
            end do
        end do



        if (mode(1) == 1) then
            ! 读取设定的抽卡次数
            do while (.TRUE.)
                call print_times_setting()
                read(*,*) arg
                if (arg == 'Q' .OR. arg == 'q') then
                    stop "程序已退出"
                else
                    ! 为了统一命令行下的变量输入，将vars用长度1的数组表示
                    call convert_input(arg,times(1),status)
                    if (status <= 0) then 
                        if (times(1) > 0) then
                            exit
                        else
                            write(*,*)"请输入合法的抽卡次数"
                        end if
                    end if
                end if
            end do

            ! 二项分布计算概率
            call forward_calculation(vars)  
        else 
            ! 读取给定的概率
            do while (.TRUE.)
                call print_given_prob_setting()
                read(*,*) arg
                if (arg == 'Q' .OR. arg == 'q') then
                    stop "程序已退出"
                else
                    ! 为了统一命令行下的变量输入，将vars用长度1的数组表示
                    call convert_input(arg,given_prob(1),status)
                    if (status <= 0) then 
                        if (given_prob(1) > 0) then
                            exit
                        else
                            write(*,*)"请输入合法的概率(单位: %)"
                        end if
                    end if
                end if
            end do

            call inversed_calculation(vars,times)
        end if
        
        deallocate(probabilities) 
   
    end subroutine main_loop


    ! 给定抽卡次数计算抽卡概率
    subroutine forward_calculation(vars)
        
        implicit none

        integer,dimension(:),intent(in) :: vars
       
        call show_fw_config(vars(1),probabilities)

        if (vars(1) >= 5) call percentage2decimal(probabilities(5),pe)
        if (vars(1) >= 4) call percentage2decimal(probabilities(4),pd)
        if (vars(1) >= 3) call percentage2decimal(probabilities(3),pc)
        if (vars(1) >= 2) call percentage2decimal(probabilities(2),pb)
        if (vars(1) >= 1) call percentage2decimal(probabilities(1),pa)
        
        if (vars(1)==1) then 
            call calculate_probability(times(1),pa,given_prob(1))
        else if ((vars(1)==2)) then
            call calculate_probability(times(1),pa,pb,given_prob(1))
        else if ((vars(1)==3)) then
            call calculate_probability(times(1),pa,pb,pc,given_prob(1))
        else if ((vars(1)==4)) then
            call calculate_probability(times(1),pa,pb,pc,pd,given_prob(1))
        else if ((vars(1)==5)) then
            call calculate_probability(times(1),pa,pb,pc,pd,pe,given_prob(1))
        else
            error stop "Case that variables are more then 5 currently is not supported!"
        end if
        
        call show_probability(times(1),given_prob(1))

    end subroutine forward_calculation



    subroutine inversed_calculation(vars,times)

        implicit none

        integer,dimension(:),intent(in) :: vars
        integer,dimension(:),intent(inout) :: times
        integer :: max_times = 9999
        integer :: i, status
        logical :: found_solution = .FALSE.
        real :: current_prob
        character(100) :: arg


        call show_inv_config(given_prob(1),probabilities)
        call percentage2decimal(given_prob(1),given_prob(1))
            

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

        

            if (current_prob < 1 - given_prob(1)) then
                times(1) = i
                found_solution = .TRUE.
                exit
            else
                
            end if
        end do

        if (found_solution .neqv. .TRUE.) then 
            write(*,*)"抽卡所需次数已超过最大上限：",max_times," 次!"
        end if

        call show_times(given_prob(1),times(1))
    
    end subroutine inversed_calculation


    subroutine percentage2decimal(percent, decimal)
        implicit none
        real, intent(in) :: percent
        real, intent(inout) :: decimal
        decimal = percent/100.0
    end subroutine percentage2decimal
    


    subroutine show_probability(vars,p)
        implicit none
        integer,intent(in) :: vars
        real,intent(in) :: p
        real :: pv      ! inversed P

        pv = 100 - INT(p*1E6)/1E4

        write(*,*) ""
        write(*,*) "┌------------------------------------------------┐"
        write(*,*) "                                                    "
        write(*,*) "                   抽卡模拟结果                     "
        write(*,*) "                                                    "
        write(*,*) "                                                    "
        print"(5X,I6,'次建造未毕业的概率为:',F7.3,'%')",times,INT(p*1E6)/1E4
        print"(5X,I6,'次建造毕业的概率为:',F7.3,'%')",times,pv
        write(*,*) "                                                    "
        write(*,*) "└------------------------------------------------┘"

    end subroutine show_probability



    subroutine show_fw_config(times,probabilities)
        implicit none
        integer, intent(in) :: times
        real, dimension(:),intent(in) :: probabilities
        integer :: k
        character(5),allocatable,dimension(:) :: probs_chars

        allocate(probs_chars(vars(1)))
        do k = 1,times
            write(probs_chars(k),'(f5.2)') probabilities(k)
        end do

        write(*,*) ""
        write(*,*) "┌------------------------------------------------┐"
        write(*,*) "                                                    "
        write(*,*) "                    输入的配置                      "
        write(*,*) "                                                    "
        write(*,*) "                                                    "
        write(*,*) "    抽卡次数：",times,"							    " 
        write(*,'(1x,A)',advance='no') "    设定的概率："
        write(*,'(*(A))',advance='no') probs_chars//"%  "
        write(*,*) ""
        write(*,*) "└------------------------------------------------┘"
        write(*,*) ""

        deallocate(probs_chars)

    end subroutine show_fw_config



    subroutine show_inv_config(given_prob,probabilities)
        implicit none
        real, intent(in) :: given_prob
        real, dimension(:),intent(in) :: probabilities
        integer :: k
        character(5),allocatable,dimension(:) :: probs_chars

        allocate(probs_chars(vars(1)))
        do k = 1,vars(1)
            write(probs_chars(k),'(f5.2)') probabilities(k)
        end do

        write(*,*) ""
        write(*,*) "┌------------------------------------------------┐"
        write(*,*) "                                                    "
        write(*,*) "                    输入的配置                      "
        write(*,*) "                                                    "
        write(*,*) "                                                    "
        write(*,'(1x,A,f5.2,A)') "    抽卡概率期望：",given_prob,"%						    " 
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
        write(*,*) "                 确定抽卡期望概率                  "
        write(*,*) "                                                    "
        write(*,*) "                                                    "
        write(*,*) "    请输入抽齐毕业的概率(单位:%)：	                "
        write(*,*) "                                                    "
        write(*,*) "    按[回车键]确认输入      任意时刻可输入Q退出程序 "
        write(*,*) "└------------------------------------------------┘"
        write(*,*) ""

    end subroutine print_given_prob_setting 



    subroutine show_times(given_prob,times)
        implicit none
        integer,intent(in) :: times
        real,intent(in) :: given_prob
        real :: given_prob_percentage


        given_prob_percentage = INT(given_prob*1E6)/1E4

        write(*,*) ""
        write(*,*) "┌------------------------------------------------┐"
        write(*,*) "                                                    "
        write(*,*) "                   抽卡模拟结果                     "
        write(*,*) "                                                    "
        write(*,*) "                                                    "
        write(*,'(1x,A,f5.2,A,I4,A)') "    以 ",given_prob_percentage,"% 概率抽齐毕业的次数为：",times," 次"
        !print"(5X,I6,'次建造毕业的概率为:',F7.3,'%')",times,pv
        write(*,*) "                                                    "
        write(*,*) "└------------------------------------------------┘"

    end subroutine show_times


end module ui