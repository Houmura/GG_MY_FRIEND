module cl


    use option
    use ui

    implicit none
    
    contains

    subroutine forward_calculation_cl()
        implicit none
        integer :: sub_argc = 1
        logical :: is_completed = .FALSE.


        call get_option('-v',vars,sub_argc)
        call get_option('-n',times,sub_argc)
        allocate(probabilities(vars(1)))
        call get_option('-ps',probabilities,vars(1))
        
        is_completed = have_option('-v') .AND. have_option('-n') .AND. have_option('-ps')
        if (is_completed .eqv. .FALSE.) error stop "参数 -v -n -ps 必须给出"

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
    
        call show_probability(vars(1),given_prob(1))

        deallocate(probabilities)

    end subroutine forward_calculation_cl


    subroutine inversed_calculation_cl()
        implicit none
        integer :: sub_argc = 1
        integer :: i
        integer :: max_times = 9999
        real :: current_prob
        logical :: found_solution = .FALSE.
        logical :: is_completed = .FALSE.

        call get_option('-v',vars,sub_argc)
        call get_option('-p',given_prob,sub_argc)
        allocate(probabilities(vars(1)))
        call get_option('-ps',probabilities,vars(1))

        is_completed = have_option('-v') .AND. have_option('-p') .AND. have_option('-ps')
        if (is_completed .eqv. .FALSE.) error stop "参数 -v -p -ps 必须给出"

        call show_inv_config(given_prob(1),probabilities)

        if (vars(1) >= 5) call percentage2decimal(probabilities(5),pe)
        if (vars(1) >= 4) call percentage2decimal(probabilities(4),pd)
        if (vars(1) >= 3) call percentage2decimal(probabilities(3),pc)
        if (vars(1) >= 2) call percentage2decimal(probabilities(2),pb)
        if (vars(1) >= 1) call percentage2decimal(probabilities(1),pa)

        call percentage2decimal(given_prob(1),given_prob(1))

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
    

        
    end subroutine inversed_calculation_cl

end module cl