module cl


    use option
    use ui

    implicit none
    
    contains

    subroutine forward_calculation_cl()
    !   Forward calculation ��������������
    !   �������÷�:
    !   -v [������] -n [�鿨����] -ps [[��Ƭ1����] [��Ƭ2����]...]
    !
        implicit none
        integer :: sub_argc = 1             ! get_option Ҫ����Ĳ�������
        integer :: i = 1;                   
        logical :: is_completed = .FALSE.   ! �����Ƿ����ȫ��flag


        call get_option('-v',vars,sub_argc)
        call get_option('-n',times,sub_argc)
        allocate(probabilities(vars(1)))
        call get_option('-ps',probabilities,vars(1))
        
        is_completed = have_option('-v') .AND. have_option('-n') .AND. have_option('-ps')
        if (is_completed .eqv. .FALSE.) error stop "���� -v -n -ps �������"

        call show_fw_config(vars(1),probabilities)

        do i = 1, vars(1)
            call percentage2decimal(probabilities(i),probabilities(i))
        end do

        call calculate_probability(vars(1),times(1),probabilities,given_prob(1))
    
        call show_probability(vars(1),given_prob(1))

        deallocate(probabilities)

    end subroutine forward_calculation_cl


    subroutine inversed_calculation_cl()
    !   Inversed calculation ��������������
    !   �������÷�:
    !   -v [������] -p [��������] -ps [[��Ƭ1����] [��Ƭ2����]...]
    !
        implicit none
        integer :: sub_argc = 1                 ! get_option Ҫ����Ĳ�������
        integer :: i
        integer :: max_times = 9999             ! ���鿨����
        real :: current_prob                    ! ��i�γ�ȡʱ�ĸ���
        logical :: found_solution = .FALSE.     ! �Ƿ�ﵽ�������ʵ�flag
        logical :: is_completed = .FALSE.       ! �����Ƿ����ȫ��flag

        call get_option('-v',vars,sub_argc)
        call get_option('-p',given_prob,sub_argc)
        allocate(probabilities(vars(1)))
        call get_option('-ps',probabilities,vars(1))

        is_completed = have_option('-v') .AND. have_option('-p') .AND. have_option('-ps')
        if (is_completed .eqv. .FALSE.) error stop "���� -v -p -ps �������"

        call show_inv_config(given_prob(1),probabilities)

        do i = 1, vars(1)
            call percentage2decimal(probabilities(i),probabilities(i))
        end do

        call percentage2decimal(given_prob(1),given_prob(1))

        do i = 0, max_times
            call  calculate_probability(vars(1),  i,probabilities,current_prob)

            if (current_prob < 1 - given_prob(1)) then
                times(1) = i
                found_solution = .TRUE.
                exit
            else
                
            end if
        end do

        if (found_solution .neqv. .TRUE.) then 
            write(*,*)"�鿨��������ѳ���������ޣ�",max_times," ��!"
        end if

        call show_times(given_prob(1),times(1))
    

        
    end subroutine inversed_calculation_cl

end module cl