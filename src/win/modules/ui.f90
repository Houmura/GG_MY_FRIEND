!
!   User Interface (Command-Line)
!

module ui

    use core, only: calculate_probability
    use option
    !use inverse

    implicit none
    
    ! ��������������
    integer,dimension(1) :: times                       !�鿨����
    integer,dimension(1) :: vars                        !������
    integer,dimension(1) :: mode                        !����ģʽ
    real,dimension(1) :: given_prob                     !��������
    real,allocatable,dimension(:) :: probabilities      !����Ƭ�ĸ�������(С����ʽ)
    

    contains
    
    subroutine print_tool_info()

        write(*,*) "��------------------------------------------------��"
        write(*,*) "   �������߻��ҵ���ʼ�����  [version 2.2]        "
        write(*,*) "                                                    "
        write(*,*) "                                                    "
        write(*,*) "                                     2021.6.15      "
        write(*,*) "                                                    "
        write(*,*) "                  Nothing. All rights reserved.     "
        write(*,*) "��------------------------------------------------��"

    end subroutine print_tool_info



    subroutine print_vars_setting()

        write(*,*) ""
        write(*,*) "��------------------------------------------------��"
        write(*,*) "                                                    "
        write(*,*) "                  ȷ�����ظ���                      "
        write(*,*) "                                                    "
        write(*,*) "                                                    "
        write(*,*) "    �������޶����ĸ�����                            "
        write(*,*) "                                                    "
        write(*,*) "    ��[�س���]ȷ������      ����ʱ�̿�����Q�˳����� "
        write(*,*) "��------------------------------------------------��"
        write(*,*) ""

    end subroutine print_vars_setting



    subroutine print_times_setting()

        write(*,*) ""
        write(*,*) "��------------------------------------------------��"
        write(*,*) "                                                    "
        write(*,*) "                  ȷ���鿨����                      "
        write(*,*) "                                                    "
        write(*,*) "                                                    "
        write(*,*) "    ������鿨�Ĵ�����                              "
        write(*,*) "                                                    "
        write(*,*) "    ��[�س���]ȷ������      ����ʱ�̿�����Q�˳����� "
        write(*,*) "��------------------------------------------------��"
        write(*,*) ""

    end subroutine print_times_setting
    


    subroutine print_probs_setting(i)
        implicit none
        integer, intent(in) :: i

        write(*,*)       ""  
        write(*,*)       "��------------------------------------------------��"
        write(*,'(1x,A,I2,A)')"   ������� ",i," �ſ��ĳ��ָ���(��λ:%)��"
        write(*,*)       "��------------------------------------------------��"
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
                write(*,*) "   �Ƿ����룬�������������,������Ϣ: "
                write(*,*) "   ",io_msg
                write(*,*) "   "
            end if
        type is(real)
            read(input_arg,*,iostat=status_code,iomsg=io_msg) output_arg
            if (status_code > 0) then
                write(*,*) "   �Ƿ����룬�������������,������Ϣ: "
                write(*,*) "   ",io_msg
                write(*,*) "   "
            end if
        type is(logical)
            read(input_arg,*,iostat=status_code,iomsg=io_msg) output_arg
            if (status_code > 0) then
                write(*,*) "   �Ƿ����룬�������������,������Ϣ: "
                write(*,*) "   ",io_msg
                write(*,*) "   "
            end if
        type is(character(*))
            read(input_arg,*,iostat=status_code,iomsg=io_msg) output_arg
            if (status_code > 0) then
                write(*,*) "   �Ƿ����룬�������������,������Ϣ: "
                write(*,*) "   ",io_msg
                write(*,*) "   "
            end if
        class default
            error stop "δ֪��������"
        end select

        return
    end subroutine



    ! ������UI
    subroutine main_loop()

        implicit none
        
        integer :: i, status
        character(100) :: msg
        character(32) :: arg

        ! ѡ�����ģʽ
        do while (.TRUE.)
            write(*,*) ""
            write(*,*) "��------------------------------------------------��"
            write(*,*) "                                                    "
            write(*,*) "                  ȷ������ģʽ                      "
            write(*,*) "                                                    "
            write(*,*) "    ���������ģʽ����ţ�                          "
            write(*,*) "                                                    "
            write(*,*) "    1 �����鿨�����������ҵ����                  "
            write(*,*) "    2 ������ҵ���ʣ�����鿨����������          "
            write(*,*) "                                                    "
            write(*,*) "    ��[�س���]ȷ������      ����ʱ�̿�����Q�˳����� "
            write(*,*) "��------------------------------------------------��"
            write(*,*) ""
            read(*,*) arg

            if (arg == 'Q' .OR. arg == 'q') then
                stop "�������˳�"
            else
                ! Ϊ��ͳһ�������µı������룬��vars�ó���1�������ʾ
                call convert_input(arg,mode(1),status)
                if (status <= 0) then 
                    if (mode(1) <= 2 .AND. mode(1)>=1) then
                        exit
                    else
                        write(*,*)"�Ƿ����룬�������������"
                    end if
                end if
            end if
        end do


        ! ��ȡ�����ĸ���
        do while (.TRUE.)
            call print_vars_setting()
            read(*,*) arg
            if (arg == 'Q' .OR. arg == 'q') then
                stop "�������˳�"
            else
                ! Ϊ��ͳһ�������µı������룬��vars�ó���1�������ʾ
                call convert_input(arg,vars(1),status)
                if (status <= 0) then 
                    if (vars(1) > 0) then
                        exit
                    else
                        write(*,*)"������Ϸ��ı�����"
                    end if
                end if
            end if
        end do

        allocate(probabilities(vars(1)))    


        ! ��ȡ�޶����ĸ���
        do i = 1,vars(1)
            do while (.TRUE.)
                call print_probs_setting(i) 
                read(*,*,iostat=status) arg
                if (arg == 'Q' .OR. arg == 'q') then
                    stop "�������˳�"
                else
                    call convert_input(arg,probabilities(i),status)
                    if (status <= 0) then 
                        if (probabilities(i) > 0) then
                            exit
                        else
                            write(*,*)"������Ϸ��ĸ���(��λ: %)"
                        end if
                    end if
                    write(*,'(1x,A,I2,A,F5.2,A)')"   ��",i,"�ſ��ĳ��ָ���Ϊ",probabilities(i),"%. "
                end if
            end do
        end do



        if (mode(1) == 1) then
            ! ��ȡ�趨�ĳ鿨����
            do while (.TRUE.)
                call print_times_setting()
                read(*,*) arg
                if (arg == 'Q' .OR. arg == 'q') then
                    stop "�������˳�"
                else
                    ! Ϊ��ͳһ�������µı������룬��vars�ó���1�������ʾ
                    call convert_input(arg,times(1),status)
                    if (status <= 0) then 
                        if (times(1) > 0) then
                            exit
                        else
                            write(*,*)"������Ϸ��ĳ鿨����"
                        end if
                    end if
                end if
            end do

            ! ����ֲ��������
            call forward_calculation(vars)  
        else 
            ! ��ȡ�����ĸ���
            do while (.TRUE.)
                call print_given_prob_setting()
                read(*,*) arg
                if (arg == 'Q' .OR. arg == 'q') then
                    stop "�������˳�"
                else
                    ! Ϊ��ͳһ�������µı������룬��vars�ó���1�������ʾ
                    call convert_input(arg,given_prob(1),status)
                    if (status <= 0) then 
                        if (given_prob(1) > 0) then
                            exit
                        else
                            write(*,*)"������Ϸ��ĸ���(��λ: %)"
                        end if
                    end if
                end if
            end do

            call inversed_calculation(vars,times)
        end if
        
        deallocate(probabilities) 
   
    end subroutine main_loop


    ! �����鿨��������鿨����
    subroutine forward_calculation(vars)
        
        implicit none

        integer,dimension(:),intent(in) :: vars
        integer                         :: i
       
        call show_fw_config(vars(1),probabilities)

        ! ת�����ʵ�С����ʽ
        do i = 1, vars(1)
            call percentage2decimal(probabilities(i),probabilities(i))
        end do
        
        call calculate_probability(vars(1), times(1), probabilities, given_prob(1))

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
            

        ! ת�����ʵ�С����ʽ
        do i = 1, vars(1)
            call percentage2decimal(probabilities(i),probabilities(i))
        end do

        do i = 0, max_times
            call calculate_probability(vars(1), i, probabilities, current_prob)

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
        write(*,*) "��------------------------------------------------��"
        write(*,*) "                                                    "
        write(*,*) "                   �鿨ģ����                     "
        write(*,*) "                                                    "
        write(*,*) "                                                    "
        print"(5X,I6,'�ν���δ��ҵ�ĸ���Ϊ:',F7.3,'%')",times,INT(p*1E6)/1E4
        print"(5X,I6,'�ν����ҵ�ĸ���Ϊ:',F7.3,'%')",times,pv
        write(*,*) "                                                    "
        write(*,*) "��------------------------------------------------��"

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
        write(*,*) "��------------------------------------------------��"
        write(*,*) "                                                    "
        write(*,*) "                    ���������                      "
        write(*,*) "                                                    "
        write(*,*) "                                                    "
        write(*,*) "    �鿨������",times,"							    " 
        write(*,'(1x,A)',advance='no') "    �趨�ĸ��ʣ�"
        write(*,'(*(A))',advance='no') probs_chars//"%  "
        write(*,*) ""
        write(*,*) "��------------------------------------------------��"
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
        write(*,*) "��------------------------------------------------��"
        write(*,*) "                                                    "
        write(*,*) "                    ���������                      "
        write(*,*) "                                                    "
        write(*,*) "                                                    "
        write(*,'(1x,A,f5.2,A)') "    �鿨����������",given_prob,"%						    " 
        write(*,'(1x,A)',advance='no') "    �趨�ĸ��ʣ�"
        write(*,'(*(A))',advance='no') probs_chars//"%  "
        write(*,*) ""
        write(*,*) "��------------------------------------------------��"
        write(*,*) ""

        deallocate(probs_chars)

    end subroutine show_inv_config



    subroutine print_given_prob_setting()

        write(*,*) ""
        write(*,*) "��------------------------------------------------��"
        write(*,*) "                                                    "
        write(*,*) "                 ȷ���鿨��������                  "
        write(*,*) "                                                    "
        write(*,*) "                                                    "
        write(*,*) "    ����������ҵ�ĸ���(��λ:%)��	                "
        write(*,*) "                                                    "
        write(*,*) "    ��[�س���]ȷ������      ����ʱ�̿�����Q�˳����� "
        write(*,*) "��------------------------------------------------��"
        write(*,*) ""

    end subroutine print_given_prob_setting 



    subroutine show_times(given_prob,times)
        implicit none
        integer,intent(in) :: times
        real,intent(in) :: given_prob
        real :: given_prob_percentage


        given_prob_percentage = INT(given_prob*1E6)/1E4

        write(*,*) ""
        write(*,*) "��------------------------------------------------��"
        write(*,*) "                                                    "
        write(*,*) "                   �鿨ģ����                     "
        write(*,*) "                                                    "
        write(*,*) "                                                    "
        write(*,'(1x,A,f5.2,A,I4,A)') "    �� ",given_prob_percentage,"% ���ʳ����ҵ�Ĵ���Ϊ��",times," ��"
        !print"(5X,I6,'�ν����ҵ�ĸ���Ϊ:',F7.3,'%')",times,pv
        write(*,*) "                                                    "
        write(*,*) "��------------------------------------------------��"

    end subroutine show_times


end module ui