!   
!   Probability Core Solver
!

module core 

    
    implicit none 
    interface calculate_probability
        module procedure calculate_probability_1var
        module procedure calculate_probability_2var
        module procedure calculate_probability_3var
        module procedure calculate_probability_4var
        module procedure calculate_probability_5var
    end interface calculate_probability

    ! 以下为事件概率
    real :: a, b, c, d, e 
    real :: ab, ac, ad, ae, bc, bd, be, cd, ce, de
    real :: abc, abd, abe, acd, ace, ade, bcd, bce, bde, cde
    real :: abcd, acde, abde, abce, bcde
    real :: abcde  
    
    
    contains
    
    subroutine calculate_probability_1var(n, pa, p)
        
        implicit none
        ! Arguments
        integer,intent(in) :: n     ! 抽卡次数
        real, intent(in) :: pa      ! 给定概率的数组(小数形式)
        real, intent(inout) :: p    ! 未抽齐的概率(小数形式)
        
        ! Perform calculation
        a =(1-pa)**n
        p = a
        
    end subroutine calculate_probability_1var
    
    
    subroutine calculate_probability_2var(n, pa, pb, p)
        
        implicit none
        ! Arguments
        integer,intent(in) :: n         ! 抽卡次数
        real, intent(in) :: pa, pb      ! 给定概率的数组(小数形式)
        real, intent(inout) :: p        ! 未抽齐的概率(小数形式)

        ! Perform calculation
        a =(1-pa)**n
        b =(1-pb)**n
        ab = (1-pa-pb)**n               ! 容斥原理
        p = a+b-ab                   
    
    end subroutine calculate_probability_2var


    subroutine calculate_probability_3var(n, pa, pb, pc, p)
        
        implicit none
        ! Arguments
        integer,intent(in) :: n         
        real, intent(in) :: pa, pb, pc      ! 给定概率的数组(小数形式)    
        real, intent(inout) :: p        
    
        ! Perform calculation
        a =(1-pa)**n
        b =(1-pb)**n
        c =(1-pc)**n
        ab = (1-pa-pb)**n 
        ac =(1-pa-pc)**n
        bc =(1-pb-pc)**n
        abc =(1-pa-pb-pc)**n
        p =a+b+c-(ab+ac+bc)+abc
                    
    end subroutine calculate_probability_3var
    

    subroutine calculate_probability_4var(n, pa, pb, pc, pd, p)
        
        implicit none
        ! Arguments
        integer,intent(in) :: n         
        real, intent(in) :: pa, pb, pc, pd      ! 给定概率的数组(小数形式)     
        real, intent(inout) :: p        
        
        ! Perform calculation
        a =(1-pa)**n
        b =(1-pb)**n
        c =(1-pc)**n
        d =(1-pd)**n
        ab = (1-pa-pb)**n 
        ac =(1-pa-pc)**n
        bc =(1-pb-pc)**n
        ad =(1-pa-pd)**n
        bd =(1-pb-pd)**n
        cd =(1-pc-pd)**n
        abc =(1-pa-pb-pc)**n
        abd =(1-pa-pb-pd)**n
        acd =(1-pa-pc-pd)**n
        bcd =(1-pb-pc-pd)**n
        abcd =(1-pa-pb-pc-pd)**n
        p =a+b+c+d-(ab+ac+ad+bc+bd+cd)+(abc+abd+acd+bcd)-(abcd)
         
    end subroutine calculate_probability_4var


    subroutine calculate_probability_5var(n, pa, pb, pc, pd, pe, p)
        
        implicit none
        ! Arguments
        integer,intent(in) :: n         
        real, intent(in) :: pa, pb, pc, pd, pe      ! 给定概率的数组(小数形式)    
        real, intent(inout) :: p        
        
        ! Perform calculation
        a =(1-pa)**n
        b =(1-pb)**n
        c =(1-pc)**n
        d =(1-pd)**n
        e =(1-pe)**n
        ab =(1-pa-pb)**n
        ac =(1-pa-pc)**n
        ad =(1-pa-pd)**n
        bc =(1-pb-pc)**n
        bd =(1-pb-pd)**n
        cd =(1-pc-pd)**n
        ae =(1-pa-pe)**n
        be =(1-pb-pe)**n
        ce =(1-pc-pe)**n
        de =(1-pd-pe)**n
        abe =(1-pa-pb-pe)**n
        ace =(1-pa-pc-pe)**n
        ade =(1-pa-pd-pe)**n
        bce =(1-pb-pc-pe)**n
        bde =(1-pb-pd-pe)**n
        cde =(1-pc-pd-pe)**n
        bcde =(1-pb-pc-pd-pe)**n
        acde =(1-pa-pc-pd-pe)**n
        abde =(1-pa-pb-pd-pe)**n
        abce =(1-pa-pb-pc-pe)**n
        abc =(1-pa-pb-pc)**n
        abd =(1-pa-pb-pd)**n
        acd =(1-pa-pc-pd)**n
        bcd =(1-pb-pc-pd)**n
        abcd =(1-pa-pb-pc-pd)**n
        abcde =(1-pa-pb-pc-pd-pe)**n
        p =a+b+c+d+e-(ab+ac+ad+bc+bd+cd+ae+be+ce+de)+&
        &(abc+abd+acd+bcd+abe+ace+ade+bce+bde+cde)-&
        &(abcd+bcde+acde+abde+abce)+abcde
    
    end subroutine calculate_probability_5var

end module core