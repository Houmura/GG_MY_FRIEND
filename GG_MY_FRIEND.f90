PROGRAM GG_MY_FRIEND
 IMPLICIT NONE
 INTEGER N
 REAL NUM
 REAL P_A, P_B, P_C, P_D, P_E     !�ٷֱ���ʽ
 REAL PA, PB, PC, PD, PE, P, PV   !С����ʽ
 !����Ϊ�¼�����
 REAL A, B, C, D, E
 REAL AB, AC, AD, AE, BC, BD, BE, CD, CE, DE
 REAL ABC, ABD, ABE, ACD, ACE, ADE, BCD, BCE, BDE, CDE
 REAL ABCD, ACDE, ABDE, ABCE, BCDE
 REAL ABCDE 
 
 PRINT *,'�������߻��ҵ���ʼ�����  [Version 1.0 beta]'
 PRINT *,''
 PRINT *,'Nothing. All rights reserved.'
 
10086 PRINT *,'';PRINT *,''
 PRINT *,'�����뽨�������   (���س���ȷ������)'
  READ *,N
   IF (N<0) THEN
    PRINT *,'������������������������룡'
    GOTO 10086
   END IF
 PRINT *,'';PRINT *,''  
 PRINT *,'�������޶����ĸ�����   (���س���ȷ������)'
  READ *,NUM
  
 
 SELECT CASE(INT(NUM))
 
  CASE(1)
   PRINT *,'�������һ�Ҵ��Ľ�����ʣ���λ:%��'
   READ *,P_A
   PRINT *,'';PRINT *,''  
    PA = P_A/100
    A =(1-PA)**N
    P = A*100
    PV = 100-P
    P = INT(P*1E6)/1E6                     
    PV = INT(PV*1E6)/1E6
   PRINT"(1X,I6,'�ν���δ��ҵ�ĸ���Ϊ:',F7.3,'%')",N,P
   PRINT"(1X,I6,'�ν����ҵ�ĸ���Ϊ:',F7.3,'%')",N,PV
   
   GO TO 10086
    
  CASE(2)
   PRINT *,'�������һ�Ҵ��Ľ�����ʣ���λ:%��'
   READ *,P_A
   PRINT *,'';PRINT *,''  
    PA = P_A/100
    A =(1-PA)**N
   PRINT *,'������ڶ��Ҵ��Ľ�����ʣ���λ:%��'
   READ *,P_B
   PRINT *,'';PRINT *,''  
    PB = P_B/100
    B =(1-PB)**N
    AB =(1-PA-PB)**N
    P = (A+B-AB)*100
    PV = 100-P
    P = INT(P*1E6)/1E6
    PV = INT(PV*1E6)/1E6
   PRINT"(1X,I6,'�ν���δ��ҵ�ĸ���Ϊ:',F7.3,'%')",N,P
   PRINT"(1X,I6,'�ν����ҵ�ĸ���Ϊ:',F7.3,'%')",N,PV
   
   GO TO 10086
    
  CASE(3)
   PRINT *,'�������һ�Ҵ��Ľ�����ʣ���λ:%��'
   READ *,P_A
   PRINT *,'';PRINT *,''  
    PA = P_A/100
    A =(1-PA)**N
   PRINT *,'������ڶ��Ҵ��Ľ�����ʣ���λ:%��'
   READ *,P_B
   PRINT *,'';PRINT *,''  
    PB = P_B/100
    B =(1-PB)**N
   PRINT *,'����������Ҵ��Ľ�����ʣ���λ:%��'
   READ *,P_C
   PRINT *,'';PRINT *,''  
    PC = P_C/100
    C =(1-PC)**N
    AB =(1-PA-PB)**N
    AC =(1-PA-PC)**N
    BC =(1-PB-PC)**N
    ABC =(1-PA-PB-PC)**N
    P =(A+B+C-(AB+AC+BC)+ABC)*100
    PV = 100-P
    P = INT(P*1E6)/1E6
    PV = INT(PV*1E6)/1E6
    PRINT"(1X,I6,'�ν���δ��ҵ�ĸ���Ϊ:',F7.3,'%')",N,P
    PRINT"(1X,I6,'�ν����ҵ�ĸ���Ϊ:',F7.3,'%')",N,PV
    
  CASE(4)
   PRINT *,'�������һ�Ҵ��Ľ�����ʣ���λ:%��'
   READ *,P_A
    PA = P_A/100
    A =(1-PA)**N
   PRINT *,'������ڶ��Ҵ��Ľ�����ʣ���λ:%��'
   READ *,P_B
    PB = P_B/100
    B =(1-PB)**N
   PRINT *,'����������Ҵ��Ľ�����ʣ���λ:%��'
   READ *,P_C
    PC = P_C/100
    C =(1-PC)**N
   PRINT *,'����������Ҵ��Ľ�����ʣ���λ:%��'
   READ *,P_D
   PRINT *,'';PRINT *,''  
    PD = P_D/100
    D =(1-PD)**N
    AB =(1-PA-PB)**N
    AC =(1-PA-PC)**N
    BC =(1-PB-PC)**N
    AD =(1-PA-PD)**N
    BD =(1-PB-PD)**N
    CD =(1-PC-PD)**N
    ABC =(1-PA-PB-PC)**N
    ABD =(1-PA-PB-PD)**N
    ACD =(1-PA-PC-PD)**N
    BCD =(1-PB-PC-PD)**N
    ABCD =(1-PA-PB-PC-PD)**N
    P =(A+B+C+D-(AB+AC+AD+BC+BD+CD)+(ABC+ABD+ACD+BCD)-(ABCD))*100
    PV = 100-P
    P = INT(P*1E6)/1E6
    PV = INT(PV*1E6)/1E6
    PRINT"(1X,I6,'�ν���δ��ҵ�ĸ���Ϊ:',F7.3,'%')",N,P
    PRINT"(1X,I6,'�ν����ҵ�ĸ���Ϊ:',F7.3,'%')",N,PV
    
    GO TO 10086
    
  CASE(5)
   PRINT *,'�������һ�Ҵ��Ľ�����ʣ���λ:%��'
   READ *,P_A
   PRINT *,'';PRINT *,''  
    PA = P_A/100
    A =(1-PA)**N
   PRINT *,'������ڶ��Ҵ��Ľ�����ʣ���λ:%��'
   READ *,P_B
   PRINT *,'';PRINT *,''  
    PB = P_B/100
    B =(1-PB)**N
   PRINT *,'����������Ҵ��Ľ�����ʣ���λ:%��'
   READ *,P_C
   PRINT *,'';PRINT *,''  
    PC = P_C/100
    C =(1-PC)**N
   PRINT *,'����������Ҵ��Ľ�����ʣ���λ:%��'
   READ *,P_D
   PRINT *,'';PRINT *,''  
    PD = P_D/100
    D =(1-PD)**N
   PRINT *,'����������Ҵ��Ľ�����ʣ���λ:%��'
   READ *,P_E
   PRINT *,'';PRINT *,''  
    PE = P_E/100
    E =(1-PE)**N
    AB =(1-PA-PB)**N
    AC =(1-PA-PC)**N
    AD =(1-PA-PD)**N
    BC =(1-PB-PC)**N
    BD =(1-PB-PD)**N
    CD =(1-PC-PD)**N
    AE =(1-PA-PE)**N
    BE =(1-PB-PE)**N
    CE =(1-PC-PE)**N
    DE =(1-PD-PE)**N
    ABE =(1-PA-PB-PE)**N
    ACE =(1-PA-PC-PE)**N
    ADE =(1-PA-PD-PE)**N
    BCE =(1-PB-PC-PE)**N
    BDE =(1-PB-PD-PE)**N
    CDE =(1-PC-PD-PE)**N
    BCDE =(1-PB-PC-PD-PE)**N
    ACDE =(1-PA-PC-PD-PE)**N
    ABDE =(1-PA-PB-PD-PE)**N
    ABCE =(1-PA-PB-PC-PE)**N
    ABC =(1-PA-PB-PC)**N
    ABD =(1-PA-PB-PD)**N
    ACD =(1-PA-PC-PD)**N
    BCD =(1-PB-PC-PD)**N
    ABCD =(1-PA-PB-PC-PD)**N
    ABCDE =(1-PA-PB-PC-PD-PE)**N
    P =(A+B+C+D+E-(AB+AC+AD+BC+BD+CD+AE+BE+CE+DE)+&
    &(ABC+ABD+ACD+BCD+ABE+ACE+ADE+BCE+BDE+CDE)-&
    &(ABCD+BCDE+ACDE+ABDE+ABCE)+ABCDE)*100
    PV = 100-P
    P = INT(P*1E6)/1E6
    PV = INT(PV*1E6)/1E6
    PRINT"(1X,I6,'�ν���δ��ҵ�ĸ���Ϊ:',F7.3,'%')",N,P
    PRINT"(1X,I6,'�ν����ҵ�ĸ���Ϊ:',F7.3,'%')",N,PV
    
    GO TO 10086
  CASE(6:)
   PRINT *,'��Ǹ�����������ݲ�֧��6�Ҵ������ϵĸ��ʼ���'
   GO TO 10086
   
  CASE DEFAULT 
   PRINT *,'������������������������룡'
   GO TO 10086
  END SELECT 
  

 
 END
 
