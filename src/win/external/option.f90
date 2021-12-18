!> author: FANGCHAO
!  data: 2017/7/12
!
!## description
! this module provide some command line operation.
! contains: `get_option`,`have_option`
! the option usually is string, like `--help`,
! the arguments can be integer, real, logical, string
!
! adapted from https://github.com/fccf/option by Nothing


module option

  implicit none

  public :: get_option
  public :: have_option

  private


contains


  function have_option(option) result(found)
    !< confirm if appear option
    character(*),intent(in) :: option
    logical   :: found

    integer :: argc
    character(32),allocatable :: argv(:)
    integer :: i
    found = .false.

    argc = command_argument_count()
    if(argc /=0) then
      allocate(argv(argc))
    end if

    do i = 1,argc
      call get_command_argument(i,argv(i))
    end do

    do i = 1,argc
      if(option == trim(adjustl(argv(i)))) then
        found = .true.
        exit
      end if
    enddo

  end function have_option


  subroutine get_option(option,arg,sub_argc,found)
    !< get option for integer, real, logical, character(*)
    character(*),intent(in)                     :: option
    class(*),dimension(:),intent(out)           :: arg
    integer,intent(inout)					    :: sub_argc
    logical,intent(out),optional                :: found


    integer :: argc
    character(100) :: msg
    character(32),allocatable :: argv(:)
    integer :: i, j, iostatus, skip_count
    logical :: found_ = .false.

    ! arg  = 0
    argc = command_argument_count()
    if(argc /=0) then
      allocate(argv(argc))
    end if

    !if(.not.present(sub_argc)) then
    !    sub_argc = 1
    !end if
    
    skip_count = 0

    do i = 1,argc
      call get_command_argument(i,argv(i))
    end do

    do i = 1,argc
      if (skip_count > 0) then
        skip_count = skip_count-1
        cycle
      else
        if(option == trim(adjustl(argv(i)))) then
          select type (arg)
          type is(integer)
            do j = 1,sub_argc
              read(argv(i+j),*,iomsg=msg,iostat=iostatus) arg(j)
              skip_count = skip_count+j
            end do
          type is(real)
            do j = 1,sub_argc
              read(argv(i+j),*,iomsg=msg,iostat=iostatus) arg(j)
              skip_count = skip_count+j
            end do
          type is(logical)
            do j = 1,sub_argc
              read(argv(i+j),*,iomsg=msg,iostat=iostatus) arg(j)
              skip_count = skip_count+j
            end do
          type is(character(*))
            do j = 1,sub_argc
              read(argv(i+j),*,iomsg=msg,iostat=iostatus) arg(j)
              skip_count = skip_count+j
            end do
          class default
              error stop "unknow option type"
          end select

          found_ = .true.
          exit
        end if
      end if
    enddo

    if(present(found)) found = found_

  end subroutine get_option


end module option
