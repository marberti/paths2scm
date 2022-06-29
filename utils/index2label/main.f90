program main

  implicit none

  integer, parameter :: fnumb_in = 100
  integer, parameter :: fnumb_pa = 100
  character(*), parameter :: fname_in = "paths2scm.in"
  character(*), parameter :: fname_pa = "graph_paths.out"
  character(16), dimension(:), allocatable :: labels
  character(400) :: buff
  character(18) :: pn_str
  character(18) :: f
  integer :: ln
  integer :: pn
  integer :: i
  integer :: indx
  integer :: err_n
  character(120) :: err_msg

  if (command_argument_count() /= 1) stop "an integer is required as argument"

  call get_command_argument(1,pn_str)
  read(pn_str,*,iostat=err_n) pn
  if ((err_n /= 0).or.(pn <= 0)) then
    stop "a positive integer is required as argument"
  end if

  ! read labels
  open(unit=fnumb_in,file=fname_in,status='old',action='read', &
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) stop trim(err_msg)

  read(fnumb_in,*)
  read(fnumb_in,*)
  read(fnumb_in,*)
  read(fnumb_in,*,iostat=err_n) ln
  if ((err_n /= 0).or.(ln <= 0)) stop "error on labels number"
  allocate(labels(ln),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) stop trim(err_msg)
  do i = 1, ln
    read(fnumb_in,*,iostat=err_n) labels(i)
    if (err_n /= 0) stop trim(err_msg)
  end do

  close(unit=fnumb_in,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) stop trim(err_msg)

  ! read path
  open(unit=fnumb_pa,file=fname_pa,status='old',action='read', &
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) stop trim(err_msg)

  do i = 1, pn-1
    read(fnumb_pa,*)
  end do
  read(fnumb_pa,'(A400)',iostat=err_n) buff
  if (err_n /= 0) stop "cannot read line "//trim(pn_str)

  i = 1
  do
    call get_field(buff,f,i,err_n,err_msg)
    if (err_n /= 0) exit
    read(f,*,iostat=err_n) indx
    if (err_n /= 0) stop "wrong format reading path: "//trim(buff)
    write(*,'(1X,A)',advance='no') trim(adjustl(labels(indx)))
    i = i + 1
  end do
  write(*,*)

  close(unit=fnumb_pa,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) stop trim(err_msg)

contains

subroutine get_field(str_in,str_out,n,err_n,err_msg)

  ! Takes an input string str_in in which
  ! every field is separated one another by one or more spaces.
  ! Gets the n field and store it in str_out.

  character(*), intent(IN)  :: str_in
  character(*), intent(OUT) :: str_out
  integer,      intent(IN)  :: n
  integer,      intent(OUT) :: err_n         ! 0 on success, 1 otherwise
  character(*), intent(OUT) :: err_msg       ! message set in case of failure
  integer, parameter        :: SUCCESS       = 0
  integer, parameter        :: FAILURE       = 1
  integer                   :: i
  integer                   :: current_field
  integer                   :: start_field
  integer                   :: end_field
  character(1)              :: ch
  character(3)              :: n_str
  character(8)              :: istr1
  character(8)              :: istr2
  logical                   :: prev_space

  if (n<1) then
    err_n   = FAILURE
    err_msg = "get_field: argument must be a non-zero positive integer"
    return
  end if

  start_field   = -1
  end_field     = -1
  current_field =  0
  prev_space    = .true.
  do i=1, len_trim(str_in)
    ch = str_in(i:i)

    if (ch==" ") then
      if ((prev_space.eqv..false.).and.(current_field==n)) then
        end_field = i-1
        exit
      end if
      prev_space = .true.
    else
      if (prev_space.eqv..true.) then
        current_field = current_field+1
        if (current_field==n) then
          start_field = i
        end if
      end if

      prev_space = .false.
    end if
  end do

  if (start_field/=-1) then
    if (end_field==-1) then
      end_field = len_trim(str_in)
    end if
  else
    write(n_str,'(I3)') n
    n_str   = adjustl(n_str)
    err_n   = FAILURE
    err_msg = "get_field: cannot get field "//trim(n_str)//&
      &" from string """//trim(str_in)//""""
    return
  end if

  if ((end_field-start_field+1)>len(str_out)) then
    err_n   = FAILURE
    write(istr1,'(I8)') len(str_out)
    istr1 = adjustl(istr1)
    write(istr2,'(I8)') end_field-start_field+1
    istr2 = adjustl(istr2)
    err_msg = "get_field: output string too small ("//trim(istr1)//&
      &") to contain the field ("//trim(istr2)//")"
    return
  end if

  str_out = str_in(start_field:end_field)
  err_n   = SUCCESS

end subroutine get_field

end program main
