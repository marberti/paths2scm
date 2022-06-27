module mod_get_field

  implicit none
  save
  private

  public :: get_field,   &
            count_fields

contains

!!! Public !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer function count_fields(str)

  character(*), intent(in) :: str
  character(120) :: ch
  integer :: i
  integer :: err_n
  character(120) :: err_msg

  i = 0
  do
    i = i + 1
    call get_field(str,ch,i,err_n,err_msg)
    if (err_n /= 0) then
      i = i - 1
      exit
    end if
  end do

  count_fields = i

end function count_fields

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_get_field
