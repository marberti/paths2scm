module mod_scm_data

  use, intrinsic :: iso_fortran_env
  use mod_error
  use mod_get_field

  implicit none
  save
  private

  ! public procedures
  public :: set_scm_variables,           &
            get_path_on_groups

  ! protected variables
  protected :: flag_set_scm_variables,   &
               flag_scm_verbose,         &
               flag_scm_groups,          &
               n_temper,                 &
               max_steps_n,              &
               v_number,                 &
               prod_des_en,              &
               reac_des_en,              &
               temper,                   &
               alllabels,                &
               allgroups,                &
               barriers
  public    :: flag_set_scm_variables,   &
               flag_scm_verbose,         &
               flag_scm_groups,          &
               n_temper,                 &
               max_steps_n,              &
               v_number,                 &
               prod_des_en,              &
               reac_des_en,              &
               temper,                   &
               alllabels,                &
               allgroups,                &
               barriers

  ! private
  integer :: n_temper, max_steps_n
  integer :: v_number = -1
  real(REAL64) :: prod_des_en, reac_des_en
  real(REAL64), dimension(:), allocatable :: temper
  character(20), dimension(:), allocatable :: alllabels
  integer, dimension(:), allocatable :: allgroups
  real(REAL64), dimension(:,:), allocatable :: barriers
  logical :: flag_set_scm_variables = .false.
  logical :: flag_scm_verbose = .false.
  logical :: flag_scm_groups = .false.

contains

!!! Public !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine set_scm_variables(t,labels,bars,pde,rde,flag_groups,groups)

  real(REAL64), dimension(:), intent(in) :: t
  character(16), dimension(:), intent(in) :: labels
  real(REAL64), dimension(:,:), intent(in) :: bars
  real(REAL64), intent(in) :: pde
  real(REAL64), intent(in) :: rde
  logical, intent(in) :: flag_groups
  integer, dimension(:), intent(in) :: groups
  character(*), parameter :: my_name = "set_scm_variables"
  character(8) :: i_str
  integer :: i
  integer :: n
  integer :: err_n
  character(120) :: err_msg

  n = size(t)
  n_temper = n
  allocate(temper(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  temper = t

  n = size(labels)
  v_number = n
  max_steps_n = n+1
  allocate(alllabels(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  do i = 1, n
    write(i_str,'(I8)') i
    i_str = adjustl(i_str)
    alllabels(i) = trim(i_str)//":"//trim(labels(i))
  end do

  n = size(bars,1)
  if (n /= size(bars,2)) call error(my_name,"wrong size of bars argument")
  allocate(barriers(n,n),stat=err_n,errmsg=err_msg)
  barriers = bars

  prod_des_en = pde
  reac_des_en = rde

  flag_scm_groups = flag_groups
  if (flag_groups) then
    n = size(groups)
    if (n /= v_number) call error(my_name,"wrong size of groups argument")
    allocate(allgroups(n),stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
    allgroups = groups
  end if

  flag_set_scm_variables = .true.

end subroutine set_scm_variables

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_path_on_groups(path,sout)

  character(*), intent(in) :: path
  character(*), intent(out) :: sout
  character(*), parameter :: my_name = "get_path_on_groups"
  character(20) :: field
  character(8) :: istr
  integer :: i
  integer :: gi
  integer :: tmp
  integer :: err_n
  character(120) :: err_msg

  if (.not.flag_scm_groups) call error(my_name,"groups not setted")

  sout = ""
  tmp = -1
  do i = 1, count_fields(path)
    call get_field(path,field,i,err_n,err_msg) ! no need to check exit status
    read(field,*) gi
    if (allgroups(gi) /= tmp) then
      tmp = allgroups(gi)
      write(istr,'(I8)') tmp
      istr = adjustl(istr)
      sout = trim(sout)//" "//trim(istr)
    end if
  end do

end subroutine get_path_on_groups

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_scm_data
