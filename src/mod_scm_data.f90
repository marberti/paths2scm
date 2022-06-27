module mod_scm_data

  use, intrinsic :: iso_fortran_env
  use mod_error

  implicit none
  save
  private

  ! public procedures
  public :: set_scm_variables

  ! protected variables
  protected :: flag_set_scm_variables,   &
               n_temper,                 &
               max_steps_n,              &
               v_number,                 &
               prod_des_en,              &
               reac_des_en,              &
               temper,                   &
               alllabels,                &
               barriers
  public    :: flag_set_scm_variables,   &
               n_temper,                 &
               max_steps_n,              &
               v_number,                 &
               prod_des_en,              &
               reac_des_en,              &
               temper,                   &
               alllabels,                &
               barriers

  integer :: n_temper, max_steps_n
  integer :: v_number = -1
  real(REAL64) :: prod_des_en, reac_des_en
  real(REAL64), dimension(:), allocatable :: temper
  character(20), dimension(:), allocatable :: alllabels
  real(REAL64), dimension(:,:), allocatable :: barriers
  logical :: flag_set_scm_variables = .false.

contains

!!! Public !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine set_scm_variables(t,labels,bars,pde,rde)

  real(REAL64), dimension(:), intent(in) :: t
  character(16), dimension(:), intent(in) :: labels
  real(REAL64), dimension(:,:), intent(in) :: bars
  real(REAL64), intent(in) :: pde
  real(REAL64), intent(in) :: rde
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

  flag_set_scm_variables = .true.

end subroutine set_scm_variables

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_scm_data
