module mod_scm_input

  use mod_error
  use mod_get_field
  use mod_scm_data

  implicit none
  save
  private

  public :: write_scm_input

contains

!!! Public !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine write_scm_input()

  use, intrinsic :: iso_fortran_env

  character(*), parameter :: my_name = "write_input2scm"
  character(*), parameter :: fname = "scm.in"
  character(*), parameter :: fname_in = "graph_paths.out"
  integer, parameter :: fnumb = 400
  integer, parameter :: fnumb_in = 401
  integer(INT64) :: paths_found
  character(400) :: path
  integer :: nofspecies
  integer, dimension(:), allocatable :: nodes
  integer :: i
  integer(INT64) :: ii
  integer :: j
  character(8) :: istr
  integer :: err_n
  character(120) :: err_msg

  if (.not.flag_set_scm_variables) then
    call error(my_name,"call set_scm_variables first!")
  end if

  ! allocation
  allocate(nodes(v_number),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! open
  open(unit=fnumb_in,file=fname_in,status="old",action="read", &
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  open(unit=fnumb,file=fname,status="replace",action="write",  &
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! get paths_found
  paths_found = 0
  do
    read(fnumb_in,*,iostat=err_n)
    if (err_n /= 0) exit
    paths_found = paths_found + 1
  end do
  rewind(unit=fnumb_in,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! write
  write(fnumb,103) 'NOFMEC | NMAXOFSPECIS| NMAXOFSTEPS'
  write(fnumb,100) paths_found, v_number, max_steps_n
  write(fnumb,*)
  write(fnumb,*)
  write(fnumb,103) 'NOFTEMP'
  write(fnumb,101) n_temper
  write(fnumb,*)
  write(fnumb,103) 'TEMPERATURES (K)'
  write(fnumb,102)  temper
  write(fnumb,*)
  write(fnumb,*) 
  write(fnumb,103) 'ALLLABELS'
  do i=1, size(alllabels)
    write(fnumb,103,advance="no") " "//trim(alllabels(i))
  end do
  write(fnumb,*)
  write(fnumb,*)
  write(fnumb,*)
  do ii = 1, paths_found
    read(fnumb_in,'(A400)') path
    nofspecies = count_fields(path)
    write(fnumb,103) 'NOFSPECIES | NOFSTEPS'
    write(fnumb,104) nofspecies, nofspecies+1
    write(fnumb,*)
    write(fnumb,103) 'LABELS'
    do j = 1, nofspecies
      call get_field(path,istr,j,err_n,err_msg)
      if (err_n /= 0) call error(my_name,err_msg)
      read(istr,*) nodes(j)
      write(fnumb,'(A)', advance="no") " "//trim(istr)
    end do
    write(fnumb,*)
    write(fnumb,*)
    write(fnumb,103) 'ENERGY BARRIERS (DIRECT, REVERSE)'
    do j = 1, nofspecies-1
      write(fnumb,105,advance="no") barriers(nodes(j),nodes(j+1))
    end do
    write(fnumb,105) prod_des_en
    do j = nofspecies-1, 1, -1
      write(fnumb,105,advance="no") barriers(nodes(j+1),nodes(j))
    end do
    write(fnumb,105) reac_des_en
    write(fnumb,*)
    write(fnumb,*)
    if (mod(ii,100000_INT64) == 0) then
      write(*,'(1X,A,A,F6.1,A)') &
        my_name,": Output generated: ",real(ii)/real(paths_found)*100.0,"%"
    end if
  end do  
  write(*,'(1X,A,A,F6.1,A)') my_name,": Output generated: ",100.0,"%"

  ! close
  close(unit=fnumb_in,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  close(unit=fnumb,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! deallocation
  deallocate(nodes,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

100 FORMAT (1X,I15,2(1X,I6))
101 FORMAT (1X,I2)
102 FORMAT (1X,F8.2)
103 FORMAT (A)
104 FORMAT (2(1X,I6))
105 FORMAT (1X,F7.2)

end subroutine write_scm_input

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_scm_input
