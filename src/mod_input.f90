module mod_input

  use mod_error
  use mod_get_field
  use mod_scm_data

  implicit none
  save
  private

  public :: read_input

contains

!!! Public !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine read_input()

  ! Input file structure
  !
  ! t_n                          | number of temperatures
  ! t1                           | list of t_n temperatures, one per line
  ! ...
  ! tn
  !     <empty line>
  ! n_n                          | number of nodes
  ! mode                         | mode can be "vertex_only" or "vertex_group"
  ! n1 [g1]                      | n_n lines containing each a node label
  ! ...                          | (string) and optionally a node group
  ! ...                          | (integer), as specified by mode.
  ! nn [gn]
  !     <empty line>
  ! e_n                          | number of edges
  ! mode                         | mode can be "index" or "label"
  ! ni nj db1 rb1                | list of e_n edges, each line contains two
  ! ...                          | indices or labels (as specified by mode)
  ! ...                          | ni and nj, as well as two reals, the direct
  ! ...                          | barrier db1 from ni to nj, and the reverse
  ! ...                          | barrier rb1 from nj to ni
  ! nk nl dbn rbn
  !     <empty line>
  ! pde rde                      | product and reactant desorption energies

  use, intrinsic :: iso_fortran_env

  character(*), parameter :: my_name = "read_input"
  character(*), parameter :: fname = "paths2scm.in"
  integer, parameter :: fnumb = 700
  character(200) :: buff
  character(100) :: arg
  character(16) :: mode
  integer :: i
  integer :: j
  integer :: i1
  integer :: i2
  real(REAL64) :: db
  real(REAL64) :: rb
  integer :: t_n
  integer :: n_n
  integer :: e_n
  real(REAL64), dimension(:), allocatable :: t
  character(16), dimension(:), allocatable :: labels
  integer, dimension(:), allocatable :: groups
  real(REAL64), dimension(:,:), allocatable :: bars
  real(REAL64) :: pde
  real(REAL64) :: rde
  logical :: flag_groups
  integer :: err_n
  character(120) :: err_msg

  flag_groups = .false.

  ! open
  open(unit=fnumb,file=fname,status="old",action="read", &
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! temperatures
  read(fnumb,*,iostat=err_n,iomsg=err_msg) t_n
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(t(t_n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  do i = 1, t_n
    read(fnumb,*,iostat=err_n,iomsg=err_msg) t(i)
    if (err_n /= 0) call error(my_name,err_msg)
  end do
  read(fnumb,*,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! nodes
  read(fnumb,*,iostat=err_n,iomsg=err_msg) n_n
  if (err_n /= 0) call error(my_name,err_msg)
  read(fnumb,'(A16)',iostat=err_n,iomsg=err_msg) mode
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(labels(n_n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(groups(n_n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  select case (mode)
  case ("vertex_only")
    do i = 1, n_n
      read(fnumb,'(A16)',iostat=err_n,iomsg=err_msg) labels(i)
      if (err_n /= 0) call error(my_name,err_msg)
      labels(i) = adjustl(labels(i))
      if (len_trim(labels(i)) == 0) call error(my_name,"error reading labels")
    end do
  case ("vertex_group")
    flag_groups = .true.
    do i = 1, n_n
      read(fnumb,*,iostat=err_n,iomsg=err_msg) labels(i), groups(i)
      if (err_n /= 0) call error(my_name,err_msg)
      labels(i) = adjustl(labels(i))
      if (len_trim(labels(i)) == 0) call error(my_name,"error reading labels")
    end do
    write(*,'(10(2X,I6))') groups
  case default
    call error(my_name,"invalid mode "//trim(mode))
  end select
  read(fnumb,*,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! edges
  read(fnumb,*,iostat=err_n,iomsg=err_msg) e_n
  if (err_n /= 0) call error(my_name,err_msg)
  read(fnumb,'(A16)',iostat=err_n,iomsg=err_msg) mode
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(bars(n_n,n_n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  bars = 0.0_REAL64
  select case (mode)
  case ("index")
    do i = 1, e_n
      read(fnumb,*,iostat=err_n,iomsg=err_msg) i1, i2, db, rb
      if (err_n /= 0) call error(my_name,err_msg)
      if ((i1 < 1).or.(i1 > n_n)) call error(my_name,"node out of range")
      if ((i2 < 1).or.(i2 > n_n)) call error(my_name,"node out of range")
      bars(i1,i2) = db
      bars(i2,i1) = rb
    end do
  case ("label")
    do i = 1, e_n
      read(fnumb,'(A200)',iostat=err_n,iomsg=err_msg) buff
      if (err_n /= 0) call error(my_name,err_msg)
      call get_field(buff,arg,1,err_n,err_msg)
      if (err_n /= 0) call error(my_name,err_msg)
      i1 = -1
      do j = 1, n_n
        if (arg == labels(j)) i1 = j
      end do
      if (i1 == -1) call error(my_name,"invalid label in edge specification")
      call get_field(buff,arg,2,err_n,err_msg)
      if (err_n /= 0) call error(my_name,err_msg)
      i2 = -1
      do j = 1, n_n
        if (arg == labels(j)) i2 = j
      end do
      if (i2 == -1) call error(my_name,"invalid label in edge specification")
      call get_field(buff,arg,3,err_n,err_msg)
      if (err_n /= 0) call error(my_name,err_msg)
      read(arg,*,iostat=err_n,iomsg=err_msg) db
      if (err_n /= 0) call error(my_name,err_msg)
      call get_field(buff,arg,4,err_n,err_msg)
      if (err_n /= 0) call error(my_name,err_msg)
      read(arg,*,iostat=err_n,iomsg=err_msg) rb
      if (err_n /= 0) call error(my_name,err_msg)
      bars(i1,i2) = db
      bars(i2,i1) = rb
    end do
  case default
    call error(my_name,"invalid mode "//trim(mode))
  end select
  read(fnumb,*,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! pde rde
  read(fnumb,*,iostat=err_n,iomsg=err_msg) pde, rde
  if (err_n /= 0) call error(my_name,err_msg)

  ! close
  close(unit=fnumb,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! set
  call set_scm_variables(t,labels,bars,pde,rde)

  ! deallocate
  deallocate(t,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(labels,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(groups,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(bars,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine read_input

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_input
