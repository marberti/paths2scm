module mod_scm_compute

  use, intrinsic :: iso_fortran_env
  use mod_error
  use mod_get_field
  use mod_scm_data

  implicit none
  save
  private

  ! public procedures
  public :: scm_compute

  ! private
  integer, parameter :: fnumb_in = 810
  integer, parameter :: fnumb_o  = 820
  integer, parameter :: fnumb_go = 830
  character(*), parameter :: fname_in = "graph_paths.out"
  character(*), parameter :: fname_o  = "OUTPUT"
  character(*), parameter :: fname_go = "GLOBAL_OUT"
  integer(INT64) :: nofmec
  integer(INT64) :: currentmec
  real(REAL64) :: s_mean
  real(REAL64), dimension(:), allocatable :: w
  real(REAL64), dimension(:), allocatable :: theta
  real(REAL64), dimension(:), allocatable :: Theta_means
  real(REAL64), dimension(:), allocatable :: S_j
  character(20), dimension(:), allocatable :: labelset

contains
  
!!! Public !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine scm_compute()

  character(*), parameter :: my_name = "scm_compute"
  integer(INT64) :: paths_found
  character(400) :: path
  character(8) :: istr
  character(20) :: iistr
  character(200), dimension(:), allocatable :: tmp_groups
  character(200), dimension(:), allocatable :: tmp_unique
  character(200), dimension(:), allocatable :: unique_paths_on_groups
  integer(INT64) :: ii
  integer(INT64) :: jj
  integer :: ti
  integer :: j
  integer :: k
  integer :: m
  integer :: ntotofspecies
  integer :: nmaxofsteps
  integer :: nofspecies
  integer :: nofsteps
  integer(INT64) :: unique_paths_on_groups_n
  real(REAL64) :: s_array
  real(REAL64) :: sum_s_array
  real(REAL64) :: s
  integer, dimension(:), allocatable :: Labels_numb
  real(REAL64), dimension(:), allocatable :: E
  logical :: flag_exist
  integer :: err_n
  character(120) :: err_msg

  ! open files
  open(unit=fnumb_in,file=fname_in,status="old",action="read", &
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  if (flag_scm_verbose) then
    open(unit=fnumb_o,file=fname_o,status="replace",action="write", &
      iostat=err_n,iomsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
  end if
  open(unit=fnumb_go,file=fname_go,status="replace",action="write", &
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! get paths_found
  write(*,'(A)',advance='no') " Counting total paths ... "
  paths_found = 0
  do
    read(fnumb_in,*,iostat=err_n)
    if (err_n /= 0) exit
    paths_found = paths_found + 1
  end do
  rewind(unit=fnumb_in,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  write(iistr,'(I20)') paths_found
  iistr = adjustl(iistr)
  write(*,'(A)') "DONE: "//trim(iistr)//" paths found"

  ! get unique_paths_on_groups
  unique_paths_on_groups_n = 0
  if (flag_scm_groups) then
    write(*,'(A)',advance='no') " Getting unique paths on groups ... "

    allocate(tmp_groups(paths_found),stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
    allocate(tmp_unique(paths_found),stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)

    ! transform from paths to paths on groups
    do ii = 1, paths_found
      read(fnumb_in,'(A400)',iostat=err_n,iomsg=err_msg) path
      if (err_n /= 0) call error(my_name,err_msg)
      call get_path_on_groups(path,tmp_groups(ii))
    end do

    ! get unique paths on groups
    do ii = 1, paths_found
      flag_exist = .false.
      do jj = 1, unique_paths_on_groups_n
        if (tmp_groups(ii) == tmp_unique(jj)) then
          flag_exist = .true.
          exit
        end if
      end do

      if (.not.flag_exist) then
        unique_paths_on_groups_n = unique_paths_on_groups_n + 1
        tmp_unique(unique_paths_on_groups_n) = tmp_groups(ii)
      end if
    end do

    allocate(unique_paths_on_groups(unique_paths_on_groups_n),&
      stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
    do ii = 1, unique_paths_on_groups_n
      unique_paths_on_groups(ii) = tmp_unique(ii)
      write(*,*) trim(unique_paths_on_groups(ii))
    end do

    ! finalization
    deallocate(tmp_groups,stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
    deallocate(tmp_unique,stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)

    rewind(unit=fnumb_in,iostat=err_n,iomsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)

    write(iistr,'(I20)') unique_paths_on_groups_n
    iistr = adjustl(iistr)
    write(*,'(A)') "DONE: "//trim(iistr)//" unique paths on groups found"
  end if

  ! set some variables
  nofmec = paths_found
  ntotofspecies = v_number
  nmaxofsteps = max_steps_n

  allocate(labelset(ntotofspecies+1),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  labelset(1) = 'FREE'
  do j = 1, ntotofspecies
    labelset(j+1) = alllabels(j)
  end do

  allocate(S_j(nofmec),stat=err_n,errmsg=err_msg)                      
  if (err_n /= 0) call error(my_name,err_msg)

  allocate(E(-nmaxofsteps:nmaxofsteps),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  allocate(Labels_numb(ntotofspecies+1),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  
  allocate(Theta_means(ntotofspecies+1),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! perform scm for every temperature
  do ti = 1, n_temper
    ! perform scm for every path
    s_array = 0.0_REAL64
    sum_s_array = 0.0_REAL64
    Theta_means = 0.0_REAL64
    do ii = 1, paths_found
      currentmec = ii

      read(fnumb_in,'(A400)',iostat=err_n,iomsg=err_msg) path
      if (err_n /= 0) call error(my_name,err_msg)
      nofspecies = count_fields(path)
      nofsteps = nofspecies + 1

      Labels_numb = -1
      Labels_numb(1) = 0
      do j = 1, nofspecies
        call get_field(path,istr,j,err_n,err_msg)
        if (err_n /= 0) call error(my_name,err_msg)
        read(istr,*) Labels_numb(j+1)
      end do

      E           = 1.0_REAL64
      E(nofsteps) = prod_des_en
      E(-1)       = reac_des_en
      do j = 2, nofsteps-1
        E(j)  = barriers(Labels_numb(j),Labels_numb(j+1))
        E(-j) = barriers(Labels_numb(j+1),Labels_numb(j))
      end do

      call calc_scm(nofsteps,temper(ti),E(-nofsteps:nofsteps),s)

      s_array = s_array + s**2
      sum_s_array = sum_s_array + s
      S_j(ii) = s

      do k = 0, ntotofspecies
        do m = 1, size(theta)
          if (Labels_numb(m) == k) then
            Theta_means(k+1) = Theta_means(k+1) + theta(m) * s
          end if
        end do
      end do

      if (mod(ii,100000_INT64) == 0) then
        write(*,'(1X,A,A,I2,A,I2,A,F6.1,A)')   &
          my_name,": T ",ti,"/",n_temper,": ", &
          real(ii)/real(paths_found)*100,"%"
      end if
    end do ! ii on paths_found
    Theta_means = Theta_means / sum_s_array
    s_mean = s_array / sum_s_array
    S_j = S_j / sum_s_array * 100
    call write_globalout(temper(ti))
    write(*,'(1X,A,A,I2,A,I2,A,F6.1,A)') &
      my_name,": T ",ti,"/",n_temper,": ",100.0,"%"
  end do ! ti on n_temper

  ! deallocation
  deallocate(labelset,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(S_j,stat=err_n,errmsg=err_msg)                      
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(E,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(Labels_numb,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(Theta_means,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  if (allocated(unique_paths_on_groups)) then
    deallocate(unique_paths_on_groups,stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
  end if

  ! close files
  close(unit=fnumb_in,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  if (flag_scm_verbose) then
    close(unit=fnumb_o,iostat=err_n,iomsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
  end if
  close(unit=fnumb_go,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine scm_compute

!!! Private !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine calc_scm(nofsteps,T,E,s)

  ! This subroutine carries out the SCM analysis one mechanism at a time.

  integer, intent(in) :: nofsteps
  real(REAL64), intent(in) :: T
  real(REAL64), dimension(-nofsteps:nofsteps), intent(in) :: E
  real(REAL64), intent(out) :: s
  character(*), parameter :: my_name = "calc_scm"
  real(REAL64), parameter :: K_B = 1.381E-23_REAL64
  real(REAL64), parameter :: h = 6.626E-34_REAL64
  real(REAL64), parameter :: R = 8.3145_REAL64
  real(REAL64) :: A
  integer :: i
  integer :: j
  integer :: k
  integer :: m
  real(REAL64) :: det_r_L
  real(REAL64) :: Mtot
  real(REAL64), dimension(:,:), allocatable :: matrix
  integer :: err_n
  character(120) :: err_msg

  ! Compute vector w
  allocate(w(-nofsteps:nofsteps),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  w = 0.0_REAL64
  A = K_B*T/h
  w(1) = A
  do i = 2, nofsteps
    w(i) = A*exp(-1000*E(i)/(R*T))
  end do

  do i = -nofsteps+1, -1
    w(i) = A*exp(-1000*E(i)/(R*T))
  end do

  ! Compute det_r_L and matrix
  det_r_L = product(w(1:nofsteps))
  allocate(matrix(nofsteps,nofsteps))
  if (err_n /= 0) call error(my_name,err_msg)
  do i = 1, nofsteps
    matrix(i,1) = det_r_L/w(i)
  end do

  do i = 1, nofsteps
    do j = 2,nofsteps
      k = mod(i+j-1,nofsteps)
      if (k == 0) then
        k = nofsteps
      end if
      m = mod(2-i-j,nofsteps)
      if (m == 0) then
        m = -nofsteps
      end if
      matrix(i,j) = matrix(i,j-1)*w(m)/w(k)
    end do
  end do

  ! Calulate s and thetas for the i-th mechanism, call subroutine writeout
  Mtot = sum(matrix)
  s = det_r_L/Mtot
  if (allocated(theta)) then
    deallocate(theta,stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
  end if
  allocate(theta(nofsteps),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  do i = 1, nofsteps
    theta(i) = sum(matrix(i,1:nofsteps))/Mtot
  end do

  if (flag_scm_verbose) call writeout(T,nofsteps,s)

  deallocate(w,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(matrix,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine calc_scm

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine writeout(T,nofsteps,s)

  !THIS SUBROUTINE WRITES THE 'OUTPUT' FILE THAT CONTAINS, AT ALL TEMPERATURES, %
  !INFORMATION ABOUT EACH MECHANISM, CONSIDERED SEPARATLY.
  !For each temperature and for each mechanism, event probabilities per unit of %
  !time, mechanism rates and surface molar fractions of involved species%
  !are reported.

  integer, intent(in) :: nofsteps
  real(REAL64), intent(in) :: T
  real(REAL64), intent(in) :: s

  write(fnumb_o,*) '_______________________________________________________________________________'
  write(fnumb_o,*)
  write(fnumb_o,100) T, currentmec
  write(fnumb_o,*) '-------------------------------------------------------------------------------'
  write(fnumb_o,*) "Probabilities per unit of time w (s^-1), in the order w1,..wn, w-n,...w-1:"
  write(fnumb_o,101)  w(1:nofsteps), w(-nofsteps:-1)
  write(fnumb_o,*) '-------------------------------------------------------------------------------'
  write(fnumb_o,102) s
  write(fnumb_o,*) '-------------------------------------------------------------------------------'
  write(fnumb_o,*) "Surface molar fractions theta, in the order theta1...thetan:"
  write(fnumb_o,101) theta
  write(fnumb_o,*) '_______________________________________________________________________________'
  write(fnumb_o,*)
  
  100 format (' Temperature:', F9.2, ' K | Mechanism: ', I3 )
  101 format (ES8.1)
  102 format (' Mechanism rate s = ', ES12.4, ' s^-1')

end subroutine writeout

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine write_globalout(T)

  !THIS SUBROUTINE WRITES THE 'GLOBAL_OUT' FILE THAT CONTAINS, FOR EACH %
  !TEMPERATURE, INFORMATION MEDIATED ON ALL MECHANISMS.
  !For each temperature, reaction rate (the avarage of s of single mechanisms), %
  !selectivities of each mechanism and molar ratio of all n involved surface %
  !species (the n-th specie being each time considered in all mechanisms) %
  !are reported.

  real(REAL64), intent(in) :: T
  real(REAL64), parameter :: th = 1.0e-3_REAL64
  integer(INT64) :: i
  integer :: sc
  character(20) :: tmpc

  write(fnumb_go,*) '_______________________________________________________________________________'
  write(fnumb_go,*)
  write(fnumb_go,103) T
  write(fnumb_go,*) '-------------------------------------------------------------------------------'
  write(fnumb_go,104) s_mean
  write(fnumb_go,*) '-------------------------------------------------------------------------------'
  write(fnumb_go,108) th
  do i = 1, nofmec
    if (S_j(i) > th) write(fnumb_go,105) i, S_j(i)
  end do
!  write(*,*)
!  write(*,107) sum(S_j)
!  write(*,*)
  write(fnumb_go,*) '-------------------------------------------------------------------------------'
  write(fnumb_go,*) 'Theta_means:'
  do i = 1, size(labelset)
    tmpc = trim(labelset(i))
    sc = 1
    if (tmpc(2:2) == ':') sc=3
    if (tmpc(3:3) == ':') sc=4
    if (tmpc(4:4) == ':') sc=5
    write(fnumb_go,106) tmpc(sc:), Theta_means(i)
  end do
  write(fnumb_go,*) '_______________________________________________________________________________'
  write(fnumb_go,*) 

  103 format (1X,'Temperature : ',F9.2,' K')
  104 format (1X,'Reaction rate = ',ES9.2,' s^-1')
  105 format (1X,'j = ',I18,' ; Sj = ',F8.3,' %')
  106 format (1X,A5,' : ',ES8.2)
!  107 format (1X,'CHECK: ', F12.4, ' %')
  108 format (1X,'Percentage selectivities Sj (1 <= j <= nofmec; threshold ', &
    ES8.1,')')

end subroutine write_globalout

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_scm_compute
