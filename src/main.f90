program main

  use mod_input
  use mod_scm_input
  use mod_scm_compute

  implicit none
  
  call read_input()
  call scm_compute()
!  call write_scm_input()

end program main
