! program to loop over different values of Monte Carlo iterations
! for calculating pi with function pi_mpi.f90 
! Jeff Lestz 
! 22 Feb 2018 
!
! set the parameter n to set the range of 10^n to loop over

program main 
  use mpi
  
  ! largest number of iterations: 10^n 
  integer*8, parameter :: n = 16
  integer*8 :: ntrials
  real*8, parameter :: pi = 3.14159265358979326846
  real*8 :: pi_appx,err
  real*8 :: tbeg,tend
  
  real*8 :: pi_mpi
  integer :: ierr,iproc

  ! initialize MPI
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD,iproc,ierr)
  
  ! run Monte Carlo calucation for 10^i trials
  ! compare the relative error to exact answer
  do i=1,n
    ntrials=10**i
    ! time each call for scaling 
    call cpu_time(tbeg)
    pi_appx = pi_mpi(ntrials)
    call cpu_time(tend)
    err = abs(pi_appx - PI)/PI
    if (iproc < 1) then 
      print *, i,err,tend-tbeg
    end if 
  end do 

  ! close MPI 
  call MPI_FINALIZE(ierr)

end program main
