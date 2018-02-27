! program to loop over different values of Monte Carlo iterations
! for calculating pi with function pi_mpi.f90 
! Jeff Lestz 
! 22 Feb 2018 
!
! set the parameter n to set the range of 10^n to loop over

program main 
  use mpi
  
  ! largest number of iterations: 10^n 
  integer*8, parameter :: n = 10
  integer*8 :: ntrials,i
  real*8, parameter :: pi = 3.14159265358979326846
  real*8 :: pi_appx,err
  
  integer :: ierr,iproc
  real *8 :: tbeg,tend

  ! initialize MPI
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD,iproc,ierr)
  
  ! run Monte Carlo calculation for 10^i trials
  ! compare the relative error to exact answer
  do i=1,n
    ! set number of Monte Carlo trials 
    ntrials=10**i

    ! have master begin clock 
    if (iproc < 1) then 
      ! time each call for scaling 
      tbeg = MPI_WTIME()
    end if 

    ! call subroutine 
    call pi_mpi(ntrials,pi_appx)

    ! master ends clock 
    if (iproc < 1) then 
      tend = MPI_WTIME()
    end if 

    ! calculate error 
    err = abs(pi_appx - PI)/PI

    ! master prints results to file 
    if (iproc < 1) then 
      print *, ntrials,"ntrials (main)"
      print *, i,err,tend-tbeg,"i,err,time (main)"
    end if 
  end do 

  ! close MPI 
  call MPI_FINALIZE(ierr)

end program main
