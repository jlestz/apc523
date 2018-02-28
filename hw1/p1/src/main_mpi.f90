! program to loop over different values of Monte Carlo iterations
! for calculating pi with function pi_mpi.f90 
! Jeff Lestz 
! 22 Feb 2018 
!
! set the parameter n to set the range of 10^n to loop over

program main_mpi 
  use mpi

  implicit none
  ! largest number of iterations: 10^n 
  integer*8, parameter :: n = 12
  integer*8 :: ntrials,i
  real*8, parameter :: pi = 3.14159265358979323846
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
    call pi_mpi(MPI_COMM_WORLD,ntrials,pi_appx)
    
    ! calculate error 
    err = abs(pi_appx - PI)/PI

    ! master ends clock and prints results to file 
    if (iproc < 1) then 
      tend = MPI_WTIME()
      print *, ntrials,err,tend-tbeg,"(main)"
    end if 
  end do 

  ! close MPI 
  call MPI_FINALIZE(ierr)

end program main_mpi
