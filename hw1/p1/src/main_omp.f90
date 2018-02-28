! program to loop over different values of Monte Carlo iterations
! for calculating pi with subroutine pi_omp.f90 
! Jeff Lestz 
! 22 Feb 2018 
!
! set the parameter n to set the range of 10^n to loop over

program main_omp
 
  implicit none 

  ! largest number of iterations: 10^n 
  integer*8, parameter :: n = 12
  integer*8 :: ntrials,i
  real*8, parameter :: pi = 3.14159265358979323846
  real*8 :: pi_appx,err

  real*8 :: OMP_GET_WTIME,tbeg,tend

  ! run Monte Carlo calucation for 10^i trials
  ! compare the relative error to exact answer
  do i=1,n
    ntrials=10**i
    
    ! time each call for scaling 
    tbeg = OMP_GET_WTIME()
    call pi_omp(ntrials,pi_appx)
    tend = OMP_GET_WTIME() 
    
    ! print error for convergence
    err = abs(pi_appx - PI)/PI
    print *, ntrials,err,tend-tbeg
  end do 

end program main_omp
