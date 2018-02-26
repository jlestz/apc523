! program to loop over different values of Monte Carlo iterations
! for calculating pi with function pi_mc.f90 
! Jeff Lestz 
! 22 Feb 2018 
!
! set the parameter n to set the range of 10^n to loop over

program main 
  
  ! largest number of iterations: 10^n 
  integer, parameter :: n = 16
  integer*8 :: ntrials
  real*8, parameter :: pi = 3.14159265358979326846
  real*8 :: pi_appx,err
  real*8 :: tbeg,tend
  
  real*8 :: pi_omp

  ! run Monte Carlo calucation for 10^i trials
  ! compare the relative error to exact answer
  do i=1,n
    ntrials=10**i
    ! time each call for scaling 
    call cpu_time(tbeg)
    pi_appx = pi_omp(ntrials)
    call cpu_time(tend)
    err = abs(pi_appx - PI)
    print *, i,err,tend-tbeg
    print *, ""
  end do 

end program main
