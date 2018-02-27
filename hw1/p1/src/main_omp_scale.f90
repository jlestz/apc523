! program to loop over different values of Monte Carlo iterations
! for calculating pi with function pi_mc.f90 
! Jeff Lestz 
! 26 Feb 2018 
!
! set the parameter n to set the range of 10^n to loop over

program main_omp_scale 
  
  implicit none 

  real*8, parameter :: pi = 3.14159265358979326846
  integer*8, parameter :: ntrials = 10**11
  integer, parameter :: maxthreads=20,minthreads=2,dthreads=2
  
  integer :: nthreads
  real*8 :: pi_appx,err
  real*8 :: OMP_GET_WTIME,tbeg,tend

  ! run Monte Carlo calucation with i omp threads
  ! fixed problem size: ntrials = 10^8
  ! compare the time elapsed to determine strong scaling 
  do nthreads = maxthreads, minthreads, -dthreads

    ! time each call for scaling 
    tbeg = OMP_GET_WTIME()
    ! set the number of threads 
    call OMP_SET_NUM_THREADS(nthreads)
    
    print *,nthreads,"before pi_omp"
    
    ! run the calculation 
    call pi_omp(ntrials,pi_appx)
    tend = OMP_GET_WTIME()

    print *,nthreads,"post pi_omp"
    
    ! print error and time elapsed 
    err = abs(pi_appx - PI)/PI
    print *, nthreads,err,tend-tbeg,"nth,err,time (main)"
  end do 

end program main_omp_scale
