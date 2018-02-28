! program to measure scaling with number of omp threads
! for a fixed problem size in subroutine pi_omp.f90
! Jeff Lestz 
! 26 Feb 2018 
!
! set the parameters maxthreads, minthreads, dthreads
! to set the number of threads to loop over

program main_omp_scale 
  
  implicit none 

  real*8, parameter :: pi = 3.14159265358979323846
  integer*8, parameter :: ntrials = 10**11
  integer, parameter :: maxthreads=20,minthreads=2,dthreads=2
  
  integer :: nthreads
  real*8 :: pi_appx,err
  real*8 :: OMP_GET_WTIME,tbeg,tend

  ! run Monte Carlo calucation with nthreads omp threads
  ! fixed problem size: ntrials = 10^11
  ! compare the time elapsed to determine strong scaling 
  do nthreads = maxthreads, minthreads, -dthreads

    ! time each call for scaling 
    tbeg = OMP_GET_WTIME()
    
    ! set the number of threads 
    call OMP_SET_NUM_THREADS(nthreads)
    
    ! run the calculation 
    call pi_omp(ntrials,pi_appx)
    tend = OMP_GET_WTIME()

    ! print error and time elapsed 
    err = abs(pi_appx - PI)/PI
    print *, nthreads,err,tend-tbeg
  end do 

end program main_omp_scale
