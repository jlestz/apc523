! program to measure scaling with number of mpi threads
! for a fixed problem size in subroutine pi_mpi.f90
! Jeff Lestz 
! 26 Feb 2018 
!
! set the parameters maxthreads, minthreads, dthreads
! to set the number of threads to loop over

program main_mpi_scale 
  
  implicit none 

  real*8, parameter :: pi = 3.14159265358979323846
  integer*8, parameter :: ntrials = 10**11
  integer, parameter :: maxwork=40,minwork=2,dwork=4
  
  integer :: nwork
  real*8 :: pi_appx,err
  real*8 :: tbeg,tend

  integer :: ierr,iproc

  ! initialize MPI 
  call MPI_INIT(ierr) 
  call MPI_COMM_RANK(MPI_COMM_WORLD,iproc,ierr)

  ! run Monte Carlo calucation with nwork mpi threads
  ! fixed problem size: ntrials = 10^11
  ! compare the time elapsed to determine strong scaling 
  do nwork = maxwork, minwork, -dwork

    ! have master begin clock 
    if (iproc < 1) then 
      ! time each call for scaling 
      tbeg = MPI_WTIME()
    end if 
    
    ! run the calculation 
    call pi_mpi(ntrials,pi_appx)

    ! master ends clock and prints results to file 
    if (iproc < 1) then 
      tend = MPI_WTIME()
      print *, nthreads,err,tend-tbeg,"(main)"
    end if 
  end do 

  ! finalize MPI 
  call MPI_Finalize(ierr)

end program main_mpi_scale
